{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Database.Beam.Migrate.Tool
  ( module Database.Beam.Migrate.Tool.Types
  , invokeMigrationTool ) where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.SQL
import           Database.Beam.Migrate.Tool.Interactive
import           Database.Beam.Migrate.Tool.Schema hiding (migration)
import           Database.Beam.Migrate.Types ( MigrationStep(..), MigrationF(..), Migration
                                             , MigrationSteps, SomeDatabasePredicate(..)
                                             , DatabasePredicate(..)

                                             , stepNames
                                             , migrationStepsToMigration
                                             , collectChecks )

import           Database.Beam.Migrate.Tool.Types

import           Control.Exception (throwIO)
import           Control.Monad
import           Control.Monad.Free.Church
import           Control.Monad.Trans

import qualified Data.ByteString.Lazy as BL hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as BL (putStrLn)
import           Data.IORef
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time

import           System.IO

import           Text.Tabular
import           Text.Tabular.AsciiArt

-- * Migration table creation script

runMigrationSteps :: MonadBeam syntax be hdl m => (syntax -> String)
                  -> Migration syntax a -> m (Either DdlError a)
runMigrationSteps renderSyntax steps =
  runF steps finish step
  where finish x = pure (Right x)
        step (MigrationRunCommand up _ next) =
          do liftIO (putStrLn (renderSyntax up))
             runNoReturn up
             next

writeMigrationsTable :: IsSql92DdlCommandSyntax cmdSyntax =>
                        BeamMigrationBackend be cmdSyntax beOptions -> beOptions -> IO ()
writeMigrationsTable BeamMigrationBackend {..} opts =
  void $
  migrationStepsToMigration 0 Nothing migrationToolSchemaMigration
    (\nm -> smartTransact . smartTransact . backendTransact opts . runMigrationSteps backendRenderSyntax)

-- * Tool entry point

smartTransact :: IO (Either DdlError a)
              -> IO a
smartTransact runTransact =
  do x <- runTransact
     case x of
       Left err -> throwIO (BeamMigrateBackendError err)
       Right x -> pure x

calcMigrationStatus :: ( IsSql92DdlCommandSyntax cmdSyntax
                       , IsSql92Syntax cmdSyntax
                       , Sql92SanityCheck cmdSyntax)
                    => BeamMigrationBackend be cmdSyntax options -> options
                    -> MigrationSteps cmdSyntax () () -> IO MigrationsStatus
calcMigrationStatus BeamMigrationBackend {..} beOptions steps =
  do let allMigrationNames = stepNames steps

     alreadyRun <- smartTransact . backendTransact beOptions $
                   runSelectReturningList $
                   select $
                   orderBy_ (\m -> asc_ (migrationStartedAt m)) $
                   all_ (migrationDbMigrations migrationDb)

     pure (migrationStatus 0 allMigrationNames alreadyRun)

invokeMigrationTool :: forall cmdSyntax be beOptions.
                       (IsSql92DdlCommandSyntax cmdSyntax, IsSql92Syntax cmdSyntax, Sql92SanityCheck cmdSyntax ) =>
                       BeamMigrationBackend be cmdSyntax beOptions -> BeamMigrationCommand beOptions
                    -> BeamMigrationSubcommand -> MigrationSteps cmdSyntax () ()
                    -> SomeCheckedDatabase be
                    -> IO ()
invokeMigrationTool be@(BeamMigrationBackend {..}) BeamMigrationCommand {..}  subcommand steps checkedDb@(SomeCheckedDatabase db) =
  case subcommand of
    ListMigrations ->
      do putStrLn "Registered migrations:"
         mapM_ (putStrLn . ("  - " ++) . T.unpack) (stepNames steps)
    WriteScript ->
      BL.putStrLn (backendRenderSteps steps)
    Init ->
      writeMigrationsTable be migrationCommandBackendOptions
    Status ->
      do status <- calcMigrationStatus be migrationCommandBackendOptions steps
         let lastMigrationNumber = case migrationNumber . fst <$> migrationsStatusApplied status of
                                     [] -> 0
                                     nums -> maximum nums
             migrations = map (\(m, complete) -> ( if complete then MigrationApplied else MigrationIncomplete
                                                 , migrationNumber m, migrationName m, Just (migrationRanAt m)))
                              (migrationsStatusApplied status) ++
                          case migrationsStatusFutureStatus status of
                            MigrationsFutureStatusUpToDate -> []
                            MigrationsFutureStatusNotYet _ nms ->
                              zipWith (\num nm -> (MigrationScheduled, num, nm, Nothing)) [lastMigrationNumber+1..] nms
                            MigrationsStatusDiverged divergedAt yetToRun unknown ->
                              map (\m -> (MigrationUnknown, migrationNumber m, migrationName m, Just (migrationRanAt m))) unknown ++
                              zipWith (\num nm -> (MigrationDiverged, num, nm, Nothing)) [lastMigrationNumber+1..] yetToRun
         case migrationsStatusFutureStatus status of
           MigrationsFutureStatusUpToDate ->
             putStrLn "Everything is up-to-date."
           MigrationsFutureStatusNotYet {} ->
             putStrLn "There are migrations to be run."
           MigrationsStatusDiverged {} ->
             putStrLn "There are migrations to be run, but it seems like someone else has modified the database in the meantime."

         let tbl = Table (Group SingleLine (map (Header . head) tblData))
                         (Group SingleLine [ Header "Name"
                                           , Header "Ran At"
                                           , Header "Status" ])
                         (map tail tblData)
             tblData = map (\(sts, num, nm, ranAt) -> [show num, T.unpack nm, maybe "Never" show (join ranAt), statusText sts]) migrations

             statusText MigrationDiverged   = "Diverged"
             statusText MigrationApplied    = "Applied"
             statusText MigrationIncomplete = "Incomplete"
             statusText MigrationUnknown    = "Unknown"
             statusText MigrationScheduled  = "Scheduled"

         putStrLn $ render id id id tbl

    Migrate until ->
      do -- ensureMigrationsTable
         status <- calcMigrationStatus be migrationCommandBackendOptions steps
         case migrationsStatusFutureStatus status of
           MigrationsStatusDiverged {} ->
             throwIO MigrationsDiverged
           MigrationsFutureStatusUpToDate {} -> pure ()
           MigrationsFutureStatusNotYet firstIdx remaining ->
             do let lastIdx = firstIdx + length remaining
                tz <- getCurrentTimeZone
                curIdxV <- newIORef firstIdx
                migrationStepsToMigration firstIdx (Just lastIdx) steps
                  (\nm migration ->
                      do putStrLn ("Running migration " <> T.unpack nm)

                         curIdx <- readIORef curIdxV
                         writeIORef curIdxV (curIdx + 1)

                         curLocalTime <- utcToLocalTime tz <$> getCurrentTime
                         smartTransact . smartTransact . backendTransact migrationCommandBackendOptions $ do
                           runInsert (insert (migrationDbMigrations migrationDb)
                                             (insertValues [ MigrationT curIdx nm curLocalTime Nothing ]))
                           res <- runMigrationSteps backendRenderSyntax migration
                           case res of
                             Left {} -> pure ()
                             Right {} ->
                               runUpdate (update (migrationDbMigrations migrationDb)
                                                 (\tbl -> [ migrationRanAt tbl <-. just_ (val_ curLocalTime) ])
                                                 (\tbl -> migrationNumber tbl ==. val_ curIdx ))
                           pure res)

         pure ()

    Diff interactive direction ->
      do putStrLn "Getting database constraints..."
         dbConstraints <- backendGetDbConstraints migrationCommandBackendOptions
         let schemaConstraints = collectChecks db

         let (preConditions, postConditions) = 
               case direction of
                 DiffDbToMigration ->
                   ( filter (not . (`elem` schemaConstraints)) dbConstraints
                   , filter (not . (`elem` dbConstraints)) schemaConstraints )
                 DiffMigrationToDb ->
                   ( filter (not . (`elem` dbConstraints)) schemaConstraints
                   , filter (not . (`elem` schemaConstraints)) dbConstraints )

         if interactive
           then diffInteractive be migrationCommandBackendOptions preConditions postConditions
           else do
             -- putStrLn "This is what our schema expected"
             -- forM_ schemaConstraints $ \(SomeDatabasePredicate p) ->
             --   putStrLn ("  - " <> englishDescription p)

             putStrLn "Preconditions:"
             forM_ preConditions $ \(SomeDatabasePredicate p) ->
               putStrLn ("  - " <> englishDescription p)
             putStrLn "\nPostconditions:"
             forM_ postConditions $ \(SomeDatabasePredicate p) ->
               putStrLn ("  - " <> englishDescription p)

             putStrLn "Beam-migrate will now"

             res <- pure (guessActions defaultActionProviders preConditions postConditions)

             case res of
               GuessResultSolved solutions -> do
                 putStrLn "Possible sequences would be"
                 showSolutions 1 backendRenderSyntax solutions

               GuessResultCandidates cs -> do
                 putStrLn "NO SOLUTION FOUND"
                 forM_ (zip [1..10] cs) $ \(i, steps) ->
                   do putStrLn (show i <> ")")
                      forM_ (dbStateCmdSequence steps) $ \step ->
                        putStrLn ("    - " ++ backendRenderSyntax step)
                      putStrLn "  Remaining to be solved:"
                      forM_ (dbStatePostConditionsLeft steps) $ \(SomeDatabasePredicate pred) ->
                        putStrLn ("    - " ++ englishDescription pred)
                      putStrLn "  Unhandled pre conditions:"
                      forM_ (dbStatePreConditionsLeft steps) $ \(SomeDatabasePredicate pred) ->
                        putStrLn ("    - " ++ englishDescription pred)

             pure ()

         -- Try to order the goals
         --V.fromList (diffGoalTrue schemaDiff)

--    Up ->
--      do ensureMigrationsTable

showSolutions _ _ [] = putStrLn "No more solutions"
showSolutions i renderSyntax (solution:solutions) =
  do putStrLn (show i <> ")")
     forM_ solution $ \step ->
       putStrLn ("  - " ++ renderSyntax step)

     putStrLn "See more?"
     a <- getLine
     if a == "q" then pure () else putStrLn "Finding another" >> showSolutions (i + 1) renderSyntax solutions
