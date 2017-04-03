{-# LANGUAGE RecordWildCards #-}

module Database.Beam.Migrate.Tool
  ( module Database.Beam.Migrate.Tool.Types
  , invokeMigrationTool ) where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.SQL
import           Database.Beam.Migrate.Tool.Schema hiding (migration)
import           Database.Beam.Migrate.Types ( MigrationStep(..), MigrationF(..), Migration
                                             , MigrationSteps, stepNames
                                             , migrationStepsToMigration )

import           Database.Beam.Migrate.Tool.Types

import           Control.Monad.Free.Church

import qualified Data.ByteString.Lazy as BL hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as BL (putStrLn)
import           Data.Text (Text)
import qualified Data.Text as T

import           System.IO

import           Text.Tabular
import           Text.Tabular.SimpleText

-- * Migration table creation script

runMigrationSteps :: MonadBeam syntax be m => (syntax -> String)
                  -> Migration syntax a -> m (Either DdlError a)
runMigrationSteps renderSyntax steps =
  runF steps finish step
  where finish x = pure (Right x)
        step (MigrationRunCommand up _ next) =
          do liftIO (putStrLn (renderSyntax up))
             runNoReturn up
             next

writeMigrationsTable :: IsSql92DdlCommandSyntax cmdSyntax =>
                        BeamMigrationBackend cmdSyntax beOptions -> beOptions -> IO ()
writeMigrationsTable BeamMigrationBackend {..} opts =
    do err <- backendTransact opts $
              runMigrationSteps backendRenderSyntax $
              migrationStepsToMigration migrationToolSchemaMigration

       case err of
         Left err -> hPutStrLn stderr ("Error writing migrations table: " ++ show err)
         Right _ -> pure ()

-- * Tool entry point

invokeMigrationTool :: (IsSql92DdlCommandSyntax cmdSyntax, IsSql92Syntax cmdSyntax, Sql92SanityCheck cmdSyntax ) =>
                       BeamMigrationBackend cmdSyntax beOptions -> BeamMigrationCommand beOptions
                    -> BeamMigrationSubcommand -> MigrationSteps cmdSyntax () -> IO ()
invokeMigrationTool be@(BeamMigrationBackend {..}) BeamMigrationCommand {..}  subcommand steps =
  case subcommand of
    ListMigrations ->
      do putStrLn "Registered migrations:"
         mapM_ (putStrLn . ("  - " ++) . T.unpack) (stepNames steps)
    WriteScript ->
      BL.putStrLn (backendRenderSteps steps)
    Init ->
      writeMigrationsTable be migrationCommandBackendOptions
    Status ->
      do let allMigrationNames = stepNames steps

         alreadyRun <- backendTransact migrationCommandBackendOptions $
                       runSelectReturningList $
                       select $
                       orderBy_ (\m -> asc_ (migrationRanAt m)) $
                       all_ (migrationDbMigrations migrationDb)

         case alreadyRun of
           Left err -> hPutStrLn stderr ("Could not run query: " ++ show err)
           Right alreadyRun  ->
             do let status = migrationStatus allMigrationNames alreadyRun
                    lastMigrationNumber = maximum (migrationNumber <$> migrationsStatusApplied status)
                    migrations = map (\m -> (MigrationApplied, migrationNumber m, migrationName m, Just (migrationRanAt m))) (migrationsStatusApplied status) ++
                                 case migrationsStatusFutureStatus status of
                                   MigrationsFutureStatusUpToDate -> []
                                   MigrationsFutureStatusNotYet nms ->
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

                let tbl = Table (Group SingleLine ([] :: [Header ()]))
                                (Group SingleLine [ Header "Number"
                                                  , Header "Name"
                                                  , Header "Ran At"
                                                  , Header "Status" ])
                                (map (\(sts, num, nm, ranAt) -> [show num, T.unpack nm, show ranAt, statusText sts]) migrations)

                    statusText MigrationDiverged = "Diverged"
                    statusText MigrationApplied  = "Applied"
                    statusText MigrationUnknown  = "Unknown"
                    statusText MigrationScheduled = "Scheduled"

                putStrLn ("Status is: " ++ show status)

                putStrLn $ render " " (const "") id id tbl



--    Up ->
--      do ensureMigrationsTable
