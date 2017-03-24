{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Beam.Migrate.Tool where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.SQL
import           Database.Beam.Migrate.Tool.Schema hiding (migration)
import           Database.Beam.Migrate.Types ( MigrationStep(..), MigrationF(..), Migration
                                             , MigrationSteps, stepNames)

import           Control.Monad
import           Control.Monad.Free.Church

import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (LocalTime)

import           Options.Applicative

import           GHC.Generics

import           System.Directory
import           System.FilePath
import           System.IO

--import Options.Applicative

-- * Options type

data BeamMigrationCommand beOptions
  = BeamMigrationCommand
  { migrationCommandBackend        :: String
  , migrationCommandMigrationModule :: String
  , migrationCommandBackendOptions :: beOptions }
  deriving Show

data BeamMigrationSubcommand
  = WriteScript
  | ListMigrations
  | Init | Status
  | Up
  deriving Show

data DdlError
  = DdlError String
  deriving Show

data BeamMigrationBackend commandSyntax beOptions where
  BeamMigrationBackend :: ( Show beOptions, MonadBeam commandSyntax be m
                          , Sql92ReasonableMarshaller be ) =>
                       { backendOptsParser :: Parser beOptions
                       , backendProxy :: Proxy be
                       , backendRenderSteps :: forall a. MigrationSteps commandSyntax a -> BL.ByteString
                       , backendRenderSyntax :: commandSyntax -> String
                       , backendTransact :: forall a. beOptions -> m a -> IO (Either DdlError a)
                       } -> BeamMigrationBackend commandSyntax beOptions
data SomeBeamMigrationBackend where
  SomeBeamMigrationBackend :: ( Typeable commandSyntax, Typeable beOptions
                              , IsSql92DdlCommandSyntax commandSyntax
                              , IsSql92Syntax commandSyntax
                              , Sql92SanityCheck commandSyntax ) =>
                              BeamMigrationBackend commandSyntax beOptions
                           -> SomeBeamMigrationBackend

migrationCommandArgParser :: Parser beOptions -> Parser (BeamMigrationCommand beOptions)
migrationCommandArgParser beOptions =
  BeamMigrationCommand <$> strOption (long "backend" <> metavar "BACKEND" <> help "Module to load for migration backend. Must be given first")
                       <*> strOption (long "migration" <> short 'M' <> metavar "MIGRATIONMODULE" <> help "Module containing migration steps")
                       <*> beOptions

subcommandParser :: Parser BeamMigrationSubcommand
subcommandParser = subparser (command "write-script" writeScriptCommand <> help "Write the entire set of migrations as one script") <|>
                   subparser (command "list-migrations" listMigrationsCommand <> help "List all discovered migrations") <|>
                   subparser (command "init" initCommand <> help "Initialize the database for running migrations") <|>
                   subparser (command "status" statusCommand <> help "Display status of applied migrations")
  where
    writeScriptCommand =
      info (writeScriptArgParser <**> helper) (fullDesc <> progDesc "Write a migration script for the given migrations")
    listMigrationsCommand =
      info (listMigrationsArgParser <**> helper) (fullDesc <> progDesc "List all discovered migrations")
    initCommand =
      info (initArgParser <**> helper) (fullDesc <> progDesc "Initialize the given database for running migrations")
    statusCommand =
      info (statusArgParser <**> helper) (fullDesc <> progDesc "Display status of applied migrations")

    writeScriptArgParser    = pure WriteScript
    listMigrationsArgParser = pure ListMigrations
    initArgParser = pure Init
    statusArgParser = pure Status

migrationCommandOptions :: Parser beOptions -> ParserInfo (BeamMigrationCommand beOptions)
migrationCommandOptions beOptions =
  info (migrationCommandArgParser beOptions <**> helper)
       (fullDesc <> progDesc "Beam schema migration tool" <> header "beam-migrate -- migrate database schemas for various beam backends")

migrationCommandAndSubcommandOptions :: Parser beOptions -> ParserInfo (BeamMigrationCommand beOptions, BeamMigrationSubcommand)
migrationCommandAndSubcommandOptions beOptions =
  info (((,) <$> migrationCommandArgParser beOptions <*> subcommandParser) <**> helper)
       (fullDesc <> progDesc "Beam schema migration tool" <> header "beam-migrate -- migrate database schemas for various beam backends")

-- * Migration table creation script

migrationStepsToMigration :: MigrationSteps syntax a -> Migration syntax a
migrationStepsToMigration steps = runF steps finish step
  where finish x = pure x
        step (MigrationStep name doStep next) = doStep >>= next

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
    do err <- backendTransact opts $ do
         runMigrationSteps backendRenderSyntax (migrationStepsToMigration migrationToolSchemaMigration)
       case err of
         Left err -> hPutStrLn stderr ("Error writing migrations table: " ++ show err)
         Right _ -> pure ()

-- * Tool entry point

data MigrationsStatus
  = MigrationsStatus
  { migrationsStatusApplied :: [ (Text, LocalTime) ]
  , migrationsStatusFutureStatus :: MigrationsFutureStatus }
  deriving Show
data MigrationsFutureStatus
  = MigrationsFutureStatusUpToDate
  | MigrationsFutureStatusNotYet [ Text ]
  | MigrationsStatusDiverged LocalTime [ Text ] [ (Text, LocalTime) ]
  deriving Show

invokeMigrationTool :: (IsSql92DdlCommandSyntax cmdSyntax, IsSql92Syntax cmdSyntax, Sql92SanityCheck cmdSyntax ) =>
                       BeamMigrationBackend cmdSyntax beOptions -> BeamMigrationCommand beOptions
                    -> BeamMigrationSubcommand -> MigrationSteps cmdSyntax () -> IO ()
invokeMigrationTool be@(BeamMigrationBackend {..}) BeamMigrationCommand {..}  subcommand steps =
  case subcommand of
    ListMigrations ->
      do putStrLn "Registered migrations:"
         mapM_ (putStrLn . ("  - " ++) . T.unpack) (stepNames steps)
    WriteScript ->
      do BL.putStrLn (backendRenderSteps steps)
    Init ->
      do writeMigrationsTable be migrationCommandBackendOptions
    Status ->
      do let allMigrationNames = stepNames steps

         alreadyRun <- backendTransact migrationCommandBackendOptions $ do
           runSelectReturningList (select (orderBy_ (\m -> asc_ (migrationRanAt m)) (all_ (migrationDbMigrations migrationDb))))

         let migrationStatus [] [] = MigrationsStatus [] MigrationsFutureStatusUpToDate
             migrationStatus names [] = MigrationsStatus [] (MigrationsFutureStatusNotYet names)
             migrationStatus [] run = MigrationsStatus [] (MigrationsStatusDiverged (migrationRanAt (head run)) [] (map migrationDescription run))
             migrationStatus (name:names) (m:ms)
               | name == migrationName m = let sts = migrationStatus names ms
                                           in sts { migrationsStatusApplied = migrationDescription m:migrationsStatusApplied sts }
               | otherwise = MigrationsStatus [] (MigrationsStatusDiverged (migrationRanAt m) (name:names) (map migrationDescription (m:ms)))

             migrationDescription m = (migrationName m, migrationRanAt m)

         case alreadyRun of
           Left err -> hPutStrLn stderr ("Could not run query: " ++ show err)
           Right alreadyRun  -> putStrLn (show $ migrationStatus allMigrationNames alreadyRun)
--    Up ->
--      do ensureMigrationsTable
