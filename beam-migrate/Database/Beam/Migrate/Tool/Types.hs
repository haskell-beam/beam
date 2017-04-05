{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Beam.Migrate.Tool.Types where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.SQL
import           Database.Beam.Migrate.Tool.Schema
import           Database.Beam.Migrate.Types (MigrationSteps)

import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import           Data.Time (LocalTime)

import           Options.Applicative

-- * Options type

data BeamMigrationCommand beOptions
  = BeamMigrationCommand
  { migrationCommandBackend        :: String
  , migrationCommandMigrationModule :: String
  , migrationCommandPackagePath :: [String]
  , migrationCommandBackendOptions :: beOptions }
  deriving Show

data BeamMigrationSubcommand
  = WriteScript
  | ListMigrations
  | Init | Status
  | Up
  deriving Show

data DdlError
  = DdlNoSuchTable Text
  | DdlCustomError String
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

-- * Migrations status

data MigrationsStatus
  = MigrationsStatus
  { migrationsStatusApplied :: [ MigrationTable ]
  , migrationsStatusFutureStatus :: MigrationsFutureStatus }
  deriving Show
data MigrationsFutureStatus
  = MigrationsFutureStatusUpToDate
  | MigrationsFutureStatusNotYet [ Text ]
  | MigrationsStatusDiverged LocalTime [ Text ] [ MigrationTable ]
  deriving Show

data MigrationStatus
  = MigrationApplied
  | MigrationScheduled
  | MigrationUnknown
  | MigrationDiverged
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Given a list of all migrations, and the current list of migrations run,
--   calculate an appropriate 'MigrationStatus'
migrationStatus :: [Text]           {-^ All available migrations, in order -}
                -> [MigrationTable] {-^ Migrations that have already been run -}
                -> MigrationsStatus
migrationStatus [] [] = MigrationsStatus [] MigrationsFutureStatusUpToDate
migrationStatus names [] = MigrationsStatus [] (MigrationsFutureStatusNotYet names)
migrationStatus [] run = MigrationsStatus [] (MigrationsStatusDiverged (migrationRanAt (head run)) [] run)
migrationStatus (name:names) (m:ms)
  | name == migrationName m = let sts = migrationStatus names ms
                              in sts { migrationsStatusApplied = m:migrationsStatusApplied sts }
  | otherwise = MigrationsStatus [] (MigrationsStatusDiverged (migrationRanAt m) (name:names) (m:ms))


-- * Options parsing

migrationCommandArgParser :: Parser beOptions -> Parser (BeamMigrationCommand beOptions)
migrationCommandArgParser beOptions =
  BeamMigrationCommand <$> strOption (long "backend" <> metavar "BACKEND" <> help "Module to load for migration backend. Must be given first")
                       <*> strOption (long "migration" <> short 'M' <> metavar "MIGRATIONMODULE" <> help "Module containing migration steps")
                       <*> many (strOption (long "package-path" <> short 'P' <> metavar "PACKAGEPATH" <> help "Additional GHC package paths"))
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
