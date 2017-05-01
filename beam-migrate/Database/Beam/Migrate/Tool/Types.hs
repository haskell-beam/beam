{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Database.Beam.Migrate.Tool.Types where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.SQL
import           Database.Beam.Migrate.Tool.Schema
import           Database.Beam.Migrate.Types (MigrationSteps, CheckedDatabaseSettings, SomeDatabasePredicate)
import           Database.Beam.Query

import           Control.Exception (Exception)

import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import           Data.Monoid
import           Data.Proxy
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Data.Time (LocalTime)

import           Options.Applicative

-- * Options type

data BeamMigrateBackendError = BeamMigrateBackendError DdlError
  deriving (Show,  Typeable)
instance Exception BeamMigrateBackendError
data MigrationsDiverged = MigrationsDiverged
  deriving (Show, Typeable)
instance Exception MigrationsDiverged

type BeamMigrateRunTransaction m options =
  forall a. options -> m a -> IO (Either String a)

data BeamMigrationCommand beOptions
  = BeamMigrationCommand
  { migrationCommandBackend         :: String
  , migrationCommandMigrationModule :: Maybe String
  , migrationCommandPackagePath     :: [String]
  , migrationCommandBackendOptions  :: beOptions }
  deriving Show

data DiffDirection
  = DiffMigrationToDb | DiffDbToMigration
  deriving Show

data BeamMigrationSubcommand
  = WriteScript
  | ListMigrations
  | Init | Status
  | Migrate (Maybe Int)

  | Diff Bool {- Interactive -} DiffDirection

  deriving Show

data DdlError
  = DdlNoSuchTable Text
  | DdlCustomError String
  deriving Show

data SomeCheckedDatabase be where
  SomeCheckedDatabase :: Database db
                      => CheckedDatabaseSettings be db
                      -> SomeCheckedDatabase be

data BeamMigrationBackend be commandSyntax beOptions where
  BeamMigrationBackend :: ( Show beOptions, MonadBeam commandSyntax be hdl m
                          , Typeable be
                          , HasQBuilder (Sql92SelectSyntax commandSyntax)
                          , HasSqlValueSyntax (Sql92ValueSyntax commandSyntax) LocalTime
                          , HasSqlValueSyntax (Sql92ValueSyntax commandSyntax) (Maybe LocalTime)
                          , HasSqlValueSyntax (Sql92ValueSyntax commandSyntax) Text
                          , HasSqlValueSyntax (Sql92ValueSyntax commandSyntax) SqlNull
                          , Sql92SanityCheck commandSyntax, Sql92SaneDdlCommandSyntax commandSyntax
                          , Sql92ReasonableMarshaller be ) =>
                       { backendOptsParser :: Parser beOptions
                       , backendRenderSteps :: forall a. MigrationSteps commandSyntax () a -> BL.ByteString
                       , backendGetDbConstraints :: beOptions -> IO [ SomeDatabasePredicate ]
                       , backendRenderSyntax :: commandSyntax -> String
                       , backendTransact :: forall a. beOptions -> m a -> IO (Either DdlError a)
                       } -> BeamMigrationBackend be commandSyntax beOptions
data SomeBeamMigrationBackend where
  SomeBeamMigrationBackend :: ( Typeable commandSyntax, Typeable beOptions
                              , IsSql92DdlCommandSyntax commandSyntax
                              , IsSql92Syntax commandSyntax
                              , Sql92SanityCheck commandSyntax ) =>
                              BeamMigrationBackend be commandSyntax beOptions
                           -> SomeBeamMigrationBackend

-- * Migrations status

data MigrationsStatus
  = MigrationsStatus
  { migrationsStatusApplied :: [ (MigrationTable, Bool) ]
  , migrationsStatusFutureStatus :: MigrationsFutureStatus }
  deriving Show
data MigrationsFutureStatus
  = MigrationsFutureStatusUpToDate
  | MigrationsFutureStatusNotYet Int [ Text ]
  | MigrationsStatusDiverged LocalTime [ Text ] [ MigrationTable ]
  deriving Show

data MigrationStatus
  = MigrationApplied
  | MigrationIncomplete
  | MigrationScheduled
  | MigrationUnknown
  | MigrationDiverged
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Given a list of all migrations, and the current list of migrations run,
--   calculate an appropriate 'MigrationStatus'
migrationStatus :: Int              {-^ Migration number of first migration -}
                -> [Text]           {-^ All available migrations, in order -}
                -> [MigrationTable] {-^ Migrations that have already been run -}
                -> MigrationsStatus
migrationStatus  _ [] [] = MigrationsStatus [] MigrationsFutureStatusUpToDate
migrationStatus !i names [] = MigrationsStatus [] (MigrationsFutureStatusNotYet i names)
migrationStatus  _ [] run = MigrationsStatus [] (MigrationsStatusDiverged (migrationStartedAt (head run)) [] run)
migrationStatus  i (name:names) (m:ms)
  | name == migrationName m = let sts = migrationStatus (i + 1) names ms
                                  complete = isJust (migrationRanAt m)
                              in sts { migrationsStatusApplied = (m, complete):migrationsStatusApplied sts }
  | otherwise = MigrationsStatus [] (MigrationsStatusDiverged (migrationStartedAt m) (name:names) (m:ms))


-- * Options parsing

migrationCommandArgParser :: Parser beOptions -> Parser (BeamMigrationCommand beOptions)
migrationCommandArgParser beOptions =
  BeamMigrationCommand <$> strOption (long "backend" <> metavar "BACKEND" <> help "Module to load for migration backend. Must be given first")
                       <*> optional (strOption (long "migration" <> short 'M' <> metavar "MIGRATIONMODULE" <> help "Module containing migration steps"))
                       <*> many (strOption (long "package-path" <> short 'P' <> metavar "PACKAGEPATH" <> help "Additional GHC package paths"))
                       <*> beOptions

subcommandParser :: Parser BeamMigrationSubcommand
subcommandParser = subparser $
                   mconcat [ command "write-script" writeScriptCommand
                           , command "list-migrations" listMigrationsCommand
                           , command "init" initCommand
                           , command "status" statusCommand
                           , command "migrate" migrateCommand
                           , command "diff" diffCommand]
  where
    writeScriptCommand =
      info (writeScriptArgParser <**> helper) (fullDesc <> progDesc "Write a migration script for the given migrations")
    listMigrationsCommand =
      info (listMigrationsArgParser <**> helper) (fullDesc <> progDesc "List all discovered migrations")
    initCommand =
      info (initArgParser <**> helper) (fullDesc <> progDesc "Initialize the given database for running migrations")
    statusCommand =
      info (statusArgParser <**> helper) (fullDesc <> progDesc "Display status of applied migrations")
    migrateCommand =
      info (migrateArgParser <**> helper) (fullDesc <> progDesc "Run necessary migrations")
    diffCommand = info (diffArgParser <**> helper) (fullDesc <> progDesc "Diff against the database/schema")

    writeScriptArgParser    = pure WriteScript
    listMigrationsArgParser = pure ListMigrations
    initArgParser = pure Init
    statusArgParser = pure Status
    migrateArgParser = Migrate <$> optional (option auto (metavar "UNTIL" <> help "Last migration to run"))
    diffArgParser = Diff <$> (switch (long "interactive" <> short 'i' <> help "Enable interactive mode"))
                         <*> ((\b -> if b then DiffMigrationToDb else DiffDbToMigration) <$>
                              switch (long "to-db" <> help "Compute the diff as though the current database schema is the target, rather than the Haskell schema"))

migrationCommandOptions :: Parser beOptions -> ParserInfo (BeamMigrationCommand beOptions)
migrationCommandOptions beOptions =
  info (migrationCommandArgParser beOptions <**> helper)
       (fullDesc <> progDesc "Beam schema migration tool" <> header "beam-migrate -- migrate database schemas for various beam backends")

migrationCommandAndSubcommandOptions :: Parser beOptions -> ParserInfo (BeamMigrationCommand beOptions, Maybe BeamMigrationSubcommand)
migrationCommandAndSubcommandOptions beOptions =
  info (((,) <$> migrationCommandArgParser beOptions <*> optional (subcommandParser)) <**> helper)
       (fullDesc <> progDesc "Beam schema migration tool" <> header "beam-migrate -- migrate database schemas for various beam backends")
