{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Database.Beam.Migrate.Tool.Types where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.SQL
import           Database.Beam.Migrate.Types (MigrationSteps, CheckedDatabaseSettings, SomeDatabasePredicate)

import           Control.Exception (Exception)

import qualified Data.ByteString.Lazy as BL
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

data MigrationsStatus tbl
  = MigrationsStatus
  { migrationsStatusApplied :: [ (tbl, Bool) ]
  , migrationsStatusFutureStatus :: MigrationsFutureStatus tbl }
  deriving Show
data MigrationsFutureStatus tbl
  = MigrationsFutureStatusUpToDate
  | MigrationsFutureStatusNotYet Int [ Text ]
  | MigrationsStatusDiverged LocalTime [ Text ] [ tbl ]
  deriving Show

data MigrationStatus
  = MigrationApplied
  | MigrationIncomplete
  | MigrationScheduled
  | MigrationUnknown
  | MigrationDiverged
  deriving (Show, Eq, Ord, Bounded, Enum)

class IsMigration migration where
  migrationName  :: migration -> Text
  migrationRanAt :: migration -> Maybe LocalTime
  migrationStartedAt :: migration -> LocalTime

-- | Given a list of all migrations, and the current list of migrations run,
--   calculate an appropriate 'MigrationStatus'
migrationStatus :: IsMigration tbl
                => Int    {-^ Migration number of first migration -}
                -> [Text] {-^ All available migrations, in order -}
                -> [tbl]  {-^ Migrations that have already been run -}
                -> MigrationsStatus tbl
migrationStatus  _ [] [] = MigrationsStatus [] MigrationsFutureStatusUpToDate
migrationStatus !i names [] = MigrationsStatus [] (MigrationsFutureStatusNotYet i names)
migrationStatus  _ [] run = MigrationsStatus [] (MigrationsStatusDiverged (migrationStartedAt (head run)) [] run)
migrationStatus  i (name:names) (m:ms)
  | name == migrationName m = let sts = migrationStatus (i + 1) names ms
                                  complete = isJust (migrationRanAt m)
                              in sts { migrationsStatusApplied = (m, complete):migrationsStatusApplied sts }
  | otherwise = MigrationsStatus [] (MigrationsStatusDiverged (migrationStartedAt m) (name:names) (m:ms))

