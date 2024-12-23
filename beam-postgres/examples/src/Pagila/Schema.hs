{-# LANGUAGE OverloadedStrings #-}
module Pagila.Schema
  ( module Pagila.Schema.V0002
  , migration, migrateDB, dbSettings ) where

import           Database.PostgreSQL.Simple

import Pagila.Schema.V0002 hiding (migration)

import qualified Pagila.Schema.V0001 as V0001 (migration)
import qualified Pagila.Schema.V0002 as V0002 (migration)

import Control.Arrow ( (>>>) )

import Database.Beam (DatabaseSettings)
import Database.Beam.Migrate.Types ( CheckedDatabaseSettings, MigrationSteps, unCheckDatabase
                                   , evaluateDatabase, migrationStep)
import Database.Beam.Postgres (Postgres, runBeamPostgresDebug)
import Database.Beam.Migrate.Simple (BringUpToDateHooks, bringUpToDateWithHooks, defaultUpToDateHooks, runIrreversibleHook)
import qualified Database.Beam.Postgres.Migrate as Pg

migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres Pagila.Schema.V0002.PagilaDb)
migration = migrationStep "Initial commit" V0001.migration >>>
            migrationStep "Add film actor, inventory, rental table" V0002.migration

dbSettings :: DatabaseSettings Postgres Pagila.Schema.V0002.PagilaDb
dbSettings = unCheckDatabase (evaluateDatabase migration)

allowDestructive :: (MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = return True
    }

migrateDB :: Connection -> IO (Maybe (CheckedDatabaseSettings Postgres Pagila.Schema.V0002.PagilaDb))
migrateDB conn =
  runBeamPostgresDebug putStrLn conn
    $ bringUpToDateWithHooks allowDestructive Pg.migrationBackend migration
