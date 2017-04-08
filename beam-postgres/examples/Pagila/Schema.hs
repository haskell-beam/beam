module Pagila.Schema
  ( module Pagila.Schema.V0002
  , migration, db ) where

import Pagila.Schema.V0002 hiding (migration)

import qualified Pagila.Schema.V0001 as V0001 (migration)
import qualified Pagila.Schema.V0002 as V0002 (migration)

import Control.Arrow

import Database.Beam (DatabaseSettings)
import Database.Beam.Migrate.Types ( CheckedDatabaseSettings, MigrationSteps, unCheckDatabase
                                   , evaluateDatabase, migrationStep)
import Database.Beam.Postgres (Postgres, PgCommandSyntax)

migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres PagilaDb)
migration = migrationStep "Initial commit" V0001.migration >>>
            migrationStep "Add film actor, inventory, rental table" V0002.migration

db :: DatabaseSettings Postgres PagilaDb
db = unCheckDatabase (evaluateDatabase migration)
