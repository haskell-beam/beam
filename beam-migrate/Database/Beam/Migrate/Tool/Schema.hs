module Database.Beam.Migrate.Tool.Schema
  ( migration, migrationDb

  , module V0001 ) where

import Database.Beam.Migrate.SQL
import Database.Beam.Migrate.Types

import qualified Database.Beam.Migrate.Tool.Schema.V0001 as V0001

migration :: IsSql92DdlCommandSyntax syntax =>
             MigrationSteps syntax (DatabaseSettings V0001.MigrationDb)
migration = migrationStep "Initial beam migration tables" V0001.migration

migrationDb :: DatabaseSettings V0001.MigrationDb
migrationDb = evaluateDatabase migration
