module Database.Beam.Migrate.Tool.Schema
  ( migration, migrationDb
  , migrationToolSchemaMigration
  , V0001.MigrationDb(..)
  , V0001.MigrationT(..)
  , V0001.MigrationTable

  , module V0001 ) where

import Database.Beam
import Database.Beam.Backend.SQL.Builder
import Database.Beam.Migrate.SQL
import Database.Beam.Migrate.SQL.Builder
import Database.Beam.Migrate.Types

import qualified Database.Beam.Migrate.Tool.Schema.V0001 as V0001

migrationToolSchemaMigration ::
  IsSql92DdlCommandSyntax syntax =>
  MigrationSteps syntax () (CheckedDatabaseSettings be V0001.MigrationDb)
migrationToolSchemaMigration = migration

migration :: IsSql92DdlCommandSyntax syntax =>
             MigrationSteps syntax () (CheckedDatabaseSettings be V0001.MigrationDb)
migration = migrationStep "Initial beam migration tables" V0001.migration

migrationDb :: forall be. DatabaseSettings be V0001.MigrationDb
migrationDb = unCheckDatabase (evaluateDatabase (migration :: MigrationSteps SqlSyntaxBuilder () (CheckedDatabaseSettings be V0001.MigrationDb)))
