{-# LANGUAGE ConstraintKinds #-}

-- | Some common SQL data types
module Database.Beam.Migrate.SQL.Types
  ( TableSchema, TableFieldSchema(..)
  , FieldSchema(..)

  , BeamMigrateOnlySqlBackend
  , BeamMigrateSqlBackend
  , BeamMigrateSql99Backend
  , BeamSqlBackendConstraintSyntax
  , BeamSqlBackendColumnConstraintDefinitionSyntax
  , BeamMigrateSqlBackendDataTypeSyntax
  , BeamSqlBackendColumnSchemaSyntax
  , BeamSqlBackendAlterTableSyntax
  , BeamSqlBackendMatchTypeSyntax
  , BeamSqlBackendReferentialActionSyntax
  , BeamSqlBackendConstraintAttributesSyntax
  ) where

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Backend.SQL

import Data.Text (Text)
import Data.Typeable (Typeable)

-- | A table schema, produced by 'createTable'
type TableSchema be tbl =
    tbl (TableFieldSchema be)

-- | A schema for a field within a given table
data TableFieldSchema be a
    = TableFieldSchema Text (FieldSchema be a) [FieldCheck]

-- | A schema for a field which hasn't been named yet
newtype FieldSchema be a = FieldSchema (BeamSqlBackendColumnSchemaSyntax be)
deriving instance BeamMigrateOnlySqlBackend be => Eq (FieldSchema be a)

class ( Typeable (BeamSqlBackendSyntax be)
      , IsSql92DdlCommandSyntax (BeamSqlBackendSyntax be)
      , Sql92SaneDdlCommandSyntaxMigrateOnly (BeamSqlBackendSyntax be)

      , Sql92DisplaySyntax (BeamMigrateSqlBackendDataTypeSyntax be)
      , Eq (BeamMigrateSqlBackendDataTypeSyntax be)
      , Typeable (BeamMigrateSqlBackendDataTypeSyntax be)
      , Sql92SerializableDataTypeSyntax (BeamMigrateSqlBackendDataTypeSyntax be)

      , Sql92SerializableConstraintDefinitionSyntax (BeamSqlBackendColumnConstraintDefinitionSyntax be)
      , Sql92DisplaySyntax (BeamSqlBackendColumnConstraintDefinitionSyntax be)
      , Eq (BeamSqlBackendColumnConstraintDefinitionSyntax be)

      , Typeable be
      ) => BeamMigrateOnlySqlBackend be

type BeamMigrateSqlBackend be =
    ( BeamMigrateOnlySqlBackend be
    , Sql92SaneDdlCommandSyntax (BeamSqlBackendSyntax be)
    , BeamSqlBackend be )

type BeamMigrateSql99Backend be =
  ( BeamMigrateSqlBackend be
  , IsSql99DataTypeSyntax (BeamSqlBackendDataTypeSyntax be))

type BeamSqlBackendConstraintSyntax be
  = Sql92DdlCommandColumnConstraintSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendColumnConstraintDefinitionSyntax be
  = Sql92DdlCommandConstraintDefinitionSyntax (BeamSqlBackendSyntax be)
type BeamMigrateSqlBackendDataTypeSyntax be
  = Sql92DdlCommandDataTypeSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendColumnSchemaSyntax be
  = Sql92DdlCommandColumnSchemaSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendAlterTableSyntax be
  = Sql92DdlCommandAlterTableSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendMatchTypeSyntax be
  = Sql92DdlCommandMatchTypeSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendReferentialActionSyntax be
  = Sql92DdlCommandReferentialActionSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendConstraintAttributesSyntax be
  = Sql92DdlCommandConstraintAttributesSyntax (BeamSqlBackendSyntax be)
