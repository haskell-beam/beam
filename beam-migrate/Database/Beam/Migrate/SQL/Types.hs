{-# LANGUAGE ConstraintKinds #-}

-- | Some common SQL data types
module Database.Beam.Migrate.SQL.Types
  ( TableSchema, TableFieldSchema(..)
  , FieldSchema(..), DataType(..)

  , BeamMigrateOnlySqlBackend
  , BeamMigrateSqlBackend
  , BeamMigrateSql99Backend
  , BeamMigrateSqlT021Backend
  , BeamMigrateSqlT071Backend
  , BeamSqlBackendConstraintSyntax
  , BeamSqlBackendColumnConstraintDefinitionSyntax
  , BeamSqlBackendDataTypeSyntax
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

-- | A data type in a given 'IsSql92DataTypeSyntax' which describes a SQL type
-- mapping to the Haskell type @a@
newtype DataType be a = DataType (BeamSqlBackendDataTypeSyntax be)

instance BeamMigrateOnlySqlBackend be => Show (DataType be a) where
  show (DataType syntax) = "DataType (" ++ displaySyntax syntax ++ ")"

instance BeamMigrateOnlySqlBackend be => Eq (DataType be a) where
  DataType a == DataType b = a == b

class ( IsSql92DdlCommandSyntax (BeamSqlBackendSyntax be)
      , Sql92SaneDdlCommandSyntaxMigrateOnly (BeamSqlBackendSyntax be)

      , Sql92DisplaySyntax (BeamSqlBackendDataTypeSyntax be)
      , Eq (BeamSqlBackendDataTypeSyntax be)
      , Typeable (BeamSqlBackendDataTypeSyntax be)
      , Sql92SerializableDataTypeSyntax (BeamSqlBackendDataTypeSyntax be)

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
type BeamMigrateSqlT021Backend be =
  ( BeamMigrateSqlBackend be
  , IsSql2003BinaryAndVarBinaryDataTypeSyntax (BeamSqlBackendDataTypeSyntax be) )
type BeamMigrateSqlT071Backend be =
  ( BeamMigrateSqlBackend be
  , IsSql2008BigIntDataTypeSyntax (BeamSqlBackendDataTypeSyntax be) )

type BeamSqlBackendConstraintSyntax be
  = Sql92DdlCommandColumnConstraintSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendColumnConstraintDefinitionSyntax be
  = Sql92DdlCommandConstraintDefinitionSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendDataTypeSyntax be
  = Sql92ColumnSchemaColumnTypeSyntax (BeamSqlBackendColumnSchemaSyntax be)
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
