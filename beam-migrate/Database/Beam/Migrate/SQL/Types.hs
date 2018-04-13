{-# LANGUAGE ConstraintKinds #-}

-- | Some common SQL data types
module Database.Beam.Migrate.SQL.Types
  ( TableSchema, TableFieldSchema(..)
  , FieldSchema(..), DataType(..)

  , BeamMigrateSqlBackend
  , BeamMigrateSql2003Backend
  , BeamMigrateSql2008Backend
  , BeamSqlBackendConstraintSyntax
  , BeamSqlBackendColumnConstraintDefinitionSyntax
  , BeamSqlBackendDataTypeSyntax
  , BeamSqlBackendColumnSchemaSyntax
  ) where

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Backend.SQL

import Data.Text (Text)

-- | A table schema, produced by 'createTable'
type TableSchema be tbl =
    tbl (TableFieldSchema be)

-- | A schema for a field within a given table
data TableFieldSchema be a
    = TableFieldSchema Text (FieldSchema be a) [FieldCheck]

-- | A schema for a field which hasn't been named yet
newtype FieldSchema be a = FieldSchema (BeamSqlBackendColumnSchemaSyntax be)
deriving instance BeamMigrateSqlBackend be => Eq (FieldSchema be a)

-- | A data type in a given 'IsSql92DataTypeSyntax' which describes a SQL type
-- mapping to the Haskell type @a@
newtype DataType be a = DataType (BeamSqlBackendDataTypeSyntax be)

instance BeamMigrateSqlBackend be => Show (DataType be a) where
  show (DataType syntax) = "DataType (" ++ displaySyntax syntax ++ ")"

instance BeamMigrateSqlBackend be => Eq (DataType be a) where
  DataType a == DataType b = a == b

class ( IsSql92DdlCommandSyntax (BeamSqlBackendSyntax be)
      , Sql92SaneDdlCommandSyntax (BeamSqlBackendSyntax be)

      , Sql92DisplaySyntax (BeamSqlBackendDataTypeSyntax be)
      , Eq (BeamSqlBackendDataTypeSyntax be)

      , BeamSqlBackend be ) => BeamMigrateSqlBackend be

type BeamMigrateSql2003Backend be =
  IsSql2003BinaryAndVarBinaryDataTypeSyntax (BeamSqlBackendDataTypeSyntax be)
type BeamMigrateSql2008Backend be =
  IsSql2008BigIntDataTypeSyntax (BeamSqlBackendDataTypeSyntax be)

type BeamSqlBackendConstraintSyntax be
  = Sql92DdlCommandColumnConstraintSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendColumnConstraintDefinitionSyntax be
  = Sql92DdlCommandConstraintDefinitionSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendDataTypeSyntax be
  = Sql92ColumnSchemaColumnTypeSyntax (BeamSqlBackendColumnSchemaSyntax be)
type BeamSqlBackendColumnSchemaSyntax be
  = Sql92DdlCommandColumnSchemaSyntax (BeamSqlBackendSyntax be)
