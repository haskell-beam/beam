-- | Some common SQL data types
module Database.Beam.Migrate.SQL.Types
  ( TableSchema, TableFieldSchema(..)
  , FieldSchema(..), DataType(..)
  ) where

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92

import Data.Text (Text)

-- | A table schema, produced by 'createTable'
type TableSchema fieldSchemaSyntax tbl =
    tbl (TableFieldSchema fieldSchemaSyntax)

-- | A schema for a field within a given table
data TableFieldSchema fieldSchemaSyntax a
    = TableFieldSchema Text (FieldSchema fieldSchemaSyntax a) [FieldCheck]

-- | A schema for a field which hasn't been named yet
newtype FieldSchema syntax a = FieldSchema syntax
  deriving (Show, Eq)

-- | A data type in a given 'IsSql92DataTypeSyntax' which describes a SQL type
-- mapping to the Haskell type @a@
newtype DataType syntax a = DataType syntax

instance Sql92DisplaySyntax syntax => Show (DataType syntax a) where
  show (DataType syntax) = "DataType (" ++ displaySyntax syntax ++ ")"

instance Eq syntax => Eq (DataType syntax a) where
  DataType a == DataType b = a == b
