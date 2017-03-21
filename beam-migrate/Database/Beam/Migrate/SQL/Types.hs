module Database.Beam.Migrate.SQL.Types where

import Database.Beam

import Data.Text (Text)

type TableSchema fieldSchemaSyntax tbl =
    tbl (TableFieldSchema fieldSchemaSyntax tbl)

data TableFieldSchema fieldSchemaSyntax (table :: (* -> *) -> *) a
    = TableFieldSchema Text fieldSchemaSyntax
      deriving (Show, Eq)


