{-# LANGUAGE UndecidableInstances #-}

-- | Defines common database checks that can be asserted against a database
module Database.Beam.Migrate.Checks where

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.SQL.SQL92

import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Monoid

-- * Table checks

data TableExistsPredicate = TableExistsPredicate Text {-^ Table name -}
  deriving (Show, Eq, Ord, Typeable)
instance DatabasePredicate TableExistsPredicate where
  englishDescription (TableExistsPredicate t) =
    "Table " <> show t <> " must exist"

data TableHasColumn syntax
  = TableHasColumn Text {-^ Table name -}
                   Text {-^ Column name -}
                   (Sql92ColumnSchemaColumnTypeSyntax syntax) {-^ Data type -}
deriving instance Show (Sql92ColumnSchemaColumnTypeSyntax syntax) => Show (TableHasColumn syntax)
deriving instance Eq (Sql92ColumnSchemaColumnTypeSyntax syntax) => Eq (TableHasColumn syntax)
instance ( Typeable syntax
         , Show (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Eq (Sql92ColumnSchemaColumnTypeSyntax syntax) ) =>
  DatabasePredicate (TableHasColumn syntax) where
  englishDescription (TableHasColumn tbl col type_) =
    "Table " <> show tbl <> " must have a column " <> show col <> " of " <> show type_
  dependencies (TableHasColumn tbl _ _) = [ SomeDatabasePredicate (TableExistsPredicate tbl) ]
