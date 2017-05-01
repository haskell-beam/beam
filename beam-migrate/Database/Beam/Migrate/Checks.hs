{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines common database checks that can be asserted against a database
module Database.Beam.Migrate.Checks where

import           Database.Beam.Migrate.Types.Predicates
import           Database.Beam.Migrate.SQL.SQL92

import           Data.Hashable
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import           Data.Typeable (Typeable, cast)

import           GHC.Generics

-- * Table checks

data TableExistsPredicate = TableExistsPredicate Text {- Table name -}
  deriving (Show, Eq, Ord, Typeable, Generic)
instance Hashable TableExistsPredicate
instance DatabasePredicate TableExistsPredicate where
  englishDescription (TableExistsPredicate t) =
    "Table " <> show t <> " must exist"

data TableHasColumn syntax where
  TableHasColumn
    :: Typeable (Sql92ColumnSchemaColumnTypeSyntax syntax)
    => Text {- Table name -}
    -> Text {- Column name -}
    -> Sql92ColumnSchemaColumnTypeSyntax syntax {- Data type -}
    -> TableHasColumn syntax
instance Hashable (Sql92ColumnSchemaColumnTypeSyntax syntax) => Hashable (TableHasColumn syntax) where
  hashWithSalt salt (TableHasColumn t c s) = hashWithSalt salt (t, c, s)
deriving instance Show (Sql92ColumnSchemaColumnTypeSyntax syntax) => Show (TableHasColumn syntax)
deriving instance Eq (Sql92ColumnSchemaColumnTypeSyntax syntax) => Eq (TableHasColumn syntax)
instance ( Typeable syntax
         , Hashable (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Show (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Eq (Sql92ColumnSchemaColumnTypeSyntax syntax) ) =>
  DatabasePredicate (TableHasColumn syntax) where
  englishDescription (TableHasColumn tbl col type_) =
    "Table " <> show tbl <> " must have a column " <> show col <> " of " <> show type_
  predicateCascadesDropOn (TableHasColumn tblNm _ _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

data TableColumnHasConstraint syntax
  = TableColumnHasConstraint Text {- Table name -} Text {- Column name -}
                             (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
  deriving Generic
instance Hashable (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) => Hashable (TableColumnHasConstraint syntax)
deriving instance Show (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) => Show (TableColumnHasConstraint syntax)
deriving instance Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) => Eq (TableColumnHasConstraint syntax)
instance ( Typeable syntax
         , Hashable (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Show (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) ) =>
         DatabasePredicate (TableColumnHasConstraint syntax) where
  englishDescription (TableColumnHasConstraint tbl col cns) =
    "Column " <> show tbl <> "." <> show col <> " has constraint " <> show cns
  predicateCascadesDropOn (TableColumnHasConstraint tblNm colNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | Just (TableHasColumn tblNm' colNm' _ :: TableHasColumn syntax) <- cast p' = tblNm' == tblNm && colNm' == colNm
    | otherwise = False

data TableHasPrimaryKey
  = TableHasPrimaryKey Text {- Table name -}
                       [Text] {- Column names -}
  deriving (Show, Eq, Generic)
instance Hashable TableHasPrimaryKey
instance DatabasePredicate TableHasPrimaryKey where
  englishDescription (TableHasPrimaryKey tblName colNames) =
    "Table " <> show tblName <> " has primary key " <> show colNames
  predicateCascadesDropOn (TableHasPrimaryKey tblNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False
