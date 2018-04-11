{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

-- | Defines common 'DatabasePredicate's that are shared among backends
module Database.Beam.Migrate.Checks where

import Database.Beam.Migrate.Serialization
import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92

import Data.Aeson ((.:), (.=), withObject, object)
import Data.Aeson.Types (Parser, Value)
import Data.Hashable (Hashable(..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import GHC.Generics (Generic)


-- * Table checks

-- | Asserts that a table with the given name exists in a database
data TableExistsPredicate = TableExistsPredicate Text {-^ Table name -}
  deriving (Show, Eq, Ord, Typeable, Generic)
instance Hashable TableExistsPredicate
instance DatabasePredicate TableExistsPredicate where
  englishDescription (TableExistsPredicate t) =
    "Table " <> show t <> " must exist"

  serializePredicate (TableExistsPredicate t) =
    object [ "table-exists" .= t ]

  predicateSpecificity _ = PredicateSpecificityAllBackends

-- | Asserts that the table specified has a column with the given data type. The
-- type paramater @syntax@ should be an instance of 'IsSql92ColumnSchemaSyntax'.
data TableHasColumn syntax where
  TableHasColumn
    :: Typeable (Sql92ColumnSchemaColumnTypeSyntax syntax)
    => { hasColumn_table  :: Text {-^ Table name -}
       , hasColumn_column :: Text {-^ Column name -}
       , hasColumn_type   ::  Sql92ColumnSchemaColumnTypeSyntax syntax {-^ Data type -}
       }
    -> TableHasColumn syntax
instance Hashable (Sql92ColumnSchemaColumnTypeSyntax syntax) => Hashable (TableHasColumn syntax) where
  hashWithSalt salt (TableHasColumn t c s) = hashWithSalt salt (t, c, s)
instance Eq (Sql92ColumnSchemaColumnTypeSyntax syntax) => Eq (TableHasColumn syntax) where
  TableHasColumn aTbl aCol aDt == TableHasColumn bTbl bCol bDt =
    aTbl == bTbl && aCol == bCol && aDt == bDt
instance ( Typeable syntax
         , Sql92SerializableDataTypeSyntax (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Hashable (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Sql92DisplaySyntax (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Eq (Sql92ColumnSchemaColumnTypeSyntax syntax) ) =>
  DatabasePredicate (TableHasColumn syntax) where
  englishDescription (TableHasColumn tbl col type_) =
    "Table " <> show tbl <> " must have a column " <> show col <> " of " <> displaySyntax type_

  predicateSpecificity _ = PredicateSpecificityAllBackends

  serializePredicate (TableHasColumn tbl col type_) =
    object [ "has-column" .= object [ "table" .= tbl, "column" .= col
                                    , "type" .= serializeDataType type_ ]]

  predicateCascadesDropOn (TableHasColumn tblNm _ _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

-- | Asserts that a particular column of a table has a given constraint. The
-- @syntax@ type parameter should be an instance of 'IsSql92ColumnSchemaSyntax'
data TableColumnHasConstraint syntax
  = TableColumnHasConstraint
  { hasConstraint_table  :: Text {-^ Table name -}
  , hasConstraint_column :: Text {-^ Column name -}
  , hasConstraint_defn   :: Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax {-^ Constraint definition -}
  } deriving Generic
instance Hashable (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) => Hashable (TableColumnHasConstraint syntax)
deriving instance Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) => Eq (TableColumnHasConstraint syntax)
instance ( Typeable syntax
         , Sql92SerializableConstraintDefinitionSyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Hashable (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Sql92DisplaySyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) ) =>
         DatabasePredicate (TableColumnHasConstraint syntax) where
  englishDescription (TableColumnHasConstraint tbl col cns) =
    "Column " <> show tbl <> "." <> show col <> " has constraint " <> displaySyntax cns

  predicateSpecificity _ = PredicateSpecificityAllBackends
  serializePredicate (TableColumnHasConstraint tbl col cns) =
    object [ "has-column-constraint" .= object [ "table" .= tbl, "column" .= col
                                               , "constraint" .= serializeConstraint cns ] ]

  predicateCascadesDropOn (TableColumnHasConstraint tblNm colNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | Just (TableHasColumn tblNm' colNm' _ :: TableHasColumn syntax) <- cast p' = tblNm' == tblNm && colNm' == colNm
    | otherwise = False

-- | Asserts that the given table has a primary key made of the given columns.
-- The order of the columns is significant.
data TableHasPrimaryKey
  = TableHasPrimaryKey
  { hasPrimaryKey_table :: Text   {-^ Table name -}
  , hasPrimaryKey_cols  :: [Text] {-^ Column names -}
  } deriving (Show, Eq, Generic)
instance Hashable TableHasPrimaryKey
instance DatabasePredicate TableHasPrimaryKey where
  englishDescription (TableHasPrimaryKey tblName colNames) =
    "Table " <> show tblName <> " has primary key " <> show colNames

  predicateSpecificity _ = PredicateSpecificityAllBackends

  serializePredicate (TableHasPrimaryKey tbl cols) =
    object [ "has-primary-key" .= object [ "table" .= tbl
                                         , "columns" .= cols ] ]

  predicateCascadesDropOn (TableHasPrimaryKey tblNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

-- * Deserialization

-- | 'BeamDeserializers' for all the predicates defined in this module
beamCheckDeserializers
  :: forall cmd
   . ( IsSql92DdlCommandSyntax cmd
     , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
     , Sql92SerializableConstraintDefinitionSyntax (Sql92DdlCommandConstraintDefinitionSyntax cmd) )
  => BeamDeserializers cmd
beamCheckDeserializers = mconcat
  [ beamDeserializer (const deserializeTableExistsPredicate)
  , beamDeserializer (const deserializeTableHasPrimaryKeyPredicate)
  , beamDeserializer deserializeTableHasColumnPredicate
  , beamDeserializer deserializeTableColumnHasConstraintPredicate
  ]
  where
    deserializeTableExistsPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableExistsPredicate =
      withObject "TableExistPredicate" $ \v ->
      SomeDatabasePredicate <$> (TableExistsPredicate <$> v .: "table-exists")

    deserializeTableHasPrimaryKeyPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableHasPrimaryKeyPredicate =
      withObject "TableHasPrimaryKey" $ \v ->
      v .: "has-primary-key" >>=
      (withObject "TableHasPrimaryKey" $ \v' ->
       SomeDatabasePredicate <$> (TableHasPrimaryKey <$> v' .: "table" <*> v' .: "columns"))

    deserializeTableHasColumnPredicate :: BeamDeserializers cmd'
                                       -> Value -> Parser SomeDatabasePredicate
    deserializeTableHasColumnPredicate d =
      withObject "TableHasColumn" $ \v ->
      v .: "has-column" >>=
      (withObject "TableHasColumn" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd)))
         (TableHasColumn <$> v' .: "table" <*> v' .: "column"
                         <*> (beamDeserialize d =<< v' .: "type")))

    deserializeTableColumnHasConstraintPredicate :: BeamDeserializers cmd'
                                                 -> Value -> Parser SomeDatabasePredicate
    deserializeTableColumnHasConstraintPredicate d =
      withObject "TableColumnHasConstraint" $ \v ->
      v .: "has-column-constraint" >>=
      (withObject "TableColumnHasConstraint" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd)))
         (TableColumnHasConstraint <$> v' .: "table" <*> v' .: "column"
                                   <*> (beamDeserialize d =<< v' .: "constraint")))
