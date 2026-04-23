{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines common 'DatabasePredicate's that are shared among backends
module Database.Beam.Migrate.Checks where

import Database.Beam.Backend.SQL (BeamSqlBackendSyntax)
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.Serialization
import Database.Beam.Migrate.Types.Predicates

import Data.Aeson ((.:), (.:?), (.=), withObject, object)
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (Parser, Value)
import Data.Hashable (Hashable(..))
import qualified Data.List.NonEmpty as NE (NonEmpty)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import GHC.Generics (Generic)

-- * Schema checks

-- | Asserts that a schema with the given name exists in a database
data SchemaExistsPredicate = SchemaExistsPredicate Text {-^ Table name -}
  deriving (Show, Eq, Ord, Typeable, Generic)
instance Hashable SchemaExistsPredicate
instance DatabasePredicate SchemaExistsPredicate where
  englishDescription (SchemaExistsPredicate s) =
    "Schema " <> show s <> " must exist"

  serializePredicate (SchemaExistsPredicate s) =
    object [ "schema-exists" .= s ]

  predicateSpecificity _ = PredicateSpecificityAllBackends

-- * Table checks

-- | Asserts that a table with the given name exists in a database
data TableExistsPredicate = TableExistsPredicate QualifiedName {-^ Table name -}
  deriving (Show, Eq, Ord, Typeable, Generic)
instance Hashable TableExistsPredicate
instance DatabasePredicate TableExistsPredicate where
  englishDescription (TableExistsPredicate t) =
    "Table " <> show t <> " must exist"

  serializePredicate (TableExistsPredicate t) =
    object [ "table-exists" .= t ]

  predicateSpecificity _ = PredicateSpecificityAllBackends

-- | A class that can check whether a particular data type is present
-- in a set of preconditions.
class HasDataTypeCreatedCheck dataType where
  dataTypeHasBeenCreated :: dataType -> (forall preCondition. Typeable preCondition => [ preCondition ]) -> Bool

-- | Asserts that the table specified has a column with the given data type. The
-- type paramater @syntax@ should be an instance of 'IsSql92ColumnSchemaSyntax'.
data TableHasColumn be where
  TableHasColumn
    :: ( HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be) )
    => { hasColumn_table  :: QualifiedName {-^ Table name -}
       , hasColumn_column :: Text {-^ Column name -}
       , hasColumn_type   :: BeamMigrateSqlBackendDataTypeSyntax be {-^ Data type -}
       }
    -> TableHasColumn be
instance Hashable (BeamMigrateSqlBackendDataTypeSyntax be) => Hashable (TableHasColumn be) where
  hashWithSalt salt (TableHasColumn t c s) = hashWithSalt salt (t, c, s)
instance Eq (BeamMigrateSqlBackendDataTypeSyntax be) => Eq (TableHasColumn be) where
  TableHasColumn aTbl aCol aDt == TableHasColumn bTbl bCol bDt =
    aTbl == bTbl && aCol == bCol && aDt == bDt
instance ( Typeable be
         , BeamMigrateOnlySqlBackend be
         , Hashable (BeamMigrateSqlBackendDataTypeSyntax be) ) =>
  DatabasePredicate (TableHasColumn be) where
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
data TableColumnHasConstraint be
  = TableColumnHasConstraint
  { hasConstraint_table  :: QualifiedName {-^ Table name -}
  , hasConstraint_column :: Text {-^ Column name -}
  , hasConstraint_defn   :: BeamSqlBackendColumnConstraintDefinitionSyntax be {-^ Constraint definition -}
  } deriving Generic
instance Hashable (BeamSqlBackendColumnConstraintDefinitionSyntax be) => Hashable (TableColumnHasConstraint be)
deriving instance Eq (BeamSqlBackendColumnConstraintDefinitionSyntax be) => Eq (TableColumnHasConstraint be)
instance ( Typeable be, BeamMigrateOnlySqlBackend be
         , Hashable (BeamSqlBackendColumnConstraintDefinitionSyntax be) ) =>
         DatabasePredicate (TableColumnHasConstraint be) where
  englishDescription (TableColumnHasConstraint tbl col cns) =
    "Column " <> show tbl <> "." <> show col <> " has constraint " <> displaySyntax cns

  predicateSpecificity _ = PredicateSpecificityAllBackends
  serializePredicate (TableColumnHasConstraint tbl col cns) =
    object [ "has-column-constraint" .= object [ "table" .= tbl, "column" .= col
                                               , "constraint" .= serializeConstraint cns ] ]

  predicateCascadesDropOn (TableColumnHasConstraint tblNm colNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | Just (TableHasColumn tblNm' colNm' _ :: TableHasColumn be) <- cast p' = tblNm' == tblNm && colNm' == colNm
    | otherwise = False

-- | Asserts that the given table has a secondary index with the given name
-- covering the given columns (in order). Create these predicates with
-- 'Database.Beam.Migrate.Types.CheckedEntities.addTableIndex'.
data TableHasIndex be
  = TableHasIndex
  { hasIndex_table   :: QualifiedName                -- ^ table name
  , hasIndex_name    :: Text                         -- ^ index name
  , hasIndex_columns :: NE.NonEmpty Text             -- ^ ordered column names
  , hasIndex_opts    :: BeamSqlBackendIndexSyntax be -- ^ index options (e.g. uniqueness)
  } deriving Generic
deriving instance Show (BeamSqlBackendIndexSyntax be) => Show (TableHasIndex be)
deriving instance Eq (BeamSqlBackendIndexSyntax be) => Eq (TableHasIndex be)
instance Hashable (BeamSqlBackendIndexSyntax be) => Hashable (TableHasIndex be)
instance ( Typeable be
         , IsSql92UniqueIndexSyntax (BeamSqlBackendSyntax be) ) =>
         DatabasePredicate (TableHasIndex be) where
  englishDescription (TableHasIndex { hasIndex_table = tbl, hasIndex_name = nm
                                    , hasIndex_columns = cols, hasIndex_opts = opts }) =
    (if indexIsUnique @(BeamSqlBackendSyntax be) opts then "Unique index " else "Index ") <>
    show nm <> " on table " <> show tbl <> " covering columns " <> show cols

  predicateSpecificity _ = PredicateSpecificityAllBackends

  serializePredicate (TableHasIndex { hasIndex_table = tbl, hasIndex_name = nm
                                    , hasIndex_columns = cols, hasIndex_opts = opts }) =
    object [ "has-index" .= object [ "table" .= tbl, "name" .= nm
                                   , "columns" .= cols
                                   , "options" .= serializeIndexOptions @(BeamSqlBackendSyntax be) opts ] ]

  predicateCascadesDropOn (TableHasIndex { hasIndex_table = tblNm }) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

-- | Asserts that the given table has a primary key made of the given columns.
-- The order of the columns is significant.
data TableHasPrimaryKey
  = TableHasPrimaryKey
  { hasPrimaryKey_table :: QualifiedName   {-^ Table name -}
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

-- | Asserts that the given table has a foreign key referencing another table.
-- Create these predicates with
-- 'Database.Beam.Migrate.Types.CheckedEntities.addTableForeignKey'.
data TableHasForeignKey
  = TableHasForeignKey
  { hasForeignKey_table      :: QualifiedName      -- ^ source table
  , hasForeignKey_columns    :: NE.NonEmpty Text   -- ^ local columns (in order)
  , hasForeignKey_refTable   :: QualifiedName      -- ^ referenced table
  , hasForeignKey_refColumns :: NE.NonEmpty Text   -- ^ referenced columns (in order)
  , hasForeignKey_onUpdate   :: ForeignKeyAction
  , hasForeignKey_onDelete   :: ForeignKeyAction
  } deriving (Show, Eq, Generic)
instance Hashable TableHasForeignKey
instance DatabasePredicate TableHasForeignKey where
  englishDescription (TableHasForeignKey { hasForeignKey_table = tbl
                                         , hasForeignKey_columns = cols
                                         , hasForeignKey_refTable = refTbl
                                         , hasForeignKey_refColumns = refCols
                                         , hasForeignKey_onUpdate = onUpd
                                         , hasForeignKey_onDelete = onDel }) =
    "Table " <> show tbl <> " has foreign key on " <> show cols <>
    " referencing " <> show refTbl <> " " <> show refCols <>
    " ON UPDATE " <> show onUpd <>
    " ON DELETE " <> show onDel

  predicateSpecificity _ = PredicateSpecificityAllBackends

  serializePredicate (TableHasForeignKey { hasForeignKey_table = tbl
                                         , hasForeignKey_columns = cols
                                         , hasForeignKey_refTable = refTbl
                                         , hasForeignKey_refColumns = refCols
                                         , hasForeignKey_onUpdate = onUpd
                                         , hasForeignKey_onDelete = onDel }) =
    object [ "has-foreign-key" .= object
               [ "table"       .= tbl
               , "columns"     .= cols
               , "ref-table"   .= refTbl
               , "ref-columns" .= refCols
               , "on-update"   .= serializeForeignKeyAction onUpd
               , "on-delete"   .= serializeForeignKeyAction onDel
               ] ]

  predicateCascadesDropOn (TableHasForeignKey { hasForeignKey_table = tblNm }) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

serializeForeignKeyAction :: ForeignKeyAction -> Text
serializeForeignKeyAction ForeignKeyActionCascade    = "cascade"
serializeForeignKeyAction ForeignKeyActionSetNull    = "set-null"
serializeForeignKeyAction ForeignKeyActionSetDefault = "set-default"
serializeForeignKeyAction ForeignKeyActionRestrict   = "restrict"
serializeForeignKeyAction ForeignKeyNoAction         = "no-action"

deserializeForeignKeyAction :: Text -> Maybe ForeignKeyAction
deserializeForeignKeyAction "cascade"     = Just ForeignKeyActionCascade
deserializeForeignKeyAction "set-null"    = Just ForeignKeyActionSetNull
deserializeForeignKeyAction "set-default" = Just ForeignKeyActionSetDefault
deserializeForeignKeyAction "restrict"    = Just ForeignKeyActionRestrict
deserializeForeignKeyAction "no-action"   = Just ForeignKeyNoAction
deserializeForeignKeyAction _             = Nothing

-- * Deserialization

-- | 'BeamDeserializers' for all the predicates defined in this module
beamCheckDeserializers
  :: forall be
   . ( Typeable be, BeamMigrateOnlySqlBackend be
     , HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be)
     , IsSql92UniqueIndexSyntax (BeamSqlBackendSyntax be) )
  => BeamDeserializers be
beamCheckDeserializers = mconcat
  [ beamDeserializer (const deserializeSchemaExistsPredicate)
  , beamDeserializer (const deserializeTableExistsPredicate)
  , beamDeserializer (const deserializeTableHasPrimaryKeyPredicate)
  , beamDeserializer (const deserializeTableHasIndexPredicate)
  , beamDeserializer (const deserializeTableHasForeignKeyPredicate)
  , beamDeserializer deserializeTableHasColumnPredicate
  , beamDeserializer deserializeTableColumnHasConstraintPredicate
  ]
  where
    deserializeSchemaExistsPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeSchemaExistsPredicate =
      withObject "SchemaExistsPredicate" $ \v ->
      SomeDatabasePredicate <$> (SchemaExistsPredicate <$> v .: "schema-exists")

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

    deserializeTableHasIndexPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableHasIndexPredicate =
      withObject "TableHasIndex" $ \v ->
      v .: "has-index" >>=
      (withObject "TableHasIndex" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableHasIndex be))
         (TableHasIndex <$> v' .: "table" <*> v' .: "name"
                        <*> v' .: "columns"
                        <*> (deserializeIndexOptions @(BeamSqlBackendSyntax be) =<< v' .: "options")))

    deserializeTableHasForeignKeyPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableHasForeignKeyPredicate =
      withObject "TableHasForeignKey" $ \v ->
      v .: "has-foreign-key" >>=
      (withObject "TableHasForeignKey" $ \v' ->
       SomeDatabasePredicate <$>
         (TableHasForeignKey
           <$> v' .: "table"
           <*> v' .: "columns"
           <*> v' .: "ref-table"
           <*> v' .: "ref-columns"
           <*> (fromMaybe ForeignKeyNoAction . (>>= deserializeForeignKeyAction) <$> v' .:? "on-update")
           <*> (fromMaybe ForeignKeyNoAction . (>>= deserializeForeignKeyAction) <$> v' .:? "on-delete")))

    deserializeTableHasColumnPredicate :: BeamDeserializers be'
                                       -> Value -> Parser SomeDatabasePredicate
    deserializeTableHasColumnPredicate d =
      withObject "TableHasColumn" $ \v ->
      v .: "has-column" >>=
      (withObject "TableHasColumn" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableHasColumn be))
         (TableHasColumn <$> v' .: "table" <*> v' .: "column"
                         <*> (beamDeserialize d =<< v' .: "type")))

    deserializeTableColumnHasConstraintPredicate :: BeamDeserializers be'
                                                 -> Value -> Parser SomeDatabasePredicate
    deserializeTableColumnHasConstraintPredicate d =
      withObject "TableColumnHasConstraint" $ \v ->
      v .: "has-column-constraint" >>=
      (withObject "TableColumnHasConstraint" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableColumnHasConstraint be))
         (TableColumnHasConstraint <$> v' .: "table" <*> v' .: "column"
                                   <*> (beamDeserialize d =<< v' .: "constraint")))
