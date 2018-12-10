{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines common 'DatabasePredicate's that are shared among backends
module Database.Beam.Migrate.Checks where

import Control.Monad (guard)
import Database.Beam.Backend.SQL.SQL92

import Database.Beam.Migrate.Serialization
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Schema.Indices
import Database.Beam.Schema.Tables

import Data.Aeson (object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Hashable (Hashable (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast, splitTyConApp, typeOf, typeRep)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup
#endif

import GHC.Exts (fromList, toList)
import GHC.Generics (Generic)

import Unsafe.Coerce (unsafeCoerce)

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

-- | Asserts that the given table has a primary key made of the given columns.
-- The order of the columns is significant.
data TableHasIndex
  = TableHasIndex
  { hasIndex_table :: QualifiedName {-^ Table name -}
  , hasIndex_cols  :: [Text] {-^ Column names -}
  , hasIndex_opts  :: IndexOptions
  } deriving (Show, Eq, Generic)
instance Hashable TableHasIndex where
    hashWithSalt salt (TableHasIndex tbl cols opts) = hashWithSalt salt (tbl, toList cols, opts)
instance DatabasePredicate TableHasIndex where
  englishDescription (TableHasIndex tblName colNames opts) =
    "Table " <> show tblName <> " has " <> indexOptionsEnglishDescription opts <>
    "index " <> show colNames

  predicateSpecificity _ = PredicateSpecificityOnlyBackend ""

  serializePredicate (TableHasIndex tbl cols opts) =
    object [ "has-index" .= object [ "table" .= tbl
                                   , "columns" .= cols
                                   , "options" .= opts ] ]

  -- we do not provide cascading delete of 'TableHasColumn' check,
  -- index should be removed explicitely first
  predicateCascadesDropOn (TableHasIndex tblNm _ _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

  predicateRestrictsDropOf (TableHasIndex tblNm colNms _) p'
    | Just (tblNm', colNm') <-
        withTyCon (\(TableHasColumn tblNm' colNm' _) -> (tblNm', colNm')) p' =
          tblNm == tblNm' && (colNm' `elem` colNms)
    | otherwise = False

-- | Match a given item's type against a type-level application with the given
-- type constructor. Applies the given function and returns 'Just' its result on match,
-- 'Nothing' otherwise.
-- Unlike 'cast', this function does not require @a@ type to be instance of 'Typeable'.
withTyCon
    :: forall (con :: * -> *) (item :: *) r.
       (Typeable con, Typeable item)
    => (forall a. con a -> r) -> item -> Maybe r
withTyCon f x = do
    (itemTyCon, itemTyArgs@(_ : _)) <- pure $ splitTyConApp (typeOf x)
    (conTyCon, conTyArgs) <- pure $ splitTyConApp (typeRep (Proxy @con))
    guard (itemTyCon == conTyCon && init itemTyArgs == conTyArgs)
    return (f $ unsafeCoerce x)

-- | Convert gathered indices into checks.
entityIndicesToChecks
    :: Table table
    => DatabaseEntityDescriptor be (TableEntity table)
    -> EntityIndices be db (TableEntity table)
    -> [TableCheck table]
entityIndicesToChecks (DatabaseTable _ origNm _ _) (EntityIndices mkTableIndices) =
    flip map (toList mkTableIndices) $ \mkTableIndex ->
        TableCheck $ \qTblNm@(QualifiedName tblSchema tblNm) tblSettings ->
          let dbEntity = DatabaseEntity (DatabaseTable tblSchema origNm tblNm tblSettings)
              Index _ (TableIndex index opts) = mkTableIndex dbEntity
          in SomeDatabasePredicate $ TableHasIndex qTblNm (fromList $ toList index) opts

-- * Deserialization

-- | 'BeamDeserializers' for all the predicates defined in this module
beamCheckDeserializers
  :: forall be
   . ( Typeable be, BeamMigrateOnlySqlBackend be
     , HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be) )
  => BeamDeserializers be
beamCheckDeserializers = mconcat
  [ beamDeserializer (const deserializeTableExistsPredicate)
  , beamDeserializer (const deserializeTableHasPrimaryKeyPredicate)
  , beamDeserializer deserializeTableHasColumnPredicate
  , beamDeserializer deserializeTableColumnHasConstraintPredicate
  , beamDeserializer (const deserializeTableHasIndexPredicate)
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

    deserializeTableHasIndexPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableHasIndexPredicate =
      withObject "TableHasIndex" $ \v ->
      v .: "has-index" >>=
      (withObject "TableHasIndex" $ \v' ->
       SomeDatabasePredicate <$>
         (TableHasIndex <$> v' .: "table" <*> v' .: "columns" <*> v' .: "options"))
