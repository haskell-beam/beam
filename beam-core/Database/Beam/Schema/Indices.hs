{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides support for table indices definition, both
-- manual and automatic (see 'dbIndices' and 'addDefaultDbIndices' functions).
module Database.Beam.Schema.Indices
    ( TableIndex (..)
    , Index (..)
    , IndexOptions (..)
    , indexOptions

    , FieldIndexBuilder
    , IndexBuilder (..)
    , EntityIndices (..)
    , DatabaseIndices
    , (:->)
    , IndexFromReference (..)

    , indexOptionsEnglishDescription
    , mkIndexName
    , buildDbIndices
    , tableIndex
    , dbIndices
    , mergeDbIndices
    , defaultTableIndices
    , defaultDbIndices
    , addDefaultDbIndices
    ) where

import           Control.Monad.Writer.Strict (runWriter, tell)

import           Data.Aeson
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Functor.Identity
import           Data.Hashable (Hashable (..))
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
#if ! MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

import           GHC.Exts (Constraint, fromList)
import           GHC.Generics hiding (C, R)
import           GHC.TypeLits

import           Database.Beam.Schema.Tables

-- Some day it should have more options and allow to modify them depending on the backend.
-- | Index options.
data IndexOptions = IndexOptions
    { indexUnique :: Bool
    } deriving (Show, Eq, Ord, Generic)

instance Hashable IndexOptions

instance ToJSON IndexOptions where
    toJSON (IndexOptions unique) =
        object [ "unique" .= unique ]
instance FromJSON IndexOptions where
    parseJSON = withObject "IndexOptions" $ \o ->
                IndexOptions <$> o .: "unique"

-- | Default options.
--   For now, they just describe a non-@UNIQUE@ index.
indexOptions :: IndexOptions
indexOptions = IndexOptions
    { indexUnique = False
    }

indexOptionsEnglishDescription :: IndexOptions -> String
indexOptionsEnglishDescription (IndexOptions uniq) =
    (if uniq then "unique " else " ")

-- | Single index settings for some table.
data TableIndex = TableIndex (NonEmpty Text) IndexOptions
    deriving (Show, Eq, Ord)

-- | Single index settings.
data Index = Index !Text !TableIndex
    deriving (Show, Eq, Ord)

-- | Make a name for an index.
mkIndexName :: Text -> [Text] -> Text
mkIndexName tblNm fields =
    "idx_" <> tblNm <> "_" <> T.intercalate "_" fields

-- | Indices for an entity (table).
--
--   Usually, you use the 'dbIndices' function to generate an empty set of indices
--   and modify its fields with 'mconcat' of several 'tableIndex'es, later wrapping
--   it with 'addDefaultDbIndices'.
newtype EntityIndices be db entity = EntityIndices
    { _entityIndices :: DList (DatabaseEntity be db entity -> Index)
      -- ^ Multiple indices info assuming the database settings is given.
      --   We have to accept database settings here rather than take them at index building
      --   stage because migrations require that.
    } deriving (Semigroup, Monoid)

-- | When parameterized by this entity tag, a database type will hold
--   a schema of indices, i.e. a way to build material indices when corresponding database
--   settings are provided. Under the hood, each entity type is transformed into its
--   'EntityIndices' type. For tables, this includes, accordingly, indices for this table; all other types of entities are assumed to be empty.
type DatabaseIndices be db = db (EntityIndices be db)

-- | Construct material indices from the given database schema and indices schema.
buildDbIndices
    :: forall be db.
       Database be db
    => DatabaseSettings be db -> DatabaseIndices be db -> [Index]
buildDbIndices dbSettings dbIdxs =
    let (_ :: DatabaseSettings be db, indices) =
            runWriter $ zipTables (Proxy @be)
                (\dbEntity (EntityIndices mkIndices) -> do
                    tell [ mkIndex dbEntity | mkIndex <- DL.toList mkIndices ]
                    return dbEntity)
                dbSettings dbIdxs
    in indices

-- | Return empty 'DatabaseIndices'. You can use it like
--
-- > dbIndices{ tbl1 = tableIndex field1 indexOptions{ uniqueIndex = True } <>
-- >                   tableIndex (field2, field3) indexOptions }
dbIndices :: forall be db. Database be db => DatabaseIndices be db
dbIndices = runIdentity $ zipTables (Proxy @be) (\_ _ -> pure mempty) undefined undefined

-- This function can't be used as 'Semigroup.<>', because 'DatabaseIndices' is not
-- a normal type we can infer an instance for.
-- | Combine two indices settings.
mergeDbIndices
    :: forall be db.
       Database be db
    => DatabaseIndices be db -> DatabaseIndices be db -> DatabaseIndices be db
mergeDbIndices i1 i2 =
    runIdentity $ zipTables (Proxy @be) (\ei1 ei2 -> pure (ei1 <> ei2)) i1 i2


-- * Manual indices definition

-- | Helper for 'IsNotEmptyData'.
type family GIsNotEmptyData (item :: Symbol) (rep :: * -> *) :: Constraint where
    GIsNotEmptyData item (D1 _d (C1 _c U1)) =
        TypeError ('Text item ':<>: 'Text " without fields is not allowed here")
    GIsNotEmptyData _ _ = ()

-- | Ensures a datatype has at least one field.
type IsNotEmptyData item x = (Generic x, GIsNotEmptyData item (Rep x))

-- | Gathers index fields from the given field of a table.
class FieldIndexBuilder field where
    buildFieldIndex :: field -> NonEmpty Text

instance FieldIndexBuilder (TableField table a) where
    buildFieldIndex field = (:| []) $ _fieldName field

instance (Beamable (PrimaryKey table),
          IsNotEmptyData "Primary key" (PrimaryKey table Identity)) =>
         FieldIndexBuilder (PrimaryKey table (TableField table')) where
    buildFieldIndex =
        fromList . allBeamValues (\(Columnar' (TableField _ fieldNm)) -> fieldNm)

instance (Beamable (PrimaryKey table),
          IsNotEmptyData "Primary key" (PrimaryKey table Identity)) =>
         FieldIndexBuilder (PrimaryKey table (Nullable (TableField table'))) where
    buildFieldIndex =
        fromList . allBeamValues (\(Columnar' (TableField _ fieldNm)) -> fieldNm)

-- | Gathers index fields from a user-supplied pack of table fields.
class IndexBuilder table a where
    buildIndex :: TableSettings table -> a -> NonEmpty Text

-- | Field accessors are building blocks for indices.
instance (f ~ TableField table, table ~ table', FieldIndexBuilder field) =>
         IndexBuilder table' (table f -> field) where
    buildIndex settings getter =
        buildFieldIndex $ getter settings

instance (IndexBuilder table a, IndexBuilder table b) =>
         IndexBuilder table (a, b) where
    buildIndex settings (a, b) = buildIndex settings a <> buildIndex settings b

instance (IndexBuilder table a, IndexBuilder table b, IndexBuilder table c) =>
         IndexBuilder table (a, b, c) where
    buildIndex settings (a, b, c) =
        buildIndex settings a <> buildIndex settings b <> buildIndex settings c

-- | Make a table index builder covering the specified fields.
--   Basic usage is to pass a table field accessor or a tuple of them to this function.
--   Currently, no more than 3 elements in a tuple are supported, but feel free to nest
--   tuples.
--   Order of fields is preserved: tuples expand straightforwardly, while primary keys
--   expand to a list of fields in the same order as they are mentioned in the
--   corresponding constructor.
tableIndex
    :: IndexBuilder table a
    => a
    -> IndexOptions
    -> EntityIndices be db (TableEntity table)
tableIndex builder idxOpts =
    EntityIndices . DL.singleton $
        \(DatabaseEntity (DatabaseTable _ _ tblNm tblSettings)) ->
            Index tblNm $ TableIndex (buildIndex tblSettings builder) idxOpts

-- | For the given part of table @tblp@, indices derived from it.
type TableIndicesBuilder tblp = DList (tblp -> TableIndex)

-- | Helper for GAutoTableIndices
contramapTableIndicesBuilder :: (b -> a) -> TableIndicesBuilder a -> TableIndicesBuilder b
contramapTableIndicesBuilder f = fmap (. f)

-- * Automatic indices definition

-- | Indicates a reference from table @tbl@ to table @tbl'@.
data (tbl :: (* -> *) -> *) :-> (tbl' :: (* -> *) -> *)

-- | Provide options for an automatically created index, which is caused by a primary key
-- of one table being embedded into another.
-- This typeclass is only needed to be defined for inter-table references (see ':->').
class IndexFromReference reference where
    referenceIndexOptions :: Proxy reference -> IndexOptions
    referenceIndexOptions _ = indexOptions

{- @martoon TODO: I see several minor (or not) problems with making user define instances of
   this typeclass:

    1. Logic locality - instances can occur to be far away from user's database settings
       declaration.
    2. Garbage - user have to care itself about removing redundant instances which no more
       in use.
    3. Extensibility - each time we want to add a type-level restriction on a created index
       (e.g. between these two tables only "hash" indices are allowed - nonsence, but we might
       want something like this one day), then user's code will break on each such addition.

    I'm thinking of an alternative approach when user has to supply some 'HList' stuff,
    element in 'HList' would be an 'IndexOption' tagged with @tbl :-> tbl'@.
    If the user annotates his 'IndexOption' with the @tbl :-> tbl'@ reference then
    he pretty well controlls correspondence between options and indices they relate to.
    This would look like the following:

    > defaultDbIndices (
    >     (autoOption @(Table1 :-> Table1) indexOptions) :&
    >     (autoOption @(Table2 :-> Table1) indexOptions .*.
    >      autoOption @(Table2 :-> Table2) indexOptions { ... })
    >   )

    Although this all is pretty cumbersome and scaring at a first glance, and dependencies on
    'hlist' and 'vinyl' which may be required here are quite heavyweight.
-}

-- | Generic helper for 'AutoTableIndices'.
class GAutoTableIndices (x :: * -> *) where
    -- | Returns list of deferred indices.
    --   Exactly this type is required, later in migrations knowing that list size does
    --   not depend on names (only on structure of the database) is important.
    autoTableIndices' :: DList (x p -> TableIndex)

instance GAutoTableIndices x => GAutoTableIndices (M1 i f x) where
    autoTableIndices' = contramapTableIndicesBuilder unM1 $ autoTableIndices' @x

instance (GAutoTableIndices x, GAutoTableIndices y) =>
          GAutoTableIndices (x :*: y) where
    autoTableIndices' =
        contramapTableIndicesBuilder (\(x :*: _) -> x) (autoTableIndices' @x) <>
        contramapTableIndicesBuilder (\(_ :*: y) -> y) (autoTableIndices' @y)

instance GAutoTableIndices (Rec0 x) where
    autoTableIndices' = mempty

instance {-# OVERLAPPING #-}
         (Beamable (PrimaryKey tbl'), IndexFromReference (tbl :-> tbl')) =>
         GAutoTableIndices (Rec0 (PrimaryKey tbl' (TableField tbl))) where
    autoTableIndices' =
        if tableValuesNeeded (Proxy @(PrimaryKey tbl')) == 0
        then DL.empty
        else DL.singleton $ \(K1 referringField) ->
            let pkFields = allBeamValues
                              (\(Columnar' (TableField _ fieldNm)) -> fieldNm)
                              referringField
                opts = referenceIndexOptions (Proxy @(tbl :-> tbl'))
                -- unsafe call, but at this point we know the list is not empty
            in TableIndex (fromList pkFields) opts

instance {-# OVERLAPPING #-}
         (Beamable (PrimaryKey tbl'), IndexFromReference (tbl :-> tbl')) =>
         GAutoTableIndices (Rec0 (PrimaryKey tbl' (Nullable (TableField tbl)))) where
    autoTableIndices' =
        if tableValuesNeeded (Proxy @(PrimaryKey tbl')) == 0
        then DL.empty
        else DL.singleton $ \(K1 referringField) ->
            let pkFields = allBeamValues
                              (\(Columnar' (TableField _ fieldNm)) -> fieldNm)
                              referringField
                opts = referenceIndexOptions (Proxy @(tbl :-> tbl'))
                -- unsafe call, but at this point we know the list is not empty
            in TableIndex (fromList pkFields) opts

-- | Traverses fields of the given table and builds indices for all encountered 'PrimaryKey's.
class AutoEntityIndex be db tbl where
    autoEntityIndices :: EntityIndices be db tbl

-- Other types of entities are approaching, and we probably don't want to define
-- instances for all of them.
-- | Traverses the given table and for every field which is some 'PrimaryKey'
-- makes corresponding SQL index, this allows "JOIN"s on this table perform nicely.
instance {-# OVERLAPPABLE #-}
         AutoEntityIndex be db entity where
    autoEntityIndices = mempty

instance (Generic (TableSettings tbl),
          GAutoTableIndices (Rep (TableSettings tbl))) =>
         AutoEntityIndex be db (TableEntity tbl) where
    autoEntityIndices =
        EntityIndices $ flip fmap autoTableIndices' $
            \mkIndex (DatabaseEntity (DatabaseTable _ _ tblNm tblSettings)) ->
                Index tblNm (mkIndex (from tblSettings))

-- | Automatically creates indices for every 'PrimaryKey' embedded into the given table.
defaultTableIndices
    :: (Generic (TableSettings table),
        GAutoTableIndices (Rep (TableSettings table)))
    => EntityIndices be db (TableEntity table)
defaultTableIndices = autoEntityIndices

-- | Traverses all tables in database and builds indices for all encountered 'PrimaryKey's.
class GAutoDbIndices (x :: * -> *) where
    autoDbIndices' :: x p

instance GAutoDbIndices x => GAutoDbIndices (M1 i f x) where
    autoDbIndices' = M1 autoDbIndices'

instance (GAutoDbIndices x, GAutoDbIndices y) =>
         GAutoDbIndices (x :*: y) where
    autoDbIndices' = autoDbIndices' :*: autoDbIndices'

instance AutoEntityIndex be db tbl =>
         GAutoDbIndices (Rec0 (EntityIndices be db tbl)) where
    autoDbIndices' = K1 $ autoEntityIndices @be @db

-- | Automatically creates indices for every 'PrimaryKey' embedded into a table, for
-- @JOIN@s sake.
-- Tables' primary keys themselves are not included as soon as for each primary key
-- a corresponding index is provided by a database engine.
defaultDbIndices
    :: forall be db.
       (Database be db,
        Generic (DatabaseIndices be db), GAutoDbIndices (Rep (DatabaseIndices be db)))
    => DatabaseIndices be db
defaultDbIndices = to autoDbIndices'

-- | Attaches default indices to the given ones. Usually more convenient than plain
--   'defaultDbIndices':
--
-- > ... `withDbIndices` addDefaultDbIndices dbIndices{ table1 = ..., ... }
addDefaultDbIndices
    :: (Database be db,
        Generic (DatabaseIndices be db), GAutoDbIndices (Rep (DatabaseIndices be db)))
    => DatabaseIndices be db -> DatabaseIndices be db
addDefaultDbIndices = mergeDbIndices defaultDbIndices
