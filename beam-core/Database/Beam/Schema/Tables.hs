{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Tables
    (
    -- * Database Types
      Database(..), GenDatabaseTable(..), DatabaseTable(..), DatabaseSettings
    , ReifiedDatabaseSchema, ReifiedTableSchema
    , DatabaseModifications, TableSchemaModifications(..), FieldSchemaModification(..)
    , tableName, tableSettings
    , defaultDbSettings, withDbModifications
    , modifyingDb, fieldModification, tableModification, tableFieldsModification
    , allTableSettings

    , Lenses, LensFor(..)

    -- * Columnar and Column Tags
    , Columnar, Columnar'(..)
    , Nullable, TableField(..)
    , Exposed(..)
    , fieldName, fieldSchema, maybeFieldSchema

    , TableSettings, TableSkeleton, Ignored(..)
    , GFieldsFulfillConstraint(..), FieldsFulfillConstraint, WithConstraint(..)

    -- * Tablesmg<
    , Table(..), Beamable(..)
    , defTblFieldSettings
    , reifyTableSchema, tableValuesNeeded
    , pk
    , allBeamValues, changeBeamRep

    -- * Fields
    , HasDefaultFieldSchema(..) )
    where

import           Database.Beam.Backend.Types

import           Control.Monad.Writer
import           Control.Monad.Identity

import           Data.Char (isUpper, toLower)
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import qualified GHC.Generics as Generic
import           GHC.Generics hiding (R)
import           GHC.Types (Constraint)

import           Lens.Micro hiding (to)

type ReifiedDatabaseSchema be = [(Text, ReifiedTableSchema be)]
type ReifiedTableSchema be = [(Text, BackendColumnSchema be)]

class Database db where
    allTables :: (forall tbl. Table tbl => f tbl -> b) -> db f -> [b]
    default allTables :: ( Generic (db f)
                         , GAllTables f (Rep (db f) ()) ) =>
                        (forall tbl. Table tbl => f tbl -> b) -> db f -> [b]
    allTables f db = allTables' f (from' db)

    zipTables :: Monad m => (forall tbl. (Beamable tbl, Table tbl) => f tbl -> g tbl -> m (h tbl)) -> db f -> db g -> m (db h)
    default zipTables :: ( Generic (db f), Generic (db g), Generic (db h)
                         , Monad m
                         , GZipDatabase f g h
                                        (Rep (db f)) (Rep (db g)) (Rep (db h)) ) =>
                         (forall tbl. (Table tbl, Beamable tbl) => f tbl -> g tbl -> m (h tbl)) ->
                         db f -> db g ->m (db h)
    zipTables combine f g = to <$> gZipDatabase combine (from f) (from g)

allTableSettings :: Database db => DatabaseSettings be db -> [GenDatabaseTable be db]
allTableSettings = allTables GenDatabaseTable

defaultDbSettings :: ( Generic (DatabaseSettings be db)
                     , GAutoDbSettings (Rep (DatabaseSettings be db) ()) ) =>
                     DatabaseSettings be db
defaultDbSettings = to' autoDbSettings'

modifyingDb :: Database db => DatabaseModifications be db
modifyingDb = runIdentity (zipTables (\_ _ -> pure (tableModification id tableFieldsModification)) undefined undefined)

withDbModifications :: Database db =>
                       DatabaseSettings be db
                    -> DatabaseModifications be db
                    -> DatabaseSettings be db
withDbModifications settings mods =
  runIdentity (zipTables (\(TableSchemaModifications modName modTable) (DatabaseTable p nm tblSettings) ->
                            DatabaseTable p (modName nm) <$>
                            zipBeamFieldsM (\(Columnar' (FieldSchemaModification f)) (Columnar' field) -> pure (Columnar' (f field)))
                                           modTable tblSettings)
                mods settings)

data GenDatabaseTable be db where
    GenDatabaseTable :: DatabaseTable be db table -> GenDatabaseTable be db
data DatabaseTable be (db :: ((((* -> *) -> *) -> *) -> *)) table where
    DatabaseTable :: Table table => Proxy table -> Text -> TableSettings be table -> DatabaseTable be db table

tableName :: Lens' (DatabaseTable be db table) Text
tableName f (DatabaseTable proxy name settings) = (\name' -> DatabaseTable proxy name' settings) <$> f name
tableSettings :: Lens' (DatabaseTable be db table) (TableSettings be table)
tableSettings f (DatabaseTable proxy name settings) = (\settings' -> DatabaseTable proxy name settings') <$> f settings

fieldModification :: (Text -> Text) -> (BackendColumnSchema be -> BackendColumnSchema be)
                  -> FieldSchemaModification be tbl ty
fieldModification modNm modField =
  FieldSchemaModification (\(TableField nm field) -> TableField (modNm nm) (modField field))

tableModification :: Beamable tbl => (Text -> Text) -> tbl (FieldSchemaModification be tbl) -> TableSchemaModifications be tbl
tableModification modNm modFields =
  TableSchemaModifications modNm modFields

tableFieldsModification :: Beamable tbl => tbl (FieldSchemaModification be tbl)
tableFieldsModification = changeBeamRep (\_ -> Columnar' (FieldSchemaModification id)) tblSkeleton

type DatabaseSettings be db = db (DatabaseTable be db)
type DatabaseModifications be db = db (TableSchemaModifications be)
data TableSchemaModifications be tbl
  = TableSchemaModifications
  { modTableName :: Text -> Text
  , modTableFields :: tbl (FieldSchemaModification be tbl) }
newtype FieldSchemaModification be tbl ty =
  FieldSchemaModification (TableField be tbl ty -> TableField be tbl ty)

class GAutoDbSettings x where
    autoDbSettings' :: x
instance GAutoDbSettings (x p) => GAutoDbSettings (D1 f x p) where
    autoDbSettings' = M1 autoDbSettings'
instance GAutoDbSettings (x p) => GAutoDbSettings (C1 f x p) where
    autoDbSettings' = M1 autoDbSettings'
instance (GAutoDbSettings (x p), GAutoDbSettings (y p)) => GAutoDbSettings ((x :*: y) p) where
    autoDbSettings' = autoDbSettings' :*: autoDbSettings'
instance ( GDefaultTableFieldSettings (Rep (TableSettings be tbl) ())
         , Generic (TableSettings be tbl), Table tbl, Selector f) => GAutoDbSettings (S1 f (K1 Generic.R (DatabaseTable be db tbl)) p) where
    autoDbSettings' = M1 (K1 (DatabaseTable (Proxy :: Proxy tbl) (fromString name) defTblFieldSettings))
        where name  = unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (DatabaseTable be db tbl)) p))

class GAllTables f x where
    allTables' :: (forall tbl. Table tbl => f tbl -> b) -> x -> [b]
instance GAllTables f (x p) => GAllTables f (M1 s m x p) where
    allTables' f (M1 x) = allTables' f x
instance (GAllTables f (x p), GAllTables f (y p)) => GAllTables f ((x :*: y) p) where
    allTables' f (x :*: y) = allTables' f x ++ allTables' f y
instance Table tbl => GAllTables f (K1 Generic.R (f tbl) p) where
    allTables' f (K1 x) = [f x]

class GZipDatabase f g h x y z where
  gZipDatabase :: Monad m => (forall tbl. (Table tbl, Beamable tbl) => f tbl -> g tbl -> m (h tbl))
               -> x () -> y () -> m (z ())
instance GZipDatabase f g h x y z =>
  GZipDatabase f g h (M1 a b x) (M1 a b y) (M1 a b z) where
  gZipDatabase combine ~(M1 f) ~(M1 g) = M1 <$> gZipDatabase combine f g
instance ( GZipDatabase f g h ax ay az
         , GZipDatabase f g h bx by bz ) =>
  GZipDatabase f g h (ax :*: bx) (ay :*: by) (az :*: bz) where
  gZipDatabase combine ~(ax :*: bx) ~(ay :*: by) =
    do a <- gZipDatabase combine ax ay
       b <- gZipDatabase combine bx by
       pure (a :*: b)
instance (Table tbl, Beamable tbl) => GZipDatabase f g h (K1 Generic.R (f tbl)) (K1 Generic.R (g tbl)) (K1 Generic.R (h tbl)) where
  gZipDatabase combine ~(K1 x) ~(K1 y) =
    K1 <$> combine x y

data Lenses (t :: (* -> *) -> *) (f :: * -> *) x
data LensFor t x where
    LensFor :: Generic t => Lens' t x -> LensFor t x

-- | A type family that we use to "tag" columns in our table datatypes.
--
--   This is what allows us to use the same table type to hold table data, describe table settings,
--   derive lenses, and provide expressions.
--
--   The basic rules are
--
-- > Columnar Identity x = x
--
--   Thus, any Beam table applied to 'Identity' will yield a simplified version of the data type, that contains
--   just what you'd expect.
--
--   The 'Nullable' type is used when referencing 'PrimaryKey's that we want to include optionally.
--   For example, if we have a table with a 'PrimaryKey', like the following
--
-- > data BeamTableT f = BeamTableT
-- >                   { _refToAnotherTable :: PrimaryKey AnotherTableT f
-- >                   , ... }
--
--   we would typically be required to provide values for the 'PrimaryKey' embedded into 'BeamTableT'. We can use
--   'Nullable' to lift this constraint.
--
-- > data BeamTableT f = BeamTableT
-- >                   { _refToAnotherTable :: PrimaryKey AnotherTableT (Nullable f)
-- >                   , ... }
--
--   Now we can use 'justRef' and 'nothingRef' to refer to this table optionally. The embedded 'PrimaryKey' in '_refToAnotherTable'
--   automatically has its fields converted into 'Maybe' using 'Nullable'.
--
--   The last 'Columnar' rule is
--
-- > Columnar f x = f x
--
--   Use this rule if you'd like to parameterize your table type over any other functor. For example, this is used
--   in the query modules to write expressions such as 'TableT QExpr', which returns a table whose fields have been
--   turned into query expressions.
--
--   The other rules are used within Beam to provide lenses and to expose the inner structure of the data type.
type family Columnar (f :: * -> *) x where
    Columnar Exposed x = Exposed x

    Columnar Identity x = x

    Columnar (Lenses t Identity) x = LensFor (t Identity) (Columnar Identity x)
    Columnar (Lenses t f) x = LensFor (t f) (f x)

    Columnar (Nullable c) x = Columnar c (Maybe x)

    Columnar f x = f x

newtype Columnar' f a = Columnar' (Columnar f a)

-- | Support for NULLable Foreign Key references.
--
-- > data MyTable f = MyTable
-- >                { nullableRef :: PrimaryKey AnotherTable (Nullable f)
-- >                , ... }
-- >                 deriving (Generic, Typeable)
--
-- See 'Columnar' for more information.
data Nullable (c :: * -> *) x

-- | Metadata for a field of type 'ty' in 'table'.
--
-- > Columnar (TableField table) ty = TableField table ty
--
--   This is used to declare 'tblFieldSettings' in the 'Table' class.
--
--   It is easiest to access these fields through the lenses 'fieldName', 'fieldConstraints', and 'fieldSettings'.
--
-- > data EmployeeT f = Employee
-- >                  { _employeeId :: Columnar f AutoId
-- >                  , _employeeDepartment :: Columnar f Text
-- >                  , _employeeFirstName :: Columnar f Text
-- >                  , _employeeLastName :: Columnar f Text }
-- >                    deriving Generic
--
--   Now we can use 'tableConfigLenses' and the 'TableField' lenses to modify the default table configuration
--
-- > Employee (LensFor employeeIdC) (LensFor employeeDepartmentC) (LensFor employeeFirstNameC) (LensFor employeeLastNameC) = tableConfigLenses
-- >
-- > instance Table EmployeeT where
-- >    data PrimaryKey EmployeeT f = EmployeeId (Columnar f AutoId)
-- >    primaryKey = EmployeeId . _beamEmployeeId
-- >
-- >    tblFieldSettings = defTblFieldSettings
-- >                     & employeeFirstNameC . fieldName .~ "fname"
-- >                     & employeeLastNameC  . fieldName .~ "lname"
-- >                     & employeeLastNameC  . fieldSettings .~ Varchar (Just 128) -- Give it a 128 character limit
data TableField be (table :: (* -> *) -> *) ty = TableField
                                               { _fieldName        :: Text                   -- ^ The field name
                                               , _fieldSchema      :: BackendColumnSchema be -- ^ SQL storage informationa for the field
                                               }
deriving instance Show (BackendColumnSchema be) => Show (TableField be t ty)
deriving instance Eq (BackendColumnSchema be) => Eq (TableField be t ty)

fieldName :: Lens' (TableField be table ty) Text
fieldName f (TableField name s) = (\name' -> TableField name' s) <$> f name
fieldSchema :: Lens (TableField be table a) (TableField be table b) (BackendColumnSchema be) (BackendColumnSchema be)
fieldSchema f (TableField name s) = (\s' -> TableField name s') <$> f s

type TableSettings be table = table (TableField be table)

data Ignored x = Ignored
type TableSkeleton table = table Ignored

from' :: Generic x => x -> Rep x ()
from' = from

to' :: Generic x => Rep x () -> x
to' = to

type HasBeamFields table f g h = ( GZipTables f g h (Rep (table Exposed)) (Rep (table f)) (Rep (table g)) (Rep (table h))
                                 , Generic (table f), Generic (table g), Generic (table h) )

-- | The big Kahuna! All beam tables implement this class.
--
--   The kind of all table types is `(* -> *) -> *`. This is because all table types are actually /table type constructors/.
--   Every table type takes in another type constructor, called the /column tag/, and uses that constructor to instantiate the column types.
--   See the documentation for 'Columnar'. In order for the default deriving to work, every type passed into 'Columnar' must be an instance
--   of 'FieldSchema'.
--
--   This class is mostly Generic-derivable. You need only specify a type for the table's primary key and a method to extract the primary key
--   given the table.
--
--   Even though all methods are derivable, you are free to override them. Typically, you may want to override 'tblFieldSettings' if you want
--   to specify options for column storage or to rename columns. See 'TableField' for more information. You may want to use 'tableConfigLenses'
--   to simplify accessing 'tblFieldSettings'.
--
--   An example table:
--
-- > data BlogPostT f = BlogPost
-- >                  { _blogPostSlug    :: Columnar f Text
-- >                  , _blogPostBody    :: Columnar f Text
-- >                  , _blogPostDate    :: Columnar f UTCTime
-- >                  , _blogPostAuthor  :: PrimaryKey AuthorT f
-- >                  , _blogPostTagline :: Columnar f (Maybe Text)
-- >                  , _blogPostImageGallery :: PrimaryKey ImageGalleryT (Nullable f) }
-- >                    deriving Generic
-- > instance Table BlogPostT where
-- >    data PrimaryKey BlogPostT f = BlogPostId (Columnar f Text)
-- >    primaryKey = BlogPostId . _blogPostSlug
--
--   We can interpret this as follows:
--
--     * The `_blogPostSlug`, `_blogPostBody`, `_blogPostDate`, and `_blogPostTagline` fields are of types 'Text', 'Text', 'UTCTime', and 'Maybe Text' respectfully.
--     * Since `_blogPostSlug`, `_blogPostBody`, `_blogPostDate`, `_blogPostAuthor` must be provided (i.e, they cannot contain 'Nothing'), they will be given SQL NOT NULL constraints.
--       `_blogPostTagline` is declared 'Maybe' so 'Nothing' will be stored as NULL in the database. `_blogPostImageGallery` will be allowed to be empty because it uses the 'Nullable' tag modifier.
--     * `blogPostAuthor` references the `AuthorT` table (not given here) and is required.
--     * `blogPostImageGallery` references the `ImageGalleryT` table (not given here), but this relation is not required (i.e., it may be 'Nothing'. See 'Nullable').
class (Typeable table, Beamable table, Beamable (PrimaryKey table)) => Table (table :: (* -> *) -> *) where

    -- | A data type representing the types of primary keys for this table.
    --   In order to play nicely with the default deriving mechanism, this type must be an instance of 'Generic'.
    data PrimaryKey table (column :: * -> *) :: *

    -- | Given a table, this should return the PrimaryKey from the table. By keeping this polymorphic over column,
    --   we ensure that the primary key values come directly from the table (i.e., they can't be arbitrary constants)
    primaryKey :: table column -> PrimaryKey table column

class Beamable table where
    zipBeamFieldsM :: Applicative m =>
                      (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> table f -> table g -> m (table h)
    default zipBeamFieldsM :: ( HasBeamFields table f g h
                              , Applicative m ) =>
                             (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> table f -> table g -> m (table h)
    zipBeamFieldsM combine (f :: table f) g =
        to' <$> gZipTables (Proxy :: Proxy (Rep (table Exposed))) combine (from' f) (from' g)

    tblSkeleton :: TableSkeleton table
    default tblSkeleton :: ( Generic (TableSkeleton table)
                           , GTableSkeleton (Rep (TableSkeleton table)) ) => TableSkeleton table
    tblSkeleton = withProxy $ \proxy -> to' (gTblSkeleton proxy)
        where withProxy :: (Proxy (Rep (TableSkeleton table)) -> TableSkeleton table) -> TableSkeleton table
              withProxy f = f Proxy
    -- zipTablesM :: Monad m => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> table f -> table g -> m (table h)
    -- default zipTablesM ::  =>
    --                     Monad m => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> table f -> table g -> m (table h)
    -- zipTablesM combine f g = do hRep <- gZipTables (Proxy :: Proxy (Rep (table Exposed))) combine (from' f) (from' g)
    --                             return (to' hRep)

    -- zipPkM :: Monad m => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> PrimaryKey table f -> PrimaryKey table g -> m (PrimaryKey table h)
    -- default zipPkM :: ( GZipTables f g h (Rep (PrimaryKey table Exposed)) (Rep (PrimaryKey table f)) (Rep (PrimaryKey table g)) (Rep (PrimaryKey table h))
    --                   , Generic (PrimaryKey table f), Generic (PrimaryKey table g), Generic (PrimaryKey table h)
    --                   , Monad m) => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> PrimaryKey table f -> PrimaryKey table g -> m (PrimaryKey table h)
    -- zipPkM combine f g = do hRep <- gZipTables (Proxy :: Proxy (Rep (PrimaryKey table Exposed))) combine (from' f) (from' g)
    --                         return (to' hRep)

reifyTableSchema :: Beamable table => TableSettings be table -> ReifiedTableSchema be
reifyTableSchema =
    allBeamValues (\(Columnar' (TableField name settings)) ->
                       (name, settings))

tableValuesNeeded :: Beamable table => Proxy table -> Int
tableValuesNeeded (Proxy :: Proxy table) = length (allBeamValues (const ()) (tblSkeleton :: TableSkeleton table))

allBeamValues :: Beamable table => (forall a. Columnar' f a -> b) -> table f -> [b]
allBeamValues (f :: forall a. Columnar' f a -> b) (tbl :: table f) =
    execWriter (zipBeamFieldsM combine tbl tbl)
    where combine :: Columnar' f a -> Columnar' f a -> Writer [b] (Columnar' f a)
          combine x _ = do tell [f x]
                           return x

-- pkAllValues :: Table t => (forall a. Columnar' f a -> b) -> PrimaryKey t f -> [b]
-- pkAllValues (f :: forall a. Columnar' f a -> b) (pk :: PrimaryKey table f) = execWriter (zipPkM combine pk pk)
--     where combine :: Columnar' f a -> Columnar' f a -> Writer [b] (Columnar' f a)
--           combine x _ = do tell [f x]
--                            return x

-- fieldAllValues :: Table t => (forall a. Columnar' f a -> b) -> t f -> [b]
-- fieldAllValues  (f :: forall a. Columnar' f a -> b) (tbl :: table f) = execWriter (zipTablesM combine tbl tbl)
--     where combine :: Columnar' f a -> Columnar' f a -> Writer [b] (Columnar' f a)
--           combine x _ = do tell [f x]
--                            return x

changeBeamRep :: Beamable table => (forall a. Columnar' f a -> Columnar' g a) -> table f -> table g
changeBeamRep f tbl = runIdentity (zipBeamFieldsM (\x _ -> return (f x)) tbl tbl)

-- pkChangeRep :: Table t => (forall a. Columnar' f a -> Columnar' g a) -> PrimaryKey t f -> PrimaryKey t g
-- pkChangeRep f pk = runIdentity (zipPkM (\x _ -> return (f x))  pk pk)

-- changeRep :: Table t => (forall a. Columnar' f a -> Columnar' g a) -> t f -> t g
-- changeRep f tbl = runIdentity (zipTablesM (\x _ -> return (f x)) tbl tbl)

data WithConstraint (c :: * -> Constraint) x where
  WithConstraint :: c x => x -> WithConstraint c x

class GFieldsFulfillConstraint (c :: * -> Constraint) (exposed :: * -> *) values withconstraint where
  gWithConstrainedFields :: Proxy c -> Proxy exposed -> values () -> withconstraint ()
instance GFieldsFulfillConstraint c exposed values withconstraint =>
    GFieldsFulfillConstraint c (M1 s m exposed) (M1 s m values) (M1 s m withconstraint) where
  gWithConstrainedFields c _ (M1 x) = M1 (gWithConstrainedFields c (Proxy @exposed) x)
instance GFieldsFulfillConstraint c U1 U1 U1 where
  gWithConstrainedFields _ _ _ = U1
instance (GFieldsFulfillConstraint c aExp a aC, GFieldsFulfillConstraint c bExp b bC) =>
  GFieldsFulfillConstraint c (aExp :*: bExp) (a :*: b) (aC :*: bC) where
  gWithConstrainedFields be _ (a :*: b) = gWithConstrainedFields be (Proxy @aExp) a :*: gWithConstrainedFields be (Proxy @bExp) b
instance (c x) => GFieldsFulfillConstraint c (K1 Generic.R (Exposed x)) (K1 Generic.R x) (K1 Generic.R (WithConstraint c x)) where
  gWithConstrainedFields _ _ (K1 x) = K1 (WithConstraint x)
instance FieldsFulfillConstraint c t =>
    GFieldsFulfillConstraint c (K1 Generic.R (t Exposed)) (K1 Generic.R (t Identity)) (K1 Generic.R (t (WithConstraint c))) where
  gWithConstrainedFields _ _ (K1 x) = K1 (to (gWithConstrainedFields (Proxy @c) (Proxy @(Rep (t Exposed))) (from x)))
instance FieldsFulfillConstraintNullable c t =>
    GFieldsFulfillConstraint c (K1 Generic.R (t (Nullable Exposed))) (K1 Generic.R (t (Nullable Identity))) (K1 Generic.R (t (Nullable (WithConstraint c)))) where
  gWithConstrainedFields _ _ (K1 x) = K1 (to (gWithConstrainedFields (Proxy @c) (Proxy @(Rep (t (Nullable Exposed)))) (from x)))

type FieldsFulfillConstraint (c :: * -> Constraint) t =
  ( Generic (t (WithConstraint c)), Generic (t Identity), Generic (t Exposed)
  , GFieldsFulfillConstraint c (Rep (t Exposed)) (Rep (t Identity)) (Rep (t (WithConstraint c))))

type FieldsFulfillConstraintNullable (c :: * -> Constraint) t =
  ( Generic (t (Nullable (WithConstraint c))), Generic (t (Nullable Identity)), Generic (t (Nullable Exposed))
  , GFieldsFulfillConstraint c (Rep (t (Nullable Exposed))) (Rep (t (Nullable Identity))) (Rep (t (Nullable (WithConstraint c)))))

-- class GFieldsAreSerializable be values backendvalue where
--     gMakeBackendLiterals :: Proxy be -> values () -> backendvalue ()
-- instance GFieldsAreSerializable be values backendvalue =>
--     GFieldsAreSerializable be (M1 s m values) (M1 s m backendvalue) where
--         gMakeBackendLiterals be (M1 x) = M1 (gMakeBackendLiterals be x)
-- instance GFieldsAreSerializable be U1 U1 where
--     gMakeBackendLiterals _ _ = U1
-- instance (GFieldsAreSerializable be a aBackend, GFieldsAreSerializable be b bBackend) =>
--     GFieldsAreSerializable be (a :*: b) (aBackend :*: bBackend) where
--         gMakeBackendLiterals be (a :*: b) = gMakeBackendLiterals be a :*: gMakeBackendLiterals be b
-- instance (FromBackendLiteral be x, Show x, Eq x, Typeable x) => GFieldsAreSerializable be (K1 Generic.R x) (K1 Generic.R (BackendLiteral' be x)) where
--     gMakeBackendLiterals be (K1 x) = K1 (BackendLiteral' x)
-- instance MakeBackendLiterals be t => GFieldsAreSerializable be (K1 Generic.R (t Identity)) (K1 Generic.R (t (BackendLiteral' be))) where
--     gMakeBackendLiterals be (K1 x) = K1 (makeBackendLiterals x)
-- instance ( Generic (t (Nullable (BackendLiteral' be))), Generic (t (Nullable Identity))
--          , GFieldsAreSerializable be (Rep (t (Nullable Identity))) (Rep (t (Nullable (BackendLiteral' be)))) )
--     => GFieldsAreSerializable be (K1 Generic.R (t (Nullable Identity))) (K1 Generic.R (t (Nullable (BackendLiteral' be)))) where

--     gMakeBackendLiterals be (K1 x) = K1 (to' (gMakeBackendLiterals (Proxy :: Proxy be) (from' x)))

-- type MakeBackendLiterals be t = ( Generic (t (BackendLiteral' be)), Generic (t Identity)
--                                 , GFieldsFulfillConstraint c be (Rep (t Identity)) (Rep (t (WithContraint  be))) )

-- makeBackendLiterals :: MakeBackendLiterals be t => t Identity -> t (BackendLiteral' be)
-- makeBackendLiterals tbl =
--     fix $ \(_ :: t (BackendLiteral' be)) ->
--             to' (gMakeBackendLiterals (Proxy :: Proxy be) (from' tbl))

-- class GTableFromBackendLiterals be x where
--     gTableFromBackendLiterals :: FromBackendLiteralsM be (x ())
-- instance GTableFromBackendLiterals be x => GTableFromBackendLiterals be (M1 s m x) where
--     gTableFromBackendLiterals = M1 <$> gTableFromBackendLiterals
-- instance GTableFromBackendLiterals be U1 where
--     gTableFromBackendLiterals = pure U1
-- instance (GTableFromBackendLiterals be a, GTableFromBackendLiterals be b) =>
--     GTableFromBackendLiterals be (a :*: b) where
--         gTableFromBackendLiterals =
--           do a <- gTableFromBackendLiterals
--              b <- gTableFromBackendLiterals
--              pure (a :*: b)
-- instance FromBackendLiterals be x => GTableFromBackendLiterals be (K1 Generic.R x) where
--     gTableFromBackendLiterals = K1 <$> fromBackendLiterals

-- type TableFromBackendLiterals be t = ( Generic (t Identity)
--                                      , GTableFromBackendLiterals be (Rep (t Identity)) )
-- tableFromBackendLiterals :: TableFromBackendLiterals be t => FromBackendLiteralsM be (t Identity)
-- tableFromBackendLiterals = to' <$> gTableFromBackendLiterals

-- | Synonym for 'primaryKey'
pk :: Table t => t f -> PrimaryKey t f
pk = primaryKey

defTblFieldSettings :: ( Generic (TableSettings be table)
                       , GDefaultTableFieldSettings (Rep (TableSettings be table) ())) =>
                       TableSettings be table
defTblFieldSettings = withProxy $ \proxy -> to' (gDefTblFieldSettings proxy)
    where withProxy :: (Proxy (Rep (TableSettings be table) ()) -> TableSettings be table) -> TableSettings be table
          withProxy f = f Proxy

class GZipTables f g h (exposedRep :: * -> *) fRep gRep hRep where
    gZipTables :: Applicative m => Proxy exposedRep -> (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> fRep () -> gRep () -> m (hRep ())
instance ( GZipTables f g h exp1 f1 g1 h1
         , GZipTables f g h exp2 f2 g2 h2) =>
    GZipTables f g h (exp1 :*: exp2) (f1 :*: f2) (g1 :*: g2) (h1 :*: h2) where

        gZipTables _ combine (f1 :*: f2) (g1 :*: g2) =
            (:*:) <$> gZipTables (Proxy :: Proxy exp1) combine f1 g1
                  <*> gZipTables (Proxy :: Proxy exp2) combine f2 g2
instance GZipTables f g h exp fRep gRep hRep =>
    GZipTables f g h (M1 x y exp) (M1 x y fRep) (M1 x y gRep) (M1 x y hRep) where
        gZipTables _ combine (M1 f) (M1 g) = M1 <$> gZipTables (Proxy :: Proxy exp) combine f g
instance ( fa ~ Columnar f a
         , ga ~ Columnar g a
         , ha ~ Columnar h a) =>
    GZipTables f g h (K1 Generic.R (Exposed a)) (K1 Generic.R fa) (K1 Generic.R ga) (K1 Generic.R ha) where
        gZipTables _ combine (K1 f) (K1 g) = (\(Columnar' h) -> K1 h) <$> combine (Columnar' f :: Columnar' f a) (Columnar' g :: Columnar' g a)
--                                                return (K1 (h :: Columnar h a))
instance ( Generic (PrimaryKey rel f)
         , Generic (PrimaryKey rel g)
         , Generic (PrimaryKey rel h)

         , GZipTables f g h (Rep (PrimaryKey rel Exposed)) (Rep (PrimaryKey rel f)) (Rep (PrimaryKey rel g)) (Rep (PrimaryKey rel h))) =>
    GZipTables f g h (K1 Generic.R (PrimaryKey rel Exposed)) (K1 Generic.R (PrimaryKey rel f)) (K1 Generic.R (PrimaryKey rel g)) (K1 Generic.R (PrimaryKey rel h)) where
    gZipTables _ combine (K1 f) (K1 g) = K1 . to' <$> gZipTables (Proxy :: Proxy (Rep (PrimaryKey rel Exposed))) combine (from' f) (from' g)

instance  ( Generic (PrimaryKey rel (Nullable f))
          , Generic (PrimaryKey rel (Nullable g))
          , Generic (PrimaryKey rel (Nullable h))

          , GZipTables f g h (Rep (PrimaryKey rel (Nullable Exposed))) (Rep (PrimaryKey rel (Nullable f))) (Rep (PrimaryKey rel (Nullable g))) (Rep (PrimaryKey rel (Nullable h)))) =>
         GZipTables f g h
                    (K1 Generic.R (PrimaryKey rel (Nullable Exposed)))
                    (K1 Generic.R (PrimaryKey rel (Nullable f)))
                    (K1 Generic.R (PrimaryKey rel (Nullable g)))
                    (K1 Generic.R (PrimaryKey rel (Nullable h))) where
    gZipTables _ combine (K1 f) (K1 g) = K1 . to' <$> gZipTables (Proxy :: Proxy (Rep (PrimaryKey rel (Nullable Exposed)))) combine (from' f) (from' g)

class GDefaultTableFieldSettings x where
    gDefTblFieldSettings :: Proxy x -> x
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (D1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (D1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (C1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (C1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance (GDefaultTableFieldSettings (a p), GDefaultTableFieldSettings (b p)) => GDefaultTableFieldSettings ((a :*: b) p) where
    gDefTblFieldSettings (_ :: Proxy ((a :*: b) p)) = gDefTblFieldSettings (Proxy :: Proxy (a p)) :*: gDefTblFieldSettings (Proxy :: Proxy (b p))

instance (Table table, HasDefaultFieldSchema be field, Selector f ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (TableField be table field)) p) where
    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (TableField be table field)) p)) = M1 (K1 s)
        where s = TableField (T.pack name) (defFieldSchema (Proxy :: Proxy field))
              name = unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (TableField be table field)) ()))

instance ( Table table, Table related
         , BeamBackend be
         , Selector f

         , Generic (PrimaryKey related (TableField be related))
         , Generic (PrimaryKey related (TableField be table))
         , Generic (TableSettings be related)
         , GDefaultTableFieldSettings (Rep (TableSettings be related) ())
         , GZipTables (TableField be related) (TableField be related) (TableField be table)
                      (Rep (PrimaryKey related Exposed))
                      (Rep (PrimaryKey related (TableField be related))) (Rep (PrimaryKey related (TableField be related))) (Rep (PrimaryKey related (TableField be table))) ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (PrimaryKey related (TableField be table))) p) where

    gDefTblFieldSettings _ = M1 . K1 $ primaryKeySettings'
        where tableSettings = defTblFieldSettings :: TableSettings be related
              primaryKeySettings :: PrimaryKey related (TableField be related)
              primaryKeySettings = primaryKey tableSettings

              primaryKeySettings' :: PrimaryKey related (TableField be table)
              primaryKeySettings' = to' (runIdentity (gZipTables (Proxy :: Proxy (Rep (PrimaryKey related Exposed))) convertToForeignKeyField (from' primaryKeySettings) (from' primaryKeySettings)))

              convertToForeignKeyField :: Columnar' (TableField be related) c -> Columnar' (TableField be related) c -> Identity (Columnar' (TableField be table) c)
              convertToForeignKeyField (Columnar' tf) _ =
                  pure . Columnar' $
                  tf { _fieldName = keyName <> "__" <> _fieldName tf
                     , _fieldSchema = nestedSchema (_fieldSchema tf :: BackendColumnSchema be) }

              keyName = T.pack (unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (PrimaryKey related (TableField be table))) ())))

instance ( Table table, Table related
         , Selector f
         , BeamBackend be

         , Generic (PrimaryKey related (TableField be related))
         , Generic (PrimaryKey related (TableField be table))
         , Generic (PrimaryKey related (Nullable (TableField be table)))
         , Generic (TableSettings be related)
         , GDefaultTableFieldSettings (Rep (TableSettings be related) ())
         , GZipTables (TableField be table) (TableField be table) (Nullable (TableField be table))
                      (Rep (PrimaryKey related Exposed))
                      (Rep (PrimaryKey related (TableField be table)))
                      (Rep (PrimaryKey related (TableField be table)))
                      (Rep (PrimaryKey related (Nullable (TableField be table))))
         , GZipTables (TableField be related) (TableField be related) (TableField be table)
                      (Rep (PrimaryKey related Exposed))
                      (Rep (PrimaryKey related (TableField be related)))
                      (Rep (PrimaryKey related (TableField be related)))
                      (Rep (PrimaryKey related (TableField be table)))) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (PrimaryKey related (Nullable (TableField be table)))) p) where

    gDefTblFieldSettings _ =
        M1 . K1 $ settings
        where M1 (K1 nonNullSettings) = gDefTblFieldSettings (Proxy :: Proxy (S1 f (K1 Generic.R (PrimaryKey related (TableField be table))) p))
              nonNullSettingsRep = from' nonNullSettings :: Rep (PrimaryKey related (TableField be table)) ()

              settings :: PrimaryKey related (Nullable (TableField be table))
              settings = to' (runIdentity (gZipTables (Proxy :: Proxy (Rep (PrimaryKey related Exposed))) removeNotNullConstraints nonNullSettingsRep nonNullSettingsRep))

              removeNotNullConstraints :: Columnar' (TableField be table) ty -> Columnar' (TableField be table) ty -> Identity (Columnar' (Nullable (TableField be table)) ty)
              removeNotNullConstraints (Columnar' tf) _ =
                  pure . Columnar' $
                  tf { _fieldSchema = maybeFieldSchema (_fieldSchema tf) }

class GTableSkeleton x where
    gTblSkeleton :: Proxy x -> x ()
instance GTableSkeleton p => GTableSkeleton (M1 t f p) where
    gTblSkeleton (_ :: Proxy (M1 t f p)) = M1 (gTblSkeleton (Proxy :: Proxy p))
instance GTableSkeleton U1 where
    gTblSkeleton _ = U1
instance (GTableSkeleton a, GTableSkeleton b) =>
    GTableSkeleton (a :*: b) where
        gTblSkeleton _ = gTblSkeleton (Proxy :: Proxy a) :*: gTblSkeleton (Proxy :: Proxy b)
instance GTableSkeleton (K1 Generic.R (Ignored field)) where
    gTblSkeleton _ = K1 Ignored
instance ( Generic (PrimaryKey related Ignored)
         , GTableSkeleton (Rep (PrimaryKey related Ignored)) ) =>
    GTableSkeleton (K1 Generic.R (PrimaryKey related Ignored)) where
    gTblSkeleton _ = K1 (to' (gTblSkeleton (Proxy :: Proxy (Rep (PrimaryKey related Ignored)))))
instance ( Generic (PrimaryKey related (Nullable Ignored))
         , GTableSkeleton (Rep (PrimaryKey related (Nullable Ignored))) ) =>
    GTableSkeleton (K1 Generic.R (PrimaryKey related (Nullable Ignored))) where
    gTblSkeleton _ = K1 (to' (gTblSkeleton (Proxy :: Proxy (Rep (PrimaryKey related (Nullable Ignored))))))

-- | Type class for types which can construct a default 'TableField' given a column name.
class HasDefaultFieldSchema be fs where
    defFieldSchema :: Proxy fs -> BackendColumnSchema be

instance (BeamColumnSchema (BackendColumnSchema be), HasDefaultFieldSchema be x) =>
  HasDefaultFieldSchema be (Auto x) where
  defFieldSchema _ = autoSchema (defFieldSchema (Proxy @x))

-- * Parsing tables from rows

-- instance (TableFromBackendLiterals be tbl, Beamable tbl, MakeBackendLiterals be tbl) => FromBackendLiterals be (tbl Identity) where
--     fromBackendLiterals = tableFromBackendLiterals
--     valuesNeeded _ _ = tableValuesNeeded (Proxy :: Proxy tbl)
--     toBackendLiterals = allBeamValues (\(Columnar' (BackendLiteral' b :: BackendLiteral' be a)) -> toBackendLiteral b) . makeBackendLiterals

-- instance (BeamBackend be, TableFromBackendLiterals be tbl, Beamable tbl, MakeBackendLiterals be tbl, FromBackendLiterals be (tbl Identity)) => FromBackendLiterals be (tbl (Nullable Identity)) where
--     valuesNeeded _ _ = tableValuesNeeded (Proxy :: Proxy tbl)
--     fromBackendLiterals =
--       do tbl <- fromBackendLiterals
--          case tbl of
--            Just tbl -> pure (changeBeamRep (\(Columnar' x) -> Columnar' (Just x)) (tbl :: tbl Identity))
--            Nothing -> pure (changeBeamRep (\_ -> Columnar' Nothing) (tblSkeleton :: TableSkeleton tbl))
--     toBackendLiterals tbl =
--       let values = allBeamValues (\(Columnar' b) -> isNothing b) tbl
--       in if all id values
--          then toBackendLiterals (Nothing :: Maybe (tbl Identity))
--          else toBackendLiterals (Just (changeBeamRep (\(Columnar' (Just x)) -> Columnar' x) tbl :: tbl Identity))

-- Internal functions

unCamelCase :: String -> [String]
unCamelCase "" = []
unCamelCase s
    | (comp@(_:_), next) <- break isUpper s =
          let next' = case next of
                        [] -> []
                        x:xs -> toLower x:xs
          in map toLower comp:unCamelCase next'
    | otherwise =
        let (comp@(_:_), next) = span isUpper s
            next' = case next of
                      [] -> []
                      x:xs -> toLower x:xs
        in map toLower comp:unCamelCase next'

-- | Camel casing magic for standard beam record field names.
--
--   All leading underscores are ignored. If what remains is camel-cased beam
--   will convert it to use underscores instead. If there are any underscores in
--   what remains, then the entire name (minus the leading underscares). If the
--   field name is solely underscores, beam will assume you know what you're
--   doing and include the full original name as the field name
unCamelCaseSel :: String -> String
unCamelCaseSel original =
  let symbolLeft = dropWhile (=='_') original
  in if null symbolLeft
     then original
     else if '_' `elem` symbolLeft
          then symbolLeft
          else case unCamelCase symbolLeft of
                 [] -> symbolLeft
                 [xs] -> xs
                 _:xs -> intercalate "_" xs

-- maybeFieldSchema :: FieldSchema be ty -> FieldSchema be (Maybe ty)
-- maybeFieldSchema base = let SQLColumnSchema desc isPrimaryKey isAuto _ constraints =  fsColDesc base
--                             in FieldSchema
--                                { fsColDesc = SQLColumnSchema desc isPrimaryKey isAuto True (constraintNullifiesAs constraints)
--                                , fsHumanReadable = "maybeFieldSchema (" ++ fsHumanReadable base ++ ")" }
