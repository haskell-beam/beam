{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Tables
    (
    -- * Database Types
      Database(..)
    , DatabaseSettings
    , IsDatabaseEntity(..)
    , DatabaseEntityDescriptor(..)
    , DatabaseEntity(..), TableEntity
    , dbEntityDescriptor
    , DatabaseModification, EntityModification(..)
    , FieldModification(..)
    , dbModification, tableModification, withDbModification
    , withTableModification, modifyTable, fieldNamed
    , defaultDbSettings

    , Lenses, LensFor(..)

    -- * Columnar and Column Tags
    , Columnar, Columnar'(..)
    , Nullable, TableField(..)
    , Exposed(..)
    , fieldName

    , TableSettings, TableSkeleton, Ignored(..)
    , GFieldsFulfillConstraint(..), FieldsFulfillConstraint, WithConstraint(..)
    , TagReducesTo(..), ReplaceBaseTag

    -- * Tables
    , Table(..), Beamable(..)
    , defTblFieldSettings
    , tableValuesNeeded
    , pk
    , allBeamValues, changeBeamRep )
    where

import           Database.Beam.Backend.Types

import           Control.Arrow (first)
import           Control.Monad.Identity
import           Control.Monad.Writer

import           Data.Char (isUpper, toUpper, toLower)
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import qualified GHC.Generics as Generic
import           GHC.Generics hiding (R)
import           GHC.TypeLits
import           GHC.Types

import           Lens.Micro hiding (to)
import qualified Lens.Micro as Lens

class Database db where
    zipTables :: forall be f g h m.
                 Monad m
              => Proxy be
              -> (forall tbl. (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl) =>
                  f tbl -> g tbl -> m (h tbl))
              -> db f -> db g -> m (db h)
    default zipTables :: forall be f g h m.
                         ( Generic (db f), Generic (db g), Generic (db h)
                         , Monad m
                         , GZipDatabase be f g h
                                        (Rep (db f)) (Rep (db g)) (Rep (db h)) ) =>
                         Proxy be ->
                         (forall tbl. (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl) => f tbl -> g tbl -> m (h tbl)) ->
                         db f -> db g -> m (db h)
    zipTables _ combine f g =
        to <$> gZipDatabase (Proxy @f, Proxy @g, Proxy @h, Proxy @be) combine (from f) (from g)

defaultDbSettings :: ( Generic (DatabaseSettings be db)
                     , GAutoDbSettings (Rep (DatabaseSettings be db) ()) ) =>
                     DatabaseSettings be db
defaultDbSettings = to' autoDbSettings'

type DatabaseModification f be db = db (EntityModification f be)
newtype EntityModification f be e = EntityModification (f e -> f e)
newtype FieldModification f a
  = FieldModification (Columnar f a -> Columnar f a)

dbModification :: forall f be db. Database db => DatabaseModification f be  db
dbModification = runIdentity $
                 zipTables (Proxy @be) (\_ _ -> pure (EntityModification id)) (undefined :: DatabaseModification f be db) (undefined :: DatabaseModification f be db)

tableModification :: forall f tbl. Beamable tbl => tbl (FieldModification f)
tableModification = runIdentity $
                    zipBeamFieldsM (\(Columnar' _ :: Columnar' Ignored x) (Columnar' _ :: Columnar' Ignored x) ->
                                      pure (Columnar' (FieldModification id :: FieldModification f x))) (undefined :: TableSkeleton tbl) (undefined :: TableSkeleton tbl)

withDbModification :: forall db f be.
                      Database db => db f -> DatabaseModification f be db -> db f
withDbModification db mods =
  runIdentity $ zipTables (Proxy @be) (\tbl (EntityModification mod) -> pure (mod tbl)) db mods

withTableModification :: Beamable tbl => tbl (FieldModification f) -> tbl f -> tbl f
withTableModification mods tbl =
  runIdentity $ zipBeamFieldsM (\(Columnar' field :: Columnar' f a) (Columnar' (FieldModification mod :: FieldModification f a)) ->
                                  pure (Columnar' (mod field))) tbl mods

modifyTable :: (Text -> Text)
            -> tbl (FieldModification (TableField tbl))
            -> EntityModification (DatabaseEntity be db) be (TableEntity tbl)
modifyTable modTblNm modFields =
  EntityModification (\(DatabaseEntity (DatabaseTable nm fields)) ->
                         (DatabaseEntity (DatabaseTable (modTblNm nm) (withTableModification modFields fields))))

fieldNamed :: Text -> FieldModification (TableField tbl) a
fieldNamed newName = FieldModification (\_ -> TableField newName)

instance IsString (FieldModification (TableField tbl) a) where
  fromString = fieldNamed . fromString

-- * Database entity types
data TableEntity (tbl :: (* -> *) -> *)
data ViewEntity (view :: (* -> *) -> *)
data UniqueConstraint (tbl :: (* -> *) -> *) (c :: (* -> *) -> *)
data DomainType (ty :: *)
data CharacterSetEntity
data CollationEntity
data TranslationEntity
data AssertionEntity

class IsDatabaseEntity be entityType where
  data DatabaseEntityDescriptor be entityType :: *
  type DatabaseEntityDefaultRequirements be entityType :: Constraint
  type DatabaseEntityRegularRequirements be entityType :: Constraint

  dbEntityName :: Lens' (DatabaseEntityDescriptor be entityType) Text
  dbEntityAuto :: DatabaseEntityDefaultRequirements be entityType =>
                  Text -> DatabaseEntityDescriptor be entityType

instance IsDatabaseEntity be (TableEntity tbl) where
  data DatabaseEntityDescriptor be (TableEntity tbl) where
    DatabaseTable :: Table tbl => Text -> TableSettings tbl -> DatabaseEntityDescriptor be (TableEntity tbl)
  type DatabaseEntityDefaultRequirements be (TableEntity tbl) =
    ( GDefaultTableFieldSettings (Rep (TableSettings tbl) ())
    , Generic (TableSettings tbl), Table tbl, Beamable tbl )
  type DatabaseEntityRegularRequirements be (TableEntity tbl) =
    ( Table tbl, Beamable tbl )

  dbEntityName f (DatabaseTable t s) = fmap (\t' -> DatabaseTable t' s) (f t)
  dbEntityAuto nm =
    DatabaseTable (unCamelCaseSel nm) defTblFieldSettings

data DatabaseEntity be (db :: (* -> *) -> *) entityType  where
    DatabaseEntity ::
      IsDatabaseEntity be entityType =>
      DatabaseEntityDescriptor be entityType ->  DatabaseEntity be db entityType

dbEntityDescriptor :: SimpleGetter (DatabaseEntity be db entityType) (DatabaseEntityDescriptor be entityType)
dbEntityDescriptor = Lens.to (\(DatabaseEntity e) -> e)

-- tableName :: Lens' (DatabaseTable db entity table) Text
-- tableName f (DatabaseTable proxy name settings) = (\name' -> DatabaseTable proxy name' settings) <$> f name
-- tableSettings :: Lens' (DatabaseTable db entity table) (TableSettings table)
-- tableSettings f (DatabaseTable proxy name settings) = (\settings' -> DatabaseTable proxy name settings') <$> f settings

type DatabaseSettings be db = db (DatabaseEntity be db)

class GAutoDbSettings x where
    autoDbSettings' :: x
instance GAutoDbSettings (x p) => GAutoDbSettings (D1 f x p) where
    autoDbSettings' = M1 autoDbSettings'
instance GAutoDbSettings (x p) => GAutoDbSettings (C1 f x p) where
    autoDbSettings' = M1 autoDbSettings'
instance (GAutoDbSettings (x p), GAutoDbSettings (y p)) => GAutoDbSettings ((x :*: y) p) where
    autoDbSettings' = autoDbSettings' :*: autoDbSettings'
instance ( Selector f, IsDatabaseEntity be x, DatabaseEntityDefaultRequirements be x ) =>
  GAutoDbSettings (S1 f (K1 Generic.R (DatabaseEntity be db x)) p) where
  autoDbSettings' = M1 (K1 (DatabaseEntity (dbEntityAuto name)))
    where name = T.pack (selName (undefined :: S1 f (K1 Generic.R (DatabaseEntity be db x)) p))

class GZipDatabase be f g h x y z where
  gZipDatabase :: Monad m =>
                  (Proxy f, Proxy g, Proxy h, Proxy be)
               -> (forall tbl. (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl) => f tbl -> g tbl -> m (h tbl))
               -> x () -> y () -> m (z ())
instance GZipDatabase be f g h x y z =>
  GZipDatabase be f g h (M1 a b x) (M1 a b y) (M1 a b z) where
  gZipDatabase p combine ~(M1 f) ~(M1 g) = M1 <$> gZipDatabase p combine f g
instance ( GZipDatabase be f g h ax ay az
         , GZipDatabase be f g h bx by bz ) =>
  GZipDatabase be f g h (ax :*: bx) (ay :*: by) (az :*: bz) where
  gZipDatabase p combine ~(ax :*: bx) ~(ay :*: by) =
    do a <- gZipDatabase p combine ax ay
       b <- gZipDatabase p combine bx by
       pure (a :*: b)
instance (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl) =>
  GZipDatabase be f g h (K1 Generic.R (f tbl)) (K1 Generic.R (g tbl)) (K1 Generic.R (h tbl)) where

  gZipDatabase _ combine ~(K1 x) ~(K1 y) =
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

-- | Metadata for a field of type 'ty' in 'table'.
--
-- > Columnar (TableField table) ty = TableField table ty
--
--   This is used to declare 'tblFieldSettings' in the 'Table' class.
--
--   It is easiest to access these fields through the lenses 'fieldName', 'fieldConstraints', and 'fieldSettings'.
--
-- > data EmployeeT f = Employee
-- >                  { _employeeId :: Columnar f (Auto Int)
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
data TableField (table :: (* -> *) -> *) ty = TableField
                                            { _fieldName        :: Text                   -- ^ The field name
                                            } deriving (Show, Eq)

fieldName :: Lens' (TableField table ty) Text
fieldName f (TableField name) = TableField <$> f name

type TableSettings table = table (TableField table)

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

tableValuesNeeded :: Beamable table => Proxy table -> Int
tableValuesNeeded (Proxy :: Proxy table) = length (allBeamValues (const ()) (tblSkeleton :: TableSkeleton table))

allBeamValues :: Beamable table => (forall a. Columnar' f a -> b) -> table f -> [b]
allBeamValues (f :: forall a. Columnar' f a -> b) (tbl :: table f) =
    execWriter (zipBeamFieldsM combine tbl tbl)
    where combine :: Columnar' f a -> Columnar' f a -> Writer [b] (Columnar' f a)
          combine x _ = do tell [f x]
                           return x

changeBeamRep :: Beamable table => (forall a. Columnar' f a -> Columnar' g a) -> table f -> table g
changeBeamRep f tbl = runIdentity (zipBeamFieldsM (\x _ -> return (f x)) tbl tbl)

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

-- | Synonym for 'primaryKey'
pk :: Table t => t f -> PrimaryKey t f
pk = primaryKey

defTblFieldSettings :: ( Generic (TableSettings table)
                       , GDefaultTableFieldSettings (Rep (TableSettings table) ())) =>
                       TableSettings table
defTblFieldSettings = withProxy $ \proxy -> to' (gDefTblFieldSettings proxy)
    where withProxy :: (Proxy (Rep (TableSettings table) ()) -> TableSettings table) -> TableSettings table
          withProxy f = f Proxy

class GZipTables f g h (exposedRep :: * -> *) fRep gRep hRep where
    gZipTables :: Applicative m => Proxy exposedRep -> (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> fRep () -> gRep () -> m (hRep ())
instance ( GZipTables f g h exp1 f1 g1 h1
         , GZipTables f g h exp2 f2 g2 h2) =>
    GZipTables f g h (exp1 :*: exp2) (f1 :*: f2) (g1 :*: g2) (h1 :*: h2) where

        gZipTables _ combine ~(f1 :*: f2) ~(g1 :*: g2) =
            (:*:) <$> gZipTables (Proxy :: Proxy exp1) combine f1 g1
                  <*> gZipTables (Proxy :: Proxy exp2) combine f2 g2
instance GZipTables f g h exp fRep gRep hRep =>
    GZipTables f g h (M1 x y exp) (M1 x y fRep) (M1 x y gRep) (M1 x y hRep) where
        gZipTables _ combine ~(M1 f) ~(M1 g) = M1 <$> gZipTables (Proxy :: Proxy exp) combine f g
instance ( fa ~ Columnar f a
         , ga ~ Columnar g a
         , ha ~ Columnar h a) =>
    GZipTables f g h (K1 Generic.R (Exposed a)) (K1 Generic.R fa) (K1 Generic.R ga) (K1 Generic.R ha) where
        gZipTables _ combine ~(K1 f) ~(K1 g) = (\(Columnar' h) -> K1 h) <$> combine (Columnar' f :: Columnar' f a) (Columnar' g :: Columnar' g a)
--                                                return (K1 (h :: Columnar h a))
instance ( Generic (tbl f)
         , Generic (tbl g)
         , Generic (tbl h)

         , GZipTables f g h (Rep (tbl Exposed)) (Rep (tbl f)) (Rep (tbl g)) (Rep (tbl h))) =>
    GZipTables f g h (K1 Generic.R (tbl Exposed)) (K1 Generic.R (tbl f)) (K1 Generic.R (tbl g)) (K1 Generic.R (tbl h)) where
    gZipTables _ combine ~(K1 f) ~(K1 g) = K1 . to' <$> gZipTables (Proxy :: Proxy (Rep (tbl Exposed))) combine (from' f) (from' g)

instance  ( Generic (tbl (Nullable f))
          , Generic (tbl (Nullable g))
          , Generic (tbl (Nullable h))

          , GZipTables f g h (Rep (tbl (Nullable Exposed))) (Rep (tbl (Nullable f))) (Rep (tbl (Nullable g))) (Rep (tbl (Nullable h)))) =>
         GZipTables f g h
                    (K1 Generic.R (tbl (Nullable Exposed)))
                    (K1 Generic.R (tbl (Nullable f)))
                    (K1 Generic.R (tbl (Nullable g)))
                    (K1 Generic.R (tbl (Nullable h))) where
    gZipTables _ combine ~(K1 f) ~(K1 g) = K1 . to' <$> gZipTables (Proxy :: Proxy (Rep (tbl (Nullable Exposed)))) combine (from' f) (from' g)

class GDefaultTableFieldSettings x where
    gDefTblFieldSettings :: Proxy x -> x
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (D1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (D1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (C1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (C1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance (GDefaultTableFieldSettings (a p), GDefaultTableFieldSettings (b p)) => GDefaultTableFieldSettings ((a :*: b) p) where
    gDefTblFieldSettings (_ :: Proxy ((a :*: b) p)) = gDefTblFieldSettings (Proxy :: Proxy (a p)) :*: gDefTblFieldSettings (Proxy :: Proxy (b p))

instance Selector f  =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (TableField table field)) p) where
    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (TableField table field)) p)) = M1 (K1 s)
        where s = TableField name
              name = unCamelCaseSel (T.pack (selName (undefined :: S1 f (K1 Generic.R (TableField table field)) ())))

-- instance {-# OVERLAPPING #-}
--          ( Selector f'
--          , Table rel
--          , Generic (rel f)
--          , Generic (PrimaryKey rel f)
--          , TagReducesTo f (TableField tbl)

--          , GDefaultTableFieldSettings (Rep (rel f) ())
--          , Beamable rel ) =>
--     GDefaultTableFieldSettings (S1 f' (K1 Generic.R (PrimaryKey rel f)) p) where

--     gDefTblFieldSettings _ = M1 . K1 $ pkSettings'
--         where tbl :: rel f
--               tbl = to' $ gDefTblFieldSettings (Proxy @(Rep (rel f) ()))
--               pkSettings = primaryKey tbl

--               relName = unCamelCaseSel (T.pack (selName (undefined :: S1 f' (K1 Generic.R (PrimaryKey rel (TableField tbl))) p)))

--               pkSettings' :: PrimaryKey rel f
--               pkSettings' = changeBeamRep (reduceTag %~ \(Columnar' (TableField nm)) -> Columnar' $ TableField (relName <> "__" <> nm)) pkSettings

instance ( TypeError ('Text "All Beamable types must be record types, so appropriate names can be given to columns")) => GDefaultTableFieldSettings (K1 r f p) where
  gDefTblFieldSettings _ = error "impossible"

-- | Type-level representation of the naming strategy to use for defaulting
--   Needed because primary keys should be named after the default naming of
--   their corresponding table, not the names of the record selectors in the
--   primary key (if any).
data SubTableStrategy
  = PrimaryKeyStrategy
  | BeamableStrategy
  | RecursiveKeyStrategy

type family ChooseSubTableStrategy (tbl :: (* -> *) -> *) (sub :: (* -> *) -> *) :: SubTableStrategy where
  ChooseSubTableStrategy tbl (PrimaryKey tbl) = 'RecursiveKeyStrategy
  ChooseSubTableStrategy tbl (PrimaryKey rel) = 'PrimaryKeyStrategy
  ChooseSubTableStrategy tbl sub = 'BeamableStrategy

-- TODO is this necessary
type family CheckNullable (f :: * -> *) :: Constraint where
  CheckNullable (Nullable f) = ()
  CheckNullable f = TypeError ('Text "Recursive reference without Nullable constraint forms an infinite loop." ':$$:
                               'Text "Hint: Only embed nullable 'PrimaryKey tbl' within the definition of 'tbl'." ':$$:
                               'Text "      For example, replace 'PrimaryKey tbl f' with 'PrimaryKey tbl (Nullable f)'")


class SubTableStrategyImpl (strategy :: SubTableStrategy) (f :: * -> *) sub where
  namedSubTable :: Proxy strategy -> sub f

-- The defaulting with TAbleField rel is necessary to avoid infinite loops
instance ( Table rel, Generic (rel (TableField rel))
         , TagReducesTo f (TableField tbl)
         , GDefaultTableFieldSettings (Rep (rel (TableField rel)) ()) ) =>
  SubTableStrategyImpl 'PrimaryKeyStrategy f (PrimaryKey rel) where
  namedSubTable _ = primaryKey tbl
    where tbl = changeBeamRep (\(Columnar' (TableField nm) :: Columnar' (TableField rel) a) ->
                                  let c = Columnar' (TableField nm) :: Columnar' (TableField tbl) a
                                  in runIdentity (reduceTag (\_ -> pure c) undefined)) $
                to' $ gDefTblFieldSettings (Proxy @(Rep (rel (TableField rel)) ()))
instance ( Generic (sub f)
         , GDefaultTableFieldSettings (Rep (sub f) ()) ) =>
         SubTableStrategyImpl 'BeamableStrategy f sub where
  namedSubTable _ = to' $ gDefTblFieldSettings (Proxy @(Rep (sub f) ()))
instance ( CheckNullable f, SubTableStrategyImpl 'PrimaryKeyStrategy f (PrimaryKey rel) ) =>
         SubTableStrategyImpl 'RecursiveKeyStrategy f (PrimaryKey rel) where
  namedSubTable _ = namedSubTable (Proxy @'PrimaryKeyStrategy)

instance {-# OVERLAPPING #-}
         ( Selector f'
         , ChooseSubTableStrategy tbl sub ~ strategy
         , SubTableStrategyImpl strategy f sub
         , TagReducesTo f (TableField tbl)
         , Beamable sub ) =>
         GDefaultTableFieldSettings (S1 f' (K1 Generic.R (sub f)) p) where
  gDefTblFieldSettings _ = M1 . K1 $ settings'
    where tbl :: sub f
          tbl = namedSubTable (Proxy @strategy)

          relName = unCamelCaseSel (T.pack (selName (undefined :: S1 f' (K1 Generic.R (sub f)) p)))

          settings' :: sub f
          settings' = changeBeamRep (reduceTag %~ \(Columnar' (TableField nm)) -> Columnar' (TableField (relName <> "__" <> nm))) tbl

type family ReplaceBaseTag tag f where
  ReplaceBaseTag tag (Nullable f) = Nullable (ReplaceBaseTag tag f)
  ReplaceBaseTag tag x = tag

-- | Class to automatically unwrap nested Nullables
class TagReducesTo f f' | f -> f' where
  reduceTag :: Functor m =>
               (Columnar' f' a' -> m (Columnar' f' a'))
            -> Columnar' f a -> m (Columnar' f a)
instance TagReducesTo (TableField tbl) (TableField tbl) where
  reduceTag f ~(Columnar' (TableField nm)) =
    (\(Columnar' (TableField nm)) -> Columnar' (TableField nm)) <$>
    f (Columnar' (TableField nm))
instance TagReducesTo f f' => TagReducesTo (Nullable f) f' where
  reduceTag fn ~(Columnar' x :: Columnar' (Nullable f) a) =
    (\(Columnar' x' :: Columnar' f (Maybe a')) -> Columnar' x') <$>
          reduceTag fn (Columnar' x :: Columnar' f (Maybe a))

-- instance (GDefaultTableFieldSettings (S1 f (K1 Generic.R (rel q)) p)) =>
--   GDefaultTableFieldSettings (S1 f (K1 Generic.R (rel (Nullable q))) p) where

--   gDefTblFieldSettings _ = M1 . K1 . magic (\(TableField x) -> TableField x) $ q
--     where q :: rel q
--           M1 (K1 q) = gDefTblFieldSettings (Proxy @(S1 f (K1 Generic.R (rel q)) p))

-- instance ( Table table, Table related
--          , BeamBackend be
--          , Selector f

--          , Generic (PrimaryKey related (TableField be related))
--          , Generic (PrimaryKey related (TableField be table))
--          , Generic (TableSettings be related)
--          , GDefaultTableFieldSettings (Rep (TableSettings be related) ())
--          , GZipTables (TableField be related) (TableField be related) (TableField be table)
--                       (Rep (PrimaryKey related Exposed))
--                       (Rep (PrimaryKey related (TableField be related))) (Rep (PrimaryKey related (TableField be related))) (Rep (PrimaryKey related (TableField be table))) ) =>
--     GDefaultTableFieldSettings (S1 f (K1 Generic.R (PrimaryKey related (TableField be table))) p) where

--     gDefTblFieldSettings _ = M1 . K1 $ primaryKeySettings'
--         where tableSettings = defTblFieldSettings :: TableSettings be related
--               primaryKeySettings :: PrimaryKey related (TableField be related)
--               primaryKeySettings = primaryKey tableSettings

--               primaryKeySettings' :: PrimaryKey related (TableField be table)
--               primaryKeySettings' = to' (runIdentity (gZipTables (Proxy :: Proxy (Rep (PrimaryKey related Exposed))) convertToForeignKeyField (from' primaryKeySettings) (from' primaryKeySettings)))

--               convertToForeignKeyField :: Columnar' (TableField be related) c -> Columnar' (TableField be related) c -> Identity (Columnar' (TableField be table) c)
--               convertToForeignKeyField (Columnar' tf) _ =
--                   pure . Columnar' $
--                   tf { _fieldName = keyName <> "__" <> _fieldName tf
--                      , _fieldSchema = nestedSchema (_fieldSchema tf :: BackendColumnSchema be) }

--               keyName = T.pack (unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (PrimaryKey related (TableField be table))) ())))

-- instance ( Table table, Table related
--          , Selector f
--          , BeamBackend be

--          , Generic (PrimaryKey related (TableField be related))
--          , Generic (PrimaryKey related (TableField be table))
--          , Generic (PrimaryKey related (Nullable (TableField be table)))
--          , Generic (TableSettings be related)
--          , GDefaultTableFieldSettings (Rep (TableSettings be related) ())
--          , GZipTables (TableField be table) (TableField be table) (Nullable (TableField be table))
--                       (Rep (PrimaryKey related Exposed))
--                       (Rep (PrimaryKey related (TableField be table)))
--                       (Rep (PrimaryKey related (TableField be table)))
--                       (Rep (PrimaryKey related (Nullable (TableField be table))))
--          , GZipTables (TableField be related) (TableField be related) (TableField be table)
--                       (Rep (PrimaryKey related Exposed))
--                       (Rep (PrimaryKey related (TableField be related)))
--                       (Rep (PrimaryKey related (TableField be related)))
--                       (Rep (PrimaryKey related (TableField be table)))) =>
--     GDefaultTableFieldSettings (S1 f (K1 Generic.R (PrimaryKey related (Nullable (TableField be table)))) p) where

--     gDefTblFieldSettings _ =
--         M1 . K1 $ settings
--         where M1 (K1 nonNullSettings) = gDefTblFieldSettings (Proxy :: Proxy (S1 f (K1 Generic.R (PrimaryKey related (TableField be table))) p))
--               nonNullSettingsRep = from' nonNullSettings :: Rep (PrimaryKey related (TableField be table)) ()

--               settings :: PrimaryKey related (Nullable (TableField be table))
--               settings = to' (runIdentity (gZipTables (Proxy :: Proxy (Rep (PrimaryKey related Exposed))) removeNotNullConstraints nonNullSettingsRep nonNullSettingsRep))

--               removeNotNullConstraints :: Columnar' (TableField be table) ty -> Columnar' (TableField be table) ty -> Identity (Columnar' (Nullable (TableField be table)) ty)
--               removeNotNullConstraints (Columnar' tf) _ =
--                   pure . Columnar' $
--                   tf { _fieldSchema = maybeFieldSchema (_fieldSchema tf) }

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
instance ( Generic (tbl Ignored)
         , GTableSkeleton (Rep (tbl Ignored)) ) =>
    GTableSkeleton (K1 Generic.R (tbl Ignored)) where
    gTblSkeleton _ = K1 (to' (gTblSkeleton (Proxy :: Proxy (Rep (tbl Ignored)))))
instance ( Generic (tbl (Nullable Ignored))
         , GTableSkeleton (Rep (tbl (Nullable Ignored))) ) =>
    GTableSkeleton (K1 Generic.R (tbl (Nullable Ignored))) where
    gTblSkeleton _ = K1 (to' (gTblSkeleton (Proxy :: Proxy (Rep (tbl (Nullable Ignored))))))

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

unCamelCase :: T.Text -> [T.Text]
unCamelCase "" = []
unCamelCase s
    | (comp, next) <- T.break isUpper s, not (T.null comp) =
          let next' = maybe mempty (uncurry T.cons . first toLower) (T.uncons next)
          in T.toLower comp:unCamelCase next'
    | otherwise =
          let (comp, next) = T.span isUpper s
              next' = maybe mempty (uncurry T.cons . first toLower) (T.uncons next)
          in T.toLower comp:unCamelCase next'

-- | Camel casing magic for standard beam record field names.
--
--   All leading underscores are ignored. If what remains is camel-cased beam
--   will convert it to use underscores instead. If there are any underscores in
--   what remains, then the entire name (minus the leading underscares). If the
--   field name is solely underscores, beam will assume you know what you're
--   doing and include the full original name as the field name
unCamelCaseSel :: Text -> Text
unCamelCaseSel original =
  let symbolLeft = T.dropWhile (=='_') original
  in if T.null symbolLeft
     then original
     else if T.any (=='_') symbolLeft
          then symbolLeft
          else case unCamelCase symbolLeft of
                 [] -> symbolLeft
                 [xs] -> xs
                 _:xs -> T.intercalate "_" xs
