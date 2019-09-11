{-# LANGUAGE CPP #-}
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
      Database
    , zipTables

    , DatabaseSettings
    , IsDatabaseEntity(..)
    , DatabaseEntityDescriptor(..)
    , DatabaseEntity(..), TableEntity, ViewEntity, DomainTypeEntity
    , dbEntityDescriptor
    , DatabaseModification, EntityModification(..)
    , FieldModification(..)
    , dbModification, tableModification, withDbModification
    , withTableModification, modifyTable, modifyEntityName
    , setEntityName, modifyTableFields, fieldNamed
    , modifyEntitySchema, setEntitySchema
    , defaultDbSettings

    , RenamableWithRule(..), RenamableField(..)
    , FieldRenamer(..)

    , Lenses, LensFor(..)

    -- * Columnar and Column Tags
    , Columnar, C, Columnar'(..)
    , ComposeColumnar(..)
    , Nullable, TableField(..)
    , Exposed
    , fieldName, fieldPath

    , TableSettings, HaskellTable
    , TableSkeleton, Ignored(..)
    , GFieldsFulfillConstraint(..), FieldsFulfillConstraint
    , FieldsFulfillConstraintNullable
    , WithConstraint(..)
    , TagReducesTo(..), ReplaceBaseTag
    , withConstrainedFields, withConstraints
    , withNullableConstrainedFields, withNullableConstraints

    -- * Tables
    , Table(..), Beamable(..)
    , Retaggable(..), (:*:)(..) -- Reexported for use with 'alongsideTable'
    , defTblFieldSettings
    , tableValuesNeeded
    , pk
    , allBeamValues, changeBeamRep
    , alongsideTable
    , defaultFieldName

    , ChooseSubTableStrategy
    , SubTableStrategy (..)
    )
    where

import           Database.Beam.Backend.Types

import           Control.Applicative (liftA2)
import           Control.Arrow (first)
import           Control.Monad.Identity
import           Control.Monad.Writer hiding ((<>))

import           Data.Char (isUpper, toLower)
import           Data.Foldable (fold)
import qualified Data.List.NonEmpty as NE
import           Data.Monoid (Endo(..))
import           Data.Proxy
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import qualified GHC.Generics as Generic
import           GHC.Generics hiding (R, C)
import           GHC.TypeLits
import           GHC.Types

import           Lens.Micro hiding (to)
import qualified Lens.Micro as Lens

-- | Allows introspection into database types.
--
--   All database types must be of kind '(* -> *) -> *'. If the type parameter
--   is named 'f', each field must be of the type of 'f' applied to some type
--   for which an 'IsDatabaseEntity' instance exists.
--
--   The 'be' type parameter is necessary so that the compiler can
--   ensure that backend-specific entities only work on the proper
--   backend.
--
--   Entities are documented under [the corresponding
--   section](Database.Beam.Schema#entities) and in the
--   [manual](http://tathougies.github.io/beam/user-guide/databases/)
class Database be db where

    -- | Default derived function. Do not implement this yourself.
    --
    --   The idea is that, for any two databases over particular entity tags 'f'
    --   and 'g', if we can take any entity in 'f' and 'g' to the corresponding
    --   entity in 'h' (in the possibly effectful monad 'm'), then we can
    --   transform the two databases over 'f' and 'g' to a database in 'h',
    --   within the monad 'm'.
    --
    --   If that doesn't make sense, don't worry. This is mostly beam internal
    zipTables :: Applicative m
              => Proxy be
              -> (forall tbl. (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl) =>
                  f tbl -> g tbl -> m (h tbl))
              -> db f -> db g -> m (db h)
    default zipTables :: ( Generic (db f), Generic (db g), Generic (db h)
                         , Applicative m
                         , GZipDatabase be f g h
                                        (Rep (db f)) (Rep (db g)) (Rep (db h)) ) =>
                         Proxy be ->
                         (forall tbl. (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl) => f tbl -> g tbl -> m (h tbl)) ->
                         db f -> db g -> m (db h)
    -- We need the pattern type signature on 'combine' to get around a type checking bug in GHC 8.0.1. In future releases,
    -- we will switch to the standard forall.
    zipTables be combine (f :: db f) (g :: db g) =
      refl $ \h ->
        to <$> gZipDatabase (Proxy @f, Proxy @g, h, be) combine (from f) (from g)
      where
        -- For GHC 8.0.1 renamer bug
        refl :: (Proxy h -> m (db h)) -> m (db h)
        refl fn = fn Proxy

-- | Automatically provide names for tables, and descriptions for tables (using
--   'defTblFieldSettings'). Your database must implement 'Generic', and must be
--   auto-derivable. For more information on name generation, see the
--   [manual](https://tathougies.github.io/beam/user-guide/models)
defaultDbSettings :: ( Generic (DatabaseSettings be db)
                     , GAutoDbSettings (Rep (DatabaseSettings be db) ()) ) =>
                     DatabaseSettings be db
defaultDbSettings = to' autoDbSettings'

-- | A helper data type that lets you modify a database schema. Converts all
-- entities in the database into functions from that entity to itself.
type DatabaseModification f be db = db (EntityModification f be)
-- | A newtype wrapper around 'f e -> f e' (i.e., an endomorphism between entity
--   types in 'f'). You usually want to use 'modifyTable' or another function to
--   contstruct these for you.
newtype EntityModification f be e = EntityModification (Endo (f e))
  deriving (Monoid, Semigroup)
-- | A newtype wrapper around 'Columnar f a -> Columnar f ' (i.e., an
--   endomorphism between 'Columnar's over 'f'). You usually want to use
--   'fieldNamed' or the 'IsString' instance to rename the field, when 'f ~
--   TableField'
newtype FieldModification f a
  = FieldModification (Columnar f a -> Columnar f a)

-- | Return a 'DatabaseModification' that does nothing. This is useful if you
--   only want to rename one table. You can do
--
-- > dbModification { tbl1 = modifyTable (\oldNm -> "NewTableName") tableModification }
dbModification :: forall f be db. Database be db => DatabaseModification f be db
dbModification = runIdentity $
                 zipTables (Proxy @be) (\_ _ -> pure mempty) (undefined :: DatabaseModification f be db) (undefined :: DatabaseModification f be db)

-- | Return a table modification (for use with 'modifyTable') that does nothing.
--   Useful if you only want to change the table name, or if you only want to
--   modify a few fields.
--
--   For example,
--
-- > tableModification { field1 = "Column1" }
--
--   is a table modification (where 'f ~ TableField tbl') that changes the
--   column name of 'field1' to "Column1".
tableModification :: forall f tbl. Beamable tbl => tbl (FieldModification f)
tableModification = runIdentity $
                    zipBeamFieldsM (\(Columnar' _ :: Columnar' Ignored x) (Columnar' _ :: Columnar' Ignored x) ->
                                      pure (Columnar' (FieldModification id :: FieldModification f x))) (undefined :: TableSkeleton tbl) (undefined :: TableSkeleton tbl)

-- | Modify a database according to a given modification. Most useful for
--   'DatabaseSettings' to change the name mappings of tables and fields. For
--   example, you can use this to modify the default names of a table
--
-- > db :: DatabaseSettings MyDb
-- > db = defaultDbSettings `withDbModification`
-- >      dbModification {
-- >        -- Change default name "table1" to "Table_1". Change the name of "table1Field1" to "first_name"
-- >        table1 = modifyTable (\_ -> "Table_1") (tableModification { table1Field1 = "first_name" }
-- >      }
withDbModification :: forall db be entity
                    . Database be db
                   => db (entity be db)
                   -> DatabaseModification (entity be db) be db
                   -> db (entity be db)
withDbModification db mods =
  runIdentity $ zipTables (Proxy @be) (\tbl (EntityModification entityFn) -> pure (appEndo entityFn tbl)) db mods

-- | Modify a table according to the given field modifications. Invoked by
--   'modifyTable' to apply the modification in the database. Not used as often in
--   user code, but provided for completeness.
withTableModification :: Beamable tbl => tbl (FieldModification f) -> tbl f -> tbl f
withTableModification mods tbl =
  runIdentity $ zipBeamFieldsM (\(Columnar' field :: Columnar' f a) (Columnar' (FieldModification fieldFn :: FieldModification f a)) ->
                                  pure (Columnar' (fieldFn field))) tbl mods


-- | Provide an 'EntityModification' for 'TableEntity's. Allows you to modify
--   the name of the table and provide a modification for each field in the
--   table. See the examples for 'withDbModification' for more.
modifyTable :: (Beamable tbl, Table tbl)
            => (Text -> Text)
            -> tbl (FieldModification (TableField tbl))
            -> EntityModification (DatabaseEntity be db) be (TableEntity tbl)
modifyTable modTblNm modFields = modifyEntityName modTblNm <> modifyTableFields modFields
{-# DEPRECATED modifyTable "Instead of 'modifyTable fTblNm fFields', use 'modifyEntityName _ <> modifyTableFields _'" #-}

-- | Construct an 'EntityModification' to rename any database entity
modifyEntityName :: IsDatabaseEntity be entity => (Text -> Text) -> EntityModification (DatabaseEntity be db) be entity
modifyEntityName modTblNm = EntityModification (Endo (\(DatabaseEntity tbl) -> DatabaseEntity (tbl & dbEntityName %~ modTblNm)))

-- | Construct an 'EntityModification' to set the schema of a database entity
modifyEntitySchema :: IsDatabaseEntity be entity => (Maybe Text -> Maybe Text) -> EntityModification (DatabaseEntity be db) be entity
modifyEntitySchema modSchema = EntityModification (Endo (\(DatabaseEntity tbl) -> DatabaseEntity (tbl & dbEntitySchema %~ modSchema)))

-- | Change the entity name without consulting the beam-assigned one
setEntityName :: IsDatabaseEntity be entity => Text -> EntityModification (DatabaseEntity be db) be entity
setEntityName nm = modifyEntityName (\_ -> nm)

setEntitySchema :: IsDatabaseEntity be entity => Maybe Text -> EntityModification (DatabaseEntity be db) be entity
setEntitySchema nm = modifyEntitySchema (\_ -> nm)

-- | Construct an 'EntityModification' to rename the fields of a 'TableEntity'
modifyTableFields :: tbl (FieldModification (TableField tbl)) -> EntityModification (DatabaseEntity be db) be (TableEntity tbl)
modifyTableFields modFields = EntityModification (Endo (\(DatabaseEntity tbl@(DatabaseTable {})) -> DatabaseEntity tbl { dbTableSettings = withTableModification modFields (dbTableSettings tbl) }))

-- | A field modification to rename the field. Also offered under the 'IsString'
--   instance for 'FieldModification (TableField tbl) a' for convenience.
fieldNamed :: Text -> FieldModification (TableField tbl) a
fieldNamed newName = FieldModification (fieldName .~ newName)

newtype FieldRenamer entity = FieldRenamer { withFieldRenamer :: entity -> entity }

class RenamableField f where
  renameField :: Proxy f -> Proxy a -> (NE.NonEmpty Text -> Text) -> Columnar f a -> Columnar f a
instance RenamableField (TableField tbl) where
  renameField _ _ f (TableField path _) = TableField path (f path)

class RenamableWithRule mod where
  renamingFields :: (NE.NonEmpty Text -> Text) -> mod
instance Database be db => RenamableWithRule (db (EntityModification (DatabaseEntity be db) be)) where
  renamingFields renamer =
    runIdentity $
    zipTables (Proxy @be) (\_ _ -> pure (renamingFields renamer))
              (undefined :: DatabaseModification f be db)
              (undefined :: DatabaseModification f be db)
instance IsDatabaseEntity be entity => RenamableWithRule (EntityModification (DatabaseEntity be db) be entity) where
  renamingFields renamer =
    EntityModification (Endo (\(DatabaseEntity tbl) -> DatabaseEntity (withFieldRenamer (renamingFields renamer) tbl)))
instance (Beamable tbl, RenamableField f) => RenamableWithRule (tbl (FieldModification f)) where
  renamingFields renamer =
    runIdentity $
    zipBeamFieldsM (\(Columnar' _ :: Columnar' Ignored x) (Columnar' _ :: Columnar' Ignored x) ->
                       pure (Columnar' (FieldModification (renameField (Proxy @f) (Proxy @x) renamer) :: FieldModification f x) ::
                               Columnar' (FieldModification f) x))
                   (undefined :: TableSkeleton tbl) (undefined :: TableSkeleton tbl)

instance IsString (FieldModification (TableField tbl) a) where
  fromString = fieldNamed . fromString

-- * Database entity types

-- | An entity tag for tables. See the documentation for 'Table' or consult the
--   [manual](https://tathougies.github.io/beam/user-guide/models) for more.
data TableEntity (tbl :: (* -> *) -> *)
data ViewEntity (view :: (* -> *) -> *)
--data UniqueConstraint (tbl :: (* -> *) -> *) (c :: (* -> *) -> *)
data DomainTypeEntity (ty :: *)
--data CharacterSetEntity
--data CollationEntity
--data TranslationEntity
--data AssertionEntity

class RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor be entityType)) =>
    IsDatabaseEntity be entityType where
  data DatabaseEntityDescriptor be entityType :: *
  type DatabaseEntityDefaultRequirements be entityType :: Constraint
  type DatabaseEntityRegularRequirements be entityType :: Constraint

  dbEntityName :: Lens' (DatabaseEntityDescriptor be entityType) Text
  dbEntitySchema :: Traversal' (DatabaseEntityDescriptor be entityType) (Maybe Text)

  dbEntityAuto :: DatabaseEntityDefaultRequirements be entityType =>
                  Text -> DatabaseEntityDescriptor be entityType

instance Beamable tbl => RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor be (TableEntity tbl))) where
  renamingFields renamer =
    FieldRenamer $ \tbl ->
      tbl { dbTableSettings =
              changeBeamRep (\(Columnar' tblField :: Columnar' (TableField tbl) a) ->
                               Columnar' (renameField (Proxy @(TableField tbl))
                                                      (Proxy @a)
                                                      renamer tblField)
                                 :: Columnar' (TableField tbl) a) $
              dbTableSettings tbl }

instance Beamable tbl => IsDatabaseEntity be (TableEntity tbl) where
  data DatabaseEntityDescriptor be (TableEntity tbl) where
    DatabaseTable
      :: Table tbl =>
       { dbTableSchema      :: Maybe Text
       , dbTableOrigName    :: Text
       , dbTableCurrentName :: Text
       , dbTableSettings    :: TableSettings tbl }
      -> DatabaseEntityDescriptor be (TableEntity tbl)
  type DatabaseEntityDefaultRequirements be (TableEntity tbl) =
    ( GDefaultTableFieldSettings (Rep (TableSettings tbl) ())
    , Generic (TableSettings tbl), Table tbl, Beamable tbl )
  type DatabaseEntityRegularRequirements be (TableEntity tbl) =
    ( Table tbl, Beamable tbl )

  dbEntityName f tbl = fmap (\t' -> tbl { dbTableCurrentName = t' }) (f (dbTableCurrentName tbl))
  dbEntitySchema f tbl = fmap (\s' -> tbl { dbTableSchema = s'}) (f (dbTableSchema tbl))
  dbEntityAuto nm =
    DatabaseTable Nothing nm (unCamelCaseSel nm) defTblFieldSettings

instance Beamable tbl => RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor be (ViewEntity tbl))) where
  renamingFields renamer =
    FieldRenamer $ \vw ->
      vw { dbViewSettings =
             changeBeamRep (\(Columnar' tblField :: Columnar' (TableField tbl) a) ->
                              Columnar' (renameField (Proxy @(TableField tbl))
                                                     (Proxy @a)
                                                     renamer tblField)
                                :: Columnar' (TableField tbl) a) $
             dbViewSettings vw }

instance Beamable tbl => IsDatabaseEntity be (ViewEntity tbl) where
  data DatabaseEntityDescriptor be (ViewEntity tbl) where
    DatabaseView
      :: { dbViewSchema :: Maybe Text
         , dbViewOrigName :: Text
         , dbViewCurrentName :: Text
         , dbViewSettings :: TableSettings tbl }
      -> DatabaseEntityDescriptor be (ViewEntity tbl)
  type DatabaseEntityDefaultRequirements be (ViewEntity tbl) =
    ( GDefaultTableFieldSettings (Rep (TableSettings tbl) ())
    , Generic (TableSettings tbl), Beamable tbl )
  type DatabaseEntityRegularRequirements be (ViewEntity tbl) =
    (  Beamable tbl )

  dbEntityName f vw = fmap (\t' -> vw { dbViewCurrentName = t' }) (f (dbViewCurrentName vw))
  dbEntitySchema f vw = fmap (\s' -> vw { dbViewSchema = s' }) (f (dbViewSchema vw))
  dbEntityAuto nm =
    DatabaseView Nothing nm (unCamelCaseSel nm) defTblFieldSettings

instance RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor be (DomainTypeEntity ty))) where
  renamingFields _ = FieldRenamer id

instance IsDatabaseEntity be (DomainTypeEntity ty) where
  data DatabaseEntityDescriptor be (DomainTypeEntity ty)
    = DatabaseDomainType !(Maybe Text) !Text
  type DatabaseEntityDefaultRequirements be (DomainTypeEntity ty) = ()
  type DatabaseEntityRegularRequirements be (DomainTypeEntity ty) = ()

  dbEntityName f (DatabaseDomainType s t) = DatabaseDomainType s <$> f t
  dbEntitySchema f (DatabaseDomainType s t) = DatabaseDomainType <$> f s <*> pure t
  dbEntityAuto = DatabaseDomainType Nothing

-- | Represents a meta-description of a particular entityType. Mostly, a wrapper
--   around 'DatabaseEntityDescriptor be entityType', but carries around the
--   'IsDatabaseEntity' dictionary.
data DatabaseEntity be (db :: (* -> *) -> *) entityType  where
    DatabaseEntity ::
      IsDatabaseEntity be entityType =>
      DatabaseEntityDescriptor be entityType ->  DatabaseEntity be db entityType

dbEntityDescriptor :: SimpleGetter (DatabaseEntity be db entityType) (DatabaseEntityDescriptor be entityType)
dbEntityDescriptor = Lens.to (\(DatabaseEntity e) -> e)

-- | When parameterized by this entity tag, a database type will hold
--   meta-information on the Haskell mappings of database entities. Under the
--   hood, each entity type is transformed into its 'DatabaseEntityDescriptor'
--   type. For tables this includes the table name as well as the corresponding
--   'TableSettings', which provides names for each column.
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
  gZipDatabase :: Applicative m =>
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
    liftA2 (:*:) (gZipDatabase p combine ax ay) (gZipDatabase p combine bx by)
instance (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl) =>
  GZipDatabase be f g h (K1 Generic.R (f tbl)) (K1 Generic.R (g tbl)) (K1 Generic.R (h tbl)) where

  gZipDatabase _ combine ~(K1 x) ~(K1 y) =
    K1 <$> combine x y

instance Database be db =>
  GZipDatabase be f g h (K1 Generic.R (db f)) (K1 Generic.R (db g)) (K1 Generic.R (db h)) where

  gZipDatabase _ combine ~(K1 x) ~(K1 y) =
    K1 <$> zipTables (Proxy :: Proxy be) combine x y

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

    Columnar (Lenses t f) x = LensFor (t f) (Columnar f x)
--    Columnar (Lenses t f) x = LensFor (t f) (f x)

    Columnar (Nullable c) x = Columnar c (Maybe x)

    Columnar f x = f x

-- | A short type-alias for 'Columnar'. May shorten your schema definitions
type C f a = Columnar f a

-- | If you declare a function 'Columnar f a -> b' and try to constrain your
--   function by a type class for 'f', GHC will complain, because 'f' is
--   ambiguous in 'Columnar f a'. For example, 'Columnar Identity (Maybe a) ~
--   Maybe a' and 'Columnar (Nullable Identity) a ~ Maybe a', so given a type
--   'Columnar f a', we cannot know the type of 'f'.
--
--   Thus, if you need to know 'f', you can instead use 'Columnar''. Since its a
--   newtype, it carries around the 'f' paramater unambiguously. Internally, it
--   simply wraps 'Columnar f a'
newtype Columnar' f a = Columnar' (Columnar f a)

-- | Like 'Data.Functor.Compose', but with an intermediate 'Columnar'
newtype ComposeColumnar f g a = ComposeColumnar (f (Columnar g a))

-- | Metadata for a field of type 'ty' in 'table'.
--
--   Essentially a wrapper over the field name, but with a phantom type
--   parameter, so that it forms an appropriate column tag.
--
--   Usually you use the 'defaultDbSettings' function to generate an appropriate
--   naming convention for you, and then modify it with 'withDbModification' if
--   necessary. Under this scheme, the field n be renamed using the 'IsString'
--   instance for 'TableField', or the 'fieldNamed' function.
data TableField (table :: (* -> *) -> *) ty
  = TableField
  { _fieldPath :: NE.NonEmpty T.Text
    -- ^ The path that led to this field. Each element is the haskell
    -- name of the record field in which this table is stored.
  , _fieldName :: Text  -- ^ The field name
  } deriving (Show, Eq)

-- | Van Laarhoven lens to retrieve or set the field name from a 'TableField'.
fieldName :: Lens' (TableField table ty) Text
fieldName f (TableField path name) = TableField path <$> f name

fieldPath :: Traversal' (TableField table ty) Text
fieldPath f (TableField orig name) = TableField <$> traverse f orig <*> pure name

-- | Represents a table that contains metadata on its fields. In particular,
--   each field of type 'Columnar f a' is transformed into 'TableField table a'.
--   You can get or update the name of each field by using the 'fieldName' lens.
type TableSettings table = table (TableField table)

-- | The regular Haskell version of the table. Equivalent to 'tbl Identity'
type HaskellTable table = table Identity

-- | Column tag that ignores the type.
data Ignored x = Ignored
-- | A form of 'table' all fields 'Ignored'. Useful as a parameter to
--   'zipTables' when you only care about one table.
type TableSkeleton table = table Ignored

from' :: Generic x => x -> Rep x ()
from' = from

to' :: Generic x => Rep x () -> x
to' = to

type HasBeamFields table f g h = ( GZipTables f g h (Rep (table Exposed))
                                                    (Rep (table f))
                                                    (Rep (table g))
                                                    (Rep (table h))

                                 , Generic (table f)
                                 , Generic (table g)
                                 , Generic (table h)
                                 )

-- | The big Kahuna! All beam tables implement this class.
--
--   The kind of all table types is '(* -> *) -> *'. This is because all table types are actually /table type constructors/.
--   Every table type takes in another type constructor, called the /column tag/, and uses that constructor to instantiate the column types.
--   See the documentation for 'Columnar'.
--
--   This class is mostly Generic-derivable. You need only specify a type for the table's primary key and a method to extract the primary key
--   given the table.
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
-- > instance Beamable BlogPostT
-- > instance Table BlogPostT where
-- >    data PrimaryKey BlogPostT f = BlogPostId (Columnar f Text) deriving Generic
-- >    primaryKey = BlogPostId . _blogPostSlug
-- > instance Beamable (PrimaryKey BlogPostT)
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

-- | Provides a number of introspection routines for the beam library. Allows us
--   to "zip" tables with different column tags together. Always instantiate an
--   empty 'Beamable' instance for tables, primary keys, and any type that you
--   would like to embed within either. See the
--   [manual](https://tathougies.github.io/beam/user-guide/models) for more
--   information on embedding.
class Beamable table where
    zipBeamFieldsM :: Applicative m =>
                      (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> table f -> table g -> m (table h)

    default zipBeamFieldsM :: ( HasBeamFields table f g h
                              , Applicative m

                              ) => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
                                -> table f
                                -> table g
                                -> m (table h)

    zipBeamFieldsM combine (f :: table f) g =
        to' <$> gZipTables (Proxy :: Proxy (Rep (table Exposed))) combine (from' f) (from' g)

    tblSkeleton :: TableSkeleton table

    default tblSkeleton :: ( Generic (TableSkeleton table)
                           , GTableSkeleton (Rep (TableSkeleton table))
                           ) => TableSkeleton table

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

alongsideTable :: Beamable tbl => tbl f -> tbl g -> tbl (Columnar' f :*: Columnar' g)
alongsideTable a b =
  runIdentity $
  zipBeamFieldsM (\x y -> pure (Columnar' (x :*: y))) a b

class Retaggable f x | x -> f where
  type Retag (tag :: (* -> *) -> * -> *) x :: *

  retag :: (forall a. Columnar' f a -> Columnar' (tag f) a) -> x
        -> Retag tag x

instance Beamable tbl => Retaggable f (tbl (f :: * -> *)) where
  type Retag tag (tbl f) = tbl (tag f)

  retag = changeBeamRep

instance (Retaggable f a, Retaggable f b) => Retaggable f (a, b) where
  type Retag tag (a, b) = (Retag tag a, Retag tag b)

  retag transform (a, b) = (retag transform a, retag transform b)

instance (Retaggable f a, Retaggable f b, Retaggable f c) =>
  Retaggable f (a, b, c) where
  type Retag tag (a, b, c) = (Retag tag a, Retag tag b, Retag tag c)

  retag transform (a, b, c) = (retag transform a, retag transform b, retag transform c)

instance (Retaggable f a, Retaggable f b, Retaggable f c, Retaggable f d) =>
  Retaggable f (a, b, c, d) where
  type Retag tag (a, b, c, d) =
    (Retag tag a, Retag tag b, Retag tag c, Retag tag d)

  retag transform (a, b, c, d) =
    (retag transform a, retag transform b, retag transform c, retag transform d)

instance ( Retaggable f a, Retaggable f b, Retaggable f c, Retaggable f d
         , Retaggable f e ) =>
  Retaggable f (a, b, c, d, e) where
  type Retag tag (a, b, c, d, e) =
    (Retag tag a, Retag tag b, Retag tag c, Retag tag d, Retag tag e)

  retag transform (a, b, c, d, e) =
    ( retag transform a, retag transform b, retag transform c, retag transform d
    , retag transform e)

instance ( Retaggable f' a, Retaggable f' b, Retaggable f' c, Retaggable f' d
         , Retaggable f' e, Retaggable f' f ) =>
  Retaggable f' (a, b, c, d, e, f) where
  type Retag tag (a, b, c, d, e, f) =
    ( Retag tag a, Retag tag b, Retag tag c, Retag tag d
    , Retag tag e, Retag tag f)

  retag transform (a, b, c, d, e, f) =
    ( retag transform a, retag transform b, retag transform c, retag transform d
    , retag transform e, retag transform f )

instance ( Retaggable f' a, Retaggable f' b, Retaggable f' c, Retaggable f' d
         , Retaggable f' e, Retaggable f' f, Retaggable f' g ) =>
  Retaggable f' (a, b, c, d, e, f, g) where
  type Retag tag (a, b, c, d, e, f, g) =
    ( Retag tag a, Retag tag b, Retag tag c, Retag tag d
    , Retag tag e, Retag tag f, Retag tag g )

  retag transform (a, b, c, d, e, f, g) =
    ( retag transform a, retag transform b, retag transform c, retag transform d
    , retag transform e, retag transform f, retag transform g )

instance ( Retaggable f' a, Retaggable f' b, Retaggable f' c, Retaggable f' d
         , Retaggable f' e, Retaggable f' f, Retaggable f' g, Retaggable f' h ) =>
  Retaggable f' (a, b, c, d, e, f, g, h) where
  type Retag tag (a, b, c, d, e, f, g, h) =
    ( Retag tag a, Retag tag b, Retag tag c, Retag tag d
    , Retag tag e, Retag tag f, Retag tag g, Retag tag h )

  retag transform (a, b, c, d, e, f, g, h) =
    ( retag transform a, retag transform b, retag transform c, retag transform d
    , retag transform e, retag transform f, retag transform g, retag transform h )

-- Carry a constraint instance
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

withConstrainedFields :: forall c tbl
                       . FieldsFulfillConstraint c tbl => tbl Identity -> tbl (WithConstraint c)
withConstrainedFields =
  to . gWithConstrainedFields (Proxy @c) (Proxy @(Rep (tbl Exposed))) . from

withConstraints :: forall c tbl. (Beamable tbl, FieldsFulfillConstraint c tbl) => tbl (WithConstraint c)
withConstraints =
  withConstrainedFields (changeBeamRep (\_ -> Columnar' undefined) tblSkeleton)

withNullableConstrainedFields :: forall c tbl
                               . FieldsFulfillConstraintNullable c tbl => tbl (Nullable Identity) -> tbl (Nullable (WithConstraint c))
withNullableConstrainedFields =
  to . gWithConstrainedFields (Proxy @c) (Proxy @(Rep (tbl (Nullable Exposed)))) . from

withNullableConstraints ::  forall c tbl. (Beamable tbl, FieldsFulfillConstraintNullable c tbl) => tbl (Nullable (WithConstraint c))
withNullableConstraints =
  withNullableConstrainedFields (changeBeamRep (\_ -> Columnar' undefined) tblSkeleton)

type FieldsFulfillConstraint (c :: * -> Constraint) t =
  ( Generic (t (WithConstraint c)), Generic (t Identity), Generic (t Exposed)
  , GFieldsFulfillConstraint c (Rep (t Exposed)) (Rep (t Identity)) (Rep (t (WithConstraint c))))

type FieldsFulfillConstraintNullable (c :: * -> Constraint) t =
  ( Generic (t (Nullable (WithConstraint c))), Generic (t (Nullable Identity)), Generic (t (Nullable Exposed))
  , GFieldsFulfillConstraint c (Rep (t (Nullable Exposed))) (Rep (t (Nullable Identity))) (Rep (t (Nullable (WithConstraint c)))))

-- | Synonym for 'primaryKey'
pk :: Table t => t f -> PrimaryKey t f
pk = primaryKey

-- | Return a 'TableSettings' for the appropriate 'table' type where each column
--   has been given its default name. See the
--   [manual](https://tathougies.github.io/beam/user-guide/models) for
--   information on the default naming convention.
defTblFieldSettings :: ( Generic (TableSettings table)
                       , GDefaultTableFieldSettings (Rep (TableSettings table) ())) =>
                       TableSettings table
defTblFieldSettings = withProxy $ \proxy -> to' (gDefTblFieldSettings proxy)
    where withProxy :: (Proxy (Rep (TableSettings table) ()) -> TableSettings table) -> TableSettings table
          withProxy f = f Proxy

class GZipTables f g h (exposedRep :: * -> *) fRep gRep hRep where
    gZipTables :: Applicative m => Proxy exposedRep
                                -> (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
                                -> fRep ()
                                -> gRep ()
                                -> m (hRep ())

instance ( GZipTables f g h exp1 f1 g1 h1
         , GZipTables f g h exp2 f2 g2 h2
         ) => GZipTables f g h (exp1 :*: exp2) (f1 :*: f2) (g1 :*: g2) (h1 :*: h2)
   where

        gZipTables _ combine ~(f1 :*: f2) ~(g1 :*: g2) =
            (:*:) <$> gZipTables (Proxy :: Proxy exp1) combine f1 g1
                  <*> gZipTables (Proxy :: Proxy exp2) combine f2 g2

instance GZipTables f g h exp fRep gRep hRep =>
    GZipTables f g h (M1 x y exp) (M1 x y fRep) (M1 x y gRep) (M1 x y hRep) where
        gZipTables _ combine ~(M1 f) ~(M1 g) = M1 <$> gZipTables (Proxy :: Proxy exp) combine f g

instance ( fa ~ Columnar f a
         , ga ~ Columnar g a
         , ha ~ Columnar h a
         , ha ~ Columnar h a) =>
    GZipTables f g h (K1 Generic.R (Exposed a)) (K1 Generic.R fa) (K1 Generic.R ga) (K1 Generic.R ha) where
        gZipTables _ combine ~(K1 f) ~(K1 g) = (\(Columnar' h) -> K1 h) <$> combine (Columnar' f :: Columnar' f a) (Columnar' g :: Columnar' g a)

instance ( Beamable tbl
         ) => GZipTables f g h (K1 Generic.R (tbl Exposed)) (K1 Generic.R (tbl f))
                                                            (K1 Generic.R (tbl g))
                                                            (K1 Generic.R (tbl h))
   where
    gZipTables _ combine ~(K1 f) ~(K1 g) = K1 <$> zipBeamFieldsM combine f g


instance GZipTables f g h U1 U1 U1 U1 where
  gZipTables _ _ _ _ = pure U1

instance  ( Beamable tbl
          ) => GZipTables f g h (K1 Generic.R (tbl (Nullable Exposed)))
                                (K1 Generic.R (tbl (Nullable f)))
                                (K1 Generic.R (tbl (Nullable g)))
                                (K1 Generic.R (tbl (Nullable h)))
   where

    gZipTables _ combine ~(K1 f) ~(K1 g) =  K1 <$> zipBeamFieldsM (adapt combine) f g
      where
        adapt :: Applicative m => (forall a . Columnar' f a            -> Columnar' g a            -> m (Columnar' h a)           )
                               -> (forall a . Columnar' (Nullable f) a -> Columnar' (Nullable g) a -> m (Columnar' (Nullable h) a))
        adapt func x y = toNullable <$> func ( fromNullable x ) ( fromNullable y )

        fromNullable :: Columnar' (Nullable w) a -> Columnar' w (Maybe a)
        fromNullable ~(Columnar' x) = Columnar' x

        toNullable   :: Columnar' w (Maybe a) -> Columnar' (Nullable w) a
        toNullable ~(Columnar' x) = Columnar' x

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
        where s = TableField (pure rawSelName) name
              name = unCamelCaseSel rawSelName
              rawSelName = T.pack (selName (undefined :: S1 f (K1 Generic.R (TableField table field)) ()))

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

-- The defaulting with @TableField rel@ is necessary to avoid infinite loops
instance ( Table rel, Generic (rel (TableField rel))
         , TagReducesTo f (TableField tbl)
         , GDefaultTableFieldSettings (Rep (rel (TableField rel)) ()) ) =>
  SubTableStrategyImpl 'PrimaryKeyStrategy f (PrimaryKey rel) where
  namedSubTable _ = primaryKey tbl
    where tbl = changeBeamRep (\(Columnar' (TableField path nm) :: Columnar' (TableField rel) a) ->
                                  let c = Columnar' (TableField path nm) :: Columnar' (TableField tbl) a
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

          origSelName = T.pack (selName (undefined :: S1 f' (K1 Generic.R (sub f)) p))
          relName = unCamelCaseSel origSelName

          settings' :: sub f
          settings' = changeBeamRep (reduceTag %~ \(Columnar' (TableField path nm)) -> Columnar' (TableField (pure origSelName <> path) (relName <> "__" <> nm))) tbl

type family ReplaceBaseTag tag f where
  ReplaceBaseTag tag (Nullable f) = Nullable (ReplaceBaseTag tag f)
  ReplaceBaseTag tag x = tag

-- | Class to automatically unwrap nested Nullables
class TagReducesTo f f' | f -> f' where
  reduceTag :: Functor m =>
               (Columnar' f' a' -> m (Columnar' f' a'))
            -> Columnar' f a -> m (Columnar' f a)
instance TagReducesTo (TableField tbl) (TableField tbl) where
  reduceTag f ~(Columnar' (TableField path nm)) =
    (\(Columnar' (TableField path' nm')) -> Columnar' (TableField path' nm')) <$>
    f (Columnar' (TableField path nm))
instance TagReducesTo f f' => TagReducesTo (Nullable f) f' where
  reduceTag fn ~(Columnar' x :: Columnar' (Nullable f) a) =
    (\(Columnar' x' :: Columnar' f (Maybe a')) -> Columnar' x') <$>
          reduceTag fn (Columnar' x :: Columnar' f (Maybe a))

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

instance ( Beamable tbl
         ) => GTableSkeleton (K1 Generic.R (tbl Ignored))
   where
    gTblSkeleton _ = K1 (tblSkeleton :: TableSkeleton tbl)

instance ( Beamable tbl
         ) => GTableSkeleton (K1 Generic.R (tbl (Nullable Ignored)))
   where
    gTblSkeleton _ = K1 . runIdentity
                   $ zipBeamFieldsM transform
                                    (tblSkeleton :: TableSkeleton tbl)
                                    (tblSkeleton :: TableSkeleton tbl)
        where
          transform :: Columnar' Ignored a
                    -> Columnar' Ignored a
                    -> Identity (Columnar' (Nullable Ignored) a)
          transform _ _ = Identity (Columnar' Ignored)


-- * Internal functions

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

-- | Produce the beam default field name for the given path
defaultFieldName :: NE.NonEmpty Text -> Text
defaultFieldName comps = fold (NE.intersperse (T.pack "__") (unCamelCaseSel <$> comps))
