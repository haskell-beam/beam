{-# LANGUAGE UndecidableInstances #-}

-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Tables
    (
    -- * Database Types
      Database(..), GenDatabaseTable(..), DatabaseTable(..), DatabaseSettings(..)
    , ReifiedDatabaseSchema(..), ReifiedTableSchema(..)
    , autoDbSettings
    , allTableSettings

    , PK(..), BeamEnum(..)

    , Lenses, LensFor(..)

    -- * Columnar and Column Tags
    , Columnar(..), Columnar'(..)
    , Nullable(..), TableField(..)
    , fieldName, fieldConstraints, fieldSettings

    , TableSettings(..)

    -- * Tables
    , Table(..), defTblFieldSettings, defFieldSettings
    , reifyTableSchema, tableValuesNeeded

    -- * Fields
    , FieldSchema(..), FromSqlValuesM(..), FromSqlValues(..)
    , popSqlValue, peekSqlValue

    -- * Foreign keys
    , ForeignKey(..), reference )
    where

import Database.Beam.SQL.Types

import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Data.Proxy
import Data.Coerce
import Data.Typeable
import Data.Text (Text)
import Data.List
import Data.Char
import Data.String
import Data.Void
import Data.Monoid
import qualified Data.Text as T

import Database.HDBC ( SqlColDesc(..), SqlValue(..), SqlTypeId(..)
                     , fromSql)

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

import Lens.Micro hiding (to)

type ReifiedDatabaseSchema = [(Text, ReifiedTableSchema)]
type ReifiedTableSchema = [(Text, SQLColumnSchema)]

class Database db where
    allTables :: (forall tbl. Table tbl => f tbl -> b) -> db f -> [b]
    default allTables :: ( Generic (db f)
                         , GAllTables f (Rep (db f) ()) ) =>
                        (forall tbl. Table tbl => f tbl -> b) -> db f -> [b]
    allTables f db = allTables' f (from' db)

allTableSettings :: Database db => DatabaseSettings db -> [GenDatabaseTable db]
allTableSettings = allTables GenDatabaseTable

autoDbSettings :: ( Generic (DatabaseSettings db)
                  , GAutoDbSettings (Rep (DatabaseSettings db) ()) ) =>
                   DatabaseSettings db
autoDbSettings = to' autoDbSettings'

data GenDatabaseTable db where
    GenDatabaseTable :: DatabaseTable db table -> GenDatabaseTable db
data DatabaseTable (db :: ((((* -> *) -> *) -> *) -> *)) table where
    DatabaseTable :: Table table => Proxy table -> Text -> DatabaseTable db table

type DatabaseSettings db = db (DatabaseTable db)

class GAutoDbSettings x where
    autoDbSettings' :: x
instance GAutoDbSettings (x p) => GAutoDbSettings (D1 f x p) where
    autoDbSettings' = M1 autoDbSettings'
instance GAutoDbSettings (x p) => GAutoDbSettings (C1 f x p) where
    autoDbSettings' = M1 autoDbSettings'
instance (GAutoDbSettings (x p), GAutoDbSettings (y p)) => GAutoDbSettings ((x :*: y) p) where
    autoDbSettings' = autoDbSettings' :*: autoDbSettings'
instance (Table tbl, Selector f) => GAutoDbSettings (S1 f (K1 Generic.R (DatabaseTable db tbl)) p) where
    autoDbSettings' = M1 (K1 (DatabaseTable (Proxy :: Proxy tbl) (fromString name)))
        where name  = unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (DatabaseTable db tbl)) p))

class GAllTables f x where
    allTables' :: (forall tbl. Table tbl => f tbl -> b) -> x -> [b]
instance GAllTables f (x p) => GAllTables f (M1 s m x p) where
    allTables' f (M1 x) = allTables' f x
instance (GAllTables f (x p), GAllTables f (y p)) => GAllTables f ((x :*: y) p) where
    allTables' f (x :*: y) = allTables' f x ++ allTables' f y
instance Table tbl => GAllTables f (K1 Generic.R (f tbl) p) where
    allTables' f (K1 x) = [f x]

data Lenses (t :: (* -> *) -> *) (f :: * -> *) x
data LensFor t x where
    LensFor :: Generic t => Lens' t x -> LensFor t x
newtype Exposed x = Exposed x

newtype BeamEnum a = BeamEnum { unBeamEnum :: a }
    deriving (Show, Typeable)

-- | A type family that we use to "tag" columns in our table datatypes.
--
--   This is what allows us to use the same table type to hold table data, describe table settings,
--   derive lenses, and provide expressions.
--
--   The basic rules are
--
-- > Columnar Identity x = x
-- > Columnar Identity (BeamEnum x) = x
--
--   Thus, any Beam table applied to 'Identity' will yield a simplified version of the data type, that contains
--   just what you'd expect. Enum types tagged with 'BeamEnum', are automatically unwrapped in the simplified data
--   structure.
--
-- > Columnar (Nullable c) x = Columnar c (Maybe x)
--
--   The 'Nullable' type is used when referencing 'ForeignKey's that we want to include optionally.
--   For example, if we have a table with a 'ForeignKey', like the following
--
-- > data BeamTableT f = BeamTableT
-- >                   { _refToAnotherTable :: ForeignKey AnotherTableT f
-- >                   , ... }
--
--   we would typically be required to provide values for the 'PrimaryKey' embedded into 'BeamTableT'. We can use
--   'Nullable' to lift this constraint.
--
-- > data BeamTableT f = BeamTableT
-- >                   { _refToAnotherTable :: ForeignKey AnotherTableT (Nullable f)
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

    Columnar Identity (BeamEnum x) = x
    Columnar Identity x = x

    Columnar (Lenses t Identity) x = LensFor (t Identity) (Columnar Identity x)
    Columnar (Lenses t f) x = LensFor (t f) (f x)

    Columnar (Nullable c) x = Columnar c (Maybe x)

    Columnar f x = f x

newtype Columnar' f a = Columnar' (Columnar f a)

-- | Support for NULLable Foreign Key references.
--
-- > data MyTable f = MyTable
-- >                { nullableRef :: ForeignKey AnotherTable (Nullable f)
-- >                , ... }
-- >                 deriving (Generic, Typeable)
--
-- See 'Columnar' for more information.
data Nullable (c :: * -> *) x

-- | Simple newtype that allows us to declare single-fielded primary keys.
--
--   For example, consider the following table with an 'AutoId' field that we would like to use as the primary key
--
-- > data BeamTableT f = BeamTable
-- >                   { _beamTableId :: Columnar f AutoId
-- >                   , ... }
-- >                    deriving Generic
--
--   When instantiating 'Table' for 'BeamTableT', we would like to do the following
--
-- > instance Table BeamTableT where
-- >     type PrimaryKey BeamTableT f = Columnar f AutoId
-- >     primaryKey = _beamTableId
--
--   Unfortunately, this does not work, due to limitations in the default deriving mechanism. Instead, we need to wrap this in 'PK'.
--
-- > instance Table BeamTableT where
-- >     type PrimaryKey BeamTableT f = PK f AutoId
-- >     primaryKey = PK . _beamTableId
--
--   Note that types with multiple primary keys are not affected by this problem. It is perfectly acceptable to do
--
-- > data BeamTableT = BeamTable'
-- >                 { _beamTableId1 :: Columnar f Text
-- >                 , _beamTableId2 :: Columnar f Int
-- >                 , ... }
-- >                  deriving Generic
-- >
-- > instance Table BeamTableT where
-- >    type PrimaryKey BeamTableT f = (Columnar f Text, Columnar f Int)
-- >    primaryKey t = (_beamTableId1 t, _beamTableId2 t)
newtype PK f t = PK { tableId :: Columnar f t }
                      deriving Generic
instance Show (Columnar f t) => Show (PK f t) where
    showsPrec d (PK x) = showParen (d > 10) $ showString "PK ". showsPrec 11 x

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
-- >    type PrimaryKey EmployeeT f = PK f AutoId
-- >    primaryKey = PK . _beamEmployeeId
-- >
-- >    tblFieldSettings = defTblFieldSettings
-- >                     & employeeFirstNameC . fieldName .~ "fname"
-- >                     & employeeLastNameC  . fieldName .~ "lname"
-- >                     & employeeLastNameC  . fieldSettings .~ Varchar (Just 128) -- Give it a 128 character limit
data TableField (table :: (* -> *) -> *) ty = TableField
                                            { _fieldName        :: Text             -- ^ The field name
                                            , _fieldConstraints :: [SQLConstraint]  -- ^ Constraints for the field (such as AutoIncrement, PrimaryKey, etc)
                                            , _fieldSettings    :: FieldSettings ty -- ^ Settings for the field
                                            }
deriving instance Show (FieldSettings ty) => Show (TableField t ty)

fieldName :: Lens' (TableField table ty) Text
fieldName f (TableField name cs s) = (\name' -> TableField name' cs s) <$> f name
fieldConstraints :: Lens' (TableField table ty) [SQLConstraint]
fieldConstraints f (TableField name cs s) = (\cs' -> TableField name cs' s) <$> f cs
fieldSettings :: Lens (TableField table a) (TableField table b) (FieldSettings a) (FieldSettings b)
fieldSettings f (TableField name cs s) = (\s' -> TableField name cs s') <$> f s

type TableSettings table = table (TableField table)

from' :: Generic x => x -> Rep x ()
from' = from

to' :: Generic x => Rep x () -> x
to' = to

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
-- >                  , _blogPostAuthor  :: ForeignKey AuthorT f
-- >                  , _blogPostTagline :: Columnar f (Maybe Text)
-- >                  , _blogPostImageGallery :: ForeignKey ImageGalleryT (Nullable f) }
-- >                    deriving Generic
-- > instance Table BlogPostT where
-- >    type PrimaryKey BlogPostT f = PK f Text
-- >    primaryKey = PK . _blogPostSlug
--
--   We can interpret this as follows:
--
--     * The `_blogPostSlug`, `_blogPostBody`, `_blogPostDate`, and `_blogPostTagline` fields are of types 'Text', 'Text', 'UTCTime', and 'Maybe Text' respectfully.
--     * Since `_blogPostSlug`, `_blogPostBody`, `_blogPostDate`, `_blogPostAuthor` must be provided (i.e, they cannot contain 'Nothing'), they will be given SQL NOT NULL constraints.
--       `_blogPostTagline` is declared 'Maybe' so 'Nothing' will be stored as NULL in the database. `_blogPostImageGallery` will be allowed to be empty because it uses the 'Nullable' tag modifier.
--     * `blogPostAuthor` references the `AuthorT` table (not given here) and is required.
--     * `blogPostImageGallery` references the `ImageGalleryT` table (not given here), but this relation is not required (i.e., it may be 'Nothing'. See 'Nullable').
class Typeable table  => Table (table :: (* -> *) -> *) where

    -- | A data type representing the types of primary keys for this table.
    --   In order to play nicely with the default deriving mechanism, this type must be an instance of 'Generic'.
    type PrimaryKey table (column :: * -> *) :: *

    -- | Given a table, this should return the PrimaryKey from the table. By keeping this polymorphic over column,
    --   we ensure that the primary key values come directly from the table (i.e., they can't be arbitrary constants)
    primaryKey :: table column -> PrimaryKey table column

    pkChangeRep :: Proxy table -> (forall a. Columnar' f a -> Columnar' g a) -> PrimaryKey table f -> PrimaryKey table g
    default pkChangeRep :: ( Generic (PrimaryKey table f)
                           , Generic (PrimaryKey table g)
                           , Generic (PrimaryKey table Exposed)
                           , GChangeRep (Rep (PrimaryKey table Exposed) ())
                                        (Rep (PrimaryKey table f) ()) (Rep (PrimaryKey table g) ())
                                        f g ) =>
                           Proxy table -> (forall a. Columnar' f a -> Columnar' g a) -> PrimaryKey table f -> PrimaryKey table g
    pkChangeRep (tbl :: Proxy table) f x = to' (gChangeRep (Proxy :: Proxy (Rep (PrimaryKey table Exposed) ()))
                                                           f (from' x))

    changeRep :: (forall a. Columnar' f a -> Columnar' g a) -> table f -> table g
    default changeRep :: ( ChangeRep table f g ) =>
                         (forall a. Columnar' f a -> Columnar' g a) -> table f -> table g
    changeRep (f :: forall a. Columnar' f a -> Columnar' g a) =
        changeRep' (Proxy :: Proxy f) (Proxy :: Proxy g) (Proxy :: Proxy table) f

    pkAllValues :: Proxy table -> (forall a. FieldSchema a => Columnar' f a -> b) -> PrimaryKey table f -> [b]
    default pkAllValues :: AllValues f (PrimaryKey table f) (PrimaryKey table Exposed) =>
                           Proxy table -> (forall a. FieldSchema a => Columnar' f a -> b) -> PrimaryKey table f -> [b]
    pkAllValues _ = allValues' (Proxy :: Proxy (PrimaryKey table Exposed))

    fieldAllValues :: (forall a. FieldSchema a => Columnar' f a -> b) -> table f -> [b]
    default fieldAllValues :: AllValues f (table f) (table Exposed)=>
                              (forall a. FieldSchema a => Columnar' f a -> b) -> table f -> [b]
    fieldAllValues = allValues' (Proxy :: Proxy (table Exposed))

    tblFieldSettings :: TableSettings table
    default tblFieldSettings :: ( Generic (TableSettings table)
                                , GDefaultTableFieldSettings (Rep (TableSettings table) ())) => TableSettings table
    tblFieldSettings = defTblFieldSettings

    makeSqlValues :: table Identity -> [SqlValue]
    default makeSqlValues :: (Generic (table Identity), GMakeSqlValues (Rep (table Exposed) ()) (Rep (table Identity) ())) => table Identity -> [SqlValue]
    makeSqlValues table = gMakeSqlValues (Proxy :: Proxy (Rep (table Exposed) ())) (from' table)

    tableFromSqlValues :: FromSqlValuesM (table Identity)
    default tableFromSqlValues :: ( Generic (table Identity)
                                  , GFromSqlValues (Rep (table Exposed)) (Rep (table Identity)) ) =>
                                  FromSqlValuesM (table Identity)
    tableFromSqlValues = to <$> gFromSqlValues (Proxy :: Proxy (Rep (table Exposed)))

reifyTableSchema :: Table table => Proxy table -> ReifiedTableSchema
reifyTableSchema (Proxy :: Proxy table) = fieldAllValues (\(Columnar' (TableField name constraints settings)) ->
                                                                  (name, fieldColDesc settings constraints)) (tblFieldSettings :: TableSettings table)

tableValuesNeeded :: Table table => Proxy table -> Int
tableValuesNeeded (Proxy :: Proxy table) = length (fieldAllValues (\_ -> ()) (tblFieldSettings :: TableSettings table))

instance FromSqlValues t => FromSqlValues (Maybe t) where
    valuesNeeded (_ :: Proxy (Maybe t)) = valuesNeeded (Proxy :: Proxy t)
    fromSqlValues' = mfix $ \(_ :: Maybe t) ->
                     do values <- get
                        let colCount = valuesNeeded (Proxy :: Proxy t)
                            colValues = take colCount values
                        if all (==SqlNull) colValues
                        then put (drop colCount values) >> return Nothing
                        else Just <$> fromSqlValues'

defTblFieldSettings :: ( Generic (TableSettings table)
                       ,  GDefaultTableFieldSettings (Rep (TableSettings table) ())) =>
                       TableSettings table
defTblFieldSettings = withProxy $ \proxy -> to' (gDefTblFieldSettings proxy)
    where withProxy :: (Proxy (Rep (TableSettings table) ()) -> TableSettings table) -> TableSettings table
          withProxy f = f Proxy

defFieldSettings :: FieldSchema fs => Text -> TableField table fs
defFieldSettings name = TableField
                      { _fieldName = name
                      , _fieldConstraints = []
                      , _fieldSettings = settings}
    where settings = defSettings

fieldColDesc :: FieldSchema fs => FieldSettings fs -> [SQLConstraint] -> SQLColumnSchema
fieldColDesc settings cs =let base = colDescFromSettings settings
                          in base { csConstraints = csConstraints base ++ cs }

class GChangeRep (ty :: *) x y f g where
    gChangeRep :: Proxy ty -> (forall a. Columnar' f a -> Columnar' g a) -> x -> y
instance GChangeRep (ty p) (a p) (b p) x y => GChangeRep (M1 s h ty p) (M1 s f a p) (M1 s g b p) x y where
    gChangeRep _ f (M1 x) = M1 (gChangeRep (Proxy :: Proxy (ty p)) f x)
instance ( GChangeRep (t1 p) (a1 p) (a2 p) x y, GChangeRep (t2 p) (b1 p) (b2 p) x y) => GChangeRep ((t1 :*: t2) p) ((a1 :*: b1) p) ((a2 :*: b2) p) x y where
    gChangeRep _ f (a :*: b) =
        gChangeRep (Proxy :: Proxy (t1 p)) f a :*: gChangeRep (Proxy :: Proxy (t2 p)) f b
instance ( Generic (PrimaryKey rel x)
         , Generic (PrimaryKey rel y)
         , GChangeRep (Rep (PrimaryKey rel Exposed) ())
                      (Rep (PrimaryKey rel x) ())
                      (Rep (PrimaryKey rel y) ())
                      x y ) =>
    GChangeRep (K1 Generic.R (ForeignKey rel Exposed) p) (K1 Generic.R (ForeignKey rel x) p) (K1 Generic.R (ForeignKey rel y) p) x y where
    gChangeRep _ (f :: forall b. Columnar' f b -> Columnar' g b) (K1 (ForeignKey x)) =
        K1 (ForeignKey (to' (gChangeRep (Proxy :: Proxy (Rep (PrimaryKey rel Exposed) ())) f (from' x))))
instance (xa ~ Columnar x a, ya ~ Columnar y a) => GChangeRep (K1 Generic.R (Exposed a) p) (K1 Generic.R xa p) (K1 Generic.R ya p) x y where
    gChangeRep (_ :: Proxy (K1 Generic.R (Exposed a) p)) (f :: forall b. Columnar' f b -> Columnar' g b) (K1 x) =
        let x' = Columnar' x :: Columnar' f a
            Columnar' y' = f x' :: Columnar' g a
        in K1 y'

-- instance GChangeRep (K1 Generic.R (Nullable x a) p) (K1 Generic.R (Nullable y a) p) x y where
--     gChangeRep f (K1 (Nullable x)) = K1 (Nullable (f x))
-- instance ( Generic (PrimaryKey table x)
--          , Generic (PrimaryKey table y)
--          , GChangeRep (Rep (PrimaryKey table x) ()) (Rep (PrimaryKey table y) ()) x y ) =>
--     GChangeRep (K1 Generic.R (ForeignKey table x) p) (K1 Generic.R (ForeignKey table y) p) x y where
--     gChangeRep f (K1 (ForeignKey x)) = K1 (ForeignKey (to' (gChangeRep f (from' x))))
-- instance ( Generic (PrimaryKey table (Nullable x))
--          , Generic (PrimaryKey table (Nullable y))
--          , GChangeRep (Rep (PrimaryKey table (Nullable x)) ()) (Rep (PrimaryKey table (Nullable y)) ()) x y ) =>
--     GChangeRep (K1 Generic.R (ForeignKey table (Nullable x)) p) (K1 Generic.R (ForeignKey table (Nullable y)) p) x y where
--     gChangeRep f (K1 (ForeignKey x)) = K1 (ForeignKey (to' (gChangeRep f (from' x))))
-- instance GChangeRep (Nullable f a) (Nullable g a) f g where
--     gChangeRep f (Nullable x) = Nullable (f x)

class ChangeRep x f g where
    changeRep' :: Proxy f -> Proxy g -> Proxy x -> (forall a. Columnar' f a -> Columnar' g a) -> x f -> x g
instance ( Generic (x f)
         , Generic (x g)
         , Generic (x Exposed)
         , GChangeRep (Rep (x Exposed) ()) (Rep (x f) ()) (Rep (x g) ()) f g) =>
    ChangeRep x f g where
    changeRep' _ _ (Proxy :: Proxy x) f x = to' (gChangeRep (Proxy :: Proxy (Rep (x Exposed) ())) f (from' x))

class GAllValues (f :: * -> *) (ty :: *) x where
    gAllValues :: Proxy ty  -> (forall a. FieldSchema a => Columnar' f a -> b) -> x -> [b]
instance (GAllValues f (t1 x) (a x), GAllValues f (t2 x) (b x)) => GAllValues f ((t1 :*: t2) x) ((a :*: b) x) where
    gAllValues Proxy f (a :*: b) = gAllValues (Proxy :: Proxy (t1 x)) f a ++ gAllValues (Proxy :: Proxy (t2 x)) f b
instance (GAllValues f (ty x) (p x)) => GAllValues f (M1 s h ty x) (M1 s g p x) where
    gAllValues Proxy f (M1 a) = gAllValues (Proxy :: Proxy (ty x)) f a
instance ( Generic (PrimaryKey rel f)
         , GAllValues f (Rep (PrimaryKey rel Exposed) ()) (Rep (PrimaryKey rel f) ()) ) =>
    GAllValues f (K1 Generic.R (ForeignKey rel Exposed) a) (K1 Generic.R (ForeignKey rel f) a) where
    gAllValues Proxy f (K1 (ForeignKey x)) =
        gAllValues (Proxy :: Proxy (Rep (PrimaryKey rel Exposed) ())) f (from' x)
instance (FieldSchema x, fx ~ Columnar f x) => GAllValues f (K1 Generic.R (Exposed x) a) (K1 Generic.R fx a) where
    gAllValues Proxy f (K1 a) = [f (Columnar' a :: Columnar' f x)]

-- instance FieldSchema x => GAllValues f (K1 Generic.R (Nullable f x) a) where
--     gAllValues f (K1 (Nullable a)) = [f a]
-- instance ( Generic (PrimaryKey related g)
--          , GAllValues f (Rep (PrimaryKey related g) ()) ) =>
--     GAllValues f (K1 Generic.R (ForeignKey related g) a) where
--     gAllValues f (K1 (ForeignKey x)) = gAllValues f (from' x)
-- instance FieldSchema a => GAllValues f (f a) where
--     gAllValues f x = [f x]

type AllValues f xf xExposed = ( Generic xf
                               , Generic xExposed
                               , GAllValues f (Rep xExposed ()) (Rep xf ()))

allValues' :: AllValues f xf xExposed =>
              Proxy xExposed -> (forall a. FieldSchema a => Columnar' f a -> b) -> xf -> [b]
allValues' (Proxy :: Proxy xExposed) f x =
    gAllValues (Proxy :: Proxy (Rep xExposed ())) f (from' x)

class GDefaultTableFieldSettings x where
    gDefTblFieldSettings :: Proxy x -> x
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (D1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (D1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (C1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (C1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance (GDefaultTableFieldSettings (a p), GDefaultTableFieldSettings (b p)) => GDefaultTableFieldSettings ((a :*: b) p) where
    gDefTblFieldSettings (_ :: Proxy ((a :*: b) p)) = gDefTblFieldSettings (Proxy :: Proxy (a p)) :*: gDefTblFieldSettings (Proxy :: Proxy (b p))

instance (Table table, FieldSchema field, Selector f ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (TableField table field)) p) where
    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (TableField table field)) p)) = M1 (K1 s)
        where s = defFieldSettings (T.pack name)
              name = unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (TableField table field)) ()))

instance ( Table table, Table related
         , Selector f

         , Generic (PrimaryKey related (TableField related))
         , Generic (PrimaryKey related (TableField table))
         , GChangeRep (Rep (PrimaryKey related Exposed) ())
                      (Rep (PrimaryKey related (TableField related)) ()) (Rep (PrimaryKey related (TableField table)) ())
                      (TableField related) (TableField table) ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (ForeignKey related (TableField table))) p) where

    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (ForeignKey related (TableField table))) p )) = M1 . K1 . ForeignKey $ primaryKeySettings'
        where tableSettings = tblFieldSettings :: TableSettings related
              primaryKeySettings :: PrimaryKey related (TableField related)
              primaryKeySettings = primaryKey tableSettings

              primaryKeySettings' :: PrimaryKey related (TableField table)
              primaryKeySettings' = to' (gChangeRep (Proxy :: Proxy (Rep (PrimaryKey related Exposed) ())) convertToForeignKeyField (from' primaryKeySettings))

              convertToForeignKeyField :: Columnar' (TableField related) c -> Columnar' (TableField table) c
              convertToForeignKeyField (Columnar' tf) =
                  Columnar' $
                  tf { _fieldName = keyName <> "__" <> _fieldName tf
                     , _fieldConstraints = removeConstraints (_fieldConstraints tf) }


              removeConstraints = filter (\x -> x /= SQLPrimaryKey && x /= SQLAutoIncrement)

              keyName = T.pack (unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (ForeignKey related (TableField table))) ())))

instance ( Table table, Table related
         , Selector f

         , Generic (PrimaryKey related (TableField related))
         , Generic (PrimaryKey related (TableField table))
         , Generic (PrimaryKey related (Nullable (TableField table)))
         , GChangeRep (Rep (PrimaryKey related Exposed) ())
                      (Rep (PrimaryKey related (TableField table)) ()) (Rep (PrimaryKey related (Nullable (TableField table))) ())
                      (TableField table) (Nullable (TableField table))
         , GChangeRep (Rep (PrimaryKey related Exposed) ())
                      (Rep (PrimaryKey related (TableField related)) ()) (Rep (PrimaryKey related (TableField table)) ()) (TableField related) (TableField table) ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (ForeignKey related (Nullable (TableField table)))) p) where

    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (ForeignKey related (Nullable (TableField table)))) p )) =
        M1 . K1 . ForeignKey $ settings
        where M1 (K1 (ForeignKey nonNullSettings)) = gDefTblFieldSettings (Proxy :: Proxy (S1 f (K1 Generic.R (ForeignKey related (TableField table))) p))
              nonNullSettingsRep = from' nonNullSettings :: Rep (PrimaryKey related (TableField table)) ()

              settings :: PrimaryKey related (Nullable (TableField table))
              settings = to' (gChangeRep (Proxy :: Proxy (Rep (PrimaryKey related Exposed) ())) removeNotNullConstraints nonNullSettingsRep)

              removeNotNullConstraints :: Columnar' (TableField table) ty -> Columnar' (Nullable (TableField table)) ty
              removeNotNullConstraints (Columnar' tf) =
                  Columnar' $
                            -- Nullable $
                  tf { _fieldSettings = MaybeFieldSettings (_fieldSettings tf) }

class GFromSqlValues (ty :: * -> *) (schema :: * -> *) where
    gFromSqlValues :: Proxy ty -> FromSqlValuesM (schema a)
instance GFromSqlValues ty x => GFromSqlValues (M1 s f ty) (M1 s f x) where
    gFromSqlValues _ = M1 <$> gFromSqlValues (Proxy :: Proxy ty)
instance FieldSchema x => GFromSqlValues (K1 Generic.R (Exposed x)) (K1 Generic.R x) where
    gFromSqlValues _ = K1 <$> fromSqlValue
instance FieldSchema (BeamEnum x) => GFromSqlValues (K1 Generic.R (Exposed (BeamEnum x))) (K1 Generic.R x) where
    gFromSqlValues _ = K1 . unBeamEnum <$> fromSqlValue
instance (GFromSqlValues t1 a, GFromSqlValues t2 b) => GFromSqlValues (t1 :*: t2) (a :*: b) where
    gFromSqlValues _ = (:*:) <$> gFromSqlValues (Proxy :: Proxy t1) <*> gFromSqlValues (Proxy :: Proxy t2)
instance ( Generic (PrimaryKey related f)
         , GFromSqlValues (Rep (PrimaryKey related Exposed)) (Rep (PrimaryKey related f)) ) =>
    GFromSqlValues (K1 Generic.R (ForeignKey related Exposed)) (K1 Generic.R (ForeignKey related f)) where

    gFromSqlValues _ = K1 . ForeignKey . to' <$> gFromSqlValues (Proxy :: Proxy (Rep (PrimaryKey related Exposed)))
-- instance FieldSchema (Maybe x) => GFromSqlValues (K1 Generic.R (Nullable Column x)) where
--     gFromSqlValues = K1 . Nullable . Column <$> fromSqlValue

class GMakeSqlValues ty x where
    gMakeSqlValues :: Proxy ty -> x -> [SqlValue]
instance GMakeSqlValues (ty a) (p a) => GMakeSqlValues (M1 s f ty a) (M1 s f p a) where
    gMakeSqlValues _ (M1 x) = gMakeSqlValues (Proxy :: Proxy (ty a)) x
instance (GMakeSqlValues (t1 a) (f a), GMakeSqlValues (t2 a) (g a)) => GMakeSqlValues ((t1 :*: t2) a) ((f :*: g) a) where
    gMakeSqlValues _ (f :*: g) = gMakeSqlValues (Proxy :: Proxy (t1 a)) f ++ gMakeSqlValues (Proxy :: Proxy (t2 a)) g
instance GMakeSqlValues (U1 a) (U1 a) where
    gMakeSqlValues _ _ = []
instance FieldSchema x => GMakeSqlValues (K1 Generic.R (Exposed x) a) (K1 Generic.R x a) where
    gMakeSqlValues _ (K1 x) = [makeSqlValue x]
instance FieldSchema (BeamEnum x) => GMakeSqlValues (K1 Generic.R (Exposed (BeamEnum x)) a) (K1 Generic.R x a) where
    gMakeSqlValues _ (K1 x) = [makeSqlValue (BeamEnum x)]
-- instance FieldSchema x => GMakeSqlValues (K1 Generic.R (Nullable Column x) a) where
--     gMakeSqlValues (K1 (Nullable x)) = [makeSqlValue (columnValue x)]
instance ( Generic (PrimaryKey related f)
         , GMakeSqlValues (Rep (PrimaryKey related Exposed) ()) (Rep (PrimaryKey related f) ()) ) =>
    GMakeSqlValues (K1 Generic.R (ForeignKey related Exposed) a) (K1 Generic.R (ForeignKey related f) a) where
    gMakeSqlValues _ (K1 (ForeignKey x)) = gMakeSqlValues (Proxy :: Proxy (Rep (PrimaryKey related Exposed) ())) (from' x)

class ( Show (FieldSettings fs), Typeable fs
      , Show fs )  => FieldSchema fs where
    data FieldSettings fs :: *

    defSettings :: FieldSettings fs

    colDescFromSettings :: FieldSettings fs -> SQLColumnSchema

    makeSqlValue :: fs -> SqlValue
    fromSqlValue :: FromSqlValuesM fs

type FromSqlValuesM a = ErrorT String (State [SqlValue]) a
popSqlValue, peekSqlValue :: FromSqlValuesM SqlValue
popSqlValue = do st <- get
                 put (tail st)
                 return (head st)
peekSqlValue = head <$> get
class FromSqlValues a where
    fromSqlValues' :: FromSqlValuesM a
    valuesNeeded :: Proxy a -> Int

    default fromSqlValues' :: FieldSchema a => FromSqlValuesM a
    fromSqlValues' = fromSqlValue
    default valuesNeeded :: FieldSchema a => Proxy a -> Int
    valuesNeeded _ = 1
instance Table tbl => FromSqlValues (tbl Identity) where
    fromSqlValues' = tableFromSqlValues
    valuesNeeded _ = tableValuesNeeded (Proxy :: Proxy tbl)
instance (FromSqlValues a, FromSqlValues b) => FromSqlValues (a, b) where
    fromSqlValues' = (,) <$> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b)
instance (FromSqlValues a, FromSqlValues b, FromSqlValues c) => FromSqlValues (a, b, c) where
    fromSqlValues' = (,,) <$> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b) + valuesNeeded (Proxy :: Proxy c)

-- | Mechanism to embed references to SQL into Haskell data type.
--
--   If 'table' is an instance of 'Table', we can use 'ForeignKey' to embed the 'PrimaryKey' of 'table' into
--   a related table.
--
-- > data AuthorT f = AuthorT
-- >                { _authorEmail     :: Columnar f Text
-- >                , _authorFirstName :: Columnar f Text
-- >                , _authorLastName  :: Columnar f Text }
-- >                  deriving Generic
-- >
-- > data BlogPostT f = BlogPost
-- >                  { _blogPostSlug    :: Columnar f Text
-- >                  , _blogPostBody    :: Columnar f Text
-- >                  , _blogPostDate    :: Columnar f UTCTime
-- >                  , _blogPostAuthor  :: ForeignKey AuthorT f
-- >                  , _blogPostTagline :: Columnar f (Maybe Text) }
-- >                    deriving Generic
-- > instance Table BlogPostT where
-- >    type PrimaryKey BlogPostT f = PK f Text
-- >    primaryKey = PK . _blogPostSlug
-- > instance Table AuthorT where
-- >    type PrimaryKey AuthorT f = PK f Text
-- >    primaryKey = PK . _authorEmail
data ForeignKey table column = ForeignKey (PrimaryKey table column)
deriving instance Show (PrimaryKey table column) => Show (ForeignKey table column)

-- | Given a 'ForeignKey' to `table`, return the 'PrimaryKey' of the related table.
reference :: ForeignKey table f -> PrimaryKey table f
reference (ForeignKey x) = x

instance FieldSchema Int where
    data FieldSettings Int = IntFieldDefault
                             deriving Show
    defSettings = IntFieldDefault
    colDescFromSettings _ = notNull
                            SqlColDesc
                            { colType = SqlNumericT
                            , colSize = Nothing
                            , colOctetLength = Nothing
                            , colDecDigits = Nothing
                            , colNullable = Nothing }
    makeSqlValue i = SqlInteger (fromIntegral i)
    fromSqlValue = fromSql <$> popSqlValue
instance FromSqlValues Int

instance FieldSchema a => FieldSchema (Maybe a) where
    data FieldSettings (Maybe a) = MaybeFieldSettings (FieldSettings a)

    defSettings = MaybeFieldSettings defSettings

    colDescFromSettings (MaybeFieldSettings settings) = let SQLColumnSchema desc constraints = colDescFromSettings settings
                                                        in SQLColumnSchema desc (filter (/=SQLNotNull) constraints)

    makeSqlValue Nothing = SqlNull
    makeSqlValue (Just x) = makeSqlValue x
    fromSqlValue = do val <- peekSqlValue
                      case val of
                        SqlNull -> Nothing <$ popSqlValue
                        val -> Just <$> fromSqlValue
deriving instance Show (FieldSettings a) => Show (FieldSettings (Maybe a))

-- Internal functions

unCamelCase :: String -> [String]
unCamelCase "" = []
unCamelCase s = let (comp, next) = span isLower s
                    next' = case next of
                              [] -> []
                              x:xs -> toLower x:xs
                in map toLower comp:unCamelCase next'

unCamelCaseSel :: String -> String
unCamelCaseSel ('_':xs) = unCamelCaseSel xs
unCamelCaseSel xs = case unCamelCase xs of
                      [xs] -> xs
                      _:xs -> intercalate "_" xs
