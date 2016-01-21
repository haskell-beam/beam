{-# LANGUAGE TypeFamilies, TypeOperators,  MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances, RankNTypes, RoleAnnotations, ConstraintKinds #-}
-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Tables
    (
    -- * Database types
      Database(..), GenDatabaseTable(..), DatabaseTable(..), DatabaseSettings(..)
    , ReifiedDatabaseSchema(..), ReifiedTableSchema(..) --, GenTable(..)
    , allTableSettings

    , (:|:)(..)

    , PK(..), BeamEnum(..)

    , Lenses, LensFor(..), Exposed(..)

    -- * Column constructors
    , Column(..), IsColumn(..), ColumnType(..), Columnar(..), Columnar'(..)
    , Nullable(..), Dummy(..), TableField(..)
    , fieldName, fieldConstraints, fieldSettings

    , TableSettings(..)

    -- * Tables
    , Table(..), allValues, defTblFieldSettings, defFieldSettings
    , ChangeRep(..)

    -- * Fields
    , FieldSchema(..), FromSqlValuesM(..), FromSqlValues(..)
    , popSqlValue, peekSqlValue
    , fieldColDesc

    -- * Foreign keys
    , ForeignKey(..), reference, tentative)
    where

import Database.Beam.SQL.Types
--import Database.Beam.Schema.Locate

import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Data.Proxy
import Data.Coerce
import Data.Typeable
import Data.Text (Text)
import Data.String
import Data.Void
import Data.Monoid
import qualified Data.Text as T

import Database.HDBC ( SqlColDesc(..), SqlValue(..), SqlTypeId(..)
                     , fromSql)

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

import Lens.Micro hiding (to)

-- * Database types

-- data GenTable where
--     GenTable :: Table t => Proxy t -> GenTable

type ReifiedDatabaseSchema = [(Text, ReifiedTableSchema)]
type ReifiedTableSchema = [(Text, SQLColumnSchema)]

-- newtype Database = Database [GenTable]

-- database_ :: [GenTable] -> Database
-- database_ = Database

-- table_ :: Table t => t a -> GenTable
-- table_ t = GenTable (proxyFor t)
--     where proxyFor :: t a -> Proxy t
--           proxyFor _ = Proxy

-- schema_ :: Table t => t a
-- schema_ = undefined

-- reifyDBSchema :: Database -> ReifiedDatabaseSchema
-- reifyDBSchema (Database tables) = map schemaForTable tables
--     where schemaForTable :: GenTable -> (Text, ReifiedTableSchema)
--           schemaForTable (GenTable t) = (dbTableName t, reifyTableSchema t)

-- tables :: Database -> [GenTable]
-- tables (Database x) = x

class Database db where
    allTables :: (forall tbl. Table tbl => f tbl -> b) -> db f -> [b]

allTableSettings :: Database db => DatabaseSettings db -> [GenDatabaseTable db]
allTableSettings db = allTables GenDatabaseTable db

data GenDatabaseTable db where
    GenDatabaseTable :: DatabaseTable db table -> GenDatabaseTable db
data DatabaseTable db table where
    DatabaseTable :: Table table => Proxy table -> Text -> DatabaseTable db table

type DatabaseSettings db = db (DatabaseTable db)

-- | Schema combinator. Takes two fields or schemas and combines them into a new schema
data a :|: b = a :|: b
               deriving Show
infixl 3 :|:

-- * Column constructors

data Lenses t (f :: * -> *) x
data LensFor t x where
    LensFor :: Generic t => Lens' t x -> LensFor t x
newtype Exposed x = Exposed x

data ForcedFunctor (f :: * -> *)

newtype BeamEnum a = BeamEnum { unBeamEnum :: a }
    deriving (Show, Typeable)

type family Columnar (f :: * -> *) x where
    Columnar Exposed x = Exposed x

    Columnar Identity (BeamEnum x) = x
    Columnar Identity x = x

    Columnar (Lenses t Identity) x = LensFor (t Identity) (Columnar Identity x)
    Columnar (Lenses t f) x = LensFor (t f) (f x)

    Columnar (Nullable c) x = Columnar c (Maybe x)

    Columnar f x = f x

newtype Columnar' f a = Columnar' (Columnar f a)

-- | The standard column constructor. Computationally and representationally, there is no cost to using this constructor,
--   since it is defined as a newtype. Use the 'column' and 'columnValue' functions to construct and deconstruct values.
newtype Column t = Column t deriving Typeable
instance Show x => Show (Column x) where
    show (Column x) = show x

class Typeable c => IsColumn c where
    type ColumnType c a :: *
    column :: ColumnType c a -> c a
    -- | The `Typeable` constraint is because it is impossible to use non-Typeable data in a QExpr, so we enforce that throughout beam
    columnValue :: Typeable a => c a -> ColumnType c a

instance IsColumn Column where
    type ColumnType Column x = x
    columnValue (Column x) = x
    column = Column

instance IsColumn c => IsColumn (Nullable c) where
    type ColumnType (Nullable c) x = ColumnType c (Maybe x)
    columnValue (Nullable x) = columnValue x
    column = Nullable . column

-- | Support for NULLable Foreign Key references.
--
-- > data MyTable column = MyTable
-- >                     { nullableRef :: ForeignKey AnotherTable (Nullable column)
-- >                     , ... }
-- >                       deriving (Generic, Typeable)
newtype Nullable c x = Nullable (c (Maybe x)) deriving Typeable
deriving instance Show (c (Maybe x)) => Show (Nullable c x)

-- | A dummy column constructor that can never be constructed.
newtype Dummy a = Dummy Void
    deriving Typeable

-- * Tables

-- | Default primary key type. Just uses one 'Int' column
newtype PK f t = PK
                    { tableId :: Columnar f t }
                      deriving Generic
instance Show (Columnar f t) => Show (PK f t) where
    showsPrec d (PK x) = showParen (d > 10) $ showString "PK ". showsPrec 11 x

-- | Column constructor that lets you specify settings for a field of type `ty` in `table`.
--
--   For example, consider the simple Employee table
--
-- > data Employee column = Employee
-- >                      { employeeName  :: column Text
-- >                      , employeeAge   :: column Int
-- >                      , employeeEmail :: column Text }
-- >                        deriving (Generic, Typeable)
--
--   You can customize the fields of `Employee` by overwriting `tableSettings` in the `Table` class.
--
-- > instance Table Employee where
-- >     tblFieldSettings = tblSettings
-- >                      { employeeName = (employeeName tblSettings)
-- >                                     { fieldName = "firstAndLastName"
-- >                                     , fieldSettings = TextFieldSettings
-- >                                                     { charOrVarChar = Varchar (Just 128) } } }
-- >                      where tblSettings = defTblFieldSettings
data TableField table ty = TableField
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

-- | Type alias for `table` under the `TableField` column constructor, which provides a way of setting options for fields
type TableSettings table = table (TableField table)

from' :: Generic x => x -> Rep x ()
from' = from

to' :: Generic x => Rep x () -> x
to' = to

-- | The big Kahuna! All beam tables implement this class.
--
--   The kind of all table types is `(* -> *) -> *`. This is because all table types are actually /table type constructors/.
--   Every table type takes in another type constructor, called the /column constructor/, and uses that constructor to create
--   columns. Every type passed to the column constructor, must be an instance of 'FieldSchema'.
--
--   This class is 100% Generic-derivable. You never have to specify any of the methods yourself, unless you want to customize
--   beyond Beam's standard behavior (such as renaming fields in the database, customizing field types, etc.)
--
--   An example table:
--
-- > data BlogPost column = BlogPost
-- >                      { blogPostBody    :: column Text
-- >                      , blogPostDate    :: column UTCTime
-- >                      , blogPostAuthor  :: ForeignKey Author column
-- >                      , blogPostTagline :: column (Maybe Text)
-- >                      , blogPostImageGallery :: ForeignKey ImageGallery (Nullable column) }
-- >                        deriving (Generic, Typeable)
-- > instance Table BlogPost
--
--   We can interpret this as follows:
--
--     * The `blogPostBody`, `blogPostDate`, and `blogPostTagline` fields are of types `Text`, `UTCTime`, and `Maybe Text` respectfully
--     * `blogPostBody` and `blogPostDate` are mandatory (and will be given SQL NOT NULL constraints). The `blogPostTagline` field may be 'Nothing'.
--     * `blogPostAuthor` references the `Author` model (not given here), and is required.
--     * `blogPostImageGallery` references the `ImageGallery` model (not given here), but this relation is not required (i.e., it may be 'Nothing').
--
class Typeable table  => Table (table :: (* -> *) -> *) where

    -- | A data type representing the types of primary keys for this table. This type must be an instance of 'Generic'.
    type PrimaryKey table (column :: * -> *) :: *

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

    -- | Given a table, this should return the PrimaryKey from the table. By keeping this polymorphic over column,
    --   we ensure that the primary key values come directly from the table (i.e., they can't be arbitrary constants)
    primaryKey :: table column -> PrimaryKey table column

    tblFieldSettings :: TableSettings table
    default tblFieldSettings :: ( Generic (TableSettings table)
                                , GDefaultTableFieldSettings (Rep (TableSettings table) ())) => TableSettings table
    tblFieldSettings = defTblFieldSettings

    -- dbTableName :: Proxy table -> Text
    -- dbTableName (_ :: Proxy table) = T.pack (tyConName (typeRepTyCon (typeOf (undefined :: table Dummy))))

    reifyTableSchema :: Proxy table -> ReifiedTableSchema
    default reifyTableSchema :: ( GReifySchema (Rep (TableSettings table) ())
                                , Generic (TableSettings table) ) =>
                                Proxy table -> ReifiedTableSchema
    reifyTableSchema (table :: Proxy table) = gReifySchema (from' (tblFieldSettings :: TableSettings table))

    makeSqlValues :: table Identity -> [SqlValue]
    default makeSqlValues :: (Generic (table Identity), GMakeSqlValues (Rep (table Exposed) ()) (Rep (table Identity) ())) => table Identity -> [SqlValue]
    makeSqlValues table = gMakeSqlValues (Proxy :: Proxy (Rep (table Exposed) ())) (from' table)

    tableFromSqlValues :: FromSqlValuesM (table Identity)
    default tableFromSqlValues :: ( Generic (table Identity)
                                  , GFromSqlValues (Rep (table Exposed)) (Rep (table Identity)) ) =>
                                  FromSqlValuesM (table Identity)
    tableFromSqlValues = to <$> gFromSqlValues (Proxy :: Proxy (Rep (table Exposed)))

    tableValuesNeeded :: Proxy table -> Int
    default tableValuesNeeded :: ( GReifySchema (Rep (TableSettings table) ())
                                 , Generic (TableSettings table) ) =>
                                 Proxy table -> Int
    tableValuesNeeded (table :: Proxy table) = length (gReifySchema (from' (tblFieldSettings :: TableSettings table)))

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
          withProxy f = f Proxy --) withProxy f = f (Proxy :: Proxy (Rep (TableSettings table) ())))

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

type AllValues f xf xExposed = (( Generic xf
                                , Generic xExposed
                                , GAllValues f (Rep xExposed ()) (Rep xf ())))

allValues' :: AllValues f xf xExposed =>
              Proxy xExposed -> (forall a. FieldSchema a => Columnar' f a -> b) -> xf -> [b]
allValues' (Proxy :: Proxy xExposed) f x =
    gAllValues (Proxy :: Proxy (Rep xExposed ())) f (from' x)

allValues :: Table t => (forall a. FieldSchema a => Columnar' f a -> b) -> t f -> [b]
allValues f (t :: t f) = fieldAllValues f t

class GDefaultTableFieldSettings x where
    gDefTblFieldSettings :: Proxy x -> x
instance (GDefaultTableFieldSettings a, GDefaultTableFieldSettings b) => GDefaultTableFieldSettings (a :|: b) where
    gDefTblFieldSettings (_ :: Proxy (a :|: b)) = gDefTblFieldSettings (Proxy :: Proxy a) :|: gDefTblFieldSettings (Proxy :: Proxy b)
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (D1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (D1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (C1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (C1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance (GDefaultTableFieldSettings (a p), GDefaultTableFieldSettings (b p)) => GDefaultTableFieldSettings ((a :*: b) p) where
    gDefTblFieldSettings (_ :: Proxy ((a :*: b) p)) = gDefTblFieldSettings (Proxy :: Proxy (a p)) :*: gDefTblFieldSettings (Proxy :: Proxy (b p))

instance (Table table, FieldSchema field, Selector f ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (TableField table field)) p) where
    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (TableField table field)) p)) = M1 (K1 s)
        where s = defFieldSettings (T.pack (selName (undefined :: S1 f (K1 Generic.R (TableField table field)) ())))

instance ( Table table, Table related
         , Selector f

         , Generic (PrimaryKey related (TableField related))
         , Generic (PrimaryKey related (TableField table))
         , GChangeRep (Rep (PrimaryKey related Exposed) ())
                      (Rep (PrimaryKey related (TableField related)) ()) (Rep (PrimaryKey related (TableField table)) ())
                      (TableField related) (TableField table) ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (ForeignKey related (TableField table))) p) where

    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (ForeignKey related (TableField table))) p )) = M1 $ K1 $ ForeignKey $ primaryKeySettings'
        where tableSettings = tblFieldSettings :: TableSettings related
              primaryKeySettings :: PrimaryKey related (TableField related)
              primaryKeySettings = primaryKey tableSettings

              primaryKeySettings' :: PrimaryKey related (TableField table)
              primaryKeySettings' = to' (gChangeRep (Proxy :: Proxy (Rep (PrimaryKey related Exposed) ())) convertToForeignKeyField (from' primaryKeySettings))

              convertToForeignKeyField :: Columnar' (TableField related) c -> Columnar' (TableField table) c
              convertToForeignKeyField (Columnar' tf) =
                  Columnar' $
                  tf { _fieldName = keyName <> "_" <> _fieldName tf
                     , _fieldConstraints = removeConstraints (_fieldConstraints tf) }


              removeConstraints = filter (\x -> x /= SQLPrimaryKey && x /= SQLAutoIncrement)

              keyName = T.pack (selName (undefined :: S1 f (K1 Generic.R (ForeignKey related (TableField table))) ()))

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
        M1 $ K1 $ ForeignKey $ settings
        where M1 (K1 (ForeignKey nonNullSettings)) = gDefTblFieldSettings (Proxy :: Proxy (S1 f (K1 Generic.R (ForeignKey related (TableField table))) p))
              nonNullSettingsRep = from' nonNullSettings :: Rep (PrimaryKey related (TableField table)) ()

              settings :: PrimaryKey related (Nullable (TableField table))
              settings = to' (gChangeRep (Proxy :: Proxy (Rep (PrimaryKey related Exposed) ())) removeNotNullConstraints nonNullSettingsRep)

              removeNotNullConstraints :: Columnar' (TableField table) ty -> Columnar' (Nullable (TableField table)) ty
              removeNotNullConstraints (Columnar' tf) =
                  Columnar' $
                            -- Nullable $
                  tf { _fieldSettings = MaybeFieldSettings (_fieldSettings tf) }

class GReifySchema genSchema where
    gReifySchema :: genSchema -> ReifiedTableSchema
instance (GReifySchema a, GReifySchema b) => GReifySchema (a :|: b) where
    gReifySchema (a :|: b) = gReifySchema a ++ gReifySchema b
instance GReifySchema (a p) => GReifySchema (D1 f a p) where
    gReifySchema (M1 a) = gReifySchema a
instance GReifySchema (a p) => GReifySchema (C1 f a p) where
    gReifySchema (M1 a) = gReifySchema a
instance GReifySchema (a p) => GReifySchema (S1 f a p) where
    gReifySchema (M1 a) = gReifySchema a
instance FieldSchema f => GReifySchema (K1 Generic.R (TableField t f) p) where
    gReifySchema (K1 x) = [(_fieldName x, fieldColDesc (_fieldSettings x) (_fieldConstraints x))]
instance FieldSchema f => GReifySchema (K1 Generic.R (Nullable (TableField t) f) p) where
    gReifySchema (K1 (Nullable x)) = [(_fieldName x, fieldColDesc (_fieldSettings x) (_fieldConstraints x))]
instance (GReifySchema (a p), GReifySchema (b p)) => GReifySchema ((a :*: b) p) where
    gReifySchema (a :*: b) = gReifySchema a ++ gReifySchema b
instance ( Generic (PrimaryKey related f)
         , Table related
         , GReifySchema (Rep (PrimaryKey related f) ()) ) =>
    GReifySchema (K1 Generic.R (ForeignKey related f) p) where

    gReifySchema (K1 (ForeignKey x :: ForeignKey related f)) = gReifySchema (from' x)


class GFromSqlValues ty schema where
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

-- data Entity table f = Entity
--                     { phantomFields :: PhantomFields table f
--                     , tableFields   :: table f }
-- instance (Show (PhantomFields table Column), Show (table Column)) => Show (Entity table Column) where
--     showsPrec d (Entity phantoms table) =
--         showParen (d > 10) $
--         showString "Entity " . showsPrec 11 phantoms . showString " " . showsPrec 11 table

-- instance Table table => FromSqlValues (Entity table Column) where
--     fromSqlValues' = Entity <$> phantomFromSqlValues (Proxy :: Proxy table) <*> (tableFromSqlValues :: FromSqlValuesM (table Column))
--     valuesNeeded (_ :: Proxy (Entity tbl Column)) = phantomValuesNeeded (Proxy :: Proxy tbl) + tableValuesNeeded (Proxy :: Proxy tbl)

-- * Field and table support

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
instance (FromSqlValues a, FromSqlValues b) => FromSqlValues (a :|: b) where
    fromSqlValues' = (:|:) <$> fromSqlValues' <*> fromSqlValues'
    valuesNeeded (_ :: Proxy (a :|: b)) = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b)
instance Table tbl => FromSqlValues (tbl Identity) where
    fromSqlValues' = tableFromSqlValues
    valuesNeeded _ = tableValuesNeeded (Proxy :: Proxy tbl)
instance (FromSqlValues a, FromSqlValues b) => FromSqlValues (a, b) where
    fromSqlValues' = (,) <$> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b)

data ForeignKey table column = ForeignKey (PrimaryKey table column)
deriving instance Show (PrimaryKey table column) => Show (ForeignKey table column)
reference :: ForeignKey table column -> PrimaryKey table column
reference (ForeignKey x) = x
tentative :: Nullable c x -> c (Maybe x)
tentative (Nullable x) = x

instance FieldSchema Int where
    data FieldSettings Int = IntFieldDefault
                             deriving Show
    defSettings = IntFieldDefault
    colDescFromSettings _ = notNull $
                            SqlColDesc
                            { colType = SqlNumericT
                            , colSize = Nothing
                            , colOctetLength = Nothing
                            , colDecDigits = Nothing
                            , colNullable = Nothing }
    makeSqlValue i = SqlInteger (fromIntegral i)
    fromSqlValue = fromSql <$> popSqlValue

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

