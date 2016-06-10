{-# LANGUAGE UndecidableInstances #-}

-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Tables
    (
    -- * Database Types
      Database(..), GenDatabaseTable(..), DatabaseTable(..), DatabaseSettings
    , ReifiedDatabaseSchema, ReifiedTableSchema
    , autoDbSettings
    , allTableSettings

    , SqlValue'(..)
    , Lenses, LensFor(..)

    -- * Columnar and Column Tags
    , Columnar, Columnar'(..)
    , Nullable, TableField(..)
    , fieldName, fieldConstraints, fieldSchema, maybeFieldSchema

    , TableSettings

    -- * Tables
    , Table(..), defTblFieldSettings
    , reifyTableSchema, tableValuesNeeded
    , pk
    , pkAllValues, fieldAllValues, pkChangeRep, changeRep
    , pkMakeSqlValues, makeSqlValues

    -- * Fields
    , HasDefaultFieldSchema(..), FieldSchema(..), FromSqlValuesM, FromSqlValues(..)
    , popSqlValue, peekSqlValue )
    where

import Database.Beam.SQL.Types

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity

import Data.Proxy
import Data.Typeable
import Data.Text (Text)
import Data.List
import Data.Char
import Data.String
import qualified Data.Text as T

import Database.HDBC (SqlValue(..))

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
newtype SqlValue' x = SqlValue' SqlValue

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
data TableField (table :: (* -> *) -> *) ty = TableField
                                            { _fieldName        :: Text             -- ^ The field name
                                            , _fieldConstraints :: [SQLConstraint]  -- ^ Constraints for the field (such as AutoIncrement, PrimaryKey, etc)
                                            , _fieldSchema      :: FieldSchema ty   -- ^ SQL storage informationa for the field
                                            }
deriving instance Show (TableField t ty)

fieldName :: Lens' (TableField table ty) Text
fieldName f (TableField name cs s) = (\name' -> TableField name' cs s) <$> f name
fieldConstraints :: Lens' (TableField table ty) [SQLConstraint]
fieldConstraints f (TableField name cs s) = (\cs' -> TableField name cs' s) <$> f cs
fieldSchema :: Lens (TableField table a) (TableField table b) (FieldSchema a) (FieldSchema b)
fieldSchema f (TableField name cs s) = (\s' -> TableField name cs s') <$> f s

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
class Typeable table  => Table (table :: (* -> *) -> *) where

    -- | A data type representing the types of primary keys for this table.
    --   In order to play nicely with the default deriving mechanism, this type must be an instance of 'Generic'.
    data PrimaryKey table (column :: * -> *) :: *

    -- | Given a table, this should return the PrimaryKey from the table. By keeping this polymorphic over column,
    --   we ensure that the primary key values come directly from the table (i.e., they can't be arbitrary constants)
    primaryKey :: table column -> PrimaryKey table column

    tblFieldSettings :: TableSettings table
    default tblFieldSettings :: ( Generic (TableSettings table)
                                , GDefaultTableFieldSettings (Rep (TableSettings table) ())) => TableSettings table
    tblFieldSettings = defTblFieldSettings

    zipTablesM :: Monad m => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> table f -> table g -> m (table h)
    default zipTablesM :: ( GZipTables f g h (Rep (table Exposed)) (Rep (table f)) (Rep (table g)) (Rep (table h))
                         , Generic (table f), Generic (table g), Generic (table h) ) =>
                        Monad m => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> table f -> table g -> m (table h)
    zipTablesM combine f g = do hRep <- gZipTables (Proxy :: Proxy (Rep (table Exposed))) combine (from' f) (from' g)
                                return (to' hRep)

    zipPkM :: Monad m => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> PrimaryKey table f -> PrimaryKey table g -> m (PrimaryKey table h)
    default zipPkM :: ( GZipTables f g h (Rep (PrimaryKey table Exposed)) (Rep (PrimaryKey table f)) (Rep (PrimaryKey table g)) (Rep (PrimaryKey table h))
                      , Generic (PrimaryKey table f), Generic (PrimaryKey table g), Generic (PrimaryKey table h)
                      , Monad m) => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> PrimaryKey table f -> PrimaryKey table g -> m (PrimaryKey table h)
    zipPkM combine f g = do hRep <- gZipTables (Proxy :: Proxy (Rep (PrimaryKey table Exposed))) combine (from' f) (from' g)
                            return (to' hRep)

reifyTableSchema :: Table table => Proxy table -> ReifiedTableSchema
reifyTableSchema (Proxy :: Proxy table) = fieldAllValues (\(Columnar' (TableField name constraints settings)) ->
                                                                  (name, fieldColDesc settings constraints)) (tblFieldSettings :: TableSettings table)

tableValuesNeeded :: Table table => Proxy table -> Int
tableValuesNeeded (Proxy :: Proxy table) = length (fieldAllValues (const ()) (tblFieldSettings :: TableSettings table))

pkAllValues :: forall f b t. Table t => (forall a. Columnar' f a -> b) -> PrimaryKey t f -> [b]
pkAllValues (f :: forall a. Columnar' f a -> b) (k :: PrimaryKey t f) = execWriter (zipPkM combine k k)
    where combine :: Columnar' f a -> Columnar' f a -> Writer [b] (Columnar' f a)
          combine x _ = do tell [f x]
                           return x

fieldAllValues :: forall t f b. Table t => (forall a. Columnar' f a -> b) -> t f -> [b]
fieldAllValues  (f :: forall a. Columnar' f a -> b) (tbl :: t f) = execWriter (zipTablesM combine tbl tbl)
    where combine :: Columnar' f a -> Columnar' f a -> Writer [b] (Columnar' f a)
          combine x _ = do tell [f x]
                           return x

pkChangeRep :: forall t f g. Table t => (forall a. Columnar' f a -> Columnar' g a) -> PrimaryKey t f -> PrimaryKey t g
pkChangeRep f k = runIdentity (zipPkM (\x _ -> return (f x)) k k)

changeRep :: forall t f g. Table t => (forall a. Columnar' f a -> Columnar' g a) -> t f -> t g
changeRep f tbl = runIdentity (zipTablesM (\x _ -> return (f x)) tbl tbl)

pkMakeSqlValues :: forall t. Table t => PrimaryKey t Identity -> PrimaryKey t SqlValue'
pkMakeSqlValues k = runIdentity (zipPkM (\(Columnar' x) (Columnar' tf) -> return (Columnar' (SqlValue' (fsMakeSqlValue (_fieldSchema tf) x)))) k (primaryKey tblFieldSettings))

makeSqlValues :: forall t. Table t => t Identity -> t SqlValue'
makeSqlValues tbl = runIdentity (zipTablesM (\(Columnar' x) (Columnar' tf) -> return (Columnar' (SqlValue' (fsMakeSqlValue (_fieldSchema tf) x)))) tbl tblFieldSettings)

-- | Synonym for 'primaryKey'
pk :: forall t f. Table t => t f -> PrimaryKey t f
pk = primaryKey

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

fieldColDesc :: FieldSchema fs -> [SQLConstraint] -> SQLColumnSchema
fieldColDesc schema cs = let base = fsColDesc schema
                         in base { csConstraints = csConstraints base ++ cs }

class GZipTables f g h (exposedRep :: * -> *) fRep gRep hRep where
    gZipTables :: Monad m => Proxy exposedRep -> (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a)) -> fRep () -> gRep () -> m (hRep ())
instance ( GZipTables f g h exp1 f1 g1 h1
         , GZipTables f g h exp2 f2 g2 h2) =>
    GZipTables f g h (exp1 :*: exp2) (f1 :*: f2) (g1 :*: g2) (h1 :*: h2) where

        gZipTables _ combine (f1 :*: f2) (g1 :*: g2) =
            do h1 <- gZipTables (Proxy :: Proxy exp1) combine f1 g1
               h2 <- gZipTables (Proxy :: Proxy exp2) combine f2 g2
               return (h1 :*: h2)
instance GZipTables f g h exp fRep gRep hRep =>
    GZipTables f g h (M1 x y exp) (M1 x y fRep) (M1 x y gRep) (M1 x y hRep) where
        gZipTables _ combine (M1 f) (M1 g) = do h <- gZipTables (Proxy :: Proxy exp) combine f g
                                                return (M1 h)
instance ( fa ~ Columnar f a
         , ga ~ Columnar g a
         , ha ~ Columnar h a) =>
    GZipTables f g h (K1 Generic.R (Exposed a)) (K1 Generic.R fa) (K1 Generic.R ga) (K1 Generic.R ha) where
        gZipTables _ combine (K1 f) (K1 g) = do Columnar' h <- combine (Columnar' f :: Columnar' f a) (Columnar' g :: Columnar' g a)
                                                return (K1 (h :: Columnar h a))
instance ( Generic (PrimaryKey rel f)
         , Generic (PrimaryKey rel g)
         , Generic (PrimaryKey rel h)

         , GZipTables f g h (Rep (PrimaryKey rel Exposed)) (Rep (PrimaryKey rel f)) (Rep (PrimaryKey rel g)) (Rep (PrimaryKey rel h))) =>
    GZipTables f g h (K1 Generic.R (PrimaryKey rel Exposed)) (K1 Generic.R (PrimaryKey rel f)) (K1 Generic.R (PrimaryKey rel g)) (K1 Generic.R (PrimaryKey rel h)) where
    gZipTables _ combine (K1 f) (K1 g) = do hRep <- gZipTables (Proxy :: Proxy (Rep (PrimaryKey rel Exposed))) combine (from' f) (from' g)
                                            return (K1 (to' hRep))

instance  ( Generic (PrimaryKey rel (Nullable f))
          , Generic (PrimaryKey rel (Nullable g))
          , Generic (PrimaryKey rel (Nullable h))

          , GZipTables f g h (Rep (PrimaryKey rel (Nullable Exposed))) (Rep (PrimaryKey rel (Nullable f))) (Rep (PrimaryKey rel (Nullable g))) (Rep (PrimaryKey rel (Nullable h)))) =>
         GZipTables f g h
                    (K1 Generic.R (PrimaryKey rel (Nullable Exposed)))
                    (K1 Generic.R (PrimaryKey rel (Nullable f)))
                    (K1 Generic.R (PrimaryKey rel (Nullable g)))
                    (K1 Generic.R (PrimaryKey rel (Nullable h))) where
    gZipTables _ combine (K1 f) (K1 g) = do hRep <- gZipTables (Proxy :: Proxy (Rep (PrimaryKey rel (Nullable Exposed)))) combine (from' f) (from' g)
                                            return (K1 (to' hRep))

class GDefaultTableFieldSettings x where
    gDefTblFieldSettings :: Proxy x -> x
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (D1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (D1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance GDefaultTableFieldSettings (p x) => GDefaultTableFieldSettings (C1 f p x) where
    gDefTblFieldSettings (_ :: Proxy (C1 f p x)) = M1 $ gDefTblFieldSettings (Proxy :: Proxy (p x))
instance (GDefaultTableFieldSettings (a p), GDefaultTableFieldSettings (b p)) => GDefaultTableFieldSettings ((a :*: b) p) where
    gDefTblFieldSettings (_ :: Proxy ((a :*: b) p)) = gDefTblFieldSettings (Proxy :: Proxy (a p)) :*: gDefTblFieldSettings (Proxy :: Proxy (b p))

instance (Table table, HasDefaultFieldSchema field, Selector f ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (TableField table field)) p) where
    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (TableField table field)) p)) = M1 (K1 s)
        where s = TableField (T.pack name) [] defFieldSchema
              name = unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (TableField table field)) ()))

instance ( Table table, Table related
         , Selector f

         , Generic (PrimaryKey related (TableField related))
         , Generic (PrimaryKey related (TableField table))
         , GZipTables (TableField related) (TableField related) (TableField table)
                      (Rep (PrimaryKey related Exposed))
                      (Rep (PrimaryKey related (TableField related))) (Rep (PrimaryKey related (TableField related))) (Rep (PrimaryKey related (TableField table))) ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (PrimaryKey related (TableField table))) p) where

    gDefTblFieldSettings _ = M1 . K1 $ primaryKeySettings'
        where tableSettings = tblFieldSettings :: TableSettings related
              primaryKeySettings :: PrimaryKey related (TableField related)
              primaryKeySettings = primaryKey tableSettings

              primaryKeySettings' :: PrimaryKey related (TableField table)
              primaryKeySettings' = to' (runIdentity (gZipTables (Proxy :: Proxy (Rep (PrimaryKey related Exposed))) convertToForeignKeyField (from' primaryKeySettings) (from' primaryKeySettings)))

              convertToForeignKeyField :: Columnar' (TableField related) c -> Columnar' (TableField related) c -> Identity (Columnar' (TableField table) c)
              convertToForeignKeyField (Columnar' tf) _ =
                  pure . Columnar' $
                  tf { _fieldName = keyName <> "__" <> _fieldName tf
                     , _fieldConstraints = removeConstraints (_fieldConstraints tf) }


              removeConstraints = filter (\x -> x /= SQLPrimaryKey && x /= SQLAutoIncrement)

              keyName = T.pack (unCamelCaseSel (selName (undefined :: S1 f (K1 Generic.R (PrimaryKey related (TableField table))) ())))

instance ( Table table, Table related
         , Selector f

         , Generic (PrimaryKey related (TableField related))
         , Generic (PrimaryKey related (TableField table))
         , Generic (PrimaryKey related (Nullable (TableField table)))
         , GZipTables (TableField table) (TableField table) (Nullable (TableField table))
                      (Rep (PrimaryKey related Exposed))
                      (Rep (PrimaryKey related (TableField table)))
                      (Rep (PrimaryKey related (TableField table)))
                      (Rep (PrimaryKey related (Nullable (TableField table))))
         , GZipTables (TableField related) (TableField related) (TableField table)
                      (Rep (PrimaryKey related Exposed))
                      (Rep (PrimaryKey related (TableField related)))
                      (Rep (PrimaryKey related (TableField related)))
                      (Rep (PrimaryKey related (TableField table)))) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (PrimaryKey related (Nullable (TableField table)))) p) where

    gDefTblFieldSettings _ =
        M1 . K1 $ settings
        where M1 (K1 nonNullSettings) = gDefTblFieldSettings (Proxy :: Proxy (S1 f (K1 Generic.R (PrimaryKey related (TableField table))) p))
              nonNullSettingsRep = from' nonNullSettings :: Rep (PrimaryKey related (TableField table)) ()

              settings :: PrimaryKey related (Nullable (TableField table))
              settings = to' (runIdentity (gZipTables (Proxy :: Proxy (Rep (PrimaryKey related Exposed))) removeNotNullConstraints nonNullSettingsRep nonNullSettingsRep))

              removeNotNullConstraints :: Columnar' (TableField table) ty -> Columnar' (TableField table) ty -> Identity (Columnar' (Nullable (TableField table)) ty)
              removeNotNullConstraints (Columnar' tf) _ =
                  pure . Columnar' $
                  tf { _fieldSchema = maybeFieldSchema (_fieldSchema tf) }

data FieldSchema ty = FieldSchema
                    { fsColDesc :: SQLColumnSchema
                    , fsHumanReadable :: String
                    , fsMakeSqlValue :: ty -> SqlValue
                    , fsFromSqlValue :: FromSqlValuesM ty }
instance Show (FieldSchema ty) where
    show (FieldSchema desc hr _ _) = concat ["FieldSchema (", show desc, ") (", show hr, ") _ _"]

-- | Type class for types which can construct a default 'TableField' given a column name.
class HasDefaultFieldSchema fs where
    defFieldSchema :: FieldSchema fs

type FromSqlValuesM a = ExceptT String (State [SqlValue]) a
popSqlValue, peekSqlValue :: FromSqlValuesM SqlValue
popSqlValue = do st <- get
                 put (tail st)
                 return (head st)
peekSqlValue = head <$> get
class FromSqlValues a where
    fromSqlValues' :: FromSqlValuesM a
    valuesNeeded :: Proxy a -> Int
    valuesNeeded _ = 1

    default fromSqlValues' :: HasDefaultFieldSchema a => FromSqlValuesM a
    fromSqlValues' = fsFromSqlValue defFieldSchema
instance Table tbl => FromSqlValues (tbl Identity) where
    fromSqlValues' = zipTablesM combine settings settings
        where settings :: TableSettings tbl
              settings = tblFieldSettings

              combine (Columnar' tf) _ = Columnar' <$> fsFromSqlValue (_fieldSchema tf)
    valuesNeeded _ = tableValuesNeeded (Proxy :: Proxy tbl)
instance (FromSqlValues a, FromSqlValues b) => FromSqlValues (a, b) where
    fromSqlValues' = (,) <$> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b)
instance (FromSqlValues a, FromSqlValues b, FromSqlValues c) => FromSqlValues (a, b, c) where
    fromSqlValues' = (,,) <$> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b) + valuesNeeded (Proxy :: Proxy c)
instance (FromSqlValues a, FromSqlValues b, FromSqlValues c, FromSqlValues d) => FromSqlValues (a, b, c, d) where
    fromSqlValues' = (,,,) <$> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b) + valuesNeeded (Proxy :: Proxy c) + valuesNeeded (Proxy :: Proxy d)
instance (FromSqlValues a, FromSqlValues b, FromSqlValues c, FromSqlValues d, FromSqlValues e) => FromSqlValues (a, b, c, d, e) where
    fromSqlValues' = (,,,,) <$> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b) + valuesNeeded (Proxy :: Proxy c) + valuesNeeded (Proxy :: Proxy d) + valuesNeeded (Proxy :: Proxy e)

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

unCamelCaseSel :: String -> String
unCamelCaseSel ('_':xs) = unCamelCaseSel xs
unCamelCaseSel x = case unCamelCase x of
                      [xs] -> xs
                      _:xs -> intercalate "_" xs

maybeFieldSchema :: FieldSchema ty -> FieldSchema (Maybe ty)
maybeFieldSchema base = let SQLColumnSchema desc constraints =  fsColDesc base
                            in FieldSchema
                               { fsColDesc = SQLColumnSchema desc (filter (/=SQLNotNull) constraints)
                               , fsHumanReadable = "maybeFieldSchema (" ++ fsHumanReadable base ++ ")"
                               , fsMakeSqlValue = \case
                                                    Nothing -> SqlNull
                                                    Just x -> fsMakeSqlValue base x
                               , fsFromSqlValue = peekSqlValue >>= \case
                                                    SqlNull -> Nothing <$ popSqlValue
                                                    _ -> Just <$> fsFromSqlValue base }
