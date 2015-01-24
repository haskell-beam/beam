{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances, MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, RankNTypes #-}
-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Types where

import Database.Beam.SQL.Types

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import Data.Time.Clock
import Data.Text (Text, unpack)
import Data.Proxy
import Data.Default
import Data.String
import Data.Typeable

import Database.HDBC ( SqlColDesc(..), SqlTypeId(..), SqlValue(..)
                     , fromSql)

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

-- * Database types

-- | Table composition operator
data t1 :#: t2
data TableSchema t

type DatabaseSchema = [(Text, ReifiedTableSchema)]
type ReifiedTableSchema = [(Text, SQLColumnSchema)]

data GenTable where
    GenTable :: Table t => Proxy t -> GenTable
newtype Gen n = Gen n
type instance NameFor (Gen n) = NameFor n
instance Locator n Found => Locator (Gen n) Found where
    type LocateResult (Gen n) Found = LocateResult (Gen n) Found
    locate (Gen n) r = locate n r

class ToDatabaseSchema a where
    reifyDBSchema :: Proxy a -> DatabaseSchema
    tableNames :: Proxy a -> [GenTable]

class ToDatabaseSchema (DBSchema d) => Database d where
    type DBSchema d :: *
    type DBSchema d = DBSchemaForGeneric (Rep d ())

    dbSchema' :: Proxy d -> Proxy (DBSchema d)
    default dbSchema' :: (Generic d, DBSchemaForGeneric (Rep d ()) ~ DBSchema d) => Proxy d -> Proxy (DBSchema d)
    dbSchema' _ = Proxy

-- ** Generic database deriving support

type family DBSchemaForGeneric x where
    DBSchemaForGeneric (M1 D f p x) = DBSchemaForGeneric (p x)
    DBSchemaForGeneric (M1 C f p x) = DBSchemaForGeneric (p x)
    DBSchemaForGeneric (M1 S f p x) = DBSchemaForGeneric (p x)
    DBSchemaForGeneric ((:*:) f g x) = DBSchemaForGeneric (f x) :#: DBSchemaForGeneric (g x)
    DBSchemaForGeneric (K1 Generic.R s x) = s

-- * Tables
data TableId = TableId deriving (Show, Generic, Typeable)

data GenField t f

type FullSchema table = PhantomFieldSchema table :|: Schema table

type DefaultPrimaryKeyFor table = IntField TableId

type family LookupFields table names where
    LookupFields schema (name1 :|: name2) = LookupFields schema name1 :|: LookupFields schema name2
    LookupFields schema name = LocateResultField schema (Locate schema name)

class ( Typeable table
      , Locator (FullSchema table) (LocateAll (FullSchema table) (PrimaryKey table))
      , Default (LocateAll (FullSchema table) (PrimaryKey table)))  => Table table where
    type Schema table :: *
    type Schema table = SchemaForGeneric (Rep table)

    -- | Phantom fields are fields that are not part of the table definitions but are nonetheless stored in the table
    type PhantomFieldSchema table :: *
    type PhantomFieldSchema table = DefaultPrimaryKeyFor table

    type PrimaryKey table :: *
    type PrimaryKey table = TableId

    dbTableName :: Proxy table -> Text
    default dbTableName :: ( Generic table, Constructor c
                           , GOneConstructor (Rep table ()) ~ (t :: * -> (* -> *) -> * -> *) c f a) => Proxy table -> Text
    dbTableName tbl = fromString (conName (onlyConstructor tbl))
        where onlyConstructor :: Proxy table -> GOneConstructor (Rep table ())
              onlyConstructor _ = undefined

    reifyTableSchema :: Proxy table -> ReifiedTableSchema
    default reifyTableSchema :: ( Generic table
                                , GReifySchema (WrapFields (GenField table) (PhantomFieldSchema table :|: SchemaForGeneric (Rep table))) ) => Proxy table -> ReifiedTableSchema
    reifyTableSchema table = reifyGenericSchema (schemaProxy table)
        where schemaProxy :: Proxy table -> Proxy (WrapFields (GenField table) (PhantomFieldSchema table :|: SchemaForGeneric (Rep table)))
              schemaProxy _ = Proxy

    getSchema :: table -> Schema table
    default getSchema :: (Generic table, GHasSchema (Rep table)
                         , Schema table ~ SchemaForGeneric (Rep table)) => table -> Schema table
    getSchema table = gGetSchema (from table)

    fromSchema :: Schema table -> table
    default fromSchema :: ( Generic table, GHasSchema (Rep table)
                          , Schema table ~ SchemaForGeneric (Rep table)) => Schema table -> table
    fromSchema schema = to (gFromSchema schema)

    makeSqlValues :: table -> [SqlValue]
    default makeSqlValues :: (Generic table, GMakeSqlValues (Rep table)) => table -> [SqlValue]
    makeSqlValues table = makePhantomValues table ++ gMakeSqlValues (from' table) -- TODO The SQLNull is a hack for autoincrement primary keys...
        where from' :: Generic table => table -> Rep table a
              from' = from

    -- | The default behavior is to assign the default phantom field schema (just an integer primary key) a sinlge value of null, to let autoincrement work
    makePhantomValues :: table -> [SqlValue]
    makePhantomValues _ = [SqlNull]

instance (Table table) => Field table TableId where
    type FieldInTable table TableId = IntField table

    fieldName _ _ = "id"

    fieldConstraints _ _ = [SQLPrimaryKey]

-- | Schema combinator. Takes two fields or schemas and combines them into a new schema
data a :|: b = a :|: b
               deriving Show
type instance NameFor (a :|: b) = NameFor a :|: NameFor b
infixr 3 :|:

-- ** Generic Table deriving support

-- | The generic table support derives Table instances for datatypes with just one constructor
class GHasSchema (x :: * -> *) where
    gGetSchema :: x a -> SchemaForGeneric x
    gFromSchema :: SchemaForGeneric x -> x a
instance GHasSchema p => GHasSchema (M1 D f p) where
    gGetSchema (M1 x) = gGetSchema x
    gFromSchema x = M1 (gFromSchema x)
instance GHasSchema p => GHasSchema (M1 C f p) where
    gGetSchema (M1 x) = gGetSchema x
    gFromSchema x = M1 (gFromSchema x)
instance GHasSchema p => GHasSchema (M1 S f p) where
    gGetSchema (M1 x) = gGetSchema x
    gFromSchema x = M1 (gFromSchema x)
instance (GHasSchema f, GHasSchema g) => GHasSchema (f :*: g) where
    gGetSchema (f :*: g) = gGetSchema f :|: gGetSchema g
    gFromSchema (f :|: g) = gFromSchema f :*: gFromSchema g
instance GHasSchema (K1 Generic.R schema) where
    gGetSchema (K1 x) = x
    gFromSchema x = K1 x

class GReifySchema genSchema where
    reifyGenericSchema :: Proxy genSchema -> ReifiedTableSchema

instance (GReifySchema a, GReifySchema b) => GReifySchema (a :|: b) where
    reifyGenericSchema schema = reifyGenericSchema aProxy ++ reifyGenericSchema bProxy
        where (aProxy, bProxy) = proxiesFor schema
              proxiesFor :: Proxy (a :|: b) -> (Proxy a, Proxy b)
              proxiesFor _ = (Proxy, Proxy)
instance Field t (NameFor f) => GReifySchema (GenField t f) where
    reifyGenericSchema schema = [(fieldName table field, fieldColDesc table field)]
        where (table, field) = fieldProxyFor schema
              fieldProxyFor :: Proxy (GenField t f) -> (Proxy t, Proxy (NameFor f))
              fieldProxyFor _ = (Proxy, Proxy)

type family GOneConstructor g where
    GOneConstructor (M1 D f p x) = p x

type family SchemaForGeneric g where
    SchemaForGeneric (M1 D f p) = SchemaForGeneric p
    SchemaForGeneric (M1 C f p) = SchemaForGeneric p
    SchemaForGeneric (M1 S f p) = SchemaForGeneric p
    SchemaForGeneric (f :*: g) = SchemaForGeneric f :|: SchemaForGeneric g
    SchemaForGeneric (K1 Generic.R s) = s

class GMakeSqlValues (x :: * -> *) where
    gMakeSqlValues :: x a -> [SqlValue]
instance GMakeSqlValues p => GMakeSqlValues (D1 f p) where
    gMakeSqlValues (M1 x) = gMakeSqlValues x
instance GMakeSqlValues p => GMakeSqlValues (C1 f p) where
    gMakeSqlValues (M1 x) = gMakeSqlValues x
instance GMakeSqlValues p => GMakeSqlValues (S1 f p) where
    gMakeSqlValues (M1 x) = gMakeSqlValues x
instance (GMakeSqlValues f, GMakeSqlValues g) => GMakeSqlValues (f :*: g) where
    gMakeSqlValues (f :*: g) = gMakeSqlValues f ++ gMakeSqlValues g
instance GMakeSqlValues U1 where
    gMakeSqlValues _ = []
instance FieldSchema x => GMakeSqlValues (K1 Generic.R x) where
    gMakeSqlValues (K1 x) = [makeSqlValue x]

type PrimaryKeySchema table = LocateResult (FullSchema table) (LocateAll (FullSchema table) (PrimaryKey table))

type family LocateAll schema fields where
    LocateAll schema (a :|: b) = LocateAll schema a :|: LocateAll schema b
    LocateAll schema a = Locate schema a

type FromSqlValuesM a = ErrorT String (State [SqlValue]) a
popSqlValue, peekSqlValue :: FromSqlValuesM SqlValue
popSqlValue = do st <- get
                 put (tail st)
                 return (head st)
peekSqlValue = head <$> get
class FromSqlValues a where
    fromSqlValues' :: FromSqlValuesM a
instance (FromSqlValues a, FromSqlValues b) => FromSqlValues (a :|: b) where
    fromSqlValues' = (:|:) <$> fromSqlValues' <*> fromSqlValues'

class SchemaPart a where
    mapSchema :: (forall x. SchemaPart x => x -> f x) -> a -> WrapFields f a
    default mapSchema :: WrapFields f a ~ f a => (forall x. SchemaPart x => x -> f x) -> a -> WrapFields f a
    mapSchema f a = f a
instance (SchemaPart a, SchemaPart b) => SchemaPart (a :|: b) where
    mapSchema f (a :|: b) = mapSchema f a :|: mapSchema f b

type family WrapFields f schema where
    WrapFields f (a :|: b) = WrapFields f a :|: WrapFields f b
    WrapFields f a = f a

-- * Primary keys

primaryKeyForTable :: Table table =>
                      PhantomFieldSchema table -> table -> PrimaryKeySchema table
primaryKeyForTable phantomData (tbl :: table) = getFields (phantomData :|: getSchema tbl) (undefined :: PrimaryKey table)

instance (Locator schema a, Locator schema b) => Locator schema (a :|: b) where
    type LocateResult schema (a :|: b) = LocateResult schema a :|: LocateResult schema b

    locate schema (_ :: (a :|: b)) = locate schema (undefined :: a) :|: locate schema (undefined :: b)

-- * Field locating support

-- | The L and R datatypes help the system navigate through the nested `:|:` table constructors
data L a = L a
data R a = R a

data Found = Found
-- | If you see type check errors with this term in them, then GHC could not find the field you're attempting to access in the schema
data ErrorNoFieldNamed name
-- | If you see type check errors with this term, then GHC found multiple fields with the same name in the schema
data ErrorMultipleFieldsNamed name

type NotFound name = ErrorNoFieldNamed name
type Duplicates name = ErrorMultipleFieldsNamed name

instance Default a => Default (L a) where
    def = L def
instance Default a => Default (R a) where
    def = R def
instance Default Found where
    def = Found
instance (Default a, Default b) => Default (a :|: b) where
    def = def :|: def

type family CheckNamesEqual actual expected where
    CheckNamesEqual name name = Found
    CheckNamesEqual a expected = ErrorNoFieldNamed expected

type family Find a b name :: * where
    Find (NotFound a) (NotFound b) name = ErrorNoFieldNamed name
    Find (NotFound a) Found name = Found
    Find Found (NotFound b) name = Found
    Find Found Found name = ErrorMultipleFieldsNamed name

type family HasFieldCheck schema name where
    HasFieldCheck (a :|: b) name = Find (HasFieldCheck a name) (HasFieldCheck b name) name
    HasFieldCheck a name = CheckNamesEqual (NameFor a) name

type family LocateIf leftYes rightYes left right name where
    LocateIf (Duplicates x) r a b name = ErrorMultipleFieldsNamed x
    LocateIf l (Duplicates x) a b name = ErrorMultipleFieldsNamed x

    LocateIf Found Found a b name = ErrorMultipleFieldsNamed name
    LocateIf (NotFound x) (NotFound y) a b name = ErrorNoFieldNamed name
    LocateIf Found (NotFound x) a b name = L (Locate a name)
    LocateIf (NotFound x) Found a b name = R (Locate b name)

type family Locate schema name where
    Locate (a :|: b) name = LocateIf (HasFieldCheck a name) (HasFieldCheck b name) a b name
    Locate a name = CheckNamesEqual (NameFor a) name

type family LocateResultField schema locator where
    LocateResultField (a :|: b) (L la) = LocateResultField a la
    LocateResultField (a :|: b) (R lb) = LocateResultField b lb
    LocateResultField a Found = a

class Locator schema l where
    type LocateResult schema l :: *

    locate :: schema -> l -> LocateResult schema l

instance Locator a la => Locator (a :|: b) (L la) where
    type LocateResult (a :|: b) (L la) = LocateResult a la

    locate (a :|: b) locator = locate a (subLocator locator)
        where subLocator :: L la -> la
              subLocator _ = undefined

instance Locator b lb => Locator (a :|: b) (R lb) where
    type LocateResult (a :|: b) (R lb) = LocateResult b lb

    locate (a :|: b) locator = locate b (subLocator locator)
        where subLocator :: R lb -> lb
              subLocator _ = undefined

-- * Fields

-- | Type family for all field types that can be named
type family NameFor t :: *

class (FieldSchema (FieldInTable table f), Typeable f, Table table) => Field (table :: *) f where
    type FieldInTable table f :: *
    type FieldInTable table f = LocateResultField (Schema table) (Locate (Schema table) f)

    fieldSettings :: Proxy table -> Proxy f -> FieldSettings (FieldInTable table f)
    fieldSettings table field = defSettings (fieldSchema table field)
        where fieldSchema :: Proxy table -> Proxy f -> Proxy (FieldInTable table f)
              fieldSchema _ _ = Proxy

    fieldConstraints :: Proxy table -> Proxy f -> [SQLConstraint]
    fieldConstraints _ _ = []

    fieldColDesc :: Proxy table -> Proxy f -> SQLColumnSchema
    fieldColDesc table field = let base = colDescFromSettings (fieldSettings table field)
                               in base { csConstraints = csConstraints base ++ fieldConstraints table field }

    fieldName :: Proxy table -> Proxy f -> Text
    default fieldName :: ( Generic f, Constructor c
                         , GOneConstructor (Rep f ()) ~ M1 Generic.C c a ()) => Proxy table -> Proxy f -> Text
    fieldName _ field = fromString (conName (onlyConstructor field))
        where onlyConstructor :: Proxy f -> GOneConstructor (Rep f ())
              onlyConstructor _ = undefined

    fieldNameD :: Proxy table -> Proxy f -> f
    default fieldNameD :: (Generic f, Rep f ~ D1 x (C1 x1 U1)) => Proxy table -> Proxy f -> f
    fieldNameD _ _ = to (M1 (M1 U1))

class ( Show (FieldSettings fs), Typeable (FieldType fs)
      , Show (FieldType fs) )  => FieldSchema fs where
    data FieldSettings fs :: *
    type FieldType fs :: *

    defSettings :: Proxy fs -> FieldSettings fs

    colDescFromSettings :: FieldSettings fs -> SQLColumnSchema

    makeSqlValue :: fs -> SqlValue
--    fromValue :: FieldType fs -> fs

    -- mapSchema :: ( Locate schema (NameFor fs) ~ locator
    --              , Locator schema locator
    --              , LocateResult schema locator ~ r ) =>
    --              (r -> a) -> schema -> fs -> a
    -- mapSchema f schema (fs :: fs) = f (getField' schema (undefined :: NameFor fs))

-- ** Relationship fields

newtype DefaultPKField table name = DefaultPKField (DefaultPrimaryKeyFor table)
    deriving Show
type instance NameFor (DefaultPKField table name) = name

instance Locator (DefaultPKField table name) Found where
    type LocateResult (DefaultPKField table name) Found = LocateResult (DefaultPrimaryKeyFor table) Found

    locate (DefaultPKField x) l = locate x l

instance FieldSchema (DefaultPKField table name) where
    data FieldSettings (DefaultPKField table name) = DefaultPKFieldSettings (FieldSettings (DefaultPrimaryKeyFor table))
                                                   deriving Show
    type FieldType (DefaultPKField table name) = FieldType (DefaultPrimaryKeyFor table)

    defSettings _ = DefaultPKFieldSettings (defSettings Proxy)

    colDescFromSettings (DefaultPKFieldSettings s) = colDescFromSettings s
    makeSqlValue (DefaultPKField x) = makeSqlValue x

instance SchemaPart (DefaultPKField table name)
instance FromSqlValues (DefaultPKField table name) where
    fromSqlValues' = DefaultPKField <$> fromSqlValues'

-- ** Maybe field

data MaybeField field = MaybeField (Maybe field)
                        deriving Show
type instance NameFor (MaybeField field) = NameFor field

instance Locator field Found => Locator (MaybeField field) Found where
    type LocateResult (MaybeField field) Found = Maybe (LocateResult field Found)

    locate (MaybeField Nothing) _ = Nothing
    locate (MaybeField (Just field)) r = Just (locate field r)

instance (FieldSchema field, Show (FieldSettings field)) => FieldSchema (MaybeField field) where
    data FieldSettings (MaybeField field) = WrapMaybeField (FieldSettings field)
    type FieldType (MaybeField field) = Maybe (FieldType field)

    defSettings (_ :: Proxy (MaybeField field)) = WrapMaybeField (defSettings (Proxy :: Proxy field))

    colDescFromSettings (WrapMaybeField settings) = let SQLColumnSchema desc constraints = colDescFromSettings settings
                                                    in SQLColumnSchema desc (filter (/=SQLNotNull) constraints)

    makeSqlValue (MaybeField Nothing) = SqlNull
    makeSqlValue (MaybeField (Just field)) = makeSqlValue field

deriving instance Show (FieldSettings field) => Show (FieldSettings (MaybeField field))

instance SchemaPart (MaybeField field)
instance FromSqlValues field => FromSqlValues (MaybeField field) where
    fromSqlValues' = do val <- peekSqlValue
                        case val of
                          SqlNull -> popSqlValue >> return (MaybeField Nothing)
                          val -> fromSqlValues'

-- ** Text fields

data TextField name = TextField Text
                    deriving Show
type instance NameFor (TextField name) = name

instance Locator (TextField name) Found where
    type LocateResult (TextField name) Found = Text

    locate (TextField string) _ = string

data CharOrVarchar = Char (Maybe Int)
                   | Varchar (Maybe Int)
                     deriving Show

instance FieldSchema (TextField name) where
    data FieldSettings (TextField name) = TextFieldSettings
                                        { charOrVarChar :: CharOrVarchar }
                                        deriving Show
    type FieldType (TextField name) = Text

    defSettings _ = TextFieldSettings (Varchar Nothing)

    colDescFromSettings (TextFieldSettings (Char n)) = notNull $
                                                       SqlColDesc
                                                       { colType = SqlCharT
                                                       , colSize = n
                                                       , colOctetLength = Nothing
                                                       , colDecDigits = Nothing
                                                       , colNullable = Nothing }
    colDescFromSettings (TextFieldSettings (Varchar n)) = notNull $
                                                          SqlColDesc
                                                          { colType = SqlVarCharT
                                                          , colSize = n
                                                          , colOctetLength = Nothing
                                                          , colDecDigits = Nothing
                                                          , colNullable = Nothing }

    makeSqlValue (TextField x) = SqlString (unpack x)

instance SchemaPart (TextField name)
instance FromSqlValues (TextField name) where
    fromSqlValues' = TextField <$> (fromSql <$> popSqlValue)

-- ** Int fields

data IntField name = IntField Int
                   deriving Show
type instance NameFor (IntField name) = name

instance Locator (IntField name) Found where
    type LocateResult (IntField name) Found = Int

    locate (IntField x) _ = x

instance FieldSchema (IntField name) where
    data FieldSettings (IntField name) = IntFieldDefault
                                         deriving Show
    type FieldType (IntField name) = Int

    defSettings _ = IntFieldDefault
    colDescFromSettings _ = notNull $
                            SqlColDesc
                            { colType = SqlNumericT
                            , colSize = Nothing
                            , colOctetLength = Nothing
                            , colDecDigits = Nothing
                            , colNullable = Nothing }
    makeSqlValue (IntField i) = SqlInteger (fromIntegral i)
instance SchemaPart (IntField name)
instance FromSqlValues (IntField name) where
    fromSqlValues' = IntField <$> (fromSql <$> popSqlValue)

-- ** Date time fields

data DateTimeField name = DateTimeField UTCTime
                          deriving Show
type instance NameFor (DateTimeField name) = name

instance Locator (DateTimeField name) Found where
    type LocateResult (DateTimeField name) Found = UTCTime

    locate (DateTimeField dateTime) _ = dateTime

instance FieldSchema (DateTimeField name) where
    data FieldSettings (DateTimeField name) = DateTimeDefault
                                              deriving Show
    type FieldType (DateTimeField name) = UTCTime

    defSettings _ = DateTimeDefault
    colDescFromSettings _ = notNull $
                            SqlColDesc
                            { colType = SqlUTCDateTimeT
                            , colSize = Nothing
                            , colOctetLength = Nothing
                            , colDecDigits = Nothing
                            , colNullable = Nothing }
    makeSqlValue (DateTimeField d) = SqlUTCTime d
instance SchemaPart (DateTimeField name)
instance FromSqlValues (DateTimeField name) where
    fromSqlValues' = DateTimeField <$> (fromSql <$> popSqlValue)

getLocator :: Locate schema name ~ locator => schema -> name -> locator
getLocator _ _ = undefined

getLocatorMultiple :: LocateAll schema names ~ locator => schema -> names -> locator
getLocatorMultiple _ _ = undefined

getField :: ( Table table
            , Locate (Schema table) name ~ locator
            , Locator (Schema table) locator) => table -> name -> LocateResult (Schema table) locator
getField table name = locate schema (getLocator schema name)
    where schema = getSchema table

getField' :: (Locate schema name ~ locator
             , Locator schema locator) => schema -> name -> LocateResult schema locator
getField' schema name = locate schema (getLocator schema name)

getFields :: ( LocateAll schema name ~ locator
             , Locator schema locator) => schema -> name -> LocateResult schema locator
getFields schema names = locate schema (getLocatorMultiple schema names)