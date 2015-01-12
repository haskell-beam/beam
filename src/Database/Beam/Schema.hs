{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances, MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs #-}
-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema where

import Database.Beam.SQL.Types

import Data.Time.Clock
import Data.Text (Text)
import Data.Proxy
import Data.String
import Data.Typeable

import Database.HDBC (SqlColDesc(..), SqlTypeId(..))

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

-- * Database types

-- | Table composition operator
data t1 :#: t2
data TableSchema t

type DatabaseSchema = [(Text, ReifiedTableSchema)]
type ReifiedTableSchema = [(Text, SqlColDesc)]

data GenTable where
    GenTable :: Table t => Proxy t -> GenTable

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
data TableId = TableId deriving Show

data GenField t f

class Typeable table => Table table where
    type Schema table :: *
    type Schema table = SchemaForGeneric (Rep table ())

    type PrimaryKeyField table :: *
    type PrimaryKeyField table = IntField TableId

    dbTableName :: Proxy table -> Text
    default dbTableName :: ( Generic table, Constructor c
                           , GOneConstructor (Rep table ()) ~ (t :: * -> (* -> *) -> * -> *) c f a) => Proxy table -> Text
    dbTableName tbl = fromString (conName (onlyConstructor tbl))
        where onlyConstructor :: Proxy table -> GOneConstructor (Rep table ())
              onlyConstructor _ = undefined

    reifyTableSchema :: Proxy table -> ReifiedTableSchema
    default reifyTableSchema :: ( Generic table
                                , GReifySchema (WrapFields (GenField table) (SchemaForGeneric (Rep table ()))) ) => Proxy table -> ReifiedTableSchema
    reifyTableSchema table = reifyGenericSchema (schemaProxy table)
        where schemaProxy :: Proxy table -> Proxy (WrapFields (GenField table) (SchemaForGeneric (Rep table ())))
              schemaProxy _ = Proxy

    getSchema :: table -> Schema table
    default getSchema :: (Generic table, GHasSchema (Rep table ())
                         , Schema table ~ SchemaForGeneric (Rep table ())) => table -> Schema table
    getSchema table = gGetSchema (from' table)
        where from' :: Generic table => table -> Rep table ()
              from' = from

    makeSqlValues :: table -> [SQLValue]
    default makeSqlValues :: (Generic table, GMakeSqlValues (Rep table)) => table -> [SQLValue]
    makeSqlValues table = gMakeSqlValues (from' table)
        where from' :: Generic table => table -> Rep table a
              from' = from

-- | Schema combinator. Takes two fields or schemas and combines them into a new schema
data a :|: b = a :|: b
               deriving Show

-- ** Generic Table deriving support

-- | The generic table support derives Table instances for datatypes with just one constructor
class GHasSchema x where
    gGetSchema :: x -> SchemaForGeneric x
instance GHasSchema (p x) => GHasSchema (M1 D f p x) where
    gGetSchema (M1 x) = gGetSchema x
instance GHasSchema (p x) => GHasSchema (M1 C f p x) where
    gGetSchema (M1 x) = gGetSchema x
instance GHasSchema (p x) => GHasSchema (M1 S f p x) where
    gGetSchema (M1 x) = gGetSchema x
instance (GHasSchema (f x), GHasSchema (g x)) => GHasSchema ((:*:) f g x) where
    gGetSchema (f :*: g) = gGetSchema f :|: gGetSchema g
instance GHasSchema (K1 Generic.R schema x) where
    gGetSchema (K1 x) = x

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
    SchemaForGeneric (M1 D f p x) = SchemaForGeneric (p x)
    SchemaForGeneric (M1 C f p x) = SchemaForGeneric (p x)
    SchemaForGeneric (M1 S f p x) = SchemaForGeneric (p x)
    SchemaForGeneric ((:*:) f g x) = SchemaForGeneric (f x) :|: SchemaForGeneric (g x)
    SchemaForGeneric (K1 Generic.R s x) = s

class GMakeSqlValues (x :: * -> *) where
    gMakeSqlValues :: x a -> [SQLValue]
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

type family WrapFields f schema where
    WrapFields f (a :|: b) = WrapFields f a :|: WrapFields f b
    WrapFields f a = f a

-- * Field locating support

-- | The L and R datatypes help the system navigate through the nested `:|:` table constructors
data L a = L a
data R a = R a

data Found
-- | If you see type check errors with this term in them, then GHC could not find the field you're attempting to access in the schema
data ErrorNoFieldNamed name
-- | If you see type check errors with this term, then GHC found multiple fields with the same name in the schema
data ErrorMultipleFieldsNamed name

type NotFound name = ErrorNoFieldNamed name
type Duplicates name = ErrorMultipleFieldsNamed name

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

    fieldColDesc :: Proxy table -> Proxy f -> SqlColDesc
    fieldColDesc table field = colDescFromSettings (fieldSettings table field)

    fieldName :: Proxy table -> Proxy f -> Text
    default fieldName :: ( Generic f, Constructor c
                         , GOneConstructor (Rep f ()) ~ M1 Generic.C c a ()) => Proxy table -> Proxy f -> Text
    fieldName _ field = fromString (conName (onlyConstructor field))
        where onlyConstructor :: Proxy f -> GOneConstructor (Rep f ())
              onlyConstructor _ = undefined

class Show (FieldSettings fs) => FieldSchema fs where
    data FieldSettings fs :: *

    defSettings :: Proxy fs -> FieldSettings fs

    colDescFromSettings :: FieldSettings fs -> SqlColDesc

    makeSqlValue :: fs -> SQLValue

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

    defSettings _ = TextFieldSettings (Varchar Nothing)

    colDescFromSettings (TextFieldSettings (Char n)) = SqlColDesc
                                                       { colType = SqlCharT
                                                       , colSize = n
                                                       , colOctetLength = Nothing
                                                       , colDecDigits = Nothing
                                                       , colNullable = Nothing }
    colDescFromSettings (TextFieldSettings (Varchar n)) = SqlColDesc
                                                       { colType = SqlVarCharT
                                                       , colSize = n
                                                       , colOctetLength = Nothing
                                                       , colDecDigits = Nothing
                                                       , colNullable = Nothing }

    makeSqlValue (TextField x) = SQLText x

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

    defSettings _ = IntFieldDefault
    colDescFromSettings _ = SqlColDesc
                            { colType = SqlNumericT
                            , colSize = Nothing
                            , colOctetLength = Nothing
                            , colDecDigits = Nothing
                            , colNullable = Nothing }
    makeSqlValue (IntField i) = SQLInt (fromIntegral i)

-- ** Date time fields

data DateTimeField name = DateTimeField UTCTime
type instance NameFor (DateTimeField name) = name

instance Locator (DateTimeField name) Found where
    type LocateResult (DateTimeField name) Found = UTCTime

    locate (DateTimeField dateTime) _ = dateTime

instance FieldSchema (DateTimeField name) where
    data FieldSettings (DateTimeField name) = DateTimeDefault
                                              deriving Show

    defSettings _ = DateTimeDefault
    colDescFromSettings _ = SqlColDesc
                            { colType = SqlUTCDateTimeT
                            , colSize = Nothing
                            , colOctetLength = Nothing
                            , colDecDigits = Nothing
                            , colNullable = Nothing }
    makeSqlValue (DateTimeField d) = undefined

getLocator :: Locate schema name ~ locator => schema -> name -> locator
getLocator _ _ = undefined

getField :: ( Table table
            , Locate (Schema table) name ~ locator
            , Locator (Schema table) locator) => table -> name -> LocateResult (Schema table) locator
getField table name = locate schema (getLocator schema name)
    where schema = getSchema table

getField' :: (Locate schema name ~ locator
            , Locator schema locator) => schema -> name -> LocateResult schema locator
getField' schema name = locate schema (getLocator schema name)