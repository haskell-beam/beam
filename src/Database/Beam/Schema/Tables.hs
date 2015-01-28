{-# LANGUAGE TypeFamilies, TypeOperators,  MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances, RankNTypes #-}
-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Tables where

import Database.Beam.SQL.Types
import Database.Beam.Schema.Locate

import Data.Proxy
import Data.Typeable
import Data.Text (Text)
import Data.String

import Database.HDBC ( SqlColDesc(..), SqlValue(..), SqlTypeId(..)
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
data AnyField table where
    AnyField :: (Table table, Field table (NameFor fs), FieldSchema fs) => table -> fs -> AnyField table

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

type FullSchema table = PhantomFieldSchema table :|: Schema table

type DefaultPrimaryKeyFor table = IntField TableId

data TableId = TableId deriving (Show, Generic, Typeable)
instance (Table table) => Field table TableId where
    type FieldInTable table TableId = IntField table

    fieldName _ _ = "id"

    fieldConstraints _ _ = [SQLPrimaryKey]

class ( Typeable table
      , Locator (FullSchema table) (LocateAll (FullSchema table) (PrimaryKey table)) )  => Table table where
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
    makeSqlValues table = makePhantomValues table ++ gMakeSqlValues (from' table)
        where from' :: Generic table => table -> Rep table a
              from' = from

    fieldValues :: table -> [(AnyField table, SqlValue)]
    default fieldValues :: (Generic table, GFieldValues table (Rep table)) => table -> [(AnyField table, SqlValue)]
    fieldValues tbl = gFieldValues tbl (from tbl)

    -- | The default behavior is to assign the default phantom field schema (just an integer primary key) a single value of null, to let autoincrement work
    makePhantomValues :: table -> [SqlValue]
    makePhantomValues _ = [SqlNull]

-- ** Generic Table deriving support

type family GOneConstructor g where
    GOneConstructor (M1 D f p x) = p x

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

class GFieldValues tbl (x :: * -> *) where
    gFieldValues :: tbl -> x a -> [(AnyField tbl, SqlValue)]
instance GFieldValues tbl p => GFieldValues tbl (D1 f p) where
    gFieldValues t (M1 x) = gFieldValues t x
instance GFieldValues tbl p => GFieldValues tbl (C1 f p) where
    gFieldValues t (M1 x) = gFieldValues t x
instance GFieldValues tbl p => GFieldValues tbl (S1 f p) where
    gFieldValues t (M1 x) = gFieldValues t x
instance (GFieldValues tbl f, GFieldValues tbl g) => GFieldValues tbl (f :*: g) where
    gFieldValues t (f :*: g) = gFieldValues t f ++ gFieldValues t g
instance GFieldValues tbl U1 where
    gFieldValues _ _ = []
instance (Field t (NameFor x), FieldSchema x, Table t) => GFieldValues t (K1 Generic.R x) where
    gFieldValues tbl (K1 x) = [( AnyField tbl x
                               , makeSqlValue x )]

type PrimaryKeySchema table = LocateResult (FullSchema table) (LocateAll (FullSchema table) (PrimaryKey table))

-- * Primary keys

primaryKeyForTable :: Table table =>
                      PhantomFieldSchema table -> table -> PrimaryKeySchema table
primaryKeyForTable phantomData (tbl :: table) = getFields (phantomData :|: getSchema tbl) (undefined :: PrimaryKey table)

-- * Field and table support

data GenField t f

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

    field :: FieldType fs -> fs
    fieldValue :: fs -> FieldType fs

-- | Defined here so it can be used as the default primary key field
data IntField name = IntField Int
                   deriving Show
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

    field = IntField
    fieldValue (IntField x) = x

getField :: ( Table table
            , Locate (Schema table) name ~ locator
            , Locator (Schema table) locator) => table -> name -> LocateResult (Schema table) locator
getField table name = locate schema (getLocator schema name)
    where schema = getSchema table