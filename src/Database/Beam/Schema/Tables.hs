{-# LANGUAGE TypeFamilies, TypeOperators,  MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances, RankNTypes #-}
-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Tables where

import Database.Beam.SQL.Types
import Database.Beam.Schema.Locate

import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import Data.Proxy
import Data.Typeable
import Data.Text (Text)
import Data.String
import qualified Data.Text as T

import Database.HDBC ( SqlColDesc(..), SqlValue(..), SqlTypeId(..)
                     , fromSql)

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

-- * Database types

-- | Table composition operator
-- data t1 :#: t2
data GenTable where
    GenTable :: Table t => Proxy t -> GenTable

type ReifiedDatabaseSchema = [(Text, ReifiedTableSchema)]
type ReifiedTableSchema = [(Text, SQLColumnSchema)]

newtype Database = Database [GenTable]

database_ :: [GenTable] -> Database
database_ = Database

table_ :: Table t => t -> GenTable
table_ t = GenTable (proxyFor t)
    where proxyFor :: t -> Proxy t
          proxyFor _ = Proxy

schema_ :: Table t => t
schema_ = undefined

reifyDBSchema :: Database -> ReifiedDatabaseSchema
reifyDBSchema (Database tables) = map schemaForTable tables
    where schemaForTable :: GenTable -> (Text, ReifiedTableSchema)
          schemaForTable (GenTable t) = (dbTableName t, reifyTableSchema t)

tables :: Database -> [GenTable]
tables (Database x) = x

data AnyField table where
    AnyField :: (Table table, Field table name, FieldSchema t) => table -> Column name t -> AnyField table

-- * Tables

type family TypeOf x :: *

type FullSchema table = PhantomFieldSchema table :|: Schema table

type DefaultPrimaryKeyFor table = Column TableId Int

data TableId = TableId deriving (Show, Generic, Typeable)
instance (Table table) => Field table TableId where
    type FieldInTable table TableId = Column TableId Int

    fieldName _ _ = "id"

    fieldConstraints _ _ = [SQLPrimaryKeyAutoIncrement]

class ( Typeable table
      , Locator (FullSchema table) (LocateAll (FullSchema table) (PrimaryKey table)) )  => Table table where
    type Schema table :: *
    type Schema table = SchemaForGeneric (Rep table)
    -- type ConcreteSchema table :: *
    -- type ConcreteSchema table = ConcreteSchemaForGeneric (Rep table)

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
                                , GReifySchema table (PhantomFieldSchema table :|: SchemaForGeneric (Rep table))) => Proxy table -> ReifiedTableSchema
    reifyTableSchema table = reifyGenericSchema table (schemaProxy table)
        where schemaProxy :: Proxy table -> Proxy (PhantomFieldSchema table :|: SchemaForGeneric (Rep table))
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
    default makeSqlValues :: (Generic table, GMakeSqlValues (Rep table ())) => table -> [SqlValue]
    makeSqlValues table = makePhantomValues table ++ gMakeSqlValues (from' table)
        where from' :: Generic table => table -> Rep table ()
              from' = from

    fieldValues :: table -> [(AnyField table, SqlValue)]
    default fieldValues :: (Generic table, GFieldValues table (Rep table ())) => table -> [(AnyField table, SqlValue)]
    fieldValues tbl = gFieldValues tbl (from' tbl)
        where from' :: Generic table => table -> Rep table ()
              from' = from

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
instance GHasSchema (K1 Generic.R (Column name t)) where
    gGetSchema (K1 x) = x
    gFromSchema x = K1 x
instance Show (PrimaryKeySchema table) => GHasSchema (K1 Generic.R (ForeignKey table name)) where
    gGetSchema (K1 x) = x -- Embedded schema
    gFromSchema x = K1 x -- (ForeignKey x)

class GReifySchema t genSchema where
    reifyGenericSchema :: Proxy t -> Proxy genSchema -> ReifiedTableSchema

instance (GReifySchema t a, GReifySchema t b) => GReifySchema t (a :|: b) where
    reifyGenericSchema tProxy schema = reifyGenericSchema tProxy aProxy ++ reifyGenericSchema tProxy bProxy
        where (aProxy, bProxy) = proxiesFor schema
              proxiesFor :: Proxy (a :|: b) -> (Proxy a, Proxy b)
              proxiesFor _ = (Proxy, Proxy)
instance Field t name => GReifySchema t (Column name ty) where
    reifyGenericSchema table schema = [(fieldName table field, fieldColDesc table field)]
        where field = fieldProxyFor schema
              fieldProxyFor :: Proxy (Column name ty) -> Proxy name
              fieldProxyFor _ = Proxy
instance ( Reference t name
         , GReifySchema relatedTbl (PrimaryKeySchema relatedTbl)) =>
    GReifySchema t (ForeignKey relatedTbl name) where
    reifyGenericSchema (table :: Proxy table) (Proxy :: Proxy (ForeignKey relatedTbl name)) =
        let subSchema = reifyGenericSchema (Proxy :: Proxy relatedTbl) (Proxy :: Proxy (PrimaryKeySchema relatedTbl))
            embedSchema prefix = map ((T.append prefix) *** removeConstraints)

            removeConstraints (SQLColumnSchema desc cs) = SQLColumnSchema desc []
        in embedSchema (T.concat [ refPrefix (Proxy :: Proxy table) (Proxy :: Proxy name), "_" ]) subSchema

type family SchemaForGeneric g where
    SchemaForGeneric (M1 D f p) = SchemaForGeneric p
    SchemaForGeneric (M1 C f p) = SchemaForGeneric p
    SchemaForGeneric (M1 S f p) = SchemaForGeneric p
    SchemaForGeneric (f :*: g) = SchemaForGeneric f :|: SchemaForGeneric g
    SchemaForGeneric (K1 Generic.R (Column name t)) = Column name t
    SchemaForGeneric (K1 Generic.R (ForeignKey table name)) = ForeignKey table name -- Embedded name (PrimaryKeySchema table)
-- type family ConcreteSchemaForGeneric g where
--     ConcreteSchemaForGeneric (M1 D f p) = ConcreteSchemaForGeneric p
--     ConcreteSchemaForGeneric (M1 C f p) = ConcreteSchemaForGeneric p
--     ConcreteSchemaForGeneric (M1 S f p) = ConcreteSchemaForGeneric p
--     ConcreteSchemaForGeneric (f :*: g) = ConcreteSchemaForGeneric f :|: ConcreteSchemaForGeneric g
--     ConcreteSchemaForGeneric (K1 Generic.R (Column name t)) = Column name t
--     ConcreteSchemaForGeneric (K1 Generic.R (ForeignKey table name)) = ForeignKey table name

class GMakeSqlValues x where
    gMakeSqlValues :: x -> [SqlValue]
instance GMakeSqlValues (p a) => GMakeSqlValues (D1 f p a) where
    gMakeSqlValues (M1 x) = gMakeSqlValues x
instance GMakeSqlValues (p a) => GMakeSqlValues (C1 f p a) where
    gMakeSqlValues (M1 x) = gMakeSqlValues x
instance GMakeSqlValues (p a) => GMakeSqlValues (S1 f p a) where
    gMakeSqlValues (M1 x) = gMakeSqlValues x
instance (GMakeSqlValues (f a), GMakeSqlValues (g a)) => GMakeSqlValues ((f :*: g) a) where
    gMakeSqlValues (f :*: g) = gMakeSqlValues f ++ gMakeSqlValues g
instance (GMakeSqlValues f, GMakeSqlValues g) => GMakeSqlValues (f :|: g) where
    gMakeSqlValues (f :|: g) = gMakeSqlValues f ++ gMakeSqlValues g
instance GMakeSqlValues (U1 a) where
    gMakeSqlValues _ = []
instance GMakeSqlValues x => GMakeSqlValues (K1 Generic.R x a) where
    gMakeSqlValues (K1 x) = gMakeSqlValues x
instance FieldSchema t => GMakeSqlValues (Column n t) where
    gMakeSqlValues (Column x) = [makeSqlValue x]
instance GMakeSqlValues (PrimaryKeySchema table) =>
    GMakeSqlValues (ForeignKey table name) where
    gMakeSqlValues (ForeignKey schema) = gMakeSqlValues schema

class GFieldValues tbl (x :: *) where
    gFieldValues :: tbl -> x -> [(AnyField tbl, SqlValue)]
instance GFieldValues tbl (p a) => GFieldValues tbl (D1 f p a) where
    gFieldValues t (M1 x) = gFieldValues t x
instance GFieldValues tbl (p a) => GFieldValues tbl (C1 f p a) where
    gFieldValues t (M1 x) = gFieldValues t x
instance GFieldValues tbl (p a) => GFieldValues tbl (S1 f p a) where
    gFieldValues t (M1 x) = gFieldValues t x
instance (GFieldValues tbl (f a), GFieldValues tbl (g a)) => GFieldValues tbl ((f :*: g) a) where
    gFieldValues t (f :*: g) = gFieldValues t f ++ gFieldValues t g
instance (GFieldValues tbl f, GFieldValues tbl g) => GFieldValues tbl (f :|: g) where
    gFieldValues t (f :|: g) = gFieldValues t f ++ gFieldValues t g
instance GFieldValues tbl (U1 a) where
    gFieldValues _ _ = []
instance GFieldValues tbl x => GFieldValues tbl (K1 Generic.R x a) where
    gFieldValues t (K1 x) = gFieldValues t x
instance (Field t name, FieldSchema x, Table t) => GFieldValues t (Column name x) where
    gFieldValues tbl field@(Column x) = [( AnyField tbl field
                                         , makeSqlValue x )]
instance GFieldValues t (PrimaryKeySchema table) => GFieldValues t (ForeignKey table name) where
    gFieldValues tbl (ForeignKey schema) = gFieldValues tbl schema

type PrimaryKeySchema table = LocateResult (FullSchema table) (LocateAll (FullSchema table) (PrimaryKey table))

-- * Primary keys

primaryKeyForTable' :: Table table =>
                       PhantomFieldSchema table -> table -> PrimaryKeySchema table
primaryKeyForTable' phantomData (tbl :: table) = getFields (phantomData :|: getSchema tbl) (undefined :: PrimaryKey table)

-- * Field and table support

class (FieldSchema (TypeOf (FieldInTable table f)),
       Typeable f, Table table) => Field (table :: *) f where
    type FieldInTable table f :: *
    type FieldInTable table f = LocateResult (Schema table) (Locate (Schema table) f)

    fieldSettings :: Proxy table -> Proxy f -> FieldSettings (TypeOf (FieldInTable table f))
    fieldSettings table field = defSettings --  (fieldSchema table field)
        -- where fieldSchema :: Proxy table -> Proxy f -> Proxy (FieldInTable table f)
        --       fieldSchema _ _ = Proxy

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

class ( Show (FieldSettings fs), Typeable fs
      , Show fs )  => FieldSchema fs where
    data FieldSettings fs :: *

    defSettings :: FieldSettings fs

    colDescFromSettings :: FieldSettings fs -> SQLColumnSchema

    makeSqlValue :: fs -> SqlValue
    fromSqlValue :: FromSqlValuesM fs

class Reference table name where
    refPrefix :: Proxy table -> Proxy name -> Text
    default refPrefix :: ( Generic name, Constructor c
                         , GOneConstructor (Rep name ()) ~ M1 Generic.C c a () ) =>
                           Proxy table -> Proxy name -> Text
    refPrefix _ name = fromString (conName (onlyConstructor name))
        where onlyConstructor :: Proxy name -> GOneConstructor (Rep name ())
              onlyConstructor _ = undefined

type ReferencedTable table name = TableFor (LocateResult (Schema table) (Locate (Schema table) name))

instance ( Table table, Typeable name
         , FieldSchema (TypeOf (FieldInTable table (name :-> subField)))
         , Field (ReferencedTable table name) subField

         , Reference table name) => Field table (name :-> subField) where

    fieldSettings (table :: Proxy table) (_ :: Proxy (name :-> subField)) =
        undefined
    fieldConstraints _ _ = []
    fieldColDesc _ _ = undefined
    fieldName (table :: Proxy table) (_ :: Proxy (name :-> subField)) =
        T.concat [ refPrefix table (Proxy :: Proxy name), "_"
                 , fieldName (Proxy :: Proxy (ReferencedTable table name)) (Proxy :: Proxy subField) ]
    fieldNameD _ _ = undefined

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

-- class Show (ValueSettings a) => SQLValable a where
--     data ValueSettings a :: *

--     defValSettings :: ValueSettings a
--     colDescFromValSettings :: ValueSettings a -> SQLColumnSchema
--     fromSqlValues'' :: FromSqlValuesM a
--     makeSqlValue' :: a -> SqlValue

data ForeignKey table name where
    ForeignKey :: Show (PrimaryKeySchema table) => PrimaryKeySchema table -> ForeignKey table name
type instance NameFor (ForeignKey table name) = name
type instance EmbeddedSchemaFor (ForeignKey table name) = Embedded name (PrimaryKeySchema table)
type instance Rename newName (ForeignKey table name) = ForeignKey table newName
type family TableFor a where
    TableFor (ForeignKey table name) = table
instance Show (ForeignKey table name) where
    show (ForeignKey schema) = concat ["(ForeignKey (", show schema, "))"]

newtype Column name t = Column t
    deriving Show
type instance EmbeddedSchemaFor (Column name t) = Empty
type instance NameFor (Column name t) = name
type instance Rename newName (Column name t) = Column newName t
type instance TypeOf (Column name t) = t

column :: t -> Column name t
column = Column
columnValue :: Column name t -> t
columnValue (Column x) = x

instance FieldSchema t => FromSqlValues (Column name t) where
    fromSqlValues' = Column <$> fromSqlValue

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

getField :: ( Table table
            , Locate (Schema table) name ~ locator
            , Locator (Schema table) locator) => table -> name -> LocateResult (Schema table) locator
getField table name = locate schema (getLocator schema name)
    where schema = getSchema table