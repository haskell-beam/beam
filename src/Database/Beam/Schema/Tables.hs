{-# LANGUAGE TypeFamilies, TypeOperators,  MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances, RankNTypes #-}
-- | Defines a generic schema type that can be used to define schemas for Beam tables
module Database.Beam.Schema.Tables where

import Database.Beam.SQL.Types
--import Database.Beam.Schema.Locate

import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Data.Proxy
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

-- * Database types

-- | Table composition operator
data GenTable where
    GenTable :: Table t => Proxy t -> GenTable

type ReifiedDatabaseSchema = [(Text, ReifiedTableSchema)]
type ReifiedTableSchema = [(Text, SQLColumnSchema)]

newtype Database = Database [GenTable]

database_ :: [GenTable] -> Database
database_ = Database

table_ :: Table t => t a -> GenTable
table_ t = GenTable (proxyFor t)
    where proxyFor :: t a -> Proxy t
          proxyFor _ = Proxy

schema_ :: Table t => t a
schema_ = undefined

reifyDBSchema :: Database -> ReifiedDatabaseSchema
reifyDBSchema (Database tables) = map schemaForTable tables
    where schemaForTable :: GenTable -> (Text, ReifiedTableSchema)
          schemaForTable (GenTable t) = (dbTableName t, reifyTableSchema t)

tables :: Database -> [GenTable]
tables (Database x) = x

-- | Schema combinator. Takes two fields or schemas and combines them into a new schema
data a :|: b = a :|: b
               deriving Show
infixl 3 :|:

-- * Tables

newtype PK column = PK
                  { tableId :: column Int }
                    deriving Generic

deriving instance Show (column Int) => Show (PK column)

newtype Dummy a = Dummy Void
    deriving Typeable

data TableField table ty = TableField
                         { fieldFromTable :: forall f. table f -> f ty
                         , fieldName      :: Text
                         , fieldConstraints :: [SQLConstraint]
                         , fieldSettings  :: FieldSettings ty }

instance Show (FieldSettings ty) => Show (TableField t ty) where
    show tf = concat $
              [ "TableField { fieldName = ", show (fieldName tf)
              , ", fieldConstraints = ", show (fieldConstraints tf)
              , ", fieldSettings = ", show (fieldSettings tf) ]

-- | The simple type can be used to get a version of the table without the functor constraint
type Simple table = table Identity
type TableSettings table = table (TableField table)

from' :: Generic x => x -> Rep x ()
from' = from

to' :: Generic x => Rep x () -> x
to' = to

class Typeable table  => Table (table :: (* -> *) -> *) where
    type PrimaryKey table (column :: * -> *) :: *
    type PrimaryKey table column = PK column

    type PhantomFields table (column :: (* -> *)) :: *
    type PhantomFields table column = PK column

    changeRep :: (forall a. f a -> g a) -> table f -> table g
    default changeRep :: ( Generic (table f)
                         , Generic (table g)
                         , GChangeRep (Rep (table f) ()) (Rep (table g) ()) f g ) =>
                         (forall a. f a -> g a) -> table f -> table g
    changeRep f t = to' (gChangeRep f (from' t))

    changePhantomRep :: (forall a. f a -> g a) -> Proxy table -> PhantomFields table f -> PhantomFields table g
    default changePhantomRep :: ( Generic (PhantomFields table f)
                                , Generic (PhantomFields table g)
                                , GChangeRep (Rep (PhantomFields table f) ()) (Rep (PhantomFields table g) ()) f g ) =>
                                (forall a. f a -> g a)
                             -> Proxy table
                             -> PhantomFields table f
                             -> PhantomFields table g
    changePhantomRep (f :: forall a. f a -> g a) (_ :: Proxy table) t = to' (gChangeRep f (from' t) :: Rep (PhantomFields table g) ())

    allValues :: (forall a. f a -> b) -> PhantomFields table f -> table f -> [b]
    default allValues :: ( Generic (PhantomFields table f)
                         , Generic (table f)
                         , GAllValues f (Rep (PhantomFields table f) () :|: Rep (table f) ()) ) =>
                         (forall a. f a -> b) -> PhantomFields table f -> table f -> [b]
    allValues f phantom tbl = gAllValues f (from' phantom :|: from' tbl)

    -- | Should return the primary key for the column as a field set (columns joined by :|:). By keeping this polymorphic over column,
    --   we ensure that the primary key values come directly from the table (i.e., they can't be arbitrary constants)
    primaryKey :: PhantomFields table column -> table column -> PrimaryKey table column
    default primaryKey :: ( PrimaryKey table column ~ PhantomFields table column) =>
                          PhantomFields table column -> table column -> PrimaryKey table column
    primaryKey pk _ = pk

    tblFieldSettings :: TableSettings table
    default tblFieldSettings :: ( Generic (TableSettings table)
                                , GDefaultTableFieldSettings (Rep (TableSettings table) ())) => TableSettings table
    tblFieldSettings = defTblFieldSettings

    phantomFieldSettings :: Proxy table -> PhantomFields table (TableField table)
    default phantomFieldSettings :: ( PhantomFields table (TableField table) ~ PK (TableField table) ) =>
                                    Proxy table -> PhantomFields table (TableField table)
    phantomFieldSettings _ = PK ( (defFieldSettings "id")
                                  { fieldConstraints = [SQLPrimaryKeyAutoIncrement] })

    dbTableName :: Proxy table -> Text
    dbTableName (_ :: Proxy table) = T.pack (tyConName (typeRepTyCon (typeOf (undefined :: table Dummy))))

    reifyTableSchema :: Proxy table -> ReifiedTableSchema
    default reifyTableSchema :: ( GReifySchema (Rep (PhantomFields table (TableField table)) () :|: Rep (TableSettings table) ())
                                , Generic (PhantomFields table (TableField table))
                                , Generic (TableSettings table) ) =>
                                Proxy table -> ReifiedTableSchema
    reifyTableSchema (table :: Proxy table) = gReifySchema (from' (phantomFieldSettings table) :|: from' (tblFieldSettings :: TableSettings table))

    makeSqlValues :: table Identity -> [SqlValue]
    default makeSqlValues :: (Generic (table Identity), GMakeSqlValues (Rep (table Identity) ())) => table Identity -> [SqlValue]
    makeSqlValues table = makePhantomValues table ++ gMakeSqlValues (from' table)

    -- | The default behavior is to assign the default phantom field schema (just an integer primary key) a single value of null, to let autoincrement work
    makePhantomValues :: table Identity -> [SqlValue]
    makePhantomValues _ = [SqlNull]

    phantomFromSqlValues :: Proxy table -> FromSqlValuesM (PhantomFields table Identity)
    default phantomFromSqlValues :: ( Generic (PhantomFields table Identity)
                                    , GFromSqlValues (Rep (PhantomFields table Identity)) ) =>
                                    Proxy table -> FromSqlValuesM (PhantomFields table Identity)
    phantomFromSqlValues (_ :: Proxy table) = to <$> (gFromSqlValues :: FromSqlValuesM (Rep (PhantomFields table Identity) ()))

    tableFromSqlValues :: FromSqlValuesM (table Identity)
    default tableFromSqlValues :: ( Generic (table Identity)
                                  , GFromSqlValues (Rep (table Identity)) ) =>
                                  FromSqlValuesM (table Identity)
    tableFromSqlValues = to <$> gFromSqlValues

    phantomValuesNeeded :: Proxy table -> Int
    default phantomValuesNeeded :: ( GReifySchema (Rep (PhantomFields table (TableField table)) ())
                                   , Generic (PhantomFields table (TableField table)) ) =>
                                   Proxy table -> Int
    phantomValuesNeeded table = length (gReifySchema (from' (phantomFieldSettings table)))

    tableValuesNeeded :: Proxy table -> Int
    default tableValuesNeeded :: ( GReifySchema (Rep (TableSettings table) ())
                                 , Generic (TableSettings table) ) =>
                                 Proxy table -> Int
    tableValuesNeeded (table :: Proxy table) = length (gReifySchema (from' (tblFieldSettings :: TableSettings table)))

-- -- | This instance is useful for nullable foreign keys and for reading tables from outer joins
-- instance ( Locator fullSchema (LocateAll fullSchema (PrimaryKey t))
--          , fullSchema ~ (Maybe (PhantomFieldSchema t) :|: Maybe (Schema t))
--          , Table t) => Table (Maybe t) where
--     type Schema (Maybe t) = Maybe (Schema t)
--     type PhantomFieldSchema (Maybe t) = Maybe (PhantomFieldSchema t)
--     type PrimaryKey (Maybe t) = PrimaryKey t

--     dbTableName (_ :: Proxy (Maybe t)) = dbTableName (Proxy :: Proxy t)
--     reifyTableSchema (_ :: Proxy (Maybe t)) = reifyTableSchema (Proxy :: Proxy t)

--     getSchema x = getSchema <$> x
--     fromSchema x = fromSchema <$> x
--     makeSqlValues (x :: Maybe t) = maybe (makeNulls (Proxy :: Proxy (Schema t))) makeSqlValues x
--     makePhantomValues (x :: Maybe t) = maybe (makeNulls (Proxy :: Proxy (PhantomFieldSchema t))) makePhantomValues x
instance FromSqlValues t => FromSqlValues (Maybe t) where
    valuesNeeded (_ :: Proxy (Maybe t)) = valuesNeeded (Proxy :: Proxy t)
    fromSqlValues' = mfix $ \(_ :: Maybe t) ->
                     do values <- get
                        let colCount = valuesNeeded (Proxy :: Proxy t)
                            colValues = take colCount values
                        if all (==SqlNull) colValues
                        then put (drop colCount values) >> return Nothing
                        else Just <$> fromSqlValues'

-- class MakeNulls a where
--     makeNulls :: Proxy a -> [SqlValue]
-- instance (MakeNulls a, MakeNulls b) => MakeNulls (a :|: b) where
--     makeNulls (_ :: Proxy (a :|: b)) = makeNulls (Proxy :: Proxy a) ++ makeNulls (Proxy :: Proxy b)
-- instance MakeNulls (Column name t) where
--     makeNulls _ = [SqlNull]
-- instance MakeNulls (PrimaryKeySchema table) => MakeNulls (ForeignKey table name) where
--     makeNulls (_ :: Proxy (ForeignKey table name)) = makeNulls (Proxy :: Proxy (PrimaryKeySchema table))
-- instance MakeNulls t => MakeNulls (Maybe t) where
--     makeNulls (_ :: Proxy (Maybe t)) = makeNulls (Proxy :: Proxy t)

-- ** Generic Table deriving support

type family GOneConstructor g where
    GOneConstructor (M1 D f p x) = p x

defTblFieldSettings :: ( Generic (TableSettings table)
                       ,  GDefaultTableFieldSettings (Rep (TableSettings table) ())) =>
                       TableSettings table
defTblFieldSettings = withProxy $ \proxy -> to' (gDefTblFieldSettings proxy)
    where withProxy :: (Proxy (Rep (TableSettings table) ()) -> TableSettings table) -> TableSettings table
          withProxy f = f Proxy --) withProxy f = f (Proxy :: Proxy (Rep (TableSettings table) ())))

defFieldSettings :: FieldSchema fs => Text -> TableField table fs
defFieldSettings name = TableField
                      { fieldFromTable = undefined
                      , fieldName = name
                      , fieldConstraints = []
                      , fieldSettings = settings}
    where settings = defSettings

fieldColDesc :: FieldSchema fs => FieldSettings fs -> [SQLConstraint] -> SQLColumnSchema
fieldColDesc settings cs =let base = colDescFromSettings settings
                          in base { csConstraints = csConstraints base ++ cs }

class GChangeRep x y f g where
    gChangeRep :: (forall a. f a -> g a) -> x -> y
instance GChangeRep (a p) (b p) x y => GChangeRep (D1 f a p) (D1 g b p) x y where
    gChangeRep f (M1 x) = M1 (gChangeRep f x)
instance GChangeRep (a p) (b p) x y => GChangeRep (C1 f a p) (C1 g b p) x y where
    gChangeRep f (M1 x) = M1 (gChangeRep f x)
instance GChangeRep (a p) (b p) x y => GChangeRep (S1 f a p) (S1 g b p) x y where
    gChangeRep f (M1 x) = M1 (gChangeRep f x)
instance ( GChangeRep (a1 p) (a2 p) x y, GChangeRep (b1 p) (b2 p) x y) => GChangeRep ((a1 :*: b1) p) ((a2 :*: b2) p) x y where
    gChangeRep f (a :*: b) = gChangeRep f a :*: gChangeRep f b
instance GChangeRep (K1 Generic.R (x a) p) (K1 Generic.R (y a) p) x y where
    gChangeRep f (K1 x) = K1 (f x)
instance ( Generic (PrimaryKey table x)
         , Generic (PrimaryKey table y)
         , GChangeRep (Rep (PrimaryKey table x) ()) (Rep (PrimaryKey table y) ()) x y ) =>
    GChangeRep (K1 Generic.R (ForeignKey table x) p) (K1 Generic.R (ForeignKey table y) p) x y where
    gChangeRep f (K1 (ForeignKey x)) = K1 (ForeignKey (to' (gChangeRep f (from' x))))

class GAllValues (f :: * -> *) x where
    gAllValues :: (forall a. f a -> b) -> x -> [b]
instance (GAllValues f a, GAllValues f b) => GAllValues f (a :|: b) where
    gAllValues f (a :|: b) = gAllValues f a ++ gAllValues f b
instance (GAllValues f (a x), GAllValues f (b x)) => GAllValues f ((a :*: b) x) where
    gAllValues f (a :*: b) = gAllValues f a ++ gAllValues f b
instance (GAllValues f (p x)) => GAllValues f (D1 g p x) where
    gAllValues f (M1 a) = gAllValues f a
instance (GAllValues f (p x)) => GAllValues f (C1 g p x) where
    gAllValues f (M1 a) = gAllValues f a
instance (GAllValues f (p x)) => GAllValues f (S1 g p x) where
    gAllValues f (M1 a) = gAllValues f a
instance GAllValues f (K1 Generic.R (f x) a) where
    gAllValues f (K1 a) = [f a]
instance ( Generic (PrimaryKey related f)
         , GAllValues f (Rep (PrimaryKey related f) ()) ) =>
    GAllValues f (K1 Generic.R (ForeignKey related f) a) where
    gAllValues f (K1 (ForeignKey x)) = gAllValues f (from' x)
instance GAllValues f (f a) where
    gAllValues f x = [f x]

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
         , GChangeRep (Rep (PrimaryKey related (TableField related)) ()) (Rep (PrimaryKey related (TableField table)) ())
                      (TableField related) (TableField table) ) =>
    GDefaultTableFieldSettings (S1 f (K1 Generic.R (ForeignKey related (TableField table))) p) where

    gDefTblFieldSettings (_ :: Proxy (S1 f (K1 Generic.R (ForeignKey related (TableField table))) p )) = M1 $ K1 $ ForeignKey $ primaryKeySettings'
        where tableSettings = tblFieldSettings :: TableSettings related
              phantomTblSettings = phantomFieldSettings (Proxy :: Proxy related)
              primaryKeySettings :: PrimaryKey related (TableField related)
              primaryKeySettings = primaryKey phantomTblSettings tableSettings

              primaryKeySettings' :: PrimaryKey related (TableField table)
              primaryKeySettings' = to' (gChangeRep convertToForeignKeyField (from' primaryKeySettings))

              convertToForeignKeyField :: TableField related c -> TableField table c
              convertToForeignKeyField tf = tf { fieldFromTable = undefined
                                               , fieldName = keyName <> "_" <> fieldName tf
                                               , fieldConstraints = removeConstraints (fieldConstraints tf) }


              removeConstraints = filter (\x -> x /= SQLPrimaryKey && x /= SQLPrimaryKeyAutoIncrement && x /= SQLAutoIncrement)

              keyName = T.pack (selName (undefined :: S1 f (K1 Generic.R (ForeignKey related (TableField table))) ()))

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
    gReifySchema (K1 x) = [(fieldName x, fieldColDesc (fieldSettings x) (fieldConstraints x))]
instance (GReifySchema (a p), GReifySchema (b p)) => GReifySchema ((a :*: b) p) where
    gReifySchema (a :*: b) = gReifySchema a ++ gReifySchema b
instance ( Generic (PrimaryKey related (TableField table))
         , GReifySchema (Rep (PrimaryKey related (TableField table)) ()) ) =>
    GReifySchema (K1 Generic.R (ForeignKey related (TableField table)) p) where

    gReifySchema (K1 (ForeignKey x)) = gReifySchema (from' x)

class GFromSqlValues schema where
    gFromSqlValues :: FromSqlValuesM (schema a)
instance GFromSqlValues x => GFromSqlValues (D1 f x) where
    gFromSqlValues = M1 <$> gFromSqlValues
instance GFromSqlValues x => GFromSqlValues (C1 f x) where
    gFromSqlValues = M1 <$> gFromSqlValues
instance GFromSqlValues x => GFromSqlValues (S1 f x) where
    gFromSqlValues = M1 <$> gFromSqlValues
instance FieldSchema x => GFromSqlValues (K1 Generic.R (Identity x)) where
    gFromSqlValues = K1 . return <$> fromSqlValue
instance (GFromSqlValues a, GFromSqlValues b) => GFromSqlValues (a :*: b) where
    gFromSqlValues = (:*:) <$> gFromSqlValues <*> gFromSqlValues
instance ( Generic (PrimaryKey related Identity)
         , GFromSqlValues (Rep (PrimaryKey related Identity)) ) =>
    GFromSqlValues (K1 Generic.R (ForeignKey related Identity)) where

    gFromSqlValues = K1 . ForeignKey . to' <$> gFromSqlValues

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
instance FieldSchema x => GMakeSqlValues (K1 Generic.R (Identity x) a) where
    gMakeSqlValues (K1 x) = [makeSqlValue (runIdentity x)]
instance ( Generic (PrimaryKey related Identity)
         , GMakeSqlValues (Rep (PrimaryKey related Identity) ()) ) =>
    GMakeSqlValues (K1 Generic.R (ForeignKey related Identity) a) where
    gMakeSqlValues (K1 (ForeignKey x)) = gMakeSqlValues (from' x)

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

data ForeignKey table column = ForeignKey { reference :: (PrimaryKey table column) }
deriving instance Show (PrimaryKey table column) => Show (ForeignKey table column)

column :: t -> Identity t
column = return
columnValue :: Identity t -> t
columnValue = runIdentity

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

instance Show x => Show (Identity x) where
    show = show . runIdentity