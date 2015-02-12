{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Types
    ( ScopedField(..)
    , QueryField, QueryTable(..), QueryExpr(..)

    , Query(..), QExpr(..), QAssignment(..), QOrder(..), GenQExpr(..)

    , ScopeFields(..), Scope(..), Project(..), ScopingRule
    , getScope ) where

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Locate
import Database.Beam.Schema.Fields
import Database.Beam.SQL
import Database.HDBC

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Writer hiding (All)

import Data.Monoid hiding (All)
import Data.Proxy
import Data.Data
import Data.String
import qualified Data.Text as T

-- * Beam queries

-- | A `ScopedField` represents a field that has been brought in scope via an `allQ`. The `q` type is a thread type that ensures that this field cannot escape this query.
data ScopedField table name = ScopedField Int
                              deriving (Show, Typeable, Eq)
instance SchemaPart (ScopedField table name)
type instance NameFor (ScopedField table name) = name
type instance EmbeddedSchemaFor (ScopedField table name) = Empty
type instance Rename newName (ScopedField table name) = ScopedField table newName
instance Locator (ScopedField table name) Found where
    type LocateResult (ScopedField table name) Found = ScopedField table name
    locate a _ = a

data QueryField table field
data QueryTable table = QueryTable (PhantomFieldSchema table) table
type instance NameFor (QueryField table field) = NameFor field
type instance Rename newName (QueryField table field) = QueryField table (Rename newName field)

instance (Show (PhantomFieldSchema table), Show table) => Show (QueryTable table) where
    show (QueryTable phantoms table) = concat ["QueryTable (", show phantoms, ") (", show table, ")"]

instance ( Table table
         , FromSqlValues (PhantomFieldSchema table)
         , FromSqlValues (Schema table)) => FromSqlValues (QueryTable table) where
    fromSqlValues' = QueryTable <$> fromSqlValues' <*> (fromSchema <$> fromSqlValues')
    valuesNeeded (_ :: Proxy (QueryTable tbl)) = valuesNeeded (Proxy :: Proxy (PhantomFieldSchema tbl)) + valuesNeeded (Proxy :: Proxy (Schema tbl))
instance SchemaPart (QueryTable table)

data QueryExpr t = QueryExpr t deriving (Show, Read, Eq, Ord)
type instance NameFor (QueryExpr t) = () -- QueryExpr's cannot be referred to by name
type instance Rename newName (QueryExpr t) = QueryExpr t

instance FieldSchema t => FromSqlValues (QueryExpr t) where
    fromSqlValues' = QueryExpr <$> fromSqlValue
    valuesNeeded _ = 1

type ScopingRule = SQLFieldName -> SQLFieldName

class Project s where
    project :: ScopingRule -> s -> [SQLExpr]

data QOrder = Ascending | Descending deriving (Show, Read, Eq, Ord, Enum)

data GenQExpr where
    GenQExpr :: Typeable t => QExpr t -> GenQExpr

-- | A query that produces results of type `a`
data Query a where
    All :: (Table table, ScopeFields (QueryTable table)) => Proxy table -> Int -> Query (QueryTable table)
    EmptySet :: Query a
    Filter ::  Query a -> QExpr Bool -> Query a
    GroupBy :: Query a -> GenQExpr -> Query a
    OrderBy :: Query a -> GenQExpr -> QOrder -> Query a
    Join :: (Project (Scope schema1), Project (Scope schema2)) =>
            Query schema1 -> Query schema2 -> Query (schema1 :|: schema2)

    -- More joins...
    LeftJoin :: (Project (Scope schema1), Project (Scope schema2)) =>
                Query schema1 -> Query schema2 -> QExpr Bool -> Query (schema1 :|: Maybe schema2)
    RightJoin :: (Project (Scope schema1), Project (Scope schema2)) =>
                 Query schema1 -> Query schema2 -> QExpr Bool -> Query (Maybe schema1 :|: schema2)
    OuterJoin :: (Project (Scope schema1), Project (Scope schema2)) =>
                 Query schema1 -> Query schema2 -> QExpr Bool -> Query (Maybe schema1 :|: Maybe schema2)

    Project :: Query a -> (Scope a -> Scope b) -> Query b

    Limit :: Query a -> Integer -> Query a
    Offset :: Query a -> Integer -> Query a

data QExpr t where
    FieldE :: (Table table, Field table field) => ScopedField table field -> QExpr (TypeOf (FieldInTable table field))
    MaybeFieldE :: (Table table, Field table field) => Maybe (ScopedField table field) -> QExpr (Maybe (TypeOf (FieldInTable table field)))

    OrE :: QExpr Bool -> QExpr Bool -> QExpr Bool
    AndE :: QExpr Bool -> QExpr Bool -> QExpr Bool

    EqE, NeqE, LtE, GtE, LeE, GeE :: (Typeable a, Show a) => QExpr a -> QExpr a -> QExpr Bool

    ValE :: SqlValue -> QExpr a

    JustE :: Show a => QExpr a -> QExpr (Maybe a)
    NothingE :: QExpr (Maybe a)

    IsNothingE :: Typeable a => QExpr (Maybe a) -> QExpr Bool

    InE :: (Typeable a, Show a) => QExpr a -> QExpr [a] -> QExpr Bool

    ListE :: [QExpr a] -> QExpr [a]

    -- Aggregate functions
    CountE :: Typeable a => QExpr a -> QExpr Int

data QAssignment where
    QAssignment :: ( Table table, Field table field
                   , FieldInTable table field ~ column
                   , FieldSchema (TypeOf column) ) =>
                   ScopedField table field
                -> QExpr (TypeOf column)
                -> QAssignment

deriving instance Typeable Query
deriving instance Typeable QExpr

-- ** Scoping class

class ScopeFields a where
    scopeFields :: Proxy a -> Int -> Scope a
instance ScopeFields (QueryField table (Column name t)) where
    scopeFields (_ :: Proxy (QueryField table (Column name t))) i = ScopedField i :: ScopedField table name
instance (ScopeFields a, ScopeFields b) => ScopeFields (a :|: b) where
    scopeFields  (_ :: Proxy (a :|: b)) i = scopeFields (Proxy :: Proxy a) i :|: scopeFields (Proxy :: Proxy b) i
instance ( ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
         , ScopeFields (WrapFields (QueryField table) (Schema table)) ) =>
         ScopeFields (QueryTable table) where
    scopeFields (_ :: Proxy (QueryTable table)) i = scopeFields (Proxy :: Proxy (WrapFields (QueryField table) (PhantomFieldSchema table))) i :|:
                                                    scopeFields (Proxy :: Proxy (WrapFields (QueryField table) (Schema table))) i
instance ScopeFields (EmbedIn name (WrapFields (QueryField table) (PrimaryKeySchema relTbl))) =>
    ScopeFields (QueryField table (ForeignKey relTbl name)) where
    scopeFields (_ :: Proxy (QueryField table (ForeignKey relTbl name))) i =
        scopeFields (Proxy :: Proxy (EmbedIn name (WrapFields (QueryField table) (PrimaryKeySchema relTbl)))) i
instance ScopeFields t => ScopeFields (Maybe t) where
    scopeFields (_ :: Proxy (Maybe t)) = Just . scopeFields (Proxy :: Proxy t)

type family Scope a where
    Scope (Maybe t) = Maybe (Scope t)
    Scope (a :|: b) = Scope a :|: Scope b
    Scope (QueryTable x) = Scope (WrapFields (QueryField x) (PhantomFieldSchema x)) :|: Scope (WrapFields (QueryField x) (Schema x))
    --Scope (QueryField table field) = ScopedField table (NameFor field)
    Scope (QueryField table (Column name t)) = ScopedField table name
    Scope (QueryField table (ForeignKey relTbl name)) = Scope (EmbedIn name (WrapFields (QueryField table) (PrimaryKeySchema relTbl)))
    Scope (QueryExpr t) = QExpr t

getScope :: Query a -> Scope a
getScope x@(All _ i) = scopeFields (aProxy x) i
    where aProxy :: Query a -> Proxy a
          aProxy _ = Proxy
getScope EmptySet = error "getScope: EmptySet"
getScope (Filter q _) = getScope q
getScope (GroupBy q _) = getScope q
getScope (Join q1 q2) = getScope q1 :|: getScope q2
getScope (LeftJoin q1 q2 _) = getScope q1 :|: Just (getScope q2)
getScope (RightJoin q1 q2 _) = Just (getScope q1) :|: getScope q2
getScope (OuterJoin q1 q2 _) = Just (getScope q1) :|: Just (getScope q2)
getScope (Project q f) = f (getScope q)
getScope (Limit q _) = getScope q
getScope (Offset q _) = getScope q
getScope (OrderBy q _ _) = getScope q

-- ** Hand-derived instances for Query and QExpr since their types are too complicated for the deriving mechanism

instance Show (Query a) where
    show (All table i) = concat ["All ", show i, " :: ", T.unpack . dbTableName $ table]
    show (Filter q e) = concat ["Filter (", show q, ") (", show e, ")"]
    show (GroupBy q es) = concat ["GroupBy (", show q, ") (", show es, ")"]
    show (OrderBy q es ord) = concat ["OrderBy (", show q, ") (", show es, ") ", show ord]
    show (Join q1 q2) = concat ["Join (", show q1, ") (", show q2, ")"]
    show (LeftJoin q1 q2 e) = concat ["LeftJoin (", show q1, ") (", show q2, ") (", show e, ")"]
    show (OuterJoin q1 q2 e) = concat ["OuterJoin (", show q1, ") (", show q2, ") (", show e, ")"]
    show (RightJoin q1 q2 e) = concat ["RightJoin (", show q1, ") (", show q2, ") (", show e, ")"]
    show (Project q _) = concat ["Project (", show q, ") _"]
    show (Limit q i) = concat ["Limit (", show q, ") ", show i]
    show (Offset q i) = concat ["Offset (", show q, ") ", show i]
    show EmptySet = "EmptySet"

instance Show (QExpr t) where
    show (FieldE (table :: ScopedField table field)) = concat ["FieldE (", show table, ") ", T.unpack (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field))]
    show (MaybeFieldE (table :: Maybe (ScopedField table field))) = concat ["MaybeFieldE (", show table, ") ", T.unpack (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field))]

    show (AndE a b) = concat ["AndE (", show a, ") (", show b, ")"]
    show (OrE a b) = concat ["OrE (", show a, ") (", show b, ")"]

    show (EqE a b) = concat ["EqE (", show a, ") (", show b, ")"]
    show (LtE a b) = concat ["LtE (", show a, ") (", show b, ")"]
    show (GtE a b) = concat ["GtE (", show a, ") (", show b, ")"]
    show (LeE a b) = concat ["LeE (", show a, ") (", show b, ")"]
    show (GeE a b) = concat ["GeE (", show a, ") (", show b, ")"]
    show (NeqE a b) = concat ["NeqE (", show a, ") (", show b, ")"]

    show (ValE v) = concat ["ValE ", show v]

    show (JustE b) = concat ["JustE ", show b]
    show NothingE = "NothingE"

    show (IsNothingE b) = concat ["IsNothingE ", show b]

    show (InE a xs) = concat ["InE (", show a, ") ", show xs]
    show (ListE xs) = concat ["ListE ", show xs]

    show (CountE x) = concat ["CountE (", show x, ")"]


instance Eq (Query a) where
    EmptySet == EmptySet = True
    All _ x == All _ y = x == y
    Filter q1 e1 == Filter q2 e2 = q1 == q2 && e1 == e2
    GroupBy q1 es1 == GroupBy q2 es2 = q1 == q2 && es1 == es2
    OrderBy q1 es1 ord1 == OrderBy q2 es2 ord2 = q1 == q2 && es1 == es2 && ord1 == ord2
    Join a1 b1 == Join a2 b2 = a1 == a2 && b1 == b2
    _ == _ = False

instance Eq (QExpr t) where
    FieldE q1 == FieldE q2 =
        case cast q1 of
          Nothing -> False
          Just q1 -> q1 == q2
    MaybeFieldE q1 == MaybeFieldE q2 =
        case cast q1 of
          Nothing -> False
          Just q1 -> q1 == q2

    AndE a1 b1 == AndE a2 b2 = a1 == a2 && b1 == b2

    EqE a1 b1 == EqE a2 b2 = case cast (a1, b1) of
                               Just (a1, b1)
                                   | a1 == a2 && b1 == b2 -> True
                                   | otherwise -> False
                               Nothing -> False

    ValE a == ValE b = a == b

    JustE a == JustE b = a == b
    NothingE == NothingE = True

    IsNothingE a == IsNothingE b = case cast a of
                                     Just a' -> a' == b
                                     Nothing -> False
    InE a as == InE b bs = case cast (a, as) of
                             Nothing -> False
                             Just (a', as') -> a' == b && as' == bs
    ListE a == ListE b = a == b

    CountE a == CountE b = case cast a of
                             Nothing -> False
                             Just a' -> a' == b



    _ == _ = False

deriving instance Show GenQExpr
instance Eq GenQExpr where
    GenQExpr q1 == GenQExpr q2 = case cast q1 of
                                   Nothing -> False
                                   Just q1 -> q1 == q2