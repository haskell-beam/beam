{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Types
    ( ScopedField(..)
    , QueryField, QueryTable(..)

    , Query(..), QExpr(..), QAssignment(..)

    , ScopeFields(..), Scope(..)
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
instance SchemaPart (QueryTable table)

-- | A query that produces results of type `a`
data Query a where
    All :: (Table table, ScopeFields (QueryTable table)) => Proxy table -> Int -> Query (QueryTable table)
    EmptySet :: Query a
    Filter ::  Query a -> QExpr Bool -> Query a
    GroupBy :: Typeable t => Query a -> [QExpr t] -> Query a
    Join :: Query schema1 -> Query schema2 -> Query (schema1 :|: schema2)
--    Project :: Query a -> [QExpr t] -> Query b

data QExpr t where
    FieldE :: (Table table, Field table field) => ScopedField table field -> QExpr (TypeOf (FieldInTable table field))

    OrE :: QExpr Bool -> QExpr Bool -> QExpr Bool
    AndE :: QExpr Bool -> QExpr Bool -> QExpr Bool

    EqE :: (Typeable a, Show a) => QExpr a -> QExpr a -> QExpr Bool

    ValE :: SqlValue -> QExpr a

    JustE :: Show a => QExpr a -> QExpr (Maybe a)
    NothingE :: QExpr (Maybe a)

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

type family Scope a where
    Scope (a :|: b) = Scope a :|: Scope b
    Scope (QueryTable x) = Scope (WrapFields (QueryField x) (PhantomFieldSchema x)) :|: Scope (WrapFields (QueryField x) (Schema x))
    --Scope (QueryField table field) = ScopedField table (NameFor field)
    Scope (QueryField table (Column name t)) = ScopedField table name
    Scope (QueryField table (ForeignKey relTbl name)) = Scope (EmbedIn name (WrapFields (QueryField table) (PrimaryKeySchema relTbl)))

getScope :: Query a -> Scope a
getScope x@(All _ i) = scopeFields (aProxy x) i
    where aProxy :: Query a -> Proxy a
          aProxy _ = Proxy
getScope EmptySet = undefined
getScope (Filter q _) = getScope q
getScope (GroupBy q _) = getScope q
getScope (Join q1 q2) = getScope q1 :|: getScope q2

-- ** Hand-derived instances for Query and QExpr since their types are too complicated for the deriving mechanism

instance Show (Query a) where
    show (All table i) = concat ["All ", show i, " :: ", T.unpack . dbTableName $ table]
    show (Filter q e) = concat ["Filter (", show q, ") (", show e, ")"]
    show (GroupBy q es) = concat ["GroupBy (", show q, ") (", show es, ")"]
    show (Join q1 q2) = concat ["Join (", show q1, ") (", show q2, ")"]
    show EmptySet = "EmptySet"

instance Show (QExpr t) where
    show (FieldE (table :: ScopedField table field)) = concat ["FieldE (", show table, ") ", T.unpack (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field))]

    show (AndE a b) = concat ["AndE (", show a, ") (", show b, ")"]
    show (OrE a b) = concat ["OrE (", show a, ") (", show b, ")"]

    show (EqE a b) = concat ["EqE (", show a, ") (", show b, ")"]

    show (ValE v) = concat ["ValE ", show v]

    show (JustE b) = concat ["JustE ", show b]
    show NothingE = "NothingE"

instance Eq (Query a) where
    EmptySet == EmptySet = True
    All _ x == All _ y = x == y
    Filter q1 e1 == Filter q2 e2 = q1 == q2 && e1 == e2
    GroupBy q1 es1 == GroupBy q2 es2 =
        q1 == q2 &&
        case cast es1 of
          Nothing -> False
          Just es1 -> es1 == es2
    Join a1 b1 == Join a2 b2 = a1 == a2 && b1 == b2
    _ == _ = False

instance Eq (QExpr t) where
    FieldE q1 == FieldE q2 =
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

    _ == _ = False