{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Types
    ( ScopedField(..)
    , QueryField, QueryTable(..)

    , Query(..), QExpr(..), QAssignment(..)

    , ScopeFields(..), Scope(..)
    , getScope

    , coerceQueryThread, coerceQExprThread, coerceScopedFieldThread ) where

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Locate
import Database.Beam.Schema.Fields
import Database.Beam.Types
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
data ScopedField q table name = ScopedField Int
                                deriving (Show, Typeable, Eq)
instance SchemaPart (ScopedField q table name)
type instance NameFor (ScopedField q table name) = name
instance Locator (ScopedField q table name) Found where
    type LocateResult (ScopedField q table name) Found = ScopedField q table name
    locate a _ = a

data QueryField table field
data QueryTable table = QueryTable (PhantomFieldSchema table) table

instance (Show (PhantomFieldSchema table), Show table) => Show (QueryTable table) where
    show (QueryTable phantoms table) = concat ["QueryTable (", show phantoms, ") (", show table, ")"]

instance ( Table table
         , FromSqlValues (PhantomFieldSchema table)
         , FromSqlValues (Schema table)) => FromSqlValues (QueryTable table) where
    fromSqlValues' = QueryTable <$> fromSqlValues' <*> (fromSchema <$> fromSqlValues')
instance SchemaPart (QueryTable table)

-- | A query that produces results of type `a`
data Query s a where
    All :: (Table table, ScopeFields (QueryTable table)) => Proxy table -> Int -> Query q (QueryTable table)
    EmptySet :: Query q a
    Filter ::  Query q a -> QExpr q Bool -> Query q a
    GroupBy :: Typeable t => Query q a -> [QExpr q t] -> Query q a
    Join :: Query q schema1 -> Query q schema2 -> Query q (schema1 :|: schema2)
--    Project :: Query q a -> [QExpr q t] -> Query q b

data QExpr q t where
    FieldE :: (Table table, Field table field) => ScopedField q table field -> QExpr q (FieldType (FieldInTable table field))

    OrE :: QExpr q Bool -> QExpr q Bool -> QExpr q Bool
    AndE :: QExpr q Bool -> QExpr q Bool -> QExpr q Bool

    EqE :: (Typeable a, Show a) => QExpr q a -> QExpr q a -> QExpr q Bool

    ValE :: SqlValue -> QExpr q a

    JustE :: Show a => QExpr q a -> QExpr q (Maybe a)
    NothingE :: QExpr q (Maybe a)

data QAssignment q where
    QAssignment :: ( Table table, Field table field
                   , FieldInTable table field ~ fs
                   , FieldSchema fs ) =>
                   ScopedField q table field -> QExpr q (FieldType fs) -> QAssignment q

deriving instance Typeable Query
deriving instance Typeable QExpr

-- ** Scoping class

class ScopeFields a where
    scopeFields :: Proxy q -> Proxy a -> Int -> Scope q a
instance ScopeFields (QueryField table field) where
    scopeFields (_ :: Proxy q) (_ :: Proxy (QueryField table field)) i = ScopedField i :: ScopedField q table (NameFor field)
instance (ScopeFields a, ScopeFields b) => ScopeFields (a :|: b) where
    scopeFields (q :: Proxy q) (_ :: Proxy (a :|: b)) i =
      let ret :: Scope q a :|: Scope q b
          ret = scopeFields q (Proxy :: Proxy a) i :|: scopeFields q (Proxy :: Proxy b) i
      in ret
instance ( ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
         , ScopeFields (WrapFields (QueryField table) (Schema table)) ) =>
         ScopeFields (QueryTable table) where
    scopeFields (_ :: Proxy q) (_ :: Proxy (QueryTable table)) i = scopeFields (Proxy :: Proxy q) (Proxy :: Proxy (WrapFields (QueryField table) (PhantomFieldSchema table))) i :|:
                                                                   scopeFields (Proxy :: Proxy q) (Proxy :: Proxy (WrapFields (QueryField table) (Schema table))) i

type family Scope q a where
    Scope q (a :|: b) = Scope q a :|: Scope q b
    Scope q (QueryTable x) = Scope q (WrapFields (QueryField x) (PhantomFieldSchema x)) :|: Scope q (WrapFields (QueryField x) (Schema x))
    Scope q (QueryField table field) = ScopedField q table (NameFor field)

getScope :: Query q a -> Scope q a
getScope x@(All _ i) = scopeFields qProxy aProxy i
    where (qProxy, aProxy) = proxies x
          proxies :: Query q a -> (Proxy q, Proxy a)
          proxies _ = (Proxy, Proxy)
getScope EmptySet = undefined
getScope (Filter q _) = getScope q
getScope (GroupBy q _) = getScope q
getScope (Join q1 q2) = getScope q1 :|: getScope q2

-- ** Hand-derived instances for Query and QExpr since their types are too complicated for the deriving mechanism

instance Show (Query s a) where
    show (All table i) = concat ["All ", show i, " :: ", T.unpack . dbTableName $ table]
    show (Filter q e) = concat ["Filter (", show q, ") (", show e, ")"]
    show (GroupBy q es) = concat ["GroupBy (", show q, ") (", show es, ")"]
    show (Join q1 q2) = concat ["Join (", show q1, ") (", show q2, ")"]
    show EmptySet = "EmptySet"

instance Show (QExpr q t) where
    show (FieldE (table :: ScopedField q table field)) = concat ["FieldE (", show table, ") ", T.unpack (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field))]

    show (AndE a b) = concat ["AndE (", show a, ") (", show b, ")"]
    show (OrE a b) = concat ["OrE (", show a, ") (", show b, ")"]

    show (EqE a b) = concat ["EqE (", show a, ") (", show b, ")"]

    show (ValE v) = concat ["ValE ", show v]

    show (JustE b) = concat ["JustE ", show b]
    show NothingE = "NothingE"

instance Eq (Query s a) where
    EmptySet == EmptySet = True
    All _ x == All _ y = x == y
    Filter q1 e1 == Filter q2 e2 = q1 == q2 && e1 == e2
    GroupBy q1 (es1 :: [QExpr q t1]) == GroupBy q2 (es2 :: [QExpr q t2]) =
        q1 == q2 &&
        case cast es1' of
          Nothing -> False
          Just es1' -> es1' == es2'
        where es1' = map coerceQExprThread es1
              es2' = map coerceQExprThread es2
    Join a1 b1 == Join a2 b2 = a1 == a2 && b1 == b2
    _ == _ = False

instance Eq (QExpr q t) where
    FieldE (q1 :: ScopedField q1 table1 field1) == FieldE (q2 :: ScopedField q2 table2 field2) =
        case cast (coerceScopedFieldThread q1) of
          Nothing -> False
          Just q1 -> q1 == coerceScopedFieldThread q2

    AndE a1 b1 == AndE a2 b2 = a1 == a2 && b1 == b2

    EqE a1 b1 == EqE a2 b2 = case cast (coerceQExprThread a1, coerceQExprThread b1) of
                               Just (a1, b1)
                                   | a1 == coerceQExprThread a2 && b1 == coerceQExprThread b2 -> True
                                   | otherwise -> False
                               Nothing -> False

    ValE a == ValE b = a == b

    JustE a == JustE b = a == b
    NothingE == NothingE = True

    _ == _ = False

-- * Query thread coercion

-- | Coerces the query thread to () in order to allow for easy typechecks. DO NOT USE UNLESS YOU KNOW WHAT YOU'RE DOING!
coerceQueryThread :: Query q a -> Query () a
coerceQueryThread (All q i) = All q i
coerceQueryThread (Filter q e) = Filter (coerceQueryThread q) (coerceQExprThread e)
coerceQueryThread (GroupBy q es) = GroupBy (coerceQueryThread q) (map coerceQExprThread es)
coerceQueryThread (Join a b) = Join (coerceQueryThread a) (coerceQueryThread b)
coerceQueryThread EmptySet = EmptySet

coerceQExprThread :: QExpr q t -> QExpr () t
coerceQExprThread (FieldE f) = FieldE (coerceScopedFieldThread f)
coerceQExprThread (AndE a b) = AndE (coerceQExprThread a) (coerceQExprThread b)
coerceQExprThread (OrE a b) = OrE (coerceQExprThread a) (coerceQExprThread b)
coerceQExprThread (EqE a b) = EqE (coerceQExprThread a) (coerceQExprThread b)
coerceQExprThread (ValE v) = ValE v

coerceQExprThread (JustE b) = JustE (coerceQExprThread b)
coerceQExprThread NothingE = NothingE

coerceScopedFieldThread :: ScopedField q t f -> ScopedField () t f
coerceScopedFieldThread (ScopedField i) = ScopedField i