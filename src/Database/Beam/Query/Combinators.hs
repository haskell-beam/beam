{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators, FlexibleContexts, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Combinators where

import Database.Beam.Schema
import Database.Beam.Query.Types
import Database.Beam.Query.Rewrite

import Database.Beam.SQL
import Database.HDBC

import Control.Monad.Writer hiding (All)

import Data.Proxy
import Data.Semigroup hiding (All)
import Data.Typeable
import Data.Convertible
import qualified Data.Text as T

-- * Query combinators

of_ :: Table table => table
of_ = undefined

all_ :: (Table table, ScopeFields (QueryTable table)) => table -> Query (QueryTable table)
all_ (_ :: table) = All (Proxy :: Proxy table) 0

maxTableOrdinal :: Query a -> Int
maxTableOrdinal q = getMax (execWriter (traverseQueryM maxQ maxE q))
    where maxQ :: Query a -> Writer (Max Int) ()
          maxQ (All _ i) = tell (Max i)
          maxQ _ = return ()

          maxE :: QExpr a -> Writer (Max Int) ()
          maxE _ = return ()

join_ :: Query a -> Query b -> Query (a :|: b)
join_ l r = Join l r'
    where maxOrdL = max (maxTableOrdinal l) (maxTableOrdinal r) + 1

          remapQuery :: Query b -> Maybe (Query b)
          remapQuery (All tbl i)
              | i < maxOrdL = Just (All tbl (i + maxOrdL))
              | otherwise = Nothing
          remapQuery _ = Nothing

          remapExpr :: QExpr b -> Maybe (QExpr b)
          remapExpr (FieldE (ScopedField i :: ScopedField table field))
              | i < maxOrdL = Just (FieldE (ScopedField (i + maxOrdL) :: ScopedField table field))
              | otherwise = Nothing
          remapExpr _ = Nothing

          r' = rewriteQuery remapQuery remapExpr r

where_ :: Query a -> (Scope a -> QExpr Bool) -> Query a
where_ q mkExpr = Filter q (mkExpr (getScope q))

-- | Get all related records for a relationship
(#@*) :: ( Relationship subject object r
         , ScopeFields (WrapFields (QueryField object) (Schema object))
         , ScopeFields (WrapFields (QueryField object) (PhantomFieldSchema object)) ) =>
         Query (QueryTable subject) -> r -> Query (QueryTable subject :|: QueryTable object)
q #@* r = injectSubjectAndObjectProxy $
          \subjectProxy (objectProxy :: Proxy object) ->
          let allInRange = all_ (of_ :: object)
          in (q `join_` allInRange) `where_` (\(sTbl :|: oTbl) -> joinCondition r subjectProxy objectProxy sTbl oTbl)
    where injectSubjectAndObjectProxy :: (Proxy subject -> Proxy object -> Query (QueryTable subject :|: QueryTable object))
                                      -> Query (QueryTable subject :|: QueryTable object)
          injectSubjectAndObjectProxy f = f Proxy Proxy

(#*@) :: ( Relationship subject object r
         , ScopeFields (QueryTable subject) ) =>
         Query (QueryTable object) -> r -> Query (QueryTable subject :|: QueryTable object)
q #*@ r = let query = (all_ of_ `join_` q) `where_` (\(sTbl :|: oTbl) -> joinCondition r subjectProxy objectProxy sTbl oTbl)

              proxies :: Query (QueryTable subject :|: QueryTable object) -> (Proxy subject, Proxy object)
              proxies _ = (Proxy, Proxy)
              (subjectProxy, objectProxy) = proxies query
          in query

(=#) :: (Table table, Field table field
        , FieldInTable table field ~ Column name fs
        , FieldSchema fs) =>
        ScopedField table field
     -> QExpr fs
     -> QAssignment
(=#) = QAssignment

text_ :: T.Text -> QExpr T.Text
text_ = ValE . SqlString . T.unpack
num_ :: Integral a => a -> QExpr a
num_ = ValE . SqlInteger . fromIntegral
val_ :: Convertible a SqlValue => a -> QExpr a
val_ = ValE . convert

just_ :: Show a => QExpr a -> QExpr (Maybe a)
just_ = JustE
nothing_ :: QExpr (Maybe a)
nothing_ = NothingE
