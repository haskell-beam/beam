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
import qualified Data.Text as T

-- * Query combinators

of_ :: Table table => table
of_ = undefined

all_ :: (Table table, ScopeFields (QueryTable table)) => table -> Query q (QueryTable table)
all_ (_ :: table) = All (Proxy :: Proxy table) 0

maxTableOrdinal :: Query q a -> Int
maxTableOrdinal q = getMax (execWriter (traverseQueryM maxQ maxE q))
    where maxQ :: Query q a -> Writer (Max Int) ()
          maxQ (All _ i) = tell (Max i)
          maxQ _ = return ()

          maxE :: QExpr q a -> Writer (Max Int) ()
          maxE _ = return ()

join_ :: Query q a -> Query q b -> Query q (a :|: b)
join_ l r = Join l r'
    where maxOrdL = maxTableOrdinal l + 1

          remapQuery :: Query q b -> Maybe (Query q b)
          remapQuery (All tbl i)
              | i < maxOrdL = Just (All tbl (i + maxOrdL))
              | otherwise = Nothing
          remapQuery x = Nothing

          r' = rewriteQuery remapQuery (\_ -> Nothing) r

where_ :: Query q a -> (forall q. Scope q a -> QExpr q Bool) -> Query q a
where_ q mkExpr = Filter q (mkExpr (getScope q))

-- | Get all related records for a one to many relationship
(#*) :: ( OneToMany r
        , ScopeFields (WrapFields (QueryField (OneToManyRange r)) (Schema (OneToManyRange r)))
        , ScopeFields (WrapFields (QueryField (OneToManyRange r)) (PhantomFieldSchema (OneToManyRange r))) ) =>
        Query q (QueryTable (OneToManyDomain r)) -> r -> Query q (QueryTable (OneToManyDomain r) :|: QueryTable (OneToManyRange r))
q #* (r :: r) = (q `join_` allInRange) `where_` (\(domainTbl :|: rangeTbl) -> generateJoinCondition r domainTbl rangeTbl)
    where allInRange = all_ (of_ :: OneToManyRange r)

text_ :: T.Text -> QExpr q T.Text
text_ = ValE . SqlString . T.unpack