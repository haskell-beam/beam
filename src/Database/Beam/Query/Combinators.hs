{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators, FlexibleContexts, GADTs, TypeFamilies #-}
module Database.Beam.Query.Combinators where

import Database.Beam.Schema
import Database.Beam.Query.Types
import Database.Beam.Query.Rewrite

import Database.Beam.SQL

import Control.Monad.Writer hiding (All)

import Data.Proxy
import Data.Semigroup hiding (All)
import Data.Typeable

-- * Query combinators

of_ :: Table table => table
of_ = undefined

allQ :: (Table table, ScopeFields (QueryTable table)) => table -> Query q (QueryTable table)
allQ (_ :: table) = All (Proxy :: Proxy table) 0

maxTableOrdinal :: Query q a -> Int
maxTableOrdinal q = getMax (execWriter (traverseQueryM maxQ maxE q))
    where maxQ :: Query q a -> Writer (Max Int) ()
          maxQ (All _ i) = tell (Max i)
          maxQ _ = return ()

          maxE :: QExpr q a -> Writer (Max Int) ()
          maxE _ = return ()

joinQ :: Query q a -> Query q b -> Query q (a :|: b)
joinQ l r = Join l r'
    where maxOrdL = maxTableOrdinal l + 1

          remapQuery :: Query q b -> Maybe (Query q b)
          remapQuery (All tbl i)
              | i < maxOrdL = Just (All tbl (i + maxOrdL))
              | otherwise = Nothing
          remapQuery x = Nothing

          r' = rewriteQuery remapQuery (\_ -> Nothing) r

where_ :: Query q a -> (forall q. Scope q a -> QExpr q Bool) -> Query q a
where_ q mkExpr = Filter q (mkExpr (getScope q))

(#.) :: ( Locate schema name ~ locator
        , Locator schema locator
        , LocateResult schema locator ~ ScopedField q table field
        , LocateResult (FieldInTable table field) Found ~ r
        , Table table, Field table field) =>
        schema -> name -> QExpr q r
s #. field = FieldE (getField' s field)


(#==) :: (SQLValable a, Typeable a) => QExpr q a -> QExpr q a -> QExpr q Bool
(#==) = EqE
