{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- | Contains definitions for rewriting Beam queries under optimizations
module Database.Beam.Query.Rewrite
    ( rewriteQueryM, traverseQueryM, rewriteQuery
    , rewriteExprM

    , combineFilterOpt, propEmptySets

    , booleanOpts

    , allQueryOpts, allExprOpts ) where

import Database.Beam.Query.Types

import Database.Beam.Schema.Tables
import Database.Beam.Types
import Database.Beam.SQL
import Database.HDBC

import Control.Applicative
import Control.Monad.Identity

import Data.Monoid hiding (All)

-- * Rewrite functions for expressions and queries
rw :: Monad m => (forall a. Query a -> m (Maybe (Query a))) -> (forall a . QExpr a -> m (Maybe (QExpr a))) -> Query a -> m (Query a)
rw f fe x = do x' <- f x
               case x' of
                 Nothing -> return x
                 Just x'
                     | x == x' -> return x
                     | otherwise -> rewriteQueryM f fe x'
rwE :: Monad m => (forall a. QExpr a -> m (Maybe (QExpr a))) -> (forall a. Query a -> m (Maybe (Query a))) -> QExpr a -> m (QExpr a)
rwE f fq x = do x' <- f x
                case x' of
                  Nothing -> return x
                  Just x'
                      | x == x' -> return x
                      | otherwise -> rewriteExprM f fq x'

rewriteQueryM :: Monad m => (forall a . Query a -> m (Maybe (Query a))) -> (forall a . QExpr a -> m (Maybe (QExpr a))) -> Query a -> m (Query a)
rewriteQueryM f fe (Filter query e) = do query' <- rewriteQueryM f fe query
                                         e' <- rewriteExprM fe f e
                                         rw f fe (Filter query' e')
rewriteQueryM f fe (GroupBy query es) = do query' <- rewriteQueryM f fe query
                                           es' <- mapM (rewriteExprM fe f) es
                                           rw f fe (GroupBy query' es')
rewriteQueryM f fe (Join q1 q2) = do q1' <- rewriteQueryM f fe q1
                                     q2' <- rewriteQueryM f fe q2
                                     rw f fe (Join q1' q2')
rewriteQueryM f fe x = rw f fe x

rewriteQuery :: (forall a. Query a -> Maybe (Query a)) -> (forall a. QExpr a -> Maybe (QExpr a)) -> Query a -> Query a
rewriteQuery f fe x = runIdentity (rewriteQueryM (return . f) (return . fe) x)

traverseQueryM :: Monad m => (forall a. Query a -> m ()) -> (forall a. QExpr a -> m ()) -> Query a -> m ()
traverseQueryM t te x = rewriteQueryM (\x -> t x >> return Nothing) (\x -> te x >> return Nothing) x >> return ()

rewriteExprM :: Monad m => (forall a . QExpr a -> m (Maybe (QExpr a))) -> (forall a . Query a -> m (Maybe (Query a))) -> QExpr a -> m (QExpr a)
rewriteExprM f fq (AndE a b) = do a' <- rewriteExprM f fq a
                                  b' <- rewriteExprM f fq b
                                  rwE f fq (AndE a' b')
rewriteExprM f fq (EqE a b) = do a' <- rewriteExprM f fq a
                                 b' <- rewriteExprM f fq b
                                 rwE f fq (EqE a' b')
rewriteExprM f fq x = rwE f fq x

-- * Query optimizations

combineFilterOpt :: Query a -> Maybe (Query a)
combineFilterOpt (Filter (Filter q e1) e2) = Just (Filter q (AndE e1 e2))
combineFilterOpt (Filter q (ValE (SqlBool True))) = Just q
combineFilterOpt (Filter q (ValE (SqlBool False))) = Just EmptySet
combineFilterOpt _ = Nothing

-- | Propagates expressions containing empty sets
propEmptySets :: Query a -> Maybe (Query a)
propEmptySets (Join EmptySet _) = Just EmptySet
propEmptySets (Join _ EmptySet) = Just EmptySet
propEmptySets (Filter EmptySet _) = Just EmptySet
propEmptySets (GroupBy EmptySet _) = Just EmptySet
propEmptySets _ = Nothing

-- | Propagates filter clauses that are inside inner joins passed the inner join, so that there is simply one where statement
propFiltersPastInnerJoins :: Query a -> Maybe (Query a)
propFiltersPastInnerJoins (Join (Filter a e) b) = Just (Filter (Join a b) e)
propFiltersPastInnerJoins (Join a (Filter b e)) = Just (Filter (Join a b) e)
propFiltersPastInnerJoins _ = Nothing

-- -- | Moves filters to the rightmost subexpression possible. This puts the expression into a canonical form for lowering join expressions
-- moveFiltersRight _ = Nothing -- Nothing for now, but may be extended as we add more query types

-- -- | Pushes filter expressions down to join clauses where possible.
-- --
-- --   Essentially, expressions containing references to tble
-- lowerOnClauses

booleanOpts :: QExpr a -> Maybe (QExpr a)
booleanOpts (AndE (ValE (SqlBool False)) _) = Just (ValE (SqlBool False))
booleanOpts (AndE _ (ValE (SqlBool False))) = Just (ValE (SqlBool False))
booleanOpts (AndE (ValE (SqlBool True)) (ValE (SqlBool True))) = Just (ValE (SqlBool True))
booleanOpts (AndE (ValE (SqlBool True)) q) = Just q
booleanOpts (AndE q (ValE (SqlBool True))) = Just q
booleanOpts x = Nothing

allQueryOpts q = combineFilterOpt q <|> propEmptySets q <|> propFiltersPastInnerJoins q -- <|> moveFiltersRight q <|> lowerOnClauses q
allExprOpts e = booleanOpts e