{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
-- | Contains definitions for rewriting Beam queries under optimizations
module Database.Beam.Query.Rewrite
    ( rewriteQueryM, traverseQueryM, rewriteQuery
    , rewriteExprM

    , combineFilterOpt, joinsAndEmptySets

    , booleanOpts

    , allQueryOpts, allExprOpts ) where

import Database.Beam.Query.Types

import Database.Beam.Schema
import Database.Beam.Types
import Database.Beam.SQL

import Control.Applicative
import Control.Monad.Identity

import Data.Monoid hiding (All)

-- * Rewrite functions for expressions and queries
rw :: Monad m => (forall q a. Query q a -> m (Maybe (Query q a))) -> (forall q a . QExpr q a -> m (Maybe (QExpr q a))) -> Query q a -> m (Query q a)
rw f fe x = do x' <- f x
               case x' of
                 Nothing -> return x
                 Just x'
                     | x == x' -> return x
                     | otherwise -> rewriteQueryM f fe x'
rwE :: Monad m => (forall q a. QExpr q a -> m (Maybe (QExpr q a))) -> (forall q a. Query q a -> m (Maybe (Query q a))) -> QExpr q a -> m (QExpr q a)
rwE f fq x = do x' <- f x
                case x' of
                  Nothing -> return x
                  Just x'
                      | x == x' -> return x
                      | otherwise -> rewriteExprM f fq x'

rewriteQueryM :: Monad m => (forall q a . Query q a -> m (Maybe (Query q a))) -> (forall q a . QExpr q a -> m (Maybe (QExpr q a))) -> Query q a -> m (Query q a)
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

rewriteQuery :: (forall q a. Query q a -> Maybe (Query q a)) -> (forall q a. QExpr q a -> Maybe (QExpr q a)) -> Query q a -> Query q a
rewriteQuery f fe x = runIdentity (rewriteQueryM (return . f) (return . fe) x)

traverseQueryM :: Monad m => (forall q a. Query q a -> m ()) -> (forall q a. QExpr q a -> m ()) -> Query q a -> m ()
traverseQueryM t te x = rewriteQueryM (\x -> t x >> return Nothing) (\x -> te x >> return Nothing) x >> return ()

rewriteExprM :: Monad m => (forall q a . QExpr q a -> m (Maybe (QExpr q a))) -> (forall q a . Query q a -> m (Maybe (Query q a))) -> QExpr q a -> m (QExpr q a)
rewriteExprM f fq (AndE a b) = do a' <- rewriteExprM f fq a
                                  b' <- rewriteExprM f fq b
                                  rwE f fq (AndE a' b')
rewriteExprM f fq (EqE a b) = do a' <- rewriteExprM f fq a
                                 b' <- rewriteExprM f fq b
                                 rwE f fq (EqE a' b')
rewriteExprM f fq x = rwE f fq x

-- * Query optimizations

combineFilterOpt :: Query q a -> Maybe (Query q a)
combineFilterOpt (Filter (Filter q e1) e2) = Just (Filter q (AndE e1 e2))
combineFilterOpt (Filter q (BoolE True)) = Just q
combineFilterOpt (Filter q (BoolE False)) = Just EmptySet
combineFilterOpt _ = Nothing

joinsAndEmptySets :: Query q a -> Maybe (Query q a)
joinsAndEmptySets (Join EmptySet _) = Just EmptySet
joinsAndEmptySets (Join _ EmptySet) = Just EmptySet
joinsAndEmptySets _ = Nothing

booleanOpts :: QExpr q a -> Maybe (QExpr q a)
booleanOpts (AndE (BoolE False) _) = Just (BoolE False)
booleanOpts (AndE _ (BoolE False)) = Just (BoolE False)
booleanOpts (AndE (BoolE True) (BoolE True)) = Just (BoolE True)
booleanOpts x = Nothing

allQueryOpts q = combineFilterOpt q <|> joinsAndEmptySets q
allExprOpts e = booleanOpts e