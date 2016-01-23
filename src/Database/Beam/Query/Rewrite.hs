{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- | Contains definitions for rewriting Beam queries under optimizations
module Database.Beam.Query.Rewrite
    ( rewriteQueryM, traverseQueryM, rewriteQuery
    , rewriteExprM, traverseExprM

    , combineFilterOpt, propEmptySets

    , booleanOpts

    , allQueryOpts, allExprOpts ) where

import Database.Beam.Query.Types

import Database.Beam.Schema.Tables
import Database.Beam.Types
import Database.Beam.SQL
import Database.HDBC

import Control.Applicative
import Control.Monad
import Control.Monad.Identity

import Data.Monoid hiding (All)
import Data.Typeable

-- * Rewrite functions for expressions and queries
rw :: Monad m => (forall a. Query db a -> m (Maybe (Query db a))) -> (forall a . QExpr a -> m (Maybe (QExpr a))) -> Query db a -> m (Query db a)
rw f fe x = do x' <- f x
               case x' of
                 Nothing -> return x
                 Just x'
                     | x == x' -> return x
                     | otherwise -> rewriteQueryM f fe x'
rwE :: Monad m => (forall a. QExpr a -> m (Maybe (QExpr a))) -> (forall a. Query db a -> m (Maybe (Query db a))) -> QExpr a -> m (QExpr a)
rwE f fq x = do x' <- f x
                case x' of
                  Nothing -> return x
                  Just x'
                      | x == x' -> return x
                      | otherwise -> rewriteExprM f fq x'

rewriteQueryM :: Monad m => (forall a . Query db a -> m (Maybe (Query db a))) -> (forall a . QExpr a -> m (Maybe (QExpr a))) -> Query db a -> m (Query db a)
rewriteQueryM f fe (Filter query e) = do query' <- rewriteQueryM f fe query
                                         e' <- rewriteExprM fe f e
                                         rw f fe (Filter query' e')
rewriteQueryM f fe (GroupBy query es) = do query' <- rewriteQueryM f fe query
                                           es' <- case es of
                                                    GenQExpr es -> GenQExpr `liftM` rewriteExprM fe f es
                                           rw f fe (GroupBy query' es')
rewriteQueryM f fe (OrderBy query es ord) = do query' <- rewriteQueryM f fe query
                                               es' <- case es of
                                                        GenQExpr es -> GenQExpr `liftM` rewriteExprM fe f es
                                               rw f fe (OrderBy query' es' ord)
rewriteQueryM f fe (Join q1 q2) = do q1' <- rewriteQueryM f fe q1
                                     q2' <- rewriteQueryM f fe q2
                                     rw f fe (Join q1' q2')
rewriteQueryM f fe (LeftJoin q1 q2 e) = do q1' <- rewriteQueryM f fe q1
                                           q2' <- rewriteQueryM f fe q2
                                           e' <- rewriteExprM fe f e
                                           rw f fe (LeftJoin q1' q2' e')
rewriteQueryM f fe (RightJoin q1 q2 e) = do q1' <- rewriteQueryM f fe q1
                                            q2' <- rewriteQueryM f fe q2
                                            e' <- rewriteExprM fe f e
                                            rw f fe (RightJoin q1' q2' e')
rewriteQueryM f fe (OuterJoin q1 q2 e) = do q1' <- rewriteQueryM f fe q1
                                            q2' <- rewriteQueryM f fe q2
                                            e' <- rewriteExprM fe f e
                                            rw f fe (OuterJoin q1' q2' e')
-- rewriteQueryM f fe (Project q g) = do q' <- rewriteQueryM f fe q
--                                       rw f fe (Project q' g)
rewriteQueryM f fe (Limit q i) = do q' <- rewriteQueryM f fe q
                                    rw f fe (Limit q' i)
rewriteQueryM f fe (Offset q i) = do q' <- rewriteQueryM f fe q
                                     rw f fe (Offset q' i)
rewriteQueryM f fe x = rw f fe x

rewriteQuery :: (forall a. Query db a -> Maybe (Query db a)) -> (forall a. QExpr a -> Maybe (QExpr a)) -> Query db a -> Query db a
rewriteQuery f fe x = runIdentity (rewriteQueryM (return . f) (return . fe) x)

traverseQueryM :: Monad m => (forall a. Query db a -> m ()) -> (forall a. QExpr a -> m ()) -> Query db a -> m ()
traverseQueryM t te x = void (rewriteQueryM (\x -> t x >> return Nothing) (\x -> te x >> return Nothing) x)

traverseExprM :: Monad m => (forall a. QExpr a -> m()) -> (forall a. Query db a -> m ()) -> QExpr a -> m ()
traverseExprM te t x = rewriteExprM (\x -> te x >> return Nothing) (\x -> t x >> return Nothing) x >> return ()

rewriteBin :: (Monad m, Show a, Typeable a, Show b, Typeable b) => (forall a . QExpr a -> m (Maybe (QExpr a))) -> (forall a . Query db a -> m (Maybe (Query db a)))
           -> (QExpr a -> QExpr b -> QExpr c) -> QExpr a -> QExpr b -> m (QExpr c)
rewriteBin f fq g a b = do a' <- rewriteExprM f fq a
                           b' <- rewriteExprM f fq b
                           rwE f fq (g a' b')

rewriteExprM :: Monad m => (forall a . QExpr a -> m (Maybe (QExpr a))) -> (forall a . Query db a -> m (Maybe (Query db a))) -> QExpr a -> m (QExpr a)
rewriteExprM f fq (AndE a b) = rewriteBin f fq AndE a b
rewriteExprM f fq (OrE a b) = rewriteBin f fq OrE a b
rewriteExprM f fq (EqE a b) = rewriteBin f fq EqE a b
rewriteExprM f fq (NeqE a b) = rewriteBin f fq NeqE a b
rewriteExprM f fq (LtE a b) = rewriteBin f fq LtE a b
rewriteExprM f fq (LeE a b) = rewriteBin f fq LeE a b
rewriteExprM f fq (GtE a b) = rewriteBin f fq GtE a b
rewriteExprM f fq (GeE a b) = rewriteBin f fq GeE a b
rewriteExprM f fq (NotE a) = do a' <- rewriteExprM f fq a
                                rwE f fq (NotE a')
rewriteExprM f fq (JustE a) = do a' <- rewriteExprM f fq a
                                 rwE f fq (JustE a')
rewriteExprM f fq (IsNothingE a) = do a' <- rewriteExprM f fq a
                                      rwE f fq (IsNothingE a')
rewriteExprM f fq (IsJustE a) = do a' <- rewriteExprM f fq a
                                   rwE f fq (IsJustE a')
rewriteExprM f fq (InE a b) = rewriteBin f fq InE a b
rewriteExprM f fq (ListE as) = do as' <- mapM (rewriteExprM f fq) as
                                  rwE f fq (ListE as')
rewriteExprM f fq (CountE x) = do x' <- rewriteExprM f fq x
                                  rwE f fq (CountE x')
rewriteExprM f fq (MinE x) = do x' <- rewriteExprM f fq x
                                rwE f fq (MinE x')
rewriteExprM f fq (MaxE x) = do x' <- rewriteExprM f fq x
                                rwE f fq (MaxE x')
rewriteExprM f fq (SumE x) = do x' <- rewriteExprM f fq x
                                rwE f fq (SumE x')
rewriteExprM f fq (AverageE x) = do x' <- rewriteExprM f fq x
                                    rwE f fq (AverageE x')
rewriteExprM f fq (RefE i x) = do x' <- rewriteExprM f fq x
                                  rwE f fq (RefE i x')
rewriteExprM f fq x = rwE f fq x

-- * Query optimizations

-- combineProjections :: Query a -> Maybe (Query a)
-- combineProjections (Project (Project q g) f) = Just (Project q (f . g))
-- combineProjections _ = Nothing

combineFilterOpt :: Query db a -> Maybe (Query db a)
combineFilterOpt (Filter (Filter q e1) e2) = Just (Filter q (AndE e1 e2))
combineFilterOpt (Filter q (ValE (SqlBool True))) = Just q
combineFilterOpt (Filter q (ValE (SqlBool False))) = Just EmptySet
combineFilterOpt _ = Nothing

-- | Propagates expressions containing empty sets
propEmptySets :: Query db a -> Maybe (Query db a)
propEmptySets (Join EmptySet _) = Just EmptySet
propEmptySets (Join _ EmptySet) = Just EmptySet
propEmptySets (Filter EmptySet _) = Just EmptySet
propEmptySets (GroupBy EmptySet _) = Just EmptySet
propEmptySets _ = Nothing

-- | Propagates filter clauses that are inside inner joins passed the inner join, so that there is simply one where statement
propFiltersPastInnerJoins :: Query db a -> Maybe (Query db a)
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
booleanOpts (AndE (ValE (SqlBool True)) q) = Just q
booleanOpts (AndE q (ValE (SqlBool True))) = Just q

booleanOpts (OrE q (ValE (SqlBool False))) = Just q
booleanOpts (OrE (ValE (SqlBool False)) q) = Just q
booleanOpts (OrE (ValE (SqlBool True)) (ValE (SqlBool True))) = Just (ValE (SqlBool True))

booleanOpts x = Nothing

allQueryOpts q = combineFilterOpt q <|> propEmptySets q <|> propFiltersPastInnerJoins q -- <|> moveFiltersRight q <|> lowerOnClauses q
allExprOpts e = booleanOpts e
