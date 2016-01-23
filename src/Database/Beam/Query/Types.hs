{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Types
    ( Q(..), QExpr(..), QExprToIdentity(..), TopLevelQ, IsQuery

    , Projectible(..)

    , Aggregation(..)

    , queryToSQL'

    , rewriteExprM, allExprOpts, mkSqlExpr, optimizeExpr ) where

import Database.Beam.Query.Internal

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Fields
import Database.Beam.SQL
import Database.HDBC

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer hiding (All)
import Control.Monad.Identity

import Data.Monoid hiding (All)
import Data.Proxy
import Data.Coerce
import Data.Data
import Data.Maybe
import Data.String
import qualified Data.Text as T

import Unsafe.Coerce

-- * Beam queries
instance IsString (QExpr T.Text) where
    fromString = ValE . SqlString

type family QExprToIdentity x
type instance QExprToIdentity (table QExpr) = table Identity
type instance QExprToIdentity (QExpr a) = a
type instance QExprToIdentity (a, b) = (QExprToIdentity a, QExprToIdentity b)
type instance QExprToIdentity (a, b, c) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c)

instance IsQuery Q where
    toQ = id
instance IsQuery TopLevelQ where
    toQ (TopLevelQ q) = q

-- * Aggregations

data Aggregation a = GroupAgg (QExpr a)
                   | GenericAgg T.Text [GenQExpr]

-- * Rewriting and optimization

rwE :: Monad m => (forall a. QExpr a -> m (Maybe (QExpr a))) -> QExpr a -> m (QExpr a)
rwE f x = do x' <- f x
             case x' of
               Nothing -> return x
               Just x'
                   | x == x' -> return x
                   | otherwise -> rewriteExprM f x'

rewriteBin :: (Monad m, Typeable a, Typeable b) => (forall a . QExpr a -> m (Maybe (QExpr a)))
           -> (QExpr a -> QExpr b -> QExpr c) -> QExpr a -> QExpr b -> m (QExpr c)
rewriteBin f g a b = do a' <- rewriteExprM f a
                        b' <- rewriteExprM f b
                        rwE f (g a' b')

rewriteExprM :: Monad m => (forall a . QExpr a -> m (Maybe (QExpr a))) -> QExpr a -> m (QExpr a)
rewriteExprM f (AndE a b) = rewriteBin f AndE a b
rewriteExprM f (OrE a b) = rewriteBin f OrE a b
rewriteExprM f (EqE a b) = rewriteBin f EqE a b
rewriteExprM f (NeqE a b) = rewriteBin f NeqE a b
rewriteExprM f (LtE a b) = rewriteBin f LtE a b
rewriteExprM f (LeE a b) = rewriteBin f LeE a b
rewriteExprM f (GtE a b) = rewriteBin f GtE a b
rewriteExprM f (GeE a b) = rewriteBin f GeE a b
-- rewriteExprM f (JustE a) = do a' <- rewriteExprM f a
--                               rwE f (JustE a')
-- rewriteExprM f (IsNothingE a) = do a' <- rewriteExprM f a
--                                    rwE f (IsNothingE a')
-- rewriteExprM f (IsJustE a) = do a' <- rewriteExprM f a
--                                 rwE f (IsJustE a')
-- rewriteExprM f (InE a b) = rewriteBin f InE a b
-- rewriteExprM f (ListE as) = do as' <- mapM (rewriteExprM f fq) as
--                                rwE f (ListE as')
-- rewriteExprM f (CountE x) = do x' <- rewriteExprM f x
--                                rwE f (CountE x')
-- rewriteExprM f (MinE x) = do x' <- rewriteExprM f x
--                              rwE f (MinE x')
-- rewriteExprM f (MaxE x) = do x' <- rewriteExprM f x
--                              rwE f (MaxE x')
-- rewriteExprM f (SumE x) = do x' <- rewriteExprM f x
--                              rwE f (SumE x')
-- rewriteExprM f (AverageE x) = do x' <- rewriteExprM f x
--                                  rwE f (AverageE x')
-- rewriteExprM f (RefE i x) = do x' <- rewriteExprM f x
--                                rwE f (RefE i x')
rewriteExprM f x = rwE f x

booleanOpts :: QExpr a -> Maybe (QExpr a)
booleanOpts (AndE (ValE (SqlBool False)) _) = Just (ValE (SqlBool False))
booleanOpts (AndE _ (ValE (SqlBool False))) = Just (ValE (SqlBool False))
booleanOpts (AndE (ValE (SqlBool True)) q) = Just q
booleanOpts (AndE q (ValE (SqlBool True))) = Just q

booleanOpts (OrE q (ValE (SqlBool False))) = Just q
booleanOpts (OrE (ValE (SqlBool False)) q) = Just q
booleanOpts (OrE (ValE (SqlBool True)) (ValE (SqlBool True))) = Just (ValE (SqlBool True))

booleanOpts x = Nothing

allExprOpts e = booleanOpts e

optimizeExpr :: QExpr a -> SQLExpr
optimizeExpr = mkSqlExpr . runIdentity . rewriteExprM (return . allExprOpts)

mkSqlExpr :: QExpr a -> SQLExpr
mkSqlExpr (FieldE tblName (Just tblOrd) fieldName) = SQLFieldE (SQLQualifiedFieldName fieldName ("t" <> fromString (show tblOrd)))
mkSqlExpr (FieldE tblName Nothing fieldName) = SQLFieldE (SQLFieldName fieldName)
mkSqlExpr (OrE a b) = SQLOrE (mkSqlExpr a) (mkSqlExpr b)
mkSqlExpr (AndE a b) = SQLAndE (mkSqlExpr a) (mkSqlExpr b)
mkSqlExpr (EqE a b) = SQLEqE (mkSqlExpr a) (mkSqlExpr b)
mkSqlExpr (NeqE a b) = SQLNeqE (mkSqlExpr a) (mkSqlExpr b)
mkSqlExpr (LtE a b) = SQLLtE (mkSqlExpr a) (mkSqlExpr b)
mkSqlExpr (GtE a b) = SQLGtE (mkSqlExpr a) (mkSqlExpr b)
mkSqlExpr (LeE a b) = SQLLeE (mkSqlExpr a) (mkSqlExpr b)
mkSqlExpr (GeE a b) = SQLGeE (mkSqlExpr a) (mkSqlExpr b)
mkSqlExpr (ValE v) = SQLValE v
mkSqlExpr (NotE v) = SQLNotE (mkSqlExpr v)
mkSqlExpr (FuncE f args) = SQLFuncE f (map (\(GenQExpr e) -> mkSqlExpr e) args)

queryToSQL' :: Projectible a => Q db s a -> (a, SQLSelect)
queryToSQL' q = let (res, qb) = runState (runQ q) emptyQb
                    emptyQb = QueryBuilder 0 Nothing (ValE (SqlBool True)) Nothing Nothing [] Nothing
                    projection = map (\(GenQExpr q) -> SQLAliased (optimizeExpr q) Nothing) (project res)

                    sel = SQLSelect
                          { selProjection = SQLProj projection
                          , selFrom = qbFrom qb
                          , selWhere = optimizeExpr (qbWhere qb)
                          , selGrouping = qbGrouping qb
                          , selOrderBy = qbOrdering qb
                          , selLimit = qbLimit qb
                          , selOffset = qbOffset qb }
                in (res, sel)
