{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Types
    ( Q, QExpr, QExprToIdentity(..), TopLevelQ, IsQuery

    , Projectible(..)

    , Aggregation(..)

    , queryToSQL'

    , allExprOpts, mkSqlField, optimizeExpr, optimizeExpr' ) where

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
import Data.Generics.Uniplate.Data

import Unsafe.Coerce

-- * Beam queries

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

data Aggregation a = GroupAgg (SQLExpr' QField)
                   | GenericAgg T.Text [SQLExpr' QField]

-- * Rewriting and optimization

-- rwE :: Monad m => (forall a. QExpr a -> m (Maybe (QExpr a))) -> QExpr a -> m (QExpr a)
-- rwE f x = do x' <- f x
--              case x' of
--                Nothing -> return x
--                Just x'
--                    | x == x' -> return x
--                    | otherwise -> rewriteExprM f x'

-- rewriteExprM :: Monad m => (forall a . QExpr a -> m (Maybe (QExpr a))) -> QExpr a -> m (QExpr a)
-- rewriteExprM f (BinOpE op a b) =
--     do a' <- rewriteExprM f a
--        b' <- rewriteExprM f b
--        rwE f (BinOpE op a' b')
-- rewriteExprM f (UnOpE op a) =
--     do a' <- rewriteExprM f a
--        rwE f (UnOpE op a')
-- -- rewriteExprM f (JustE a) = do a' <- rewriteExprM f a
-- --                               rwE f (JustE a')
-- rewriteExprM f (IsNothingE a) = do a' <- rewriteExprM f a
--                                    rwE f (IsNothingE a')
-- rewriteExprM f (IsJustE a) = do a' <- rewriteExprM f a
--                                 rwE f (IsJustE a')
-- -- rewriteExprM f (InE a b) = rewriteBin f InE a b
-- -- rewriteExprM f (ListE as) = do as' <- mapM (rewriteExprM f fq) as
-- --                                rwE f (ListE as')
-- rewriteExprM f x = rwE f x

booleanOpts :: SQLExpr -> Maybe SQLExpr
booleanOpts (SQLBinOpE "AND" (SQLValE (SqlBool False)) _) = Just (SQLValE (SqlBool False))
booleanOpts (SQLBinOpE "AND" _ (SQLValE (SqlBool False))) = Just (SQLValE (SqlBool False))
booleanOpts (SQLBinOpE "AND" (SQLValE (SqlBool True)) q) = Just q
booleanOpts (SQLBinOpE "AND" q (SQLValE (SqlBool True))) = Just q

booleanOpts (SQLBinOpE "OR" q (SQLValE (SqlBool False))) = Just q
booleanOpts (SQLBinOpE "OR" (SQLValE (SqlBool False)) q) = Just q
booleanOpts (SQLBinOpE "OR" (SQLValE (SqlBool True)) (SQLValE (SqlBool True))) = Just (SQLValE (SqlBool True))

booleanOpts x = Nothing

allExprOpts e = pure (booleanOpts e)

optimizeExpr' :: SQLExpr' QField -> SQLExpr
optimizeExpr' = runIdentity . rewriteM allExprOpts . fmap mkSqlField
optimizeExpr :: QExpr a -> SQLExpr
optimizeExpr (QExpr e) = optimizeExpr' e

mkSqlField :: QField -> SQLFieldName
mkSqlField (QField tblName (Just tblOrd) fieldName) = SQLQualifiedFieldName fieldName ("t" <> fromString (show tblOrd))
mkSqlField (QField tblName Nothing fieldName) = SQLFieldName fieldName

queryToSQL' :: Projectible a => Q db s a -> (a, SQLSelect)
queryToSQL' q = let (res, qb) = runState (runQ q) emptyQb
                    emptyQb = QueryBuilder 0 Nothing (QExpr (SQLValE (SqlBool True))) Nothing Nothing [] Nothing
                    projection = map (\q -> SQLAliased (optimizeExpr' q) Nothing) (project res)

                    sel = SQLSelect
                          { selProjection = SQLProj projection
                          , selFrom = qbFrom qb
                          , selWhere = optimizeExpr (qbWhere qb)
                          , selGrouping = qbGrouping qb
                          , selOrderBy = qbOrdering qb
                          , selLimit = qbLimit qb
                          , selOffset = qbOffset qb }
                in (res, sel)
