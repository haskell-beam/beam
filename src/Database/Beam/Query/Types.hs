{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Types
    ( Q, QExpr, QExprToIdentity(..), TopLevelQ, IsQuery

    , Projectible(..)

    , Aggregation

    , queryToSQL'

    , optimizeExpr, optimizeExpr' ) where

import Database.Beam.Query.Internal

import Database.Beam.Internal
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
type instance QExprToIdentity (table (QExpr be s)) = table Identity
type instance QExprToIdentity (table (Nullable c)) = Maybe (QExprToIdentity (table c))
type instance QExprToIdentity (QExpr be s a) = a
type instance QExprToIdentity (a, b) = (QExprToIdentity a, QExprToIdentity b)
type instance QExprToIdentity (a, b, c) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c)
type instance QExprToIdentity (a, b, c, d) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d)
type instance QExprToIdentity (a, b, c, d, e) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e)

instance IsQuery Q where
    toQ = id
instance IsQuery TopLevelQ where
    toQ (TopLevelQ q) = q

-- * Rewriting and optimization

booleanOpts :: BeamBackend be => SQLExpr be -> Maybe (SQLExpr be)
booleanOpts (SQLBinOpE "AND" (SQLValE false) _) | False <- backendAsBool false = Just (SQLValE (sqlBool False))
booleanOpts (SQLBinOpE "AND" _ (SQLValE false)) | False <- backendAsBool false = Just (SQLValE (sqlBool False))
booleanOpts (SQLBinOpE "AND" (SQLValE true) q) | True <- backendAsBool true = Just q
booleanOpts (SQLBinOpE "AND" q (SQLValE true)) | True <- backendAsBool true = Just q

booleanOpts (SQLBinOpE "OR" q (SQLValE false)) | False <- backendAsBool false = Just q
booleanOpts (SQLBinOpE "OR" (SQLValE false) q) | False <- backendAsBool false = Just q
booleanOpts (SQLBinOpE "OR" (SQLValE true1) (SQLValE true2))
    | True <- backendAsBool true1,
      True <- backendAsBool true2 = Just (SQLValE (sqlBool True))

booleanOpts x = Nothing

-- | Rewrite function to optimize an expression, will return `Nothing`
-- if the current expression cannot be rewritten any further. Suitable
-- to be passed to `rewriteM`.
allExprOpts e = pure (booleanOpts e)

-- | Given a `SQLExpr' QField` optimize the expression and turn it into a `SQLExpr`.
optimizeExpr' :: BeamBackend be => SQLExpr' be QField -> SQLExpr be
optimizeExpr' = runIdentity . rewriteM allExprOpts . fmap mkSqlField

-- | Optimize a `QExpr` and turn it in into a `SQLExpr`.
optimizeExpr :: BeamBackend be => QExpr be s a -> SQLExpr be
optimizeExpr (QExpr e) = optimizeExpr' e

mkSqlField :: QField -> SQLFieldName
mkSqlField (QField tblName (Just tblOrd) fieldName) = SQLQualifiedFieldName fieldName ("t" <> fromString (show tblOrd))
mkSqlField (QField tblName Nothing fieldName) = SQLFieldName fieldName

-- | Turn a `Q` into a `SQLSelect` starting the table references at the given number
queryToSQL' :: (BeamBackend be, Projectible be a) => Q be db s a -> Int -> (a, Int, SQLSelect be)
queryToSQL' q curTbl = let (res, qb) = runState (runQ q) emptyQb
                           emptyQb = QueryBuilder curTbl Nothing (SQLValE (sqlBool True)) Nothing Nothing [] Nothing
                           projection = map (\q -> SQLAliased (optimizeExpr' q) Nothing) (project res)

                           sel = SQLSelect
                                 { selProjection = SQLProj projection
                                 , selFrom = qbFrom qb
                                 , selWhere = optimizeExpr' (qbWhere qb)
                                 , selGrouping = qbGrouping qb
                                 , selOrderBy = qbOrdering qb
                                 , selLimit = qbLimit qb
                                 , selOffset = qbOffset qb }
                       in (res, qbNextTblRef qb, sel)
