{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeApplications #-}
module Database.Beam.Query.Types
    ( Q, QExpr, QExprToIdentity(..), TopLevelQ, IsQuery

    , Projectible(..)

    , Aggregation

    , buildSql92Query ) where

import Database.Beam.Query.Internal
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL92

import Database.Beam.Backend.Types
import Database.Beam.Schema.Tables

import Control.Applicative
import Control.Monad.State
import Control.Monad.Identity

import Data.Monoid hiding (All)
import Data.Proxy
import Data.Maybe
import Data.String
import qualified Data.Text as T

-- * Beam queries

type family QExprToIdentity x
type instance QExprToIdentity (table (QExpr syntax s)) = table Identity
type instance QExprToIdentity (table (Nullable c)) = Maybe (QExprToIdentity (table c))
type instance QExprToIdentity (QExpr syntax s a) = a
type instance QExprToIdentity ()     = ()
type instance QExprToIdentity (a, b) = (QExprToIdentity a, QExprToIdentity b)
type instance QExprToIdentity (a, b, c) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c)
type instance QExprToIdentity (a, b, c, d) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d)
type instance QExprToIdentity (a, b, c, d, e) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e)

instance IsQuery Q where
    toQ = id
instance IsQuery TopLevelQ where
    toQ (TopLevelQ q) = q

-- * Rewriting and optimization

-- -- | Given a `SQLExpr' QField` optimize the expression and turn it into a `SQLExpr`.
-- optimizeExpr' :: BeamSqlBackend be => SQLExpr' be QField -> SQLExpr be
-- optimizeExpr' = runIdentity . rewriteM sqlExprOptimizations . fmap mkSqlField
-- -- | Optimize a `QExpr` and turn it in into a `SQLExpr`.
-- optimizeExpr :: BeamSqlBackend be => QExpr be s a -> SQLExpr be
-- optimizeExpr (QExpr e) = optimizeExpr' e

-- mkSqlField :: QField -> SQLFieldName
-- mkSqlField (QField tblName (Just tblOrd) fieldName) = SQLQualifiedFieldName fieldName ("t" <> fromString (show tblOrd))
-- mkSqlField (QField tblName Nothing fieldName) = SQLFieldName fieldName

buildSql92Query ::
  forall select exprSyntax valueSyntax projSyntax db s a.
  ( IsSql92SelectSyntax select

  , exprSyntax ~ Sql92SelectExpressionSyntax select
  , IsSql92ExpressionSyntax exprSyntax

  , valueSyntax ~ Sql92ExpressionValueSyntax exprSyntax
  , HasSqlValueSyntax valueSyntax Bool

  , projSyntax ~ Sql92SelectProjectionSyntax select
  , IsSql92ProjectionSyntax projSyntax

  , Projectible (Sql92ProjectionExpressionSyntax projSyntax) a ) =>
  Q select db s a -> Int -> (a, Int, select)
buildSql92Query q curTbl =
  let (res, qb) = runState (runQ q) emptyQb
      emptyQb = QueryBuilder curTbl Nothing (valueE (sqlValueSyntax True))
                             Nothing Nothing [] Nothing
      projection = map (\q -> (q, Nothing)) (project res)

      sel = selectStmt (projExprs projection) (qbFrom qb)
                       (qbWhere qb) (qbGrouping qb)
                       (qbOrdering qb) (qbLimit qb) (qbOffset qb)
  in (res, qbNextTblRef qb, sel)

-- -- | Turn a `Q` into a `SQLSelect` starting the table references at the given number
-- queryToSQL' :: (BeamSqlBackend be, Projectible be a) =>
--   Q be db s a -> Int -> (a, Int, SQLSelect be)
-- queryToSQL' q curTbl = let (res, qb) = runState (runQ q) emptyQb
--                            emptyQb = QueryBuilder curTbl Nothing (SQLValE (SQLValue True)) Nothing Nothing [] Nothing
--                            projection = map (\q -> SQLAliased (optimizeExpr' q) Nothing) (project res)

--                            sel = SQLSelect
--                                  { selProjection = SQLProj projection
--                                  , selFrom = qbFrom qb
--                                  , selWhere = optimizeExpr' (qbWhere qb)
--                                  , selGrouping = qbGrouping qb
--                                  , selOrderBy = qbOrdering qb
--                                  , selLimit = qbLimit qb
--                                  , selOffset = qbOffset qb }
--                        in (res, qbNextTblRef qb, sel)
