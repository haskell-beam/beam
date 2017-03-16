{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeApplications #-}
module Database.Beam.Query.Types
    ( Q, QExpr, QExprToIdentity(..), TopLevelQ, IsQuery

    , Projectible(..)

    , Aggregation

    , buildSql92Query, buildSelect ) where

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

-- instance IsQuery TopLevelQ where
--     toQ (TopLevelQ q) = q

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
  forall select projSyntax db s a.
  ( IsSql92SelectSyntax select
  , projSyntax ~ Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax select)

  , Projectible (Sql92ProjectionExpressionSyntax projSyntax) s a ) =>
  Q select db s a -> Int -> (a, Int, Sql92SelectSelectTableSyntax select)
buildSql92Query q curTbl =
  let (res, qb) = runState (runQ q) emptyQb
      emptyQb = QueryBuilder curTbl Nothing Nothing Nothing
      projection = zipWith (\i q -> (q, Just (fromString ("res" <> fromString (show i))))) [0..] (project res)

      sel = selectTableStmt (projExprs projection) (qbFrom qb)
                            (qbWhere qb) (qbGrouping qb) Nothing
  in (res, qbNextTblRef qb, sel)

buildSelect :: IsSql92SelectSyntax syntax =>
               SelectBuilder syntax db s res -> syntax
buildSelect (SelectBuilderSelectSyntax _ select) =
  selectStmt select [] Nothing Nothing
buildSelect (SelectBuilderQ q) =
  let (res, _, select) = buildSql92Query q 0
  in buildSelect (SelectBuilderSelectSyntax res select)
buildSelect (SelectBuilderTopLevel Nothing Nothing [] x) = buildSelect x
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderTopLevel limit' offset' _ x)) =
  buildSelect (SelectBuilderTopLevel (min limit limit') ((+) <$> offset <*> offset' <|> offset <|> offset') ordering x)
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderSelectSyntax _ select)) =
  selectStmt select ordering limit offset
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderQ q)) =
  let (res, _, select) = buildSql92Query q 0
  in buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderSelectSyntax res select))

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
