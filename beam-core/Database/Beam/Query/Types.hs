{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeApplications #-}
module Database.Beam.Query.Types
    ( Q, QExpr, QExprToIdentity(..)

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
import Control.Monad.Free.Church
import Control.Monad.Free

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

  , Sql92ProjectionExpressionSyntax projSyntax ~ Sql92SelectExpressionSyntax select
  , Projectible (Sql92ProjectionExpressionSyntax projSyntax) a ) =>
  Q select db s a -> select
buildSql92Query (Q q) =
    buildQuery (fromF q) emptyQb
  where
    emptyQb = QueryBuilder 0 Nothing Nothing

    andE' :: Maybe (Sql92SelectExpressionSyntax select) -> Maybe (Sql92SelectExpressionSyntax select)
          -> Maybe (Sql92SelectExpressionSyntax select)
    andE' Nothing Nothing = Nothing
    andE' (Just x) Nothing = Just x
    andE' Nothing (Just y) = Just y
    andE' (Just x) (Just y) = Just (andE x y)

    fieldNameFunc mkField i = fieldE (mkField ("res" <> fromString (show i)))

    defaultProjection :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                         x -> [ ( Sql92ProjectionExpressionSyntax projSyntax, Maybe T.Text ) ]
    defaultProjection =
        zipWith (\i e -> (e, Just (fromString "res" <> fromString (show (i :: Integer)))))
                [0..] . project

    finishSelectTable' :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                          x -> Maybe (Sql92SelectGroupingSyntax select)
                       -> Maybe (Sql92SelectExpressionSyntax select)
                       -> QueryBuilder select -> (x, Sql92SelectSelectTableSyntax select)
    finishSelectTable' a grouping having qb =
        (a, selectTableStmt (projExprs (defaultProjection a)) (qbFrom qb) (qbWhere qb) grouping having)

    finishSelectTable :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x => x -> QueryBuilder select -> Sql92SelectSelectTableSyntax select
    finishSelectTable a qb = snd (finishSelectTable' a Nothing Nothing qb)

    finishSelect ::  Projectible (Sql92ProjectionExpressionSyntax projSyntax) x => x -> QueryBuilder select -> select
    finishSelect a qb =
        selectStmt (finishSelectTable a qb) [] Nothing Nothing

    buildInnerJoinQuery
        :: forall s be table.
           DatabaseTable be db table
        -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (Sql92SelectExpressionSyntax select))
        -> QueryBuilder select -> (table (QExpr (Sql92SelectExpressionSyntax select) s), QueryBuilder select)
    buildInnerJoinQuery (DatabaseTable _ tbl tblSettings) mkOn qb =
      let qb' = QueryBuilder (tblRef + 1) from' where'
          tblRef = qbNextTblRef qb
          newTblNm = "t" <> fromString (show tblRef)
          newSource = fromTable (tableNamed tbl) (Just newTblNm)
          (from', where') =
            case qbFrom qb of
              Nothing -> (Just newSource, andE' (qbWhere qb) (mkOn newTbl))
              Just oldFrom -> (Just (innerJoin oldFrom newSource (mkOn newTbl)), qbWhere qb)

          newTbl = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
      in (newTbl, qb')
    buildLeftJoinQuery
        :: forall s be table.
           DatabaseTable be db table
        -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (Sql92SelectExpressionSyntax select))
        -> QueryBuilder select -> (table (Nullable (QExpr (Sql92SelectExpressionSyntax select) s)), QueryBuilder select)
    buildLeftJoinQuery (DatabaseTable _ tbl tblSettings) mkOn qb =
      let qb' = QueryBuilder (tblRef + 1) from' where'
          tblRef = qbNextTblRef qb
          newTblNm  = "t" <> fromString (show tblRef)
          newTbl = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
          newTblNullable = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
          newSource = fromTable (tableNamed tbl) (Just newTblNm)
          (from', where') =
            case qbFrom qb of
              Nothing -> (Just newSource, andE' (qbWhere qb) (mkOn newTbl))
              Just oldFrom -> (Just (leftJoin oldFrom newSource (mkOn newTbl)), qbWhere qb)
      in (newTblNullable, qb')

    buildQuery :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                  Free (QF select db s) x -> QueryBuilder select -> select
    buildQuery (Pure a) qb = finishSelect a qb
    buildQuery (Free (QAll tbl on next)) qb =
      let (newTblNm, qb') = buildInnerJoinQuery tbl on qb
      in buildQuery (next newTblNm) qb'
    buildQuery (Free (QLeftJoin tbl on next)) qb =
      let (newTblNm, qb') = buildLeftJoinQuery tbl on qb
      in buildQuery (next newTblNm) qb'
    buildQuery (Free (QGuard cond next)) qb =
      buildQuery next (qb { qbWhere = andE' (qbWhere qb) (Just cond) })

    -- If the next steps are guard expressions, then we can use HAVING
    buildQuery (Free (QAggregate grouping underlying next)) qb =
        let (a, qb') = buildQueryInQ (fromF underlying) emptyQb
        in case buildAggregateSimple grouping Nothing qb' (next a) of
             Nothing -> error "buildQuery: aggregate too complicated"
             Just select -> select

    -- TODO we should examine underlying to see if we can simplify
    buildQuery (Free (QLimit limit underlying next)) qb =
        let (a, qb') = buildQueryInQ (fromF underlying) emptyQb
        in case buildLimitOffsetSimple (Just limit) Nothing qb' (next a) of
             Nothing -> error "buildQuery: limit too complicated"
             Just select -> select
    buildQuery (Free (QOffset offset underlying next)) qb =
        case fromF underlying of
          Free (QOffset offset' underlying' next) -> 
        let (a, qb') = buildQueryInQ (fromF underlying) emptyQb
        in case buildLimitOffsetSimple Nothing (Just offset) qb' (next a) of
             Nothing -> error "buildQuery: limit too complicated"
             Just select -> select

    buildLimitOffsetSimple :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                              Maybe Integer -> Maybe Integer -> QueryBuilder select
                           -> Free (QF select db s) x -> Maybe select
    buildLimitOffsetSimple limit offset qb (Pure x) =
        let selectTable = finishSelectTable x qb
        in Just (selectStmt selectTable [] limit offset)
    buildLimitOffsetSimple limit offset qb _ = error "buildLimitOffsetSimple: unhandled"

    buildQueryInQ :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                     Free (QF select db s) x -> QueryBuilder select
                  -> (x, QueryBuilder select)
    buildQueryInQ (Pure x) qb = (x, qb)
    buildQueryInQ (Free (QAll tbl on next)) qb =
      let (newTblNm, qb') = buildInnerJoinQuery tbl on qb
      in buildQueryInQ (next newTblNm) qb'
    buildQueryInQ (Free (QLeftJoin tbl on next)) qb =
      let (newTblNm, qb') = buildLeftJoinQuery tbl on qb
      in buildQueryInQ (next newTblNm) qb'
    buildQueryInQ _ qb = error "buildQueryInQ: unhandled"

    buildAggregateSimple :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                            Sql92SelectGroupingSyntax select -> Maybe (Sql92SelectExpressionSyntax select)
                         -> QueryBuilder select -> Free (QF select db s) x -> Maybe select
    buildAggregateSimple grouping having qb (Pure x) =
      let (_, selectTable) = finishSelectTable' x (Just grouping) having qb
      in Just (selectStmt selectTable [] Nothing Nothing)
    buildAggregateSimple grouping having qb (Free (QGuard cond' next)) =
      buildAggregateSimple grouping (andE' having (Just cond')) qb next
    buildAggregateSimple grouping having qb _ = Nothing

    -- buildQuery (QAggregate grouping underlying next) =5
    --   buildAggregate next
    --   joinSubquery (evalUnderlying underlying) next
    -- buildQuery (QUnion all left right) =
    --   joinSubquery (selectFromSource (unionTables all (evalUnderlying left) (evalUnderlying right))) next
    -- buildQuery (QIntersect all left right) =
    --   joinSubquery (selectFromSource (intersectTables all (evalUnderlying left) (evalUnderlying right)))
    -- buildQuery (QExcept all left right) =
    --   joinSubquery (selectFromSource (intersectTables all (evalUnderlying left) (evalUnderlying right)))


  -- let (res, qb) = runState (runQ q) emptyQb
  --     emptyQb = QueryBuilder curTbl Nothing Nothing Nothing
  --     projection = zipWith (\i q -> (q, Just (fromString ("res" <> fromString (show i))))) [0..] (project res)

  --     sel = selectTableStmt (projExprs projection) (qbFrom qb)
  --                           (qbWhere qb) (qbGrouping qb) Nothing
  -- in (res, qbNextTblRef qb, sel)

-- buildSelect :: IsSql92SelectSyntax syntax =>
--                SelectBuilder syntax db s res -> syntax
-- buildSelect (SelectBuilderSelectSyntax _ select) =
--   selectStmt select [] Nothing Nothing
-- buildSelect (SelectBuilderQ q) =
--   let (res, _, select) = buildSql92Query q 0
--   in buildSelect (SelectBuilderSelectSyntax res select)
-- buildSelect (SelectBuilderTopLevel Nothing Nothing [] x) = buildSelect x
-- buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderTopLevel limit' offset' _ x)) =
--   buildSelect (SelectBuilderTopLevel (min limit limit') ((+) <$> offset <*> offset' <|> offset <|> offset') ordering x)
-- buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderSelectSyntax _ select)) =
--   selectStmt select ordering limit offset
-- buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderQ q)) =
--   let (res, _, select) = buildSql92Query q 0
--   in buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderSelectSyntax res select))

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
