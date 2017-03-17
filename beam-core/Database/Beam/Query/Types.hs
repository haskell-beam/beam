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
import Control.Monad.Writer

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

andE' :: IsSql92ExpressionSyntax expr =>
         Maybe expr -> Maybe expr -> Maybe expr
andE' Nothing Nothing = Nothing
andE' (Just x) Nothing = Just x
andE' Nothing (Just y) = Just y
andE' (Just x) (Just y) = Just (andE x y)

data QueryBuilder select
  = QueryBuilder
  { qbNextTblRef :: Int
  , qbFrom  :: Maybe (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
  , qbWhere :: Maybe (Sql92SelectExpressionSyntax select) }

data SelectBuilder syntax (db :: (((* -> *) -> *) -> *) -> *) a where
  SelectBuilderQ :: ( IsSql92SelectSyntax syntax
                    , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a ) =>
                    a -> QueryBuilder syntax -> SelectBuilder syntax db a
  SelectBuilderSelectSyntax :: a -> Sql92SelectSelectTableSyntax syntax -> SelectBuilder syntax db a
  SelectBuilderTopLevel ::
    { sbLimit, sbOffset :: Maybe Integer
    , sbOrdering        :: [ Sql92SelectOrderingSyntax syntax ]
    , sbTable           :: SelectBuilder syntax db a } ->
    SelectBuilder syntax db a

fieldNameFunc mkField i = fieldE (mkField ("res" <> fromString (show i)))

defaultProjection :: Projectible expr x =>
                     x -> [ ( expr, Maybe T.Text ) ]
defaultProjection =
    zipWith (\i e -> (e, Just (fromString "res" <> fromString (show (i :: Integer)))))
            [0..] . project

buildSelect :: ( IsSql92SelectSyntax syntax
               , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
               SelectBuilder syntax db a -> syntax
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderSelectSyntax proj table)) =
    selectStmt table ordering limit offset
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderQ proj (QueryBuilder _ from where_))) =
    selectStmt (selectTableStmt (projExprs (defaultProjection proj)) from where_ Nothing Nothing) ordering limit offset
buildSelect x = buildSelect (SelectBuilderTopLevel Nothing Nothing [] x)

selectBuilderToTableSource :: ( Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax syntax)) ~ syntax
                              , IsSql92SelectSyntax syntax
                              , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
                              SelectBuilder syntax db a -> Sql92SelectSelectTableSyntax syntax
selectBuilderToTableSource (SelectBuilderSelectSyntax _ x) = x
selectBuilderToTableSource (SelectBuilderQ x (QueryBuilder _ from where_)) =
  selectTableStmt (projExprs (defaultProjection x)) from where_ Nothing Nothing
selectBuilderToTableSource sb =
    let (x, QueryBuilder _ from where_) = selectBuilderToQueryBuilder sb
    in selectTableStmt (projExprs (defaultProjection x)) from where_ Nothing Nothing

selectBuilderToQueryBuilder :: ( Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax syntax)) ~ syntax
                               , IsSql92SelectSyntax syntax
                               , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
                               SelectBuilder syntax db a -> (a, QueryBuilder syntax)
selectBuilderToQueryBuilder sb =
    let select = buildSelect sb
        x' = reproject (fieldNameFunc (qualifiedField "t0")) (sbProj sb)
    in (x', QueryBuilder 1 (Just (fromTable (tableFromSubSelect select) (Just "t0"))) Nothing)

emptyQb :: QueryBuilder select
emptyQb = QueryBuilder 0 Nothing Nothing

sbProj :: SelectBuilder syntax db a -> a
sbProj (SelectBuilderQ proj _) = proj
sbProj (SelectBuilderSelectSyntax proj _) = proj
sbProj (SelectBuilderTopLevel _ _ _ sb) = sbProj sb

setSelectBuilderProjection :: Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) b =>
                              SelectBuilder syntax db a -> b -> SelectBuilder syntax db b
setSelectBuilderProjection (SelectBuilderQ _ q) proj = SelectBuilderQ proj q
setSelectBuilderProjection (SelectBuilderSelectSyntax _ q) proj = SelectBuilderSelectSyntax proj q
setSelectBuilderProjection (SelectBuilderTopLevel limit offset ord sb) proj =
    SelectBuilderTopLevel limit offset ord (setSelectBuilderProjection sb proj)

limitSelectBuilder, offsetSelectBuilder :: Integer -> SelectBuilder syntax db a -> SelectBuilder syntax db a
limitSelectBuilder limit (SelectBuilderTopLevel limit' offset ordering tbl) =
    SelectBuilderTopLevel (Just $ maybe limit (min limit) limit') offset ordering tbl
limitSelectBuilder limit x = SelectBuilderTopLevel (Just limit) Nothing [] x

offsetSelectBuilder offset (SelectBuilderTopLevel Nothing offset' ordering tbl) =
    SelectBuilderTopLevel Nothing (Just $ offset + fromMaybe 0 offset') ordering tbl
offsetSelectBuilder offset (SelectBuilderTopLevel (Just limit) offset' ordering tbl) =
    SelectBuilderTopLevel (Just $ max 0 (limit - offset)) (Just $ offset + fromMaybe 0 offset') ordering tbl
offsetSelectBuilder offset x = SelectBuilderTopLevel Nothing (Just offset) [] x

buildInnerJoinQuery
    :: forall select db s be table.
       (IsSql92SelectSyntax select) =>
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
    :: forall select db s be table.
       (IsSql92SelectSyntax select) =>
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

projOrder :: Projectible expr x =>
             x -> [ expr ]
projOrder = execWriter . project' (\x -> tell [x] >> pure x)

buildSql92Query ::
    forall select projSyntax db s a.
    ( IsSql92SelectSyntax select
    , Eq (Sql92SelectExpressionSyntax select)
    , projSyntax ~ Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax select)
    , Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax select)) ~ select

    , Sql92ProjectionExpressionSyntax projSyntax ~ Sql92SelectExpressionSyntax select
    , Projectible (Sql92ProjectionExpressionSyntax projSyntax) a ) =>
    Q select db s a -> select
buildSql92Query (Q q) =
    buildSelect (buildQuery (fromF q))
  where
    buildQuery :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                  Free (QF select db s) x
               -> SelectBuilder select db x
    buildQuery (Pure x) = SelectBuilderQ x emptyQb
    buildQuery (Free (QGuard _ next)) = buildQuery next
    buildQuery f@(Free (QAll {})) = buildJoinedQuery f emptyQb
    buildQuery f@(Free (QLeftJoin {})) = buildJoinedQuery f emptyQb

    buildQuery (Free (QLimit limit q next)) =
        let sb = limitSelectBuilder limit (buildQuery (fromF q))
            x = sbProj sb
        -- In the case of limit, we must directly return whatever was given
        in case next x of
             Pure x -> setSelectBuilderProjection sb x

             -- Otherwise, this is going to be part of a join...
             _ -> let (x', qb) = selectBuilderToQueryBuilder sb
                  in buildJoinedQuery (next x') qb

    buildQuery (Free (QOffset offset q next)) =
        let sb = offsetSelectBuilder offset (buildQuery (fromF q))
            x = sbProj sb
        -- In the case of limit, we must directly return whatever was given
        in case next x of
             Pure x -> setSelectBuilderProjection sb x
             -- Otherwise, this is going to be part of a join...
             _ -> let (x', qb) = selectBuilderToQueryBuilder sb
                  in buildJoinedQuery (next x') qb

    buildQuery (Free (QUnion all left right next)) =
      buildTableCombination (unionTables all) left right next
    buildQuery (Free (QIntersect all left right next)) =
      buildTableCombination (intersectTables all) left right next
    buildQuery (Free (QExcept all left right next)) =
      buildTableCombination (exceptTable all) left right next

    buildTableCombination ::
        ( Projectible (Sql92ProjectionExpressionSyntax projSyntax) r
        , Projectible (Sql92ProjectionExpressionSyntax projSyntax) x ) =>
        (Sql92SelectSelectTableSyntax select -> Sql92SelectSelectTableSyntax select -> Sql92SelectSelectTableSyntax select) ->
        QM select db s x -> QM select db s x -> (x -> Free (QF select db s) r) -> SelectBuilder select db r
    buildTableCombination combineTables left right next =
        let leftSb = buildQuery (fromF left)
            leftTb = selectBuilderToTableSource leftSb
            rightTb = selectBuilderToTableSource $ buildQuery (fromF right)

            proj = reproject (fieldNameFunc unqualifiedField) (sbProj leftSb)

            sb = SelectBuilderSelectSyntax proj (combineTables leftTb rightTb)
        in case next proj of
             Pure proj'
               | projOrder proj == projOrder proj' -> setSelectBuilderProjection sb proj'
             _ -> let (x', qb) = selectBuilderToQueryBuilder sb
                  in buildJoinedQuery (next x') qb

    buildJoinedQuery :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                        Free (QF select db s) x -> QueryBuilder select -> SelectBuilder select db x
    buildJoinedQuery (Pure x) qb = SelectBuilderQ x qb
    buildJoinedQuery (Free (QAll tbl on next)) qb =
        let (newTbl, qb') = buildInnerJoinQuery tbl on qb
        in buildJoinedQuery (next newTbl) qb'
    buildJoinedQuery (Free (QLeftJoin tbl on next)) qb =
        let (newTbl, qb') = buildLeftJoinQuery tbl on qb
        in buildJoinedQuery (next newTbl) qb'
    buildJoinedQuery (Free (QGuard cond next)) qb =
        buildJoinedQuery next (qb { qbWhere = andE' (qbWhere qb) (Just cond) })
--    buildQuery (Free (QOffset offset q next)) =
--        let sb = offsetSelectBuilder offset (buildQuery (fromF q))


-- buildSql92Query ::
--   forall select projSyntax db s a.
--   ( IsSql92SelectSyntax select
--   , projSyntax ~ Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax select)

--   , Sql92ProjectionExpressionSyntax projSyntax ~ Sql92SelectExpressionSyntax select
--   , Projectible (Sql92ProjectionExpressionSyntax projSyntax) a ) =>
--   Q select db s a -> select
-- buildSql92Query (Q q) =
--     buildQuery (fromF q) emptyQb
--   where
--     emptyQb = QueryBuilder 0 Nothing Nothing


--     fieldNameFunc mkField i = fieldE (mkField ("res" <> fromString (show i)))

--     defaultProjection :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
--                          x -> [ ( Sql92ProjectionExpressionSyntax projSyntax, Maybe T.Text ) ]
--     defaultProjection =
--         zipWith (\i e -> (e, Just (fromString "res" <> fromString (show (i :: Integer)))))
--                 [0..] . project

--     finishSelectTable' :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
--                           x -> Maybe (Sql92SelectGroupingSyntax select)
--                        -> Maybe (Sql92SelectExpressionSyntax select)
--                        -> QueryBuilder select -> (x, Sql92SelectSelectTableSyntax select)
--     finishSelectTable' a grouping having qb =
--         (a, selectTableStmt (projExprs (defaultProjection a)) (qbFrom qb) (qbWhere qb) grouping having)

--     finishSelectTable :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x => x -> QueryBuilder select -> Sql92SelectSelectTableSyntax select
--     finishSelectTable a qb = snd (finishSelectTable' a Nothing Nothing qb)

--     finishSelect ::  Projectible (Sql92ProjectionExpressionSyntax projSyntax) x => x -> QueryBuilder select -> select
--     finishSelect a qb =
--         selectStmt (finishSelectTable a qb) [] Nothing Nothing

--     buildInnerJoinQuery
--         :: forall s be table.
--            DatabaseTable be db table
--         -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (Sql92SelectExpressionSyntax select))
--         -> QueryBuilder select -> (table (QExpr (Sql92SelectExpressionSyntax select) s), QueryBuilder select)
--     buildInnerJoinQuery (DatabaseTable _ tbl tblSettings) mkOn qb =
--       let qb' = QueryBuilder (tblRef + 1) from' where'
--           tblRef = qbNextTblRef qb
--           newTblNm = "t" <> fromString (show tblRef)
--           newSource = fromTable (tableNamed tbl) (Just newTblNm)
--           (from', where') =
--             case qbFrom qb of
--               Nothing -> (Just newSource, andE' (qbWhere qb) (mkOn newTbl))
--               Just oldFrom -> (Just (innerJoin oldFrom newSource (mkOn newTbl)), qbWhere qb)

--           newTbl = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
--       in (newTbl, qb')
--     buildLeftJoinQuery
--         :: forall s be table.
--            DatabaseTable be db table
--         -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (Sql92SelectExpressionSyntax select))
--         -> QueryBuilder select -> (table (Nullable (QExpr (Sql92SelectExpressionSyntax select) s)), QueryBuilder select)
--     buildLeftJoinQuery (DatabaseTable _ tbl tblSettings) mkOn qb =
--       let qb' = QueryBuilder (tblRef + 1) from' where'
--           tblRef = qbNextTblRef qb
--           newTblNm  = "t" <> fromString (show tblRef)
--           newTbl = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
--           newTblNullable = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
--           newSource = fromTable (tableNamed tbl) (Just newTblNm)
--           (from', where') =
--             case qbFrom qb of
--               Nothing -> (Just newSource, andE' (qbWhere qb) (mkOn newTbl))
--               Just oldFrom -> (Just (leftJoin oldFrom newSource (mkOn newTbl)), qbWhere qb)
--       in (newTblNullable, qb')

--     buildQuery :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
--                   Free (QF select db s) x -> QueryBuilder select -> select
--     buildQuery (Pure a) qb = finishSelect a qb
--     buildQuery (Free (QAll tbl on next)) qb =
--       let (newTblNm, qb') = buildInnerJoinQuery tbl on qb
--       in buildQuery (next newTblNm) qb'
--     buildQuery (Free (QLeftJoin tbl on next)) qb =
--       let (newTblNm, qb') = buildLeftJoinQuery tbl on qb
--       in buildQuery (next newTblNm) qb'
--     buildQuery (Free (QGuard cond next)) qb =
--       buildQuery next (qb { qbWhere = andE' (qbWhere qb) (Just cond) })

--     If the next steps are guard expressions, then we can use HAVING
--     buildQuery (Free (QAggregate grouping underlying next)) qb =
--         let (a, qb') = buildQueryInQ (fromF underlying) emptyQb
--         in case buildAggregateSimple grouping Nothing qb' (next a) of
--              Nothing -> error "buildQuery: aggregate too complicated"
--              Just select -> select

--     TODO we should examine underlying to see if we can simplify
--     buildQuery (Free (QLimit limit underlying next)) qb =
--         let (a, qb') = buildQueryInQ (fromF underlying) emptyQb
--         in case buildLimitOffsetSimple (Just limit) Nothing qb' (next a) of
--              Nothing -> error "buildQuery: limit too complicated"
--              Just select -> select
--     buildQuery (Free (QOffset offset underlying next)) qb =
--         case fromF underlying of
--           Free (QOffset offset' underlying' next) -> 
--          let (a, qb') = buildQueryInQ (fromF underlying) emptyQb
--         in case buildLimitOffsetSimple Nothing (Just offset) qb' (next a) of
--              Nothing -> error "buildQuery: limit too complicated"
--              Just select -> select

--     buildLimitOffsetSimple :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
--                               Maybe Integer -> Maybe Integer -> QueryBuilder select
--                            -> Free (QF select db s) x -> Maybe select
--     buildLimitOffsetSimple limit offset qb (Pure x) =
--         let selectTable = finishSelectTable x qb
--         in Just (selectStmt selectTable [] limit offset)
--     buildLimitOffsetSimple limit offset qb _ = error "buildLimitOffsetSimple: unhandled"

--     buildQueryInQ :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
--                      Free (QF select db s) x -> QueryBuilder select
--                   -> (x, QueryBuilder select)
--     buildQueryInQ (Pure x) qb = (x, qb)
--     buildQueryInQ (Free (QAll tbl on next)) qb =
--       let (newTblNm, qb') = buildInnerJoinQuery tbl on qb
--       in buildQueryInQ (next newTblNm) qb'
--     buildQueryInQ (Free (QLeftJoin tbl on next)) qb =
--       let (newTblNm, qb') = buildLeftJoinQuery tbl on qb
--       in buildQueryInQ (next newTblNm) qb'
--     buildQueryInQ _ qb = error "buildQueryInQ: unhandled"

--     buildAggregateSimple :: Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
--                             Sql92SelectGroupingSyntax select -> Maybe (Sql92SelectExpressionSyntax select)
--                          -> QueryBuilder select -> Free (QF select db s) x -> Maybe select
--     buildAggregateSimple grouping having qb (Pure x) =
--       let (_, selectTable) = finishSelectTable' x (Just grouping) having qb
--       in Just (selectStmt selectTable [] Nothing Nothing)
--     buildAggregateSimple grouping having qb (Free (QGuard cond' next)) =
--       buildAggregateSimple grouping (andE' having (Just cond')) qb next
--     buildAggregateSimple grouping having qb _ = Nothing

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
