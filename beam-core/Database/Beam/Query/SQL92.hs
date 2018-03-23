{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Query.SQL92
    ( buildSql92Query' ) where

import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL

import           Database.Beam.Schema.Tables

import           Control.Monad.Free.Church
import           Control.Monad.Free
import           Control.Monad.Writer

import           Data.Maybe
import           Data.String
import qualified Data.Text as T

-- * Beam queries

andE' :: IsSql92ExpressionSyntax expr =>
         Maybe expr -> Maybe expr -> Maybe expr
andE' Nothing Nothing = Nothing
andE' (Just x) Nothing = Just x
andE' Nothing (Just y) = Just y
andE' (Just x) (Just y) = Just (andE x y)

newtype PreserveLeft a b = PreserveLeft { unPreserveLeft :: (a, b) }
instance ProjectibleWithPredicate c syntax b => ProjectibleWithPredicate c syntax (PreserveLeft a b) where
  project' p f (PreserveLeft (a, b)) =
    PreserveLeft . (a,) <$> project' p f b

type SelectStmtFn select
  =  Sql92SelectSelectTableSyntax select
 -> [Sql92SelectOrderingSyntax select]
 -> Maybe Integer {-^ LIMIT -}
 -> Maybe Integer {-^ OFFSET -}
 -> select

data QueryBuilder select
  = QueryBuilder
  { qbNextTblRef :: Int
  , qbFrom  :: Maybe (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
  , qbWhere :: Maybe (Sql92SelectExpressionSyntax select) }

data SelectBuilder syntax (db :: (* -> *) -> *) a where
  SelectBuilderQ :: ( IsSql92SelectSyntax syntax
                    , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a ) =>
                    a -> QueryBuilder syntax -> SelectBuilder syntax db a
  SelectBuilderGrouping
      :: ( IsSql92SelectSyntax syntax
         , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a )
      => a -> QueryBuilder syntax
      -> Maybe (Sql92SelectGroupingSyntax syntax)
      -> Maybe (Sql92SelectExpressionSyntax syntax)
      -> Maybe (Sql92SelectTableSetQuantifierSyntax (Sql92SelectSelectTableSyntax syntax))
      -> SelectBuilder syntax db a
  SelectBuilderSelectSyntax :: Bool {- Whether or not this contains UNION, INTERSECT, EXCEPT, etc -}
                            -> a -> Sql92SelectSelectTableSyntax syntax
                            -> SelectBuilder syntax db a
  SelectBuilderTopLevel ::
    { sbLimit, sbOffset :: Maybe Integer
    , sbOrdering        :: [ Sql92SelectOrderingSyntax syntax ]
    , sbTable           :: SelectBuilder syntax db a
    , sbSelectFn        :: Maybe (SelectStmtFn syntax)
                        -- ^ Which 'SelectStmtFn' to use to build this select. If 'Nothing', use the default
    } -> SelectBuilder syntax db a

sbContainsSetOperation :: SelectBuilder syntax db a -> Bool
sbContainsSetOperation (SelectBuilderSelectSyntax contains _ _) = contains
sbContainsSetOperation (SelectBuilderTopLevel { sbTable = tbl }) = sbContainsSetOperation tbl
sbContainsSetOperation _ = False

fieldNameFunc :: IsSql92ExpressionSyntax expr =>
                 (T.Text -> Sql92ExpressionFieldNameSyntax expr) -> Int
              -> expr
fieldNameFunc mkField i = fieldE (mkField ("res" <> fromString (show i)))

nextTblPfx :: TablePrefix -> TablePrefix
nextTblPfx = ("sub_" <>)

defaultProjection :: Projectible expr x =>
                     TablePrefix -> x -> [ ( expr, Maybe T.Text ) ]
defaultProjection pfx =
    zipWith (\i e -> (e, Just (fromString "res" <> fromString (show (i :: Integer)))))
            [0..] . flip project (nextTblPfx pfx)

buildSelect :: ( IsSql92SelectSyntax syntax
               , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
               TablePrefix -> SelectBuilder syntax db a -> syntax
buildSelect _ (SelectBuilderTopLevel limit offset ordering (SelectBuilderSelectSyntax _ _ table) selectStmt') =
    (fromMaybe selectStmt selectStmt') table ordering limit offset
buildSelect pfx (SelectBuilderTopLevel limit offset ordering (SelectBuilderQ proj (QueryBuilder _ from where_)) selectStmt') =
    (fromMaybe selectStmt selectStmt') (selectTableStmt Nothing (projExprs (defaultProjection pfx proj)) from where_ Nothing Nothing) ordering limit offset
buildSelect pfx (SelectBuilderTopLevel limit offset ordering (SelectBuilderGrouping proj (QueryBuilder _ from where_) grouping having distinct) selectStmt') =
    (fromMaybe selectStmt selectStmt') (selectTableStmt distinct (projExprs (defaultProjection pfx proj)) from where_ grouping having) ordering limit offset
buildSelect pfx x = buildSelect pfx (SelectBuilderTopLevel Nothing Nothing [] x Nothing)

selectBuilderToTableSource :: ( Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax syntax)) ~ syntax
                              , IsSql92SelectSyntax syntax
                              , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
                              TablePrefix -> SelectBuilder syntax db a -> Sql92SelectSelectTableSyntax syntax
selectBuilderToTableSource _ (SelectBuilderSelectSyntax _ _ x) = x
selectBuilderToTableSource pfx (SelectBuilderQ x (QueryBuilder _ from where_)) =
  selectTableStmt Nothing (projExprs (defaultProjection pfx x)) from where_ Nothing Nothing
selectBuilderToTableSource pfx (SelectBuilderGrouping x (QueryBuilder _ from where_) grouping having distinct) =
  selectTableStmt distinct (projExprs (defaultProjection pfx x)) from where_ grouping having
selectBuilderToTableSource pfx sb =
    let (x, QueryBuilder _ from where_) = selectBuilderToQueryBuilder pfx sb
    in selectTableStmt Nothing (projExprs (defaultProjection pfx x)) from where_ Nothing Nothing

selectBuilderToQueryBuilder :: ( Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax syntax)) ~ syntax
                               , IsSql92SelectSyntax syntax
                               , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
                               TablePrefix -> SelectBuilder syntax db a -> (a, QueryBuilder syntax)
selectBuilderToQueryBuilder pfx sb =
    let select = buildSelect pfx sb
        x' = reproject (fieldNameFunc (qualifiedField t0)) (sbProj sb)
        t0 = pfx <> "0"
    in (x', QueryBuilder 1 (Just (fromTable (tableFromSubSelect select) (Just t0))) Nothing)

emptyQb :: QueryBuilder select
emptyQb = QueryBuilder 0 Nothing Nothing

sbProj :: SelectBuilder syntax db a -> a
sbProj (SelectBuilderQ proj _) = proj
sbProj (SelectBuilderGrouping proj _ _ _ _) = proj
sbProj (SelectBuilderSelectSyntax _ proj _) = proj
sbProj (SelectBuilderTopLevel _ _ _ sb _) = sbProj sb

setSelectBuilderProjection :: Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) b =>
                              SelectBuilder syntax db a -> b -> SelectBuilder syntax db b
setSelectBuilderProjection (SelectBuilderQ _ q) proj = SelectBuilderQ proj q
setSelectBuilderProjection (SelectBuilderGrouping _ q grouping having d) proj = SelectBuilderGrouping proj q grouping having d
setSelectBuilderProjection (SelectBuilderSelectSyntax containsSetOp _ q) proj = SelectBuilderSelectSyntax containsSetOp proj q
setSelectBuilderProjection (SelectBuilderTopLevel limit offset ord sb s) proj =
    SelectBuilderTopLevel limit offset ord (setSelectBuilderProjection sb proj) s

limitSelectBuilder, offsetSelectBuilder :: Integer -> SelectBuilder syntax db a -> SelectBuilder syntax db a
limitSelectBuilder limit (SelectBuilderTopLevel limit' offset ordering tbl build) =
    SelectBuilderTopLevel (Just $ maybe limit (min limit) limit') offset ordering tbl build
limitSelectBuilder limit x = SelectBuilderTopLevel (Just limit) Nothing [] x Nothing

offsetSelectBuilder offset (SelectBuilderTopLevel Nothing offset' ordering tbl build) =
    SelectBuilderTopLevel Nothing (Just $ offset + fromMaybe 0 offset') ordering tbl build
offsetSelectBuilder offset (SelectBuilderTopLevel (Just limit) offset' ordering tbl build) =
    SelectBuilderTopLevel (Just $ max 0 (limit - offset)) (Just $ offset + fromMaybe 0 offset') ordering tbl build
offsetSelectBuilder offset x = SelectBuilderTopLevel Nothing (Just offset) [] x Nothing

exprWithContext :: TablePrefix -> WithExprContext a -> a
exprWithContext pfx = ($ nextTblPfx pfx)

buildJoinTableSourceQuery
  :: ( IsSql92SelectSyntax select
     , Projectible (Sql92SelectExpressionSyntax select) x
     , Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax select)) ~ select )
  => TablePrefix -> select
  -> x -> QueryBuilder select
  -> (x, QueryBuilder select)
buildJoinTableSourceQuery tblPfx tblSource x qb =
  let qb' = QueryBuilder (tblRef + 1) from' (qbWhere qb)
      !tblRef = qbNextTblRef qb
      from' = case qbFrom qb of
                Nothing -> Just newSource
                Just oldFrom -> Just (innerJoin oldFrom newSource Nothing)
      newSource = fromTable (tableFromSubSelect tblSource) (Just newTblNm)
      newTblNm = tblPfx <> fromString (show tblRef)
  in (reproject (fieldNameFunc (qualifiedField newTblNm)) x, qb')

buildInnerJoinQuery
    :: forall select s table
     . (Beamable table, IsSql92SelectSyntax select)
    => TablePrefix -> T.Text -> TableSettings table
    -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (WithExprContext (Sql92SelectExpressionSyntax select)))
    -> QueryBuilder select -> (T.Text, table (QExpr (Sql92SelectExpressionSyntax select) s), QueryBuilder select)
buildInnerJoinQuery tblPfx tbl tblSettings mkOn qb =
  let qb' = QueryBuilder (tblRef + 1) from' where'
      tblRef = qbNextTblRef qb
      newTblNm = tblPfx <> fromString (show tblRef)
      newSource = fromTable (tableNamed tbl) (Just newTblNm)
      (from', where') =
        case qbFrom qb of
          Nothing -> (Just newSource, andE' (qbWhere qb) (exprWithContext tblPfx <$> mkOn newTbl))
          Just oldFrom -> (Just (innerJoin oldFrom newSource (exprWithContext tblPfx <$> mkOn newTbl)), qbWhere qb)

      newTbl = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
  in (newTblNm, newTbl, qb')

nextTbl :: (IsSql92SelectSyntax select, Beamable table)
        => QueryBuilder select
        -> TablePrefix -> T.Text -> TableSettings table
        -> ( table (QExpr (Sql92SelectExpressionSyntax select) s)
           , T.Text
           , QueryBuilder select )
nextTbl qb tblPfx _ tblSettings =
  let tblRef = qbNextTblRef qb
      newTblNm = tblPfx <> fromString (show tblRef)
      newTbl = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
  in (newTbl, newTblNm, qb { qbNextTblRef = qbNextTblRef qb + 1})

projOrder :: Projectible expr x =>
             x -> WithExprContext [ expr ]
projOrder = project -- (Proxy @AnyType) (\_ x -> tell [x] >> pure x)

-- | Convenience functions to construct an arbitrary SQL92 select syntax type
-- from a 'Q'. Used by most backends as the default implementation of
-- 'buildSqlQuery' in 'HasQBuilder'.
buildSql92Query' ::
    forall select projSyntax db s a.
    ( IsSql92SelectSyntax select
    , Eq (Sql92SelectExpressionSyntax select)
    , projSyntax ~ Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax select)
    , Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax select)) ~ select

    , Sql92ProjectionExpressionSyntax projSyntax ~ Sql92SelectExpressionSyntax select
    , Projectible (Sql92ProjectionExpressionSyntax projSyntax) a ) =>
    Bool {-^ Whether this backend supports arbitrary nested UNION, INTERSECT, EXCEPT -} ->
    T.Text {-^ Table prefix -} ->
    Q select db s a ->
    select
buildSql92Query' arbitrarilyNestedCombinations tblPfx (Q q) =
    buildSelect tblPfx (buildQuery (fromF q))
  where
    buildQuery :: forall s x.
                  Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                  Free (QF select db s) x
               -> SelectBuilder select db x
    buildQuery (Pure x) = SelectBuilderQ x emptyQb
    buildQuery (Free (QGuard _ next)) = buildQuery next
    buildQuery f@(Free QAll {}) = buildJoinedQuery f emptyQb
    buildQuery f@(Free QArbitraryJoin {}) = buildJoinedQuery f emptyQb
    buildQuery f@(Free QTwoWayJoin {}) = buildJoinedQuery f emptyQb
    buildQuery (Free (QSubSelect q' next)) =
        let sb = buildQuery (fromF q')
            (proj, qb) = selectBuilderToQueryBuilder tblPfx sb
        in buildJoinedQuery (next proj) qb
    buildQuery (Free (QDistinct nubType q' next)) =
      let (proj, qb, gp, hv) =
            case buildQuery (fromF q') of
              SelectBuilderQ proj qb ->
                ( proj, qb, Nothing, Nothing)
              SelectBuilderGrouping proj qb gp hv Nothing ->
                ( proj, qb, gp, hv)
              sb ->
                let (proj, qb) = selectBuilderToQueryBuilder tblPfx sb
                in ( proj, qb, Nothing, Nothing)
      in case next proj of
           Pure x -> SelectBuilderGrouping x qb gp hv (Just (exprWithContext tblPfx (nubType proj)))
           _ -> let ( proj', qb' ) = selectBuilderToQueryBuilder tblPfx (SelectBuilderGrouping proj qb gp hv (Just (exprWithContext tblPfx (nubType proj))))
                in buildJoinedQuery (next proj') qb'
    buildQuery (Free (QAggregate mkAgg q' next)) =
        let sb = buildQuery (fromF q')
            (groupingSyntax, aggProj) = mkAgg (sbProj sb) (nextTblPfx tblPfx)
        in case tryBuildGuardsOnly (next aggProj) Nothing of
            Just (proj, having) ->
                case sb of
                  SelectBuilderQ _ q'' -> SelectBuilderGrouping proj q'' groupingSyntax having Nothing

                  -- We'll have to generate a subselect
                  _ -> let (subProj, qb) = selectBuilderToQueryBuilder tblPfx sb --(setSelectBuilderProjection sb aggProj)
                           (groupingSyntax, aggProj') = mkAgg subProj (nextTblPfx tblPfx)
                       in case tryBuildGuardsOnly (next aggProj') Nothing of
                            Nothing -> error "buildQuery (Free (QAggregate ...)): Impossible"
                            Just (aggProj'', having') ->
                              SelectBuilderGrouping aggProj'' qb groupingSyntax having' Nothing
            Nothing ->
              let (_, having) = tryCollectHaving (next aggProj') Nothing
                  (next', _) = tryCollectHaving (next x') Nothing
                  (groupingSyntax', aggProj', qb) =
                    case sb of
                      SelectBuilderQ _ q'' -> (groupingSyntax, aggProj, q'')
                      _ -> let (proj', qb''') = selectBuilderToQueryBuilder tblPfx sb
                               (groupingSyntax', aggProj') = mkAgg proj' (nextTblPfx tblPfx)
                           in (groupingSyntax', aggProj', qb''')
                  (x', qb') = selectBuilderToQueryBuilder tblPfx $
                              SelectBuilderGrouping aggProj' qb groupingSyntax' having Nothing
              in buildJoinedQuery next' qb'

    buildQuery (Free (QOrderBy mkOrdering q' next)) =
        let sb = buildQuery (fromF q')
            proj = sbProj sb
            ordering = exprWithContext tblPfx (mkOrdering proj)

            doJoined =
                let sb' = case sb of
                            SelectBuilderQ {} ->
                                SelectBuilderTopLevel Nothing Nothing ordering sb Nothing
                            SelectBuilderGrouping {} ->
                                SelectBuilderTopLevel Nothing Nothing ordering sb Nothing
                            SelectBuilderSelectSyntax {} ->
                                SelectBuilderTopLevel Nothing Nothing ordering sb Nothing
                            SelectBuilderTopLevel Nothing Nothing [] sb' build ->
                                SelectBuilderTopLevel Nothing Nothing ordering sb' build
                            SelectBuilderTopLevel Nothing (Just 0) [] sb' build ->
                                SelectBuilderTopLevel Nothing (Just 0) ordering sb' build
                            SelectBuilderTopLevel {}
                                | (proj'', qb) <- selectBuilderToQueryBuilder tblPfx sb ->
                                    SelectBuilderTopLevel Nothing Nothing (exprWithContext tblPfx (mkOrdering proj'')) (SelectBuilderQ proj'' qb) Nothing
                                | otherwise -> error "buildQuery (Free (QOrderBy ...)): query inspected expression"

                    (joinedProj, qb) = selectBuilderToQueryBuilder tblPfx sb'
                in buildJoinedQuery (next joinedProj) qb
        in case next proj of
             Pure proj' ->
               case ordering of
                 [] -> setSelectBuilderProjection sb proj'
                 ordering ->
                     case sb of
                       SelectBuilderQ {} ->
                           SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb proj') Nothing
                       SelectBuilderGrouping {} ->
                           SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb proj') Nothing
                       SelectBuilderSelectSyntax {} ->
                           SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb proj') Nothing
                       SelectBuilderTopLevel Nothing Nothing [] sb' build ->
                           SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb' proj') build
                       SelectBuilderTopLevel (Just 0) (Just 0) [] sb' build ->
                           SelectBuilderTopLevel (Just 0) (Just 0) ordering (setSelectBuilderProjection sb' proj') build
                       SelectBuilderTopLevel {}
                           | (proj'', qb) <- selectBuilderToQueryBuilder tblPfx sb,
                             Pure proj''' <- next proj'' ->
                               SelectBuilderTopLevel Nothing Nothing (exprWithContext tblPfx (mkOrdering proj'')) (SelectBuilderQ proj''' qb) Nothing
                           | otherwise -> error "buildQuery (Free (QOrderBy ...)): query inspected expression"
             _ -> doJoined

    buildQuery (Free (QWindowOver mkWindows mkProjection q' next)) =
        let sb = buildQuery (fromF q')

            x = sbProj sb
            windows = mkWindows x
            projection = mkProjection x windows
        in case next projection of
             Pure x' ->
               -- Windowing makes this automatically a top-level (this prevents aggregates from being added directly)
               case setSelectBuilderProjection sb x' of
                 sb'@SelectBuilderTopLevel {} -> sb'
                 sb' -> SelectBuilderTopLevel Nothing Nothing [] sb' Nothing
             _       ->
               let (x', qb) = selectBuilderToQueryBuilder tblPfx (setSelectBuilderProjection sb projection)
               in buildJoinedQuery (next x') qb

    buildQuery (Free (QLimit limit q' next)) =
        let sb = limitSelectBuilder limit (buildQuery (fromF q'))
            x = sbProj sb
        -- In the case of limit, we must directly return whatever was given
        in case next x of
             Pure x' -> setSelectBuilderProjection sb x'

             -- Otherwise, this is going to be part of a join...
             _ -> let (x', qb) = selectBuilderToQueryBuilder tblPfx sb
                  in buildJoinedQuery (next x') qb

    buildQuery (Free (QOffset offset q' next)) =
        let sb = offsetSelectBuilder offset (buildQuery (fromF q'))
            x = sbProj sb
        -- In the case of limit, we must directly return whatever was given
        in case next x of
             Pure x' -> setSelectBuilderProjection sb x'
             -- Otherwise, this is going to be part of a join...
             _ -> let (x', qb) = selectBuilderToQueryBuilder tblPfx sb
                  in buildJoinedQuery (next x') qb

    buildQuery (Free (QUnion all_ left right next)) =
      buildTableCombination (unionTables all_) left right next
    buildQuery (Free (QIntersect all_ left right next)) =
      buildTableCombination (intersectTables all_) left right next
    buildQuery (Free (QExcept all_ left right next)) =
      buildTableCombination (exceptTable all_) left right next

    buildQuery (Free (QForceSelect selectStmt' over next)) =
      let sb = buildQuery (fromF over)
          x = sbProj sb

          selectStmt'' = selectStmt' (sbProj sb)

          sb' = case sb of
                  SelectBuilderTopLevel { sbSelectFn = Nothing } ->
                    sb { sbSelectFn = Just selectStmt'' }
                  SelectBuilderTopLevel { sbSelectFn = Just {} } ->
                    error "Force select too hard"
                  _ -> SelectBuilderTopLevel Nothing Nothing [] sb (Just selectStmt'')
      in case next (sbProj sb') of
           Pure x' -> setSelectBuilderProjection sb' x'
           _ -> let (x', qb) = selectBuilderToQueryBuilder tblPfx sb'
                in buildJoinedQuery (next x') qb

    tryBuildGuardsOnly :: forall s x.
                          Free (QF select db s) x
                       -> Maybe (Sql92SelectExpressionSyntax select)
                       -> Maybe (x, Maybe (Sql92SelectExpressionSyntax select))
    tryBuildGuardsOnly next having =
      case tryCollectHaving next having of
        (Pure x, having') -> Just (x, having')
        _ -> Nothing

    tryCollectHaving :: forall s x.
                        Free (QF select db s) x
                     -> Maybe (Sql92SelectExpressionSyntax select)
                     -> (Free (QF select db s) x, Maybe (Sql92SelectExpressionSyntax select))
    tryCollectHaving (Free (QGuard cond next)) having = tryCollectHaving next (andE' having (Just (exprWithContext tblPfx cond)))
    tryCollectHaving next having = (next, having)

    buildTableCombination ::
      forall s x r.
        ( Projectible (Sql92ProjectionExpressionSyntax projSyntax) r
        , Projectible (Sql92ProjectionExpressionSyntax projSyntax) x ) =>
        (Sql92SelectSelectTableSyntax select -> Sql92SelectSelectTableSyntax select -> Sql92SelectSelectTableSyntax select) ->
        QM select db (QNested s) x -> QM select db (QNested s) x -> (x -> Free (QF select db s) r) -> SelectBuilder select db r
    buildTableCombination combineTables left right next =
        let leftSb = buildQuery (fromF left)
            leftTb = selectBuilderToTableSource tblPfx leftSb
            rightSb = buildQuery (fromF right)
            rightTb = selectBuilderToTableSource tblPfx rightSb

            proj = reproject (fieldNameFunc unqualifiedField) (sbProj leftSb)

            leftTb' | arbitrarilyNestedCombinations = leftTb
                    | sbContainsSetOperation leftSb =
                      let (x', qb) = selectBuilderToQueryBuilder tblPfx leftSb
                      in selectBuilderToTableSource tblPfx (SelectBuilderQ x' qb)
                    | otherwise = leftTb
            rightTb' | arbitrarilyNestedCombinations = rightTb
                     | sbContainsSetOperation rightSb =
                       let (x', qb) = selectBuilderToQueryBuilder tblPfx rightSb
                       in selectBuilderToTableSource tblPfx (SelectBuilderQ x' qb)
                     | otherwise = rightTb

            sb = SelectBuilderSelectSyntax True proj (combineTables leftTb' rightTb')
        in case next proj of
             Pure proj'
               | projOrder proj (nextTblPfx tblPfx) == projOrder proj' (nextTblPfx tblPfx) ->
                   setSelectBuilderProjection sb proj'
             _ -> let (x', qb) = selectBuilderToQueryBuilder tblPfx sb
                  in buildJoinedQuery (next x') qb

    buildJoinedQuery :: forall s x.
                        Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                        Free (QF select db s) x -> QueryBuilder select -> SelectBuilder select db x
    buildJoinedQuery (Pure x) qb = SelectBuilderQ x qb
    buildJoinedQuery (Free (QAll tbl tblSettings on next)) qb =
        let (newTblNm, newTbl, qb') = buildInnerJoinQuery tblPfx tbl tblSettings on qb
        in buildJoinedQuery (next (newTblNm, newTbl)) qb'
    buildJoinedQuery (Free (QArbitraryJoin q mkJoin on next)) qb =
      case fromF q of
        Free (QAll dbTblNm dbTblSettings on' next')
          | (newTbl, newTblNm, qb') <- nextTbl qb tblPfx dbTblNm dbTblSettings,
            Nothing <- exprWithContext tblPfx <$> on' newTbl,
            Pure proj <- next' (newTblNm, newTbl) ->
            let newSource = fromTable (tableNamed dbTblNm) (Just newTblNm)
                on'' = exprWithContext tblPfx <$> on proj
                (from', where') =
                  case qbFrom qb' of
                    Nothing -> (Just newSource, andE' (qbWhere qb) on'')
                    Just oldFrom -> (Just (mkJoin oldFrom newSource on''), qbWhere qb)
            in buildJoinedQuery (next proj) (qb' { qbFrom = from', qbWhere = where' })

        q' -> let sb = buildQuery q'
                  tblSource = buildSelect tblPfx sb
                  newTblNm = tblPfx <> fromString (show (qbNextTblRef qb))

                  newSource = fromTable (tableFromSubSelect tblSource) (Just newTblNm)

                  proj' = reproject (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
                  on' = exprWithContext tblPfx <$> on proj'

                  (from', where') =
                    case qbFrom qb of
                      Nothing -> (Just newSource, andE' (qbWhere qb) on')
                      Just oldFrom -> (Just (mkJoin oldFrom newSource on'), qbWhere qb)

              in buildJoinedQuery (next proj') (qb { qbNextTblRef = qbNextTblRef qb + 1
                                                   , qbFrom = from', qbWhere = where' })
    buildJoinedQuery (Free (QTwoWayJoin a b mkJoin on next)) qb =
      let (aProj, aSource, qb') =
            case fromF a of
              Free (QAll dbTblNm dbTblSettings on' next')
                | (newTbl, newTblNm, qb') <- nextTbl qb tblPfx dbTblNm dbTblSettings,
                  Nothing <- on' newTbl, Pure proj <- next' (newTblNm, newTbl) ->
                    (proj, fromTable (tableNamed dbTblNm) (Just newTblNm), qb')

              a -> let sb = buildQuery a
                       tblSource = buildSelect tblPfx sb

                       newTblNm = tblPfx <> fromString (show (qbNextTblRef qb))

                       proj' = reproject (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
                   in (proj', fromTable (tableFromSubSelect tblSource) (Just newTblNm), qb { qbNextTblRef = qbNextTblRef qb + 1 })

          (bProj, bSource, qb'') =
            case fromF b of
              Free (QAll dbTblNm dbTblSettings on' next')
                | (newTbl, newTblNm, qb'') <- nextTbl qb' tblPfx dbTblNm dbTblSettings,
                  Nothing <- on' newTbl, Pure proj <- next' (newTblNm, newTbl) ->
                    (proj, fromTable (tableNamed dbTblNm) (Just newTblNm), qb'')

              b -> let sb = buildQuery b
                       tblSource = buildSelect tblPfx sb

                       newTblNm = tblPfx <> fromString (show (qbNextTblRef qb))

                       proj' = reproject (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
                   in (proj', fromTable (tableFromSubSelect tblSource) (Just newTblNm), qb { qbNextTblRef = qbNextTblRef qb + 1 })

          abSource = mkJoin aSource bSource (exprWithContext tblPfx <$> on (aProj, bProj))

          from' =
            case qbFrom qb'' of
              Nothing -> Just abSource
              Just oldFrom -> Just (innerJoin oldFrom abSource Nothing)

      in buildJoinedQuery (next (aProj, bProj)) (qb'' { qbFrom = from' })
    buildJoinedQuery (Free (QGuard cond next)) qb =
        buildJoinedQuery next (qb { qbWhere = andE' (qbWhere qb) (Just (exprWithContext tblPfx cond)) })
    buildJoinedQuery now qb =
      onlyQ now
        (\now' next ->
           let sb = buildQuery now'
               tblSource = buildSelect tblPfx sb
               (x', qb') = buildJoinTableSourceQuery tblPfx tblSource (sbProj sb) qb
           in buildJoinedQuery (next x') qb')

    onlyQ :: forall s x.
             Free (QF select db s) x
          -> (forall a'. Projectible (Sql92SelectExpressionSyntax select) a' => Free (QF select db s) a' -> (a' -> Free (QF select db s) x) -> SelectBuilder select db x)
          -> SelectBuilder select db x
    onlyQ (Free (QAll entityNm entitySettings mkOn next)) f =
      f (Free (QAll entityNm entitySettings mkOn (Pure . PreserveLeft))) (next . unPreserveLeft)
    onlyQ (Free (QArbitraryJoin entity mkJoin mkOn next)) f =
      f (Free (QArbitraryJoin entity mkJoin mkOn Pure)) next
    onlyQ (Free (QTwoWayJoin a b mkJoin mkOn next)) f =
      f (Free (QTwoWayJoin a b mkJoin mkOn Pure)) next
    onlyQ (Free (QSubSelect q' next)) f =
      f (Free (QSubSelect q' Pure)) next
    onlyQ (Free (QLimit limit q' next)) f =
      f (Free (QLimit limit q' Pure)) next
    onlyQ (Free (QOffset offset q' next)) f =
      f (Free (QOffset offset q' Pure)) next
    onlyQ (Free (QUnion all_ a b next)) f =
      f (Free (QUnion all_ a b Pure)) next
    onlyQ (Free (QIntersect all_ a b next)) f =
      f (Free (QIntersect all_ a b Pure)) next
    onlyQ (Free (QExcept all_ a b next)) f =
      f (Free (QExcept all_ a b Pure)) next
    onlyQ (Free (QOrderBy mkOrdering q' next)) f =
      f (Free (QOrderBy mkOrdering q' Pure)) next
    onlyQ (Free (QWindowOver mkWindow mkProj q' next)) f =
      f (Free (QWindowOver mkWindow mkProj q' Pure)) next
    onlyQ (Free (QAggregate mkAgg q' next)) f =
      f (Free (QAggregate mkAgg q' Pure)) next
    onlyQ (Free (QDistinct d q' next)) f =
      f (Free (QDistinct d q' Pure)) next
    onlyQ (Free (QForceSelect s over next)) f =
      f (Free (QForceSelect s over Pure)) next
    onlyQ _ _ = error "impossible"
