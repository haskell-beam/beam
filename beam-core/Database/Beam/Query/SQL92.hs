{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}

module Database.Beam.Query.SQL92
    ( buildSql92Query' ) where

import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL

import           Database.Beam.Schema.Tables

import           Control.Monad.Free.Church
import           Control.Monad.Free
import           Control.Monad.Writer

import           Data.Maybe
import           Data.Proxy
import           Data.String
import qualified Data.Text as T

-- * Beam queries

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

data SelectBuilder syntax (db :: (* -> *) -> *) a where
  SelectBuilderQ :: ( IsSql92SelectSyntax syntax
                    , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a ) =>
                    a -> QueryBuilder syntax -> SelectBuilder syntax db a
  SelectBuilderGrouping ::
      ( IsSql92SelectSyntax syntax
      , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a ) =>
      a -> QueryBuilder syntax -> Maybe (Sql92SelectGroupingSyntax syntax) -> Maybe (Sql92SelectExpressionSyntax syntax) -> SelectBuilder syntax db a
  SelectBuilderSelectSyntax :: Bool {- Whether or not this contains UNION, INTERSECT, EXCEPT, etc -}
                            -> a -> Sql92SelectSelectTableSyntax syntax
                            -> SelectBuilder syntax db a
  SelectBuilderTopLevel ::
    { sbLimit, sbOffset :: Maybe Integer
    , sbOrdering        :: [ Sql92SelectOrderingSyntax syntax ]
    , sbTable           :: SelectBuilder syntax db a } ->
    SelectBuilder syntax db a

sbContainsSetOperation :: SelectBuilder syntax db a -> Bool
sbContainsSetOperation (SelectBuilderSelectSyntax contains _ _) = contains
sbContainsSetOperation (SelectBuilderTopLevel { sbTable = tbl }) = sbContainsSetOperation tbl
sbContainsSetOperation _ = False

fieldNameFunc :: IsSql92ExpressionSyntax expr =>
                 (T.Text -> Sql92ExpressionFieldNameSyntax expr) -> Int
              -> expr
fieldNameFunc mkField i = fieldE (mkField ("res" <> fromString (show i)))

defaultProjection :: Projectible expr x =>
                     x -> [ ( expr, Maybe T.Text ) ]
defaultProjection =
    zipWith (\i e -> (e, Just (fromString "res" <> fromString (show (i :: Integer)))))
            [0..] . project

buildSelect :: ( IsSql92SelectSyntax syntax
               , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
               SelectBuilder syntax db a -> syntax
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderSelectSyntax _ _ table)) =
    selectStmt table ordering limit offset
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderQ proj (QueryBuilder _ from where_))) =
    selectStmt (selectTableStmt (projExprs (defaultProjection proj)) from where_ Nothing Nothing) ordering limit offset
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderGrouping proj (QueryBuilder _ from where_) grouping having)) =
    selectStmt (selectTableStmt (projExprs (defaultProjection proj)) from where_ grouping having) ordering limit offset
buildSelect x = buildSelect (SelectBuilderTopLevel Nothing Nothing [] x)

selectBuilderToTableSource :: ( Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax syntax)) ~ syntax
                              , IsSql92SelectSyntax syntax
                              , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
                              SelectBuilder syntax db a -> Sql92SelectSelectTableSyntax syntax
selectBuilderToTableSource (SelectBuilderSelectSyntax _ _ x) = x
selectBuilderToTableSource (SelectBuilderQ x (QueryBuilder _ from where_)) =
  selectTableStmt (projExprs (defaultProjection x)) from where_ Nothing Nothing
selectBuilderToTableSource (SelectBuilderGrouping x (QueryBuilder _ from where_) grouping having) =
  selectTableStmt (projExprs (defaultProjection x)) from where_ grouping having
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
sbProj (SelectBuilderGrouping proj _ _ _) = proj
sbProj (SelectBuilderSelectSyntax _ proj _) = proj
sbProj (SelectBuilderTopLevel _ _ _ sb) = sbProj sb

setSelectBuilderProjection :: Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) b =>
                              SelectBuilder syntax db a -> b -> SelectBuilder syntax db b
setSelectBuilderProjection (SelectBuilderQ _ q) proj = SelectBuilderQ proj q
setSelectBuilderProjection (SelectBuilderGrouping _ q grouping having) proj = SelectBuilderGrouping proj q grouping having
setSelectBuilderProjection (SelectBuilderSelectSyntax containsSetOp _ q) proj = SelectBuilderSelectSyntax containsSetOp proj q
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

buildJoinTableSourceQuery
  :: ( IsSql92SelectSyntax select
     , Projectible (Sql92SelectExpressionSyntax select) x
     , Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax select)) ~ select )
  => select
  -> x -> QueryBuilder select
  -> (x, QueryBuilder select)
buildJoinTableSourceQuery tblSource x qb =
  let qb' = QueryBuilder (tblRef + 1) from' (qbWhere qb)
      !tblRef = qbNextTblRef qb
      from' = case qbFrom qb of
                Nothing -> Just newSource
                Just oldFrom -> Just (innerJoin oldFrom newSource Nothing)
      newSource = fromTable (tableFromSubSelect tblSource) (Just newTblNm)
      newTblNm = "t" <> fromString (show tblRef)
  in (reproject (fieldNameFunc (qualifiedField newTblNm)) x, qb')

buildInnerJoinQuery
    :: forall select s table
     . (Beamable table, IsSql92SelectSyntax select)
    => T.Text -> TableSettings table
    -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (Sql92SelectExpressionSyntax select))
    -> QueryBuilder select -> (table (QExpr (Sql92SelectExpressionSyntax select) s), QueryBuilder select)
buildInnerJoinQuery tbl tblSettings mkOn qb =
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

nextTbl :: (IsSql92SelectSyntax select, Beamable table)
        => QueryBuilder select
        -> T.Text -> TableSettings table
        -> ( table (QExpr (Sql92SelectExpressionSyntax select) s)
           , T.Text
           , QueryBuilder select )
nextTbl qb _ tblSettings =
  let tblRef = qbNextTblRef qb
      newTblNm = "t" <> fromString (show tblRef)
      newTbl = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings
  in (newTbl, newTblNm, qb { qbNextTblRef = qbNextTblRef qb + 1})

projOrder :: Projectible expr x =>
             x -> [ expr ]
projOrder = execWriter . project' (Proxy @AnyType) (\_ x -> tell [x] >> pure x)

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
    Q select db s a ->
    select
buildSql92Query' arbitrarilyNestedCombinations (Q q) =
    buildSelect (buildQuery (fromF q))
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
            (proj, qb) = selectBuilderToQueryBuilder sb
        in buildJoinedQuery (next proj) qb
    buildQuery (Free (QAggregate mkAgg q' next)) =
        let sb = buildQuery (fromF q')
            (groupingSyntax, aggProj) = mkAgg (sbProj sb)
        in case tryBuildGuardsOnly (next aggProj) Nothing of
            Just (proj, having) ->
                case sb of
                  SelectBuilderQ _ q'' -> SelectBuilderGrouping proj q'' groupingSyntax having

                  -- We'll have to generate a subselect
                  _ -> let (subProj, qb) = selectBuilderToQueryBuilder sb --(setSelectBuilderProjection sb aggProj)
                           (groupingSyntax, aggProj') = mkAgg subProj
                       in case tryBuildGuardsOnly (next aggProj') Nothing of
                            Nothing -> error "buildQuery (Free (QAggregate ...)): Impossible"
                            Just (aggProj'', having') ->
                              SelectBuilderGrouping aggProj'' qb groupingSyntax having'
            Nothing ->
              let (_, having) = tryCollectHaving (next aggProj') Nothing
                  (next', _) = tryCollectHaving (next x') Nothing
                  (groupingSyntax', aggProj', qb) =
                    case sb of
                      SelectBuilderQ _ q'' -> (groupingSyntax, aggProj, q'')
                      _ -> let (proj', qb''') = selectBuilderToQueryBuilder sb
                               (groupingSyntax', aggProj') = mkAgg proj'
                           in (groupingSyntax', aggProj', qb''')
                  (x', qb') = selectBuilderToQueryBuilder $
                              SelectBuilderGrouping aggProj' qb groupingSyntax' having
              in buildJoinedQuery next' qb'

    buildQuery (Free (QOrderBy mkOrdering q' next)) =
        let sb = buildQuery (fromF q')
            proj = sbProj sb
            ordering = mkOrdering proj

            doJoined =
                let sb' = case sb of
                            SelectBuilderQ {} ->
                                SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb reproj)
                            SelectBuilderGrouping {} ->
                                SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb reproj)
                            SelectBuilderSelectSyntax {} ->
                                SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb reproj)
                            SelectBuilderTopLevel Nothing Nothing [] sb' ->
                                SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb' reproj)
                            SelectBuilderTopLevel (Just 0) (Just 0) [] sb' ->
                                SelectBuilderTopLevel (Just 0) (Just 0) ordering (setSelectBuilderProjection sb' reproj)
                            SelectBuilderTopLevel {}
                                | (proj'', qb) <- selectBuilderToQueryBuilder sb ->
                                    SelectBuilderTopLevel Nothing Nothing (mkOrdering proj'') (SelectBuilderQ proj'' qb)
                                | otherwise -> error "buildQuery (Free (QOrderBy ...)): query inspected expression"

                    (reproj, _) = selectBuilderToQueryBuilder sb
                    (joinedProj, qb) = selectBuilderToQueryBuilder sb'
                in buildJoinedQuery (next joinedProj) qb
        in case next proj of
             Pure proj' ->
               case ordering of
                 [] -> setSelectBuilderProjection sb proj'
                 ordering ->
                     case sb of
                       SelectBuilderQ {} ->
                           SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb proj')
                       SelectBuilderGrouping {} ->
                           SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb proj')
                       SelectBuilderSelectSyntax {} ->
                           SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb proj')
                       SelectBuilderTopLevel Nothing Nothing [] sb' ->
                           SelectBuilderTopLevel Nothing Nothing ordering (setSelectBuilderProjection sb' proj')
                       SelectBuilderTopLevel (Just 0) (Just 0) [] sb' ->
                           SelectBuilderTopLevel (Just 0) (Just 0) ordering (setSelectBuilderProjection sb' proj')
                       SelectBuilderTopLevel {}
                           | (proj'', qb) <- selectBuilderToQueryBuilder sb,
                             Pure proj''' <- next proj'' ->
                               SelectBuilderTopLevel Nothing Nothing (mkOrdering proj'') (SelectBuilderQ proj''' qb)
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
                 sb' -> SelectBuilderTopLevel Nothing Nothing [] sb'
             _       ->
               let (x', qb) = selectBuilderToQueryBuilder (setSelectBuilderProjection sb projection)
               in buildJoinedQuery (next x') qb

    buildQuery (Free (QLimit limit q' next)) =
        let sb = limitSelectBuilder limit (buildQuery (fromF q'))
            x = sbProj sb
        -- In the case of limit, we must directly return whatever was given
        in case next x of
             Pure x' -> setSelectBuilderProjection sb x'

             -- Otherwise, this is going to be part of a join...
             _ -> let (x', qb) = selectBuilderToQueryBuilder sb
                  in buildJoinedQuery (next x') qb

    buildQuery (Free (QOffset offset q' next)) =
        let sb = offsetSelectBuilder offset (buildQuery (fromF q'))
            x = sbProj sb
        -- In the case of limit, we must directly return whatever was given
        in case next x of
             Pure x' -> setSelectBuilderProjection sb x'
             -- Otherwise, this is going to be part of a join...
             _ -> let (x', qb) = selectBuilderToQueryBuilder sb
                  in buildJoinedQuery (next x') qb

    buildQuery (Free (QUnion all_ left right next)) =
      buildTableCombination (unionTables all_) left right next
    buildQuery (Free (QIntersect all_ left right next)) =
      buildTableCombination (intersectTables all_) left right next
    buildQuery (Free (QExcept all_ left right next)) =
      buildTableCombination (exceptTable all_) left right next

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
    tryCollectHaving (Free (QGuard cond next)) having = tryCollectHaving next (andE' having (Just cond))
    tryCollectHaving next having = (next, having)

    buildTableCombination ::
      forall s x r.
        ( Projectible (Sql92ProjectionExpressionSyntax projSyntax) r
        , Projectible (Sql92ProjectionExpressionSyntax projSyntax) x ) =>
        (Sql92SelectSelectTableSyntax select -> Sql92SelectSelectTableSyntax select -> Sql92SelectSelectTableSyntax select) ->
        QM select db (QNested s) x -> QM select db (QNested s) x -> (x -> Free (QF select db s) r) -> SelectBuilder select db r
    buildTableCombination combineTables left right next =
        let leftSb = buildQuery (fromF left)
            leftTb = selectBuilderToTableSource leftSb
            rightSb = buildQuery (fromF right)
            rightTb = selectBuilderToTableSource rightSb

            proj = reproject (fieldNameFunc unqualifiedField) (sbProj leftSb)

            leftTb' | arbitrarilyNestedCombinations = leftTb
                    | sbContainsSetOperation leftSb =
                      let (x', qb) = selectBuilderToQueryBuilder leftSb
                      in selectBuilderToTableSource (SelectBuilderQ x' qb)
                    | otherwise = leftTb
            rightTb' | arbitrarilyNestedCombinations = rightTb
                     | sbContainsSetOperation rightSb =
                       let (x', qb) = selectBuilderToQueryBuilder rightSb
                       in selectBuilderToTableSource (SelectBuilderQ x' qb)
                     | otherwise = rightTb

            sb = SelectBuilderSelectSyntax True proj (combineTables leftTb' rightTb')
        in case next proj of
             Pure proj'
               | projOrder proj == projOrder proj' -> setSelectBuilderProjection sb proj'
             _ -> let (x', qb) = selectBuilderToQueryBuilder sb
                  in buildJoinedQuery (next x') qb

    buildJoinedQuery :: forall s x.
                        Projectible (Sql92ProjectionExpressionSyntax projSyntax) x =>
                        Free (QF select db s) x -> QueryBuilder select -> SelectBuilder select db x
    buildJoinedQuery (Pure x) qb = SelectBuilderQ x qb
    buildJoinedQuery (Free (QAll tbl tblSettings on next)) qb =
        let (newTbl, qb') = buildInnerJoinQuery tbl tblSettings on qb
        in buildJoinedQuery (next newTbl) qb'
    buildJoinedQuery (Free (QArbitraryJoin q mkJoin on next)) qb =
      case fromF q of
        Free (QAll dbTblNm dbTblSettings on' next')
          | (newTbl, newTblNm, qb') <- nextTbl qb dbTblNm dbTblSettings,
            Nothing <- on' newTbl,
            Pure proj <- next' newTbl ->
            let newSource = fromTable (tableNamed dbTblNm) (Just newTblNm)
                on'' =  on proj
                (from', where') =
                  case qbFrom qb' of
                    Nothing -> (Just newSource, andE' (qbWhere qb) on'')
                    Just oldFrom -> (Just (mkJoin oldFrom newSource on''), qbWhere qb)
            in buildJoinedQuery (next proj) (qb' { qbFrom = from', qbWhere = where' })

        q' -> let sb = buildQuery q'
                  tblSource = buildSelect sb
                  newTblNm = "t" <> fromString (show (qbNextTblRef qb))

                  newSource = fromTable (tableFromSubSelect tblSource) (Just newTblNm)

                  proj' = reproject (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
                  on' = on proj'

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
                | (newTbl, newTblNm, qb') <- nextTbl qb dbTblNm dbTblSettings,
                  Nothing <- on' newTbl, Pure proj <- next' newTbl ->
                    (proj, fromTable (tableNamed dbTblNm) (Just newTblNm), qb')

              a -> let sb = buildQuery a
                       tblSource = buildSelect sb

                       newTblNm = "t" <> fromString (show (qbNextTblRef qb))

                       proj' = reproject (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
                   in (proj', fromTable (tableFromSubSelect tblSource) (Just newTblNm), qb { qbNextTblRef = qbNextTblRef qb + 1 })

          (bProj, bSource, qb'') =
            case fromF b of
              Free (QAll dbTblNm dbTblSettings on' next')
                | (newTbl, newTblNm, qb'') <- nextTbl qb' dbTblNm dbTblSettings,
                  Nothing <- on' newTbl, Pure proj <- next' newTbl ->
                    (proj, fromTable (tableNamed dbTblNm) (Just newTblNm), qb'')

              b -> let sb = buildQuery b
                       tblSource = buildSelect sb

                       newTblNm = "t" <> fromString (show (qbNextTblRef qb))

                       proj' = reproject (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
                   in (proj', fromTable (tableFromSubSelect tblSource) (Just newTblNm), qb { qbNextTblRef = qbNextTblRef qb + 1 })

          abSource = mkJoin aSource bSource (on (aProj, bProj))

          from' =
            case qbFrom qb'' of
              Nothing -> Just abSource
              Just oldFrom -> Just (innerJoin oldFrom abSource Nothing)

      in buildJoinedQuery (next (aProj, bProj)) (qb'' { qbFrom = from' })
    buildJoinedQuery (Free (QGuard cond next)) qb =
        buildJoinedQuery next (qb { qbWhere = andE' (qbWhere qb) (Just cond) })
    buildJoinedQuery now qb =
      onlyQ now
        (\now' next ->
           let sb = buildQuery now'
               tblSource = buildSelect sb
               (x', qb') = buildJoinTableSourceQuery tblSource (sbProj sb) qb
           in buildJoinedQuery (next x') qb')

    onlyQ :: forall s x.
             Free (QF select db s) x
          -> (forall a'. Projectible (Sql92SelectExpressionSyntax select) a' => Free (QF select db s) a' -> (a' -> Free (QF select db s) x) -> SelectBuilder select db x)
          -> SelectBuilder select db x
    onlyQ (Free (QAll entityNm entitySettings mkOn next)) f =
      f (Free (QAll entityNm entitySettings mkOn Pure)) next
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
    onlyQ _ _ = error "impossible"
