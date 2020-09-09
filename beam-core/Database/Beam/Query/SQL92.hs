{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Database.Beam.Query.SQL92
    ( buildSql92Query' ) where

import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL

import           Control.Monad.Free.Church
import           Control.Monad.Free

import           Data.Kind (Type)
import           Data.Maybe
import           Data.Proxy (Proxy(Proxy))
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
instance (Monoid a, ProjectibleWithPredicate c syntax res b) => ProjectibleWithPredicate c syntax res (PreserveLeft a b) where
  project' context be f (PreserveLeft (a, b)) =
    PreserveLeft . (a,) <$> project' context be f b

  projectSkeleton' ctxt be mkM =
    PreserveLeft . (mempty,) <$> projectSkeleton' ctxt be mkM

type SelectStmtFn be
  =  BeamSqlBackendSelectTableSyntax be
 -> [BeamSqlBackendOrderingSyntax be]
 -> Maybe Integer {-^ LIMIT -}
 -> Maybe Integer {-^ OFFSET -}
 -> BeamSqlBackendSelectSyntax be

data QueryBuilder be
  = QueryBuilder
  { qbNextTblRef :: Int
  , qbFrom  :: Maybe (BeamSqlBackendFromSyntax be)
  , qbWhere :: Maybe (BeamSqlBackendExpressionSyntax be) }

data SelectBuilder be (db :: (Type -> Type) -> Type) a where
  SelectBuilderQ :: ( BeamSqlBackend be
                    , Projectible be a )
                 => a -> QueryBuilder be -> SelectBuilder be db a
  SelectBuilderGrouping
      :: ( BeamSqlBackend be
         , Projectible be a )
      => a -> QueryBuilder be
      -> Maybe (BeamSqlBackendGroupingSyntax be)
      -> Maybe (BeamSqlBackendExpressionSyntax be)
      -> Maybe (BeamSqlBackendSetQuantifierSyntax be)
      -> SelectBuilder be db a
  SelectBuilderSelectSyntax :: Bool {- Whether or not this contains UNION, INTERSECT, EXCEPT, etc -}
                            -> a -> BeamSqlBackendSelectTableSyntax be
                            -> SelectBuilder be db a
  SelectBuilderTopLevel ::
    { sbLimit, sbOffset :: Maybe Integer
    , sbOrdering        :: [ BeamSqlBackendOrderingSyntax be ]
    , sbTable           :: SelectBuilder be db a
    , sbSelectFn        :: Maybe (SelectStmtFn be)
                        -- ^ Which 'SelectStmtFn' to use to build this select. If 'Nothing', use the default
    } -> SelectBuilder be db a

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

defaultProjection :: Projectible be x
                  => Proxy be -> TablePrefix -> x -> [ ( BeamSqlBackendExpressionSyntax be , Maybe T.Text ) ]
defaultProjection be pfx =
    zipWith (\i e -> (e, Just (fromString "res" <> fromString (show (i :: Integer)))))
            [0..] . flip (project be) (nextTblPfx pfx)

buildSelect :: forall be db a
             . ( BeamSqlBackend be, Projectible be a )
            => TablePrefix -> SelectBuilder be db a -> BeamSqlBackendSelectSyntax be
buildSelect _ (SelectBuilderTopLevel limit offset ordering (SelectBuilderSelectSyntax _ _ table) selectStmt') =
    (fromMaybe selectStmt selectStmt') table ordering limit offset
buildSelect pfx (SelectBuilderTopLevel limit offset ordering (SelectBuilderQ proj (QueryBuilder _ from where_)) selectStmt') =
    (fromMaybe selectStmt selectStmt') (selectTableStmt Nothing (projExprs (defaultProjection (Proxy @be) pfx proj)) from where_ Nothing Nothing) ordering limit offset
buildSelect pfx (SelectBuilderTopLevel limit offset ordering (SelectBuilderGrouping proj (QueryBuilder _ from where_) grouping having distinct) selectStmt') =
    (fromMaybe selectStmt selectStmt') (selectTableStmt distinct (projExprs (defaultProjection (Proxy @be) pfx proj)) from where_ grouping having) ordering limit offset
buildSelect pfx x = buildSelect pfx (SelectBuilderTopLevel Nothing Nothing [] x Nothing)

selectBuilderToTableSource :: forall be db a
                            . ( BeamSqlBackend be, Projectible be a )
                           => TablePrefix -> SelectBuilder be db a -> BeamSqlBackendSelectTableSyntax be
selectBuilderToTableSource _ (SelectBuilderSelectSyntax _ _ x) = x
selectBuilderToTableSource pfx (SelectBuilderQ x (QueryBuilder _ from where_)) =
  selectTableStmt Nothing (projExprs (defaultProjection (Proxy @be) pfx x)) from where_ Nothing Nothing
selectBuilderToTableSource pfx (SelectBuilderGrouping x (QueryBuilder _ from where_) grouping having distinct) =
  selectTableStmt distinct (projExprs (defaultProjection (Proxy @be) pfx x)) from where_ grouping having
selectBuilderToTableSource pfx sb =
    let (x, QueryBuilder _ from where_) = selectBuilderToQueryBuilder pfx sb
    in selectTableStmt Nothing (projExprs (defaultProjection (Proxy @be) pfx x)) from where_ Nothing Nothing

selectBuilderToQueryBuilder :: forall be db a
                             . ( BeamSqlBackend be, Projectible be a)
                            => TablePrefix -> SelectBuilder be db a -> (a, QueryBuilder be)
selectBuilderToQueryBuilder pfx sb =
    let select = buildSelect pfx sb
        x' = reproject (Proxy @be) (fieldNameFunc (qualifiedField t0)) (sbProj sb)
        t0 = pfx <> "0"
    in (x', QueryBuilder 1 (Just (fromTable (tableFromSubSelect select) (Just (t0, Nothing)))) Nothing)

emptyQb :: QueryBuilder select
emptyQb = QueryBuilder 0 Nothing Nothing

sbProj :: SelectBuilder syntax db a -> a
sbProj (SelectBuilderQ proj _) = proj
sbProj (SelectBuilderGrouping proj _ _ _ _) = proj
sbProj (SelectBuilderSelectSyntax _ proj _) = proj
sbProj (SelectBuilderTopLevel _ _ _ sb _) = sbProj sb

setSelectBuilderProjection :: Projectible be b
                           => SelectBuilder be db a -> b -> SelectBuilder be db b
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
  :: forall be x
   . ( BeamSqlBackend be, Projectible be x )
  => TablePrefix -> BeamSqlBackendSelectSyntax be
  -> x -> QueryBuilder be
  -> (x, QueryBuilder be)
buildJoinTableSourceQuery tblPfx tblSource x qb =
  let qb' = QueryBuilder (tblRef + 1) from' (qbWhere qb)
      !tblRef = qbNextTblRef qb
      from' = case qbFrom qb of
                Nothing -> Just newSource
                Just oldFrom -> Just (innerJoin oldFrom newSource Nothing)
      newSource = fromTable (tableFromSubSelect tblSource) (Just (newTblNm, Nothing))
      newTblNm = tblPfx <> fromString (show tblRef)
  in (reproject (Proxy @be) (fieldNameFunc (qualifiedField newTblNm)) x, qb')

buildInnerJoinQuery
  :: forall be r
   . BeamSqlBackend be
  => TablePrefix -> (TablePrefix -> T.Text -> BeamSqlBackendFromSyntax be)
  -> (T.Text -> r)
  -> (r-> Maybe (WithExprContext (BeamSqlBackendExpressionSyntax be)))
  -> QueryBuilder be -> (T.Text, r, QueryBuilder be)
buildInnerJoinQuery tblPfx mkFrom mkTbl mkOn qb =
  let qb' = QueryBuilder (tblRef + 1) from' where'
      tblRef = qbNextTblRef qb
      newTblNm = tblPfx <> fromString (show tblRef)
      newSource = mkFrom (nextTblPfx tblPfx) newTblNm
      (from', where') =
        case qbFrom qb of
          Nothing -> (Just newSource, andE' (qbWhere qb) (exprWithContext tblPfx <$> mkOn newTbl))
          Just oldFrom -> (Just (innerJoin oldFrom newSource (exprWithContext tblPfx <$> mkOn newTbl)), qbWhere qb)

      newTbl = mkTbl newTblNm
  in (newTblNm, newTbl, qb')

nextTbl :: BeamSqlBackend be
        => QueryBuilder be -> TablePrefix
        -> (T.Text -> r)
        -> ( r, T.Text, QueryBuilder be )
nextTbl qb tblPfx mkTbl =
  let tblRef = qbNextTblRef qb
      newTblNm = tblPfx <> fromString (show tblRef)
      newTbl = mkTbl newTblNm
  in (newTbl, newTblNm, qb { qbNextTblRef = qbNextTblRef qb + 1})

projOrder :: Projectible be x
          => Proxy be -> x -> WithExprContext [ BeamSqlBackendExpressionSyntax be ]
projOrder = project -- (Proxy @AnyType) (\_ x -> tell [x] >> pure x)

-- | Convenience functions to construct an arbitrary SQL92 select syntax type
-- from a 'Q'. Used by most backends as the default implementation of
-- 'buildSqlQuery' in 'HasQBuilder'.
buildSql92Query' :: forall be db s a
                  . ( BeamSqlBackend be, Projectible be a)
                 => Bool {-^ Whether this backend supports arbitrary nested UNION, INTERSECT, EXCEPT -}
                 -> T.Text {-^ Table prefix -}
                 -> Q be db s a
                 -> BeamSqlBackendSelectSyntax be
buildSql92Query' arbitrarilyNestedCombinations tblPfx (Q q) =
    buildSelect tblPfx (buildQuery (fromF q))
  where
    be :: Proxy be
    be = Proxy

    buildQuery :: forall s x
                . Projectible be x
               => Free (QF be db s) x
               -> SelectBuilder be db x
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

    buildQuery (Free (QSetOp combine left right next)) =
      buildTableCombination combine left right next

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

    tryBuildGuardsOnly :: forall s x
                        . Free (QF be db s) x
                       -> Maybe (BeamSqlBackendExpressionSyntax be)
                       -> Maybe (x, Maybe (BeamSqlBackendExpressionSyntax be))
    tryBuildGuardsOnly next having =
      case tryCollectHaving next having of
        (Pure x, having') -> Just (x, having')
        _ -> Nothing

    tryCollectHaving :: forall s x.
                        Free (QF be db s) x
                     -> Maybe (BeamSqlBackendExpressionSyntax be)
                     -> (Free (QF be db s) x, Maybe (BeamSqlBackendExpressionSyntax be))
    tryCollectHaving (Free (QGuard cond next)) having = tryCollectHaving next (andE' having (Just (exprWithContext tblPfx cond)))
    tryCollectHaving next having = (next, having)

    buildTableCombination
      :: forall s x r
       . ( Projectible be r, Projectible be x )
      => (BeamSqlBackendSelectTableSyntax be -> BeamSqlBackendSelectTableSyntax be -> BeamSqlBackendSelectTableSyntax be)
      -> QM be db (QNested s) x -> QM be db (QNested s) x -> (x -> Free (QF be db s) r) -> SelectBuilder be db r
    buildTableCombination combineTables left right next =
        let leftSb = buildQuery (fromF left)
            leftTb = selectBuilderToTableSource tblPfx leftSb
            rightSb = buildQuery (fromF right)
            rightTb = selectBuilderToTableSource tblPfx rightSb

            proj = reproject be (fieldNameFunc unqualifiedField) (sbProj leftSb)

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
               | projOrder be proj (nextTblPfx tblPfx) == projOrder be proj' (nextTblPfx tblPfx) ->
                   setSelectBuilderProjection sb proj'
             _ -> let (x', qb) = selectBuilderToQueryBuilder tblPfx sb
                  in buildJoinedQuery (next x') qb

    buildJoinedQuery :: forall s x.
                        Projectible be x =>
                        Free (QF be db s) x -> QueryBuilder be -> SelectBuilder be db x
    buildJoinedQuery (Pure x) qb = SelectBuilderQ x qb
    buildJoinedQuery (Free (QAll mkFrom mkTbl on next)) qb =
        let (newTblNm, newTbl, qb') = buildInnerJoinQuery tblPfx mkFrom mkTbl on qb
        in buildJoinedQuery (next (newTblNm, newTbl)) qb'
    buildJoinedQuery (Free (QArbitraryJoin q mkJoin on next)) qb =
      case fromF q of
        Free (QAll mkDbFrom dbMkTbl on' next')
          | (newTbl, newTblNm, qb') <- nextTbl qb tblPfx dbMkTbl,
            Nothing <- exprWithContext tblPfx <$> on' newTbl,
            Pure proj <- next' (newTblNm, newTbl) ->
            let newSource = mkDbFrom (nextTblPfx tblPfx) newTblNm
                on'' = exprWithContext tblPfx <$> on proj
                (from', where') =
                  case qbFrom qb' of
                    Nothing -> (Just newSource, andE' (qbWhere qb) on'')
                    Just oldFrom -> (Just (mkJoin oldFrom newSource on''), qbWhere qb)
            in buildJoinedQuery (next proj) (qb' { qbFrom = from', qbWhere = where' })

        q' -> let sb = buildQuery q'
                  tblSource = buildSelect tblPfx sb
                  newTblNm = tblPfx <> fromString (show (qbNextTblRef qb))

                  newSource = fromTable (tableFromSubSelect tblSource) (Just (newTblNm, Nothing))

                  proj' = reproject be (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
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
              Free (QAll mkDbFrom dbMkTbl on' next')
                | (newTbl, newTblNm, qb') <- nextTbl qb tblPfx dbMkTbl,
                  Nothing <- on' newTbl, Pure proj <- next' (newTblNm, newTbl) ->
                    (proj, mkDbFrom (nextTblPfx tblPfx) newTblNm, qb')

              a -> let sb = buildQuery a
                       tblSource = buildSelect tblPfx sb

                       newTblNm = tblPfx <> fromString (show (qbNextTblRef qb))

                       proj' = reproject be (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
                   in (proj', fromTable (tableFromSubSelect tblSource) (Just (newTblNm, Nothing)), qb { qbNextTblRef = qbNextTblRef qb + 1 })

          (bProj, bSource, qb'') =
            case fromF b of
              Free (QAll mkDbFrom dbMkTbl on' next')
                | (newTbl, newTblNm, qb'') <- nextTbl qb' tblPfx dbMkTbl,
                  Nothing <- on' newTbl, Pure proj <- next' (newTblNm, newTbl) ->
                    (proj, mkDbFrom (nextTblPfx tblPfx) newTblNm, qb'')

              b -> let sb = buildQuery b
                       tblSource = buildSelect tblPfx sb

                       newTblNm = tblPfx <> fromString (show (qbNextTblRef qb))

                       proj' = reproject be (fieldNameFunc (qualifiedField newTblNm)) (sbProj sb)
                   in (proj', fromTable (tableFromSubSelect tblSource) (Just (newTblNm, Nothing)), qb { qbNextTblRef = qbNextTblRef qb + 1 })

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
             Free (QF be db s) x
          -> (forall a'. Projectible be a' => Free (QF be db s) a' -> (a' -> Free (QF be db s) x) -> SelectBuilder be db x)
          -> SelectBuilder be db x
    onlyQ (Free (QAll entityNm mkTbl mkOn next)) f =
      f (Free (QAll entityNm mkTbl mkOn (Pure . PreserveLeft))) (next . unPreserveLeft)
--      f (Free (QAll entityNm mkTbl mkOn (Pure . PreserveLeft))) (next . unPreserveLeft)
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
    onlyQ (Free (QSetOp combine a b next)) f =
      f (Free (QSetOp combine a b Pure)) next
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
