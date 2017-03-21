{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeApplications #-}
module Database.Beam.Query.Types
    ( Q, QExpr, QExprToIdentity

    , Projectible(..)

    , Aggregation

    , buildSql92Query ) where

import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL

import           Database.Beam.Schema.Tables

import           Control.Monad.Identity
import           Control.Monad.Free.Church
import           Control.Monad.Free
import           Control.Monad.Writer

import           Data.Maybe
import           Data.Proxy
import           Data.String
import qualified Data.Text as T

import Debug.Trace

-- * Beam queries

type family QExprToIdentity x
type instance QExprToIdentity (table (QGenExpr context syntax s)) = table Identity
type instance QExprToIdentity (table (Nullable c)) = Maybe (QExprToIdentity (table c))
type instance QExprToIdentity (QGenExpr context syntax s a) = a
type instance QExprToIdentity ()     = ()
type instance QExprToIdentity (a, b) = (QExprToIdentity a, QExprToIdentity b)
type instance QExprToIdentity (a, b, c) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c)
type instance QExprToIdentity (a, b, c, d) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d)
type instance QExprToIdentity (a, b, c, d, e) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e)
type instance QExprToIdentity (a, b, c, d, e, f) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f)
type instance QExprToIdentity (a, b, c, d, e, f, g) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g)
type instance QExprToIdentity (a, b, c, d, e, f, g, h) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h )

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
  SelectBuilderGrouping ::
      ( IsSql92SelectSyntax syntax
      , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a ) =>
      a -> QueryBuilder syntax -> Sql92SelectGroupingSyntax syntax -> Maybe (Sql92SelectExpressionSyntax syntax) -> SelectBuilder syntax db a
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
buildSelect (SelectBuilderTopLevel limit offset ordering (SelectBuilderGrouping proj (QueryBuilder _ from where_) grouping having)) =
    selectStmt (selectTableStmt (projExprs (defaultProjection proj)) from where_ (Just grouping) having) ordering limit offset
buildSelect x = buildSelect (SelectBuilderTopLevel Nothing Nothing [] x)

selectBuilderToTableSource :: ( Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax syntax)) ~ syntax
                              , IsSql92SelectSyntax syntax
                              , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) a ) =>
                              SelectBuilder syntax db a -> Sql92SelectSelectTableSyntax syntax
selectBuilderToTableSource (SelectBuilderSelectSyntax _ x) = x
selectBuilderToTableSource (SelectBuilderQ x (QueryBuilder _ from where_)) =
  selectTableStmt (projExprs (defaultProjection x)) from where_ Nothing Nothing
selectBuilderToTableSource (SelectBuilderGrouping x (QueryBuilder _ from where_) grouping having) =
  selectTableStmt (projExprs (defaultProjection x)) from where_ (Just grouping) having
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
sbProj (SelectBuilderSelectSyntax proj _) = proj
sbProj (SelectBuilderTopLevel _ _ _ sb) = sbProj sb

setSelectBuilderProjection :: Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax)) b =>
                              SelectBuilder syntax db a -> b -> SelectBuilder syntax db b
setSelectBuilderProjection (SelectBuilderQ _ q) proj = SelectBuilderQ proj q
setSelectBuilderProjection (SelectBuilderGrouping _ q grouping having) proj = SelectBuilderGrouping proj q grouping having
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
       DatabaseTable db table
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
       DatabaseTable db table
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
projOrder = execWriter . project' (Proxy @AnyType) (\_ x -> tell [x] >> pure x)

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
    buildQuery f@(Free (QAggregate mkAgg q next)) =
        let sb = buildQuery (fromF q)
            aggProj = sbProj sb

            havingProj = reproject (fieldNameFunc unqualifiedField) aggProj
        in case tryBuildGuardsOnly (next aggProj) Nothing of
            Just (proj, having) ->
                case sb of
                  SelectBuilderQ _ q -> SelectBuilderGrouping proj q (mkAgg aggProj) having
                  _ -> let (proj', qb) = selectBuilderToQueryBuilder (setSelectBuilderProjection sb proj)
                       in SelectBuilderQ proj' qb
            Nothing -> error "Can't join aggregates yet"

    buildQuery f@(Free (QOrderBy mkOrdering q next)) =
        let sb = buildQuery (fromF q)
            proj = sbProj sb
            ordering = mkOrdering proj

            doJoined = error "TODO: QOrderBy: doJoined"
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
                       SelectBuilderTopLevel limit offset [] sb ->
                           SelectBuilderTopLevel limit offset ordering (setSelectBuilderProjection sb proj')
                       SelectBuilderTopLevel {}
                           | (proj'', qb) <- selectBuilderToQueryBuilder sb,
                             Pure proj''' <- next proj'' ->
                               SelectBuilderTopLevel Nothing Nothing (mkOrdering proj'') (SelectBuilderQ proj''' qb)
                           | otherwise -> doJoined
             _ -> doJoined

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

    tryBuildGuardsOnly :: Free (QF select db s) x
                       -> Maybe (Sql92SelectExpressionSyntax select)
                       -> Maybe (x, Maybe (Sql92SelectExpressionSyntax select))
    tryBuildGuardsOnly (Pure x) having = Just (x, having)
    tryBuildGuardsOnly (Free (QGuard cond next)) having = tryBuildGuardsOnly next (andE' having (Just cond))
    tryBuildGuardsOnly _ _ = Nothing

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
