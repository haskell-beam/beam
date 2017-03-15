{-# LANGUAGE UndecidableInstances, FunctionalDependencies, TypeApplications, NamedFieldPuns #-}
module Database.Beam.Query.Combinators
    ( all_, join_, guard_, related_, relatedBy_
    , leftJoin_
    , buildJoinFrom
    , SqlReferences(..)
    , SqlJustable(..)
    , SqlDeconstructMaybe(..)
    , SqlOrderable

    , limit_, offset_

    , exists_, unique_, distinct_, subquery_

    , union_, unionAll_
    , intersect_, intersectAll_
    , except_, exceptAll_

    , (<.), (>.), (<=.), (>=.), (==.), (&&.), (||.), not_, div_, mod_
    , HaskellLiteralForQExpr
    , SqlValable(..), As(..)

    -- * SQL GROUP BY and aggregation
--    , aggregate, SqlGroupable(..)
--    , sum_, count_

    -- * SQL ORDER BY
--    , orderBy, asc_, desc_

    -- * SQL nested select
    , sourceSelect_
    ) where

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL92

import Database.Beam.Query.Internal
import Database.Beam.Query.Types

import Database.Beam.Schema.Tables

import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Identity

import Data.Monoid
import Data.String
import Data.Maybe
import Data.Proxy
import Data.Text (Text, unpack)
import Data.Coerce

import GHC.Generics

-- instance (BeamBackend be, FromBackendLiteral be String) => IsString (Aggregation be s Text) where
--     fromString = ProjectAgg . SQLValE . SQLValue
-- instance (Num a, BeamBackend be, FromBackendLiteral be Integer) => Num (Aggregation be s a) where
--     fromInteger x = ProjectAgg (SQLValE (SQLValue x))
--     ProjectAgg a + ProjectAgg b = ProjectAgg (SQLBinOpE "+" a b)
--     ProjectAgg a - ProjectAgg b = ProjectAgg (SQLBinOpE "-" a b)
--     ProjectAgg a * ProjectAgg b = ProjectAgg (SQLBinOpE "*" a b)
--     negate (ProjectAgg a) = ProjectAgg (SQLUnOpE "-" a)
--     abs (ProjectAgg x) = ProjectAgg (SQLFuncE "ABS" [x])
--     signum x = error "signum: not defined for Aggregation. Use CASE...WHEN"

-- | Introduce all entries of a table into the 'Q' monad
all_ :: forall db be table select s.
        ( Database db
        , IsSql92SelectSyntax select

        , IsSql92FromSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
        , IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)))
        , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)) ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)

        , Table table )
       => DatabaseTable be db table -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
all_ tbl = buildJoin tbl Nothing

-- | Introduce all entries of a table into the 'Q' monad based on the given SQLExprbuildJoin :: forall db syntax table be s.
join_ :: ( Database db, Table table
         , IsSql92SelectSyntax select
         , IsSql92FromSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
         , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)) ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
         , IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))) ) =>
         DatabaseTable be db table -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool
      -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
join_ tbl on = buildJoin tbl (Just on)

buildJoin :: forall db select table be s.
             ( Database db, Table table
             , IsSql92SelectSyntax select ) =>
             DatabaseTable be db table
          -> Maybe (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool)
          -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
buildJoin tbl@(DatabaseTable _ name _) on = do
  curTbl <- gets qbNextTblRef
  let newSource = fromTable (tableNamed name)
                            (Just (fromString ("t" <> show curTbl)))
  buildJoinFrom tbl newSource on

buildJoinFrom :: forall db select table be s.
                 ( Database db, Table table
                 , IsSql92SelectSyntax select ) =>
                 DatabaseTable be db table
              -> Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)
              -> Maybe (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool)
              -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
buildJoinFrom (DatabaseTable _ name tableSettings :: DatabaseTable be db table) newSource on =
    do curTbl <- gets qbNextTblRef
       modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                 , qbFrom = from
                                 , qbWhere = where_ } ->
           let (from', where') = case from of
                                   Nothing -> ( Just newSource
                                              , case on of
                                                  Nothing -> where_
                                                  Just (QExpr on') ->
                                                    case where_ of
                                                      Nothing -> Just $ on'
                                                      Just where_ ->
                                                        Just $ andE where_ on')
                                   Just from -> ( Just (innerJoin from newSource $
                                                        case on of
                                                          Nothing -> Nothing
                                                          Just (QExpr on) -> Just on)
                                                , where_ )
           in qb { qbNextTblRef = curTbl + 1
                 , qbFrom = from'
                 , qbWhere = where' }

       let mkScopedField :: Columnar' (TableField be table) a -> Columnar' (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s) a
           mkScopedField (Columnar' f) = Columnar' (QExpr (fieldE (qualifiedField (fromString ("t" <> show curTbl)) (_fieldName f))))
       pure (changeBeamRep mkScopedField tableSettings)

-- | Introduce a table using a left join. Because this is not an inner join, the resulting table is
-- made nullable. This means that each field that would normally have type 'QExpr x' will now have
-- type 'QExpr (Maybe x)'.
leftJoin_ ::
  forall be db table select s.
  ( Database db, Table table
  , IsSql92SelectSyntax select ) =>
  DatabaseTable be db table ->
  (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s) -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool) ->
  Q select db s (table (Nullable (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s)))
leftJoin_ (DatabaseTable table name tableSettings :: DatabaseTable be db table) mkOn =
    do curTbl <- gets qbNextTblRef
       let mkScopedField :: Columnar' (TableField be table) a -> Columnar' (Nullable (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s)) a
           mkScopedField (Columnar' f) = Columnar' (QExpr (fieldE (qualifiedField (fromString ("t" <> show curTbl)) (_fieldName f))))

           scopedTbl = changeBeamRep mkScopedField tableSettings

           mkNonNullableField :: Columnar' (TableField be table) a -> Columnar' (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s) a
           mkNonNullableField (Columnar' f) = Columnar' (QExpr (fieldE (qualifiedField (fromString ("t" <> show curTbl)) (_fieldName f))))
           QExpr on = mkOn (changeBeamRep mkNonNullableField tableSettings)

       modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                 , qbFrom = from } ->
                let from' = case from of
                              Nothing -> error "leftJoin_: empty select source"
                              Just from -> leftJoin from newSource (Just on)
                    newSource = fromTable
                                    (tableNamed name)
                                    (Just (fromString ("t" <> show curTbl)))
                in qb { qbNextTblRef = curTbl + 1
                      , qbFrom = Just from' }

       pure scopedTbl

-- | Only allow results for which the 'QExpr' yields 'True'
guard_ :: forall select db s.
          ( IsSql92SelectSyntax select ) =>
          QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool -> Q select db s ()
guard_ (QExpr guardE') = modify $ \qb@QueryBuilder { qbWhere = guardE } ->
  qb { qbWhere = case guardE of
                   Nothing -> Just guardE'
                   Just guardE -> Just $ andE guardE guardE' }

-- | Introduce all entries of the given table which are referenced by the given 'PrimaryKey'
related_ :: ( IsSql92SelectSyntax select
            , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select))) Bool
            , Database db, Table rel ) =>
            DatabaseTable be db rel
         -> PrimaryKey rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s)
         -> Q select db s (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
related_ (relTbl :: DatabaseTable be db rel) pk =
    mdo rel <- join_ relTbl (pk ==. primaryKey rel)
        pure rel

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_ :: ( Database db, Table rel
              , IsSql92SelectSyntax select )
           => DatabaseTable be db rel -> (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s) ->
                                          QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool)
           -> Q select db s (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
relatedBy_ (relTbl :: DatabaseTable be db rel) mkOn =
    mdo rel <- join_ relTbl (mkOn rel)
        pure rel

class IsSql92ExpressionSyntax syntax => SqlReferences syntax f s | f -> syntax where
    -- | Check that the 'PrimaryKey' given matches the table. Polymorphic so it works over both
    -- regular tables and those that have been made nullable by 'leftJoin_'.
    references_ :: Table tbl => PrimaryKey tbl f -> tbl f -> QExpr syntax s Bool
instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool ) =>
    SqlReferences syntax (QExpr syntax s) s where
    references_ pk (tbl :: tbl (QExpr syntax s)) = pk ==. primaryKey tbl
instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool ) =>
    SqlReferences syntax (Nullable (QExpr syntax s)) s where
    references_ pk (tbl :: tbl (Nullable (QExpr syntax s))) = pk ==. primaryKey tbl

-- | Limit the number of results returned by a query.
--
limit_ :: (IsQuery q, ProjectibleInSelectSyntax select a) =>
          Integer -> q select db s a -> SelectBuilder select db s a
limit_ limit' q =
  case toSelectBuilder q of
    sel@SelectBuilderTopLevel { } ->
      sel { sbLimit = Just $ maybe limit' (min limit') (sbLimit sel) }
    sel -> SelectBuilderTopLevel (Just limit') Nothing [] sel

-- | Drop the first `offset'` results.
offset_ :: ( IsQuery q, ProjectibleInSelectSyntax select a ) =>
           Integer -> q select db s a -> SelectBuilder select db s a
offset_ offset' q =
  case toSelectBuilder q of
    sel@SelectBuilderTopLevel { } ->
      sel { sbOffset = Just $ maybe offset' (offset'+) (sbOffset sel) }
    sel -> SelectBuilderTopLevel Nothing (Just offset') [] sel

-- | Use the SQL exists operator to determine if the given query returns any results
exists_, unique_, distinct_ ::
  ( IsQuery q, IsSql92SelectSyntax select
  , ProjectibleInSelectSyntax select a
  , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select) =>
  q select (db :: (((* -> *) -> *) -> *) -> *) s a
  -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool
exists_ = QExpr . existsE . buildSelect . toSelectBuilder
unique_ = QExpr . uniqueE . buildSelect . toSelectBuilder
distinct_ = QExpr . distinctE . buildSelect . toSelectBuilder

subquery_ ::
  ( IsQuery q, IsSql92SelectSyntax select
  , ProjectibleInSelectSyntax select (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a)
  , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select) =>
  q select (db :: (((* -> *) -> *) -> *) -> *) s (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a)
  -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a
subquery_ =
  QExpr . subqueryE . buildSelect . toSelectBuilder

-- ** Combinators for boolean expressions

class IsSql92ExpressionSyntax syntax => SqlOrd syntax a s | a -> syntax where
    (==.), (/=.) :: a -> a -> QExpr syntax s Bool
    a /=. b = not_ (a ==. b)

instance IsSql92ExpressionSyntax syntax => SqlOrd syntax (QExpr syntax s a) s where
    (==.) = qBinOpE (eqE Nothing)
    (/=.) = qBinOpE (neqE Nothing)

newtype QExprBool syntax s a = QExprBool (QExpr syntax s Bool)

-- instance {-# OVERLAPPING #-} (BeamBackend be, Table tbl) => SqlOrd be (PrimaryKey tbl (QExpr be s)) s where
--     a ==. b = let pkCmp = runIdentity (zipPkM (\(Columnar' x) (Columnar' y) -> return (Columnar' (QExprBool (x ==. y))) ) a b) :: PrimaryKey tbl (QExprBool be s)
--               in foldr (&&.)  (QExpr (SQLValE (sqlBool True))) (pkAllValues (\(Columnar' (QExprBool x)) -> x) pkCmp)
instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable tbl ) => SqlOrd syntax (tbl (QExpr syntax s)) s where
    a ==. b = let (_, e) = runState (zipBeamFieldsM
                                     (\x'@(Columnar' x) (Columnar' y) -> do
                                         modify (\expr ->
                                                   case expr of
                                                     Nothing -> Just $ x ==. y
                                                     Just expr -> Just $ expr &&. x ==. y)
                                         return x') a b) Nothing
              in fromMaybe (QExpr (valueE (sqlValueSyntax True))) e

-- instance {-# OVERLAPPING #-} (BeamBackend be, Table tbl) => SqlOrd be (PrimaryKey tbl (Nullable (QExpr be s))) s where
--     a ==. b = let pkCmp = runIdentity (zipPkM (\(Columnar' x) (Columnar' y) -> return (Columnar' (QExprBool (x ==. y))) ) a b) :: PrimaryKey tbl (QExprBool be s)
--               in foldr (&&.) (QExpr (SQLValE (sqlBool True))) (pkAllValues (\(Columnar' (QExprBool x)) -> x) pkCmp)
instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable tbl)
    => SqlOrd syntax (tbl (Nullable (QExpr syntax s))) s where
    a ==. b = let (_, e) = runState (zipBeamFieldsM
                                      (\x'@(Columnar' x) (Columnar' y) -> do
                                          modify (\expr ->
                                                    case expr of
                                                      Nothing -> Just $ x ==. y
                                                      Just expr -> Just $ expr &&. x ==. y)
                                          return x') a b) Nothing
              in fromMaybe (QExpr (valueE (sqlValueSyntax True))) e

qBinOpE :: forall syntax s a b c. IsSql92ExpressionSyntax syntax =>
           (syntax -> syntax -> syntax)
        -> QExpr syntax s a -> QExpr syntax s b -> QExpr syntax s c
qBinOpE mkOpE (QExpr a) (QExpr b) = QExpr (mkOpE a b)

(<.), (>.), (<=.), (>=.) ::
  IsSql92ExpressionSyntax syntax => QExpr syntax s a -> QExpr syntax s a -> QExpr syntax s Bool
(<.) = qBinOpE (ltE Nothing)
(>.) = qBinOpE (gtE Nothing)
(<=.) = qBinOpE (leE Nothing)
(>=.) = qBinOpE (geE Nothing)

allE :: ( IsSql92ExpressionSyntax syntax, HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool) =>
        [ QExpr syntax s Bool ] -> QExpr syntax s Bool
allE es = fromMaybe (QExpr (valueE (sqlValueSyntax True))) $
          foldl (\expr x ->
                   Just $ maybe x (\e -> e &&. x) expr)
                Nothing es

(&&.), (||.) ::
  IsSql92ExpressionSyntax syntax => QExpr syntax s Bool -> QExpr syntax s Bool -> QExpr syntax s Bool
(&&.) = qBinOpE andE
(||.) = qBinOpE orE

infixr 3 &&.
infixr 2 ||.
infix 4 ==., /=.

not_ :: forall syntax s.
  IsSql92ExpressionSyntax syntax => QExpr syntax s Bool -> QExpr syntax s Bool
not_ (QExpr a) = QExpr (notE a)

mod_, div_ ::
  (Integral a, IsSql92ExpressionSyntax syntax) =>
  QExpr syntax s a -> QExpr syntax s a -> QExpr syntax s a
div_ = qBinOpE divE
mod_ = qBinOpE modE

-- * Combine table sources via UNION, INTERSECT, and EXCEPT

class Reprojectable syntax s a | a -> syntax s where
  reproject :: (Int -> syntax) -> a -> State Int a
instance (IsSql92ExpressionSyntax syntax, Beamable t) => Reprojectable syntax s (t (QExpr syntax s)) where
  reproject mkE a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ -> do
                       i <- state (\i -> (i, i + 1))
                       let fieldName = fromString ("res" <> show i)
                       pure (Columnar' (QExpr (mkE i)))) a a
instance IsSql92ExpressionSyntax syntax => Reprojectable syntax s (QExpr syntax s a) where
  reproject mkE a = do
    i <- state (\i -> (i, i+1))
    pure (QExpr (mkE i))
instance ( Reprojectable syntax s a, Reprojectable syntax s b ) =>
  Reprojectable syntax s (a, b) where

  reproject mkE (a, b) = (,) <$> reproject mkE a <*> reproject mkE b
instance ( Reprojectable syntax s a, Reprojectable syntax s b, Reprojectable syntax s c ) =>
  Reprojectable syntax s (a, b, c) where

  reproject mkE (a, b, c) = (,,) <$> reproject mkE a <*> reproject mkE b <*> reproject mkE c
instance ( Reprojectable syntax s a, Reprojectable syntax s b, Reprojectable syntax s c
         , Reprojectable syntax s d ) =>
  Reprojectable syntax s (a, b, c, d) where

  reproject mkE (a, b, c, d) = (,,,) <$> reproject mkE a <*> reproject mkE b <*> reproject mkE c
                                     <*> reproject mkE d
instance ( Reprojectable syntax s a, Reprojectable syntax s b, Reprojectable syntax s c
         , Reprojectable syntax s d, Reprojectable syntax s e ) =>
  Reprojectable syntax s (a, b, c, d, e) where

  reproject mkE (a, b, c, d, e) = (,,,,) <$> reproject mkE a <*> reproject mkE b <*> reproject mkE c
                                         <*> reproject mkE d <*> reproject mkE e
instance ( Reprojectable syntax s a, Reprojectable syntax s b, Reprojectable syntax s c
         , Reprojectable syntax s d, Reprojectable syntax s e, Reprojectable syntax s f ) =>
  Reprojectable syntax s (a, b, c, d, e, f) where

  reproject mkE  (a, b, c, d, e, f) = (,,,,,) <$> reproject mkE a <*> reproject mkE b <*> reproject mkE c
                                              <*> reproject mkE d <*> reproject mkE e <*> reproject mkE f

selectBuilderToSelectTable :: SelectBuilder syntax db s a -> (a, Sql92SelectSelectTableSyntax syntax)
selectBuilderToSelectTable (SelectBuilderQ q) =
  let (res, _, select) = buildSql92Query q 0
  in (res, select)
selectBuilderToSelectTable (SelectBuilderSelectSyntax a tableSyntax) = (a, tableSyntax)
selectBuilderToSelectTable (SelectBuilderTopLevel Nothing Nothing [] x) = selectBuilderToSelectTable x
selectBuilderToSelectTable (SelectBuilderTopLevel limit offset ordering x) = error "TODO"

union_, unionAll_, intersect_, intersectAll_, except_, exceptAll_ ::
  forall q select db s a.
  ( IsQuery q
  , IsSql92SelectSyntax select
  , Reprojectable (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a
  , ProjectibleInSelectSyntax select a ) =>
  q select db s a -> q select db s a -> SelectBuilder select db s a
union_ = combineTable_ (unionTables False)
unionAll_ = combineTable_ (unionTables True)
intersect_ = combineTable_ (intersectTables False)
intersectAll_ = combineTable_ (intersectTables True)
except_ = combineTable_ (exceptTable False)
exceptAll_ = combineTable_ (exceptTable True)

combineTable_ :: forall q select db s a.
                 ( IsQuery q
                 , IsSql92SelectSyntax select
                 , Reprojectable (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a
                 , ProjectibleInSelectSyntax select a ) =>
                 (Sql92SelectSelectTableSyntax select -> Sql92SelectSelectTableSyntax select -> Sql92SelectSelectTableSyntax select)
              -> q select db s a -> q select db s a -> SelectBuilder select db s a
combineTable_ combine a b =
  let (aRes, aSelTbl) = selectBuilderToSelectTable (toSelectBuilder a :: SelectBuilder select db s a)
      (_, bSelTbl) = selectBuilderToSelectTable (toSelectBuilder b :: SelectBuilder select db s a)

      projection = evalState (reproject (\i -> fieldE (unqualifiedField (fromString ("res" <> show i)))) aRes) 0

  in SelectBuilderSelectSyntax projection (combine aSelTbl bSelTbl)

-- * Marshalling between Haskell literals and QExprs

type family HaskellLiteralForQExpr x = a
type instance HaskellLiteralForQExpr (QExpr syntax s a) = a
type instance HaskellLiteralForQExpr (table (QExpr syntax s)) = table Identity
type instance HaskellLiteralForQExpr (As x -> QExpr syntax s x) = x

type family QExprSyntax x where
  QExprSyntax (QExpr syntax s a) = syntax

data As x = As

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a

instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a, IsSql92ExpressionSyntax syntax) =>
  SqlValable (QExpr syntax s a) where

  val_ = QExpr . valueE . sqlValueSyntax
instance ( x ~ QExpr syntax s a
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax (QExprSyntax x)) a
         , IsSql92ExpressionSyntax (QExprSyntax x) ) =>
  SqlValable (As a -> x) where
  val_ x _ = val_ x
instance ( Beamable table
         , IsSql92ExpressionSyntax syntax
         , FieldsFulfillConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)) table ) =>
  SqlValable (table (QExpr syntax s)) where
  val_ tbl =
    let fields :: table (WithConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)))
        fields = to (gWithConstrainedFields (Proxy @(HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)))
                                            (Proxy @(Rep (table Exposed))) (from tbl))
    in changeBeamRep (\(Columnar' (WithConstraint x :: WithConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)) x)) ->
                         Columnar' (QExpr (valueE (sqlValueSyntax x)))) fields

-- * Aggregators

-- class BeamSqlBackend be => Aggregating be agg s | agg -> s, agg -> be where
--     type LiftAggregationsToQExpr agg s

--     aggToSql :: Proxy s -> agg -> SQLGrouping be
--     liftAggToQExpr :: Proxy s -> agg -> LiftAggregationsToQExpr agg s
-- instance ( Table t, BeamSqlBackend be ) => Aggregating be (t (Aggregation be s)) s where
--     type LiftAggregationsToQExpr (t (Aggregation be s)) s = t (QExpr be s)
--     aggToSql s table = mconcat (allBeamValues (\(Columnar' x) -> aggToSql s x) table)
--     liftAggToQExpr s = changeBeamRep (\(Columnar' x) -> Columnar' (liftAggToQExpr s x))
-- instance BeamSqlBackend be => Aggregating be (Aggregation be s a) s where
--     type LiftAggregationsToQExpr (Aggregation be s a) s = QExpr be s a
--     aggToSql _ (GroupAgg e) = let eSql = optimizeExpr' e
--                               in mempty { sqlGroupBy = [eSql] }
--     aggToSql _ (ProjectAgg _) = mempty

--     liftAggToQExpr _ (GroupAgg e) = QExpr e
--     liftAggToQExpr _ (ProjectAgg e) = QExpr e
-- instance (Aggregating be a s, Aggregating be b s) => Aggregating be (a, b) s where
--     type LiftAggregationsToQExpr (a, b) s = ( LiftAggregationsToQExpr a s
--                                             , LiftAggregationsToQExpr b s)
--     aggToSql s (a, b) = aggToSql s a <> aggToSql s b
--     liftAggToQExpr s (a, b) = (liftAggToQExpr s a, liftAggToQExpr s b)
-- instance (Aggregating be a s, Aggregating be b s, Aggregating be c s) => Aggregating be (a, b, c) s where
--     type LiftAggregationsToQExpr (a, b, c) s = ( LiftAggregationsToQExpr a s
--                                                , LiftAggregationsToQExpr b s
--                                                , LiftAggregationsToQExpr c s )
--     aggToSql s (a, b, c) = aggToSql s a <> aggToSql s b <> aggToSql s c
--     liftAggToQExpr s (a, b, c) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c)
-- instance (Aggregating be a s, Aggregating be b s, Aggregating be c s, Aggregating be d s) => Aggregating be (a, b, c, d) s where
--     type LiftAggregationsToQExpr (a, b, c, d) s = ( LiftAggregationsToQExpr a s
--                                                   , LiftAggregationsToQExpr b s
--                                                   , LiftAggregationsToQExpr c s
--                                                   , LiftAggregationsToQExpr d s)
--     aggToSql s (a, b, c, d) = aggToSql s a <> aggToSql s b <> aggToSql s c <> aggToSql s d
--     liftAggToQExpr s (a, b, c, d) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c, liftAggToQExpr s d)
-- instance (Aggregating be a s, Aggregating be b s, Aggregating be c s, Aggregating be d s, Aggregating be e s) => Aggregating be (a, b, c, d, e) s where
--     type LiftAggregationsToQExpr (a, b, c, d, e) s = ( LiftAggregationsToQExpr a s
--                                                      , LiftAggregationsToQExpr b s
--                                                      , LiftAggregationsToQExpr c s
--                                                      , LiftAggregationsToQExpr d s
--                                                      , LiftAggregationsToQExpr e s)
--     aggToSql s (a, b, c, d, e) = aggToSql s a <> aggToSql s b <> aggToSql s c <> aggToSql s d <> aggToSql s e
--     liftAggToQExpr s (a, b, c, d, e) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c, liftAggToQExpr s d, liftAggToQExpr s e)

-- -- | Type class for things that can be used as the basis of a grouping in a SQL GROUP BY
-- -- clause. This includes 'QExpr a', 'Table's, and 'PrimaryKey's. Because the given object forms the
-- -- basis of the group, its value is available for use in the result set.
-- class SqlGroupable a where
--     type GroupResult a

--     -- | When included in an 'Aggregating' expression, causes the results to be grouped by the
--     -- given column.
--     group_ :: a -> GroupResult a
-- instance SqlGroupable (QExpr be s a) where
--     type GroupResult (QExpr be s a) = Aggregation be s a
--     group_ (QExpr a) = GroupAgg a
-- -- instance {-# OVERLAPPING #-} Table t => SqlGroupable (PrimaryKey t (QExpr be s)) where
-- --     type GroupResult (PrimaryKey t (QExpr be s)) = PrimaryKey t (Aggregation be s)
-- --     group_ = pkChangeRep (\(Columnar' (QExpr e)) -> Columnar' (GroupAgg e))
-- instance Table t => SqlGroupable (t (QExpr be s)) where
--     type GroupResult (t (QExpr be s)) = t (Aggregation be s)
--     group_ = changeBeamRep (\(Columnar' (QExpr e)) -> Columnar' (GroupAgg e))

-- sum_ :: Num a => QExpr be s a -> Aggregation be s a
-- sum_ (QExpr over) = ProjectAgg (SQLFuncE "SUM" [over])

-- count_ :: QExpr be s a -> Aggregation be s Int
-- count_ (QExpr over) = ProjectAgg (SQLFuncE "COUNT" [over])

-- -- | Return a 'TopLevelQ' that will aggregate over the results of the original query. The
-- -- aggregation function (first argument) should accept the output of the query, and return a member
-- -- of the 'Aggregating' class which will become the result of the new query. See the 'group_'
-- -- combinator as well as the various aggregation combinators ('sum_', 'count_', etc.)
-- --
-- -- For example,
-- --
-- -- > aggregate (\employee -> (group_ (_employeeRegion employee), count_ (_employeeId employee))) (all_ employeesTable)
-- --
-- -- will group the result of the `all_ employeesTable` query using the `_employeeRegion` record
-- -- field, and then count up the number of employees for each region.
-- aggregate :: (Projectible be a, Aggregating be agg s) => (a -> agg) -> Q be db s a -> TopLevelQ be db s (LiftAggregationsToQExpr agg s)
-- aggregate (aggregator :: a -> agg) (q :: Q be db s a) =
--     TopLevelQ $
--     do res <- q

--        curTbl <- gets qbNextTblRef
--        let aggregation = aggregator res
--            grouping' = aggToSql (Proxy :: Proxy s) aggregation
--        modify $ \qb -> case sqlGroupBy grouping' of
--                          [] -> qb
--                          _ -> case qbGrouping qb of
--                                 Nothing -> qb { qbGrouping = Just grouping' }
--                                 Just grouping -> qb { qbGrouping = Just (grouping <> grouping') }
--        pure (liftAggToQExpr (Proxy :: Proxy s) aggregation)

-- * Order bys

class SqlOrderable syntax a | a -> syntax where
    makeSQLOrdering :: a -> [ syntax]
--instance SqlOrderable syntax (Sql92OrderingSyntax syntax) where
--    makeSQLOrdering x = [x]
instance SqlOrderable syntax a => SqlOrderable syntax [a] where
    makeSQLOrdering = concatMap makeSQLOrdering
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b ) => SqlOrderable syntax (a, b) where
    makeSQLOrdering (a, b) = makeSQLOrdering a <> makeSQLOrdering b
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c ) => SqlOrderable syntax (a, b, c) where
    makeSQLOrdering (a, b, c) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c
         , SqlOrderable syntax d ) => SqlOrderable syntax (a, b, c, d) where
    makeSQLOrdering (a, b, c, d) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c
         , SqlOrderable syntax d
         , SqlOrderable syntax e ) => SqlOrderable syntax (a, b, c, d, e) where
    makeSQLOrdering (a, b, c, d, e) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d <> makeSQLOrdering e

-- -- | Order by certain expressions, either ascending ('asc_') or descending ('desc_')
-- orderBy :: (SqlOrderable (Sql92SelectOrderingSyntax syntax) ordering, IsQuery q) =>
--            (a -> ordering) -> q syntax db s a -> TopLevelQ syntax db s a
-- orderBy orderer q =
--     TopLevelQ $
--     do res <- toQ q
--        let ordering = makeSQLOrdering (orderer res)
--        modify $ \qb -> qb { qbOrdering = qbOrdering qb <> ordering }
--        pure res

-- desc_, asc_ :: forall syntax s a.
--   IsSql92OrderingSyntax syntax => QExpr (Sql92OrderingExpressionSyntax syntax) s a -> syntax
-- asc_ (QExpr e) = ascOrdering e
-- desc_ (QExpr e) = descOrdering e

-- * Subqueries

class Subqueryable syntax a s | a -> s, a -> syntax where
    type Unnested a s
    subqueryProjections :: Proxy s -> a
                        -> RWS (Text, Int) [ (syntax, Maybe Text) ] Int
                               (Unnested a s)
instance (IsSql92ExpressionSyntax syntax, Beamable t) => Subqueryable syntax (t (QExpr syntax (QNested s))) s where
  type Unnested (t (QExpr syntax (QNested s))) s = t (QExpr syntax s)

  subqueryProjections s a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ -> do
                       i <- state (\i -> (i, i + 1))
                       let fieldName = fromString ("e" <> show i)
                       (tblName, tblOrd) <- ask
                       tell [(e, Just fieldName)]
                       pure (Columnar' (QExpr (fieldE (qualifiedField (fromString ("t" <> show tblOrd)) fieldName)))))
                       a a
instance IsSql92ExpressionSyntax syntax => Subqueryable syntax (QExpr syntax (QNested s) a) s where
    type Unnested (QExpr syntax (QNested s) a) s = QExpr syntax s a
    subqueryProjections s (QExpr e) =
        do i <- state (\i -> (i, i+1))
           (tblName, tblOrd) <- ask
           let fieldName = fromString ("e" <> show i)
           tell [(e, Just fieldName)]
           pure (QExpr (fieldE (qualifiedField (fromString ("t" <> show tblOrd)) fieldName)))

instance ( Subqueryable syntax a s
         , Subqueryable syntax b s ) =>
    Subqueryable syntax (a, b) s where
    type Unnested (a, b) s = (Unnested a s, Unnested b s)
    subqueryProjections s (a, b) =
        (,) <$> subqueryProjections s a
            <*> subqueryProjections s b
instance ( Subqueryable syntax a s
         , Subqueryable syntax b s
         , Subqueryable syntax c s ) =>
    Subqueryable syntax (a, b, c) s where
    type Unnested (a, b, c) s = (Unnested a s, Unnested b s, Unnested c s)
    subqueryProjections s (a, b, c) =
        (,,) <$> subqueryProjections s a
             <*> subqueryProjections s b
             <*> subqueryProjections s c
instance ( Subqueryable syntax a s
         , Subqueryable syntax b s
         , Subqueryable syntax c s
         , Subqueryable syntax d s ) =>
    Subqueryable syntax (a, b, c, d) s where
    type Unnested (a, b, c, d) s = (Unnested a s, Unnested b s, Unnested c s, Unnested d s)
    subqueryProjections s (a, b, c, d) =
        (,,,) <$> subqueryProjections s a
              <*> subqueryProjections s b
              <*> subqueryProjections s c
              <*> subqueryProjections s d
instance ( Subqueryable syntax a s
         , Subqueryable syntax b s
         , Subqueryable syntax c s
         , Subqueryable syntax d s
         , Subqueryable syntax e s ) =>
    Subqueryable syntax (a, b, c, d, e) s where
    type Unnested (a, b, c, d, e) s = (Unnested a s, Unnested b s, Unnested c s, Unnested d s, Unnested e s)
    subqueryProjections s (a, b, c, d, e) =
        (,,,,) <$> subqueryProjections s a
               <*> subqueryProjections s b
               <*> subqueryProjections s c
               <*> subqueryProjections s d
               <*> subqueryProjections s e

type family Unnest expr :: *
type instance Unnest (QExpr be (QNested s) a) = QExpr be s a
type instance Unnest (t (QExpr be (QNested s))) = t (QExpr be s)
type instance Unnest (a, b) = (Unnest a, Unnest b)
type instance Unnest (a, b, c) = (Unnest a, Unnest b, Unnest c)
type instance Unnest (a, b, c, d) = (Unnest a, Unnest b, Unnest c, Unnest d)
type instance Unnest (a, b, c, d, e) = (Unnest a, Unnest b, Unnest c, Unnest d, Unnest e)
type instance Unnest (a, b, c, d, e, f) = (Unnest a, Unnest b, Unnest c, Unnest d, Unnest e, Unnest f)

sourceSelect_ :: forall q select db s a.
                 ( IsQuery q
                 , Coercible a (Unnest a)
                 , IsSql92SelectSyntax select
                 , ProjectibleInSelectSyntax select a
                 , Reprojectable (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax select))) s (Unnest a)
                 , Sql92TableSourceSelectSyntax
                   (Sql92FromTableSourceSyntax
                    (Sql92SelectTableFromSyntax
                     (Sql92SelectSelectTableSyntax select))) ~ select ) =>
                 q select db (QNested s) a -> Q select db s (Unnest a)
sourceSelect_ q =
  -- Some SelectBuilders can be easily turned back to selects. See if this is one
  case coerce (toSelectBuilder q) :: SelectBuilder select db s a of
    SelectBuilderQ mkTbl -> coerce mkTbl :: Q select db s (Unnest a)
    sel@SelectBuilderSelectSyntax {} ->  sourceSelect_ (SelectBuilderTopLevel Nothing Nothing [] (coerce sel :: SelectBuilder select db (QNested s) a))
    SelectBuilderTopLevel { sbLimit = limit, sbOffset = offset
                          , sbOrdering = ordering, sbTable = lowerTbl } ->
      do curTbl <- state (\qb@QueryBuilder { qbNextTblRef } -> (qbNextTblRef, qb { qbNextTblRef = qbNextTblRef + 1 }) )
         let res :: a
             (res, selectTbl) = selectBuilderToSelectTable lowerTbl -- (coerce lowerTbl :: SelectBuilder (QuerySelectSyntax q) (QueryDb q) s (Unnest (QueryResult q)))
             subTblNm = fromString ("t" <> show curTbl)
         modify $ \qb@QueryBuilder { qbFrom = from } ->
                   let from' = case from of
                                 Nothing -> Just newSource
                                 Just from -> Just (innerJoin from newSource Nothing)
                       newSource = fromTable (tableFromSubSelect (selectStmt (coerce selectTbl) ordering limit offset)) (Just subTblNm)
                   in qb { qbFrom = from' }
         pure (evalState (reproject (\i -> fieldE (qualifiedField subTblNm (fromString ("res" <> show i))))
                                    (coerce res :: Unnest a)) 0)

-- -- | Run the given 'Q'-like object as a subquery, joining the results with the current result
-- -- set. This allows embedding of 'TopLevelQ's inside 'Q's or other 'TopLevelQ's.
-- subquery_ :: (IsQuery q, Projectible be a, Subqueryable be a s, BeamSqlBackend be) => q be db (QNested s) a -> Q be db s (Unnested a s)
-- subquery_ (q :: q be db (QNested s) a) =
--     do curTbl <- gets qbNextTblRef

--        let (res, curTbl', select') = queryToSQL' (toQ q) curTbl

--            subTblName = fromString ("t" <> show curTbl)
--            (res', projection') = evalRWS (subqueryProjections (Proxy :: Proxy s) res) (subTblName, curTbl') 0

--            select'' = select' { selProjection = SQLProj projection' }

--        modify $ \qb@QueryBuilder { qbFrom = from } ->
--                  let from' = case from of
--                                Nothing -> Just newSource
--                                Just from -> Just (SQLJoin SQLInnerJoin from newSource (SQLValE (SQLValue True)))
--                      newSource = SQLFromSource (SQLAliased (SQLSourceSelect select'') (Just (fromString ("t" <> show curTbl'))))
--                  in qb { qbNextTblRef = curTbl' + 1
--                        , qbFrom = from' }

--        pure res'

-- * Nullable conversions

-- | Type class for things that can be nullable. This includes 'QExpr (Maybe a)', 'tbl (Nullable
-- QExpr)', and 'PrimaryKey tbl (Nullable QExpr)'
class SqlJustable a b | b -> a where

    -- | Given something of type 'QExpr a', 'tbl QExpr', or 'PrimaryKey tbl QExpr', turn it into a
    -- 'QExpr (Maybe a)', 'tbl (Nullable QExpr)', or 'PrimaryKey t (Nullable QExpr)' respectively
    -- that contains the same values.
    just_ :: a -> b

    -- | Return either a 'QExpr (Maybe x)' representing 'Nothing' or a nullable 'Table' or
    -- 'PrimaryKey' filled with 'Nothing'.
    nothing_ :: b

instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SQLNull) =>
    SqlJustable (QExpr syntax s a) (QExpr syntax s (Maybe a)) where

    just_ (QExpr e) = QExpr e
    nothing_ = QExpr (valueE (sqlValueSyntax SQLNull))

instance {-# OVERLAPPING #-} ( Table t
                             , IsSql92ExpressionSyntax syntax
                             , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SQLNull ) =>
    SqlJustable (PrimaryKey t (QExpr syntax s)) (PrimaryKey t (Nullable (QExpr syntax s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' nothing_) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} ( Table t
                             , IsSql92ExpressionSyntax syntax
                             , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SQLNull ) =>
    SqlJustable (t (QExpr syntax s)) (t (Nullable (QExpr syntax s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' nothing_) (tblSkeleton :: TableSkeleton t)

instance {-# OVERLAPPING #-} Table t => SqlJustable (PrimaryKey t Identity) (PrimaryKey t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' Nothing) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} Table t => SqlJustable (t Identity) (t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' Nothing) (tblSkeleton :: TableSkeleton t)

-- * Nullable checking

-- | Type class for anything which can be checked for null-ness. This includes 'QExpr (Maybe a)' as
-- well as 'Table's or 'PrimaryKey's over 'Nullable QExpr'.
class IsSql92ExpressionSyntax syntax => SqlDeconstructMaybe syntax a nonNullA s | a s -> syntax, a -> nonNullA, a -> s, nonNullA -> s where
    -- | Returns a 'QExpr' that evaluates to true when the first argument is not null
    isJust_ :: a -> QExpr syntax s Bool

    -- | Returns a 'QExpr' that evaluates to true when the first argument is null
    isNothing_ :: a -> QExpr syntax s Bool

    -- | Given an object (third argument) which may or may not be null, return the default value if
    -- null (first argument), or transform the value that could be null to yield the result of the
    -- expression (second argument)
    maybe_ :: QExpr syntax s y -> (nonNullA -> QExpr syntax s y) -> a -> QExpr syntax s y

instance IsSql92ExpressionSyntax syntax => SqlDeconstructMaybe syntax (QExpr syntax s (Maybe x)) (QExpr syntax s x) s where
    isJust_ (QExpr x) = QExpr (isNotNullE x)
    isNothing_ (QExpr x) = QExpr (isNullE x)

    maybe_ (QExpr onNothing) onJust (QExpr e) = let QExpr onJust' = onJust (QExpr e)
                                                in QExpr (caseE [(isNotNullE e, onJust')] onNothing)

-- instance {-# OVERLAPPING #-} ( BeamBackend be, Table t
--                              , AllBeamValues t (QExprBool be s)
--                              , HasBeamFields t (Nullable (QExpr be s)) (Nullable (QExpr be s)) (QExprBool be s) )
--     => SqlDeconstructMaybe be (PrimaryKey t (Nullable (QExpr be s))) (PrimaryKey t (QExpr be s)) s where
--     isJust_ pk = let fieldsAreJust = changeBeamRep (\(Columnar' x) -> Columnar' (QExprBool (isJust_ x))) pk :: PrimaryKey t (QExprBool be s)
--                  in foldr (&&.) (QExpr (SQLValE (sqlBool True))) (allBeamValues (\(Columnar' (QExprBool e)) -> e) fieldsAreJust)
--     isNothing_ pk = let fieldsAreNothing = changeBeamRep (\(Columnar' x) -> Columnar' (QExprBool (isNothing_ x))) pk :: PrimaryKey t (QExprBool be s)
--                     in foldr (&&.) (QExpr (SQLValE (sqlBool True))) (allBeamValues (\(Columnar' (QExprBool e)) -> e) fieldsAreNothing)
--     maybe_ = undefined

instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable t )
    => SqlDeconstructMaybe syntax (t (Nullable (QExpr syntax s))) (t (QExpr syntax s)) s where
    isJust_ t = allE (allBeamValues (\(Columnar' e) -> isJust_ e) t)
    isNothing_ t = allE (allBeamValues (\(Columnar' e) -> isNothing_ e) t)
    maybe_ = undefined

-- class BeamUnwrapMaybe c where
--     beamUnwrapMaybe :: Columnar' (Nullable c) x -> Columnar' c x
-- instance BeamUnwrapMaybe (QExpr syntax s) where
--     beamUnwrapMaybe (Columnar' (QExpr e)) = Columnar' (QExpr e)
-- instance BeamUnwrapMaybe c => BeamUnwrapMaybe (Nullable c) where
--     beamUnwrapMaybe (Columnar' x :: Columnar' (Nullable (Nullable c)) x) =
--         let Columnar' x' = beamUnwrapMaybe (Columnar' x :: Columnar' (Nullable c) (Maybe x)) :: Columnar' c (Maybe x)

--             xCol :: Columnar' (Nullable c) x
--             xCol = Columnar' x'
--         in xCol

-- instance {-# OVERLAPPING #-} ( SqlDeconstructMaybe be (PrimaryKey t (Nullable c)) res s, Table t, BeamUnwrapMaybe (Nullable c)) => SqlDeconstructMaybe be (PrimaryKey t (Nullable (Nullable c))) res s where
--     isJust_ t = isJust_ (changeBeamRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
--     isNothing_ t = isNothing_ (changeBeamRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
--     maybe_ = undefined

-- instance ( SqlDeconstructMaybe syntax (t (Nullable c)) res s, BeamUnwrapMaybe (Nullable c)
--          , Beamable t) => SqlDeconstructMaybe syntax (t (Nullable (Nullable c))) res s where
--     isJust_ t = isJust_ (changeBeamRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
--     isNothing_ t = isNothing_ (changeBeamRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
--     maybe_ = undefined
