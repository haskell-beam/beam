{-# LANGUAGE UndecidableInstances, FunctionalDependencies, TypeApplications, NamedFieldPuns #-}
module Database.Beam.Query.Combinators
    ( all_, join_, guard_, related_, relatedBy_
    , leftJoin_
--    , buildJoinFrom
    , SqlReferences(..)
    , SqlJustable(..)
    , SqlDeconstructMaybe(..)
    , SqlOrderable
    , QIfCond, QIfElse

    , limit_, offset_

    , as_

    , exists_, unique_, distinct_, subquery_

    , union_, unionAll_
    , intersect_, intersectAll_
    , except_, exceptAll_

    , coalesce_, if_, then_, else_
    , between_, like_, similarTo_, position_
    , charLength_, octetLength_, bitLength_
    , isTrue_, isNotTrue_
    , isFalse_, isNotFalse_
    , isUnknown_, isNotUnknown_
--  , overlaps_, nullIf_, cast_
    , (<.), (>.), (<=.), (>=.), (==.), (/=.)
    , (&&.), (||.), not_, div_, mod_
    , (<-.)
    , HaskellLiteralForQExpr
    , SqlValable(..), As(..)

    , over_, frame_, bounds_, fromBound_, noBounds_, noOrder_
    , partitionBy_, withWindow_

    -- * SQL GROUP BY and aggregation
    , aggregate_
    , group_
    , sum_, avg_, min_, max_, count_, countAll_
--    , aggregate, SqlGroupable(..)
--    , sum_, count_

    -- * SQL ORDER BY
    , orderBy_, asc_, desc_
    ) where

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL

import Database.Beam.Query.Internal
import Database.Beam.Query.Types

import Database.Beam.Schema.Tables

import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Free

import Data.Monoid
import Data.String
import Data.Maybe
import Data.Proxy
import Data.Typeable

import GHC.Generics

-- | Introduce all entries of a table into the 'Q' monad
all_ :: forall be (db :: (* -> *) -> *) table select s.
        ( Database db
        , IsSql92SelectSyntax select

        , IsSql92FromSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
        , IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)))
        , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)) ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)

        , Table table )
       => DatabaseEntity be db (TableEntity table)
       -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
all_ tbl =
    Q $ liftF (QAll tbl (\_ -> Nothing) id)

-- | Introduce all entries of a table into the 'Q' monad based on the given SQLExprbuildJoin :: forall db syntax table be s.
join_ :: ( Database db, Table table
         , IsSql92SelectSyntax select
         , IsSql92FromSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
         , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)) ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
         , IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))) ) =>
         DatabaseEntity be db (TableEntity table)
      -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool
      -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
join_ tbl (QExpr on) =
    Q $ liftF (QAll tbl (\_ -> Just on) id)

-- | Introduce a table using a left join. Because this is not an inner join, the resulting table is
-- made nullable. This means that each field that would normally have type 'QExpr x' will now have
-- type 'QExpr (Maybe x)'.
leftJoin_ ::
  forall be db table select s.
  ( Database db, Table table
  , IsSql92SelectSyntax select ) =>
  DatabaseEntity be db (TableEntity table) ->
  (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s) -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool) ->
  Q select db s (table (Nullable (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s)))
leftJoin_ tbl mkOn =
    Q $ liftF (QLeftJoin tbl (\tbl -> let QExpr x = mkOn tbl in Just x) id)

-- | Only allow results for which the 'QExpr' yields 'True'
guard_ :: forall select db s.
          ( IsSql92SelectSyntax select ) =>
          QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool -> Q select db s ()
guard_ (QExpr guardE') =
    Q (liftF (QGuard guardE' ()))
    -- modify $ \qb@QueryBuilder { qbWhere = guardE } ->
  -- qb { qbWhere = case guardE of
  --                  Nothing -> Just guardE'
  --                  Just guardE -> Just $ andE guardE guardE' }

-- | Introduce all entries of the given table which are referenced by the given 'PrimaryKey'
related_ :: forall be db rel select s.
            ( IsSql92SelectSyntax select
            , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select))) Bool
            , Database db, Table rel ) =>
            DatabaseEntity be db (TableEntity rel)
         -> PrimaryKey rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s)
         -> Q select db s (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
related_ relTbl pk =
    Q $ liftF (QAll relTbl (\rel -> let QExpr on = pk ==. primaryKey rel :: QExpr (Sql92SelectExpressionSyntax select) s Bool in Just on) id)

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_ :: forall be db rel select s.
              ( Database db, Table rel
              , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select))) Bool
              , IsSql92SelectSyntax select )
           => DatabaseEntity be db (TableEntity rel)
           -> (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s) ->
                QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool)
           -> Q select db s (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
relatedBy_ relTbl mkOn =
    Q $ liftF (QAll relTbl (\rel -> let QExpr on = mkOn rel  :: QExpr (Sql92SelectExpressionSyntax select) s Bool in Just on) id)

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
limit_ :: ProjectibleInSelectSyntax select a =>
          Integer -> Q select db s a -> Q select db s a
limit_ limit' (Q q) =
  Q (liftF (QLimit limit' q id))

-- | Drop the first `offset'` results.
offset_ :: ProjectibleInSelectSyntax select a =>
           Integer -> Q select db s a -> Q select db s a
offset_ offset' (Q q) =
  Q (liftF (QOffset offset' q id))

-- | Use the SQL exists operator to determine if the given query returns any results
exists_, unique_ ::
  ( IsSql92SelectSyntax select
  , ProjectibleInSelectSyntax select a
  , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select) =>
  Q select db s a
  -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool
exists_ = QExpr . existsE . buildSql92Query
unique_ = QExpr . uniqueE . buildSql92Query
distinct_ ::
  ( IsSql99ExpressionSyntax (Sql92SelectExpressionSyntax select)
  , ProjectibleInSelectSyntax select a
  , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select) =>
  Q select db s a
  -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool
distinct_ = QExpr . distinctE . buildSql92Query

subquery_ ::
  ( IsSql92SelectSyntax select
  , ProjectibleInSelectSyntax select (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a)
  , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select) =>
  Q select (db :: (* -> *) -> *) s (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a)
  -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a
subquery_ =
  QExpr . subqueryE . buildSql92Query

-- ** Combinators for boolean expressions

class IsSql92ExpressionSyntax syntax => SqlOrd syntax a s | a -> syntax where
    (==.), (/=.) :: a -> a -> QExpr syntax s Bool
    a /=. b = not_ (a ==. b)

instance IsSql92ExpressionSyntax syntax => SqlOrd syntax (QExpr syntax s a) s where
    (==.) = qBinOpE (eqE Nothing)
    (/=.) = qBinOpE (neqE Nothing)

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

between_ :: IsSql92ExpressionSyntax syntax =>
            QExpr syntax s a -> QExpr syntax s a -> QExpr syntax s a
         -> QExpr syntax s Bool
between_ (QExpr a) (QExpr min_) (QExpr max_) =
  QExpr (betweenE a min_ max_)

charLength_, octetLength_ ::
  ( IsSqlExpressionSyntaxStringType syntax text
  , IsSql92ExpressionSyntax syntax ) =>
  QExpr syntax s text -> QExpr syntax s Int
charLength_ (QExpr s) = QExpr (charLengthE s)
octetLength_ (QExpr s) = QExpr (octetLengthE s)
bitLength_ ::
  IsSql92ExpressionSyntax syntax =>
  QExpr syntax s SqlBitString -> QExpr syntax s Int
bitLength_ (QExpr x) = QExpr (bitLengthE x)

isTrue_, isNotTrue_,
  isFalse_, isNotFalse_,
  isUnknown_, isNotUnknown_
    :: IsSql92ExpressionSyntax syntax =>
       QExpr syntax s a -> QExpr syntax s Bool
isTrue_ (QExpr s) = QExpr (isTrueE s)
isNotTrue_ (QExpr s) = QExpr (isNotTrueE s)
isFalse_ (QExpr s) = QExpr (isFalseE s)
isNotFalse_ (QExpr s) = QExpr (isNotFalseE s)
isUnknown_ (QExpr s) = QExpr (isUnknownE s)
isNotUnknown_ (QExpr s) = QExpr (isNotUnknownE s)

like_, position_ ::
  ( IsSqlExpressionSyntaxStringType syntax text
  , IsSql92ExpressionSyntax syntax ) =>
  QExpr syntax s text -> QExpr syntax s text -> QExpr syntax s text
like_ (QExpr scrutinee) (QExpr search) =
  QExpr (likeE scrutinee search)
position_ (QExpr needle) (QExpr haystack) =
  QExpr (likeE needle haystack)

similarTo_ ::
  ( IsSqlExpressionSyntaxStringType syntax text
  , IsSql99ExpressionSyntax syntax ) =>
  QExpr syntax s text -> QExpr syntax s text -> QExpr syntax s text
similarTo_ (QExpr scrutinee) (QExpr search) =
  QExpr (similarToE scrutinee search)


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

(<-.) :: IsSql92FieldNameSyntax fieldName
      => QField s a
      -> QExpr expr s a
      -> QAssignment fieldName expr s
QField _ fieldName <-. QExpr expr =
  QAssignment (unqualifiedField fieldName) expr

-- * Combine table sources via UNION, INTERSECT, and EXCEPT

union_, unionAll_, intersect_, intersectAll_, except_, exceptAll_ ::
  forall select db s a.
  ( IsSql92SelectSyntax select
  , Projectible (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) a
  , ProjectibleInSelectSyntax select a ) =>
  Q select db s a -> Q select db s a -> Q select db s a
union_ (Q a) (Q b) = Q (liftF (QUnion False a b id))
unionAll_ (Q a) (Q b) = Q (liftF (QUnion True a b id))
intersect_ (Q a) (Q b) = Q (liftF (QIntersect False a b id))
intersectAll_ (Q a) (Q b) = Q (liftF (QIntersect True a b id))
except_ (Q a) (Q b) = Q (liftF (QExcept False a b id))
exceptAll_ (Q a) (Q b) = Q (liftF (QExcept True a b id))

as_ :: forall a ctxt syntax s. QGenExpr ctxt syntax s a -> QGenExpr ctxt syntax s a
as_ = id

-- * Marshalling between Haskell literals and QExprs

type family HaskellLiteralForQExpr x = a
type instance HaskellLiteralForQExpr (QGenExpr context syntax s a) = a
type instance HaskellLiteralForQExpr (table (QGenExpr context syntax s)) = table Identity
type instance HaskellLiteralForQExpr (As x -> QGenExpr context syntax s x) = x

type family QExprSyntax x where
  QExprSyntax (QGenExpr ctxt syntax s a) = syntax

data As x = As

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a

instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a, IsSql92ExpressionSyntax syntax) =>
  SqlValable (QGenExpr ctxt syntax s a) where

  val_ = QExpr . valueE . sqlValueSyntax
instance ( x ~ QGenExpr context syntax s a
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax (QExprSyntax x)) a
         , IsSql92ExpressionSyntax (QExprSyntax x) ) =>
  SqlValable (As a -> x) where
  val_ x _ = val_ x
instance ( Beamable table
         , IsSql92ExpressionSyntax syntax
         , FieldsFulfillConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)) table ) =>
  SqlValable (table (QGenExpr ctxt syntax s)) where
  val_ tbl =
    let fields :: table (WithConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)))
        fields = to (gWithConstrainedFields (Proxy @(HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)))
                                            (Proxy @(Rep (table Exposed))) (from tbl))
    in changeBeamRep (\(Columnar' (WithConstraint x :: WithConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)) x)) ->
                         Columnar' (QExpr (valueE (sqlValueSyntax x)))) fields

-- * Window functions

noBounds_ :: QFrameBounds syntax
noBounds_ = QFrameBounds Nothing

fromBound_ :: IsSql2003WindowFrameBoundsSyntax syntax
           => QFrameBound (Sql2003WindowFrameBoundsBoundSyntax syntax)
           -> QFrameBounds syntax
fromBound_ start = bounds_ start Nothing

bounds_ :: IsSql2003WindowFrameBoundsSyntax syntax
        => QFrameBound (Sql2003WindowFrameBoundsBoundSyntax syntax)
        -> Maybe (QFrameBound (Sql2003WindowFrameBoundsBoundSyntax syntax))
        -> QFrameBounds syntax
bounds_ (QFrameBound start) end =
    QFrameBounds . Just $
    fromToBoundSyntax start
      (fmap (\(QFrameBound end) -> end) end)

noOrder_ :: Maybe (QOrd syntax s Int)
noOrder_ = Nothing

partitionBy_ :: forall syntax partition s.
                ( Projectible (Sql2003WindowFrameExpressionSyntax syntax) partition
                , IsSql2003WindowFrameSyntax syntax ) =>
                partition -> QWindow syntax s
partitionBy_ p =
  QWindow $
  frameSyntax Nothing (case project p of { [] -> Nothing; xs -> Just xs }) Nothing Nothing --frame_ Nothing p (noOrder_ :: Maybe (QOrd syntax s Int)) noBounds_

frame_ :: ( IsSql2003ExpressionSyntax syntax
          , SqlOrderable (Sql2003WindowFrameOrderingSyntax (Sql2003ExpressionWindowFrameSyntax syntax)) ordering
          , Projectible syntax partition
          , Sql2003ExpressionSanityCheck syntax ) =>
          Maybe (QExpr syntax s Bool) {-^ FILTER -}
       -> partition                   {-^ PARTITION BY -}
       -> Maybe ordering              {-^ ORDER BY -}
       -> QFrameBounds (Sql2003WindowFrameBoundsSyntax (Sql2003ExpressionWindowFrameSyntax syntax)) {-^ RANGE / ROWS -}
       -> QWindow (Sql2003ExpressionWindowFrameSyntax syntax) s
frame_ filter_ partition_ ordering_ (QFrameBounds bounds) =
    QWindow $
    frameSyntax (fmap (\(QExpr e) -> e) filter_)
                (case project partition_ of
                   [] -> Nothing
                   xs -> Just xs)
                (case fmap makeSQLOrdering ordering_ of
                   Nothing -> Nothing
                   Just [] -> Nothing
                   Just xs -> Just xs)
                bounds

over_ :: IsSql2003ExpressionSyntax syntax =>
         QAgg syntax s a -> QWindow (Sql2003ExpressionWindowFrameSyntax syntax) s -> QWindowExpr syntax s a
over_ (QExpr a) (QWindow frame) = QExpr (overE a frame)

withWindow_ :: ( ProjectibleWithPredicate WindowFrameContext (Sql2003ExpressionWindowFrameSyntax (Sql92SelectExpressionSyntax select)) window
               , Projectible (Sql92SelectExpressionSyntax select) r
               , Projectible (Sql92SelectExpressionSyntax select) a
               , ContextRewritable a
               , IsSql92SelectSyntax select)
            => (r -> window) -> (r -> window -> a)
            -> Q select db s r
            -> Q select db s (WithRewrittenContext a QValueContext)
withWindow_ mkWindow mkProjection (Q windowOver)=
  Q (liftF (QWindowOver mkWindow mkProjection windowOver (rewriteContext (Proxy @QValueContext))))

-- * Aggregators

aggregate_ :: forall select a r db s.
              ( ProjectibleWithPredicate AggregateContext (Sql92SelectExpressionSyntax select) a
              , Projectible (Sql92SelectExpressionSyntax select) r

              , ContextRewritable a
              , IsSql92SelectSyntax select )
           => (r -> a)
           -> Q select db s r
           -> Q select db s (WithRewrittenContext a QValueContext)
aggregate_ mkAggregation (Q aggregating) =
  Q (liftF (QAggregate mkAggregation' aggregating (rewriteContext (Proxy @QValueContext) . mkAggregation)))
  where
    mkAggregation' x =
      let agg = mkAggregation x
          doProject :: AggregateContext c => Proxy c -> Sql92SelectExpressionSyntax select
                    -> Writer [Sql92SelectExpressionSyntax select] (Sql92SelectExpressionSyntax select)
          doProject p expr =
            case cast p of
              Just (Proxy :: Proxy QGroupingContext) ->
                tell [ expr ] >> pure expr
              Nothing ->
                case cast p of
                  Just (Proxy :: Proxy QAggregateContext) ->
                    pure expr
                  Nothing -> error "aggregate_: impossible"

          groupingExprs = execWriter (project' (Proxy @AggregateContext) doProject agg)
      in case groupingExprs of
           [] -> Nothing
           _ -> Just $ groupByExpressions groupingExprs

group_ :: QExpr expr s a -> QGroupExpr expr s a
group_ (QExpr a) = QExpr a

min_, max_, avg_, sum_
  :: ( IsSql92AggregationExpressionSyntax expr
     , Num a ) => QExpr expr s a -> QAgg expr s a
sum_ (QExpr over) = QExpr (sumE Nothing over)
avg_ (QExpr over) = QExpr (avgE Nothing over)
min_ (QExpr over) = QExpr (minE Nothing over)
max_ (QExpr over) = QExpr (maxE Nothing over)

countAll_ :: IsSql92AggregationExpressionSyntax expr => QAgg expr s Int
countAll_ = QExpr countAllE

count_ :: ( IsSql92AggregationExpressionSyntax expr
          , Integral b ) => QExpr expr s a -> QAgg expr s b
count_ (QExpr over) = QExpr (countE Nothing over)

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
    makeSQLOrdering :: a -> [ syntax ]
instance SqlOrderable syntax (QOrd syntax s a) where
    makeSQLOrdering (QExpr x) = [x]
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

-- | Order by certain expressions, either ascending ('asc_') or descending ('desc_')
orderBy_ :: ( Projectible (Sql92SelectExpressionSyntax syntax) a
            , SqlOrderable (Sql92SelectOrderingSyntax syntax) ordering ) =>
            (a -> ordering) -> Q syntax db s a -> Q syntax db s a
orderBy_ orderer (Q q) =
    Q (liftF (QOrderBy (makeSQLOrdering . orderer) q id))

desc_, asc_ :: forall syntax s a.
  IsSql92OrderingSyntax syntax =>
  QExpr (Sql92OrderingExpressionSyntax syntax) s a ->
  QOrd syntax s a
asc_ (QExpr e) = QExpr (ascOrdering e)
desc_ (QExpr e) = QExpr (descOrdering e)

-- * Subqueries

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
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SqlNull) =>
    SqlJustable (QExpr syntax s a) (QExpr syntax s (Maybe a)) where

    just_ (QExpr e) = QExpr e
    nothing_ = QExpr (valueE (sqlValueSyntax SqlNull))

instance {-# OVERLAPPING #-} ( Table t
                             , IsSql92ExpressionSyntax syntax
                             , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SqlNull ) =>
    SqlJustable (PrimaryKey t (QExpr syntax s)) (PrimaryKey t (Nullable (QExpr syntax s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' nothing_) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} ( Table t
                             , IsSql92ExpressionSyntax syntax
                             , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SqlNull ) =>
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

data QIfCond expr s a = QIfCond (QExpr expr s Bool) (QExpr expr s a)
newtype QIfElse expr s a = QIfElse (QExpr expr s a)

then_ :: QExpr expr s Bool -> QExpr expr s a -> QIfCond expr s a
then_ cond res = QIfCond cond res

else_ :: QExpr expr s a -> QIfElse expr s a
else_ = QIfElse

if_ :: IsSql92ExpressionSyntax expr =>
       [ QIfCond expr s a ]
    -> QIfElse expr s a
    -> QExpr expr s a
if_ conds (QIfElse (QExpr else_)) =
  QExpr (caseE (map (\(QIfCond (QExpr cond) (QExpr res)) -> (cond, res)) conds) else_)

coalesce_ :: IsSql92ExpressionSyntax expr =>
             [ QExpr expr s (Maybe a) ] -> QExpr expr s a -> QExpr expr s a
coalesce_ qs (QExpr onNull) =
  QExpr (coalesceE (map (\(QExpr q) -> q) qs <> [ onNull ]))

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
