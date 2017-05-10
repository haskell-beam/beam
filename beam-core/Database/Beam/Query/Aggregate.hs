module Database.Beam.Query.Aggregate
  ( -- * Aggregates
    -- | See the corresponding
    --   <https://tathougies.github.io/beam/user-guide/queries/aggregates.md manual section>
    --   for more detail

    aggregate_
  , filterWhere_

  , QGroupable(..)

    -- ** General-purpose aggregate functions #gp-agg-funcs#
  , sum_, avg_, min_, max_, count_, countAll_
  , rank_, cumeDist_, percentRank_

  , every_, any_, some_

    -- ** Quantified aggregate functions
    -- | These functions correspond one-to-one with the <#gp-agg-funcs
    --   general-purpose aggregate functions>. However, they each take a
    --   mandatory "set quantifier", which is any of the
    --   <#set-quantifiers set quantifier> values.
  , sumOver_, avgOver_, minOver_, maxOver_, countOver_
  , everyOver_, anyOver_, someOver_

    -- *** Set quantifiers #set-quantifiers#
  , distinctInGroup_, allInGroup_, allInGroupExplicitly_
  ) where

import Database.Beam.Query.Internal

import Database.Beam.Backend.SQL
import Database.Beam.Schema.Tables

import Control.Monad.Writer
import Control.Monad.Free

import Data.Typeable

-- | Compute an aggregate over a query.
--
--   The supplied aggregate projection should return an aggregate expression (an
--   expression containing an aggregate function such as 'count_', 'sum_',
--   'countAll_', etc), a grouping key (specified with the 'group_' function),
--   or a combination of tuples of the above.
--
--   Appropriate instances are provided up to 8-tuples.
--
--   Semantically, all grouping expressions in the projection will be added to a
--   SQL @GROUP BY@ clause and all aggregate expressions will be computed.
--
--   The return value will be the type of the aggregate projection, but
--   transformed to be in the normal value context (i.e., everything will become
--   'QExpr's).
--
--   For usage examples, see
--   <https://tathougies.github.io/beam/user-guide/queries/aggregates/ the manual>.
aggregate_ :: forall select a r db s.
              ( ProjectibleWithPredicate AggregateContext (Sql92SelectExpressionSyntax select) a
              , Projectible (Sql92SelectExpressionSyntax select) r
              , Projectible (Sql92SelectExpressionSyntax select) a

              , ContextRewritable a
              , ThreadRewritable (QNested s) (WithRewrittenContext a QValueContext)

              , IsSql92SelectSyntax select )
           => (r -> a)                  -- ^ Aggregate projection
           -> Q select db (QNested s) r -- ^ Query to aggregate over
           -> Q select db s (WithRewrittenThread (QNested s) s (WithRewrittenContext a QValueContext))
aggregate_ mkAggregation (Q aggregating) =
  Q (liftF (QAggregate mkAggregation' aggregating (rewriteThread (Proxy @s) . rewriteContext (Proxy @QValueContext))))
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
           [] -> (Nothing, agg)
           _ -> (Just (groupByExpressions groupingExprs), agg)

-- | Type class for grouping keys. 'expr' is the type of the grouping key after
--   projection. 'grouped' is the type of the grouping key in the aggregate
--   expression (usually something that contains 'QGenExpr's in the
--   'QGroupingContext').
class QGroupable expr grouped | expr -> grouped, grouped -> expr where
  group_ :: expr -> grouped

-- | 'group_' for simple value expressions.
instance QGroupable (QExpr expr s a) (QGroupExpr expr s a) where
  group_ (QExpr a) = QExpr a

-- | 'group_' for any 'Beamable' type. Adds every field in the type to the
--   grouping key. This is the equivalent of including the grouping expression
--   of each field in the type as part of the aggregate projection
instance Beamable tbl =>
  QGroupable (tbl (QExpr expr s)) (tbl (QGroupExpr expr s)) where
  group_ = changeBeamRep (\(Columnar' (QExpr x)) -> Columnar' (QExpr x))

-- | Compute an aggregate over all values in a group. Corresponds semantically
--   to the @AGG(ALL ..)@ syntax, but doesn't produce an explicit @ALL@. To
--   produce @ALL@ expicitly, see 'allInGroupExplicitly_'.
allInGroup_ :: IsSql92AggregationSetQuantifierSyntax s
            => Maybe s
allInGroup_ = Nothing

-- | Compute an aggregate only over distinct values in a group. Corresponds to
--   the @AGG(DISTINCT ..)@ syntax.
distinctInGroup_ :: IsSql92AggregationSetQuantifierSyntax s
                 => Maybe s
distinctInGroup_ = Just setQuantifierDistinct

-- | Compute an aggregate over all values in a group. Corresponds to the
--   @AGG(ALL ..)@ syntax. Note that @ALL@ is the default for most aggregations,
--   so you don't normally explicitly specify @ALL@. However, if you need to,
--   you can use this function. To be explicit about quantification in the beam
--   query DSL, but not produce an explicit @ALL@, use 'allInGroup_'.
--   'allInGroup_' has the same semantic meaning, but does not produce an
--   explicit @ALL@.
allInGroupExplicitly_ :: IsSql92AggregationSetQuantifierSyntax s
                     => Maybe s
allInGroupExplicitly_ = Just setQuantifierAll

-- ** Aggregations

-- | SQL @MIN(ALL ..)@ function (but without the explicit ALL)
min_ :: ( IsSql92AggregationExpressionSyntax expr, Num a )
     => QExpr expr s a -> QAgg expr s a
min_ = minOver_ allInGroup_

-- | SQL @MAX(ALL ..)@ function (but without the explicit ALL)
max_ :: ( IsSql92AggregationExpressionSyntax expr, Num a )
     => QExpr expr s a -> QAgg expr s a
max_ = maxOver_ allInGroup_

-- | SQL @AVG(ALL ..)@ function (but without the explicit ALL)
avg_ :: ( IsSql92AggregationExpressionSyntax expr, Num a )
     => QExpr expr s a -> QAgg expr s a
avg_ = avgOver_ allInGroup_

-- | SQL @SUM(ALL ..)@ function (but without the explicit ALL)
sum_ :: ( IsSql92AggregationExpressionSyntax expr, Num a )
     => QExpr expr s a -> QAgg expr s a
sum_ = sumOver_ allInGroup_

-- | SQL @COUNT(*)@ function
countAll_ :: IsSql92AggregationExpressionSyntax expr => QAgg expr s Int
countAll_ = QExpr countAllE

-- | SQL @COUNT(ALL ..)@ function (but without the explicit ALL)
count_ :: ( IsSql92AggregationExpressionSyntax expr
          , Integral b ) => QExpr expr s a -> QAgg expr s b
count_ (QExpr over) = QExpr (countE Nothing over)

-- | SQL2003 @CUME_DIST@ function (Requires T612 Advanced OLAP operations support)
cumeDist_ :: IsSql2003ExpressionAdvancedOLAPOperationsSyntax expr
          => QAgg expr s Double
cumeDist_ = QExpr cumeDistAggE

-- | SQL2003 @PERCENT_RANK@ function (Requires T612 Advanced OLAP operations support)
percentRank_ :: IsSql2003ExpressionAdvancedOLAPOperationsSyntax expr
             => QAgg expr s Double
percentRank_ = QExpr percentRankAggE

-- | SQL2003 @RANK@ function (Requires T611 Elementary OLAP operations support)
rank_ :: IsSql2003ExpressionElementaryOLAPOperationsSyntax expr
      => QAgg expr s Int
rank_ = QExpr rankAggE

minOver_, maxOver_, avgOver_, sumOver_
  :: ( IsSql92AggregationExpressionSyntax expr
     , Num a )
  => Maybe (Sql92AggregationSetQuantifierSyntax expr)
  -> QExpr expr s a -> QAgg expr s a
minOver_ q (QExpr a) = QExpr (minE q a)
maxOver_ q (QExpr a) = QExpr (maxE q a)
avgOver_ q (QExpr a) = QExpr (avgE q a)
sumOver_ q (QExpr a) = QExpr (sumE q a)

countOver_
  :: ( IsSql92AggregationExpressionSyntax expr
     , Integral b )
  => Maybe (Sql92AggregationSetQuantifierSyntax expr)
  -> QExpr expr s a -> QAgg expr s b
countOver_ q (QExpr a) = QExpr (countE q a)

everyOver_, someOver_, anyOver_
  :: IsSql99AggregationExpressionSyntax expr
  => Maybe (Sql92AggregationSetQuantifierSyntax expr)
  -> QExpr expr s Bool -> QAgg expr s Bool
everyOver_ q (QExpr a) = QExpr (everyE q a)
someOver_  q (QExpr a) = QExpr (someE q a)
anyOver_   q (QExpr a) = QExpr (anyE  q a)

-- | Support for FILTER (WHERE ...) syntax for aggregates.
--   Part of SQL2003 Advanced OLAP operations feature (T612)
filterWhere_ :: IsSql2003ExpressionElementaryOLAPOperationsSyntax expr
             => QAgg expr s a -> QExpr expr s Bool -> QAgg expr s a
filterWhere_ (QExpr agg) (QExpr cond) = QExpr (filterAggE agg cond)

-- | SQL99 @EVERY(ALL ..)@ function (but without the explicit ALL)
every_ :: IsSql99AggregationExpressionSyntax expr
       => QExpr expr s Bool -> QAgg expr s Bool
every_ = everyOver_ allInGroup_

-- | SQL99 @SOME(ALL ..)@ function (but without the explicit ALL)
some_ :: IsSql99AggregationExpressionSyntax expr
      => QExpr expr s Bool -> QAgg expr s Bool
some_  = someOver_  allInGroup_

-- | SQL99 @ANY(ALL ..)@ function (but without the explicit ALL)
any_ :: IsSql99AggregationExpressionSyntax expr
     => QExpr expr s Bool -> QAgg expr s Bool
any_   = anyOver_   allInGroup_
