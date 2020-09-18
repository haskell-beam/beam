module Database.Beam.Query.Aggregate
  ( -- * Aggregates
    -- | See the corresponding
    --   <https://haskell-beam.github.io/beam/user-guide/queries/aggregates.md manual section>
    --   for more detail

    aggregate_
  , filterWhere_, filterWhere_'

  , QGroupable(..)

    -- ** General-purpose aggregate functions #gp-agg-funcs#
  , sum_, avg_, min_, max_, count_, countAll_
  , rank_, cumeDist_, percentRank_, denseRank_
  , rowNumber_

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
import Database.Beam.Query.Operator
import Database.Beam.Query.Ord

import Database.Beam.Backend.SQL
import Database.Beam.Schema.Tables

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Free

import Data.Typeable

type Aggregable be a =
  ProjectibleWithPredicate AggregateContext be (WithExprContext (BeamSqlBackendExpressionSyntax' be)) a

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
--   <https://haskell-beam.github.io/beam/user-guide/queries/aggregates/ the manual>.
aggregate_ :: forall be a r db s.
              ( BeamSqlBackend be
              , Aggregable be a, Projectible be r, Projectible be a

              , ContextRewritable a
              , ThreadRewritable (QNested s) (WithRewrittenContext a QValueContext)
              )
           => (r -> a)                  -- ^ Aggregate projection
           -> Q be db (QNested s) r -- ^ Query to aggregate over
           -> Q be db s (WithRewrittenThread (QNested s) s (WithRewrittenContext a QValueContext))
aggregate_ mkAggregation (Q aggregating) =
  Q (liftF (QAggregate mkAggregation' aggregating (rewriteThread (Proxy @s) . rewriteContext (Proxy @QValueContext))))
  where
    mkAggregation' x tblPfx =
      let agg = mkAggregation x
          doProject :: AggregateContext c
                    => Proxy c -> Proxy be
                    -> WithExprContext (BeamSqlBackendExpressionSyntax' be)
                    -> Writer [WithExprContext (BeamSqlBackendExpressionSyntax' be)]
                              (WithExprContext (BeamSqlBackendExpressionSyntax' be))
          doProject p _ expr =
            case cast p of
              Just (Proxy :: Proxy QGroupingContext) ->
                tell [ expr ] >> pure expr
              Nothing ->
                case cast p of
                  Just (Proxy :: Proxy QAggregateContext) ->
                    pure expr
                  Nothing -> error "aggregate_: impossible"

          groupingExprs =
            fmap (fmap fromBeamSqlBackendExpressionSyntax) $
            execWriter (project' (Proxy @AggregateContext) (Proxy @(be, WithExprContext (BeamSqlBackendExpressionSyntax' be))) doProject agg)
      in case groupingExprs of
           [] -> (Nothing, agg)
           _ -> (Just (groupByExpressions (sequenceA groupingExprs tblPfx)), agg)

-- | Type class for grouping keys. 'expr' is the type of the grouping key after
--   projection. 'grouped' is the type of the grouping key in the aggregate
--   expression (usually something that contains 'QGenExpr's in the
--   'QGroupingContext').
class QGroupable expr grouped | expr -> grouped, grouped -> expr where
  group_ :: expr -> grouped

-- | 'group_' for simple value expressions.
instance QGroupable (QExpr be s a) (QGroupExpr be s a) where
  group_ (QExpr a) = QExpr a

-- | 'group_' for any 'Beamable' type. Adds every field in the type to the
--   grouping key. This is the equivalent of including the grouping expression
--   of each field in the type as part of the aggregate projection
instance Beamable tbl =>
  QGroupable (tbl (QExpr be s)) (tbl (QGroupExpr be s)) where
  group_ = changeBeamRep (\(Columnar' (QExpr x)) -> Columnar' (QExpr x))

-- | 'group_' for any 'Beamable' type. Adds every field in the type to the
--   grouping key. This is the equivalent of including the grouping expression
--   of each field in the type as part of the aggregate projection
instance Beamable tbl =>
  QGroupable (tbl (Nullable (QExpr be s))) (tbl (Nullable (QGroupExpr be s))) where
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
--
--    These functions all return 'Maybe' (except for `count_` and
--    `countAll_`) because empty aggregates return SQL @NULL@ values.

-- | SQL @MIN(ALL ..)@ function (but without the explicit ALL)
min_ :: BeamSqlBackend be
     => QExpr be s a -> QAgg be s (Maybe a)
min_ = minOver_ allInGroup_

-- | SQL @MAX(ALL ..)@ function (but without the explicit ALL)
max_ :: BeamSqlBackend be
     => QExpr be s a -> QAgg be s (Maybe a)
max_ = maxOver_ allInGroup_

-- | SQL @AVG(ALL ..)@ function (but without the explicit ALL)
avg_ :: ( BeamSqlBackend be, Num a )
     => QExpr be s a -> QAgg be s (Maybe a)
avg_ = avgOver_ allInGroup_

-- | SQL @SUM(ALL ..)@ function (but without the explicit ALL)
sum_ :: ( BeamSqlBackend be, Num a )
     => QExpr be s a -> QAgg be s (Maybe a)
sum_ = sumOver_ allInGroup_

-- | SQL @COUNT(*)@ function
countAll_ :: ( BeamSqlBackend be, Integral a ) => QAgg be s a
countAll_ = QExpr (pure countAllE)

-- | SQL @COUNT(ALL ..)@ function (but without the explicit ALL)
count_ :: ( BeamSqlBackend be, Integral b )
       => QExpr be s a -> QAgg be s b
count_ (QExpr over) = QExpr (countE Nothing <$> over)

-- | SQL2003 @CUME_DIST@ function (Requires T612 Advanced OLAP operations support)
cumeDist_ :: BeamSqlT612Backend be
          => QAgg be s Double
cumeDist_ = QExpr (pure cumeDistAggE)

-- | SQL2003 @PERCENT_RANK@ function (Requires T612 Advanced OLAP operations support)
percentRank_ :: BeamSqlT612Backend be
             => QAgg be s Double
percentRank_ = QExpr (pure percentRankAggE)

-- | SQL2003 @DENSE_RANK@ function (Requires T612 Advanced OLAP operations support)
denseRank_ :: ( BeamSqlT612Backend be, Integral a )
           => QAgg be s a
denseRank_ = QExpr (pure denseRankAggE)

-- | SQL2003 @ROW_NUMBER@ function
rowNumber_ :: ( BeamSql2003ExpressionBackend be, Integral a )
           =>  QAgg be s a
rowNumber_ = QExpr (pure rowNumberE)

-- | SQL2003 @RANK@ function (Requires T611 Elementary OLAP operations support)
rank_ :: ( BeamSqlT611Backend be, Integral a )
      => QAgg be s a
rank_ = QExpr (pure rankAggE)

minOver_, maxOver_
  :: BeamSqlBackend be
  => Maybe (BeamSqlBackendAggregationQuantifierSyntax be)
  -> QExpr be s a -> QAgg be s (Maybe a)
minOver_ q (QExpr a) = QExpr (minE q <$> a)
maxOver_ q (QExpr a) = QExpr (maxE q <$> a)

avgOver_, sumOver_
  :: ( BeamSqlBackend be, Num a )
  => Maybe (BeamSqlBackendAggregationQuantifierSyntax be)
  -> QExpr be s a -> QAgg be s (Maybe a)
avgOver_ q (QExpr a) = QExpr (avgE q <$> a)
sumOver_ q (QExpr a) = QExpr (sumE q <$> a)

countOver_
  :: ( BeamSqlBackend be, Integral b )
  => Maybe (BeamSqlBackendAggregationQuantifierSyntax be)
  -> QExpr be s a -> QAgg be s b
countOver_ q (QExpr a) = QExpr (countE q <$> a)

-- | SQL @EVERY@, @SOME@, and @ANY@ aggregates. Operates over
-- 'SqlBool' only, as the result can be @NULL@, even if all inputs are
-- known (no input rows).
everyOver_, someOver_, anyOver_
  :: BeamSql99AggregationBackend be
  => Maybe (BeamSqlBackendAggregationQuantifierSyntax be)
  -> QExpr be s SqlBool -> QAgg be s SqlBool
everyOver_ q (QExpr a) = QExpr (everyE q <$> a)
someOver_  q (QExpr a) = QExpr (someE  q <$> a)
anyOver_   q (QExpr a) = QExpr (anyE   q <$> a)

-- | Support for FILTER (WHERE ...) syntax for aggregates.
--   Part of SQL2003 Elementary OLAP operations feature (T611).
--
-- See 'filterWhere_'' for a version that accepts 'SqlBool'.
filterWhere_ :: BeamSqlT611Backend be
             => QAgg be s a -> QExpr be s Bool -> QAgg be s a
filterWhere_ agg cond = filterWhere_' agg (sqlBool_ cond)

-- | Like 'filterWhere_' but accepting 'SqlBool'.
filterWhere_' :: BeamSqlT611Backend be
              => QAgg be s a -> QExpr be s SqlBool -> QAgg be s a
filterWhere_' (QExpr agg) (QExpr cond) = QExpr (liftA2 filterAggE agg cond)

-- | SQL99 @EVERY(ALL ..)@ function (but without the explicit ALL)
every_ :: BeamSql99AggregationBackend be
       => QExpr be s SqlBool -> QAgg be s SqlBool
every_ = everyOver_ allInGroup_

-- | SQL99 @SOME(ALL ..)@ function (but without the explicit ALL)
some_ :: BeamSql99AggregationBackend be
      => QExpr be s SqlBool -> QAgg be s SqlBool
some_  = someOver_  allInGroup_

-- | SQL99 @ANY(ALL ..)@ function (but without the explicit ALL)
any_ :: BeamSql99AggregationBackend be
     => QExpr be s SqlBool -> QAgg be s SqlBool
any_   = anyOver_   allInGroup_
