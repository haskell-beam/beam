module Database.Beam.Query.Aggregate
  ( -- * Aggregates
    -- | See the corresponding
    --   <https://tathougies.github.io/beam/user-guide/queries/aggregates.md manual section>
    --   for more detail

    aggregate_

  , QGroupable(..)

    -- ** General-purpose aggregate functions #gp-agg-funcs#
  , sum_, avg_, min_, max_, count_, countAll_

    -- ** Quantified aggregate functions
    -- | These functions correspond one-to-one with the <#gp-agg-funcs
    --   general-purpose aggregate functions>. However, they each take a
    --   mandatory "set quantifier", which is any of the
    --   <#set-quantifiers set quantifier> values.
  , sumOver_, avgOver_, minOver_, maxOver_, countOver_

    -- *** Set quantifiers #set-quantifiers#
  , distinctInGroup_, allInGroup_, allInGroupExplicitly_
  ) where

import Database.Beam.Syntax
import Database.Beam.Query.Internal
import Database.Beam.Query.Operator
import Database.Beam.Query.Ord

import Database.Beam.Backend.SQL
import Database.Beam.Schema.Tables

import Control.Applicative
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
aggregate_ :: forall a r db s.
              ( ProjectibleWithPredicate AggregateContext ExpressionSyntax a
              , Projectible ExpressionSyntax r
              , Projectible ExpressionSyntax a

              , ContextRewritable a
              , ThreadRewritable (QNested s) (WithRewrittenContext a QValueContext) 
              , ContextCompatible a QValueContext
              )
           => (r -> a)                  -- ^ Aggregate projection
           -> Q db (QNested s) r -- ^ Query to aggregate over
           -> Q db s (WithRewrittenThread (QNested s) s (WithRewrittenContext a QValueContext))
aggregate_ mkAggregation (Q aggregating) =
  Q (liftF (QAggregate mkAggregation' aggregating (rewriteThread (Proxy @s) . rewriteContext (Proxy @QValueContext))))
  where
    mkAggregation' x tblPfx =
      let agg = mkAggregation x
          doProject :: AggregateContext c => Proxy c -> WithExprContext ExpressionSyntax
                    -> Writer [WithExprContext ExpressionSyntax]
                              (WithExprContext ExpressionSyntax)
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
           _ -> (Just (groupByExpressions (sequenceA groupingExprs tblPfx)), agg)

-- | Type class for grouping keys. 'expr' is the type of the grouping key after
--   projection. 'grouped' is the type of the grouping key in the aggregate
--   expression (usually something that contains 'QGenExpr's in the
--   'QGroupingContext').
class QGroupable expr grouped | expr -> grouped, grouped -> expr where
  group_ :: expr -> grouped

-- | 'group_' for simple value expressions.
instance QGroupable (QExpr s a) (QGroupExpr s a) where
  group_ (QExpr a) = QExpr a

-- | 'group_' for any 'Beamable' type. Adds every field in the type to the
--   grouping key. This is the equivalent of including the grouping expression
--   of each field in the type as part of the aggregate projection
instance Beamable tbl =>
  QGroupable (tbl (QExpr s)) (tbl (QGroupExpr s)) where
  group_ = changeBeamRep (\(Columnar' (QExpr x)) -> Columnar' (QExpr x))

-- | Compute an aggregate over all values in a group. Corresponds semantically
--   to the @AGG(ALL ..)@ syntax, but doesn't produce an explicit @ALL@. To
--   produce @ALL@ expicitly, see 'allInGroupExplicitly_'.
allInGroup_ :: Maybe AggregationSetQuantifierSyntax
allInGroup_ = Nothing

-- | Compute an aggregate only over distinct values in a group. Corresponds to
--   the @AGG(DISTINCT ..)@ syntax.
distinctInGroup_ :: Maybe SelectSetQuantifierSyntax
distinctInGroup_ = Just setQuantifierDistinct

-- | Compute an aggregate over all values in a group. Corresponds to the
--   @AGG(ALL ..)@ syntax. Note that @ALL@ is the default for most aggregations,
--   so you don't normally explicitly specify @ALL@. However, if you need to,
--   you can use this function. To be explicit about quantification in the beam
--   query DSL, but not produce an explicit @ALL@, use 'allInGroup_'.
--   'allInGroup_' has the same semantic meaning, but does not produce an
--   explicit @ALL@.
allInGroupExplicitly_ :: Maybe SelectSetQuantifierSyntax
allInGroupExplicitly_ = Just setQuantifierAll

-- ** Aggregations
--
--    These functions all return 'Maybe' (except for `count_` and
--    `countAll_`) because empty aggregates return SQL @NULL@ values.

-- | SQL @MIN(ALL ..)@ function (but without the explicit ALL)
min_ :: QExpr s a -> QAgg s (Maybe a)
min_ = minOver_ allInGroup_

-- | SQL @MAX(ALL ..)@ function (but without the explicit ALL)
max_ :: QExpr s a -> QAgg s (Maybe a)
max_ = maxOver_ allInGroup_

-- | SQL @AVG(ALL ..)@ function (but without the explicit ALL)
avg_ :: Num a => QExpr s a -> QAgg s (Maybe a)
avg_ = avgOver_ allInGroup_

-- | SQL @SUM(ALL ..)@ function (but without the explicit ALL)
sum_ :: ( Num a )
     => QExpr s a -> QAgg s (Maybe a)
sum_ = sumOver_ allInGroup_

-- | SQL @COUNT(*)@ function
countAll_ :: QAgg s Int
countAll_ = QExpr (pure countAllE)

-- | SQL @COUNT(ALL ..)@ function (but without the explicit ALL)
count_ :: ( Integral b ) => QExpr s a -> QAgg s b
count_ (QExpr over) = QExpr (countE Nothing <$> over)

minOver_, maxOver_
  :: Maybe AggregationSetQuantifierSyntax
  -> QExpr s a -> QAgg s (Maybe a)
minOver_ q (QExpr a) = QExpr (minE q <$> a)
maxOver_ q (QExpr a) = QExpr (maxE q <$> a)

avgOver_, sumOver_
  :: ( Num a )
  => Maybe AggregationSetQuantifierSyntax
  -> QExpr s a -> QAgg s (Maybe a)
avgOver_ q (QExpr a) = QExpr (avgE q <$> a)
sumOver_ q (QExpr a) = QExpr (sumE q <$> a)

countOver_
  :: ( Integral b )
  => Maybe AggregationSetQuantifierSyntax
  -> QExpr s a -> QAgg s b
countOver_ q (QExpr a) = QExpr (countE q <$> a)
