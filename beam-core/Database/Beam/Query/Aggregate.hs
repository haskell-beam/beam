module Database.Beam.Query.Aggregate
  ( aggregate_

  , QGroupable(..)

  , sum_, avg_, min_, max_, count_, countAll_
  , rank_, cumeDist_, percentRank_
  , sumOver_, avgOver_, minOver_, maxOver_, countOver_
  , filterWhere_

  , every_, any_, some_
  , everyOver_, anyOver_, someOver_

  , distinctInGroup_, allInGroup_, allInGroupExplicitly_
  ) where

import Database.Beam.Query.Internal

import Database.Beam.Backend.SQL
import Database.Beam.Schema.Tables

import Control.Monad.Writer
import Control.Monad.Free

import Data.Typeable
import Data.Proxy
import Data.Maybe

aggregate_ :: forall select a r db s.
              ( ProjectibleWithPredicate AggregateContext (Sql92SelectExpressionSyntax select) a
              , Projectible (Sql92SelectExpressionSyntax select) r
              , Projectible (Sql92SelectExpressionSyntax select) a

              , ContextRewritable a
              , ThreadRewritable (QNested s) (WithRewrittenContext a QValueContext)

              , IsSql92SelectSyntax select )
           => (r -> a)
           -> Q select db (QNested s) r
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

class QGroupable expr grouped | expr -> grouped, grouped -> expr where
  group_ :: expr -> grouped
instance QGroupable (QExpr expr s a) (QGroupExpr expr s a) where
  group_ (QExpr a) = QExpr a
instance Beamable tbl =>
  QGroupable (tbl (QExpr expr s)) (tbl (QGroupExpr expr s)) where
  group_ = changeBeamRep (\(Columnar' (QExpr x)) -> Columnar' (QExpr x))

min_, max_, avg_, sum_
  :: ( IsSql92AggregationExpressionSyntax expr
     , Num a ) => QExpr expr s a -> QAgg expr s a
sum_ = sumOver_ allInGroup_
avg_ = avgOver_ allInGroup_
min_ = minOver_ allInGroup_
max_ = maxOver_ allInGroup_

countAll_ :: IsSql92AggregationExpressionSyntax expr => QAgg expr s Int
countAll_ = QExpr countAllE

count_ :: ( IsSql92AggregationExpressionSyntax expr
          , Integral b ) => QExpr expr s a -> QAgg expr s b
count_ (QExpr over) = QExpr (countE Nothing over)

allInGroup_, distinctInGroup_, allInGroupExplicitly_
  :: IsSql92AggregationSetQuantifierSyntax s
  => Maybe s
allInGroup_ = Nothing
distinctInGroup_ = Just setQuantifierDistinct
allInGroupExplicitly_ = Just setQuantifierAll

cumeDist_, percentRank_
  :: IsSql2003ExpressionAdvancedOLAPOperationsSyntax expr
  => QAgg expr s Double
cumeDist_ = QExpr cumeDistAggE
percentRank_ = QExpr percentRankAggE

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
filterWhere_ (QExpr agg) (QExpr filter) = QExpr (filterAggE agg filter)

every_, some_, any_
  :: IsSql99AggregationExpressionSyntax expr
  => QExpr expr s Bool -> QAgg expr s Bool
every_ = everyOver_ allInGroup_
some_  = someOver_  allInGroup_
any_   = anyOver_   allInGroup_
