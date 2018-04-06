module Database.Beam.Query.SQL99
  ( -- * Sub-queries
    distinct_

    -- * Aggregation
  , any_
  , anyOver_
  , some_
  , someOver_
  , every_
  , everyOver_
  ) where

import Database.Beam.Syntax

-- | Use the SQL99 @DISTINCT@ operator to determine if the given query produces a distinct result
distinct_ :: Projectible ExpressionSyntax a => Q db s a -> QExpr s Bool
distinct_ q = QExpr (\tbl -> distinctE (buildSqlQuery tbl q))


-- | SQL @EVERY@, @SOME@, and @ANY@ aggregates. Operates over
-- 'SqlBool' only, as the result can be @NULL@, even if all inputs are
-- known (no input rows).
everyOver_, someOver_, anyOver_
  :: Maybe AggregationSetQuantifierSyntax
  -> QExpr s SqlBool -> QAgg s SqlBool
everyOver_ q (QExpr a) = QExpr (everyE q <$> a)
someOver_  q (QExpr a) = QExpr (someE  q <$> a)
anyOver_   q (QExpr a) = QExpr (anyE   q <$> a)

-- | SQL99 @EVERY(ALL ..)@ function (but without the explicit ALL)
every_ :: QExpr s SqlBool -> QAgg s SqlBool
every_ = everyOver_ allInGroup_

-- | SQL99 @SOME(ALL ..)@ function (but without the explicit ALL)
some_ :: QExpr s SqlBool -> QAgg s SqlBool
some_  = someOver_  allInGroup_

-- | SQL99 @ANY(ALL ..)@ function (but without the explicit ALL)
any_ :: QExpr s SqlBool -> QAgg s SqlBool
any_   = anyOver_   allInGroup_
