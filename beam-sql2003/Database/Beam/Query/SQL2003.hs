{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Database.Beam.Query.SQL2003
  ( -- * Window functions
    -- | See the corresponding
    --   <https://tathougies.github.io/beam/user-guide/queries/window-functions manual section> for more.
      frame_, bounds_, unbounded_, nrows_, fromBound_
    , noBounds_, noOrder_, noPartition_
    , partitionBy_, orderPartitionBy_, withWindow_
  ) where

import Control.Applicative
import Data.Proxy
import Control.Monad.Free (liftF)
import Database.Beam.Query
import Database.Beam.Query.Internal
import Database.Beam.Syntax

newtype QWindow s = QWindow (WithExprContext ExpressionSyntax)
newtype QFrameBounds = QFrameBounds (Maybe ExpressionSyntax)

newtype QFrameBound = QFrameBound WindowFrameBoundSyntax

nullsFirst_ :: QOrd s a -> QOrd s a
nullsFirst_ (QExpr e) = QExpr (nullsFirstOrdering <$> e)

nullsLast_ :: QOrd s a -> QOrd s a
nullsLast_ (QExpr e) = QExpr (nullsLastOrdering <$> e)

-- | Produce a window expression given an aggregate function and a window.
over_ :: QAgg s a -> QWindow s -> QWindowExpr s a
over_ (QExpr a) (QWindow frame) = QExpr (overE <$> a <*> frame)


-- | Support for FILTER (WHERE ...) syntax for aggregates.
--   Part of SQL2003 Advanced OLAP operations feature (T612).
--
-- See 'filterWhere_'' for a version that accepts 'SqlBool'.
filterWhere_ :: QAgg s a -> QExpr s Bool -> QAgg s a
filterWhere_ agg cond = filterWhere_' agg (sqlBool_ cond)

-- | Like 'filterWhere_' but accepting 'SqlBool'.
filterWhere_' :: QAgg s a -> QExpr s SqlBool -> QAgg s a
filterWhere_' (QExpr agg) (QExpr cond) = QExpr (liftA2 filterAggE agg cond)

-- | SQL2003 @CUME_DIST@ function (Requires T612 Advanced OLAP operations support)
cumeDist_ :: QAgg s Double
cumeDist_ = QExpr (pure cumeDistAggE)

-- | SQL2003 @PERCENT_RANK@ function (Requires T612 Advanced OLAP operations support)
percentRank_ :: QAgg s Double
percentRank_ = QExpr (pure percentRankAggE)

denseRank_ :: QAgg s Int
denseRank_ = QExpr (pure denseRankAggE)

-- | SQL2003 @RANK@ function (Requires T611 Elementary OLAP operations support)
rank_ :: QAgg s Int
rank_ = QExpr (pure rankAggE)

-- | Specify a window frame with all the options
frame_ :: ( SqlOrderable ordering
          , Projectible ExpressionSyntax partition
          )
       => Maybe partition             {-^ PARTITION BY -}
       -> Maybe ordering              {-^ ORDER BY -}
       -> QFrameBounds {-^ RANGE / ROWS -}
       -> QWindow s
frame_ partition_ ordering_ (QFrameBounds bounds) =
    QWindow $ \tblPfx ->
    frameSyntax (case maybe [] (flip project tblPfx) partition_ of
                   [] -> Nothing
                   xs -> Just xs)
                (case fmap makeSQLOrdering ordering_ of
                   Nothing -> Nothing
                   Just [] -> Nothing
                   Just xs -> Just (sequenceA xs tblPfx))
                bounds

-- * Window functions

noBounds_ :: QFrameBounds
noBounds_ = QFrameBounds Nothing

fromBound_ :: QFrameBound -> QFrameBounds
fromBound_ start = bounds_ start Nothing

bounds_ :: QFrameBound
        -> Maybe QFrameBound 
        -> QFrameBounds
bounds_ (QFrameBound start) end =
    QFrameBounds . Just $
    fromToBoundSyntax start
      (fmap (\(QFrameBound end') -> end') end)

unbounded_ :: QFrameBound
unbounded_ = QFrameBound unboundedSyntax

nrows_ :: Int -> QFrameBound
nrows_ x = QFrameBound (nrowsBoundSyntax x)

stddevPopOver_, stddevSampOver_, varPopOver_, varSampOver_
  :: (Num a, Floating b)
  => Maybe AggregationSetQuantifierSyntax -> QExpr s a
  -> QAgg s b
stddevPopOver_ q (QExpr x) = QExpr (stddevPopE q <$> x)
stddevSampOver_ q (QExpr x) = QExpr (stddevSampE q <$> x)
varPopOver_ q (QExpr x) = QExpr (varPopE q <$> x)
varSampOver_ q (QExpr x) = QExpr (varSampE q <$> x)

stddevPop_, stddevSamp_, varPop_, varSamp_
  :: (Num a, Floating b)
  => QExpr s a -> QAgg s b
stddevPop_ = stddevPopOver_ allInGroup_
stddevSamp_ = stddevSampOver_ allInGroup_
varPop_ = varPopOver_ allInGroup_
varSamp_ = varSampOver_ allInGroup_

covarPopOver_, covarSampOver_, corrOver_, regrSlopeOver_, regrInterceptOver_,
  regrCountOver_, regrRSquaredOver_, regrAvgYOver_, regrAvgXOver_,
  regrSXXOver_, regrSXYOver_, regrSYYOver_
  :: (Num a, Floating b)
  => Maybe AggregationSetQuantifierSyntax -> QExpr s a -> QExpr s a
  -> QExpr s b
covarPopOver_ q (QExpr x) (QExpr y) = QExpr (covarPopE q <$> x <*> y)
covarSampOver_ q (QExpr x) (QExpr y) = QExpr (covarSampE q <$> x <*> y)
corrOver_ q (QExpr x) (QExpr y) = QExpr (corrE q <$> x <*> y)
regrSlopeOver_ q (QExpr x) (QExpr y) = QExpr (regrSlopeE q <$> x <*> y)
regrInterceptOver_ q (QExpr x) (QExpr y) = QExpr (regrInterceptE q <$> x <*> y)
regrCountOver_ q (QExpr x) (QExpr y) = QExpr (regrCountE q <$> x <*> y)
regrRSquaredOver_ q (QExpr x) (QExpr y) = QExpr (regrRSquaredE q <$> x <*> y)
regrAvgYOver_ q (QExpr x) (QExpr y) = QExpr (regrAvgYE q <$> x <*> y)
regrAvgXOver_ q (QExpr x) (QExpr y) = QExpr (regrAvgXE q <$> x <*> y)
regrSXXOver_ q (QExpr x) (QExpr y) = QExpr (regrSXXE q <$> x <*> y)
regrSYYOver_ q (QExpr x) (QExpr y) = QExpr (regrSYYE q <$> x <*> y)
regrSXYOver_ q (QExpr x) (QExpr y) = QExpr (regrSXYE q <$> x <*> y)

ntile_ :: Integral a => QExpr s Int -> QAgg s a
ntile_ (QExpr a) = QExpr (ntileE <$> a)

lead1_, lag1_ :: QExpr s a -> QAgg s a
lead1_ (QExpr a) = QExpr (leadE <$> a <*> pure Nothing <*> pure Nothing)
lag1_ (QExpr a) = QExpr (lagE <$> a <*> pure Nothing <*> pure Nothing)

lead_, lag_ :: QExpr s a -> QExpr s Int -> QAgg s a
lead_ (QExpr a) (QExpr n) = QExpr (leadE <$> a <*> (Just <$> n) <*> pure Nothing)
lag_ (QExpr a) (QExpr n) = QExpr (lagE <$> a <*> (Just <$> n) <*> pure Nothing)

leadWithDefault_, lagWithDefault_
  :: QExpr s a -> QExpr s Int -> QExpr s a
  -> QAgg s a
leadWithDefault_ (QExpr a) (QExpr n) (QExpr def) =
  QExpr (leadE <$> a <*> fmap Just n <*> fmap Just def)
lagWithDefault_ (QExpr a) (QExpr n) (QExpr def) =
  QExpr (lagE <$> a <*> fmap Just n <*> fmap Just def)

-- TODO the first 'a' should be nullable, and the second one not
firstValue_, lastValue_ :: QExpr s a -> QAgg s a
firstValue_ (QExpr a) = QExpr (firstValueE <$> a)
lastValue_ (QExpr a) = QExpr (lastValueE <$> a)

-- TODO see comment for 'firstValue_' and 'lastValue_'
nthValue_ :: QExpr s a -> QExpr s Int -> QAgg s a
nthValue_ (QExpr a) (QExpr n) = QExpr (nthValueE <$> a <*> n)

ln_, exp_, sqrt_ :: (Floating a, ExpressionContext ctxt)
                 => QGenExpr ctxt s a -> QGenExpr ctxt s a
ln_ (QExpr a) = QExpr (lnE <$> a)
exp_ (QExpr a) = QExpr (expE <$> a)
sqrt_ (QExpr a) = QExpr (sqrtE <$> a)

ceiling_, floor_ :: (RealFrac a, Integral b, ExpressionContext ctxt)
                 => QGenExpr ctxt s a -> QGenExpr ctxt s b
ceiling_ (QExpr a) = QExpr (ceilE <$> a)
floor_ (QExpr a) = QExpr (floorE <$> a)

infixr 8 **.
(**.) :: (Floating a, ExpressionContext ctxt)
      => QGenExpr ctxt s a -> QGenExpr ctxt s a -> QGenExpr ctxt s a
QExpr a **. QExpr b = QExpr (powerE <$> a <*> b)

-- | Compute a query over windows.
--
--   The first function builds window frames using the 'frame_', 'partitionBy_',
--   etc functions. The return type can be a single frame, tuples of frame, or
--   any arbitrarily nested tuple of the above. Instances up to 8-tuples are
--   provided.
--
--   The second function builds the resulting projection using the result of the
--   subquery as well as the window frames built in the first function. In this
--   function, window expressions can be included in the output using the
--   'over_' function.
--
withWindow_ :: forall window a s r db.
               ( ProjectibleWithPredicate WindowFrameContext ExpressionSyntax window
               , Projectible ExpressionSyntax r
               , Projectible ExpressionSyntax a
               , ContextRewritable a
               , ThreadRewritable (QNested s) (WithRewrittenContext a QValueContext)
               , ContextCompatible a QValueContext
               )
            => (r -> window)      -- ^ Window builder function
            -> (r -> window -> a) -- ^ Projection builder function. Has access to the windows generated above
            -> Q db (QNested s) r -- ^ Query to window over
            -> Q db s (WithRewrittenThread (QNested s) s (WithRewrittenContext a QValueContext))
withWindow_ mkWindow mkProjection (Q windowOver)=
  Q (liftF (QWindowOver mkWindow mkProjection windowOver (rewriteThread (Proxy @s) . rewriteContext (Proxy @QValueContext))))

noPartition_ :: Maybe (QOrd s Int)
noPartition_ = Nothing

noOrder_ :: Maybe (QOrd s Int)
noOrder_ = Nothing

partitionBy_, orderPartitionBy_ :: partition -> Maybe partition
partitionBy_  = Just
orderPartitionBy_ = Just
