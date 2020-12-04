module Database.Beam.Query.Extensions
  ( -- * Various combinators corresponding to SQL extensions

    -- ** T614 NTILE function
    ntile_

    -- ** T615 LEAD and LAG function
  , lead1_, lag1_, lead_, lag_
  , leadWithDefault_, lagWithDefault_

    -- ** T616 FIRST_VALUE and LAST_VALUE functions
  ,  firstValue_, lastValue_

    -- ** T618 NTH_VALUE function
  , nthValue_

    -- ** T621 Enhanced numeric functions
  , (**.), ln_, exp_, sqrt_
  , ceiling_, floor_

  , stddevPopOver_, stddevSampOver_
  , varPopOver_, varSampOver_

  , stddevPop_, stddevSamp_
  , varPop_, varSamp_

  , covarPopOver_, covarSampOver_, corrOver_
  , regrSlopeOver_, regrInterceptOver_
  , regrCountOver_, regrRSquaredOver_
  , regrAvgXOver_, regrAvgYOver_
  , regrSXXOver_, regrSYYOver_, regrSXYOver_

  , covarPop_, covarSamp_, corr_, regrSlope_
  , regrIntercept_, regrCount_, regrRSquared_
  , regrAvgX_, regrAvgY_, regrSXX_
  , regrSYY_, regrSXY_
  ) where

import Database.Beam.Query.Internal
import Database.Beam.Query.Aggregate

import Database.Beam.Backend.SQL

ntile_ :: (BeamSqlBackend be, BeamSqlT614Backend be, Integral n)
       => QExpr be s n -> QAgg be s a
ntile_ (QExpr a) = QExpr (ntileE <$> a)

lead1_, lag1_
  :: (BeamSqlBackend be, BeamSqlT615Backend be)
  => QExpr be s a -> QAgg be s a
lead1_ (QExpr a) = QExpr (leadE <$> a <*> pure Nothing <*> pure Nothing)
lag1_ (QExpr a) = QExpr (lagE <$> a <*> pure Nothing <*> pure Nothing)

lead_, lag_
  :: (BeamSqlBackend be, BeamSqlT615Backend be, Integral n)
  => QExpr be s a -> QExpr be s n -> QAgg be s a
lead_ (QExpr a) (QExpr n) = QExpr (leadE <$> a <*> (Just <$> n) <*> pure Nothing)
lag_ (QExpr a) (QExpr n) = QExpr (lagE <$> a <*> (Just <$> n) <*> pure Nothing)

leadWithDefault_, lagWithDefault_
  :: (BeamSqlBackend be, BeamSqlT615Backend be, Integral n)
  => QExpr be s a -> QExpr be s n -> QExpr be s a
  -> QAgg be s a
leadWithDefault_ (QExpr a) (QExpr n) (QExpr def) =
  QExpr (leadE <$> a <*> fmap Just n <*> fmap Just def)
lagWithDefault_ (QExpr a) (QExpr n) (QExpr def) =
  QExpr (lagE <$> a <*> fmap Just n <*> fmap Just def)

-- TODO the first 'a' should be nullable, and the second one not
firstValue_, lastValue_
  :: (BeamSqlBackend be, BeamSqlT616Backend be)
  => QExpr be s a -> QAgg be s a
firstValue_ (QExpr a) = QExpr (firstValueE <$> a)
lastValue_ (QExpr a) = QExpr (lastValueE <$> a)

-- TODO see comment for 'firstValue_' and 'lastValue_'
nthValue_
  :: (BeamSqlBackend be, BeamSqlT618Backend be, Integral n)
  => QExpr be s a -> QExpr be s n -> QAgg be s a
nthValue_ (QExpr a) (QExpr n) = QExpr (nthValueE <$> a <*> n)

ln_, exp_, sqrt_
  :: (Floating a, BeamSqlBackend be, BeamSqlT621Backend be)
  => QGenExpr ctxt be s a -> QGenExpr ctxt be s a
ln_ (QExpr a) = QExpr (lnE <$> a)
exp_ (QExpr a) = QExpr (expE <$> a)
sqrt_ (QExpr a) = QExpr (sqrtE <$> a)

ceiling_, floor_
  :: (RealFrac a, Integral b, BeamSqlBackend be, BeamSqlT621Backend be)
  => QGenExpr ctxt be s a -> QGenExpr ctxt be s b
ceiling_ (QExpr a) = QExpr (ceilE <$> a)
floor_ (QExpr a) = QExpr (floorE <$> a)

infixr 8 **.
(**.) :: (Floating a, BeamSqlBackend be, BeamSqlT621Backend be)
      => QGenExpr ctxt be s a -> QGenExpr ctxt be s a -> QGenExpr ctxt be s a
QExpr a **. QExpr b = QExpr (powerE <$> a <*> b)

stddevPopOver_, stddevSampOver_, varPopOver_, varSampOver_
  :: (Num a, Floating b, BeamSqlBackend be, BeamSqlT621Backend be)
  => Maybe (BeamSqlBackendAggregationQuantifierSyntax be) -> QExpr be s a
  -> QAgg be s b
stddevPopOver_ q (QExpr x) = QExpr (stddevPopE q <$> x)
stddevSampOver_ q (QExpr x) = QExpr (stddevSampE q <$> x)
varPopOver_ q (QExpr x) = QExpr (varPopE q <$> x)
varSampOver_ q (QExpr x) = QExpr (varSampE q <$> x)

stddevPop_, stddevSamp_, varPop_, varSamp_
  :: (Num a, Floating b, BeamSqlBackend be, BeamSqlT621Backend be)
  => QExpr be s a -> QAgg be s b
stddevPop_ = stddevPopOver_ allInGroup_
stddevSamp_ = stddevSampOver_ allInGroup_
varPop_ = varPopOver_ allInGroup_
varSamp_ = varSampOver_ allInGroup_

covarPopOver_, covarSampOver_, corrOver_, regrSlopeOver_, regrInterceptOver_,
  regrCountOver_, regrRSquaredOver_, regrAvgYOver_, regrAvgXOver_,
  regrSXXOver_, regrSXYOver_, regrSYYOver_
  :: (Num a, Floating b, BeamSqlBackend be, BeamSqlT621Backend be)
  => Maybe (BeamSqlBackendAggregationQuantifierSyntax be) -> QExpr be s a -> QExpr be s a
  -> QExpr be s b
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

covarPop_, covarSamp_, corr_, regrSlope_, regrIntercept_, regrCount_,
  regrRSquared_, regrAvgY_, regrAvgX_, regrSXX_, regrSXY_, regrSYY_
  :: (Num a, Floating b, BeamSqlBackend be, BeamSqlT621Backend be)
  => QExpr be s a -> QExpr be s a -> QExpr be s b
covarPop_ = covarPopOver_ allInGroup_
covarSamp_ = covarSampOver_ allInGroup_
corr_ = corrOver_ allInGroup_
regrSlope_ = regrSlopeOver_ allInGroup_
regrIntercept_ = regrInterceptOver_ allInGroup_
regrCount_ = regrCountOver_ allInGroup_
regrRSquared_ = regrRSquaredOver_ allInGroup_
regrAvgY_ = regrAvgYOver_ allInGroup_
regrAvgX_ = regrAvgXOver_ allInGroup_
regrSXX_ = regrSXXOver_ allInGroup_
regrSYY_ = regrSYYOver_ allInGroup_
regrSXY_ = regrSXYOver_ allInGroup_
