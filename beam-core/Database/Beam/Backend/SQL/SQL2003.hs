-- | Modular finally tagless extension of SQL99 and SQL92 syntaxes for various
--   SQL2003 core and optional features.
module Database.Beam.Backend.SQL.SQL2003
    ( module Database.Beam.Backend.SQL.SQL99

    , IsSql2003FromSyntax(..)
    , IsSql2003OrderingElementaryOLAPOperationsSyntax(..)
    , IsSql2003ExpressionSyntax(..)
    , IsSql2003ExpressionElementaryOLAPOperationsSyntax(..)
    , IsSql2003ExpressionAdvancedOLAPOperationsSyntax(..)
    , IsSql2003BinaryAndVarBinaryDataTypeSyntax(..)
    , IsSql2003WindowFrameSyntax(..)
    , IsSql2003WindowFrameBoundsSyntax(..)
    , IsSql2003WindowFrameBoundSyntax(..)
    , IsSql2003EnhancedNumericFunctionsExpressionSyntax(..)
    , IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax(..)
    , IsSql2003FirstValueAndLastValueExpressionSyntax(..)
    , IsSql2003NtileExpressionSyntax(..)
    , IsSql2003NthValueExpressionSyntax(..)
    , IsSql2003LeadAndLagExpressionSyntax(..)
    , IsSql2008BigIntDataTypeSyntax(..)

    , Sql2003SanityCheck
    ) where

import Database.Beam.Backend.SQL.SQL99

import Data.Kind (Type)
import Data.Text (Text)

type Sql2003SanityCheck syntax =
    ( Sql92ExpressionSyntax syntax ~ Sql2003WindowFrameExpressionSyntax (Sql2003ExpressionWindowFrameSyntax (Sql92ExpressionSyntax syntax))
    , Sql92SelectOrderingSyntax (Sql92SelectSyntax syntax) ~
      Sql2003WindowFrameOrderingSyntax (Sql2003ExpressionWindowFrameSyntax (Sql92ExpressionSyntax syntax))
    )

class IsSql92FromSyntax from =>
    IsSql2003FromSyntax from where

    type Sql2003FromSampleMethodSyntax from :: Type

    fromTableSample :: Sql92FromTableSourceSyntax from
                    -> Sql2003FromSampleMethodSyntax from
                    -> Maybe Double
                    -> Maybe Integer
                    -> Maybe Text
                    -> from

-- | Optional SQL2003 "Elementary OLAP operations" T611 support
class IsSql92OrderingSyntax ord =>
    IsSql2003OrderingElementaryOLAPOperationsSyntax ord where
    nullsFirstOrdering, nullsLastOrdering :: ord -> ord

class ( IsSql99ExpressionSyntax expr
      , IsSql2003WindowFrameSyntax (Sql2003ExpressionWindowFrameSyntax expr) ) =>
    IsSql2003ExpressionSyntax expr where

    type Sql2003ExpressionWindowFrameSyntax expr :: Type

    overE :: expr
          -> Sql2003ExpressionWindowFrameSyntax expr
          -> expr
    rowNumberE :: expr

-- | Optional SQL2003 "Advanced OLAP operations" T612 support
class IsSql2003ExpressionSyntax expr =>
  IsSql2003ExpressionAdvancedOLAPOperationsSyntax expr where
  percentRankAggE :: expr
  denseRankAggE :: expr
  cumeDistAggE :: expr

-- | Optional SQL2003 "Elementary OLAP operations" T611 support
class IsSql2003ExpressionSyntax expr =>
  IsSql2003ExpressionElementaryOLAPOperationsSyntax expr where

  filterAggE :: expr -> expr -> expr
  rankAggE :: expr

-- | Optional SQL2003 "BINARY AND VARBINARY data type" T021 support
class IsSql99DataTypeSyntax dataType =>
  IsSql2003BinaryAndVarBinaryDataTypeSyntax dataType where
  binaryType :: Maybe Word -> dataType
  varBinaryType :: Maybe Word -> dataType

class IsSql2003WindowFrameBoundsSyntax (Sql2003WindowFrameBoundsSyntax frame) =>
    IsSql2003WindowFrameSyntax frame where
    type Sql2003WindowFrameExpressionSyntax frame :: Type
    type Sql2003WindowFrameOrderingSyntax frame :: Type
    type Sql2003WindowFrameBoundsSyntax frame :: Type

    frameSyntax :: Maybe [Sql2003WindowFrameExpressionSyntax frame]
                -> Maybe [Sql2003WindowFrameOrderingSyntax frame]
                -> Maybe (Sql2003WindowFrameBoundsSyntax frame)
                -> frame

class IsSql2003WindowFrameBoundSyntax (Sql2003WindowFrameBoundsBoundSyntax bounds) =>
    IsSql2003WindowFrameBoundsSyntax bounds where
    type Sql2003WindowFrameBoundsBoundSyntax bounds :: Type
    fromToBoundSyntax :: Sql2003WindowFrameBoundsBoundSyntax bounds
                      -> Maybe (Sql2003WindowFrameBoundsBoundSyntax bounds)
                      -> bounds

class IsSql2003WindowFrameBoundSyntax bound where
    unboundedSyntax :: bound
    nrowsBoundSyntax :: Int -> bound

-- | Optional SQL2003 "Enhanced numeric functions" T621 support
class IsSql99ExpressionSyntax expr =>
   IsSql2003EnhancedNumericFunctionsExpressionSyntax expr where

  lnE, expE, sqrtE, ceilE, floorE :: expr -> expr
  powerE :: expr -> expr -> expr

class IsSql99AggregationExpressionSyntax agg =>
   IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax agg where

  stddevPopE, stddevSampE, varPopE, varSampE
    :: Maybe (Sql92AggregationSetQuantifierSyntax agg) -> agg -> agg

  covarPopE, covarSampE, corrE, regrSlopeE, regrInterceptE, regrCountE,
    regrRSquaredE, regrAvgXE, regrAvgYE, regrSXXE, regrSXYE, regrSYYE ::
    Maybe (Sql92AggregationSetQuantifierSyntax agg) -> agg -> agg -> agg

-- | Optional SQL2003 "NTILE function" T614 support
class IsSql99AggregationExpressionSyntax agg =>
   IsSql2003NtileExpressionSyntax agg where
  ntileE :: agg -> agg

-- | Optional SQL2003 "LEAD and LAG function" T615 support
class IsSql99AggregationExpressionSyntax agg =>
   IsSql2003LeadAndLagExpressionSyntax agg where
  leadE, lagE :: agg -> Maybe agg -> Maybe agg -> agg

-- | Optional SQL2003 "FIRST_VALUE and LAST_VALUE function" T616 support
class IsSql99AggregationExpressionSyntax agg =>
   IsSql2003FirstValueAndLastValueExpressionSyntax agg where
  firstValueE, lastValueE :: agg -> agg

-- | Optional SQL2003 "NTH_VALUE function" T618 support
class IsSql99AggregationExpressionSyntax agg =>
   IsSql2003NthValueExpressionSyntax agg where
  nthValueE :: agg -> agg -> agg

-- | Optional SQL2008 "BIGINT data type" T071 support
class IsSql99DataTypeSyntax dataType =>
  IsSql2008BigIntDataTypeSyntax dataType where
  bigIntType :: dataType
