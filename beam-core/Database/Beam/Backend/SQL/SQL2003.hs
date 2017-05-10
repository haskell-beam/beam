-- | Modular finally tagless extension of SQL99 and SQL92 syntaxes for various
--   SQL2003 core and optional features.
module Database.Beam.Backend.SQL.SQL2003
    ( module Database.Beam.Backend.SQL.SQL99

    , IsSql2003FromSyntax(..)
    , IsSql2003ExpressionSyntax(..)
    , IsSql2003ExpressionElementaryOLAPOperationsSyntax(..)
    , IsSql2003ExpressionAdvancedOLAPOperationsSyntax(..)
    , IsSql2003WindowFrameSyntax(..)
    , IsSql2003WindowFrameBoundsSyntax(..)
    , IsSql2003WindowFrameBoundSyntax(..)

    , Sql2003ExpressionSanityCheck
    ) where

import Database.Beam.Backend.SQL.SQL99

import Data.Text (Text)

type Sql2003ExpressionSanityCheck syntax =
    ( syntax ~ Sql2003WindowFrameExpressionSyntax (Sql2003ExpressionWindowFrameSyntax syntax) )

class IsSql92FromSyntax from =>
    IsSql2003FromSyntax from where

    type Sql2003FromSampleMethodSyntax from :: *

    fromTableSample :: Sql92FromTableSourceSyntax from
                    -> Sql2003FromSampleMethodSyntax from
                    -> Maybe Double
                    -> Maybe Integer
                    -> Maybe Text
                    -> from

class ( IsSql99ExpressionSyntax expr
      , IsSql2003WindowFrameSyntax (Sql2003ExpressionWindowFrameSyntax expr) ) =>
    IsSql2003ExpressionSyntax expr where

    type Sql2003ExpressionWindowFrameSyntax expr :: *

    overE :: expr
          -> Sql2003ExpressionWindowFrameSyntax expr
          -> expr

-- | Optional SQL2003 "Advanced OLAP operations" T612 support
class IsSql2003ExpressionSyntax expr =>
  IsSql2003ExpressionAdvancedOLAPOperationsSyntax expr where
  percentRankAggE :: expr
  cumeDistAggE :: expr

-- | Optional SQL2003 "Elementary OLAP operations" T611 support
class IsSql2003ExpressionSyntax expr =>
  IsSql2003ExpressionElementaryOLAPOperationsSyntax expr where

  filterAggE :: expr -> expr -> expr
  rankAggE :: expr

class IsSql2003WindowFrameBoundsSyntax (Sql2003WindowFrameBoundsSyntax frame) =>
    IsSql2003WindowFrameSyntax frame where
    type Sql2003WindowFrameExpressionSyntax frame :: *
    type Sql2003WindowFrameOrderingSyntax frame :: *
    type Sql2003WindowFrameBoundsSyntax frame :: *

    frameSyntax :: Maybe [Sql2003WindowFrameExpressionSyntax frame]
                -> Maybe [Sql2003WindowFrameOrderingSyntax frame]
                -> Maybe (Sql2003WindowFrameBoundsSyntax frame)
                -> frame

class IsSql2003WindowFrameBoundSyntax (Sql2003WindowFrameBoundsBoundSyntax bounds) =>
    IsSql2003WindowFrameBoundsSyntax bounds where
    type Sql2003WindowFrameBoundsBoundSyntax bounds :: *
    fromToBoundSyntax :: Sql2003WindowFrameBoundsBoundSyntax bounds
                      -> Maybe (Sql2003WindowFrameBoundsBoundSyntax bounds)
                      -> bounds

class IsSql2003WindowFrameBoundSyntax bound where
    unboundedSyntax :: bound
    nrowsBoundSyntax :: Int -> bound
