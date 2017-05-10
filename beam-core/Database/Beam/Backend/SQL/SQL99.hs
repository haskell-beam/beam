{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

-- | Finally tagless extension of SQL92 syntaxes for SQL99
module Database.Beam.Backend.SQL.SQL99
  ( module Database.Beam.Backend.SQL.SQL92
  , IsSql99ExpressionSyntax(..)
  , IsSql99AggregationExpressionSyntax(..)
  , IsSql99SelectSyntax(..)
  , IsSql99DataTypeSyntax(..) ) where

import Database.Beam.Backend.SQL.SQL92

import Data.Text ( Text )

class IsSql92SelectSyntax select =>
  IsSql99SelectSyntax select

class IsSql92ExpressionSyntax expr =>
  IsSql99ExpressionSyntax expr where

  distinctE :: Sql92ExpressionSelectSyntax expr -> expr
  similarToE :: expr -> expr -> expr

  functionCallE :: expr -> [ expr ] -> expr

  instanceFieldE :: expr -> Text -> expr
  refFieldE :: expr -> Text -> expr

class IsSql92AggregationExpressionSyntax expr =>
  IsSql99AggregationExpressionSyntax expr where
  everyE, someE, anyE :: Maybe (Sql92AggregationSetQuantifierSyntax expr) -> expr -> expr

class IsSql99DataTypeSyntax dataType where
  characterLargeObjectType :: dataType
  binaryLargeObjectType :: dataType
  booleanType :: dataType
  arrayType :: dataType -> Int -> dataType
  rowType :: [ (Text, dataType) ] -> dataType
