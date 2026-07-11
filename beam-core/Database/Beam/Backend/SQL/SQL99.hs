{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

-- | Finally tagless extension of SQL92 syntaxes for SQL99
module Database.Beam.Backend.SQL.SQL99
  ( module Database.Beam.Backend.SQL.SQL92
  , IsSql99FunctionExpressionSyntax(..)
  , IsSql99ExpressionSyntax(..)
  , IsSql99ConcatExpressionSyntax(..)
  , IsSql99AggregationExpressionSyntax(..)
  , IsSql99CommonTableExpressionSelectSyntax(..)
  , IsSql99CommonTableExpressionSyntax(..)
  , IsSql99DataModifyingCommonTableExpressionSyntax(..)
  , IsSql99RecursiveCommonTableExpressionSelectSyntax(..)
  , IsSql99SelectSyntax(..)
  , IsSql99DataTypeSyntax(..) ) where

import Database.Beam.Backend.SQL.SQL92

import Data.Kind ( Type )
import Data.Text ( Text )

class IsSql92SelectSyntax select =>
  IsSql99SelectSyntax select

class IsSql92ExpressionSyntax expr =>
  IsSql99FunctionExpressionSyntax expr where

  functionCallE :: expr -> [ expr ] -> expr
  functionNameE :: Text -> expr

class IsSql99FunctionExpressionSyntax expr =>
  IsSql99ExpressionSyntax expr where

  distinctE :: Sql92ExpressionSelectSyntax expr -> expr
  similarToE :: expr -> expr -> expr

  instanceFieldE :: expr -> Text -> expr
  refFieldE :: expr -> Text -> expr

class IsSql92ExpressionSyntax expr =>
  IsSql99ConcatExpressionSyntax expr where
  concatE :: [ expr ] -> expr

class IsSql92AggregationExpressionSyntax expr =>
  IsSql99AggregationExpressionSyntax expr where
  everyE, someE, anyE :: Maybe (Sql92AggregationSetQuantifierSyntax expr) -> expr -> expr

class IsSql92DataTypeSyntax dataType =>
  IsSql99DataTypeSyntax dataType where
  characterLargeObjectType :: dataType
  binaryLargeObjectType :: dataType
  booleanType :: dataType
  arrayType :: dataType -> Int -> dataType
  rowType :: [ (Text, dataType) ] -> dataType

class IsSql92SelectSyntax syntax =>
  IsSql99CommonTableExpressionSelectSyntax syntax where
  type Sql99SelectCTESyntax syntax :: Type

  withSyntax :: [ Sql99SelectCTESyntax syntax ] -> syntax -> syntax

class IsSql99CommonTableExpressionSelectSyntax syntax
    => IsSql99RecursiveCommonTableExpressionSelectSyntax syntax where

  withRecursiveSyntax :: [ Sql99SelectCTESyntax syntax ] -> syntax -> syntax

class IsSql99CommonTableExpressionSyntax syntax where
  type Sql99CTESelectSyntax syntax :: Type

  cteSubquerySyntax :: Text -> [Text] -> Sql99CTESelectSyntax syntax -> syntax

-- | Extension of SQL99 common-table-expression syntax for backends that allow
-- a CTE body to be a data-modifying statement rather than a @SELECT@.
--
-- The data-modifying body is kept distinct from 'Sql99CTESelectSyntax' so a
-- backend must opt into this extension explicitly. 'cteDataModifyingSyntax'
-- supplies the CTE name and output column names around a backend-specific
-- statement such as @DELETE ... RETURNING ...@.
class IsSql99CommonTableExpressionSyntax syntax
    => IsSql99DataModifyingCommonTableExpressionSyntax syntax where

  -- | Backend-specific syntax for the statement inside the CTE body.
  type Sql99CTEDataModifyingSyntax syntax :: Type

  -- | Wrap a data-modifying statement as one named CTE definition.
  cteDataModifyingSyntax :: Text -> [Text] -> Sql99CTEDataModifyingSyntax syntax -> syntax
