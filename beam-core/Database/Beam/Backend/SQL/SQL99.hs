module Database.Beam.Backend.SQL.SQL99
  ( module Database.Beam.Backend.SQL.SQL92
  , IsSql99ExpressionSyntax(..)
  , IsSql99SelectSyntax(..) ) where

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

