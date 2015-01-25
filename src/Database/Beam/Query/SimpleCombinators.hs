{-# LANGUAGE ScopedTypeVariables, TypeOperators, GADTs, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.SimpleCombinators where

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Locate
import Database.Beam.SQL.Types

import Database.Beam.Query.Types

import Data.Proxy
import Data.Typeable

(#) :: ( Locate schema name ~ locator
        , Locator schema locator
        , LocateResult schema locator ~ ScopedField q table field
        , FieldType (FieldInTable table field) ~ r
        , Table table, Field table field) =>
        schema -> name -> QExpr q r
s # field = FieldE (getField' s field)

(==#) :: (Typeable a, Show a) => QExpr q a -> QExpr q a -> QExpr q Bool
(==#) = EqE

(&&#), (||#) :: QExpr q Bool -> QExpr q Bool -> QExpr q Bool
(&&#) = AndE
(||#) = OrE

infixr 3 &&#
infixr 2 ||#
infix 4 ==#
infixl 9 #