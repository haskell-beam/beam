{-# LANGUAGE ScopedTypeVariables, TypeOperators, GADTs, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.SimpleCombinators where

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Locate
import Database.Beam.SQL.Types

import Database.Beam.Query.Types

import Data.Proxy
import Data.Typeable
import Data.Maybe

(#) :: ( Locate schema name ~ locator
        , Locator schema locator
        , LocateResult schema locator ~ ScopedField table field
        , TypeOf (FieldInTable table field) ~ r
        , Table table, Field table field) =>
        schema -> name -> QExpr r
s # field = FieldE (getField' s field)

(#?) :: ( Locate schema name ~ locator
        , Locator schema locator
        , LocateResult schema locator ~ Maybe (ScopedField table field)
        , TypeOf (FieldInTable table field) ~ r
        , Table table, Field table field) =>
        schema -> name -> QExpr (Maybe r)
s #? field = MaybeFieldE (getField' s field)

(#!) :: ( Locate schema name ~ locator
        , Locator schema locator
        , LocateResult schema locator ~ ScopedField table field
        , TypeOf (FieldInTable table field) ~ r
        , Table table, Field table field) =>
        schema -> name -> ScopedField table field
s #! field = getField' s field

(<#), (>#), (<=#), (>=#), (==#) :: (Typeable a, Show a) => QExpr a -> QExpr a -> QExpr Bool
(==#) = EqE
(<#) = LtE
(>#) = GtE
(<=#) = LeE
(>=#) = GeE

(&&#), (||#) :: QExpr Bool -> QExpr Bool -> QExpr Bool
(&&#) = AndE
(||#) = OrE

infixr 3 &&#
infixr 2 ||#
infix 4 ==#
infixl 9 #