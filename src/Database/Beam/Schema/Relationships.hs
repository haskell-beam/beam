{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances, MultiParamTypeClasses, DefaultSignatures, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
module Database.Beam.Schema.Relationships where

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Locate
import Database.Beam.Query.Types
import Database.Beam.Query.SimpleCombinators
import Database.Beam.SQL.Types

import Data.Proxy

-- * Relationships

type SchemaFromNames table = WrapFields (FieldInTable table)
type FieldTypesFromNames table a = WrapFields FieldType (SchemaFromNames table a)

class ( Table (OneToManyRange relationship), Table (OneToManyDomain relationship) ) =>
    OneToMany relationship -- ^ The name of the relationship we're defining
        where
    type OneToManyRange relationship :: *
    type OneToManyDomain  relationship :: *

    -- | The key on the hasMany table
    type OneToManyDomainKey relationship :: *

    -- | The key on the hasOne table
    type OneToManyRangeKey relationship :: *

    -- | Generates the condition that should be used to join these tables, based on the keys used to join
    generateJoinCondition :: relationship -> Scope (QueryTable (OneToManyDomain relationship)) -> Scope (QueryTable (OneToManyRange relationship)) -> QExpr Bool
    default generateJoinCondition :: ( GGenerateJoinCondition (WrapFields (ScopedField (OneToManyDomain relationship)) (OneToManyDomainKey relationship))
                                                              (WrapFields (ScopedField (OneToManyRange relationship)) (OneToManyRangeKey relationship))
                                                              (QueryTable (OneToManyDomain relationship)) (QueryTable (OneToManyRange relationship))) =>
                                     relationship -> Scope (QueryTable (OneToManyDomain relationship)) -> Scope (QueryTable (OneToManyRange relationship)) -> QExpr Bool
    generateJoinCondition (relationship :: relationship) hasManyS hasOneS =
        coerceQ $ gGenerateJoinCondition hasManyKey hasOneKey (Proxy :: Proxy (QueryTable (OneToManyDomain relationship))) (Proxy :: Proxy (QueryTable (OneToManyRange relationship))) hasManyS hasOneS
        where keys :: ( WrapFields (ScopedField (OneToManyDomain relationship)) (OneToManyDomainKey relationship)
                      , WrapFields (ScopedField (OneToManyRange relationship)) (OneToManyRangeKey relationship)
                      , QExpr Bool -> QExpr Bool)
              keys = (undefined, undefined, id)
              (hasManyKey,hasOneKey, coerceQ) = keys

class GGenerateJoinCondition aSchema bSchema a b where
    gGenerateJoinCondition :: aSchema -> bSchema -> Proxy a -> Proxy b -> Scope a -> Scope b -> QExpr Bool
instance (GGenerateJoinCondition a1 b1 a b,
          GGenerateJoinCondition a2 b2 a b) => GGenerateJoinCondition (a1 :|: a2) (b1 :|: b2) a b where
    gGenerateJoinCondition (a1 :|: a2) (b1 :|: b2) a b aTable bTable =
        AndE (gGenerateJoinCondition a1 b1 a b aTable bTable) (gGenerateJoinCondition a2 b2 a b aTable bTable)

coerceFields :: ScopedField tbl fld -> ScopedField tbl1 fld1
coerceFields (ScopedField i) = ScopedField i

instance ( LocateResult (Scope (QueryTable b)) (Locate (Scope (QueryTable b)) bName) ~ ScopedField b bName
         , LocateResult (Scope (QueryTable a)) (Locate (Scope (QueryTable a)) aName) ~ ScopedField a aName

         , FieldType (FieldInTable b bName) ~ FieldType (FieldInTable a aName)

         , Table a, Table b
         , Field b bName, Field a aName

         , Locator (Scope (QueryTable a)) (Locate (Scope (QueryTable a)) aName)
         , Locator (Scope (QueryTable b)) (Locate (Scope (QueryTable b)) bName) ) =>

          GGenerateJoinCondition (ScopedField a aName) (ScopedField b bName) (QueryTable a) (QueryTable b) where
    gGenerateJoinCondition (a :: ScopedField aTbl aName) (b :: ScopedField bTbl bName) _ _ aTable bTable =
        (aTable # (fieldNameD (Proxy :: Proxy aTbl) (Proxy :: Proxy aName))) ==#
        (bTable # (fieldNameD (Proxy :: Proxy bTbl) (Proxy :: Proxy bName)))