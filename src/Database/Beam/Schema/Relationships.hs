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
    generateJoinCondition :: relationship -> Scope q (QueryTable (OneToManyDomain relationship)) -> Scope q (QueryTable (OneToManyRange relationship)) -> QExpr q Bool
    default generateJoinCondition :: ( GGenerateJoinCondition (WrapFields (ScopedField q (OneToManyDomain relationship)) (OneToManyDomainKey relationship))
                                                              (WrapFields (ScopedField q (OneToManyRange relationship)) (OneToManyRangeKey relationship))
                                                              (QueryTable (OneToManyDomain relationship)) (QueryTable (OneToManyRange relationship)) q) =>
                                     relationship -> Scope q (QueryTable (OneToManyDomain relationship)) -> Scope q (QueryTable (OneToManyRange relationship)) -> QExpr q Bool
    generateJoinCondition (relationship :: relationship) hasManyS hasOneS =
        coerceQ $ gGenerateJoinCondition hasManyKey hasOneKey (Proxy :: Proxy (QueryTable (OneToManyDomain relationship))) (Proxy :: Proxy (QueryTable (OneToManyRange relationship))) hasManyS hasOneS
        where keys :: ( WrapFields (ScopedField q (OneToManyDomain relationship)) (OneToManyDomainKey relationship)
                      , WrapFields (ScopedField q (OneToManyRange relationship)) (OneToManyRangeKey relationship)
                      , QExpr q Bool -> QExpr q Bool)
              keys = (undefined, undefined, id)
              (hasManyKey,hasOneKey, coerceQ) = keys

class GGenerateJoinCondition aSchema bSchema a b q where
    gGenerateJoinCondition :: aSchema -> bSchema -> Proxy a -> Proxy b -> Scope q a -> Scope q b -> QExpr q Bool
instance (GGenerateJoinCondition a1 b1 a b q,
          GGenerateJoinCondition a2 b2 a b q) => GGenerateJoinCondition (a1 :|: a2) (b1 :|: b2) a b q where
    gGenerateJoinCondition (a1 :|: a2) (b1 :|: b2) a b aTable bTable =
        AndE (gGenerateJoinCondition a1 b1 a b aTable bTable) (gGenerateJoinCondition a2 b2 a b aTable bTable)

coerceFields :: ScopedField q tbl fld -> ScopedField q tbl1 fld1
coerceFields (ScopedField i) = ScopedField i

instance ( LocateResult (Scope q (QueryTable b)) (Locate (Scope q (QueryTable b)) bName) ~ ScopedField q b bName
         , LocateResult (Scope q (QueryTable a)) (Locate (Scope q (QueryTable a)) aName) ~ ScopedField q a aName

         , FieldType (FieldInTable b bName) ~ FieldType (FieldInTable a aName)

         , Table a, Table b
         , Field b bName, Field a aName

         , Locator (Scope q (QueryTable a)) (Locate (Scope q (QueryTable a)) aName)
         , Locator (Scope q (QueryTable b)) (Locate (Scope q (QueryTable b)) bName) ) =>

          GGenerateJoinCondition (ScopedField q a aName) (ScopedField q b bName) (QueryTable a) (QueryTable b) q where
    gGenerateJoinCondition (a :: ScopedField q aTbl aName) (b :: ScopedField q bTbl bName) _ _ aTable bTable =
        (aTable # (fieldNameD (Proxy :: Proxy aTbl) (Proxy :: Proxy aName))) ==#
        (bTable # (fieldNameD (Proxy :: Proxy bTbl) (Proxy :: Proxy bName)))