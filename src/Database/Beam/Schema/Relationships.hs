{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances, MultiParamTypeClasses, DefaultSignatures, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, FunctionalDependencies #-}
module Database.Beam.Schema.Relationships where

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Locate
import Database.Beam.Query.Types
import Database.Beam.Query.SimpleCombinators
import Database.Beam.SQL.Types

import Data.Proxy

-- * Relationships

-- type SchemaFromNames table = WrapFields (FieldInTable table)
-- type FieldTypesFromNames table a = WrapFields FieldType (SchemaFromNames table a)

-- class ( Table (OneToManyRange relationship), Table (OneToManyDomain relationship) ) =>
--     OneToMany relationship -- ^ The name of the relationship we're defining
--         where
--     type OneToManyRange relationship :: *
--     type OneToManyDomain  relationship :: *

--     -- | The key on the hasMany table
--     type OneToManyDomainKey relationship :: *

--     -- | The key on the hasOne table
--     type OneToManyRangeKey relationship :: *

--     -- | Generates the condition that should be used to join these tables, based on the keys used to join
--     generateJoinCondition :: relationship -> Scope (QueryTable (OneToManyDomain relationship)) -> Scope (QueryTable (OneToManyRange relationship)) -> QExpr Bool
--     default generateJoinCondition :: ( GGenerateJoinCondition (WrapFields (ScopedField (OneToManyDomain relationship)) (OneToManyDomainKey relationship))
--                                                               (WrapFields (ScopedField (OneToManyRange relationship)) (OneToManyRangeKey relationship))
--                                                               (QueryTable (OneToManyDomain relationship)) (QueryTable (OneToManyRange relationship))) =>
--                                      relationship -> Scope (QueryTable (OneToManyDomain relationship)) -> Scope (QueryTable (OneToManyRange relationship)) -> QExpr Bool
--     generateJoinCondition (relationship :: relationship) hasManyS hasOneS =
--         gGenerateJoinCondition hasManyKey hasOneKey (Proxy :: Proxy (QueryTable (OneToManyDomain relationship))) (Proxy :: Proxy (QueryTable (OneToManyRange relationship))) hasManyS hasOneS
--         where keys :: ( WrapFields (ScopedField (OneToManyDomain relationship)) (OneToManyDomainKey relationship)
--                       , WrapFields (ScopedField (OneToManyRange relationship)) (OneToManyRangeKey relationship))
--               keys = (undefined, undefined)
--               (hasManyKey,hasOneKey) = keys

-- class GGenerateJoinCondition aSchema bSchema a b where
--     gGenerateJoinCondition :: aSchema -> bSchema -> Proxy a -> Proxy b -> Scope a -> Scope b -> QExpr Bool
-- instance (GGenerateJoinCondition a1 b1 a b,
--           GGenerateJoinCondition a2 b2 a b) => GGenerateJoinCondition (a1 :|: a2) (b1 :|: b2) a b where
--     gGenerateJoinCondition (a1 :|: a2) (b1 :|: b2) a b aTable bTable =
--         AndE (gGenerateJoinCondition a1 b1 a b aTable bTable) (gGenerateJoinCondition a2 b2 a b aTable bTable)

-- instance ( LocateResult (Scope (QueryTable b)) (Locate (Scope (QueryTable b)) bName) ~ ScopedField b bName
--          , LocateResult (Scope (QueryTable a)) (Locate (Scope (QueryTable a)) aName) ~ ScopedField a aName

--          , FieldType (FieldInTable b bName) ~ FieldType (FieldInTable a aName)

--          , Table a, Table b
--          , Field b bName, Field a aName

--          , Locator (Scope (QueryTable a)) (Locate (Scope (QueryTable a)) aName)
--          , Locator (Scope (QueryTable b)) (Locate (Scope (QueryTable b)) bName) ) =>

--           GGenerateJoinCondition (ScopedField a aName) (ScopedField b bName) (QueryTable a) (QueryTable b) where
--     gGenerateJoinCondition (a :: ScopedField aTbl aName) (b :: ScopedField bTbl bName) _ _ aTable bTable =
--         (aTable # (fieldNameD (Proxy :: Proxy aTbl) (Proxy :: Proxy aName))) ==#
--         (bTable # (fieldNameD (Proxy :: Proxy bTbl) (Proxy :: Proxy bName)))

class ( Table subject, Table object -- Both the subject and objects must be tables

      -- The types of fields that SubjectFields and ObjectFields point to must be of the same type
      , WrapFields FieldType (LocateResult (FullSchema subject) (LocateAll (FullSchema subject) (SubjectFields subject object relationship))) ~
        WrapFields FieldType (LocateResult (FullSchema object) (LocateAll (FullSchema object)  (ObjectFields subject object relationship))) ) =>

    Relationship subject object relationship | subject relationship -> object
                                             , object  relationship -> subject

        where

    type SubjectFields subject object relationship :: *
    type ObjectFields subject object relationship :: *

    joinCondition :: relationship -> Proxy subject -> Proxy object -> Scope (QueryTable subject) -> Scope (QueryTable object) -> QExpr Bool
    default joinCondition :: ( GEquate (LocateResult (Scope (QueryTable subject)) subjectLocations) (LocateResult (Scope (QueryTable object)) objectLocations)
                             , LocateAll (Scope (QueryTable subject)) (SubjectFields subject object relationship) ~ subjectLocations
                             , LocateAll (Scope (QueryTable object)) (ObjectFields subject object relationship) ~ objectLocations

                             , Locator (Scope (QueryTable subject)) subjectLocations
                             , Locator (Scope (QueryTable object)) objectLocations ) =>
                             relationship
                          -> Proxy subject -> Proxy object
                          -> Scope (QueryTable subject) -> Scope (QueryTable object)
                          -> QExpr Bool
    joinCondition _ (_ :: Proxy subject) (_ :: Proxy object) subjectS objectS =
        gEquate (getFields subjectS subjectLocations) (getFields objectS objectLocations)
        where locations :: (SubjectFields subject object relationship, ObjectFields subject object relationship)
              locations = (undefined, undefined)
              (subjectLocations, objectLocations) = locations

class GEquate a b where
    gEquate :: a -> b -> QExpr Bool
instance (GEquate a1 b1, GEquate a2 b2) => GEquate (a1 :|: a2) (b1 :|: b2) where
    gEquate (a1 :|: a2) (b1 :|: b2) = gEquate a1 b1 `AndE` gEquate a2 b2
instance ( Table table1, Table table2
         , Field table1 field1
         , Field table2 field2

         , FieldType (FieldInTable table1 field1) ~
           FieldType (FieldInTable table2 field2) ) =>
    GEquate (ScopedField table1 field1) (ScopedField table2 field2) where
    gEquate a b = EqE (FieldE a) (FieldE b)