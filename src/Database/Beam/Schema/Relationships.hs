{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances, MultiParamTypeClasses, DefaultSignatures, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, FunctionalDependencies #-}
module Database.Beam.Schema.Relationships where

import Database.Beam.Schema.Tables
import Database.Beam.Query.Types
import Database.Beam.SQL.Types

import Data.Proxy

-- * Relationships

-- class ( Table subject, Table object -- Both the subject and objects must be tables

--       -- The types of fields that SubjectFields and ObjectFields point to must be of the same type
--       -- , WrapFields TypeOf (LocateResult (FullSchema subject) (LocateAll (FullSchema subject) (SubjectFields subject object relationship))) ~
--       --   WrapFields TypeOf (LocateResult (FullSchema object) (LocateAll (FullSchema object)  (ObjectFields subject object relationship)))
--       ) =>

--     Relationship subject object relationship | subject relationship -> object
--                                              , object  relationship -> subject

--         where

--     type SubjectFields subject object relationship :: *
--     type ObjectFields subject object relationship :: *

--     joinCondition :: relationship -> Proxy subject -> Proxy object -> Scope (QueryTable subject) -> Scope (QueryTable object) -> QExpr Bool
--     default joinCondition :: ( GEquate (LocateResult (Scope (QueryTable subject)) subjectLocations) (LocateResult (Scope (QueryTable object)) objectLocations)
--                              , LocateAll (Scope (QueryTable subject)) (SubjectFields subject object relationship) ~ subjectLocations
--                              , LocateAll (Scope (QueryTable object)) (ObjectFields subject object relationship) ~ objectLocations

--                              , Locator (Scope (QueryTable subject)) subjectLocations
--                              , Locator (Scope (QueryTable object)) objectLocations ) =>
--                              relationship
--                           -> Proxy subject -> Proxy object
--                           -> Scope (QueryTable subject) -> Scope (QueryTable object)
--                           -> QExpr Bool
--     joinCondition _ (_ :: Proxy subject) (_ :: Proxy object) subjectS objectS =
--         gEquate (getFields subjectS subjectLocations) (getFields objectS objectLocations)
--         where locations :: (SubjectFields subject object relationship, ObjectFields subject object relationship)
--               locations = (undefined, undefined)
--               (subjectLocations, objectLocations) = locations

-- class GEquate a b where
--     gEquate :: a -> b -> QExpr Bool
-- instance (GEquate a1 b1, GEquate a2 b2) => GEquate (a1 :|: a2) (b1 :|: b2) where
--     gEquate (a1 :|: a2) (b1 :|: b2) = gEquate a1 b1 `AndE` gEquate a2 b2
-- instance ( Table table1, Table table2
--          , Field table1 field1
--          , Field table2 field2

--          , TypeOf (FieldInTable table1 field1) ~
--            TypeOf (FieldInTable table2 field2) ) =>
--     GEquate (ScopedField table1 field1) (ScopedField table2 field2) where
--     gEquate a b = EqE (FieldE a) (FieldE b)