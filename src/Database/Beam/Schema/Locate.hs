{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances, MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, RankNTypes #-}
module Database.Beam.Schema.Locate where

import Data.Proxy
import Data.Typeable

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

-- | Schema combinator. Takes two fields or schemas and combines them into a new schema
data a :|: b = a :|: b
               deriving Show
type instance NameFor (a :|: b) = NameFor a :|: NameFor b
infixr 3 :|:

-- * Field locating support

-- | Type family for all field types that can be named
type family NameFor t :: *

-- | The L and R datatypes help the system navigate through the nested `:|:` table constructors
data L a = L a
data R a = R a

data Found = Found
-- | If you see type check errors with this term in them, then GHC could not find the field you're attempting to access in the schema
data ErrorNoFieldNamed name
-- | If you see type check errors with this term, then GHC found multiple fields with the same name in the schema
data ErrorMultipleFieldsNamed name

type NotFound name = ErrorNoFieldNamed name
type Duplicates name = ErrorMultipleFieldsNamed name

type family CheckNamesEqual actual expected where
    CheckNamesEqual name name = Found
    CheckNamesEqual a expected = ErrorNoFieldNamed expected

type family Find a b name :: * where
    Find (NotFound a) (NotFound b) name = ErrorNoFieldNamed name
    Find (NotFound a) Found name = Found
    Find Found (NotFound b) name = Found
    Find Found Found name = ErrorMultipleFieldsNamed name

type family HasFieldCheck schema name where
    HasFieldCheck (a :|: b) name = Find (HasFieldCheck a name) (HasFieldCheck b name) name
    HasFieldCheck a name = CheckNamesEqual (NameFor a) name

type family LocateIf leftYes rightYes left right name where
    LocateIf (Duplicates x) r a b name = ErrorMultipleFieldsNamed x
    LocateIf l (Duplicates x) a b name = ErrorMultipleFieldsNamed x

    LocateIf Found Found a b name = ErrorMultipleFieldsNamed name
    LocateIf (NotFound x) (NotFound y) a b name = ErrorNoFieldNamed name
    LocateIf Found (NotFound x) a b name = L (Locate a name)
    LocateIf (NotFound x) Found a b name = R (Locate b name)

type family Locate schema name where
    Locate (a :|: b) name = LocateIf (HasFieldCheck a name) (HasFieldCheck b name) a b name
    Locate a name = CheckNamesEqual (NameFor a) name

type family LookupFields table names where
    LookupFields schema (name1 :|: name2) = LookupFields schema name1 :|: LookupFields schema name2
    LookupFields schema name = LocateResultField schema (Locate schema name)

type family LocateAll schema fields where
    LocateAll schema (a :|: b) = LocateAll schema a :|: LocateAll schema b
    LocateAll schema a = Locate schema a

type family LocateResultField schema locator where
    LocateResultField (a :|: b) (L la) = LocateResultField a la
    LocateResultField (a :|: b) (R lb) = LocateResultField b lb
    LocateResultField a Found = a

type family WrapFields f schema where
    WrapFields f (a :|: b) = WrapFields f a :|: WrapFields f b
    WrapFields f a = f a

class Locator schema l where
    type LocateResult schema l :: *

    locate :: schema -> l -> LocateResult schema l
    default locate :: schema ~ LocateResult schema l => schema -> l -> schema
    locate schema _ = schema

class SchemaPart a where
    mapSchema :: (forall x. SchemaPart x => x -> f x) -> a -> WrapFields f a
    default mapSchema :: WrapFields f a ~ f a => (forall x. SchemaPart x => x -> f x) -> a -> WrapFields f a
    mapSchema f a = f a

instance Locator a la => Locator (a :|: b) (L la) where
    type LocateResult (a :|: b) (L la) = LocateResult a la

    locate (a :|: b) locator = locate a (subLocator locator)
        where subLocator :: L la -> la
              subLocator _ = undefined

-- * Instances for schema combinator
instance Locator b lb => Locator (a :|: b) (R lb) where
    type LocateResult (a :|: b) (R lb) = LocateResult b lb

    locate (a :|: b) locator = locate b (subLocator locator)
        where subLocator :: R lb -> lb
              subLocator _ = undefined

instance (Locator schema a, Locator schema b) => Locator schema (a :|: b) where
    type LocateResult schema (a :|: b) = LocateResult schema a :|: LocateResult schema b

    locate schema (_ :: (a :|: b)) = locate schema (undefined :: a) :|: locate schema (undefined :: b)

instance (SchemaPart a, SchemaPart b) => SchemaPart (a :|: b) where
    mapSchema f (a :|: b) = mapSchema f a :|: mapSchema f b

getLocator :: Locate schema name ~ locator => schema -> name -> locator
getLocator _ _ = undefined

getLocatorMultiple :: LocateAll schema names ~ locator => schema -> names -> locator
getLocatorMultiple _ _ = undefined

getField' :: (Locate schema name ~ locator
             , Locator schema locator) => schema -> name -> LocateResult schema locator
getField' schema name = locate schema (getLocator schema name)

getFields :: ( LocateAll schema name ~ locator
             , Locator schema locator) => schema -> name -> LocateResult schema locator
getFields schema names = locate schema (getLocatorMultiple schema names)

newtype Gen n = Gen n
type instance NameFor (Gen n) = NameFor n
instance Locator n Found => Locator (Gen n) Found where
    type LocateResult (Gen n) Found = LocateResult n Found
    locate (Gen n) r = locate n r