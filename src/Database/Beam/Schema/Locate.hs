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
type instance Rename (aName :|: bName) (a :|: b) = Rename aName a :|: Rename bName b
instance (RenameFields aName a, RenameFields bName b) => RenameFields (aName :|: bName) (a :|: b) where
    renameFields (_ :: Proxy (aName :|: bName)) (a :|: b) = renameFields (Proxy :: Proxy aName) a :|: renameFields (Proxy :: Proxy bName) b
infixl 3 :|:

data a :-> b = a :-> b
               deriving (Show, Typeable)

data Embedded name a = Embedded a
                       deriving (Show)
type instance NameFor (Embedded name a) = name
type family EmbeddedSchemaFor a
type instance EmbeddedSchemaFor (Embedded name a) = Embedded name a

type family EmbedIn parent fields where
    EmbedIn parent (a :|: b) = EmbedIn parent a :|: EmbedIn parent b
    EmbedIn parent x = Rename (parent :-> NameFor x) x

class RenameFields name a where
    renameFields :: Proxy name -> a -> Rename name a

-- * Field locating support

-- | Type family for all field types that can be named
type family NameFor t :: *
type family Rename newName a :: *

-- | The L and R datatypes help the system navigate through the nested `:|:` table constructors
data L a = L a
data R a = R a
data M a = M a
data Descend a = Descend a

data Empty = Empty
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

type family HasFieldCheckEmbedded schema parent name where
    HasFieldCheckEmbedded Empty parent name = CheckNamesEqual (NameFor parent) name
    HasFieldCheckEmbedded (Embedded parentName schema) parent (parentName :-> sub) = HasFieldCheck schema sub
    HasFieldCheckEmbedded schema parent name = CheckNamesEqual (NameFor parent) name

type family HasFieldCheck schema name where
    HasFieldCheck (Maybe t) name = HasFieldCheck t name
    HasFieldCheck (a :|: b) name = Find (HasFieldCheck a name) (HasFieldCheck b name) name
    HasFieldCheck (Embedded name schema) (name :-> sub) = HasFieldCheck schema sub
    HasFieldCheck a name = HasFieldCheckEmbedded (EmbeddedSchemaFor a) a name

type family LocateIf leftYes rightYes left right name where
    LocateIf (Duplicates x) r a b name = ErrorMultipleFieldsNamed x
    LocateIf l (Duplicates x) a b name = ErrorMultipleFieldsNamed x

    LocateIf Found Found a b name = ErrorMultipleFieldsNamed name
    LocateIf (NotFound x) (NotFound y) a b name = ErrorNoFieldNamed name
    LocateIf Found (NotFound x) a b name = L (Locate a name)
    LocateIf (NotFound x) Found a b name = R (Locate b name)

type family CheckEmbedded embeddedSchema parent name where
    CheckEmbedded Empty parent name = CheckNamesEqual (NameFor parent) name
    CheckEmbedded (Embedded parentName schema) parent (parentName :-> sub) = Descend (Locate schema sub)
    CheckEmbedded schema parent name = CheckNamesEqual (NameFor parent) name

type family Locate schema name where
    Locate (Maybe t) name = M (Locate t name)
    Locate (a :|: b) name = LocateIf (HasFieldCheck a name) (HasFieldCheck b name) a b name
    Locate a name = CheckEmbedded (EmbeddedSchemaFor a) a name

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

instance Locator a ma => Locator (Maybe a) (M ma) where
    type LocateResult (Maybe a) (M ma) = Maybe (LocateResult a ma)

    locate Nothing _ = Nothing
    locate (Just x) l = Just (locate x (subLocator l))
        where subLocator :: M ma -> ma
              subLocator _ = undefined

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

instance Locator schema a => Locator (Embedded name schema) (Descend a) where
    type LocateResult (Embedded name schema) (Descend a) = LocateResult schema a

    locate (Embedded schema) locator = locate schema (subLocator locator)
        where subLocator :: Descend a -> a
              subLocator _ = undefined

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