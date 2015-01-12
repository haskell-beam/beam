{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances, MultiParamTypeClasses #-}
module Test where

data a :|: b = a :|: b
data TextSchema a = TextSchema String

data L a = L a
data R a = R a
data NotFound = NotFound
data Found = Found
data Duplicates = Duplicates

type family Find a b :: * where
    Find NotFound NotFound = NotFound
    Find NotFound Found = Found
    Find Found NotFound = Found
    Find Found Found = Duplicates

type family HasFieldCheck schema name where
    HasFieldCheck (a :|: b) name = Find (HasFieldCheck a name) (HasFieldCheck b name)
    HasFieldCheck (TextSchema name) name = Found
    HasFieldCheck a name = NotFound

type family LocateIf leftYes rightYes left right name where
    LocateIf Found Found a b name = Duplicates
    LocateIf NotFound NotFound a b name = NotFound
    LocateIf Found NotFound a b name = L (Locate a name)
    LocateIf NotFound Found a b name = R (Locate b name)

type family Locate schema name where
    Locate (a :|: b) name = LocateIf (HasFieldCheck a name) (HasFieldCheck b name) a b name
    Locate (TextSchema name) name = Found
    Locate a name = NotFound

class Locator schema l where
    type LocateResult schema l :: *

    locate :: schema -> l -> LocateResult schema l

instance Locator (TextSchema name) Found where
    type LocateResult (TextSchema name) Found = String

    locate (TextSchema string) _ = string

instance Locator a la => Locator (a :|: b) (L la) where
    type LocateResult (a :|: b) (L la) = LocateResult a la

    locate (a :|: b) locator = locate a (subLocator locator)
        where subLocator :: L la -> la
              subLocator _ = undefined

instance Locator b lb => Locator (a :|: b) (R lb) where
    type LocateResult (a :|: b) (R lb) = LocateResult b lb

    locate (a :|: b) locator = locate b (subLocator locator)
        where subLocator :: R lb -> lb
              subLocator _ = undefined

getLocator :: Locate schema name ~ locator => schema -> name -> locator
getLocator _ _ = undefined

getField :: ( Locate schema name ~ locator
            , Locator schema locator) => schema -> name -> LocateResult schema locator
getField schema name = locate schema (getLocator schema name)