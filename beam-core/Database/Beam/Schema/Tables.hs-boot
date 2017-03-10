{-# LANGUAGE UndecidableInstances, RoleAnnotations #-}
module Database.Beam.Schema.Tables
    ( Table(..), Database(..)
    , TableField(..), DatabaseTable(..), Columnar'(..)
    , Nullable, Lenses, LensFor(..), Columnar(..)
    , ReifiedTableSchema(..), ReifiedDatabaseSchema(..)
    , TableSettings(..), DatabaseSettings(..)

    , allBeamValues) where

import Data.Text (Text)
import {-# SOURCE #-} Database.Beam.SQL.Types
import Control.Monad.Identity

import Lens.Micro

import GHC.Generics

class Table (table :: (* -> *) -> *)
class Beamable (table :: (* -> *) -> *)
class Database (database :: ((((* -> *) -> *) -> *) -> *))

data TableField be (table :: (* -> *) -> *) ty
type role TableField phantom phantom phantom
data DatabaseTable be (db :: ((((* -> *) -> *) -> *) -> *)) (table :: (* -> *) -> *)
type role DatabaseTable nominal phantom nominal
newtype Columnar' (f  :: * -> *) a = Columnar' (Columnar f a)
data Nullable (c :: * -> *) x
type role Nullable phantom phantom
data Lenses (t :: (* -> *) -> *) (f :: * -> *) x
type role Lenses phantom phantom phantom
data LensFor t x where
    LensFor :: Generic t => Lens' t x -> LensFor t x
newtype Exposed x = Exposed x

type family Columnar (f :: * -> *) x where
    Columnar Exposed x = Exposed x

    Columnar Identity x = x

    Columnar (Lenses t Identity) x = LensFor (t Identity) (Columnar Identity x)
    Columnar (Lenses t f) x = LensFor (t f) (f x)

    Columnar (Nullable c) x = Columnar c (Maybe x)

    Columnar f x = f x
--type role SQLColumnSchema phantom
type ReifiedTableSchema be = [(Text, SQLColumnSchema be)]
type ReifiedDatabaseSchema be = [(Text, ReifiedTableSchema be)]
--type role ReifiedDatabaseSchema phantom

type TableSettings be table = table (TableField be table)
type DatabaseSettings be db = db (DatabaseTable be db)

class GZipTables (f :: * -> *) (g :: * -> *) (h :: * -> *)
                 (exposedRep :: * -> *) (fRep :: * -> *) (gRep :: * -> *) (hRep :: * -> *)
type HasBeamFields table f g h = ( GZipTables f g h (Rep (table Exposed)) (Rep (table f)) (Rep (table g)) (Rep (table h))
                                 , Generic (table f), Generic (table g), Generic (table h) )
allBeamValues :: Beamable table => (forall a. Columnar' f a -> b) -> table f -> [b]
