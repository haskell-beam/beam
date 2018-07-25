{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Database.Beam.Backend.Types
  ( BeamBackend(..)

  , FromBackendRowF(..), FromBackendRowA
  , parseOneField, peekField

  , FromBackendRow(..)
  , valuesNeeded

  , Exposed, Nullable ) where

import           Control.Applicative
import           Control.Applicative.Free
import           Control.Monad.Identity
import           Data.Tagged
import           Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as Vector

import Data.Proxy

import GHC.Generics
import GHC.TypeLits
import GHC.Types

-- | Class for all beam backends
class BeamBackend be where
  -- | Requirements to marshal a certain type from a database of a particular backend
  type BackendFromField be :: * -> Constraint

data FromBackendRowF be f where
  ParseOneField :: BackendFromField be a => (Maybe a -> f) -> FromBackendRowF be f
  PeekField :: BackendFromField be a => (Maybe a -> f) -> FromBackendRowF be f
deriving instance Functor (FromBackendRowF be)
type FromBackendRowA be = Ap (FromBackendRowF be)

parseOneField :: BackendFromField be a => FromBackendRowA be (Maybe a)
parseOneField = liftAp (ParseOneField id)

peekField :: BackendFromField be a => FromBackendRowA be (Maybe a)
peekField = liftAp (PeekField id)

class BeamBackend be => FromBackendRow be a where
  fromBackendRow :: FromBackendRowA be (Maybe a)
  default fromBackendRow :: (BackendFromField be a) => FromBackendRowA be (Maybe a)
  fromBackendRow = parseOneField

valuesNeeded :: FromBackendRowA be a -> Int
valuesNeeded (Pure _) = 0
valuesNeeded (Ap _ a) = 1 + valuesNeeded a

-- | newtype mainly used to inspect tho tag structure of a particular
--   'Beamable'. Prevents overlapping instances in some case. Usually not used
--   in end-user code.
data Exposed x

-- | Support for NULLable Foreign Key references.
--
-- > data MyTable f = MyTable
-- >                { nullableRef :: PrimaryKey AnotherTable (Nullable f)
-- >                , ... }
-- >                 deriving (Generic, Typeable)
--
-- See 'Columnar' for more information.
data Nullable (c :: * -> *) x

class GFromBackendRow be (exposed :: * -> *) rep where
  gFromBackendRow :: Proxy exposed -> FromBackendRowA be (Maybe (rep ()))
instance GFromBackendRow be e U1 where
  gFromBackendRow _ = pure (Just U1)
instance GFromBackendRow be e p => GFromBackendRow be (M1 t f e) (M1 t f p) where
  gFromBackendRow _ = fmap M1 <$> gFromBackendRow (Proxy @e)
instance (GFromBackendRow be ea a, GFromBackendRow be eb b) => GFromBackendRow be (ea :*: eb) (a :*: b) where
  gFromBackendRow _ = liftA2 (:*:) <$> gFromBackendRow (Proxy @ea) <*> gFromBackendRow (Proxy @eb)
instance FromBackendRow be x => GFromBackendRow be (K1 R (Exposed x)) (K1 R x) where
  gFromBackendRow _ = fmap K1 <$> fromBackendRow
instance FromBackendRow be (t Identity) => GFromBackendRow be (K1 R (t Exposed)) (K1 R (t Identity)) where
  gFromBackendRow _ = fmap K1 <$> fromBackendRow
instance FromBackendRow be (t (Nullable Identity)) => GFromBackendRow be (K1 R (t (Nullable Exposed))) (K1 R (t (Nullable Identity))) where
  gFromBackendRow _ = fmap K1 <$> fromBackendRow


instance BeamBackend be => FromBackendRow be () where
  fromBackendRow = pure (Just ())

instance ( BeamBackend be, KnownNat n, FromBackendRow be a ) => FromBackendRow be (Vector n a) where
  fromBackendRow = fmap sequenceA $ sequenceA $ Vector.replicate fromBackendRow

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b ) =>
  FromBackendRow be (a, b) where
  fromBackendRow = liftA2 (,) <$> fromBackendRow <*> fromBackendRow

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c ) =>
  FromBackendRow be (a, b, c) where
  fromBackendRow = liftA3 (,,) <$> fromBackendRow <*> fromBackendRow <*> fromBackendRow

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c, FromBackendRow be d ) =>
  FromBackendRow be (a, b, c, d) where
  fromBackendRow = liftA4 (,,,) <$> fromBackendRow <*> fromBackendRow <*> fromBackendRow <*> fromBackendRow

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c, FromBackendRow be d
                         , FromBackendRow be e ) =>
  FromBackendRow be (a, b, c, d, e) where
  fromBackendRow = liftA5 (,,,,) <$> fromBackendRow <*> fromBackendRow <*> fromBackendRow <*> fromBackendRow
                                 <*> fromBackendRow

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c, FromBackendRow be d
                         , FromBackendRow be e, FromBackendRow be f ) =>
  FromBackendRow be (a, b, c, d, e, f) where
  fromBackendRow = liftA6 (,,,,,) <$> fromBackendRow <*> fromBackendRow <*> fromBackendRow <*> fromBackendRow
                                  <*> fromBackendRow <*> fromBackendRow

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c, FromBackendRow be d
                         , FromBackendRow be e, FromBackendRow be f, FromBackendRow be g ) =>
  FromBackendRow be (a, b, c, d, e, f, g) where
  fromBackendRow = liftA7 (,,,,,,) <$> fromBackendRow <*> fromBackendRow <*> fromBackendRow <*> fromBackendRow
                                   <*> fromBackendRow <*> fromBackendRow <*> fromBackendRow

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c, FromBackendRow be d
                         , FromBackendRow be e, FromBackendRow be f, FromBackendRow be g, FromBackendRow be h ) =>
  FromBackendRow be (a, b, c, d, e, f, g, h) where
  fromBackendRow = liftA8 (,,,,,,,) <$> fromBackendRow <*> fromBackendRow <*> fromBackendRow <*> fromBackendRow
                                    <*> fromBackendRow <*> fromBackendRow <*> fromBackendRow <*> fromBackendRow

instance FromBackendRow be a => FromBackendRow be (Maybe a) where
  fromBackendRow = fmap f (fromBackendRow :: FromBackendRowA be (Maybe a))
    where
      f :: Maybe a -> Maybe (Maybe a)
      f (Just a) = Just (Just a)
      f Nothing = Just Nothing

instance ( BeamBackend be, Generic (tbl Identity), Generic (tbl Exposed)
         , GFromBackendRow be (Rep (tbl Exposed)) (Rep (tbl Identity))) =>
    FromBackendRow be (tbl Identity) where
  fromBackendRow = fmap to <$> gFromBackendRow (Proxy @(Rep (tbl Exposed)))

instance ( BeamBackend be, Generic (tbl (Nullable Identity)), Generic (tbl (Nullable Exposed))
         , GFromBackendRow be (Rep (tbl (Nullable Exposed))) (Rep (tbl (Nullable Identity)))) =>
    FromBackendRow be (tbl (Nullable Identity)) where
  fromBackendRow = fmap to <$> gFromBackendRow (Proxy @(Rep (tbl (Nullable Exposed))))

-- Tagged

instance (BeamBackend be, FromBackendRow be t) => FromBackendRow be (Tagged tag t) where
  fromBackendRow = fmap Tagged <$> fromBackendRow

liftA4 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> b) -> f a1 -> f a2 -> f a3 -> f a4 -> f b
liftA4 f a1 a2 a3 a4 = f <$> a1 <*> a2 <*> a3 <*> a4

liftA5 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f b
liftA5 f a1 a2 a3 a4 a5 = f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5

liftA6 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b) -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6 -> f b
liftA6 f a1 a2 a3 a4 a5 a6 = f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6

liftA7 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b) -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6 -> f a7 -> f b
liftA7 f a1 a2 a3 a4 a5 a6 a7 = f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7

liftA8 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> b) -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6 -> f a7 -> f a8 -> f b
liftA8 f a1 a2 a3 a4 a5 a6 a7 a8 = f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8

