{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Database.Beam.Backend.Types
  ( BeamBackend(..)
  , Auto(..)

  , FromBackendRowF(..), FromBackendRowM
  , parseOneField, peekField, checkNextNNull

  , FromBackendRow(..)

  , Exposed, Nullable ) where

import Control.Monad.Free.Church
import Control.Monad.Identity

import Data.Proxy

import GHC.Generics
import GHC.Types

-- | Class for all beam backends
class BeamBackend be where
  -- | Requirements to marshal a certain type from a database of a particular backend
  type BackendFromField be :: * -> Constraint

-- | Newtype wrapper for types that may be given default values by the database.
--   Essentially, a wrapper around 'Maybe x'.
--
--   When a value of type 'Auto x' is written to a database (via @INSERT@ or
--   @UPDATE@, for example), backends will translate a 'Nothing' value into an
--   expression that will evaluate to whatever default value the database would
--   choose. This is useful to insert rows with columns that are
--   auto-incrementable or have a @DEFAULT@ value.
--
--   When read from the database, the wrapped value will always be a 'Just'
--   value. This isn't currently enforced at the type-level, but may be in
--   future versions of beam.
newtype Auto x = Auto { unAuto :: Maybe x }
  deriving (Show, Read, Eq, Ord)

data FromBackendRowF be f where
  ParseOneField :: BackendFromField be a => (a -> f) -> FromBackendRowF be f
  PeekField :: BackendFromField be a => (Maybe a -> f) -> FromBackendRowF be f
  CheckNextNNull :: Int -> (Bool -> f) -> FromBackendRowF be f
deriving instance Functor (FromBackendRowF be)
type FromBackendRowM be = F (FromBackendRowF be)

parseOneField :: BackendFromField be a => FromBackendRowM be a
parseOneField = liftF (ParseOneField id)

peekField :: BackendFromField be a => FromBackendRowM be (Maybe a)
peekField = liftF (PeekField id)

checkNextNNull :: Int -> FromBackendRowM be Bool
checkNextNNull n = liftF (CheckNextNNull n id)

class BeamBackend be => FromBackendRow be a where
  fromBackendRow :: FromBackendRowM be a
  default fromBackendRow :: BackendFromField be a => FromBackendRowM be a
  fromBackendRow = parseOneField

  valuesNeeded :: Proxy be -> Proxy a -> Int
  valuesNeeded _ _ = 1

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
  gFromBackendRow :: Proxy exposed -> FromBackendRowM be (rep ())
  gValuesNeeded :: Proxy be -> Proxy exposed -> Proxy rep -> Int
instance GFromBackendRow be e p => GFromBackendRow be (M1 t f e) (M1 t f p) where
  gFromBackendRow _ = M1 <$> gFromBackendRow (Proxy @e)
  gValuesNeeded be _ _ = gValuesNeeded be (Proxy @e) (Proxy @p)
instance GFromBackendRow be e U1 where
  gFromBackendRow _ = pure U1
  gValuesNeeded _ _ _ = 0
instance (GFromBackendRow be aExp a, GFromBackendRow be bExp b) => GFromBackendRow be (aExp :*: bExp) (a :*: b) where
  gFromBackendRow _ = (:*:) <$> gFromBackendRow (Proxy @aExp) <*> gFromBackendRow (Proxy @bExp)
  gValuesNeeded be _ _ = gValuesNeeded be (Proxy @aExp) (Proxy @a) + gValuesNeeded be (Proxy @bExp) (Proxy @b)
instance FromBackendRow be x => GFromBackendRow be (K1 R (Exposed x)) (K1 R x) where
  gFromBackendRow _ = K1 <$> fromBackendRow
  gValuesNeeded be _ _ = valuesNeeded be (Proxy @x)
instance FromBackendRow be (t Identity) => GFromBackendRow be (K1 R (t Exposed)) (K1 R (t Identity)) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded be _ _ = valuesNeeded be (Proxy @(t Identity))
instance FromBackendRow be (t (Nullable Identity)) => GFromBackendRow be (K1 R (t (Nullable Exposed))) (K1 R (t (Nullable Identity))) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded be _ _ = valuesNeeded be (Proxy @(t (Nullable Identity)))
instance BeamBackend be => FromBackendRow be () where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep ()))
  valuesNeeded _ _ = 0

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b ) =>
  FromBackendRow be (a, b) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b)
instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c ) =>
  FromBackendRow be (a, b, c) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c)
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d ) =>
  FromBackendRow be (a, b, c, d) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d)
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e ) =>
  FromBackendRow be (a, b, c, d, e) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e)
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f ) =>
  FromBackendRow be (a, b, c, d, e, f) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e) + valuesNeeded be (Proxy @f)
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f
         , FromBackendRow be g ) =>
  FromBackendRow be (a, b, c, d, e, f, g) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e) + valuesNeeded be (Proxy @f) + valuesNeeded be (Proxy @g)
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f
         , FromBackendRow be g, FromBackendRow be h ) =>
  FromBackendRow be (a, b, c, d, e, f, g, h) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g, Exposed h)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e) + valuesNeeded be (Proxy @f) + valuesNeeded be (Proxy @g) + valuesNeeded be (Proxy @h)

instance ( BeamBackend be, Generic (tbl Identity), Generic (tbl Exposed)
         , GFromBackendRow be (Rep (tbl Exposed)) (Rep (tbl Identity))) =>

    FromBackendRow be (tbl Identity) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (tbl Exposed)))
  valuesNeeded be _ = gValuesNeeded be (Proxy @(Rep (tbl Exposed))) (Proxy @(Rep (tbl Identity)))
instance ( BeamBackend be, Generic (tbl (Nullable Identity)), Generic (tbl (Nullable Exposed))
         , GFromBackendRow be (Rep (tbl (Nullable Exposed))) (Rep (tbl (Nullable Identity)))) =>

    FromBackendRow be (tbl (Nullable Identity)) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (tbl (Nullable Exposed))))
  valuesNeeded be _ = gValuesNeeded be (Proxy @(Rep (tbl (Nullable Exposed)))) (Proxy @(Rep (tbl (Nullable Identity))))

instance FromBackendRow be x => FromBackendRow be (Maybe x) where
  fromBackendRow =
    do isNull <- checkNextNNull (valuesNeeded (Proxy @be) (Proxy @(Maybe x)))
       if isNull then pure Nothing else Just <$> fromBackendRow
  valuesNeeded be _ = valuesNeeded be (Proxy @x)
instance (BeamBackend be, FromBackendRow be (Maybe x)) => FromBackendRow be (Auto x) where
  fromBackendRow = Auto <$> fromBackendRow
  valuesNeeded be _ = valuesNeeded be (Proxy @(Maybe x))

deriving instance Generic (a, b, c, d, e, f, g, h)
