{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Database.Beam.Backend.Types
  ( BeamBackend(..)

  , FromBackendRowF(..), FromBackendRowM
  , parseOneField, peekField, checkNextNNull

  , FromBackendRow(..)

  , AnyField(..)

  , Exposed, Nullable ) where

import           Control.Monad.Free.Church
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

data AnyField be = forall a. (BackendFromField be a) => AF (Proxy a)

class BeamBackend be => FromBackendRow be a where
  fromBackendRow :: FromBackendRowM be a
  default fromBackendRow :: BackendFromField be a => FromBackendRowM be a
  fromBackendRow = parseOneField

  valuesNeeded :: Proxy be -> Proxy a -> Int
  valuesNeeded _ _ = 1

  rowRep :: Proxy be -> Proxy a -> [AnyField be]
  default rowRep :: BackendFromField be a => Proxy be -> Proxy a -> [AnyField be]
  rowRep _ _ = [AF (Proxy @a)]

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
  gRowRep :: Proxy be -> Proxy exposed -> Proxy rep -> [AnyField be]
instance GFromBackendRow be e p => GFromBackendRow be (M1 t f e) (M1 t f p) where
  gFromBackendRow _ = M1 <$> gFromBackendRow (Proxy @e)
  gValuesNeeded be _ _ = gValuesNeeded be (Proxy @e) (Proxy @p)
  gRowRep be _ _ = gRowRep be (Proxy @e) (Proxy @p)
instance GFromBackendRow be e U1 where
  gFromBackendRow _ = pure U1
  gValuesNeeded _ _ _ = 0
  gRowRep _ _ _ = []
instance (GFromBackendRow be aExp a, GFromBackendRow be bExp b) => GFromBackendRow be (aExp :*: bExp) (a :*: b) where
  gFromBackendRow _ = (:*:) <$> gFromBackendRow (Proxy @aExp) <*> gFromBackendRow (Proxy @bExp)
  gValuesNeeded be _ _ = gValuesNeeded be (Proxy @aExp) (Proxy @a) + gValuesNeeded be (Proxy @bExp) (Proxy @b)
  gRowRep be _ _ = gRowRep be (Proxy @aExp) (Proxy @a) ++ gRowRep be (Proxy @bExp) (Proxy @b)
instance FromBackendRow be x => GFromBackendRow be (K1 R (Exposed x)) (K1 R x) where
  gFromBackendRow _ = K1 <$> fromBackendRow
  gValuesNeeded be _ _ = valuesNeeded be (Proxy @x)
  gRowRep be _ _ = rowRep be (Proxy @x)
instance FromBackendRow be (t Identity) => GFromBackendRow be (K1 R (t Exposed)) (K1 R (t Identity)) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded be _ _ = valuesNeeded be (Proxy @(t Identity))
    gRowRep be _ _ = rowRep be (Proxy @(t Identity))
instance FromBackendRow be (t (Nullable Identity)) => GFromBackendRow be (K1 R (t (Nullable Exposed))) (K1 R (t (Nullable Identity))) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded be _ _ = valuesNeeded be (Proxy @(t (Nullable Identity)))
    gRowRep be _ _ = rowRep be (Proxy @(t (Nullable Identity)))
instance BeamBackend be => FromBackendRow be () where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep ()))
  valuesNeeded _ _ = 0
  rowRep _ _ = []

instance ( BeamBackend be, KnownNat n, FromBackendRow be a ) => FromBackendRow be (Vector n a) where
  fromBackendRow = Vector.replicateM fromBackendRow
  valuesNeeded _ _ = fromIntegral (natVal (Proxy @n))
  rowRep be a = concat $ replicate (fromIntegral $ natVal (Proxy @n)) (rowRep be a)

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b ) =>
  FromBackendRow be (a, b) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b)
  rowRep be _ = rowRep be (Proxy @a) ++ rowRep be (Proxy @b)
instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c ) =>
  FromBackendRow be (a, b, c) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c)
  rowRep be _ = concat [rowRep be (Proxy @a), rowRep be (Proxy @b), rowRep be (Proxy @c)]
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d ) =>
  FromBackendRow be (a, b, c, d) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d)
  rowRep be _ = concat [rowRep be (Proxy @a), rowRep be (Proxy @b), rowRep be (Proxy @c), rowRep be (Proxy @d)]
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e ) =>
  FromBackendRow be (a, b, c, d, e) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e)
  rowRep be _ = concat [ rowRep be (Proxy @a), rowRep be (Proxy @b), rowRep be (Proxy @c), rowRep be (Proxy @d)
                       , rowRep be (Proxy @e)
                       ]
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f ) =>
  FromBackendRow be (a, b, c, d, e, f) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e) + valuesNeeded be (Proxy @f)
  rowRep be _ = concat [ rowRep be (Proxy @a), rowRep be (Proxy @b), rowRep be (Proxy @c), rowRep be (Proxy @d), rowRep be (Proxy @d)
                       , rowRep be (Proxy @e), rowRep be (Proxy @f) ]
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f
         , FromBackendRow be g ) =>
  FromBackendRow be (a, b, c, d, e, f, g) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e) + valuesNeeded be (Proxy @f) + valuesNeeded be (Proxy @g)
  rowRep be _ = concat [ rowRep be (Proxy @a), rowRep be (Proxy @b), rowRep be (Proxy @c), rowRep be (Proxy @d), rowRep be (Proxy @d)
                       , rowRep be (Proxy @e), rowRep be (Proxy @f), rowRep be (Proxy @g) ]
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f
         , FromBackendRow be g, FromBackendRow be h ) =>
  FromBackendRow be (a, b, c, d, e, f, g, h) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g, Exposed h)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e) + valuesNeeded be (Proxy @f) + valuesNeeded be (Proxy @g) + valuesNeeded be (Proxy @h)
  rowRep be _ = concat [ rowRep be (Proxy @a), rowRep be (Proxy @b), rowRep be (Proxy @c), rowRep be (Proxy @d), rowRep be (Proxy @d)
                       , rowRep be (Proxy @e), rowRep be (Proxy @f), rowRep be (Proxy @g), rowRep be (Proxy @h) ]

instance ( BeamBackend be, Generic (tbl Identity), Generic (tbl Exposed)
         , GFromBackendRow be (Rep (tbl Exposed)) (Rep (tbl Identity))) =>

    FromBackendRow be (tbl Identity) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (tbl Exposed)))
  valuesNeeded be _ = gValuesNeeded be (Proxy @(Rep (tbl Exposed))) (Proxy @(Rep (tbl Identity)))
  rowRep be _ = gRowRep be (Proxy @(Rep (tbl Exposed))) (Proxy @(Rep (tbl Identity)))
instance ( BeamBackend be, Generic (tbl (Nullable Identity)), Generic (tbl (Nullable Exposed))
         , GFromBackendRow be (Rep (tbl (Nullable Exposed))) (Rep (tbl (Nullable Identity)))) =>

    FromBackendRow be (tbl (Nullable Identity)) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (tbl (Nullable Exposed))))
  valuesNeeded be _ = gValuesNeeded be (Proxy @(Rep (tbl (Nullable Exposed)))) (Proxy @(Rep (tbl (Nullable Identity))))
  rowRep be _ = gRowRep be (Proxy @(Rep (tbl (Nullable Exposed)))) (Proxy @(Rep (tbl (Nullable Identity))))

instance FromBackendRow be x => FromBackendRow be (Maybe x) where
  fromBackendRow =
    do isNull <- checkNextNNull (valuesNeeded (Proxy @be) (Proxy @(Maybe x)))
       if isNull then pure Nothing else Just <$> fromBackendRow
  valuesNeeded be _ = valuesNeeded be (Proxy @x)
  rowRep be _ = rowRep be (Proxy @x)

deriving instance Generic (a, b, c, d, e, f, g, h)

-- Tagged

instance (BeamBackend be, FromBackendRow be t) => FromBackendRow be (Tagged tag t) where
  fromBackendRow = Tagged <$> fromBackendRow
  rowRep be _ = rowRep be (Proxy @t)
