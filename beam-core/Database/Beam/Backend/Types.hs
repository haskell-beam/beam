{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Database.Beam.Backend.Types
  ( FromBackendRowF(..), FromBackendRowM
  , parseOneField, peekField, checkNextNNull

  , FromBackendRow(..)

  , Exposed, Nullable ) where

import           Control.Monad.Free.Church
import           Control.Monad.Identity
import           Data.Tagged
import           Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as Vector

import Database.Beam.Syntax

import Data.Proxy

import GHC.Generics
import GHC.TypeLits

data FromBackendRowF f where
  ParseOneField :: BackendFromField a => (a -> f) -> FromBackendRowF f
  PeekField :: BackendFromField a => (Maybe a -> f) -> FromBackendRowF f
  CheckNextNNull :: Int -> (Bool -> f) -> FromBackendRowF f

deriving instance Functor FromBackendRowF 
type FromBackendRowM = F FromBackendRowF

parseOneField :: BackendFromField a => FromBackendRowM a
parseOneField = liftF (ParseOneField id)

peekField :: BackendFromField a => FromBackendRowM (Maybe a)
peekField = liftF (PeekField id)

checkNextNNull :: Int -> FromBackendRowM Bool
checkNextNNull n = liftF (CheckNextNNull n id)

class FromBackendRow a where
  fromBackendRow :: FromBackendRowM a
  default fromBackendRow :: BackendFromField a => FromBackendRowM a
  fromBackendRow = parseOneField

  valuesNeeded :: Proxy a -> Int
  valuesNeeded _ = 1

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

class GFromBackendRow (exposed :: * -> *) rep where
  gFromBackendRow :: Proxy exposed -> FromBackendRowM (rep ())
  gValuesNeeded :: Proxy exposed -> Proxy rep -> Int
instance GFromBackendRow e p => GFromBackendRow (M1 t f e) (M1 t f p) where
  gFromBackendRow _ = M1 <$> gFromBackendRow (Proxy @e)
  gValuesNeeded _ _ = gValuesNeeded (Proxy @e) (Proxy @p)
instance GFromBackendRow e U1 where
  gFromBackendRow _ = pure U1
  gValuesNeeded _ _ = 0
instance (GFromBackendRow aExp a, GFromBackendRow bExp b) => GFromBackendRow (aExp :*: bExp) (a :*: b) where
  gFromBackendRow _ = (:*:) <$> gFromBackendRow (Proxy @aExp) <*> gFromBackendRow (Proxy @bExp)
  gValuesNeeded _ _ = gValuesNeeded (Proxy @aExp) (Proxy @a) + gValuesNeeded (Proxy @bExp) (Proxy @b)
instance FromBackendRow x => GFromBackendRow (K1 R (Exposed x)) (K1 R x) where
  gFromBackendRow _ = K1 <$> fromBackendRow
  gValuesNeeded _ _ = valuesNeeded (Proxy @x)
instance FromBackendRow (t Identity) => GFromBackendRow (K1 R (t Exposed)) (K1 R (t Identity)) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded _ _ = valuesNeeded (Proxy @(t Identity))
instance FromBackendRow (t (Nullable Identity)) => GFromBackendRow (K1 R (t (Nullable Exposed))) (K1 R (t (Nullable Identity))) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded _ _ = valuesNeeded (Proxy @(t (Nullable Identity)))
instance FromBackendRow () where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep ()))
  valuesNeeded _ = 0

instance ( KnownNat n, FromBackendRow a ) => FromBackendRow (Vector n a) where
  fromBackendRow = Vector.replicateM fromBackendRow
  valuesNeeded _ = fromIntegral (natVal (Proxy @n))

instance ( FromBackendRow a, FromBackendRow b ) =>
  FromBackendRow (a, b) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b)))
  valuesNeeded _ = valuesNeeded (Proxy @a) + valuesNeeded (Proxy @b)
instance ( FromBackendRow a, FromBackendRow b, FromBackendRow c ) =>
  FromBackendRow (a, b, c) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c)))
  valuesNeeded _ = valuesNeeded (Proxy @a) + valuesNeeded (Proxy @b) + valuesNeeded (Proxy @c)
instance ( FromBackendRow a, FromBackendRow b, FromBackendRow c
         , FromBackendRow d ) =>
  FromBackendRow (a, b, c, d) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d)))
  valuesNeeded _ = valuesNeeded (Proxy @a) + valuesNeeded (Proxy @b) + valuesNeeded (Proxy @c) + valuesNeeded (Proxy @d)
instance ( FromBackendRow a, FromBackendRow b, FromBackendRow c
         , FromBackendRow d, FromBackendRow e ) =>
  FromBackendRow (a, b, c, d, e) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e)))
  valuesNeeded _ = valuesNeeded (Proxy @a) + valuesNeeded (Proxy @b) + valuesNeeded (Proxy @c) + valuesNeeded (Proxy @d) +
                      valuesNeeded (Proxy @e)
instance ( FromBackendRow a, FromBackendRow b, FromBackendRow c
         , FromBackendRow d, FromBackendRow e, FromBackendRow f ) =>
  FromBackendRow (a, b, c, d, e, f) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f)))
  valuesNeeded _ = valuesNeeded (Proxy @a) + valuesNeeded (Proxy @b) + valuesNeeded (Proxy @c) + valuesNeeded (Proxy @d) +
                      valuesNeeded (Proxy @e) + valuesNeeded (Proxy @f)
instance ( FromBackendRow a, FromBackendRow b, FromBackendRow c
         , FromBackendRow d, FromBackendRow e, FromBackendRow f
         , FromBackendRow g ) =>
  FromBackendRow (a, b, c, d, e, f, g) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g)))
  valuesNeeded _ = valuesNeeded (Proxy @a) + valuesNeeded (Proxy @b) + valuesNeeded (Proxy @c) + valuesNeeded (Proxy @d) +
                      valuesNeeded (Proxy @e) + valuesNeeded (Proxy @f) + valuesNeeded (Proxy @g)
instance ( FromBackendRow a, FromBackendRow b, FromBackendRow c
         , FromBackendRow d, FromBackendRow e, FromBackendRow f
         , FromBackendRow g, FromBackendRow h ) =>
  FromBackendRow (a, b, c, d, e, f, g, h) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g, Exposed h)))
  valuesNeeded _ = valuesNeeded (Proxy @a) + valuesNeeded (Proxy @b) + valuesNeeded (Proxy @c) + valuesNeeded (Proxy @d) +
                      valuesNeeded (Proxy @e) + valuesNeeded (Proxy @f) + valuesNeeded (Proxy @g) + valuesNeeded (Proxy @h)

instance ( Generic (tbl Identity), Generic (tbl Exposed), GFromBackendRow (Rep (tbl Exposed)) (Rep (tbl Identity))) =>

    FromBackendRow (tbl Identity) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (tbl Exposed)))
  valuesNeeded _ = gValuesNeeded (Proxy @(Rep (tbl Exposed))) (Proxy @(Rep (tbl Identity)))
instance ( Generic (tbl (Nullable Identity)), Generic (tbl (Nullable Exposed)), GFromBackendRow (Rep (tbl (Nullable Exposed))) (Rep (tbl (Nullable Identity)))) =>

    FromBackendRow (tbl (Nullable Identity)) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (tbl (Nullable Exposed))))
  valuesNeeded _ = gValuesNeeded (Proxy @(Rep (tbl (Nullable Exposed)))) (Proxy @(Rep (tbl (Nullable Identity))))

instance FromBackendRow x => FromBackendRow (Maybe x) where
  fromBackendRow =
    do isNull <- checkNextNNull (valuesNeeded (Proxy @(Maybe x)))
       if isNull then pure Nothing else Just <$> fromBackendRow
  valuesNeeded _ = valuesNeeded (Proxy @x)

deriving instance Generic (a, b, c, d, e, f, g, h)

-- Tagged

instance (FromBackendRow t) => FromBackendRow (Tagged tag t) where
  fromBackendRow = Tagged <$> fromBackendRow
