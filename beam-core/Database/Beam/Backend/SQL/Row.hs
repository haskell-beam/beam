{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Database.Beam.Backend.SQL.Row
  ( FromBackendRowF(..), FromBackendRowM(..)
  , parseOneField, peekField

  , ColumnParseError(..), BeamRowReadError(..)

  , FromBackendRow(..)
  ) where

import           Database.Beam.Backend.SQL.Types
import           Database.Beam.Backend.Types

import           Control.Applicative
import           Control.Exception (Exception)
import           Control.Monad.Free.Church
import           Control.Monad.Identity
import           Data.Tagged
import           Data.Typeable
import           Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as Vector

#if !MIN_VERSION_base(4, 7, 0)
import           Data.Proxy
#endif

import           GHC.Generics
import           GHC.TypeLits

-- | The exact error encountered
data ColumnParseError
  = ColumnUnexpectedNull
  | ColumnNotEnoughColumns !Int
  | ColumnTypeMismatch
  { ctmHaskellType :: String
  , ctmSQLType     :: String
  , ctmMessage     :: String
  }
  | ColumnErrorInternal String
  deriving (Show, Eq, Ord)

-- | An error that may occur when parsing a row. Contains an optional
-- annotation of which column was being parsed (if available).
data BeamRowReadError
  = BeamRowReadError
  { brreColumn :: !(Maybe Int)
  , brreError  :: !ColumnParseError
  } deriving (Show, Eq, Ord)
instance Exception BeamRowReadError

data FromBackendRowF be f where
  ParseOneField :: (BackendFromField be a, Typeable a) => (a -> f) -> FromBackendRowF be f
  Alt :: FromBackendRowM be a -> FromBackendRowM be a -> (a -> f) -> FromBackendRowF be f
  FailParseWith :: BeamRowReadError -> FromBackendRowF be f
deriving instance Functor (FromBackendRowF be)
newtype FromBackendRowM be a = FromBackendRowM (F (FromBackendRowF be) a)
  deriving (Functor, Applicative)

instance Monad (FromBackendRowM be) where
  return = pure
  FromBackendRowM a >>= b =
    FromBackendRowM $
    a >>= (\x -> let FromBackendRowM b' = b x in b')

  fail = FromBackendRowM . liftF . FailParseWith .
         BeamRowReadError Nothing . ColumnErrorInternal

instance Alternative (FromBackendRowM be) where
  empty   = fail "empty"
  a <|> b =
    FromBackendRowM (liftF (Alt a b id))

parseOneField :: (BackendFromField be a, Typeable a) => FromBackendRowM be a
parseOneField = do
  x <- FromBackendRowM (liftF (ParseOneField id))
  pure x

peekField :: (Typeable a, BackendFromField be a) => FromBackendRowM be (Maybe a)
peekField = fmap Just (FromBackendRowM (liftF (ParseOneField id))) <|> pure Nothing

-- BeamBackend instead of BeamSqlBackend to prevent circular super class
class BeamBackend be => FromBackendRow be a where
  -- | Parses a beam row. This should not fail, except in the case of
  -- an internal bug in beam deserialization code. If it does fail,
  -- this should throw a 'BeamRowParseError'.
  fromBackendRow :: FromBackendRowM be a
  default fromBackendRow :: (Typeable a, BackendFromField be a) => FromBackendRowM be a
  fromBackendRow = parseOneField

  valuesNeeded :: Proxy be -> Proxy a -> Int
  valuesNeeded _ _ = 1

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

instance ( BeamBackend be, KnownNat n, FromBackendRow be a ) => FromBackendRow be (Vector n a) where
  fromBackendRow = Vector.replicateM fromBackendRow
  valuesNeeded _ _ = fromIntegral (natVal (Proxy @n))

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

instance (FromBackendRow be x, FromBackendRow be SqlNull) => FromBackendRow be (Maybe x) where
  fromBackendRow =
    (Just <$> fromBackendRow) <|>
    (Nothing <$
      replicateM_ (valuesNeeded (Proxy @be) (Proxy @(Maybe x)))
                  (do SqlNull <- fromBackendRow
                      pure ()))
  valuesNeeded be _ = valuesNeeded be (Proxy @x)

deriving instance Generic (a, b, c, d, e, f, g, h)

instance (BeamBackend be, FromBackendRow be t) => FromBackendRow be (Tagged tag t) where
  fromBackendRow = Tagged <$> fromBackendRow

instance FromBackendRow be x => FromBackendRow be (SqlSerial x) where
  fromBackendRow = SqlSerial <$> fromBackendRow
