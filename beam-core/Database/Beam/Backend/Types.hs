{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Database.Beam.Backend.Types where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Free.Church
import Control.Monad.Identity

import Data.Proxy
import Data.Monoid

import GHC.Generics
import GHC.Types

newtype Exposed x = Exposed x

class BeamBackend be where
  type BackendFromField be :: * -> Constraint

--  backendNull :: BackendLiteral be
--  backendIsNull :: BackendLiteral be -> Bool

class BeamBackend be => SupportedSyntax be syntax

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
instance BeamBackend be => FromBackendRow be () where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep ()))
  valuesNeeded _ _ = 0

instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b ) =>
  FromBackendRow be (a, b) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b)))
  valuesNeeded _ _ = 2
instance ( BeamBackend be, FromBackendRow be a, FromBackendRow be b, FromBackendRow be c ) =>
  FromBackendRow be (a, b, c) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c)))
  valuesNeeded _ _ = 3
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d ) =>
  FromBackendRow be (a, b, c, d) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d)))
  valuesNeeded _ _ = 4
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e ) =>
  FromBackendRow be (a, b, c, d, e) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e)))
  valuesNeeded _ _ = 5
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f ) =>
  FromBackendRow be (a, b, c, d, e, f) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f)))
  valuesNeeded _ _ = 6
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f
         , FromBackendRow be g ) =>
  FromBackendRow be (a, b, c, d, e, f, g) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g)))
  valuesNeeded _ _ = 7
deriving instance Generic (a, b, c, d, e, f, g, h)
instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f
         , FromBackendRow be g, FromBackendRow be h ) =>
  FromBackendRow be (a, b, c, d, e, f, g, h) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g, Exposed h)))
  valuesNeeded _ _ = 8

instance ( BeamBackend be, Generic (tbl Identity), Generic (tbl Exposed)
         , GFromBackendRow be (Rep (tbl Exposed)) (Rep (tbl Identity))) =>

    FromBackendRow be (tbl Identity) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (tbl Exposed)))
  valuesNeeded be _ = gValuesNeeded be (Proxy @(Rep (tbl Exposed))) (Proxy @(Rep (tbl Identity)))

instance FromBackendRow be x => FromBackendRow be (Maybe x) where
  fromBackendRow =
    do isNull <- checkNextNNull (valuesNeeded (Proxy @be) (Proxy @(Maybe x)))
       if isNull then pure Nothing else Just <$> fromBackendRow
  valuesNeeded be _ = valuesNeeded be (Proxy @x)

-- * MonadBeam class

class (BeamBackend be, Monad m) => MonadBeam syntax be m | m -> syntax be where

  runNoReturn :: syntax -> m ()
  runNoReturn cmd =
      runReturningMany cmd $ \(_ :: m (Maybe ())) -> pure ()

  runReturningOne :: FromBackendRow be x => syntax -> m (Maybe x)
  runReturningOne cmd =
      runReturningMany cmd $ \next ->
        do a <- next
           case a of
             Nothing -> pure Nothing
             Just x -> do
               a' <- next
               case a' of
                 Nothing -> pure (Just x)
                 Just _ -> pure Nothing

  runReturningMany :: FromBackendRow be x => syntax -> (m (Maybe x) -> m a) -> m a

runReturningList :: (FromBackendRow be x, MonadBeam syntax be m) => syntax -> m [x]
runReturningList cmd =
  runReturningMany cmd $ \next ->
    let collectM acc = do
          a <- next
          case a of
            Nothing -> pure (acc [])
            Just x -> collectM (acc . (x:))
    in collectM id
