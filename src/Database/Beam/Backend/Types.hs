module Database.Beam.Backend.Types where

import Control.Monad.Except
import Control.Monad.State

import Data.Proxy
import Data.Monoid
import Data.Data

class ( Eq (BackendLiteral be), Show (BackendLiteral be)
      , Data (BackendLiteral be), Data be) =>
  BeamBackend be where

  data BackendLiteral be :: *

  backendNull :: BackendLiteral be

type FromBackendLiteralsM be a = ExceptT String (State [BackendLiteral be]) a

nextLiteral :: FromBackendLiteralsM be (BackendLiteral be)
nextLiteral =
  do st <- get
     case st of
       [] -> throwError "No more values left"
       x:xs -> put xs >> pure x

peekNextLiteral :: FromBackendLiteralsM be (BackendLiteral be)
peekNextLiteral =
  do st <- get
     l <- nextLiteral
     put st
     pure l

-- | Type class for things that can be converted to/from a stream of backend literals
class FromBackendLiterals be a where
  fromBackendLiterals :: FromBackendLiteralsM be a
  toBackendLiterals :: a -> [BackendLiteral be]
  valuesNeeded :: Proxy be -> Proxy a -> Int

  -- We supply a default implementation for types that can be converted to one backend literal
  default fromBackendLiterals :: FromBackendLiteral be a => FromBackendLiteralsM be a
  fromBackendLiterals =
    do l <- nextLiteral
       case fromBackendLiteral l of
         Nothing -> throwError "No parse"
         Just l' -> pure l'

  default toBackendLiterals :: FromBackendLiteral be a => a -> [BackendLiteral be]
  toBackendLiterals a = [ toBackendLiteral a ]

  default valuesNeeded :: FromBackendLiteral be a => Proxy be -> Proxy a -> Int
  valuesNeeded _ _ = 1

 -- | Type class for things that can be converted to/from one backend literal
class FromBackendLiteral be a where
  fromBackendLiteral :: BackendLiteral be -> Maybe a
  toBackendLiteral :: a -> BackendLiteral be

-- * Instances for common types

instance (FromBackendLiterals be t, BeamBackend be) => FromBackendLiterals be (Maybe t) where
    valuesNeeded be (_ :: Proxy (Maybe t)) = valuesNeeded be (Proxy :: Proxy t)
    fromBackendLiterals =
      mfix $ \(_ :: Maybe t) ->
      do values <- get
         let colCount = valuesNeeded (Proxy :: Proxy be) (Proxy :: Proxy t)
             colValues = take colCount values
         if all (==backendNull) colValues
           then put (drop colCount values) >> return Nothing
           else Just <$> fromBackendLiterals
    toBackendLiterals Nothing = replicate (valuesNeeded (Proxy :: Proxy be) (Proxy :: Proxy t)) backendNull
    toBackendLiterals (Just x) = toBackendLiterals x
instance (FromBackendLiterals be a, FromBackendLiterals be b) => FromBackendLiterals be (a, b) where
    fromBackendLiterals = (,) <$> fromBackendLiterals <*> fromBackendLiterals
    toBackendLiterals (a, b) = toBackendLiterals a <> toBackendLiterals b
    valuesNeeded be _ = valuesNeeded be (Proxy :: Proxy a) + valuesNeeded be (Proxy :: Proxy b)
instance (FromBackendLiterals be a, FromBackendLiterals be b, FromBackendLiterals be c) => FromBackendLiterals be (a, b, c) where
    fromBackendLiterals = (,,) <$> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals
    toBackendLiterals (a, b, c) = toBackendLiterals a <> toBackendLiterals b <> toBackendLiterals c
    valuesNeeded be _ = valuesNeeded be (Proxy :: Proxy a) + valuesNeeded be (Proxy :: Proxy b) + valuesNeeded be (Proxy :: Proxy c)
instance (FromBackendLiterals be a, FromBackendLiterals be b, FromBackendLiterals be c, FromBackendLiterals be d) => FromBackendLiterals be (a, b, c, d) where
    fromBackendLiterals = (,,,) <$> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals
    toBackendLiterals (a, b, c, d) = toBackendLiterals a <> toBackendLiterals b <> toBackendLiterals c <> toBackendLiterals d
    valuesNeeded be _ = valuesNeeded be (Proxy :: Proxy a) + valuesNeeded be (Proxy :: Proxy b) + valuesNeeded be (Proxy :: Proxy c) + valuesNeeded be (Proxy :: Proxy d)
instance (FromBackendLiterals be a, FromBackendLiterals be b, FromBackendLiterals be c, FromBackendLiterals be d, FromBackendLiterals be e) => FromBackendLiterals be (a, b, c, d, e) where
    fromBackendLiterals = (,,,,) <$> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals
    toBackendLiterals (a, b, c, d, e) = toBackendLiterals a <> toBackendLiterals b <> toBackendLiterals c <> toBackendLiterals d <> toBackendLiterals e
    valuesNeeded be _ = valuesNeeded be (Proxy :: Proxy a) + valuesNeeded be (Proxy :: Proxy b) + valuesNeeded be (Proxy :: Proxy c) + valuesNeeded be (Proxy :: Proxy d) + valuesNeeded be (Proxy :: Proxy e)

