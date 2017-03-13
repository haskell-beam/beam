{-# LANGUAGE UndecidableInstances #-}

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

class BeamColumnSchema schema where
  maybeFieldSchema :: schema -> schema
  nestedSchema     :: schema -> schema
  autoSchema       :: schema -> schema
  setPrimaryKey    :: schema -> schema

class BeamColumnSchema (BackendColumnSchema be) => BeamBackend be where
  data BackendColumnSchema be :: *
  type BackendFromField be :: * -> Constraint

--  backendNull :: BackendLiteral be
--  backendIsNull :: BackendLiteral be -> Bool

class BeamBackend be => SupportedSyntax be syntax

newtype Auto x = Auto { unAuto :: Maybe x }
  deriving (Show, Read, Eq, Ord)

data FromBackendRowF be f where
  ParseOneField :: BackendFromField be a => (a -> f) -> FromBackendRowF be f
  PeekField :: BackendFromField be a => (Maybe a -> f) -> FromBackendRowF be f
deriving instance Functor (FromBackendRowF be)
type FromBackendRowM be = F (FromBackendRowF be)

parseOneField :: BackendFromField be a => FromBackendRowM be a
parseOneField = liftF (ParseOneField id)

peekField :: BackendFromField be a => FromBackendRowM be (Maybe a)
peekField = liftF (PeekField id)

class BeamBackend be => FromBackendRow be a where
  fromBackendRow :: FromBackendRowM be a

class GFromBackendRow be (exposed :: * -> *) rep where
  gFromBackendRow :: Proxy exposed -> FromBackendRowM be (rep ())
instance GFromBackendRow be e p => GFromBackendRow be (M1 t f e) (M1 t f p) where
  gFromBackendRow _ = M1 <$> gFromBackendRow (Proxy @e)
instance GFromBackendRow be e U1 where
  gFromBackendRow _ = pure U1
instance (GFromBackendRow be aExp a, GFromBackendRow be bExp b) => GFromBackendRow be (aExp :*: bExp) (a :*: b) where
  gFromBackendRow _ = (:*:) <$> gFromBackendRow (Proxy @aExp) <*> gFromBackendRow (Proxy @bExp)

instance BackendFromField be x => GFromBackendRow be (K1 R (Exposed x)) (K1 R x) where
  gFromBackendRow _ = K1 <$> parseOneField

instance FromBackendRow be (t Identity) => GFromBackendRow be (K1 R (t Exposed)) (K1 R (t Identity)) where
    gFromBackendRow _ = K1 <$> fromBackendRow

instance BeamBackend be => FromBackendRow be () where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep ()))

instance ( BeamBackend be, BackendFromField be a, BackendFromField be b ) =>
  FromBackendRow be (a, b) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b)))
instance ( BeamBackend be, BackendFromField be a, BackendFromField be b, BackendFromField be c ) =>
  FromBackendRow be (a, b, c) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c)))
instance ( BeamBackend be
         , BackendFromField be a, BackendFromField be b, BackendFromField be c
         , BackendFromField be d ) =>
  FromBackendRow be (a, b, c, d) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d)))
instance ( BeamBackend be
         , BackendFromField be a, BackendFromField be b, BackendFromField be c
         , BackendFromField be d, BackendFromField be e ) =>
  FromBackendRow be (a, b, c, d, e) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e)))
instance ( BeamBackend be
         , BackendFromField be a, BackendFromField be b, BackendFromField be c
         , BackendFromField be d, BackendFromField be e, BackendFromField be f ) =>
  FromBackendRow be (a, b, c, d, e, f) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f)))

instance ( BeamBackend be, Generic (tbl Identity), Generic (tbl Exposed)
         , GFromBackendRow be (Rep (tbl Exposed)) (Rep (tbl Identity))) =>

    FromBackendRow be (tbl Identity) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (tbl Exposed)))
-- type FromBackendLiteralsM be a = ExceptT String (State [BackendLiteral be]) a

-- nextLiteral :: FromBackendLiteralsM be (BackendLiteral be)
-- nextLiteral =
--   do st <- get
--      case st of
--        [] -> throwError "No more values left"
--        x:xs -> put xs >> pure x

-- peekNextLiteral :: FromBackendLiteralsM be (BackendLiteral be)
-- peekNextLiteral =
--   do st <- get
--      l <- nextLiteral
--      put st
--      pure l

-- | Type class for things that can be converted to/from a stream of backend literals
-- class FromBackendLiterals be a where
--   fromBackendLiterals :: FromBackendLiteralsM be a
--   toBackendLiterals :: a -> [BackendLiteral be]
--   valuesNeeded :: Proxy be -> Proxy a -> Int

--   -- We supply a default implementation for types that can be converted to one backend literal
--   default fromBackendLiterals :: FromBackendLiteral be a => FromBackendLiteralsM be a
--   fromBackendLiterals =
--     do l <- nextLiteral
--        case fromBackendLiteral l of
--          Nothing -> throwError "No parse"
--          Just l' -> pure l'

--   default toBackendLiterals :: FromBackendLiteral be a => a -> [BackendLiteral be]
--   toBackendLiterals a = [ toBackendLiteral a ]

--   default valuesNeeded :: FromBackendLiteral be a => Proxy be -> Proxy a -> Int
--   valuesNeeded _ _ = 1

--  -- | Type class for things that can be converted to/from one backend literal
-- class FromBackendLiteral be a where
--   fromBackendLiteral :: BackendLiteral be -> Maybe a
--   toBackendLiteral :: a -> BackendLiteral be

-- * Instances for common types

-- instance (FromBackendLiterals be t, BeamBackend be) => FromBackendLiterals be (Maybe t) where
--     valuesNeeded be (_ :: Proxy (Maybe t)) = valuesNeeded be (Proxy :: Proxy t)
--     fromBackendLiterals =
--       mfix $ \(_ :: Maybe t) ->
--       do values <- get
--          let colCount = valuesNeeded (Proxy :: Proxy be) (Proxy :: Proxy t)
--              colValues = take colCount values
--          if all backendIsNull colValues
--            then put (drop colCount values) >> return Nothing
--            else Just <$> fromBackendLiterals
--     toBackendLiterals Nothing = replicate (valuesNeeded (Proxy :: Proxy be) (Proxy :: Proxy t)) backendNull
--     toBackendLiterals (Just x) = toBackendLiterals x
-- instance (FromBackendLiterals be a, FromBackendLiterals be b) => FromBackendLiterals be (a, b) where
--     fromBackendLiterals = (,) <$> fromBackendLiterals <*> fromBackendLiterals
--     toBackendLiterals (a, b) = toBackendLiterals a <> toBackendLiterals b
--     valuesNeeded be _ = valuesNeeded be (Proxy :: Proxy a) + valuesNeeded be (Proxy :: Proxy b)
-- instance (FromBackendLiterals be a, FromBackendLiterals be b, FromBackendLiterals be c) => FromBackendLiterals be (a, b, c) where
--     fromBackendLiterals = (,,) <$> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals
--     toBackendLiterals (a, b, c) = toBackendLiterals a <> toBackendLiterals b <> toBackendLiterals c
--     valuesNeeded be _ = valuesNeeded be (Proxy :: Proxy a) + valuesNeeded be (Proxy :: Proxy b) + valuesNeeded be (Proxy :: Proxy c)
-- instance (FromBackendLiterals be a, FromBackendLiterals be b, FromBackendLiterals be c, FromBackendLiterals be d) => FromBackendLiterals be (a, b, c, d) where
--     fromBackendLiterals = (,,,) <$> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals
--     toBackendLiterals (a, b, c, d) = toBackendLiterals a <> toBackendLiterals b <> toBackendLiterals c <> toBackendLiterals d
--     valuesNeeded be _ = valuesNeeded be (Proxy :: Proxy a) + valuesNeeded be (Proxy :: Proxy b) + valuesNeeded be (Proxy :: Proxy c) + valuesNeeded be (Proxy :: Proxy d)
-- instance (FromBackendLiterals be a, FromBackendLiterals be b, FromBackendLiterals be c, FromBackendLiterals be d, FromBackendLiterals be e) => FromBackendLiterals be (a, b, c, d, e) where
--     fromBackendLiterals = (,,,,) <$> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals <*> fromBackendLiterals
--     toBackendLiterals (a, b, c, d, e) = toBackendLiterals a <> toBackendLiterals b <> toBackendLiterals c <> toBackendLiterals d <> toBackendLiterals e
--     valuesNeeded be _ = valuesNeeded be (Proxy :: Proxy a) + valuesNeeded be (Proxy :: Proxy b) + valuesNeeded be (Proxy :: Proxy c) + valuesNeeded be (Proxy :: Proxy d) + valuesNeeded be (Proxy :: Proxy e)
