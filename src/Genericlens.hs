{-# LANGUAGE DeriveGeneric, TypeFamilies, TypeOperators, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
module Data.Label.Generic where

import Data.Label
import Data.Typeable

import GHC.Generics

type GenericLensesFor m a = GenericLenses a m (Rep (a m))
newtype LensFor a c x = LensFor (c a :-> a x)

type family GenericLenses c m a where
    GenericLenses c m (D1 d a) = D1 d (GenericLenses c m a)
    GenericLenses c m (C1 d a) = C1 d (GenericLenses c m a)
    GenericLenses c m (S1 d a) = S1 d (GenericLenses c m a)
    GenericLenses c m (K1 R (m x)) = K1 R (LensFor m c x)
    GenericLenses c m (a :*: b) = GenericLenses c m a :*: GenericLenses c m b
    GenericLenses c m U1 = U1

class GLenses c m a where
    glenses :: (c m -> a p) -> ((a p -> a p) -> c m -> c m) -> GenericLenses c m a p
instance GLenses c m a => GLenses c m (D1 d a) where
    glenses (f :: c m -> D1 d a p) set = M1 $ glenses (unM1 . f) (\modifier -> set (M1 . modifier . unM1)) :: D1 d (GenericLenses c m a) p
instance GLenses c m a => GLenses c m (C1 d a) where
    glenses (f :: c m -> C1 d a p) set = M1 $ glenses (unM1 . f) (\modifier -> set (M1 . modifier . unM1)) :: C1 d (GenericLenses c m a) p
instance GLenses c m a => GLenses c m (S1 d a) where
    glenses (f :: c m -> S1 d a p) set = M1 $ glenses (unM1 . f) (\modifier -> set (M1 . modifier . unM1)) :: S1 d (GenericLenses c m a) p
instance (GLenses c m a, GLenses c m b) => GLenses c m (a :*: b) where
    glenses f set = glenses (left . f) modifyLeft :*: glenses (right . f) modifyRight
        where left (a :*: b) = a
              right (a :*: b) = b

              modifyLeft modifier = set (\(a :*: b) -> modifier a :*: b)
              modifyRight modifier = set (\(a :*: b) -> a :*: modifier b)
instance GLenses c m (K1 R (m a)) where
    glenses f set = K1 . LensFor $
                    lens (unK1 . f) (\modifier -> set (K1 . modifier . unK1))

lenses' :: ( GLenses a m (Rep (a m))
              , Generic (a m), Generic (a (LensFor m a))
              , GenericLensesFor m a ~ Rep (a (LensFor m a)) ) => Proxy a -> Proxy m -> a (LensFor m a)
lenses' (_ :: Proxy a) (_ :: Proxy m) = to (glenses (from :: a m -> Rep (a m) p) (\modifier x -> to $ modifier $ from x))

lenses ::  ( GLenses a m (Rep (a m))
              , Generic (a m), Generic (a (LensFor m a))
              , GenericLensesFor m a ~ Rep (a (LensFor m a)) ) => a (LensFor m a)
lenses = inj lenses'
    where inj :: (Proxy a -> Proxy m -> a (LensFor m a)) -> a (LensFor m a)
          inj f = f Proxy Proxy
