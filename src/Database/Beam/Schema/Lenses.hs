{-# LANGUAGE ScopedTypeVariables, TypeOperators, TypeFamilies, RankNTypes, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Database.Beam.Schema.Lenses where

import Database.Beam.Types
import Database.Beam.Schema.Tables
import Database.Beam.Schema.Fields

import Control.Applicative
import Control.Monad.Identity

import Data.Functor
import Data.Proxy

import GHC.Generics

import Lens.Micro hiding (to)

type family GenericTableLens t a where
    GenericTableLens t (M1 s d a) = M1 s d (GenericTableLens t a)
--    GenericTableLens t f (K1 R (ForeignKey rel f)) = K1 R (LensFor t (PrimaryKey rel f))
--    GenericTableLens t f (K1 R (f x)) = K1 R (LensFor t f x)
    GenericTableLens t (K1 R (LensFor t x)) = K1 R (LensFor t x)
    GenericTableLens t (a :*: b) = GenericTableLens t a :*: GenericTableLens t b
    GenericTableLens t U1 = U1

class GTableLenses t (m :: * -> *) a (lensType :: * -> *) where
    gTableLenses :: Proxy a -> Lens' (t m) (a p) -> lensType ()
instance GTableLenses t m a al => GTableLenses t m (M1 s d a) (M1 s d al) where
    gTableLenses (Proxy :: Proxy (M1 s d a)) lensToHere = M1 $ gTableLenses (Proxy :: Proxy a) (\f -> lensToHere (\(M1 x) -> M1 <$> f x))
instance (GTableLenses t m a aLens, GTableLenses t m b bLens) => GTableLenses t m (a :*: b) (aLens :*: bLens) where
    gTableLenses (Proxy :: Proxy (a :*: b)) lensToHere = leftLenses :*: rightLenses
        where leftLenses = gTableLenses (Proxy :: Proxy a) (\f -> lensToHere (\(a :*: b) -> (:*: b) <$> f a))
              rightLenses = gTableLenses (Proxy :: Proxy b) (\f -> lensToHere (\(a :*: b) -> (a :*:) <$> f b))
instance Generic (t m) => GTableLenses t m (K1 R x) (K1 R (LensFor (t m) x)) where
    gTableLenses _ lensToHere = K1 (LensFor (\f -> lensToHere (\(K1 x) -> K1 <$> f x)))
instance ( Generic (PrimaryKey rel m)
         , Generic (PrimaryKey rel (Lenses t m))
         , GTableLenses t m (Rep (PrimaryKey rel m)) (Rep (PrimaryKey rel (Lenses t m))) ) =>
         GTableLenses t m (K1 R (ForeignKey rel m)) (K1 R (ForeignKey rel (Lenses t m))) where
    gTableLenses _ lensToHere = K1 (ForeignKey (to (gTableLenses (Proxy :: Proxy (Rep (PrimaryKey rel m))) (\f -> lensToHere (\(K1 (ForeignKey x)) -> K1 . ForeignKey . to <$> f (from x))))))

tableLenses' :: ( lensType ~ Lenses t f
                , Generic (t lensType)
                , Generic (t f)
                , GTableLenses t f (Rep (t f)) (Rep (t lensType)) ) =>
                 Proxy t -> Proxy f -> t lensType
tableLenses' (Proxy :: Proxy t) (Proxy :: Proxy f) =
    to (gTableLenses (Proxy :: Proxy (Rep (t f))) ((\f x -> to <$> (f (from x))) :: Lens' (t f) (Rep (t f) ())))

tableLenses :: ( lensType ~ Lenses t f
                , Generic (t lensType)
                , Generic (t f)
                , GTableLenses t f (Rep (t f)) (Rep (t lensType)) ) =>
               t (Lenses t f)
tableLenses = let res = tableLenses' (tProxy res) (fProxy res)

                  tProxy :: t (Lenses t f) -> Proxy t
                  tProxy _ = Proxy
                  fProxy :: t (Lenses t f) -> Proxy f
                  fProxy _ = Proxy
              in res

simpleTableLenses :: ( lensType ~ Lenses t Identity
                     , Generic (t lensType)
                     , Generic (t Identity)
                     , GTableLenses t Identity (Rep (t Identity)) (Rep (t lensType)) ) =>
                     t (Lenses t Identity)
simpleTableLenses = tableLenses

tableConfigLenses :: ( lensType ~ Lenses t (TableField t)
                     , Generic (t lensType)
                     , Generic (t (TableField t))
                     , GTableLenses t (TableField t) (Rep (t (TableField t))) (Rep (t lensType)) ) =>
                     t (Lenses t (TableField t))
tableConfigLenses = tableLenses

-- dbLenses :: ( Generic (db (LensForT db))
--             , Generic (db f)

--             , GTableLenses db f (Rep (db f)) (Rep (db (LensFor db))) ) =>
--             db (LensFor db)

-- dbLenses :: ( Generic (db (LensFor db))
--             , Generic (db f)

--             , GTableLenses db f (Rep (db f)) (Rep (db (LensFor db))) ) =>
--             db (LensFor db)
-- dbLenses = let res = dbLenses' (dbProxy res) (fProxy res)

--                   dbProxy :: t (Lenses t f) -> Proxy t
--                   dbProxy _ = Proxy
--                   fProxy :: t (Lenses t f) -> Proxy f
--                   fProxy _ = Proxy
--               in res
