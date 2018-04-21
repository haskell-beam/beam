{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Schema.Tables.GPrimaryKeyOf 
     ( GPrimaryKeyOf()
     , gPrimaryKey
     ) where

import GHC.Generics

{- Whenever the first columns of a primary key matches the first columns of the table, it is possible to auto-derive
   `primaryKey` based on `gPrimaryKey`.
-}

class GPrimaryKeyOf consumer producer where
  -- | Constructors from `producer` unused after having taken the first
  --   ones to create a `consumer`
  type GRemainds consumer producer :: * -> *
  
  gPrimaryKey :: producer a -> (consumer a, GRemainds consumer producer a)


instance ( GPrimaryKeyOf consumer producer
         ) => GPrimaryKeyOf (D1 meta1 consumer) (D1 meta2 producer) 
   where
   
    type GRemainds (D1 meta1 consumer) (D1 meta2 producer) = GRemainds consumer producer
    gPrimaryKey (M1 x) = let (y,x') = gPrimaryKey x
                          in (M1 y, x')

instance ( GPrimaryKeyOf consumer producer
         ) => GPrimaryKeyOf (C1 meta1 consumer) (C1 meta2 producer) 
   where
    type GRemainds (C1 meta1 consumer) (C1 meta2 producer) = GRemainds consumer producer
    gPrimaryKey (M1 x) = let (y,x') = gPrimaryKey x
                          in (M1 y, x')

instance ( GPrimaryKeyOf consumer producer
         ) => GPrimaryKeyOf (S1 meta1 consumer) (S1 meta2 producer) 
   where
    type GRemainds (S1 meta1 consumer) (S1 meta2 producer) = GRemainds consumer producer
    gPrimaryKey (M1 x) = let (y,x') = gPrimaryKey x
                          in (M1 y, x')


instance GPrimaryKeyOf U1 producer where
    type GRemainds U1 producer = producer
    gPrimaryKey x = (U1,x)

instance ( GPrimaryKeyOf consumer1 producer
         , GPrimaryKeyOf consumer2 (GRemainds consumer1 producer)
         
         ) => GPrimaryKeyOf (consumer1 :*: consumer2) producer 
   where
    type GRemainds (consumer1 :*: consumer2) producer = GRemainds consumer2 (GRemainds consumer1 producer)
    gPrimaryKey x = let (y1,x' ) = gPrimaryKey x
                        (y2,x'') = gPrimaryKey x'
                
                     in ( y1:*:y2 , x'')


instance GPrimaryKeyOf (K1 info1 a) (K1 info2 a) 
   where
    type GRemainds (K1 info1 a) (K1 info2 a) = U1
    gPrimaryKey (K1 x) = (K1 x, U1)

instance ( GPrimaryKeyOf (S1 info1 consumer) producer1
         , TypeProduct (GRemainds (S1 info1 consumer) producer1) producer2
                       (ProductIgnoringUnit (GRemainds (S1 info1 consumer) producer1) producer2)

         ) => GPrimaryKeyOf (S1 info1 consumer) (producer1 :*: producer2) 
   where
    type GRemainds (S1 info1 consumer) (producer1 :*: producer2) = ProductIgnoringUnit (GRemainds (S1 info1 consumer) producer1) producer2
    
    gPrimaryKey (x1 :*: x2) = let (y,x1') = gPrimaryKey x1
                               in (y, typeProduct x1' x2)

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
-- Auxiliar classes:

class TypeProduct a b c where
    typeProduct :: a x -> b x -> c x

instance TypeProduct U1 a a where
    typeProduct _ a = a

instance TypeProduct a b (a:*:b) where
    typeProduct a b = a :*: b

type family ProductIgnoringUnit (a :: * -> *) (b :: * -> *) :: * -> * where
    ProductIgnoringUnit U1 g = g
    ProductIgnoringUnit f  g = f :*: g




