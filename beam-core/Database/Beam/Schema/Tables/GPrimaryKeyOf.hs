{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds      #-}


module Database.Beam.Schema.Tables.GPrimaryKeyOf
     ( IsKeyDerivableFrom()
     , derivedPrimaryKey
     ) where

import GHC.Generics
import GHC.TypeLits
import GHC.Exts(Constraint)
import Data.Proxy
{- Whenever the first columns of a primary key matches the first columns of the table, it is possible to auto-derive
   `primaryKey` based on `gPrimaryKey`.
-}


class IsKeyDerivableFrom consumer producer where
    derivedPrimaryKey :: producer -> consumer

instance ( Generic consumer
         , Generic producer
         , GPrimaryKeyOf producer (Rep consumer) (Rep producer)
         ) => IsKeyDerivableFrom consumer producer 
   where
    derivedPrimaryKey = to . fst . gPrimaryKey (Proxy :: Proxy producer) . from
    

class GPrimaryKeyOf debugInfo consumer producer where
  -- | Constructors from `producer` unused after having taken the first
  --   ones to create a `consumer`
  type GRemainds consumer producer :: * -> *
  
  gPrimaryKey :: Proxy debugInfo -> producer a -> (consumer a, GRemainds consumer producer a)


instance ( GPrimaryKeyOf debugInfo consumer producer
         ) => GPrimaryKeyOf debugInfo (D1 meta1 consumer) (D1 meta2 producer) 
   where
   
    type GRemainds (D1 meta1 consumer) (D1 meta2 producer) = GRemainds consumer producer
    gPrimaryKey debugInfo (M1 x) = let (y,x') = gPrimaryKey debugInfo x
                                    in (M1 y, x')

instance ( GPrimaryKeyOf debugInfo consumer producer
         ) => GPrimaryKeyOf debugInfo (C1 meta1 consumer) (C1 meta2 producer) 
   where
    type GRemainds (C1 meta1 consumer) (C1 meta2 producer) = GRemainds consumer producer
    gPrimaryKey debugInfo (M1 x) = let (y,x') = gPrimaryKey debugInfo x
                                    in (M1 y, x')

instance ( GPrimaryKeyOf debugInfo consumer producer
         ) => GPrimaryKeyOf debugInfo (S1 meta1 consumer) (S1 meta2 producer) 
   where
    type GRemainds (S1 meta1 consumer) (S1 meta2 producer) = GRemainds consumer producer
    gPrimaryKey debugInfo (M1 x) = let (y,x') = gPrimaryKey debugInfo x
                                    in (M1 y, x')


instance GPrimaryKeyOf debugInfo U1 producer where
    type GRemainds U1 producer = producer
    gPrimaryKey _ x = (U1,x)

instance ( GPrimaryKeyOf debugInfo consumer1 producer
         , GPrimaryKeyOf debugInfo consumer2 (GRemainds consumer1 producer)
         
         ) => GPrimaryKeyOf debugInfo (consumer1 :*: consumer2) producer 
   where
    type GRemainds (consumer1 :*: consumer2) producer = GRemainds consumer2 (GRemainds consumer1 producer)
    gPrimaryKey debugInfo x = let (y1,x' ) = gPrimaryKey debugInfo x
                                  (y2,x'') = gPrimaryKey debugInfo x'
                
                               in ( y1:*:y2 , x'')


instance GPrimaryKeyOf debugInfo (K1 info1 a) (K1 info2 a) 
   where
    type GRemainds (K1 info1 a) (K1 info2 a) = U1
    gPrimaryKey _ (K1 x) = (K1 x, U1)


instance ( ReportToLongPrimaryKey debugInfo
         ) => GPrimaryKeyOf debugInfo (S1 info1 a) U1 
   where
    type GRemainds (S1 info1 a) U1 = U1
    gPrimaryKey _ _ = error "this branch can not be reached"

instance {-# INCOHERENT #-} 
         ( ReportMissfitError debugInfo
         ) => GPrimaryKeyOf debugInfo (K1 info1 a) (K1 info2 b) 
   where
    type GRemainds (K1 info1 a) (K1 info2 b) = U1
    gPrimaryKey _ _ = error "this branch can not be reached"



instance ( GPrimaryKeyOf debugInfo (S1 info1 consumer) producer1
         , TypeProduct (GRemainds (S1 info1 consumer) producer1) producer2
                       (ProductIgnoringUnit (GRemainds (S1 info1 consumer) producer1) producer2)

         ) => GPrimaryKeyOf debugInfo (S1 info1 consumer) (producer1 :*: producer2) 
   where
    type GRemainds (S1 info1 consumer) (producer1 :*: producer2) = ProductIgnoringUnit (GRemainds (S1 info1 consumer) producer1) producer2
    
    gPrimaryKey debugInfo (x1 :*: x2) = let (y,x1') = gPrimaryKey debugInfo x1
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



type family ReportToLongPrimaryKey debugInfo :: Constraint where
     ReportToLongPrimaryKey  debugInfo = TypeError 
      (     'Text    "`primaryKey` for table:"
      ':$$: 'ShowType debugInfo
      ':$$: 'Text    "could not be derived because the table has more fields than its primary key."
      ':$$: 'Text    "You must explicitly define the `primaryKey` method"
      )

type family ReportMissfitError debugInfo :: Constraint where
     ReportMissfitError  debugInfo = TypeError 
      (     'Text    "First primary key fields do not match first fields from the table:"
      ':$$: 'ShowType debugInfo
      ':$$: 'Text    "so function `primaryKey` could not be derived."
      ':$$: 'Text    "You must either explicitly define or rearrange fields such they match."
      )


