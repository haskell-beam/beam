{-# LANGUAGE PolyKinds #-}
module Database.Beam.Schema.Lenses
    ( tableConfigLenses
    , TableLens(..), dbLenses ) where

import Database.Beam.Schema.Tables

import Control.Monad.Identity

import Data.Proxy

import GHC.Generics

import Lens.Micro hiding (to)

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
         GTableLenses t m (K1 R (PrimaryKey rel m)) (K1 R (PrimaryKey rel (Lenses t m))) where
    gTableLenses _ lensToHere = K1 (to (gTableLenses (Proxy :: Proxy (Rep (PrimaryKey rel m))) (\f -> lensToHere (\(K1 x) -> K1 . to <$> f (from x)))))

tableLenses' :: ( lensType ~ Lenses t f
                , Generic (t lensType)
                , Generic (t f)
                , GTableLenses t f (Rep (t f)) (Rep (t lensType)) ) =>
                 Proxy t -> Proxy f -> t lensType
tableLenses' (Proxy :: Proxy t) (Proxy :: Proxy f) =
    to (gTableLenses (Proxy :: Proxy (Rep (t f))) ((\f x -> to <$> f (from x)) :: Lens' (t f) (Rep (t f) ())))

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

-- | Automatically deduce lenses for 'TableSettings table'. You can expose the lenses at global level by doing a
--   top-level pattern match on 'tableConfigLenses', replacing every column in the pattern with `LensFor <nameOfLensForField>'.
--
--   For example,
--
-- > data AuthorT f = AuthorT
-- >                { _authorEmail     :: Columnar f Text
-- >                , _authorFirstName :: Columnar f Text
-- >                , _authorLastName  :: Columnar f Text }
-- >                  deriving Generic
-- >
-- > data BlogPostT f = BlogPost
-- >                  { _blogPostSlug    :: Columnar f Text
-- >                  , _blogPostBody    :: Columnar f Text
-- >                  , _blogPostDate    :: Columnar f UTCTime
-- >                  , _blogPostAuthor  :: PrimaryKey AuthorT f
-- >                  , _blogPostTagline :: Columnar f (Maybe Text) }
-- >                    deriving Generic
-- > instance Table BlogPostT where
-- >    data PrimaryKey BlogPostT f = BlogPostId (Columnar f Text)
-- >    primaryKey = BlogPostId . _blogPostSlug
-- > instance Table AuthorT where
-- >    data PrimaryKey AuthorT f = AuthorId (Columnar f Text)
-- >    primaryKey = AuthorId . _authorEmail
--
-- > BlogPost (LensFor blogPostSlug
-- >          (LensFor blogPostBody)
-- >          (LensFor blogPostDate)
-- >          (AuthorId (LensFor blogPostAuthorEmail))
-- >          (LensFor blogPostTagLine) = tableConfigLenses
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

-- Database lenses
-- type GenericLensesFor m a = GenericLenses a m (Rep (a m))
newtype TableLens f db (x :: k) = TableLens (Lens' (db f) (f x))

class GDatabaseLenses outer structure lensType where
    gDatabaseLenses :: Lens' outer (structure p) -> lensType ()
instance GDatabaseLenses db a al => GDatabaseLenses db (M1 s d a) (M1 s d al) where
    gDatabaseLenses lensToHere = M1 $ gDatabaseLenses (\f -> lensToHere (\(M1 x) -> M1 <$> f x))
instance (GDatabaseLenses db a al, GDatabaseLenses db b bl) => GDatabaseLenses db (a :*: b) (al :*: bl) where
    gDatabaseLenses lensToHere = leftLenses :*: rightLenses
        where leftLenses = gDatabaseLenses (\f -> lensToHere (\(a :*: b) -> (:*: b) <$> f a))
              rightLenses = gDatabaseLenses (\f -> lensToHere (\(a :*: b) -> (a :*:) <$> f b))
instance GDatabaseLenses (db f) (K1 R (f x))
                                (K1 R (TableLens f db x)) where
    gDatabaseLenses lensToHere = K1 (TableLens (\f -> lensToHere (\(K1 x) -> K1 <$> f x)))

dbLenses :: ( Generic (db (TableLens f db))
            , Generic (db f)
            , GDatabaseLenses (db f) (Rep (db f)) (Rep (db (TableLens f db))) )
           => db (TableLens f db)
dbLenses = fix $ \(_ :: db (TableLens f db)) ->
           to (gDatabaseLenses (\f (x :: db f) -> to <$> f (from x)) :: Rep (db (TableLens f db)) ())

-- type family GenericLenses c m a where
--     GenericLenses c m (D1 d a) = D1 d (GenericLenses c m a)
--     GenericLenses c m (C1 d a) = C1 d (GenericLenses c m a)
--     GenericLenses c m (S1 d a) = S1 d (GenericLenses c m a)
--     GenericLenses c m (K1 R (m x)) = K1 R (TableLens m c x)
--     GenericLenses c m (a :*: b) = GenericLenses c m a :*: GenericLenses c m b
--     GenericLenses c m U1 = U1

-- class GLenses c m a where
--     glenses :: (forall f. Functor f => (a p -> f (a p)) -> c m -> f (c m)) -> GenericLenses c m a p
-- instance GLenses c m a => GLenses c m (D1 d a) where
--     glenses (set :: forall f. Functor f => (D1 d a p -> f (D1 d a p)) -> c m -> f (c m)) = M1 $ glenses (\modifier -> set ((M1 <$>) . modifier . unM1)) :: D1 d (GenericLenses c m a) p
-- instance GLenses c m a => GLenses c m (C1 d a) where
--     glenses (set :: forall f. Functor f => (C1 d a p -> f (C1 d a p)) -> c m -> f (c m)) = M1 $ glenses (\modifier -> set ((M1 <$>) . modifier . unM1)) :: C1 d (GenericLenses c m a) p
-- instance GLenses c m a => GLenses c m (S1 d a) where
--     glenses (set :: forall f. Functor f => (S1 d a p -> f (S1 d a p)) -> c m -> f (c m)) = M1 $ glenses (\modifier -> set ((M1 <$>) . modifier . unM1)) :: S1 d (GenericLenses c m a) p
-- instance (GLenses c m a, GLenses c m b) => GLenses c m (a :*: b) where
--     glenses (set :: forall f. Functor f => ((a :*: b) p -> f ((a :*: b) p)) -> c m -> f (c m)) = glenses modifyLeft :*: glenses modifyRight :: GenericLenses c m (a :*: b) p
--         where modifyLeft :: forall f. Functor f => (a p -> f (a p)) -> c m -> f (c m)
--               modifyLeft modifier = set (\(a :*: b) -> (:*: b) <$> modifier a)
--               modifyRight :: forall f. Functor f => (b p -> f (b p)) -> c m -> f (c m)
--               modifyRight modifier = set (\(a :*: b) -> (a :*:) <$> modifier b)
-- instance GLenses c m (K1 R (m a)) where
--     glenses (set :: forall f. Functor f => (K1 R (m a) p -> f (K1 R (m a) p)) -> c m -> f (c m)) = K1 (TableLens modify)
--         where modify :: forall f. Functor f => (m a -> f (m a)) -> c m -> f (c m)
--               modify modifier = set ((K1 <$>) . modifier . unK1)

-- dbConfigLenses' :: ( GLenses a m (Rep (a m))
--                    , Generic (a m), Generic (a (TableLens m a))
--                    , GenericLensesFor m a ~ Rep (a (TableLens m a)) ) => Proxy a -> Proxy m -> a (TableLens m a)
-- dbConfigLenses' (_ :: Proxy a) (_ :: Proxy m) = to (glenses modify)
--     where modify :: forall f. Functor f => (Rep (a m) ()  -> f (Rep (a m) ())) -> a m -> f (a m)
--           modify modifier = (to <$>) . modifier . from

-- dbConfigLenses ::  ( GLenses a m (Rep (a m))
--               , Generic (a m), Generic (a (TableLens m a))
--               , GenericLensesFor m a ~ Rep (a (TableLens m a)) ) => a (TableLens m a)
-- dbConfigLenses = inj dbConfigLenses'
--     where inj :: (Proxy a -> Proxy m -> a (TableLens m a)) -> a (TableLens m a)
--           inj f = f Proxy Proxy
