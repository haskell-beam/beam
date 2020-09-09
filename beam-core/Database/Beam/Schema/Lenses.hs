{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Schema.Lenses
    ( tableLenses
    , TableLens(..)

    , dbLenses ) where

import Database.Beam.Schema.Tables

import Control.Monad.Identity

import Data.Kind (Type)
import Data.Proxy

import GHC.Generics

import Lens.Micro hiding (to)

class GTableLenses t (m :: Type -> Type) a (lensType :: Type -> Type) where
    gTableLenses :: Proxy a -> Lens' (t m) (a p) -> lensType ()
instance GTableLenses t m a al => GTableLenses t m (M1 s d a) (M1 s d al) where
    gTableLenses (Proxy :: Proxy (M1 s d a)) lensToHere = M1 $ gTableLenses (Proxy :: Proxy a) (\f -> lensToHere (\(M1 x) -> M1 <$> f x))
instance (GTableLenses t m a aLens, GTableLenses t m b bLens) => GTableLenses t m (a :*: b) (aLens :*: bLens) where
    gTableLenses (Proxy :: Proxy (a :*: b)) lensToHere = leftLenses :*: rightLenses
        where leftLenses = gTableLenses (Proxy :: Proxy a) (\f -> lensToHere (\(a :*: b) -> (:*: b) <$> f a))
              rightLenses = gTableLenses (Proxy :: Proxy b) (\f -> lensToHere (\(a :*: b) -> (a :*:) <$> f b))
instance Generic (t m) => GTableLenses t m (K1 R x) (K1 R (LensFor (t m) x)) where
    gTableLenses _ lensToHere = K1 (LensFor (\f -> lensToHere (\(K1 x) -> K1 <$> f x)))

instance ( Generic (sub m)
         , Generic (sub (Lenses t m))
         , GTableLenses t m (Rep (sub m)) (Rep (sub (Lenses t m))) ) =>
         GTableLenses t m (K1 R (sub m)) (K1 R (sub (Lenses t m))) where
    gTableLenses _ lensToHere = K1 (to (gTableLenses (Proxy :: Proxy (Rep (sub m))) (\f -> lensToHere (\(K1 x) -> K1 . to <$> f (from x)))))

instance ( Generic (sub (Nullable m))
         , Generic (sub (Nullable (Lenses t m)))
         , GTableLenses t m (Rep (sub (Nullable m))) (Rep (sub (Nullable (Lenses t m))))) =>
         GTableLenses t m (K1 R (sub (Nullable m))) (K1 R (sub (Nullable (Lenses t m)))) where
    gTableLenses _ lensToHere = K1 (to (gTableLenses (Proxy :: Proxy (Rep (sub (Nullable m)))) (\f -> lensToHere (\(K1 x) -> K1 . to <$> f (from x)))))

tableLenses' :: ( lensType ~ Lenses t f
                , Generic (t lensType)
                , Generic (t f)
                , GTableLenses t f (Rep (t f)) (Rep (t lensType)) ) =>
                 Proxy t -> Proxy f -> t lensType
tableLenses' (Proxy :: Proxy t) (Proxy :: Proxy f) =
    to (gTableLenses (Proxy :: Proxy (Rep (t f))) ((\f x -> to <$> f (from x)) :: Lens' (t f) (Rep (t f) ())))

-- | Automatically deduce lenses for a table over any column tag. lenses at
--   global level by doing a top-level pattern match on 'tableLenses', replacing
--   every column in the pattern with `LensFor <nameOfLensForField>'. The lenses
--   are generated per-column, not per field in the record. Thus if you have
--   nested 'Beamable' types, lenses are generated for each nested field.
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
-- >          (LensFor blogPostTagLine) = tableLenses
--
--   Note: In order to have GHC deduce the right type, you will need to turn off
--   the monomorphism restriction. This is a part of the Haskell standard that
--   specifies that top-level definitions must be inferred to have a monomorphic
--   type. However, lenses need a polymorphic type to work properly. You can
--   turn off the monomorphism restriction by enabling the
--   'NoMonomorphismRestriction' extension. You can do this per-file by using
--   the {-# LANGUAGE NoMonomorphismRestriction #-} pragma at the top of the
--   file. You can also pass the @-XNoMonomorphismRestriction@ command line flag
--   to GHC during compilation.
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

-- | Like 'tableLenses' but for types that are instances of 'Database'. Instead
--   of pattern matching on 'LensFor', pattern match on 'TableLens'.
dbLenses :: ( Generic (db (TableLens f db))
            , Generic (db f)
            , GDatabaseLenses (db f) (Rep (db f)) (Rep (db (TableLens f db))) )
           => db (TableLens f db)
dbLenses = fix $ \(_ :: db (TableLens f db)) ->
           to (gDatabaseLenses (\f (x :: db f) -> to <$> f (from x)) :: Rep (db (TableLens f db)) ())
