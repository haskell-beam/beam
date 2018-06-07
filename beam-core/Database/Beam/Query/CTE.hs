{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Query.CTE where

import Database.Beam.Backend.SQL
import Database.Beam.Query.Internal
import Database.Beam.Query.Types

import Control.Monad.Free.Church
import Control.Monad.Writer
import Control.Monad.State.Strict

import Data.Text (Text)
import Data.String
import Data.Proxy (Proxy(Proxy))

import Unsafe.Coerce

data Recursiveness be where
    Nonrecursive :: Recursiveness be
    Recursive    :: IsSql99RecursiveCommonTableExpressionSelectSyntax (BeamSqlBackendSelectSyntax be)
                 => Recursiveness be

instance Monoid (Recursiveness be) where
    mempty = Nonrecursive
    mappend Recursive _ = Recursive
    mappend _ Recursive = Recursive
    mappend _ _ = Nonrecursive

newtype With be (db :: (* -> *) -> *) a
    = With { runWith :: WriterT (Recursiveness be, [ BeamSql99BackendCTESyntax be ])
                                (State Int) a }
    deriving (Monad, Applicative, Functor)

instance IsSql99RecursiveCommonTableExpressionSelectSyntax (BeamSqlBackendSelectSyntax be)
    => MonadFix (With be db) where
    mfix f = With (tell (Recursive, mempty) >> mfix (runWith . f))

data QAnyScope

data ReusableQ be db res where
    ReusableQ :: Proxy res -> (forall s. Proxy s -> Q be db s (WithRewrittenThread QAnyScope s res)) -> ReusableQ be db res

mkFieldsSkeleton :: forall be res m
                  . (Projectible be res, MonadState Int m)
                 => (Int -> m (WithExprContext (BeamSqlBackendExpressionSyntax' be))) -> m res
mkFieldsSkeleton go =
    projectSkeleton' (Proxy @AnyType) (Proxy @(be, WithExprContext (BeamSqlBackendExpressionSyntax' be))) $ \_ _ ->
    do i <- get
       put (i + 1)
       go i

mkFieldNames :: forall be res
              . ( BeamSqlBackend be, Projectible be res )
             => Text -> ( res, [ Text ])
mkFieldNames tblNm =
    runWriter . flip evalStateT 0 $
    mkFieldsSkeleton @be @res $ \i -> do
      let fieldName = fromString ("res" ++ show i)
      tell [ fieldName ]
      pure (\_ -> BeamSqlBackendExpressionSyntax' (fieldE (qualifiedField tblNm fieldName)))

reusableForCTE :: forall be res db
                . ( ThreadRewritable QAnyScope res
                  , Projectible be res
                  , BeamSqlBackend be )
               => Text -> ReusableQ be db res
reusableForCTE tblNm =
    ReusableQ (Proxy @res)
              (\proxyS ->
                 Q $ liftF (QAll (\_ -> fromTable (tableNamed tblNm) . Just)
                                 (\tblNm' -> fst $ mkFieldNames @be @res tblNm')
                                 (\_ -> Nothing)
                                 (rewriteThread @QAnyScope @res proxyS . snd)))

selecting :: forall res be db
           . ( BeamSql99CommonTableExpressionBackend be, HasQBuilder be
             , Projectible be res
             , ThreadRewritable QAnyScope res )
          => Q be db QAnyScope res -> With be db (ReusableQ be db res)
selecting q =
  With $ do
    cteId <- get
    put (cteId + 1)

    let tblNm = fromString ("cte" ++ show cteId)

        (_ :: res, fieldNames) = mkFieldNames @be tblNm
    tell (Nonrecursive, [ cteSubquerySyntax tblNm fieldNames (buildSqlQuery (tblNm <> "_") q) ])

    pure (reusableForCTE tblNm)

rescopeQ :: QM be db s res -> QM be db s' res
rescopeQ = unsafeCoerce

reuse :: forall s be db res
       . ReusableQ be db res -> Q be db s (WithRewrittenThread QAnyScope s res)
reuse (ReusableQ _ q) = q (Proxy @s)

