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

reusableForCTE :: forall be res db
                . ( ThreadRewritable QAnyScope res
                  , Projectible be res
                  , BeamSqlBackend be )
               => Text -> ReusableQ be db res
reusableForCTE tblNm =
    ReusableQ (Proxy @res)
              (\proxyS ->
                 Q $ liftF (QAll (\_ -> fromTable (tableNamed tblNm) . Just . (, Nothing))
                                 (\tblNm' -> fst $ mkFieldNames @be @res (qualifiedField tblNm'))
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

        (_ :: res, fieldNames) = mkFieldNames @be (qualifiedField tblNm)
    tell (Nonrecursive, [ cteSubquerySyntax tblNm fieldNames (buildSqlQuery (tblNm <> "_") q) ])

    pure (reusableForCTE tblNm)

rescopeQ :: QM be db s res -> QM be db s' res
rescopeQ = unsafeCoerce

reuse :: forall s be db res
       . ReusableQ be db res -> Q be db s (WithRewrittenThread QAnyScope s res)
reuse (ReusableQ _ q) = q (Proxy @s)

