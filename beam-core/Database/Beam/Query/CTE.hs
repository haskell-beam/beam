{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Database.Beam.Query.CTE where

import Database.Beam.Backend.SQL
import Database.Beam.Query.Internal
import Database.Beam.Query.Types

import Control.Monad.Free.Church
import Control.Monad.Writer hiding ((<>))
import Control.Monad.State.Strict

import Data.Text (Text)
import Data.String
import Data.Proxy (Proxy(Proxy))
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

data Recursiveness be where
    Nonrecursive :: Recursiveness be
    Recursive    :: IsSql99RecursiveCommonTableExpressionSelectSyntax (BeamSqlBackendSelectSyntax be)
                 => Recursiveness be

instance Monoid (Recursiveness be) where
    mempty = Nonrecursive
    mappend Recursive _ = Recursive
    mappend _ Recursive = Recursive
    mappend _ _ = Nonrecursive

instance Semigroup (Recursiveness be) where
  (<>) = mappend

-- | Monad in which @SELECT@ statements can be made (via 'selecting')
-- and bound to result names for re-use later. This has the advantage
-- of only computing each result once. In SQL, this is translated to a
-- common table expression.
--
-- Once introduced, results can be re-used in future queries with 'reuse'.
--
-- 'With' is also a member of 'MonadFix' for backends that support
-- recursive CTEs. In this case, you can use @mdo@ or @rec@ notation
-- (with @RecursiveDo@ enabled) to bind result values (again, using
-- 'reuse') even /before/ they're introduced.
--
-- See further documentation <http://tathougies.github.io/beam/user-guide/queries/common-table-expressions/ here>.
newtype With be (db :: (* -> *) -> *) a
    = With { runWith :: WriterT (Recursiveness be, [ BeamSql99BackendCTESyntax be ])
                                (State Int) a }
    deriving (Monad, Applicative, Functor)

instance IsSql99RecursiveCommonTableExpressionSelectSyntax (BeamSqlBackendSelectSyntax be)
    => MonadFix (With be db) where
    mfix f = With (tell (Recursive, mempty) >> mfix (runWith . f))

data QAnyScope

-- | Query results that have been introduced into a common table
-- expression via 'selecting' that can be used in future queries with
-- 'reuse'.
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
                 Q $ liftF (QAll (\_ -> fromTable (tableNamed (tableName Nothing tblNm)) . Just . (, Nothing))
                                 (\tblNm' -> fst $ mkFieldNames @be @res (qualifiedField tblNm'))
                                 (\_ -> Nothing)
                                 (rewriteThread @QAnyScope @res proxyS . snd)))

-- | Introduce the result of a query as a result in a common table
-- expression. The returned value can be used in future queries by
-- applying 'reuse'.
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

-- | Introduces the result of a previous 'selecting' (a CTE) into a new query
reuse :: forall s be db res
       . ReusableQ be db res -> Q be db s (WithRewrittenThread QAnyScope s res)
reuse (ReusableQ _ q) = q (Proxy @s)

