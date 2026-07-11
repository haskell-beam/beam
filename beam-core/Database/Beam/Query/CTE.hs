{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Construction and reuse of common table expressions.
--
-- The 'CtePlacement' index records a property which SQL otherwise checks only
-- at execution time: whether a complete @WITH@ block may be placed inside a
-- subquery. Most users do not need to mention the index because 'selecting' is
-- placement-polymorphic and backend-specific operations refine it as needed.
module Database.Beam.Query.CTE
  ( CtePlacement(..)
  , With, runWith
  , toTopLevel
  , Recursiveness(..)
  , QAnyScope
  , ReusableQ(..)
  , reusableForCTE
  , selecting
  , dataModifyingCte
  , reuse
  ) where

import Database.Beam.Backend.SQL
import Database.Beam.Query.Internal
import Database.Beam.Query.Types

import Control.Monad.Fix
import Control.Monad.Free.Church
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.State.Strict

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.String
import Data.Text (Text)

data Recursiveness be where
    Nonrecursive :: Recursiveness be
    Recursive    :: IsSql99RecursiveCommonTableExpressionSelectSyntax (BeamSqlBackendSelectSyntax be)
                 => Recursiveness be

instance Monoid (Recursiveness be) where
    mempty = Nonrecursive
    mappend = (<>)

instance Semigroup (Recursiveness be) where
    Recursive <> _ = Recursive
    _ <> Recursive = Recursive
    _ <> _ = Nonrecursive

-- | Whether a common-table-expression block may be embedded in a subquery or
-- must remain attached to a top-level statement.
--
-- Plain @SELECT@ CTEs can be built at either placement. A data-modifying CTE
-- forces its enclosing 'With' block to 'CteTopLevelOnly'. This prevents
-- backends such as PostgreSQL from embedding data-modifying statements in a
-- location where the server would reject them.
data CtePlacement
  = CteNestedAllowed -- ^ The complete @WITH@ block is safe in a subquery.
  | CteTopLevelOnly  -- ^ The complete @WITH@ block must remain top-level.

-- | Monad in which @SELECT@ statements can be made (via 'selecting')
-- and bound to result names for re-use later. This has the advantage
-- of only computing each result once. In SQL, this is translated to a
-- common table expression.
--
-- Once introduced, results can be re-used in future queries with 'reuse'.
--
-- A nested-safe 'With' block is also a member of 'MonadFix' for backends that
-- support recursive CTEs. In this case, you can use @mdo@ or @rec@ notation
-- (with @RecursiveDo@ enabled) to bind result values (again, using 'reuse')
-- even /before/ they're introduced. Use 'toTopLevel' after constructing a
-- recursive @SELECT@ block if it must be combined with data-modifying CTEs.
--
-- The 'CtePlacement' index records whether the block may be embedded in a
-- subquery. It is normally inferred: 'selecting' is valid at either placement,
-- while a backend-specific data-modifying operation makes the complete block
-- 'CteTopLevelOnly'.
--
-- A normal, non-recursive use looks like:
--
-- > selectWith $ do
-- >   reusableRows <- selecting someQuery
-- >   pure (reuse reusableRows)
--
-- See further documentation <https://haskell-beam.github.io/beam/user-guide/queries/common-table-expressions/ here>.
newtype With be (db :: (Type -> Type) -> Type) (placement :: CtePlacement) a
    = With
        { -- | Unwrap a CTE builder. This is primarily intended for top-level
          -- statement consumers such as @selectWith@ and backend-specific
          -- equivalents.
          runWith :: WriterT (Recursiveness be, [ BeamSql99BackendCTESyntax be ])
                             (State Int) a
        }
    deriving (Monad, Applicative, Functor)

-- The placement index is phantom in the runtime representation. Keep every
-- parameter nominal so Data.Coerce cannot relabel a top-level-only block and
-- bypass the smart constructors which establish the invariant.
type role With nominal nominal nominal nominal

-- Restrict the recursive knot to SELECT-only, nested-safe construction. A
-- data-modifying operation fixes the placement to CteTopLevelOnly and therefore
-- cannot recursively depend on its own RETURNING rows.
instance IsSql99RecursiveCommonTableExpressionSelectSyntax (BeamSqlBackendSelectSyntax be)
    => MonadFix (With be db 'CteNestedAllowed) where
    mfix f = With (tell (Recursive, mempty) >> mfix (runWith . f))

-- | Promote a nested-safe CTE block for composition with top-level-only CTEs.
--
-- Recursion is deliberately available only while constructing a
-- 'CteNestedAllowed' block. Promote the completed recursive @SELECT@ block with
-- this function before sequencing it with data-modifying CTEs. This permits
-- recursive queries to feed data-modifying statements without allowing a
-- data-modifying statement itself to participate in the recursive knot.
--
-- For example, a backend can first finish the recursive, SELECT-only portion
-- and then continue in a top-level block:
--
-- > recursiveRows <- toTopLevel $ mdo
-- >   rows <- selecting (seedQuery `unionAll_` stepQuery (reuse rows))
-- >   pure rows
-- > changedRows <- backendDataModifyingCte recursiveRows
toTopLevel
  :: With be db 'CteNestedAllowed a
  -> With be db 'CteTopLevelOnly a
toTopLevel (With action) = With action

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
--
-- > reusableRows <- selecting someQuery
-- > pure $ do
-- >   row <- reuse reusableRows
-- >   guard_ (isWanted row)
-- >   pure row
selecting :: forall res be db placement
           . ( BeamSql99CommonTableExpressionBackend be, HasQBuilder be
             , Projectible be res
             , ThreadRewritable QAnyScope res )
          => Q be db QAnyScope res -> With be db placement (ReusableQ be db res)
selecting q =
  With $ do
    cteId <- get
    put (cteId + 1)

    let tblNm = fromString ("cte" ++ show cteId)

        (_ :: res, fieldNames) = mkFieldNames @be (qualifiedField tblNm)
    tell (Nonrecursive, [ cteSubquerySyntax tblNm fieldNames (buildSqlQuery (tblNm <> "_") q) ])

    pure (reusableForCTE tblNm)

-- | Introduce the result of a backend-specific data-modifying statement as a
-- common table expression. The statement is expected to return rows shaped like
-- @res@, for example by using @INSERT ... RETURNING@, @UPDATE ... RETURNING@,
-- or @DELETE ... RETURNING@ on backends that support those forms.
--
-- This is a low-level helper intended for backend-specific APIs. The returned
-- value can be used in future queries by applying 'reuse'. Its enclosing
-- 'With' block is marked 'CteTopLevelOnly', so it cannot be passed to a backend
-- combinator for nested CTEs.
--
-- Backend APIs normally obtain @body@ from an existing @... RETURNING@
-- builder, then expose a typed wrapper to users:
--
-- > backendCteReturning statement =
-- >   dataModifyingCte (backendDataModifyingSyntax statement)
--
-- This produces one definition such as:
--
-- @
-- changed(res0) AS (DELETE FROM items WHERE expired RETURNING id)
-- @
dataModifyingCte :: forall res be db
                  . ( BeamSql99DataModifyingCommonTableExpressionBackend be
                  , Projectible be res
                  , ThreadRewritable QAnyScope res )
             => Sql99CTEDataModifyingSyntax (BeamSql99BackendCTESyntax be)
             -> With be db 'CteTopLevelOnly (ReusableQ be db res)
dataModifyingCte body =
  With $ do
    cteId <- get
    put (cteId + 1)

    let tblNm = fromString ("cte" ++ show cteId)

        (_ :: res, fieldNames) = mkFieldNames @be (qualifiedField tblNm)
    tell (Nonrecursive, [ cteDataModifyingSyntax tblNm fieldNames body ])

    pure (reusableForCTE tblNm)

-- | Introduces the result of a previous 'selecting' (a CTE) into a new query
reuse :: forall s be db res
       . ReusableQ be db res -> Q be db s (WithRewrittenThread QAnyScope s res)
reuse (ReusableQ _ q) = q (Proxy @s)
