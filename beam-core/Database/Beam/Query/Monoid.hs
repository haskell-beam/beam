{- | @newtype@ wrappers for choosing specific Monoid operations on SQL expressions.
-}
module Database.Beam.Query.Monoid
  ( ExprAll(..)
  , ExprAny(..)
  ) where

import qualified Data.List.NonEmpty as NEL
import Data.Semigroup(Semigroup(..), stimesIdempotentMonoid)
import Database.Beam.Backend
import Database.Beam.Query.Internal
import Database.Beam.Query.Operator
import Database.Beam.Query.Combinators
import Database.Beam.Query.Ord

-- | A default mconcat implementation which only produces an 'mempty' if
-- necessary.
-- It is slightly less lazy, checking whether the list is empty before returning
-- `mempty` or `sconcat`, but this laziness isn't used here anyway.
mconcatMinimal :: Monoid m => [m] -> m
mconcatMinimal = maybe mempty sconcat . NEL.nonEmpty

-- | A 'Monoid' and 'Semigroup' using the "and" operation, analogous with 'Data.Monoid.All'.
--
-- Allows search criteria expressions to be composed monoidally, for example.
newtype ExprAll a = ExprAll {getExprAll :: a}

-- | Boolean conjunction using '&&.'.
instance BeamSqlBackend be => Semigroup (ExprAll (QGenExpr context be s Bool)) where
  ExprAll a <> ExprAll b = ExprAll (a &&. b)
  stimes = stimesIdempotentMonoid

-- | Boolean conjunction using '&&.'.
instance BeamSqlBackend be => Monoid (ExprAll (QGenExpr context be s Bool)) where
  mempty = ExprAll (val_ True)
  mconcat = mconcatMinimal

-- | Tri-state SQL boolean conjunction using '&&?.'.
instance BeamSqlBackend be => Semigroup (ExprAll (QGenExpr context be s SqlBool)) where
  ExprAll a <> ExprAll b = ExprAll (a &&?. b)

-- | Tri-state SQL boolean conjunction using '&&?.'.
instance BeamSqlBackend be => Monoid (ExprAll (QGenExpr context be s SqlBool)) where
  mempty = ExprAll (sqlBool_ (val_ True))
  mconcat = mconcatMinimal

-- | A 'Monoid' and 'Semigroup' for the "or" operation, analogous with 'Data.Monoid.Any'.
--
-- Allows alternatives within search criteria expressions to be composed monoidally, for example.
newtype ExprAny a = ExprAny {getExprAny :: a}

-- | Boolean disjunction using '||.'.
instance BeamSqlBackend be => Semigroup (ExprAny (QGenExpr context be s Bool)) where
  ExprAny a <> ExprAny b = ExprAny (a ||. b)
  stimes = stimesIdempotentMonoid

-- | Boolean disjunction using '||.'.
instance BeamSqlBackend be => Monoid (ExprAny (QGenExpr context be s Bool)) where
  mempty = ExprAny (val_ False)
  mconcat = mconcatMinimal

-- | Tri-state SQL boolean disjunction using '||?.'.
instance BeamSqlBackend be => Semigroup (ExprAny (QGenExpr context be s SqlBool)) where
  ExprAny a <> ExprAny b = ExprAny (a ||?. b)

-- | Tri-state SQL boolean disjunction using '||?.'.
instance BeamSqlBackend be => Monoid (ExprAny (QGenExpr context be s SqlBool)) where
  mempty = ExprAny (sqlBool_ (val_ False))
  mconcat = mconcatMinimal
