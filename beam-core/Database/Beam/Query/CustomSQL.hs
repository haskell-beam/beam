{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Allows the creation of custom SQL expressions from arbitrary 'ByteStrings'.
--
--   Simply create a function with an arbitrary number of 'ByteString' arguments
--   that returns a 'ByteString'. Then, apply 'customExpr_' to your function.
--   This will result in a function with the same arity, that takes in
--   'QGenExpr's instead of 'ByteString's', and returns a 'QGenExpr' as well.
--
--   Semantically, the expression builder function is called with arguments
--   representing SQL expressions, that, when evaluated, will evaluate to the
--   result of the expressions supplied as arguments to 'customExpr_'. See the
--   section on <http://tathougies.github.io/beam/user-guide/queries/custom-queries/  extensibility>
--   in the user guide for example usage.
module Database.Beam.Query.CustomSQL
  (
  -- * The 'customExpr_' function
    customExpr_
  -- ** Type-inference help
  , valueExpr_, agg_

  -- * For backends
  , IsCustomSqlSyntax(..) ) where

import Database.Beam.Query.Internal
import Database.Beam.Backend.SQL.Builder

import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy (toStrict)

import Data.String

-- | A type-class for expression syntaxes that can embed custom expressions.
class (Monoid (CustomSqlSyntax syntax), IsString (CustomSqlSyntax syntax)) =>
  IsCustomSqlSyntax syntax where
  data CustomSqlSyntax syntax :: *

  -- | Given an arbitrary 'ByteString', produce a 'syntax' that represents the
  --   'ByteString' as a SQL expression.
  customExprSyntax :: CustomSqlSyntax syntax -> syntax

  -- | Given an arbitrary 'syntax', produce a 'ByteString' that corresponds to
  --   how that syntax would look when rendered in the backend.
  renderSyntax :: syntax -> CustomSqlSyntax syntax

instance IsCustomSqlSyntax SqlSyntaxBuilder where
  newtype CustomSqlSyntax SqlSyntaxBuilder = SqlSyntaxBuilderCustom ByteString
    deriving (IsString, Monoid)

  customExprSyntax (SqlSyntaxBuilderCustom bs) = SqlSyntaxBuilder (byteString bs)
  renderSyntax = SqlSyntaxBuilderCustom . toStrict . toLazyByteString . buildSql

class IsCustomExprFn fn res | res -> fn where
  customExpr_ :: fn -> res

instance IsCustomSqlSyntax syntax => IsCustomExprFn (CustomSqlSyntax syntax) (QGenExpr ctxt syntax s a) where
  customExpr_ = QExpr . customExprSyntax
instance (IsCustomExprFn a res, IsCustomSqlSyntax syntax) => IsCustomExprFn (CustomSqlSyntax syntax -> a) (QGenExpr ctxt syntax s r -> res) where
  customExpr_ fn (QExpr e) = customExpr_ $ fn (renderSyntax e)

-- | Force a 'QGenExpr' to be typed as a value expression (a 'QExpr'). Useful
--   for getting around type-inference errors with supplying the entire type.
valueExpr_ :: QExpr syntax s a -> QExpr syntax s a
valueExpr_ = id

-- | Force a 'QGenExpr' to be typed as an aggregate. Useful for defining custom
--   aggregates for use in 'aggregate_'.
agg_ :: QAgg syntax s a -> QAgg syntax s a
agg_ = id
