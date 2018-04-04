{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Allows the creation of custom SQL expressions from arbitrary string-like values.
--
--   Simply write a polymorphic function with an arbitrary number of arguments,
--   all of the same type, and returns a value of the same type. The type will
--   have instances of 'Monoid' and 'IsString'.
--
--   For example, to implement a function @MYFUNC@ that takes three arguments
--
-- @
-- myFuncImpl :: (Monoid a, IsString a) => a -> a -> a -> a
-- @
--
--   Then, apply 'customExpr_' to your function.  This will result in a function
--   with the same arity, that takes in and returns 'QGenExpr's instead of
--   generic @a@s.
--
--   The returned function is polymorphic in the types of SQL expressions it
--   will accept, but you can give it a more specific signature. For example, to
--   mandate that we receive two 'Int's and a 'T.Text' and return a 'Bool'.
--
-- @
-- myFunc_ :: QGenExpr e ctxt s Int -> QGenExpr e ctxt s Int -> QGenExpr e ctxt s T.Text -> QGenExpr e ctxt s Bool
-- myFunc_ = customExpr_ myFuncImpl
-- @
--
--   Semantically, the expression builder function (@myFuncImpl@ in this case)
--   is called with arguments representing SQL expressions, that, when
--   evaluated, will evaluate to the result of the expressions supplied as
--   arguments to 'customExpr_'. See the section on
--   <http://tathougies.github.io/beam/user-guide/queries/custom-queries/
--   extensibility> in the user guide for example usage.
module Database.Beam.Query.CustomSQL
  (
  -- * The 'customExpr_' function
  IsCustomExprFn(..)

  -- ** Type-inference help
  , valueExpr_, agg_
  ) where

import           Database.Beam.Backend
import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL.Builder

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (byteString, toLazyByteString)
import           Data.ByteString.Lazy (toStrict)

import           Data.Monoid
import           Data.String
import qualified Data.Text as T

newtype CustomSqlSnippet = CustomSqlSnippet (T.Text -> CustomSqlSyntax)
instance Semigroup CustomSqlSnippet where
  (<>) (CustomSqlSnippet a) (CustomSqlSnippet b) =
    CustomSqlSnippet $ \pfx -> a pfx <> b pfx
  
instance Monoid CustomSqlSnippet where
  mempty = CustomSqlSnippet (pure mempty)
instance IsString CustomSqlSnippet where
  fromString s = CustomSqlSnippet $ \_ -> fromString s

class IsCustomExprFn fn res | res -> fn where
  customExpr_ :: fn -> res

instance IsCustomExprFn CustomSqlSnippet (QGenExpr ctxt s res) where
  customExpr_ (CustomSqlSnippet mkSyntax) = QExpr (customExprSyntax . mkSyntax)
instance IsCustomExprFn a res => IsCustomExprFn (CustomSqlSnippet -> a) (QGenExpr ctxt s r -> res) where
  customExpr_ fn (QExpr e) = customExpr_ $ fn (CustomSqlSnippet (renderSyntax . e))

-- | Force a 'QGenExpr' to be typed as a value expression (a 'QExpr'). Useful
--   for getting around type-inference errors with supplying the entire type.
valueExpr_ :: QExpr s a -> QExpr s a
valueExpr_ = id

-- | Force a 'QGenExpr' to be typed as an aggregate. Useful for defining custom
--   aggregates for use in 'aggregate_'.
agg_ :: QAgg s a -> QAgg s a
agg_ = id

