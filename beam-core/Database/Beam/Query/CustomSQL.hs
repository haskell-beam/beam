{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Query.CustomSQL where

import Database.Beam.Query.Internal
import Database.Beam.Backend.SQL.Builder

import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy (toStrict)

class IsCustomSqlSyntax syntax where
  customExprSyntax :: ByteString -> syntax
  renderSyntax :: syntax -> ByteString
instance IsCustomSqlSyntax SqlSyntaxBuilder where
  customExprSyntax s = SqlSyntaxBuilder (byteString s)
  renderSyntax = toStrict . toLazyByteString . buildSql

class IsCustomExprFn fn res | res -> fn where
  customExpr_ :: fn -> res

instance IsCustomSqlSyntax syntax => IsCustomExprFn ByteString (QGenExpr ctxt syntax s a) where
  customExpr_ = QExpr . customExprSyntax
instance (IsCustomExprFn a res, IsCustomSqlSyntax syntax) => IsCustomExprFn (ByteString -> a) (QGenExpr ctxt syntax s r -> res) where
  customExpr_ fn (QExpr e) = customExpr_ $ fn (renderSyntax e)

agg_ :: QAgg syntax s a -> QAgg syntax s a
agg_ = id
