module Database.Beam.Sqlite.Syntax
  ( SqliteSyntax(..) ) where

import Data.DList as DL
import Data.ByteString.Builder

import Database.SQLite.Simple (SQLData)

data SqliteSyntax = SqliteSyntax Builder (DL.DList SQLData)
  deriving (Show, Eq)

instance Monoid SqliteSyntax where
  mempty = SqliteSyntax mempty mempty
  mappend (SqliteSyntax ab av) (SqliteSyntax bb bv) =
    SqliteSyntax (ab <> bb) (av <> bv)

emit :: ByteString -> SqliteSyntax
emit b = SqliteSyntax (byteString b) mempty

emitValue :: SQLValue -> SqliteSyntax
emitValue v = SqliteSyntax mempty (DL.singleton v)

-- * Syntax types

newtype SqliteCommandSyntax = SqliteCommandSyntax { fromSqliteCommand :: SqliteSyntax }
