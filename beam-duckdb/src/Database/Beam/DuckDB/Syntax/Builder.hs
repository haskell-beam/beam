{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.DuckDB.Syntax.Builder
  ( DuckDBSyntax (..),
    SomeField (..),
    emitChar,
    emit,
    emitIntegral,
    emitRealFloat,
    emit',
    emitValue,
    spaces,
    parens,
    sepBy,
    commas,
    quotedIdentifier,
    withPlaceholder,
  )
where

import Data.DList (DList)
import qualified Data.DList as DL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.Builder.RealFloat as Builder
import Database.Beam.Backend (Sql92DisplaySyntax (..))
import Database.DuckDB.Simple (ToField (toField))
import Database.DuckDB.Simple.ToField (renderFieldBinding)

data SomeField = forall a. (ToField a, Eq a) => SomeField a

instance Show SomeField where
  show (SomeField f) = renderFieldBinding (toField f)

instance ToField SomeField where
  toField (SomeField f) = toField f

data DuckDBSyntax
  = DuckDBSyntax
      ((SomeField -> Builder) -> Builder)
      (DList SomeField)

emitChar :: Char -> DuckDBSyntax
emitChar c = DuckDBSyntax (const (Builder.singleton c)) mempty

emit :: Text -> DuckDBSyntax
emit t = DuckDBSyntax (const (Builder.fromText t)) mempty

emitIntegral :: (Integral a) => a -> DuckDBSyntax
emitIntegral i = DuckDBSyntax (const (Builder.decimal i)) mempty

emitRealFloat :: (RealFloat f) => f -> DuckDBSyntax
emitRealFloat f = DuckDBSyntax (const (Builder.realFloat f)) mempty

emit' :: (Show a) => a -> DuckDBSyntax
emit' s = DuckDBSyntax (const (Builder.fromString (show s))) mempty

-- | Emit a properly escaped value into the syntax
emitValue :: (ToField a, Eq a) => a -> DuckDBSyntax
emitValue v =
  DuckDBSyntax
    ($ SomeField v)
    (DL.singleton (SomeField v))

spaces :: DuckDBSyntax -> DuckDBSyntax
spaces a = emit " " <> a <> emit " "

parens :: DuckDBSyntax -> DuckDBSyntax
parens a = emit "(" <> a <> emit ")"

sepBy :: DuckDBSyntax -> [DuckDBSyntax] -> DuckDBSyntax
sepBy _ [] = mempty
sepBy _ [x] = x
sepBy sep (x : xs) = x <> foldMap (sep <>) xs

commas :: [DuckDBSyntax] -> DuckDBSyntax
commas = sepBy (emit ", ")

quotedIdentifier :: Text -> DuckDBSyntax
quotedIdentifier txt = emit "\"" <> DuckDBSyntax (\_ -> Builder.fromText (duckDBEscape txt)) mempty <> emit "\""

instance Show DuckDBSyntax where
  show (DuckDBSyntax s vs) =
    mconcat
      [ "DuckDBSyntax (",
        show (Builder.toLazyText (withPlaceholder s)),
        ")",
        show vs
      ]

instance Eq DuckDBSyntax where
  DuckDBSyntax sx vx == DuckDBSyntax sy vy =
    -- TODO: is there a cleaner way to do this? Is it even important to have this instance?
    show vx == show vy
      && withPlaceholder sx == withPlaceholder sy

instance Semigroup DuckDBSyntax where
  DuckDBSyntax sx vx <> DuckDBSyntax sy vy =
    DuckDBSyntax (\v -> sx v <> sy v) (vx <> vy)

instance Monoid DuckDBSyntax where
  mempty = DuckDBSyntax (const mempty) mempty

instance Sql92DisplaySyntax DuckDBSyntax where
  displaySyntax = Text.unpack . duckDBRenderSyntaxScript

withPlaceholder :: ((SomeField -> Builder) -> Builder) -> Builder
withPlaceholder build = build (const (Builder.singleton '?'))

-- | A best effort attempt to implement the escaping rules of DuckDB. This is
-- never used to escape data sent to the database; only for emitting scripts or
-- displaying syntax to the user.
duckDBEscape :: Text -> Text
duckDBEscape = Text.concatMap (\c -> if c == '"' then "\"\"" else Text.singleton c)

duckDBRenderSyntaxScript :: DuckDBSyntax -> Text
duckDBRenderSyntaxScript (DuckDBSyntax s _) =
  Text.Lazy.toStrict . Builder.toLazyText . s $ render
  where
    render :: SomeField -> Builder
    render (SomeField f) = Builder.fromString (renderFieldBinding (toField f))
