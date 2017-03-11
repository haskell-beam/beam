{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

module Database.Beam.Postgres.Syntax where

import           Database.Beam.Postgres.Types

import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL92
import           Database.Beam.Backend.Types
import           Database.Beam.Query.Combinators
import           Database.Beam.Query.Internal
import           Database.Beam.Schema.Tables

import           Control.Monad.Free
import           Control.Monad.Free.Church
import           Control.Monad.State

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder, toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Monoid
import           Data.Proxy
import           Data.Ratio
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg

data PgSyntaxF f where
  EmitByteString :: ByteString -> f -> PgSyntaxF f
  EmitBuilder    :: Builder -> f -> PgSyntaxF f

  EscapeString :: ByteString -> f -> PgSyntaxF f
  EscapeBytea  :: ByteString -> f -> PgSyntaxF f
  EscapeIdentifier :: ByteString -> f -> PgSyntaxF f
deriving instance Functor PgSyntaxF

type PgSyntaxM = F PgSyntaxF
newtype PgSyntax
  = PgSyntax { buildPgSyntax :: PgSyntaxM () }
newtype PgSyntax1 a
  = PgSyntax1 { buildPgSyntax1 :: PgSyntax }

instance Monoid PgSyntax where
  mempty = PgSyntax (pure ())
  mappend a b = PgSyntax (buildPgSyntax a >> buildPgSyntax b)

instance SupportedSyntax Postgres PgSyntax

emit :: ByteString -> PgSyntax
emit bs = PgSyntax (liftF (EmitByteString bs ()))

emitBuilder :: Builder -> PgSyntax
emitBuilder b = PgSyntax (liftF (EmitBuilder b ()))

escapeString, escapeBytea, escapeIdentifier :: ByteString -> PgSyntax
escapeString bs = PgSyntax (liftF (EscapeString bs ()))
escapeBytea bin = PgSyntax (liftF (EscapeBytea bin ()))
escapeIdentifier id = PgSyntax (liftF (EscapeIdentifier id ()))

nextSyntaxStep :: PgSyntaxF f -> f
nextSyntaxStep (EmitByteString _ next) = next
nextSyntaxStep (EmitBuilder _ next) = next
nextSyntaxStep (EscapeString _ next) = next
nextSyntaxStep (EscapeBytea _ next) = next
nextSyntaxStep (EscapeIdentifier _ next) = next

instance Sql92Syntax PgSyntax where
  type Sql92SelectSyntax PgSyntax = PgSyntax
  type Sql92UpdateSyntax PgSyntax = PgSyntax
  type Sql92InsertSyntax PgSyntax = PgSyntax
  type Sql92DeleteSyntax PgSyntax = PgSyntax

  type Sql92ExpressionSyntax PgSyntax = PgSyntax
  type Sql92ExpressionSyntax PgSyntax = PgSyntax
  type Sql92ValueSyntax PgSyntax = PgSyntax
  type Sql92ValuesSyntax PgSyntax = PgSyntax

  type Sql92FieldNameSyntax PgSyntax = PgSyntax

  type Sql92ProjectionSyntax PgSyntax = PgSyntax
  type Sql92FromSyntax PgSyntax = PgSyntax
  type Sql92GroupingSyntax PgSyntax = PgSyntax
  type Sql92OrderingSyntax PgSyntax = PgSyntax

  type Sql92TableSourceSyntax PgSyntax = PgSyntax

  type Sql92AliasingSyntax PgSyntax = PgSyntax1

  selectCmd = id

  selectStmt _ proj from where_ grouping ordering limit offset =
    emit "SELECT " <> proj <>
    (maybe mempty (emit " " <> ) from) <>
    emit " WHERE " <> where_ <>
    (maybe mempty (emit " GROUP BY" <>) grouping) <>
    (case ordering of
       [] -> mempty
       ordering -> emit " ORDER BY " <> pgSepBy (emit ", ") ordering) <>
    (maybe mempty (emit . fromString . (" LIMIT " <>) . show) limit) <>
    (maybe mempty (emit . fromString . (" OFFSET " <>) . show) offset)

  qualifiedFieldE _ a b =
    pgQuotedIdentifier a <> emit "." <> pgQuotedIdentifier b
  unqualifiedFieldE _ = pgQuotedIdentifier

  addE _ = pgBinOp "+"
  subE _ = pgBinOp "-"
  mulE _ = pgBinOp "*"
  divE _ = pgBinOp "/"
  modE _ = pgBinOp "%"
  orE _ = pgBinOp "OR"
  andE _ = pgBinOp "AND"
  eqE _ = pgBinOp "="
  neqE _ = pgBinOp "<>"
  ltE _ = pgBinOp "<"
  gtE _ = pgBinOp ">"
  leE _ = pgBinOp "<="
  geE _ = pgBinOp ">="
  negateE _ = pgUnOp "-"
  notE _ = pgUnOp "NOT"
  existsE _ = pgUnOp "EXISTS"
  isJustE _ a =
    emit "(" <> a <> emit ") IS NOT NULL"
  isNothingE _ a =
    emit "(" <> a <> emit ") IS NULL"
  valueE _ = id

  trueV _ = emit "TRUE"
  falseV _ = emit "FALSE"
  stringV _ = pgBuildAction . pure . Pg.toField
  numericV _ = pgBuildAction . pure . Pg.toField
  rationalV p x = divE p (valueE p (numericV p (  numerator x)))
                         (valueE p (numericV p (denominator x)))
  nullV _ = emit "NULL"

  fromTable _ tableSrc Nothing = tableSrc
  fromTable _ tableSrc (Just nm) =
    tableSrc <> emit " AS " <> pgQuotedIdentifier nm

  innerJoin _ = pgJoin "INNER JOIN"
  leftJoin _ = pgJoin "LEFT JOIN"
  rightJoin _ = pgJoin "RIGHT JOIN"

  tableNamed _ nm = pgQuotedIdentifier nm

  projExprs _ exprs =
    pgSepBy (emit ", ") (map buildPgSyntax1 exprs)

  aliasExpr _ expr Nothing = PgSyntax1 expr
  aliasExpr _ expr (Just lbl) = PgSyntax1 (expr <> emit " AS " <> pgQuotedIdentifier lbl)

  values _ vs = emit "VALUES(" <> pgSepBy (emit ", ") vs <> emit ")"

instance Pg.ToField a => HasSqlValueSyntax PgSyntax a where
  sqlValueSyntax _ =
    pgBuildAction . pure . Pg.toField

pgQuotedIdentifier :: T.Text -> PgSyntax
pgQuotedIdentifier t =
  emit "\"" <>
  (emit . TE.encodeUtf8 $
   T.concatMap quoteIdentifierChar t) <>
  emit "\""
  where
    quoteIdentifierChar '"' = "\"\""
    quoteIdentifierChar c = T.singleton c

pgBinOp :: ByteString -> PgSyntax -> PgSyntax -> PgSyntax
pgBinOp op a b =
  emit "(" <> a <> emit (") " <> op <> " (") <> b <> emit ")"

pgUnOp :: ByteString -> PgSyntax -> PgSyntax
pgUnOp op a =
  emit (op <> "(") <> a <> emit ")"

pgJoin :: ByteString -> PgSyntax -> PgSyntax -> Maybe PgSyntax -> PgSyntax
pgJoin joinType a b Nothing =
  a <> emit (" " <> joinType <> " ") <> b
pgJoin joinType a b (Just on) =
  pgJoin joinType a b Nothing <> emit " ON " <> on

pgSepBy :: PgSyntax -> [PgSyntax] -> PgSyntax
pgSepBy _ [] = mempty
pgSepBy _ [x] = x
pgSepBy sep (x:xs) = x <> sep <> pgSepBy sep xs

pgDebugRenderSyntax :: PgSyntax -> IO ()
pgDebugRenderSyntax (PgSyntax p) = go p Nothing
  where go :: PgSyntaxM () -> Maybe (PgSyntaxF ()) -> IO ()
        go p = runF p finish step
        step x lastBs =
          case (x, lastBs) of
            (EmitBuilder s next, lastBs) ->
              step (EmitByteString (toStrict (toLazyByteString s)) next) lastBs
            (x, Nothing) ->
              nextSyntaxStep x (Just (fmap (const ()) x))
            (EmitByteString x next, Just (EmitByteString before _)) ->
              next (Just (EmitByteString (before <> x) ()))
            (EscapeString x next, Just (EscapeString before _)) ->
              next (Just (EscapeString (before <> x) ()))
            (EscapeBytea x next, Just (EscapeBytea before _)) ->
              next (Just (EscapeBytea (before <> x) ()))
            (EscapeIdentifier x next, Just (EscapeIdentifier before _)) ->
              next (Just (EscapeIdentifier (before <> x) ()))
            (s, Just e) ->
              renderStep e >>
              nextSyntaxStep s (Just (fmap (const ()) s))

        renderStep (EmitByteString x _) = putStrLn ("EmitByteString " <> show x)
        renderStep (EscapeString x _) = putStrLn ("EscapeString " <> show x)
        renderStep (EscapeBytea x _) = putStrLn ("EscapeBytea " <> show x)
        renderStep (EscapeIdentifier x _) = putStrLn ("EscapeIdentifier " <> show x)

        finish x Nothing = pure x
        finish x (Just s) = renderStep s >> pure x

pgBuildAction :: [ Pg.Action ] -> PgSyntax
pgBuildAction =
  foldMap $ \action ->
  case action of
    Pg.Plain x -> emitBuilder x
    Pg.Escape str -> emit "'" <> escapeString str <> emit "'"
    Pg.EscapeByteA bin -> emit "'" <> escapeBytea bin <> emit "'"
    Pg.EscapeIdentifier id -> escapeIdentifier id
    Pg.Many as -> pgBuildAction as

-- * Postrges-specific extensions

class Sql92Syntax syntax => PgExtensionsSyntax syntax where
  tableSampleSyntax ::
       Proxy syntax -> T.Text -> Maybe T.Text -> PgTableSamplingMethod
    -> [Sql92ExpressionSyntax syntax] {-^ Arguments to sampling method -}
    -> Maybe (Sql92ExpressionSyntax syntax) {-^ Seed -}
    -> Sql92FromSyntax syntax

instance PgExtensionsSyntax PgSyntax where
  tableSampleSyntax _ tblName tblAlias (PgTableSamplingMethod sampleMethod) sampleMethodParams seedExpr =
    pgQuotedIdentifier tblName <>
    maybe mempty (\x -> emit " AS " <> pgQuotedIdentifier x) tblAlias <>
    emit " TABLESAMPLE " <> emit (TE.encodeUtf8 sampleMethod) <> emit "(" <> pgSepBy (emit ", ") sampleMethodParams <> emit ")" <>
    maybe mempty (\x -> emit " REPEATABLE (" <> x <> emit ")") seedExpr

-- * Pg-specific Q monad

bernoulliSample ::
  forall syntax db table s.
  ( Database db, SupportedSyntax Postgres syntax, PgExtensionsSyntax syntax) =>
  DatabaseTable Postgres db table -> QExpr syntax s Double ->
  Q syntax db s (table (QExpr syntax s))
bernoulliSample
  tbl@(DatabaseTable table name tableSettings :: DatabaseTable Postgres db table)
  (QExpr prob) =

  do curTbl <- gets qbNextTblRef
     let newSource = tableSampleSyntax (Proxy @syntax) name
                                       (Just (fromString ("t" <> show curTbl)))
                                       pgBernoulliSamplingMethod
                                       [ prob ]
                                       Nothing
     buildJoinFrom tbl newSource Nothing
