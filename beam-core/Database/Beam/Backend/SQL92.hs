module Database.Beam.Backend.SQL92 where

import Database.Beam.Schema.Tables
import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T

import Data.ByteString.Builder
import Data.Monoid
import Data.Proxy

class ( BeamSqlBackend be
      , Sql92Syntax (Sql92BackendSyntaxBuilder be)
      , Sql92Schema (BackendColumnSchema be)) =>
      BeamSql92Backend be where

  type Sql92BackendSyntaxBuilder be :: *
  type Sql92BackendSyntaxBuilder be = Sql92SyntaxBuilder

-- * Schemas

class Sql92Schema schema where
  int :: schema
  smallint :: schema
  tinyint :: schema
  bigint :: schema

  char :: Word -> schema
  varchar :: Maybe Word -> schema

  float :: schema
  double :: schema

-- * Finally tagless style

newtype Sql92Command
  = Sql92Command (forall cmd. Sql92Syntax cmd => cmd)
newtype Sql92SyntaxBuilder
  = Sql92SyntaxBuilder { buildSql92 :: Builder }
newtype Sql92SyntaxBuilder1 x
  = Sql92SyntaxBuilder1 { buildSql92_1 :: Builder }

instance Monoid Sql92SyntaxBuilder where
  mempty = Sql92SyntaxBuilder mempty
  mappend (Sql92SyntaxBuilder a) (Sql92SyntaxBuilder b) =
    Sql92SyntaxBuilder (mappend a b)

class HasSqlValueSyntax cmd ty where
  sqlValueSyntax :: Proxy cmd -> ty -> Sql92ValueSyntax cmd

class Sql92Syntax cmd where
  type Sql92SelectSyntax cmd :: *
  type Sql92UpdateSyntax cmd :: *
  type Sql92InsertSyntax cmd :: *
  type Sql92DeleteSyntax cmd :: *

  type Sql92ExpressionSyntax cmd :: *
  type Sql92ValueSyntax cmd :: *
  type Sql92ValuesSyntax cmd :: *
  type Sql92FieldNameSyntax cmd :: *

  selectCmd :: Sql92SelectSyntax cmd -> cmd

  values :: Proxy cmd -> [ Sql92ValueSyntax cmd ] -> Sql92ValuesSyntax cmd

  type Sql92ProjectionSyntax cmd :: *
  type Sql92FromSyntax cmd :: *
  type Sql92GroupingSyntax cmd :: *
  type Sql92OrderingSyntax cmd :: *

  type Sql92TableSourceSyntax cmd :: *

  type Sql92AliasingSyntax cmd :: * -> *

  selectStmt :: Proxy cmd
             -> Sql92ProjectionSyntax cmd
             -> Maybe (Sql92FromSyntax cmd)
             -> Sql92ExpressionSyntax cmd   {-^ Where clause -}
             -> Maybe (Sql92GroupingSyntax cmd)
             -> [Sql92OrderingSyntax cmd]
             -> Maybe Integer {-^ LIMIT -}
             -> Maybe Integer {-^ OFFSET -}
             -> Sql92SelectSyntax cmd

  insertStmt :: Proxy cmd
             -> Text
             -> Either (Sql92SelectSyntax cmd) (Sql92ValuesSyntax cmd)
             -> Sql92InsertSyntax cmd

  updateStmt :: Proxy cmd
             -> Text
             -> [(Sql92FieldNameSyntax cmd, Sql92ExpressionSyntax cmd)]
             -> Sql92ExpressionSyntax cmd {-^ WHERE -}
             -> Sql92UpdateSyntax cmd

  deleteStmt :: Proxy cmd
             -> Text
             -> Sql92ExpressionSyntax cmd
             -> Sql92DeleteSyntax cmd

  valueE :: Proxy cmd -> Sql92ValueSyntax cmd -> Sql92ExpressionSyntax cmd
  isJustE :: Proxy cmd -> Sql92ExpressionSyntax cmd -> Sql92ExpressionSyntax cmd
  isNothingE :: Proxy cmd -> Sql92ExpressionSyntax cmd -> Sql92ExpressionSyntax cmd
  caseE :: Proxy cmd -> [(Sql92ExpressionSyntax cmd, Sql92ExpressionSyntax cmd)]
    -> Sql92ExpressionSyntax cmd -> Sql92ExpressionSyntax cmd
  qualifiedFieldE :: Proxy cmd -> Text -> Text
                  -> Sql92ExpressionSyntax cmd
  unqualifiedFieldE :: Proxy cmd -> Text
                    -> Sql92ExpressionSyntax cmd

  andE, orE, eqE, neqE, ltE, gtE, leE, geE,
    addE, subE, mulE, divE, modE
    :: Proxy cmd
    -> Sql92ExpressionSyntax cmd
    -> Sql92ExpressionSyntax cmd
    -> Sql92ExpressionSyntax cmd

  notE, negateE, absE
    :: Proxy cmd
    -> Sql92ExpressionSyntax cmd
    -> Sql92ExpressionSyntax cmd

  existsE :: Proxy cmd
          -> Sql92SelectSyntax cmd
          -> Sql92ExpressionSyntax cmd

  nullV :: Proxy cmd -> Sql92ValueSyntax cmd
  trueV :: Proxy cmd -> Sql92ValueSyntax cmd
  falseV :: Proxy cmd -> Sql92ValueSyntax cmd
  stringV :: Proxy cmd -> String -> Sql92ValueSyntax cmd
  numericV :: Proxy cmd -> Integer -> Sql92ValueSyntax cmd

  aliasExpr :: Proxy cmd -> Sql92ExpressionSyntax cmd
            -> Maybe Text
            -> Sql92AliasingSyntax cmd (Sql92ExpressionSyntax cmd)

  projExprs :: Proxy cmd -> [Sql92AliasingSyntax cmd (Sql92ExpressionSyntax cmd)]
            -> Sql92ProjectionSyntax cmd

  ascOrdering, descOrdering
    :: Proxy cmd -> Sql92ExpressionSyntax cmd -> Sql92OrderingSyntax cmd

  tableNamed :: Proxy cmd -> Text -> Sql92TableSourceSyntax cmd

  fromTable :: Proxy cmd
    -> Sql92TableSourceSyntax cmd
    -> Maybe Text
    -> Sql92FromSyntax cmd

  innerJoin, leftJoin, rightJoin :: Proxy cmd
    -> Sql92FromSyntax cmd
    -> Sql92FromSyntax cmd
    -> Maybe (Sql92ExpressionSyntax cmd)
    -> Sql92FromSyntax cmd

instance HasSqlValueSyntax Sql92SyntaxBuilder Int32 where
  sqlValueSyntax p = numericV p . fromIntegral

-- | Build a query using ANSI SQL92 syntax. This is likely to work out-of-the-box
--   in many databases, but its use is a security risk, as different databases have
--   different means of escaping values. It is best to customize this class per-backend
instance Sql92Syntax Sql92SyntaxBuilder where
  type Sql92SelectSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92UpdateSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92InsertSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92DeleteSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  type Sql92ExpressionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92ExpressionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92ValueSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92ValuesSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  type Sql92FieldNameSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  type Sql92ProjectionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92FromSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92GroupingSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92OrderingSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  type Sql92TableSourceSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  type Sql92AliasingSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder1

  selectCmd = id

  selectStmt _ proj from where_ grouping ordering limit offset =
    Sql92SyntaxBuilder $
    byteString "SELECT " <> buildSql92 proj <>
    (maybe mempty ((byteString " " <>) . buildSql92) from) <>
    byteString " WHERE " <> buildSql92 where_

  qualifiedFieldE _ a b =
    Sql92SyntaxBuilder $
    byteString "`" <> stringUtf8 (T.unpack a) <> byteString "`.`" <>
    stringUtf8 (T.unpack b) <> byteString "`"
  andE _ = sqlBinOp "AND"
  orE _ = sqlBinOp "OR"
  eqE _ = sqlBinOp "="
  neqE _ = sqlBinOp "<>"
  ltE _ = sqlBinOp "<"
  gtE _ = sqlBinOp ">"
  leE _ = sqlBinOp "<="
  geE _ = sqlBinOp ">="
  valueE _ a = a

  trueV _ = Sql92SyntaxBuilder (byteString "TRUE")
  falseV _ = Sql92SyntaxBuilder (byteString "FALSE")
  stringV _ x = Sql92SyntaxBuilder (byteString "\'" <>
                                    stringUtf8 (foldMap escapeChar x) <>
                                    byteString "\'")
    where escapeChar '\'' = "''"
          escapeChar x = [x]
  numericV _ x = Sql92SyntaxBuilder (stringUtf8 (show x))

  fromTable _ tableSrc Nothing = tableSrc
  fromTable _ tableSrc (Just nm) =
    Sql92SyntaxBuilder $
    buildSql92 tableSrc <> byteString " AS " <> stringUtf8 (T.unpack nm)
  innerJoin _ = join "INNER JOIN"

  tableNamed _ nm = Sql92SyntaxBuilder (stringUtf8 (T.unpack nm))

  projExprs _ exprs =
    Sql92SyntaxBuilder $ buildSepBy (byteString ", ") (map buildSql92_1 exprs)
  aliasExpr _ expr Nothing = Sql92SyntaxBuilder1 (buildSql92 expr)
  aliasExpr _ expr (Just lbl) = Sql92SyntaxBuilder1 (buildSql92 expr <> byteString " AS " <> stringUtf8 (T.unpack lbl))

  values _ vs = Sql92SyntaxBuilder $
                byteString "VALUES(" <>
                buildSepBy (byteString ", ") (map buildSql92 vs) <>
                byteString ")"

join type_ a b on =
    Sql92SyntaxBuilder $
    buildSql92 a <> byteString " " <>  byteString type_ <> byteString " " <> buildSql92 b <>
    case on of
      Nothing -> mempty
      Just on -> byteString " ON (" <> buildSql92 on <> byteString ")"
sqlBinOp op a b =
  Sql92SyntaxBuilder (byteString "(" <> buildSql92 a <> byteString ") " <> byteString op <> byteString " (" <>
                       buildSql92 b <> byteString ")")

renderSql92 :: Sql92SyntaxBuilder -> String
renderSql92 (Sql92SyntaxBuilder b) = show (toLazyByteString b)

buildSepBy :: Builder -> [Builder] -> Builder
buildSepBy sep [] = mempty
buildSepBy sep [x] = x
buildSepBy sep (x:xs) = x <> sep <> buildSepBy sep xs

