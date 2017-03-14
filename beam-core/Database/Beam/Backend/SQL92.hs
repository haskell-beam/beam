{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

module Database.Beam.Backend.SQL92 where

import           Control.Monad.Identity hiding (join)

import           Database.Beam.Schema.Tables
import           Database.Beam.Backend.Types
import           Database.Beam.Backend.SQL
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Coerce
import           Data.Monoid
import           Data.Proxy
import           Data.Ratio
import           Data.String
import qualified Data.Text.Encoding as TE

import           GHC.Generics

class ( BeamSqlBackend be
      , Sql92Schema (BackendColumnSchema be)) =>
      BeamSql92Backend be where

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

  timestamp :: schema

-- * Finally tagless style

newtype Sql92Command
  = Sql92Command (forall cmd. IsSql92Syntax cmd => cmd)
newtype Sql92SyntaxBuilder
  = Sql92SyntaxBuilder { buildSql92 :: Builder }
newtype Sql92SyntaxBuilder1 (x :: k)
  = Sql92SyntaxBuilder1 { buildSql92_1 :: Builder }

instance Monoid Sql92SyntaxBuilder where
  mempty = Sql92SyntaxBuilder mempty
  mappend (Sql92SyntaxBuilder a) (Sql92SyntaxBuilder b) =
    Sql92SyntaxBuilder (mappend a b)

class HasSqlValueSyntax expr ty where
  sqlValueSyntax :: ty -> expr

class IsSql92Syntax cmd where
  type Sql92SelectSyntax cmd :: *
  type Sql92InsertSyntax cmd :: *
  type Sql92UpdateSyntax cmd :: *
  type Sql92DeleteSyntax cmd :: *

  selectCmd :: Sql92SelectSyntax cmd -> cmd
  insertCmd :: Sql92InsertSyntax cmd -> cmd
  updateCmd :: Sql92UpdateSyntax cmd -> cmd
  deleteCmd :: Sql92DeleteSyntax cmd -> cmd

class ( IsSql92SelectTableSyntax (Sql92SelectSelectTableSyntax select)
      , IsSql92OrderingSyntax (Sql92SelectOrderingSyntax select) ) =>
    IsSql92SelectSyntax select where
    type Sql92SelectSelectTableSyntax select :: *
    type Sql92SelectOrderingSyntax select :: *

    selectStmt :: Sql92SelectSelectTableSyntax select
               -> [Sql92SelectOrderingSyntax select]
               -> Maybe Integer {-^ LIMIT -}
               -> Maybe Integer {-^ OFFSET -}
               -> select

class ( IsSql92ExpressionSyntax (Sql92SelectTableExpressionSyntax select)
      , IsSql92ProjectionSyntax (Sql92SelectTableProjectionSyntax select)
      , IsSql92FromSyntax (Sql92SelectTableFromSyntax select)

      , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax select) ~ Sql92SelectTableExpressionSyntax select ) =>
    IsSql92SelectTableSyntax select where
  type Sql92SelectTableExpressionSyntax select :: *
  type Sql92SelectTableProjectionSyntax select :: *
  type Sql92SelectTableFromSyntax select :: *
  type Sql92SelectTableGroupingSyntax select :: *

  selectTableStmt :: Sql92SelectTableProjectionSyntax select
                  -> Maybe (Sql92SelectTableFromSyntax select)
                  -> Maybe (Sql92SelectTableExpressionSyntax select)   {-^ Where clause -}
                  -> Maybe (Sql92SelectTableGroupingSyntax select)
                  -> Maybe (Sql92SelectTableExpressionSyntax select) {-^ having clause -}
                  -> select

class IsSql92InsertSyntax insert where
  type Sql92InsertValuesSyntax insert :: *
  insertStmt :: Text
             -> [ Text ]
             -> Sql92InsertValuesSyntax insert
             -> insert

class IsSql92InsertValuesSyntax insertValues where
  type Sql92InsertValuesExpressionSyntax insertValues :: *
  type Sql92InsertValuesSelectSyntax insertValues :: *

  insertSqlExpressions :: [ [ Sql92InsertValuesExpressionSyntax insertValues ] ]
                       -> insertValues
  insertFromSql :: Sql92InsertValuesSelectSyntax insertValues
                -> insertValues

class IsSql92UpdateSyntax update where
  type Sql92UpdateFieldNameSyntax update :: *
  type Sql92UpdateExpressionSyntax update :: *

  updateStmt :: Text
             -> [(Sql92UpdateFieldNameSyntax update, Sql92UpdateExpressionSyntax update)]
             -> Sql92UpdateExpressionSyntax update {-^ WHERE -}
             -> update

class IsSql92DeleteSyntax delete where
  type Sql92DeleteExpressionSyntax delete :: *

  deleteStmt :: Text
             -> Sql92DeleteExpressionSyntax delete
             -> delete

class IsSql92FieldNameSyntax fn where
  qualifiedField :: Text -> Text -> fn
  unqualifiedField :: Text -> fn

class ( IsSql92FieldNameSyntax (Sql92ExpressionFieldNameSyntax expr) ) =>
    IsSql92ExpressionSyntax expr where
  type Sql92ExpressionValueSyntax expr :: *
  type Sql92ExpressionSelectSyntax expr :: *
  type Sql92ExpressionFieldNameSyntax expr :: *

  valueE :: Sql92ExpressionValueSyntax expr -> expr
  valuesE :: [ expr ] -> expr
  isJustE :: expr -> expr
  isNothingE :: expr -> expr
  caseE :: [(expr, expr)]
    -> expr -> expr
  fieldE :: Sql92ExpressionFieldNameSyntax expr -> expr

  andE, orE, eqE, neqE, ltE, gtE, leE, geE,
    addE, subE, mulE, divE, modE
    :: expr
    -> expr
    -> expr

  notE, negateE, absE
    :: expr
    -> expr

  existsE :: Sql92ExpressionSelectSyntax expr
          -> expr

class IsSql92ProjectionSyntax proj where
  type Sql92ProjectionExpressionSyntax proj :: *

  projExprs :: [ (Sql92ProjectionExpressionSyntax proj, Maybe Text) ]
            -> proj

class IsSql92OrderingSyntax ord where
  type Sql92OrderingExpressionSyntax ord :: *
  ascOrdering, descOrdering
    :: Sql92OrderingExpressionSyntax ord -> ord

class IsSql92TableSourceSyntax tblSource where
  tableNamed :: Text -> tblSource

class ( IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax from)
      , IsSql92ExpressionSyntax (Sql92FromExpressionSyntax from) ) =>
    IsSql92FromSyntax from where
  type Sql92FromTableSourceSyntax from :: *
  type Sql92FromExpressionSyntax from :: *

  fromTable :: Sql92FromTableSourceSyntax from
            -> Maybe Text
            -> from

  innerJoin, leftJoin, rightJoin
    :: from -> from
      -> Maybe (Sql92FromExpressionSyntax from)
      -> from

-- class Sql92Syntax cmd where

--   -- data Sql92ValueSyntax cmd :: *
--   -- data Sql92FieldNameSyntax cmd :: *

--   selectCmd :: Sql92SelectSyntax cmd -> cmd


--   -- data Sql92TableSourceSyntax cmd :: *

--   -- data Sql92InsertValuesSyntax cmd ::  *

--   -- data Sql92AliasingSyntax cmd :: * -> *

--   insertSqlExpressions :: [ [ Sql92ExpressionSyntax cmd ] ]
--                        -> Sql92InsertValuesSyntax cmd
--   insertFromSql :: Sql92SelectSyntax cmd
--                 -> Sql92InsertValuesSyntax cmd

--   -- nullV     :: Sql92ValueSyntax cmd
--   -- trueV     :: Sql92ValueSyntax cmd
--   -- falseV    :: Sql92ValueSyntax cmd
--   -- stringV   :: String -> Sql92ValueSyntax cmd
--   -- numericV  :: Integer -> Sql92ValueSyntax cmd
--   -- rationalV :: Rational -> Sql92ValueSyntax cmd

--   -- aliasExpr :: Sql92ExpressionSyntax cmd
--   --           -> Maybe Text
--   --           -> Sql92AliasingSyntax cmd (Sql92ExpressionSyntax cmd)

--   -- projExprs :: [Sql92AliasingSyntax cmd (Sql92ExpressionSyntax cmd)]
--   --           -> Sql92ProjectionSyntax cmd

--   -- ascOrdering, descOrdering
--   --   :: Sql92ExpressionSyntax cmd -> Sql92OrderingSyntax cmd

--   tableNamed :: Text -> Sql92TableSourceSyntax cmd

--   -- fromTable
--   --   :: Sql92TableSourceSyntax cmd
--   --   -> Maybe Text
--   --   -> Sql92FromSyntax cmd

--   innerJoin, leftJoin, rightJoin
--     :: Sql92FromSyntax cmd
--     -> Sql92FromSyntax cmd
--     -> Maybe (Sql92ExpressionSyntax cmd)
--     -> Sql92FromSyntax cmd

-- instance HasSqlValueSyntax Sql92SyntaxBuilder Int32 where
--   sqlValueSyntax = numericV . fromIntegral

-- -- | Build a query using ANSI SQL92 syntax. This is likely to work out-of-the-box
-- --   in many databases, but its use is a security risk, as different databases have
-- --   different means of escaping values. It is best to customize this class per-backend
-- instance Sql92Syntax Sql92SyntaxBuilder where

--   trueV = Sql92ValueSyntaxBuilder (Sql92SyntaxBuilder (byteString "TRUE"))
--   falseV = Sql92ValueSyntaxBuilder (Sql92SyntaxBuilder (byteString "FALSE"))
--   stringV x = Sql92ValueSyntaxBuilder . Sql92SyntaxBuilder $
--                 byteString "\'" <>
--                 stringUtf8 (foldMap escapeChar x) <>
--                 byteString "\'"
--     where escapeChar '\'' = "''"
--           escapeChar x = [x]
--   numericV x = Sql92ValueSyntaxBuilder (Sql92SyntaxBuilder (stringUtf8 (show x)))
--   rationalV x = let Sql92ExpressionSyntaxBuilder e =
--                         divE (valueE (numericV (numerator x)))
--                              (valueE (numericV (denominator x)))
--                 in Sql92ValueSyntaxBuilder e
--   nullV = Sql92ValueSyntaxBuilder (Sql92SyntaxBuilder (byteString "NULL"))

--   fromTable tableSrc Nothing = coerce tableSrc
--   fromTable tableSrc (Just nm) =
--     Sql92FromSyntaxBuilder . Sql92SyntaxBuilder $
--     buildSql92 (coerce tableSrc) <> byteString " AS " <> stringUtf8 (T.unpack nm)

--   innerJoin = join "INNER JOIN"
--   leftJoin = join "LEFT JOIN"
--   rightJoin = join "RIGHT JOIN"

--   tableNamed nm = Sql92TableSourceSyntaxBuilder (Sql92SyntaxBuilder (stringUtf8 (T.unpack nm)))

instance IsSql92SelectSyntax Sql92SyntaxBuilder where
  type Sql92SelectSelectTableSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92SelectOrderingSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  selectStmt tableSrc ordering limit offset =
      Sql92SyntaxBuilder $
      buildSql92 tableSrc <> byteString " ORDER BY " <>
      buildSepBy (byteString ", ") (map buildSql92 ordering) <>
      maybe mempty (\l -> byteString " LIMIT " <> byteString (fromString (show l))) limit <>
      maybe mempty (\o -> byteString " OFFSET " <> byteString (fromString (show o))) offset

instance IsSql92SelectTableSyntax Sql92SyntaxBuilder where
  type Sql92SelectTableExpressionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92SelectTableProjectionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92SelectTableFromSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92SelectTableGroupingSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  selectTableStmt proj from where_ grouping having =
    Sql92SyntaxBuilder $
    byteString "SELECT " <> buildSql92 proj <>
    (maybe mempty ((byteString " " <>) . buildSql92) from) <>
    (maybe mempty (\w -> byteString " WHERE " <> buildSql92 w) where_) <>
    (maybe mempty (\g -> byteString " GROUP BY " <> buildSql92 g) grouping) <>
    (maybe mempty (\e -> byteString " HAVING " <> buildSql92 e) having)

instance IsSql92InsertSyntax Sql92SyntaxBuilder where
  type Sql92InsertValuesSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  insertStmt table fields values =
    Sql92SyntaxBuilder $
    byteString "INSERT INTO " <> quoteSql table <>
    byteString "(" <> buildSepBy (byteString ", ") (map quoteSql fields) <> byteString ") " <>
    buildSql92 values

instance IsSql92UpdateSyntax Sql92SyntaxBuilder where
  type Sql92UpdateFieldNameSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92UpdateExpressionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  updateStmt table set where_ =
    Sql92SyntaxBuilder undefined

instance IsSql92DeleteSyntax Sql92SyntaxBuilder where
  type Sql92DeleteExpressionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  deleteStmt tbl where_ =
    Sql92SyntaxBuilder $
    byteString "DELETE FROM " <> quoteSql tbl <>
    byteString " WHERE " <> buildSql92 where_

instance IsSql92FieldNameSyntax Sql92SyntaxBuilder where
  qualifiedField a b =
    Sql92SyntaxBuilder $
    byteString "`" <> stringUtf8 (T.unpack a) <> byteString "`.`" <>
    stringUtf8 (T.unpack b) <> byteString "`"
  unqualifiedField a =
    Sql92SyntaxBuilder $
    byteString "`" <> stringUtf8 (T.unpack a) <> byteString "`"

instance IsSql92ExpressionSyntax Sql92SyntaxBuilder where
  type Sql92ExpressionValueSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92ExpressionSelectSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
  type Sql92ExpressionFieldNameSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  valuesE vs = Sql92SyntaxBuilder $
               byteString "VALUES(" <>
               buildSepBy (byteString ", ") (map buildSql92 (coerce vs)) <>
               byteString ")"
  isJustE a =
    Sql92SyntaxBuilder $
    byteString "(" <> buildSql92 (coerce a) <> byteString ") IS NOT NULL"
  isNothingE a =
    Sql92SyntaxBuilder $
    byteString "(" <> buildSql92 (coerce a) <> byteString ") IS NULL"
  caseE cases else_ =
    Sql92SyntaxBuilder $
    byteString "CASE " <>
    foldMap (\(cond, res) -> byteString "WHEN " <> buildSql92 cond <>
                             byteString " THEN " <> buildSql92 res <> byteString " ") cases <>
    byteString "ELSE " <> buildSql92 else_ <> byteString " END"
  fieldE = id

  addE = sqlBinOp "+"
  subE = sqlBinOp "-"
  mulE = sqlBinOp "*"
  divE = sqlBinOp "/"
  modE = sqlBinOp "%"
  andE = sqlBinOp "AND"
  orE  = sqlBinOp "OR"
  eqE  = sqlBinOp "="
  neqE = sqlBinOp "<>"
  ltE  = sqlBinOp "<"
  gtE  = sqlBinOp ">"
  leE  = sqlBinOp "<="
  geE  = sqlBinOp ">="
  negateE = sqlUnOp "-"
  notE = sqlUnOp "NOT"
  existsE = sqlUnOp "EXISTS"
  valueE = id

instance IsSql92ProjectionSyntax Sql92SyntaxBuilder where
  type Sql92ProjectionExpressionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  projExprs exprs =
      Sql92SyntaxBuilder $
      buildSepBy (byteString ", ")
                 (map (\(expr, nm) -> buildSql92 expr <>
                                      maybe mempty (\nm -> byteString " AS " <> quoteSql nm) nm) exprs)

instance IsSql92OrderingSyntax Sql92SyntaxBuilder where
  type Sql92OrderingExpressionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

  ascOrdering expr = Sql92SyntaxBuilder (buildSql92 expr <> byteString " ASC")
  descOrdering expr = Sql92SyntaxBuilder (buildSql92 expr <> byteString " DESC")

instance IsSql92TableSourceSyntax Sql92SyntaxBuilder where
    tableNamed t = Sql92SyntaxBuilder (quoteSql t)

instance IsSql92FromSyntax Sql92SyntaxBuilder where
    type Sql92FromTableSourceSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder
    type Sql92FromExpressionSyntax Sql92SyntaxBuilder = Sql92SyntaxBuilder

    fromTable t Nothing = t
    fromTable t (Just nm) = Sql92SyntaxBuilder (buildSql92 t <> byteString " AS " <> quoteSql nm)

    innerJoin = join "INNER JOIN"
    leftJoin = join "LEFT JOIN"
    rightJoin = join "RIGHT JOIN"

-- TODO actual quoting
quoteSql table =
    byteString "\"" <> byteString (TE.encodeUtf8 table) <> byteString "\""

join type_ a b on =
    Sql92SyntaxBuilder $
    buildSql92 a <> byteString " " <>  byteString type_ <> byteString " " <> buildSql92 b <>
    case on of
      Nothing -> mempty
      Just on -> byteString " ON (" <> buildSql92 on <> byteString ")"
sqlUnOp op a =
  Sql92SyntaxBuilder $
  byteString op <> byteString " (" <> buildSql92 a <> byteString ")"
sqlBinOp op a b =
    Sql92SyntaxBuilder $
    byteString "(" <> buildSql92 a <> byteString ") " <>
    byteString op <>
    byteString " (" <> buildSql92 b <> byteString ")"

renderSql92 :: Sql92SyntaxBuilder -> String
renderSql92 (Sql92SyntaxBuilder b) = BL.unpack (toLazyByteString b)

buildSepBy :: Builder -> [Builder] -> Builder
buildSepBy sep [] = mempty
buildSepBy sep [x] = x
buildSepBy sep (x:xs) = x <> sep <> buildSepBy sep xs

-- * Make SQL Literals

type MakeSqlLiterals syntax t = FieldsFulfillConstraint (HasSqlValueSyntax syntax) t

makeSqlLiterals ::
  forall sql t.
  MakeSqlLiterals sql t =>
  t Identity -> t (WithConstraint (HasSqlValueSyntax sql))
makeSqlLiterals tbl =
  to (gWithConstrainedFields (Proxy @(HasSqlValueSyntax sql)) (Proxy @(Rep (t Exposed))) (from tbl))

insertValuesGeneric ::
    forall syntax table exprValues.
    ( Beamable table
    , IsSql92InsertValuesSyntax syntax
    , IsSql92ExpressionSyntax (Sql92InsertValuesExpressionSyntax syntax)
    , exprValues ~ Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax syntax)
    , MakeSqlLiterals exprValues table ) =>
    [ table Identity ] -> syntax
insertValuesGeneric tbls =
    insertSqlExpressions (map mkSqlExprs tbls)
  where
    mkSqlExprs = allBeamValues
                   (\(Columnar' (WithConstraint x :: WithConstraint (HasSqlValueSyntax exprValues) x)) ->
                        valueE (sqlValueSyntax x)) .
                 makeSqlLiterals
