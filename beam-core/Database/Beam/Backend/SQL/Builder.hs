{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a syntax 'SqlSyntaxBuilder' that uses a
--   'Data.ByteString.Builder.Builder' to construct SQL expressions as strings.
--   Mainly serves as documentation for how to write a syntax for backends. Note
--   that, although you can use this to turn most 'Q' and 'QGenExpr's into
--   'ByteString' queries, it is /very unwise/ to ship these to the database.
--   This module does not take into account server-specific quoting. Some
--   backends are very particular to quoting, and shipping arbitrary
--   'ByteString's as queries can expose you to SQL injection vulnerabilities.
--   Always use the provided backends to submit queries and data manipulation
--   commands to the database.
module Database.Beam.Backend.SQL.Builder
  ( SqlSyntaxBuilder(..) --, SqlSyntaxBackend
  , buildSepBy
  , quoteSql
  , renderSql ) where

import           Database.Beam.Backend.Internal.Compat
import           Database.Beam.Backend.SQL

import           Control.Monad.IO.Class

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Data.Text (Text)

import           Data.Coerce
import           Data.Hashable
import           Data.Int
import           Data.String
import qualified Control.Monad.Fail as Fail
import           GHC.TypeLits

-- | The main syntax. A wrapper over 'Builder'
newtype SqlSyntaxBuilder
  = SqlSyntaxBuilder { buildSql :: Builder }

instance Hashable SqlSyntaxBuilder where
  hashWithSalt salt (SqlSyntaxBuilder b) = hashWithSalt salt (toLazyByteString b)

instance Show SqlSyntaxBuilder where
  showsPrec prec (SqlSyntaxBuilder s) =
    showParen (prec > 10) $
    showString "SqlSyntaxBuilder (" .
    shows (toLazyByteString s) .
    showString ")"

instance Eq SqlSyntaxBuilder where
  a == b = toLazyByteString (buildSql a) == toLazyByteString (buildSql b)

instance Semigroup SqlSyntaxBuilder where
  SqlSyntaxBuilder a <> SqlSyntaxBuilder b =  SqlSyntaxBuilder (a <> b)

instance Monoid SqlSyntaxBuilder where
  mempty = SqlSyntaxBuilder mempty
  mappend = (<>)

instance IsSql92Syntax SqlSyntaxBuilder where
  type Sql92SelectSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92InsertSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92DeleteSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92UpdateSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  selectCmd = id
  insertCmd = id
  updateCmd = id
  deleteCmd = id

instance IsSql92SelectSyntax SqlSyntaxBuilder where
  type Sql92SelectSelectTableSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92SelectOrderingSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  selectStmt tableSrc ordering limit offset =
      SqlSyntaxBuilder $
      buildSql tableSrc <>
      ( case ordering of
          [] -> mempty
          _ -> byteString " ORDER BY " <>
               buildSepBy (byteString ", ") (map buildSql ordering) ) <>
      maybe mempty (\l -> byteString " LIMIT " <> byteString (fromString (show l))) limit <>
      maybe mempty (\o -> byteString " OFFSET " <> byteString (fromString (show o))) offset

instance IsSql92GroupingSyntax SqlSyntaxBuilder where
  type Sql92GroupingExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  groupByExpressions es =
    SqlSyntaxBuilder $
    buildSepBy (byteString ", ") (map buildSql es)

instance IsSql92SelectTableSyntax SqlSyntaxBuilder where
  type Sql92SelectTableSelectSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92SelectTableExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92SelectTableProjectionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92SelectTableFromSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92SelectTableGroupingSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92SelectTableSetQuantifierSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  selectTableStmt setQuantifier proj from where_ grouping having =
    SqlSyntaxBuilder $
    byteString "SELECT " <>
    maybe mempty (\setQuantifier' -> buildSql setQuantifier' <> byteString " ") setQuantifier <>
    buildSql proj <>
    maybe mempty ((byteString " FROM " <>) . buildSql) from <>
    maybe mempty (\w -> byteString " WHERE " <> buildSql w) where_ <>
    maybe mempty (\g -> byteString " GROUP BY " <> buildSql g) grouping <>
    maybe mempty (\e -> byteString " HAVING " <> buildSql e) having

  unionTables = tableOp "UNION"
  intersectTables  = tableOp "INTERSECT"
  exceptTable = tableOp "EXCEPT"

tableOp :: ByteString -> Bool -> SqlSyntaxBuilder -> SqlSyntaxBuilder -> SqlSyntaxBuilder
tableOp op all a b =
  SqlSyntaxBuilder $
  byteString "(" <> buildSql a <> byteString ") " <>
  byteString op <> if all then byteString " ALL " else mempty <>
  byteString " (" <> buildSql b <> byteString ")"

instance IsSql92InsertSyntax SqlSyntaxBuilder where
  type Sql92InsertValuesSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92InsertTableNameSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  insertStmt tblNm fields values =
    SqlSyntaxBuilder $
    byteString "INSERT INTO " <> buildSql tblNm <>
    byteString "(" <> buildSepBy (byteString ", ") (map quoteSql fields) <> byteString ") " <>
    buildSql values

instance IsSql92InsertValuesSyntax SqlSyntaxBuilder where
  type Sql92InsertValuesExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92InsertValuesSelectSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  insertSqlExpressions values =
    SqlSyntaxBuilder $
    byteString "VALUES " <>
    buildSepBy (byteString ", ") (map mkValues values)
    where mkValues values' =
            byteString "(" <>
            buildSepBy (byteString ", ") (map buildSql values') <>
            byteString ")"

  insertFromSql select = select

instance IsSql92UpdateSyntax SqlSyntaxBuilder where
  type Sql92UpdateFieldNameSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92UpdateExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92UpdateTableNameSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  updateStmt tblNm set where_ =
    SqlSyntaxBuilder $
    byteString "UPDATE " <> buildSql tblNm <>
    (case set of
       [] -> mempty
       es -> byteString " SET " <> buildSepBy (byteString ", ") (map (\(field, expr) -> buildSql field <> byteString "=" <> buildSql expr) es)) <>
    maybe mempty (\where_ -> byteString " WHERE " <> buildSql where_) where_

instance IsSql92DeleteSyntax SqlSyntaxBuilder where
  type Sql92DeleteExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92DeleteTableNameSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  deleteStmt tblNm alias where_ =
    SqlSyntaxBuilder $
    byteString "DELETE FROM " <> buildSql tblNm <>
    maybe mempty (\alias_ -> byteString " AS " <> quoteSql alias_) alias <>
    maybe mempty (\where_ -> byteString " WHERE " <> buildSql where_) where_

  deleteSupportsAlias _ = True

instance IsSql92FieldNameSyntax SqlSyntaxBuilder where
  qualifiedField a b =
    SqlSyntaxBuilder $
    byteString "`" <> stringUtf8 (T.unpack a) <> byteString "`.`" <>
    stringUtf8 (T.unpack b) <> byteString "`"
  unqualifiedField a =
    SqlSyntaxBuilder $
    byteString "`" <> stringUtf8 (T.unpack a) <> byteString "`"

instance IsSql92QuantifierSyntax SqlSyntaxBuilder where
  quantifyOverAll = SqlSyntaxBuilder "ALL"
  quantifyOverAny = SqlSyntaxBuilder "ANY"

instance IsSql92ExtractFieldSyntax SqlSyntaxBuilder where
  secondsField = SqlSyntaxBuilder (byteString "SECOND")
  minutesField = SqlSyntaxBuilder (byteString "MINUTE")
  hourField    = SqlSyntaxBuilder (byteString "HOUR")
  dayField     = SqlSyntaxBuilder (byteString "DAY")
  monthField   = SqlSyntaxBuilder (byteString "MONTH")
  yearField    = SqlSyntaxBuilder (byteString "YEAR")

instance IsSql92ExpressionSyntax SqlSyntaxBuilder where
  type Sql92ExpressionValueSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ExpressionSelectSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ExpressionFieldNameSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ExpressionQuantifierSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ExpressionCastTargetSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ExpressionExtractFieldSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  rowE vs = SqlSyntaxBuilder $
            byteString "(" <>
            buildSepBy (byteString ", ") (map buildSql (coerce vs)) <>
            byteString ")"
  isNotNullE = sqlPostFixOp "IS NOT NULL"
  isNullE = sqlPostFixOp "IS NULL"
  isTrueE = sqlPostFixOp "IS TRUE"
  isFalseE = sqlPostFixOp "IS FALSE"
  isUnknownE = sqlPostFixOp "IS UNKNOWN"
  isNotTrueE = sqlPostFixOp "IS NOT TRUE"
  isNotFalseE = sqlPostFixOp "IS NOT FALSE"
  isNotUnknownE = sqlPostFixOp "IS NOT UNKNOWN"
  caseE cases else_ =
    SqlSyntaxBuilder $
    byteString "CASE " <>
    foldMap (\(cond, res) -> byteString "WHEN " <> buildSql cond <>
                             byteString " THEN " <> buildSql res <> byteString " ") cases <>
    byteString "ELSE " <> buildSql else_ <> byteString " END"
  fieldE = id

  nullIfE a b = sqlFuncOp "NULLIF" $
                SqlSyntaxBuilder $
                byteString "(" <> buildSql a <> byteString "), (" <>
                buildSql b <> byteString ")"
  positionE needle haystack =
    SqlSyntaxBuilder $ byteString "POSITION(" <> buildSql needle <> byteString ") IN (" <> buildSql haystack <> byteString ")"
  extractE what from =
    SqlSyntaxBuilder $ byteString "EXTRACT(" <> buildSql what <> byteString " FROM (" <> buildSql from <> byteString "))"
  absE = sqlFuncOp "ABS"
  charLengthE = sqlFuncOp "CHAR_LENGTH"
  bitLengthE = sqlFuncOp "BIT_LENGTH"
  octetLengthE = sqlFuncOp "OCTET_LENGTH"
  lowerE = sqlFuncOp "LOWER"
  upperE = sqlFuncOp "UPPER"
  trimE = sqlFuncOp "TRIM"

  addE = sqlBinOp "+"
  likeE = sqlBinOp "LIKE"
  subE = sqlBinOp "-"
  mulE = sqlBinOp "*"
  divE = sqlBinOp "/"
  modE = sqlBinOp "%"
  overlapsE = sqlBinOp "OVERLAPS"
  andE = sqlBinOp "AND"
  orE  = sqlBinOp "OR"
  castE a ty = SqlSyntaxBuilder $
    byteString "CAST((" <> buildSql a <> byteString ") AS " <> buildSql ty <> byteString ")"
  coalesceE es = SqlSyntaxBuilder $
    byteString "COALESCE(" <>
    buildSepBy (byteString ", ") (map (\e -> byteString"(" <> buildSql e <> byteString ")") es) <>
    byteString ")"
  betweenE a b c = SqlSyntaxBuilder $
    byteString "(" <> buildSql a <> byteString ") BETWEEN (" <> buildSql b <> byteString ") AND (" <> buildSql c <> byteString ")"
  eqE  = sqlCompOp "="
  neqE = sqlCompOp "<>"
  ltE  = sqlCompOp "<"
  gtE  = sqlCompOp ">"
  leE  = sqlCompOp "<="
  geE  = sqlCompOp ">="
  negateE = sqlUnOp "-"
  notE = sqlUnOp "NOT"
  existsE = sqlUnOp "EXISTS"
  uniqueE = sqlUnOp "UNIQUE"
  subqueryE a = SqlSyntaxBuilder $ byteString "(" <> buildSql a <> byteString ")"
  valueE = id

  currentTimestampE = SqlSyntaxBuilder (byteString "CURRENT_TIMESTAMP")

  defaultE = SqlSyntaxBuilder (byteString "DEFAULT")
  inE a es = SqlSyntaxBuilder (byteString "(" <> buildSql a <> byteString ") IN (" <>
                               buildSepBy (byteString ", ") (map buildSql es))

instance IsSql99FunctionExpressionSyntax SqlSyntaxBuilder where
  functionNameE fn = SqlSyntaxBuilder (byteString (TE.encodeUtf8 fn))
  functionCallE function args =
    SqlSyntaxBuilder $
    buildSql function <>
    byteString "(" <>
    buildSepBy (byteString ", ") (map buildSql args) <>
    byteString ")"

instance IsSql99ExpressionSyntax SqlSyntaxBuilder where
  distinctE = sqlUnOp "DISTINCT"
  similarToE = sqlBinOp "SIMILAR TO"

  instanceFieldE e fieldNm =
    SqlSyntaxBuilder $
    byteString "(" <> buildSql e <> byteString ")." <> byteString (TE.encodeUtf8 fieldNm)
  refFieldE e fieldNm =
    SqlSyntaxBuilder $
    byteString "(" <> buildSql e <> byteString ")->" <> byteString (TE.encodeUtf8 fieldNm)

instance IsSql2003ExpressionSyntax SqlSyntaxBuilder where
  type Sql2003ExpressionWindowFrameSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  overE expr frame =
      SqlSyntaxBuilder $
      buildSql expr <> buildSql frame
  rowNumberE = SqlSyntaxBuilder (byteString "ROW_NUMBER()")

instance IsSql2003WindowFrameSyntax SqlSyntaxBuilder where
  type Sql2003WindowFrameExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql2003WindowFrameOrderingSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql2003WindowFrameBoundsSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  frameSyntax partition_ ordering_ bounds_ =
      SqlSyntaxBuilder $
      byteString " OVER (" <>
      maybe mempty (\p -> byteString "PARTITION BY " <> buildSepBy (byteString ", ") (map buildSql p)) partition_ <>
      maybe mempty (\o -> byteString " ORDER BY " <> buildSepBy (byteString ", ") (map buildSql o)) ordering_ <>
      maybe mempty (\b -> byteString " ROWS " <> buildSql b) bounds_ <>
      byteString ")"

instance IsSql2003ExpressionElementaryOLAPOperationsSyntax SqlSyntaxBuilder where
  filterAggE agg_ filter_ =
    SqlSyntaxBuilder $
    buildSql agg_ <> byteString " FILTER (WHERE " <> buildSql filter_ <> byteString ")"
  rankAggE = SqlSyntaxBuilder "RANK()"

instance IsSql2003ExpressionAdvancedOLAPOperationsSyntax SqlSyntaxBuilder where
  denseRankAggE = SqlSyntaxBuilder "DENSE_RANK()"
  percentRankAggE = SqlSyntaxBuilder "PERCENT_RANK()"
  cumeDistAggE = SqlSyntaxBuilder "CUME_DIST()"

data SqlWindowFrameBound = SqlWindowFrameUnbounded
                         | SqlWindowFrameBounded Int
                           deriving Show

instance IsSql2003WindowFrameBoundsSyntax SqlSyntaxBuilder where
  type Sql2003WindowFrameBoundsBoundSyntax SqlSyntaxBuilder = SqlWindowFrameBound

  fromToBoundSyntax SqlWindowFrameUnbounded Nothing = SqlSyntaxBuilder "UNBOUNDED PRECEDING"
  fromToBoundSyntax (SqlWindowFrameBounded 0) Nothing = SqlSyntaxBuilder "CURRENT ROW"
  fromToBoundSyntax (SqlWindowFrameBounded n) Nothing = SqlSyntaxBuilder (fromString (show n) <> " PRECEDING")
  fromToBoundSyntax SqlWindowFrameUnbounded (Just SqlWindowFrameUnbounded) =
      SqlSyntaxBuilder "BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING"
  fromToBoundSyntax SqlWindowFrameUnbounded (Just (SqlWindowFrameBounded 0)) =
      SqlSyntaxBuilder "BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW"
  fromToBoundSyntax SqlWindowFrameUnbounded (Just (SqlWindowFrameBounded n)) =
      SqlSyntaxBuilder ("BETWEEN UNBOUNDED PRECEDING AND " <> fromString (show n) <> " FOLLOWING")
  fromToBoundSyntax (SqlWindowFrameBounded 0) (Just SqlWindowFrameUnbounded) =
      SqlSyntaxBuilder "BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING"
  fromToBoundSyntax (SqlWindowFrameBounded 0) (Just (SqlWindowFrameBounded 0)) =
      SqlSyntaxBuilder "BETWEEN CURRENT ROW AND CURRENT ROW"
  fromToBoundSyntax (SqlWindowFrameBounded 0) (Just (SqlWindowFrameBounded n)) =
      SqlSyntaxBuilder ("BETWEEN CURRENT ROW AND " <> fromString (show n) <> " FOLLOWING")
  fromToBoundSyntax (SqlWindowFrameBounded n) (Just SqlWindowFrameUnbounded) =
      SqlSyntaxBuilder ("BETWEEN " <> fromString (show n) <> " PRECEDING AND UNBOUNDED FOLLOWING")
  fromToBoundSyntax (SqlWindowFrameBounded n) (Just (SqlWindowFrameBounded 0)) =
      SqlSyntaxBuilder ("BETWEEN " <> fromString (show n) <> " PRECEDING AND CURRENT ROW")
  fromToBoundSyntax (SqlWindowFrameBounded n1) (Just (SqlWindowFrameBounded n2)) =
      SqlSyntaxBuilder ("BETWEEN " <> fromString (show n1) <> " PRECEDING AND " <> fromString (show n2) <> " FOLLOWING")

instance IsSql2003WindowFrameBoundSyntax SqlWindowFrameBound where
  unboundedSyntax = SqlWindowFrameUnbounded
  nrowsBoundSyntax = SqlWindowFrameBounded

instance IsSql92AggregationExpressionSyntax SqlSyntaxBuilder where
  type Sql92AggregationSetQuantifierSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  countAllE = SqlSyntaxBuilder (byteString "COUNT(*)")
  countE q x = SqlSyntaxBuilder (byteString "COUNT(" <> maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")
  avgE q x = SqlSyntaxBuilder (byteString "AVG(" <>  maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")
  minE q x = SqlSyntaxBuilder (byteString "MIN(" <> maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")
  maxE q x = SqlSyntaxBuilder (byteString "MAX(" <> maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")
  sumE q x = SqlSyntaxBuilder (byteString "SUM(" <> maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")

instance IsSql92AggregationSetQuantifierSyntax SqlSyntaxBuilder where
  setQuantifierAll = SqlSyntaxBuilder (byteString "ALL")
  setQuantifierDistinct = SqlSyntaxBuilder (byteString "DISTINCT")

instance IsSql92ProjectionSyntax SqlSyntaxBuilder where
  type Sql92ProjectionExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  projExprs exprs =
      SqlSyntaxBuilder $
      buildSepBy (byteString ", ")
                 (map (\(expr, nm) -> buildSql expr <>
                                      maybe mempty (\nm -> byteString " AS " <> quoteSql nm) nm) exprs)

instance IsSql92OrderingSyntax SqlSyntaxBuilder where
  type Sql92OrderingExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  ascOrdering expr = SqlSyntaxBuilder (buildSql expr <> byteString " ASC")
  descOrdering expr = SqlSyntaxBuilder (buildSql expr <> byteString " DESC")

instance IsSql92TableSourceSyntax SqlSyntaxBuilder where
  type Sql92TableSourceTableNameSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92TableSourceSelectSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92TableSourceExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  tableNamed = id
  tableFromSubSelect query = SqlSyntaxBuilder (byteString "(" <> buildSql query <> byteString ")")
  tableFromValues vss =
      SqlSyntaxBuilder $
      byteString "VALUES " <>
      buildSepBy (byteString ", ")
       (map (\vs -> byteString "(" <>
                    buildSepBy (byteString ", ") (map buildSql vs) <>
                    byteString ")") vss)

instance IsSql92TableNameSyntax SqlSyntaxBuilder where
  tableName Nothing t  = SqlSyntaxBuilder $ quoteSql t
  tableName (Just s) t = SqlSyntaxBuilder $ quoteSql s <> byteString "." <> quoteSql t

instance IsSql92FromSyntax SqlSyntaxBuilder where
    type Sql92FromTableSourceSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
    type Sql92FromExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

    fromTable t Nothing = t
    fromTable t (Just (nm, colNms)) =
        SqlSyntaxBuilder (buildSql t <> byteString " AS " <> quoteSql nm <>
                          maybe mempty (\colNms' -> byteString "(" <> buildSepBy (byteString ", ") (map quoteSql colNms') <> byteString ")") colNms)

    innerJoin = join "INNER JOIN"
    leftJoin = join "LEFT JOIN"
    rightJoin = join "RIGHT JOIN"

instance IsSql92DataTypeSyntax SqlSyntaxBuilder where
    domainType nm = SqlSyntaxBuilder (quoteSql nm)
    charType prec charSet = SqlSyntaxBuilder ("CHAR" <> sqlOptPrec prec <> sqlOptCharSet charSet)
    varCharType prec charSet = SqlSyntaxBuilder ("VARCHAR" <> sqlOptPrec prec <> sqlOptCharSet charSet)
    nationalCharType prec = SqlSyntaxBuilder ("NATIONAL CHAR" <> sqlOptPrec prec)
    nationalVarCharType prec = SqlSyntaxBuilder ("NATIONAL CHARACTER VARYING" <> sqlOptPrec prec)

    bitType prec = SqlSyntaxBuilder ("BIT" <> sqlOptPrec prec)
    varBitType prec = SqlSyntaxBuilder ("BIT VARYING" <> sqlOptPrec prec)

    numericType prec = SqlSyntaxBuilder ("NUMERIC" <> sqlOptNumericPrec prec)
    decimalType prec = SqlSyntaxBuilder ("DECIMAL" <> sqlOptNumericPrec prec)

    intType = SqlSyntaxBuilder "INT"
    smallIntType = SqlSyntaxBuilder "SMALLINT"

    floatType prec = SqlSyntaxBuilder ("FLOAT" <> sqlOptPrec prec)
    doubleType = SqlSyntaxBuilder "DOUBLE PRECISION"
    realType = SqlSyntaxBuilder "REAL"
    dateType = SqlSyntaxBuilder "DATE"
    timeType prec withTz =
        SqlSyntaxBuilder ("TIME" <> sqlOptPrec prec <> if withTz then " WITH TIME ZONE" else mempty)
    timestampType prec withTz =
        SqlSyntaxBuilder ("TIMESTAMP" <> sqlOptPrec prec <> if withTz then " WITH TIME ZONE" else mempty)

sqlOptPrec :: Maybe Word -> Builder
sqlOptPrec Nothing = mempty
sqlOptPrec (Just x) = "(" <> byteString (fromString (show x)) <> ")"

sqlOptCharSet :: Maybe T.Text -> Builder
sqlOptCharSet Nothing = mempty
sqlOptCharSet (Just cs) = " CHARACTER SET " <> byteString (TE.encodeUtf8 cs)

sqlOptNumericPrec :: Maybe (Word, Maybe Word) -> Builder
sqlOptNumericPrec Nothing = mempty
sqlOptNumericPrec (Just (prec, Nothing)) = sqlOptPrec (Just prec)
sqlOptNumericPrec (Just (prec, Just dec)) = "(" <> fromString (show prec) <> ", " <> fromString (show dec) <> ")"

-- TODO These instances are wrong (Text doesn't handle quoting for example)
instance HasSqlValueSyntax SqlSyntaxBuilder Int32 where
  sqlValueSyntax x = SqlSyntaxBuilder $
    byteString (fromString (show x))
instance HasSqlValueSyntax SqlSyntaxBuilder Bool where
  sqlValueSyntax True = SqlSyntaxBuilder (byteString "TRUE")
  sqlValueSyntax False = SqlSyntaxBuilder (byteString "FALSE")
instance HasSqlValueSyntax SqlSyntaxBuilder Text where
  sqlValueSyntax x = SqlSyntaxBuilder $
    byteString (fromString (show x))
instance HasSqlValueSyntax SqlSyntaxBuilder SqlNull where
  sqlValueSyntax _ = SqlSyntaxBuilder (byteString "NULL")

instance TypeError (PreferExplicitSize Int Int32) => HasSqlValueSyntax SqlSyntaxBuilder Int where
  sqlValueSyntax x = SqlSyntaxBuilder $
    byteString (fromString (show x))

renderSql :: SqlSyntaxBuilder -> String
renderSql (SqlSyntaxBuilder b) = BL.unpack (toLazyByteString b)

buildSepBy :: Builder -> [Builder] -> Builder
buildSepBy _   [] = mempty
buildSepBy _   [x] = x
buildSepBy sep (x:xs) = x <> sep <> buildSepBy sep xs

-- TODO actual quoting
quoteSql :: Text -> Builder
quoteSql table =
    byteString "\"" <> byteString (TE.encodeUtf8 table) <> byteString "\""

join :: ByteString
     -> SqlSyntaxBuilder -> SqlSyntaxBuilder
     -> Maybe SqlSyntaxBuilder -> SqlSyntaxBuilder
join type_ a b on =
    SqlSyntaxBuilder $
    buildSql a <> byteString " " <>  byteString type_ <> byteString " " <> buildSql b <>
    case on of
      Nothing -> mempty
      Just on -> byteString " ON (" <> buildSql on <> byteString ")"
sqlPostFixOp, sqlUnOp :: ByteString -> SqlSyntaxBuilder -> SqlSyntaxBuilder
sqlUnOp op a =
  SqlSyntaxBuilder $
  byteString op <> byteString " (" <> buildSql a <> byteString ")"
sqlPostFixOp op a =
  SqlSyntaxBuilder $
  byteString "(" <> buildSql a <> byteString ") " <> byteString op
sqlCompOp :: ByteString -> Maybe SqlSyntaxBuilder
          -> SqlSyntaxBuilder -> SqlSyntaxBuilder
          -> SqlSyntaxBuilder
sqlCompOp op quant a b =
    SqlSyntaxBuilder $
    byteString "(" <> buildSql a <> byteString ") " <>
    byteString op <>
    maybe mempty (\quant -> byteString " " <> buildSql quant) quant <>
    byteString " (" <> buildSql b <> byteString ")"
sqlBinOp :: ByteString -> SqlSyntaxBuilder -> SqlSyntaxBuilder
         -> SqlSyntaxBuilder
sqlBinOp op a b =
    SqlSyntaxBuilder $
    byteString "(" <> buildSql a <> byteString ") " <>
    byteString op <>
    byteString " (" <> buildSql b <> byteString ")"
sqlFuncOp :: ByteString -> SqlSyntaxBuilder -> SqlSyntaxBuilder
sqlFuncOp fun a =
  SqlSyntaxBuilder $
  byteString fun <> byteString "(" <> buildSql a <> byteString")"


-- * Fake 'MonadBeam' instance (for using 'SqlSyntaxBuilder' with migrations mainly)

-- data SqlSyntaxBackend

-- class Trivial a
-- instance Trivial a

newtype SqlSyntaxM a = SqlSyntaxM (IO a)
  deriving (Applicative, Functor, Monad, MonadIO, Fail.MonadFail)
