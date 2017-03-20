{-# LANGUAGE PolyKinds #-}
module Database.Beam.Backend.SQL.Builder
  ( SqlSyntaxBuilder, buildSql
  , renderSql ) where

import           Database.Beam.Backend.SQL.Types
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Backend.SQL.SQL99

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Data.Text (Text)

import           Data.Coerce
import           Data.Monoid
import           Data.String

newtype SqlSyntaxBuilder
  = SqlSyntaxBuilder { buildSql :: Builder }
newtype SqlSyntaxBuilder1 (x :: k)
  = SqlSyntaxBuilder1 { buildSql_1 :: Builder }

instance Eq SqlSyntaxBuilder where
  a == b = toLazyByteString (buildSql a) == toLazyByteString (buildSql b)

instance Monoid SqlSyntaxBuilder where
  mempty = SqlSyntaxBuilder mempty
  mappend (SqlSyntaxBuilder a) (SqlSyntaxBuilder b) =
    SqlSyntaxBuilder (mappend a b)

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

  selectTableStmt proj from where_ grouping having =
    SqlSyntaxBuilder $
    byteString "SELECT " <> buildSql proj <>
    (maybe mempty ((byteString " FROM " <>) . buildSql) from) <>
    (maybe mempty (\w -> byteString " WHERE " <> buildSql w) where_) <>
    (maybe mempty (\g -> byteString " GROUP BY " <> buildSql g) grouping) <>
    (maybe mempty (\e -> byteString " HAVING " <> buildSql e) having)

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

  insertStmt table fields values =
    SqlSyntaxBuilder $
    byteString "INSERT INTO " <> quoteSql table <>
    byteString "(" <> buildSepBy (byteString ", ") (map quoteSql fields) <> byteString ") " <>
    buildSql values

instance IsSql92UpdateSyntax SqlSyntaxBuilder where
  type Sql92UpdateFieldNameSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92UpdateExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  updateStmt table set where_ =
    SqlSyntaxBuilder undefined

instance IsSql92DeleteSyntax SqlSyntaxBuilder where
  type Sql92DeleteExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  deleteStmt tbl where_ =
    SqlSyntaxBuilder $
    byteString "DELETE FROM " <> quoteSql tbl <>
    byteString " WHERE " <> buildSql where_

instance IsSql92FieldNameSyntax SqlSyntaxBuilder where
  qualifiedField a b =
    SqlSyntaxBuilder $
    byteString "`" <> stringUtf8 (T.unpack a) <> byteString "`.`" <>
    stringUtf8 (T.unpack b) <> byteString "`"
  unqualifiedField a =
    SqlSyntaxBuilder $
    byteString "`" <> stringUtf8 (T.unpack a) <> byteString "`"

instance IsSqlExpressionSyntaxStringType SqlSyntaxBuilder Text

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
    SqlSyntaxBuilder $ buildSql what <> byteString " FROM (" <> buildSql from <> byteString ")"
  absE = sqlFuncOp "ABS"
  charLengthE = sqlFuncOp "CHAR_LENGTH"
  bitLengthE = sqlFuncOp "BIT_LENGTH"
  octetLengthE = sqlFuncOp "OCTET_LENGTH"

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

instance IsSql99ExpressionSyntax SqlSyntaxBuilder where
  distinctE = sqlUnOp "DISTINCT"
  similarToE = sqlBinOp "SIMILAR TO"

instance IsSql92AggregationExpressionSyntax SqlSyntaxBuilder where
  type Sql92AggregationSetQuantifierSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  countAllE = SqlSyntaxBuilder (byteString "COUNT(*)")
  countE q x = SqlSyntaxBuilder (byteString "COUNT(" <> maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")
  avgE q x = SqlSyntaxBuilder (byteString "AVG(" <>  maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")
  minE q x = SqlSyntaxBuilder (byteString "COUNT(" <> maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")
  maxE q x = SqlSyntaxBuilder (byteString "COUNT(" <> maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")
  sumE q x = SqlSyntaxBuilder (byteString "COUNT(" <> maybe mempty (\q' -> buildSql q' <> byteString " ") q <> buildSql x <> byteString ")")

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
  type Sql92TableSourceSelectSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  tableNamed t = SqlSyntaxBuilder (quoteSql t)
  tableFromSubSelect query = SqlSyntaxBuilder (byteString "(" <> buildSql query <> byteString ")")

instance IsSql92FromSyntax SqlSyntaxBuilder where
    type Sql92FromTableSourceSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
    type Sql92FromExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

    fromTable t Nothing = t
    fromTable t (Just nm) = SqlSyntaxBuilder (buildSql t <> byteString " AS " <> quoteSql nm)

    innerJoin = join "INNER JOIN"
    leftJoin = join "LEFT JOIN"
    rightJoin = join "RIGHT JOIN"

-- TODO These instances are wrong (Text doesn't handle quoting for example)
instance HasSqlValueSyntax SqlSyntaxBuilder Int where
  sqlValueSyntax x = SqlSyntaxBuilder $
    byteString (fromString (show x))
instance HasSqlValueSyntax SqlSyntaxBuilder Bool where
  sqlValueSyntax True = SqlSyntaxBuilder (byteString "TRUE")
  sqlValueSyntax False = SqlSyntaxBuilder (byteString "FALSE")
instance HasSqlValueSyntax SqlSyntaxBuilder Text where
  sqlValueSyntax x = SqlSyntaxBuilder $
    byteString (fromString (show x))
instance HasSqlValueSyntax SqlSyntaxBuilder SqlNull where
  sqlValueSyntax x = SqlSyntaxBuilder (byteString "NULL")


renderSql :: SqlSyntaxBuilder -> String
renderSql (SqlSyntaxBuilder b) = BL.unpack (toLazyByteString b)

buildSepBy :: Builder -> [Builder] -> Builder
buildSepBy sep [] = mempty
buildSepBy sep [x] = x
buildSepBy sep (x:xs) = x <> sep <> buildSepBy sep xs

-- TODO actual quoting
quoteSql table =
    byteString "\"" <> byteString (TE.encodeUtf8 table) <> byteString "\""

join type_ a b on =
    SqlSyntaxBuilder $
    buildSql a <> byteString " " <>  byteString type_ <> byteString " " <> buildSql b <>
    case on of
      Nothing -> mempty
      Just on -> byteString " ON (" <> buildSql on <> byteString ")"
sqlUnOp op a =
  SqlSyntaxBuilder $
  byteString op <> byteString " (" <> buildSql a <> byteString ")"
sqlPostFixOp op a =
  SqlSyntaxBuilder $
  byteString "(" <> buildSql a <> byteString ") " <> byteString op
sqlCompOp op quant a b =
    SqlSyntaxBuilder $
    byteString "(" <> buildSql a <> byteString ") " <>
    byteString op <>
    maybe mempty (\quant -> byteString " " <> buildSql quant) quant <>
    byteString " (" <> buildSql b <> byteString ")"
sqlBinOp op a b =
    SqlSyntaxBuilder $
    byteString "(" <> buildSql a <> byteString ") " <>
    byteString op <>
    byteString " (" <> buildSql b <> byteString ")"
sqlFuncOp fun a =
  SqlSyntaxBuilder $
  byteString fun <> byteString "(" <> buildSql a <> byteString")"
