{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing#-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Beam.Sqlite.Syntax
  ( SqliteSyntax(..)

  , SqliteCommandSyntax(..)

  , SqliteSelectSyntax(..), SqliteInsertSyntax(..)
  , SqliteInsertValuesSyntax(..)

  , SqliteValueSyntax(..)

  , sqliteEscape ) where

import           Database.Beam.Backend.SQL
import           Database.Beam.Query.SQL92
import           Database.Beam.Query

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.Coerce
import qualified Data.DList as DL
import           Data.Int
import           Data.Monoid
import           Data.String
import qualified Data.Text as T

import           Database.SQLite.Simple (SQLData(..))

data SqliteSyntax = SqliteSyntax Builder (DL.DList SQLData)

instance Monoid SqliteSyntax where
  mempty = SqliteSyntax mempty mempty
  mappend (SqliteSyntax ab av) (SqliteSyntax bb bv) =
    SqliteSyntax (ab <> bb) (av <> bv)

instance Eq SqliteSyntax where
  SqliteSyntax ab av == SqliteSyntax bb bv =
    toLazyByteString ab == toLazyByteString bb &&
    av == bv

emit :: ByteString -> SqliteSyntax
emit b = SqliteSyntax (byteString b) mempty

emit' :: Show a => a -> SqliteSyntax
emit' x = SqliteSyntax (byteString (fromString (show x))) mempty

quotedIdentifier :: T.Text -> SqliteSyntax
quotedIdentifier txt = emit "\"" <> SqliteSyntax (stringUtf8 (T.unpack (sqliteEscape txt))) mempty <> emit "\""

sqliteEscape :: T.Text -> T.Text
sqliteEscape = T.concatMap (\c -> if c == '"' then "\"\"" else T.singleton c)

emitValue :: SQLData -> SqliteSyntax
emitValue v = SqliteSyntax (byteString "?") (DL.singleton v)

-- * Syntax types

newtype SqliteCommandSyntax = SqliteCommandSyntax { fromSqliteCommand :: SqliteSyntax }
newtype SqliteSelectSyntax = SqliteSelectSyntax { fromSqliteSelect :: SqliteSyntax }
instance HasQBuilder SqliteSelectSyntax where
  buildSqlQuery = buildSql92Query' False -- SQLite does not support arbitrarily nesting UNION, INTERSECT, and EXCEPT
newtype SqliteInsertSyntax = SqliteInsertSyntax { fromSqliteInsert :: SqliteSyntax }
newtype SqliteUpdateSyntax = SqliteUpdateSyntax { fromSqliteUpdate :: SqliteSyntax }
newtype SqliteDeleteSyntax = SqliteDeleteSyntax { fromSqliteDelete :: SqliteSyntax }

newtype SqliteSelectTableSyntax = SqliteSelectTableSyntax { fromSqliteSelectTable :: SqliteSyntax }
newtype SqliteExpressionSyntax = SqliteExpressionSyntax { fromSqliteExpression :: SqliteSyntax } deriving Eq
newtype SqliteFromSyntax = SqliteFromSyntax { fromSqliteFromSyntax :: SqliteSyntax }
newtype SqliteComparisonQuantifierSyntax = SqliteComparisonQuantifierSyntax { fromSqliteComparisonQuantifier :: SqliteSyntax }
newtype SqliteExtractFieldSyntax = SqliteExtractFieldSyntax { fromSqliteExtractField :: SqliteSyntax }
newtype SqliteAggregationSetQuantifierSyntax = SqliteAggregationSetQuantifierSyntax { fromSqliteAggregationSetQuantifier :: SqliteSyntax }
newtype SqliteProjectionSyntax = SqliteProjectionSyntax { fromSqliteProjection :: SqliteSyntax }
newtype SqliteGroupingSyntax = SqliteGroupingSyntax { fromSqliteGrouping :: SqliteSyntax }
newtype SqliteOrderingSyntax = SqliteOrderingSyntax { fromSqliteOrdering :: SqliteSyntax }
newtype SqliteValueSyntax = SqliteValueSyntax { fromSqliteValue :: SqliteSyntax }
newtype SqliteTableSourceSyntax = SqliteTableSourceSyntax { fromSqliteTableSource :: SqliteSyntax }
newtype SqliteFieldNameSyntax = SqliteFieldNameSyntax { fromSqliteFieldNameSyntax :: SqliteSyntax }
newtype SqliteInsertValuesSyntax = SqliteInsertValuesSyntax { fromSqliteInsertValues :: SqliteSyntax }
newtype SqliteCreateTableSyntax = SqliteCreateTableSyntax { fromSqliteCreateTable :: SqliteSyntax }
data SqliteTableOptionsSyntax = SqliteTableOptionsSyntax SqliteSyntax SqliteSyntax
newtype SqliteColumnSchemaSyntax = SqliteColumnSchemaSyntax { fromSqliteColumnSchema :: SqliteSyntax }
newtype SqliteDataTypeSyntax = SqliteDataTypeSyntax { fromSqliteDataType :: SqliteSyntax }
newtype SqliteColumnConstraintDefinitionSyntax = SqliteColumnConstraintDefinitionSyntax { fromSqliteColumnConstraintDefinition :: SqliteSyntax }
newtype SqliteColumnConstraintSyntax = SqliteColumnConstraintSyntax { fromSqliteColumnConstraint :: SqliteSyntax }
newtype SqliteTableConstraintSyntax = SqliteTableConstraintSyntax { fromSqliteTableConstraint :: SqliteSyntax }
newtype SqliteMatchTypeSyntax = SqliteMatchTypeSyntax { fromSqliteMatchType :: SqliteSyntax }
newtype SqliteReferentialActionSyntax = SqliteReferentialActionSyntax { fromSqliteReferentialAction :: SqliteSyntax }

instance IsSql92Syntax SqliteCommandSyntax where
  type Sql92SelectSyntax SqliteCommandSyntax = SqliteSelectSyntax
  type Sql92InsertSyntax SqliteCommandSyntax = SqliteInsertSyntax
  type Sql92UpdateSyntax SqliteCommandSyntax = SqliteUpdateSyntax
  type Sql92DeleteSyntax SqliteCommandSyntax = SqliteDeleteSyntax

  selectCmd = SqliteCommandSyntax . fromSqliteSelect
  insertCmd = SqliteCommandSyntax . fromSqliteInsert
  updateCmd = SqliteCommandSyntax . fromSqliteUpdate
  deleteCmd = SqliteCommandSyntax . fromSqliteDelete

instance IsSql92SelectSyntax SqliteSelectSyntax where
  type Sql92SelectSelectTableSyntax SqliteSelectSyntax = SqliteSelectTableSyntax
  type Sql92SelectOrderingSyntax SqliteSelectSyntax = SqliteOrderingSyntax

  selectStmt tbl ordering limit offset =
    SqliteSelectSyntax $
    fromSqliteSelectTable tbl <>
    (case ordering of
       [] -> mempty
       _ -> emit " ORDER BY " <> commas (coerce ordering)) <>
    maybe mempty ((emit " LIMIT " <>) . emit') limit <>
    maybe mempty ((emit " OFFSET " <>) . emit') offset

instance IsSql92SelectTableSyntax SqliteSelectTableSyntax where
  type Sql92SelectTableSelectSyntax SqliteSelectTableSyntax = SqliteSelectSyntax
  type Sql92SelectTableExpressionSyntax SqliteSelectTableSyntax = SqliteExpressionSyntax
  type Sql92SelectTableProjectionSyntax SqliteSelectTableSyntax = SqliteProjectionSyntax
  type Sql92SelectTableFromSyntax SqliteSelectTableSyntax = SqliteFromSyntax
  type Sql92SelectTableGroupingSyntax SqliteSelectTableSyntax = SqliteGroupingSyntax

  selectTableStmt proj from where_ grouping having =
    SqliteSelectTableSyntax $
    emit "SELECT " <> fromSqliteProjection proj <>
    maybe mempty (emit " FROM " <>) (fromSqliteFromSyntax <$> from) <>
    maybe mempty (emit " WHERE " <>) (fromSqliteExpression <$> where_) <>
    maybe mempty (emit " GROUP BY " <>) (fromSqliteGrouping <$> grouping) <>
    maybe mempty (emit " HAVING " <>) (fromSqliteExpression <$> having)

  unionTables all = tableOp (if all then "UNION ALL" else "UNION")
  intersectTables all = tableOp (if all then "INTERSECT ALL" else "INTERSECT")
  exceptTable all = tableOp (if all then "EXCEPT ALL" else "EXCEPT")

tableOp :: ByteString -> SqliteSelectTableSyntax -> SqliteSelectTableSyntax -> SqliteSelectTableSyntax
tableOp op a b =
  SqliteSelectTableSyntax $
  fromSqliteSelectTable a <> spaces (emit op) <> fromSqliteSelectTable b

instance IsSql92FromSyntax SqliteFromSyntax where
  type Sql92FromExpressionSyntax SqliteFromSyntax = SqliteExpressionSyntax
  type Sql92FromTableSourceSyntax SqliteFromSyntax = SqliteTableSourceSyntax

  fromTable tableSrc Nothing = SqliteFromSyntax (fromSqliteTableSource tableSrc)
  fromTable tableSrc (Just nm) =
    SqliteFromSyntax (fromSqliteTableSource tableSrc <> emit " AS " <> quotedIdentifier nm)

  innerJoin = _join "INNER JOIN"
  leftJoin = _join "LEFT JOIN"
  rightJoin = _join "RIGHT JOIN"

_join :: ByteString -> SqliteFromSyntax -> SqliteFromSyntax -> Maybe SqliteExpressionSyntax -> SqliteFromSyntax
_join joinType a b Nothing =
  SqliteFromSyntax (fromSqliteFromSyntax a <> spaces (emit joinType) <> fromSqliteFromSyntax b)
_join joinType a b (Just on) =
  SqliteFromSyntax (fromSqliteFromSyntax a <> spaces (emit joinType) <> fromSqliteFromSyntax b <> emit " ON " <> fromSqliteExpression on)

instance IsSql92ProjectionSyntax SqliteProjectionSyntax where
  type Sql92ProjectionExpressionSyntax SqliteProjectionSyntax = SqliteExpressionSyntax

  projExprs exprs =
    SqliteProjectionSyntax $
    commas (map (\(expr, nm) -> fromSqliteExpression expr <>
                                maybe mempty (\nm -> emit " AS " <> quotedIdentifier nm) nm) exprs)

instance IsSql92FieldNameSyntax SqliteFieldNameSyntax where
  qualifiedField a b =
    SqliteFieldNameSyntax $
    quotedIdentifier a <> emit "." <> quotedIdentifier b
  unqualifiedField a =
    SqliteFieldNameSyntax $
    quotedIdentifier a

instance IsSql92TableSourceSyntax SqliteTableSourceSyntax where
  type Sql92TableSourceSelectSyntax SqliteTableSourceSyntax = SqliteSelectSyntax

  tableNamed = SqliteTableSourceSyntax . quotedIdentifier
  tableFromSubSelect s =
    SqliteTableSourceSyntax (parens (fromSqliteSelect s))

instance IsSql92GroupingSyntax SqliteGroupingSyntax where
  type Sql92GroupingExpressionSyntax SqliteGroupingSyntax = SqliteExpressionSyntax

  groupByExpressions es =
    SqliteGroupingSyntax $
    commas (map fromSqliteExpression es)

instance IsSql92OrderingSyntax SqliteOrderingSyntax where
  type Sql92OrderingExpressionSyntax SqliteOrderingSyntax = SqliteExpressionSyntax

  ascOrdering e = SqliteOrderingSyntax (fromSqliteExpression e <> emit " ASC")
  descOrdering e = SqliteOrderingSyntax (fromSqliteExpression e <> emit " DESC")

instance IsSqlExpressionSyntaxStringType SqliteExpressionSyntax T.Text
instance IsSqlExpressionSyntaxStringType SqliteExpressionSyntax String
instance HasSqlValueSyntax SqliteValueSyntax Int where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Int8 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Int16 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Int32 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Int64 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Bool where
  sqlValueSyntax = sqlValueSyntax . (\b -> if b then 1 else 0 :: Int)
instance HasSqlValueSyntax SqliteValueSyntax SqlNull where
  sqlValueSyntax _ = SqliteValueSyntax (emit "NULL")
instance HasSqlValueSyntax SqliteValueSyntax String where
  sqlValueSyntax s = SqliteValueSyntax (emitValue (SQLText (fromString s)))
instance HasSqlValueSyntax SqliteValueSyntax T.Text where
  sqlValueSyntax s = SqliteValueSyntax (emitValue (SQLText s))
instance HasSqlValueSyntax SqliteValueSyntax x =>
  HasSqlValueSyntax SqliteValueSyntax (Maybe x) where
  sqlValueSyntax (Just x) = sqlValueSyntax x
  sqlValueSyntax Nothing = sqlValueSyntax SqlNull
instance HasSqlValueSyntax SqliteValueSyntax (Maybe x) => HasSqlValueSyntax SqliteValueSyntax (Auto x) where
  sqlValueSyntax (Auto x) = sqlValueSyntax x

instance IsCustomSqlSyntax SqliteExpressionSyntax where
  newtype CustomSqlSyntax SqliteExpressionSyntax =
    SqliteCustomExpressionSyntax { fromSqliteCustomExpression :: SqliteSyntax }
    deriving Monoid

  customExprSyntax = SqliteExpressionSyntax . fromSqliteCustomExpression
  renderSyntax = SqliteCustomExpressionSyntax . fromSqliteExpression
instance IsString (CustomSqlSyntax SqliteExpressionSyntax) where
  fromString = SqliteCustomExpressionSyntax . emit . fromString

instance IsSql92QuantifierSyntax SqliteComparisonQuantifierSyntax where
  quantifyOverAll = SqliteComparisonQuantifierSyntax (emit "ALL")
  quantifyOverAny = SqliteComparisonQuantifierSyntax (emit "ANY")

instance IsSql92ExpressionSyntax SqliteExpressionSyntax where
  type Sql92ExpressionValueSyntax SqliteExpressionSyntax = SqliteValueSyntax
  type Sql92ExpressionSelectSyntax SqliteExpressionSyntax = SqliteSelectSyntax
  type Sql92ExpressionFieldNameSyntax SqliteExpressionSyntax = SqliteFieldNameSyntax
  type Sql92ExpressionQuantifierSyntax SqliteExpressionSyntax = SqliteComparisonQuantifierSyntax
  type Sql92ExpressionCastTargetSyntax SqliteExpressionSyntax = SqliteDataTypeSyntax
  type Sql92ExpressionExtractFieldSyntax SqliteExpressionSyntax = SqliteExtractFieldSyntax

  addE = binOp "+"; subE = binOp "-"; mulE = binOp "*"; divE = binOp "/"
  modE = binOp "%"; orE = binOp "OR"; andE = binOp "AND"; likeE = binOp "LIKE"
  overlapsE = binOp "OVERLAPS"

  eqE = compOp "="; neqE = compOp "<>"; ltE = compOp "<"; gtE = compOp ">"
  leE = compOp "<="; geE = compOp ">="

  negateE = unOp "-"; notE = unOp "NOT"

  isNotNullE = postFix "IS NOT NULL"; isNullE = postFix "IS NULL"
  isTrueE = postFix "IS TRUE"; isNotTrueE = postFix "IS NOT TRUE"
  isFalseE = postFix "IS FALSE"; isNotFalseE = postFix "IS NOT FALSE"
  isUnknownE = postFix "IS UNKNOWN"; isNotUnknownE = postFix "IS NOT UNKNOWN"

  existsE select = SqliteExpressionSyntax (emit "EXISTS " <> parens (fromSqliteSelect select))
  uniqueE select = SqliteExpressionSyntax (emit "UNIQUE " <> parens (fromSqliteSelect select))

  betweenE a b c = SqliteExpressionSyntax (parens (fromSqliteExpression a) <>
                                           emit " BETWEEN " <>
                                           parens (fromSqliteExpression b) <>
                                           emit " AND " <>
                                           parens (fromSqliteExpression c))

  valueE = SqliteExpressionSyntax . fromSqliteValue

  rowE vs = SqliteExpressionSyntax (parens (commas (map fromSqliteExpression vs)))
  fieldE = SqliteExpressionSyntax . fromSqliteFieldNameSyntax

  subqueryE = SqliteExpressionSyntax . parens . fromSqliteSelect

  positionE needle haystack =
    SqliteExpressionSyntax $
    emit "POSITION" <> parens (parens (fromSqliteExpression needle) <> emit " IN " <> parens (fromSqliteExpression haystack))
  nullIfE a b =
    SqliteExpressionSyntax $
    emit "NULLIF" <> parens (fromSqliteExpression a <> emit ", " <> fromSqliteExpression b)
  absE x = SqliteExpressionSyntax (emit "ABS" <> parens (fromSqliteExpression x))
  bitLengthE x = SqliteExpressionSyntax (emit "BIT_LENGTH" <> parens (fromSqliteExpression x))
  charLengthE x = SqliteExpressionSyntax (emit "CHAR_LENGTH" <> parens (fromSqliteExpression x))
  octetLengthE x = SqliteExpressionSyntax (emit "OCTET_LENGTH" <> parens (fromSqliteExpression x))
  coalesceE es = SqliteExpressionSyntax (emit "COALESCE" <> parens (commas (map fromSqliteExpression es)))
  extractE field from =
    SqliteExpressionSyntax $
    emit "EXTRACT" <> parens (fromSqliteExtractField field <> emit " FROM " <> parens (fromSqliteExpression from))
  castE e t = SqliteExpressionSyntax (emit "CAST" <> parens (parens (fromSqliteExpression e) <> emit " TO " <> fromSqliteDataType t))
  caseE cases else_ =
    SqliteExpressionSyntax $
    emit "CASE " <>
    foldMap (\(cond, res) -> emit "WHEN " <> fromSqliteExpression cond <> emit " THEN " <> fromSqliteExpression res <> emit " ") cases <>
    emit "ELSE " <> fromSqliteExpression else_ <> emit " END"

  currentTimestampE = SqliteExpressionSyntax (emit "CURRENT_TIMESTAMP")

binOp :: ByteString -> SqliteExpressionSyntax -> SqliteExpressionSyntax -> SqliteExpressionSyntax
binOp op a b =
  SqliteExpressionSyntax $
  parens (fromSqliteExpression a) <> emit " " <> emit op <> emit " " <> parens (fromSqliteExpression b)

compOp :: ByteString -> Maybe SqliteComparisonQuantifierSyntax
       -> SqliteExpressionSyntax -> SqliteExpressionSyntax
       -> SqliteExpressionSyntax
compOp op quantifier a b =
  SqliteExpressionSyntax $
  parens (fromSqliteExpression a) <>
  emit op <>
  parens (maybe mempty (\q -> emit " " <> fromSqliteComparisonQuantifier q <> emit " ") quantifier <>
          fromSqliteExpression b)

unOp, postFix :: ByteString -> SqliteExpressionSyntax -> SqliteExpressionSyntax
unOp op a =
  SqliteExpressionSyntax (emit op <> parens (fromSqliteExpression a))
postFix op a =
  SqliteExpressionSyntax (parens (fromSqliteExpression a) <> emit " " <> emit op)

instance IsSql92AggregationExpressionSyntax SqliteExpressionSyntax where
  type Sql92AggregationSetQuantifierSyntax SqliteExpressionSyntax = SqliteAggregationSetQuantifierSyntax

  countAllE = SqliteExpressionSyntax (emit "COUNT(*)")
  countE = unAgg "COUNT"
  sumE = unAgg "SUM"
  avgE = unAgg "AVG"
  minE = unAgg "MIN"
  maxE = unAgg "MAX"

unAgg :: ByteString -> Maybe SqliteAggregationSetQuantifierSyntax -> SqliteExpressionSyntax
      -> SqliteExpressionSyntax
unAgg fn q e =
  SqliteExpressionSyntax $
  emit fn <> parens (maybe mempty (\q -> fromSqliteAggregationSetQuantifier q <> emit " ") q <>
                     fromSqliteExpression e)

instance IsSql92AggregationSetQuantifierSyntax SqliteAggregationSetQuantifierSyntax where
  setQuantifierDistinct = SqliteAggregationSetQuantifierSyntax (emit "DISTINCT")
  setQuantifierAll = SqliteAggregationSetQuantifierSyntax (emit "ALL")

instance IsSql92InsertSyntax SqliteInsertSyntax where
  type Sql92InsertValuesSyntax SqliteInsertSyntax = SqliteInsertValuesSyntax

  insertStmt tblName fields values =
    SqliteInsertSyntax $
    emit "INSERT INTO " <> quotedIdentifier tblName <> parens (commas (map quotedIdentifier fields)) <> emit " " <>
    fromSqliteInsertValues values

instance IsSql92InsertValuesSyntax SqliteInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax SqliteInsertValuesSyntax = SqliteExpressionSyntax
  type Sql92InsertValuesSelectSyntax SqliteInsertValuesSyntax = SqliteSelectSyntax

  insertSqlExpressions es =
    SqliteInsertValuesSyntax $
    emit "VALUES " <>
    commas (map (parens . commas . map fromSqliteExpression) es)
  insertFromSql (SqliteSelectSyntax a) = SqliteInsertValuesSyntax a

instance IsSql92UpdateSyntax SqliteUpdateSyntax where
  type Sql92UpdateFieldNameSyntax SqliteUpdateSyntax = SqliteFieldNameSyntax
  type Sql92UpdateExpressionSyntax SqliteUpdateSyntax = SqliteExpressionSyntax

  updateStmt tbl fields where_ =
    SqliteUpdateSyntax $
    emit "UPDATE " <> quotedIdentifier tbl <>
    (case fields of
       [] -> mempty
       _ -> emit " SET " <>
            commas (map (\(field, val) -> fromSqliteFieldNameSyntax field <> emit "=" <> fromSqliteExpression val) fields)) <>
    maybe mempty (\where_ -> emit " WHERE " <> fromSqliteExpression where_) where_

instance IsSql92DeleteSyntax SqliteDeleteSyntax where
  type Sql92DeleteExpressionSyntax SqliteDeleteSyntax = SqliteExpressionSyntax

  deleteStmt tbl where_ =
    SqliteDeleteSyntax $
    emit "DELETE FROM " <> quotedIdentifier tbl <>
    maybe mempty (\where_ -> emit " WHERE " <> fromSqliteExpression where_) where_

spaces, parens :: SqliteSyntax -> SqliteSyntax
spaces a = emit " " <> a <> emit " " 
parens a = emit "(" <> a <> emit ")"

commas :: [SqliteSyntax] -> SqliteSyntax
commas [] = mempty
commas [x] = x
commas (x:xs) = x <> foldMap (emit ", " <>) xs
