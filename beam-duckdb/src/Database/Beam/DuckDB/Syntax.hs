{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- TODO: clean up unused top binds
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Database.Beam.DuckDB.Syntax (
  -- * Command
  DuckDBCommandSyntax (..),

  -- * Concrete syntaxes
  DuckDBSelectSyntax (..),
  DuckDBFromSyntax (..),
  DuckDBExpressionSyntax (..),
  DuckDBInsertSyntax (..),
  DuckDBUpdateSyntax (..),
  DuckDBDeleteSyntax (..),
  DuckDBTableNameSyntax (..),
  DuckDBOnConflictSyntax (..),
)
where

import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Beam.Backend (
  HasSqlValueSyntax (..),
  IsSql92AggregationExpressionSyntax (..),
  IsSql92AggregationSetQuantifierSyntax (..),
  IsSql92DataTypeSyntax (..),
  IsSql92DeleteSyntax (..),
  IsSql92ExpressionSyntax (..),
  IsSql92ExtractFieldSyntax (..),
  IsSql92FieldNameSyntax (..),
  IsSql92FromOuterJoinSyntax (..),
  IsSql92FromSyntax (..),
  IsSql92GroupingSyntax (..),
  IsSql92InsertSyntax (..),
  IsSql92InsertValuesSyntax (..),
  IsSql92OrderingSyntax (..),
  IsSql92ProjectionSyntax (..),
  IsSql92QuantifierSyntax (..),
  IsSql92SelectSyntax (..),
  IsSql92SelectTableSyntax (..),
  IsSql92Syntax (..),
  IsSql92TableNameSyntax (..),
  IsSql92TableSourceSyntax (..),
  IsSql92UpdateSyntax (..),
  SqlNull (..),
 )
import Database.Beam.DuckDB.Syntax.Builder (DuckDBSyntax, commas, emit, emitIntegral, emitRealFloat, emitValue, parens, quotedIdentifier, spaces)
import Database.Beam.Migrate.Serialization (BeamSerializedDataType)
import Database.DuckDB.Simple (Null (Null), ToField)

newtype DuckDBCommandSyntax = DuckDBCommandSyntax {fromDuckDBSyntax :: DuckDBSyntax}

--
instance IsSql92Syntax DuckDBCommandSyntax where
  type Sql92SelectSyntax DuckDBCommandSyntax = DuckDBSelectSyntax
  type Sql92InsertSyntax DuckDBCommandSyntax = DuckDBInsertSyntax
  type Sql92UpdateSyntax DuckDBCommandSyntax = DuckDBUpdateSyntax
  type Sql92DeleteSyntax DuckDBCommandSyntax = DuckDBDeleteSyntax

  selectCmd = DuckDBCommandSyntax . fromDuckDBSelect
  insertCmd = DuckDBCommandSyntax . fromDuckDBInsert
  updateCmd = DuckDBCommandSyntax . fromDuckDBUpdate
  deleteCmd = DuckDBCommandSyntax . fromDuckDBDelete

newtype DuckDBTableNameSyntax = DuckDBTableNameSyntax {fromDuckDBTableName :: DuckDBSyntax}

-- | DuckDB @SELECT@ syntax
newtype DuckDBSelectSyntax = DuckDBSelectSyntax {fromDuckDBSelect :: DuckDBSyntax}

newtype DuckDBSelectTableSyntax = DuckDBSelectTableSyntax {fromDuckDBSelectTable :: DuckDBSyntax}

data DuckDBOrderingSyntax = DuckDBOrderingSyntax
  { duckDBOrdering :: DuckDBSyntax
  , -- DuckDB re-uses the Postgres parser, and therefore
    -- has the same null ordering properties
    duckDBNullOrdering :: Maybe DuckDBNullOrdering
  }

data DuckDBNullOrdering
  = DuckDBNullOrderingNullsFirst
  | DuckDBNullOrderingNullsLast
  deriving (Show, Eq)

newtype DuckDBExpressionSyntax = DuckDBExpressionSyntax {fromDuckDBExpression :: DuckDBSyntax} deriving (Eq)

newtype DuckDBTableSourceSyntax = DuckDBTableSourceSyntax {fromDuckDBTableSource :: DuckDBSyntax}

newtype DuckDBProjectionSyntax = DuckDBProjectionSyntax {fromDuckDBProjection :: DuckDBSyntax}

newtype DuckDBFromSyntax = DuckDBFromSyntax {fromDuckDBFrom :: DuckDBSyntax}

newtype DuckDBValueSyntax = DuckDBValueSyntax {fromDuckDBValue :: DuckDBSyntax}

newtype DuckDBFieldNameSyntax = DuckDBFieldNameSyntax {fromDuckDBFieldName :: DuckDBSyntax}

newtype DuckDBComparisonQuantifierSyntax = DuckDBComparisonQuantifierSyntax {fromDuckDBComparisonQuantifier :: DuckDBSyntax}

data DuckDBDataTypeSyntax = DuckDBDataTypeSyntax
  { duckDBDataType :: DuckDBSyntax
  , duckDBDataTypeSerialized :: BeamSerializedDataType
  }

newtype DuckDBExtractFieldSyntax = DuckDBExtractFieldSyntax {fromDuckDBExtractField :: DuckDBSyntax}

newtype DuckDBGroupingSyntax = DuckDBGroupingSyntax {fromDuckDBGrouping :: DuckDBSyntax}

newtype DuckDBSelectSetQuantifierSyntax = DuckDBSelectSetQuantifierSyntax {fromDuckDBSelectSetQuantifier :: DuckDBSyntax}

newtype DuckDBAggregationSetQuantifierSyntax = DuckDBAggregationSetQuantifierSyntax {fromDuckDBAggregationSetQuantifier :: DuckDBSyntax}

instance IsSql92AggregationExpressionSyntax DuckDBExpressionSyntax where
  type Sql92AggregationSetQuantifierSyntax DuckDBExpressionSyntax = DuckDBAggregationSetQuantifierSyntax

  countAllE = DuckDBExpressionSyntax (emit "COUNT(*)")
  countE = aggFunc "COUNT"
  avgE = aggFunc "AVG"
  sumE = aggFunc "SUM"
  minE = aggFunc "MIN"
  maxE = aggFunc "MAX"

aggFunc :: Text -> Maybe DuckDBAggregationSetQuantifierSyntax -> DuckDBExpressionSyntax -> DuckDBExpressionSyntax
aggFunc fn q e =
  DuckDBExpressionSyntax $
    emit fn <> emit "(" <> maybe mempty (\qInner -> fromDuckDBAggregationSetQuantifier qInner <> emit " ") q <> fromDuckDBExpression e <> emit ")"

instance IsSql92SelectTableSyntax DuckDBSelectTableSyntax where
  type Sql92SelectTableSelectSyntax DuckDBSelectTableSyntax = DuckDBSelectSyntax
  type Sql92SelectTableExpressionSyntax DuckDBSelectTableSyntax = DuckDBExpressionSyntax
  type Sql92SelectTableProjectionSyntax DuckDBSelectTableSyntax = DuckDBProjectionSyntax
  type Sql92SelectTableFromSyntax DuckDBSelectTableSyntax = DuckDBFromSyntax
  type Sql92SelectTableGroupingSyntax DuckDBSelectTableSyntax = DuckDBGroupingSyntax
  type Sql92SelectTableSetQuantifierSyntax DuckDBSelectTableSyntax = DuckDBAggregationSetQuantifierSyntax

  selectTableStmt setQuantifier proj from where_ grouping having =
    DuckDBSelectTableSyntax $
      mconcat
        [ emit "SELECT "
        , maybe mempty ((<> emit " ") . fromDuckDBAggregationSetQuantifier) setQuantifier
        , fromDuckDBProjection proj
        , maybe mempty ((emit " FROM " <>) . fromDuckDBFrom) from
        , maybe mempty ((emit " WHERE " <>) . fromDuckDBExpression) where_
        , maybe mempty ((emit " GROUP BY " <>) . fromDuckDBGrouping) grouping
        , maybe mempty ((emit " HAVING " <>) . fromDuckDBExpression) having
        ]

  unionTables unionAll = tableOp (if unionAll then "UNION ALL" else "UNION")
  intersectTables intersectAcc = tableOp (if intersectAcc then "INTERSECT ALL" else "INTERSECT")
  exceptTable exceptAll = tableOp (if exceptAll then "EXCEPT ALL" else "EXCEPT")

tableOp :: Text -> DuckDBSelectTableSyntax -> DuckDBSelectTableSyntax -> DuckDBSelectTableSyntax
tableOp op a b =
  DuckDBSelectTableSyntax $
    fromDuckDBSelectTable a <> spaces (emit op) <> fromDuckDBSelectTable b

instance IsSql92QuantifierSyntax DuckDBComparisonQuantifierSyntax where
  quantifyOverAll = DuckDBComparisonQuantifierSyntax (emit "ALL")
  quantifyOverAny = DuckDBComparisonQuantifierSyntax (emit "ANY")

instance IsSql92ExtractFieldSyntax DuckDBExtractFieldSyntax where
  secondsField = DuckDBExtractFieldSyntax (emit "SECOND")
  minutesField = DuckDBExtractFieldSyntax (emit "MINUTE")
  hourField = DuckDBExtractFieldSyntax (emit "HOUR")
  dayField = DuckDBExtractFieldSyntax (emit "DAY")
  monthField = DuckDBExtractFieldSyntax (emit "MONTH")
  yearField = DuckDBExtractFieldSyntax (emit "YEAR")

instance IsSql92AggregationSetQuantifierSyntax DuckDBAggregationSetQuantifierSyntax where
  setQuantifierDistinct = DuckDBAggregationSetQuantifierSyntax $ emit "DISTINCT"
  setQuantifierAll = DuckDBAggregationSetQuantifierSyntax $ emit "ALL"

instance IsSql92AggregationSetQuantifierSyntax DuckDBSelectSetQuantifierSyntax where
  setQuantifierDistinct = DuckDBSelectSetQuantifierSyntax $ emit "DISTINCT"
  setQuantifierAll = DuckDBSelectSetQuantifierSyntax $ emit "ALL"

instance IsSql92GroupingSyntax DuckDBGroupingSyntax where
  type Sql92GroupingExpressionSyntax DuckDBGroupingSyntax = DuckDBExpressionSyntax

  groupByExpressions es =
    DuckDBGroupingSyntax $
      commas (map fromDuckDBExpression es)

instance IsSql92TableNameSyntax DuckDBTableNameSyntax where
  tableName Nothing t = DuckDBTableNameSyntax (quotedIdentifier t)
  tableName (Just s) t = DuckDBTableNameSyntax (quotedIdentifier s <> emit "." <> quotedIdentifier t)

instance IsSql92TableSourceSyntax DuckDBTableSourceSyntax where
  type Sql92TableSourceSelectSyntax DuckDBTableSourceSyntax = DuckDBSelectSyntax
  type Sql92TableSourceExpressionSyntax DuckDBTableSourceSyntax = DuckDBExpressionSyntax
  type Sql92TableSourceTableNameSyntax DuckDBTableSourceSyntax = DuckDBTableNameSyntax

  tableNamed = DuckDBTableSourceSyntax . fromDuckDBTableName
  tableFromSubSelect s = DuckDBTableSourceSyntax $ emit "(" <> fromDuckDBSelect s <> emit ")"
  tableFromValues vss =
    DuckDBTableSourceSyntax . parens $
      emit "VALUES "
        <> commas
          ( map
              (parens . commas . map fromDuckDBExpression)
              vss
          )

instance IsSql92FromSyntax DuckDBFromSyntax where
  type Sql92FromExpressionSyntax DuckDBFromSyntax = DuckDBExpressionSyntax
  type Sql92FromTableSourceSyntax DuckDBFromSyntax = DuckDBTableSourceSyntax

  fromTable tableSrc Nothing = coerce tableSrc
  fromTable tableSrc (Just (nm, colNms)) =
    DuckDBFromSyntax $
      coerce tableSrc
        <> emit " AS "
        <> quotedIdentifier nm
        <> maybe mempty (parens . commas . map quotedIdentifier) colNms

  innerJoin a b Nothing = DuckDBFromSyntax (fromDuckDBFrom a <> emit " CROSS JOIN " <> fromDuckDBFrom b)
  innerJoin a b (Just e) = join "INNER JOIN" a b (Just e)

  leftJoin = join "LEFT JOIN"
  rightJoin = join "RIGHT JOIN"

join :: Text -> DuckDBFromSyntax -> DuckDBFromSyntax -> Maybe DuckDBExpressionSyntax -> DuckDBFromSyntax
join joinType a b Nothing =
  DuckDBFromSyntax $
    fromDuckDBFrom a <> emit (" " <> joinType <> " ") <> fromDuckDBFrom b <> emit " ON TRUE"
join joinType a b (Just on) =
  DuckDBFromSyntax $
    fromDuckDBFrom a
      <> emit (" " <> joinType <> " ")
      <> fromDuckDBFrom b
      <> emit " ON "
      <> fromDuckDBExpression on

instance IsSql92FromOuterJoinSyntax DuckDBFromSyntax where
  outerJoin = join "FULL OUTER JOIN"

instance IsSql92FieldNameSyntax DuckDBFieldNameSyntax where
  qualifiedField a b =
    DuckDBFieldNameSyntax $
      quotedIdentifier a <> emit "." <> quotedIdentifier b
  unqualifiedField = DuckDBFieldNameSyntax . quotedIdentifier

instance IsSql92DataTypeSyntax DuckDBDataTypeSyntax where
  domainType nm =
    DuckDBDataTypeSyntax
      (quotedIdentifier nm)
      (domainType nm)

  charType prec charSet =
    DuckDBDataTypeSyntax
      (emit "CHAR" <> optPrec prec <> optCharSet charSet)
      (charType prec charSet)
  varCharType prec charSet =
    DuckDBDataTypeSyntax
      (emit "VARCHAR" <> optPrec prec <> optCharSet charSet)
      (varCharType prec charSet)
  nationalCharType prec =
    DuckDBDataTypeSyntax
      (emit "NATIONAL CHAR" <> optPrec prec)
      (nationalCharType prec)
  nationalVarCharType prec =
    DuckDBDataTypeSyntax
      (emit "NATIONAL CHARACTER VARYING" <> optPrec prec)
      (nationalVarCharType prec)

  bitType prec =
    DuckDBDataTypeSyntax
      (emit "BIT" <> optPrec prec)
      (bitType prec)
  varBitType prec =
    DuckDBDataTypeSyntax
      (emit "BIT VARYING" <> optPrec prec)
      (varBitType prec)

  numericType prec =
    DuckDBDataTypeSyntax
      (emit "NUMERIC" <> optNumericPrec prec)
      (numericType prec)
  decimalType prec =
    DuckDBDataTypeSyntax
      (emit "DECIMAL" <> optNumericPrec prec)
      (decimalType prec)

  intType = DuckDBDataTypeSyntax (emit "INT") intType
  smallIntType = DuckDBDataTypeSyntax (emit "SMALLINT") smallIntType

  floatType prec =
    DuckDBDataTypeSyntax
      (emit "FLOAT" <> optPrec prec)
      (floatType prec)
  doubleType = DuckDBDataTypeSyntax (emit "DOUBLE PRECISION") doubleType
  realType = DuckDBDataTypeSyntax (emit "REAL") realType
  dateType = DuckDBDataTypeSyntax (emit "DATE") dateType
  timeType prec withTz =
    DuckDBDataTypeSyntax
      (emit "TIME" <> optPrec prec <> if withTz then emit " WITH TIME ZONE" else mempty)
      (timeType prec withTz)
  timestampType prec withTz =
    DuckDBDataTypeSyntax
      (emit "TIMESTAMP" <> optPrec prec <> if withTz then emit " WITH TIME ZONE" else mempty)
      (timestampType prec withTz)

optPrec :: Maybe Word -> DuckDBSyntax
optPrec Nothing = mempty
optPrec (Just x) = emit "(" <> emit (fromString (show x)) <> emit ")"

optCharSet :: Maybe Text -> DuckDBSyntax
optCharSet Nothing = mempty
optCharSet (Just cs) = emit " CHARACTER SET " <> emit cs

optNumericPrec :: Maybe (Word, Maybe Word) -> DuckDBSyntax
optNumericPrec Nothing = mempty
optNumericPrec (Just (prec, Nothing)) = optPrec (Just prec)
optNumericPrec (Just (prec, Just dec)) = emit "(" <> emit (fromString (show prec)) <> emit ", " <> emit (fromString (show dec)) <> emit ")"

instance HasSqlValueSyntax DuckDBValueSyntax Int8 where
  sqlValueSyntax i = DuckDBValueSyntax (emitIntegral i)

instance HasSqlValueSyntax DuckDBValueSyntax Int16 where
  sqlValueSyntax i = DuckDBValueSyntax (emitIntegral i)

instance HasSqlValueSyntax DuckDBValueSyntax Int32 where
  sqlValueSyntax i = DuckDBValueSyntax (emitIntegral i)

instance HasSqlValueSyntax DuckDBValueSyntax Int64 where
  sqlValueSyntax i = DuckDBValueSyntax (emitIntegral i)

instance HasSqlValueSyntax DuckDBValueSyntax Word8 where
  sqlValueSyntax i = DuckDBValueSyntax (emitIntegral i)

instance HasSqlValueSyntax DuckDBValueSyntax Word16 where
  sqlValueSyntax i = DuckDBValueSyntax (emitIntegral i)

instance HasSqlValueSyntax DuckDBValueSyntax Word32 where
  sqlValueSyntax i = DuckDBValueSyntax (emitIntegral i)

instance HasSqlValueSyntax DuckDBValueSyntax Word64 where
  sqlValueSyntax i = DuckDBValueSyntax (emitIntegral i)

instance HasSqlValueSyntax DuckDBValueSyntax Float where
  sqlValueSyntax f = DuckDBValueSyntax (emitRealFloat f)

instance HasSqlValueSyntax DuckDBValueSyntax Double where
  sqlValueSyntax f = DuckDBValueSyntax (emitRealFloat f)

instance HasSqlValueSyntax DuckDBValueSyntax Bool where
  sqlValueSyntax = DuckDBValueSyntax . emitValue

instance HasSqlValueSyntax DuckDBValueSyntax SqlNull where
  sqlValueSyntax _ = DuckDBValueSyntax (emitValue Null)

instance HasSqlValueSyntax DuckDBValueSyntax String where
  sqlValueSyntax = sqlValueSyntax . Text.pack

instance HasSqlValueSyntax DuckDBValueSyntax Text where
  sqlValueSyntax = DuckDBValueSyntax . emitValue

instance {-# OVERLAPPABLE #-} (ToField a, Eq a) => HasSqlValueSyntax DuckDBValueSyntax a where
  sqlValueSyntax = DuckDBValueSyntax . emitValue

instance (HasSqlValueSyntax DuckDBValueSyntax x) => HasSqlValueSyntax DuckDBValueSyntax (Maybe x) where
  sqlValueSyntax (Just x) = sqlValueSyntax x
  sqlValueSyntax Nothing = sqlValueSyntax SqlNull

instance IsSql92ExpressionSyntax DuckDBExpressionSyntax where
  type Sql92ExpressionValueSyntax DuckDBExpressionSyntax = DuckDBValueSyntax
  type Sql92ExpressionSelectSyntax DuckDBExpressionSyntax = DuckDBSelectSyntax
  type Sql92ExpressionFieldNameSyntax DuckDBExpressionSyntax = DuckDBFieldNameSyntax
  type Sql92ExpressionQuantifierSyntax DuckDBExpressionSyntax = DuckDBComparisonQuantifierSyntax
  type Sql92ExpressionCastTargetSyntax DuckDBExpressionSyntax = DuckDBDataTypeSyntax
  type Sql92ExpressionExtractFieldSyntax DuckDBExpressionSyntax = DuckDBExtractFieldSyntax

  addE = binOp "+"
  subE = binOp "-"
  mulE = binOp "*"
  divE = binOp "/"
  modE = binOp "%"
  orE = binOp "OR"
  andE = binOp "AND"
  likeE = binOp "LIKE"
  overlapsE = binOp "OVERLAPS"

  eqE = compOp "="
  neqE = compOp "<>"
  ltE = compOp "<"
  gtE = compOp ">"
  leE = compOp "<="
  geE = compOp ">="

  negateE = unOp "-"
  notE = unOp "NOT"

  isNotNullE = postFix "IS NOT NULL"
  isNullE = postFix "IS NULL"
  isTrueE = postFix "IS TRUE"
  isFalseE = postFix "IS FALSE"
  isNotTrueE = postFix "IS NOT TRUE"
  isNotFalseE = postFix "IS NOT FALSE"
  isUnknownE = postFix "IS UNKNOWN"
  isNotUnknownE = postFix "IS NOT UNKNOWN"

  existsE select = DuckDBExpressionSyntax (emit "EXISTS " <> parens (fromDuckDBSelect select))
  uniqueE select = DuckDBExpressionSyntax (emit "UNIQUE " <> parens (fromDuckDBSelect select))

  betweenE a b c =
    DuckDBExpressionSyntax
      ( parens (fromDuckDBExpression a)
          <> emit " BETWEEN "
          <> parens (fromDuckDBExpression b)
          <> emit " AND "
          <> parens (fromDuckDBExpression c)
      )

  valueE = DuckDBExpressionSyntax . fromDuckDBValue

  rowE vs = DuckDBExpressionSyntax (parens (commas (map fromDuckDBExpression vs)))
  fieldE = DuckDBExpressionSyntax . fromDuckDBFieldName

  subqueryE = DuckDBExpressionSyntax . parens . fromDuckDBSelect

  positionE needle haystack =
    DuckDBExpressionSyntax $
      emit "POSITION" <> parens (parens (fromDuckDBExpression needle) <> emit " IN " <> parens (fromDuckDBExpression haystack))
  nullIfE a b =
    DuckDBExpressionSyntax $
      emit "NULLIF" <> parens (fromDuckDBExpression a <> emit ", " <> fromDuckDBExpression b)
  absE x = DuckDBExpressionSyntax (emit "ABS" <> parens (fromDuckDBExpression x))
  bitLengthE x = DuckDBExpressionSyntax (emit "8 * LENGTH" <> parens (emit "CAST" <> parens (parens (fromDuckDBExpression x) <> emit " AS BLOB")))
  charLengthE x = DuckDBExpressionSyntax (emit "LENGTH" <> parens (fromDuckDBExpression x))
  octetLengthE x = DuckDBExpressionSyntax (emit "LENGTH" <> parens (emit "CAST" <> parens (parens (fromDuckDBExpression x) <> emit " AS BLOB")))
  lowerE x = DuckDBExpressionSyntax (emit "LOWER" <> parens (fromDuckDBExpression x))
  upperE x = DuckDBExpressionSyntax (emit "UPPER" <> parens (fromDuckDBExpression x))
  trimE x = DuckDBExpressionSyntax (emit "TRIM" <> parens (fromDuckDBExpression x))
  coalesceE es = DuckDBExpressionSyntax (emit "COALESCE" <> parens (commas (map fromDuckDBExpression es)))
  extractE field from = DuckDBExpressionSyntax (emit "EXTRACT(" <> fromDuckDBExtractField field <> emit " FROM (" <> fromDuckDBExpression from <> emit "))")
  castE e t = DuckDBExpressionSyntax (emit "CAST" <> parens (parens (fromDuckDBExpression e) <> emit " AS " <> duckDBDataType t))
  caseE cases else_ =
    DuckDBExpressionSyntax $
      emit "CASE "
        <> foldMap (\(cond, res) -> emit "WHEN " <> fromDuckDBExpression cond <> emit " THEN " <> fromDuckDBExpression res <> emit " ") cases
        <> emit "ELSE "
        <> fromDuckDBExpression else_
        <> emit " END"

  currentTimestampE = DuckDBExpressionSyntax (emit "CURRENT_TIMESTAMP")

  defaultE = DuckDBExpressionSyntax (emit "DEFAULT")
  inE e es = DuckDBExpressionSyntax (parens (fromDuckDBExpression e) <> emit " IN " <> parens (commas (map fromDuckDBExpression es)))
  inSelectE e sel =
    DuckDBExpressionSyntax (parens (fromDuckDBExpression e) <> emit " IN " <> parens (fromDuckDBSelect sel))

binOp :: Text -> DuckDBExpressionSyntax -> DuckDBExpressionSyntax -> DuckDBExpressionSyntax
binOp op a b =
  DuckDBExpressionSyntax $
    parens (fromDuckDBExpression a) <> emit " " <> emit op <> emit " " <> parens (fromDuckDBExpression b)

compOp ::
  Text ->
  Maybe DuckDBComparisonQuantifierSyntax ->
  DuckDBExpressionSyntax ->
  DuckDBExpressionSyntax ->
  DuckDBExpressionSyntax
compOp op quantifier a b =
  DuckDBExpressionSyntax $
    parens (fromDuckDBExpression a)
      <> emit op
      <> maybe mempty (\q -> emit " " <> fromDuckDBComparisonQuantifier q <> emit " ") quantifier
      <> parens (fromDuckDBExpression b)

unOp, postFix :: Text -> DuckDBExpressionSyntax -> DuckDBExpressionSyntax
unOp op a =
  DuckDBExpressionSyntax (emit op <> parens (fromDuckDBExpression a))
postFix op a =
  DuckDBExpressionSyntax (parens (fromDuckDBExpression a) <> emit " " <> emit op)

instance IsSql92ProjectionSyntax DuckDBProjectionSyntax where
  type Sql92ProjectionExpressionSyntax DuckDBProjectionSyntax = DuckDBExpressionSyntax

  projExprs exprs =
    DuckDBProjectionSyntax $
      commas
        ( map
            ( \(expr, nm) ->
                fromDuckDBExpression expr
                  <> maybe mempty (\nm' -> emit " AS " <> quotedIdentifier nm') nm
            )
            exprs
        )

instance IsSql92OrderingSyntax DuckDBOrderingSyntax where
  type Sql92OrderingExpressionSyntax DuckDBOrderingSyntax = DuckDBExpressionSyntax

  ascOrdering e = DuckDBOrderingSyntax (fromDuckDBExpression e <> emit " ASC") Nothing
  descOrdering e = DuckDBOrderingSyntax (fromDuckDBExpression e <> emit " DESC") Nothing

instance IsSql92SelectSyntax DuckDBSelectSyntax where
  type Sql92SelectSelectTableSyntax DuckDBSelectSyntax = DuckDBSelectTableSyntax
  type Sql92SelectOrderingSyntax DuckDBSelectSyntax = DuckDBOrderingSyntax

  selectStmt tbl ordering limit offset =
    DuckDBSelectSyntax $
      mconcat
        [ fromDuckDBSelectTable tbl
        , case ordering of
            [] -> mempty
            ordering' -> emit " ORDER BY " <> commas (map duckDBOrdering ordering')
        , maybe mempty (emit . fromString . (" LIMIT " <>) . show) limit
        , maybe mempty (emit . fromString . (" OFFSET " <>) . show) offset
        ]

-- | DuckDB @ON CONFLICT@ syntax
newtype DuckDBOnConflictSyntax = DuckDBOnConflictSyntax {fromDuckDBOnConflict :: DuckDBSyntax}

-- | SQLite @INSERT@ syntax.
newtype DuckDBInsertSyntax = DuckDBInsertSyntax {fromDuckDBInsert :: DuckDBSyntax}

newtype DuckDBInsertValuesSyntax = DuckDBInsertValuesSyntax {fromDuckDBInsertValues :: DuckDBSyntax}

instance IsSql92InsertSyntax DuckDBInsertSyntax where
  type Sql92InsertTableNameSyntax DuckDBInsertSyntax = DuckDBTableNameSyntax
  type Sql92InsertValuesSyntax DuckDBInsertSyntax = DuckDBInsertValuesSyntax

  insertStmt tblName fields values =
    DuckDBInsertSyntax $
      emit "INSERT INTO "
        <> fromDuckDBTableName tblName
        <> emit "("
        <> commas (map quotedIdentifier fields)
        <> emit ") "
        <> fromDuckDBInsertValues values

instance IsSql92InsertValuesSyntax DuckDBInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax DuckDBInsertValuesSyntax = DuckDBExpressionSyntax
  type Sql92InsertValuesSelectSyntax DuckDBInsertValuesSyntax = DuckDBSelectSyntax

  insertSqlExpressions es =
    DuckDBInsertValuesSyntax $
      emit "VALUES "
        <> commas
          ( map
              (\values -> emit "(" <> commas (coerce values) <> emit ")")
              es
          )
  insertFromSql (DuckDBSelectSyntax a) = DuckDBInsertValuesSyntax a

-- | DuckDB @UPDATE@ syntax
newtype DuckDBUpdateSyntax = DuckDBUpdateSyntax {fromDuckDBUpdate :: DuckDBSyntax}

instance IsSql92UpdateSyntax DuckDBUpdateSyntax where
  type Sql92UpdateFieldNameSyntax DuckDBUpdateSyntax = DuckDBFieldNameSyntax
  type Sql92UpdateExpressionSyntax DuckDBUpdateSyntax = DuckDBExpressionSyntax
  type Sql92UpdateTableNameSyntax DuckDBUpdateSyntax = DuckDBTableNameSyntax

  updateStmt tbl fields where_ =
    DuckDBUpdateSyntax $
      emit "UPDATE "
        <> fromDuckDBTableName tbl
        <> ( case fields of
               [] -> mempty
               fs ->
                 emit " SET "
                   <> commas (map (\(field, val) -> fromDuckDBFieldName field <> emit "=" <> fromDuckDBExpression val) fs)
           )
        <> maybe mempty (\whereInner -> emit " WHERE " <> fromDuckDBExpression whereInner) where_

-- | DuckDB @DELETE@ syntax
newtype DuckDBDeleteSyntax = DuckDBDeleteSyntax {fromDuckDBDelete :: DuckDBSyntax}

instance IsSql92DeleteSyntax DuckDBDeleteSyntax where
  type Sql92DeleteExpressionSyntax DuckDBDeleteSyntax = DuckDBExpressionSyntax
  type Sql92DeleteTableNameSyntax DuckDBDeleteSyntax = DuckDBTableNameSyntax

  deleteStmt tbl alias where_ =
    DuckDBDeleteSyntax $
      emit "DELETE FROM "
        <> fromDuckDBTableName tbl
        <> maybe mempty (\alias_ -> emit " AS " <> quotedIdentifier alias_) alias
        <> maybe mempty (\whereInner -> emit " WHERE " <> fromDuckDBExpression whereInner) where_

  deleteSupportsAlias _ = True
