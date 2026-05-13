{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Database.Beam.DuckDB.Syntax
  ( -- * Command
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
    DuckDBFieldNameSyntax (..),

    -- * DDL syntaxes
    DuckDBDataTypeSyntax (..),
    DuckDBCreateTableSyntax (..),
    DuckDBDropTableSyntax (..),
    DuckDBAlterTableSyntax (..),
    DuckDBAlterTableActionSyntax (..),
    DuckDBAlterColumnActionSyntax (..),
    DuckDBCreateSchemaSyntax (..),
    DuckDBDropSchemaSyntax (..),
    DuckDBSchemaNameSyntax (..),
    DuckDBColumnSchemaSyntax (..),
    DuckDBColumnConstraintDefinitionSyntax (..),
    DuckDBColumnConstraintSyntax (..),
    DuckDBTableConstraintSyntax (..),
    DuckDBMatchTypeSyntax (..),
    DuckDBReferentialActionSyntax (..),
    DuckDBTableOptionsSyntax (..),
    DuckDBIndexOptions (..),
  )
where

import Data.Aeson (object, withObject, (.:), (.=))
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Hashable (Hashable (..))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.List.NonEmpty as NE
import Data.Scientific (Scientific)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Beam.Backend
  ( HasSqlValueSyntax (..),
    IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax (..),
    IsSql2003EnhancedNumericFunctionsExpressionSyntax (..),
    IsSql2003ExpressionAdvancedOLAPOperationsSyntax (..),
    IsSql2003ExpressionElementaryOLAPOperationsSyntax (..),
    IsSql2003ExpressionSyntax (..),
    IsSql2003FirstValueAndLastValueExpressionSyntax (..),
    IsSql2003LeadAndLagExpressionSyntax (..),
    IsSql2003NthValueExpressionSyntax (..),
    IsSql2003NtileExpressionSyntax (..),
    IsSql2003OrderingElementaryOLAPOperationsSyntax (..),
    IsSql2003WindowFrameBoundSyntax (..),
    IsSql2003WindowFrameBoundsSyntax (..),
    IsSql2003WindowFrameSyntax (..),
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
    IsSql92SchemaNameSyntax (..),
    IsSql92SelectSyntax (..),
    IsSql92SelectTableSyntax (..),
    IsSql92Syntax (..),
    IsSql92TableNameSyntax (..),
    IsSql92TableSourceSyntax (..),
    IsSql92UpdateSyntax (..),
    IsSql99AggregationExpressionSyntax (..),
    IsSql99CommonTableExpressionSelectSyntax (..),
    IsSql99CommonTableExpressionSyntax (..),
    IsSql99ConcatExpressionSyntax (..),
    IsSql99DataTypeSyntax (..),
    IsSql99ExpressionSyntax (..),
    IsSql99FunctionExpressionSyntax (..),
    IsSql99RecursiveCommonTableExpressionSelectSyntax (..),
    SqlNull (..),
  )
import Database.Beam.Backend.SQL
  ( IsSql2003BinaryAndVarBinaryDataTypeSyntax (..),
    IsSql2008BigIntDataTypeSyntax (..),
    Sql92DisplaySyntax (..),
  )
import Database.Beam.DuckDB.Syntax.Builder (DuckDBSyntax, commas, duckDBRenderSyntaxScript, emit, emitChar, emitIntegral, emitRealFloat, emitScientific, emitValue, parens, quotedIdentifier, sepBy, spaces)
import Database.Beam.Migrate.Checks (HasDataTypeCreatedCheck (..))
import Database.Beam.Migrate.SQL
  ( IsSql92AlterColumnActionSyntax (..),
    IsSql92AlterTableActionSyntax (..),
    IsSql92AlterTableSyntax (..),
    IsSql92ColumnConstraintDefinitionSyntax (..),
    IsSql92ColumnConstraintSyntax (..),
    IsSql92ColumnSchemaSyntax (..),
    IsSql92CreateDropIndexSyntax (..),
    IsSql92CreateSchemaSyntax (..),
    IsSql92CreateTableSyntax (..),
    IsSql92DdlCommandSyntax (..),
    IsSql92DdlSchemaCommandSyntax (..),
    IsSql92DropSchemaSyntax (..),
    IsSql92DropTableSyntax (..),
    IsSql92MatchTypeSyntax (..),
    IsSql92ReferentialActionSyntax (..),
    IsSql92TableConstraintSyntax (..),
    IsSql92UniqueIndexSyntax (..),
    Sql92SerializableConstraintDefinitionSyntax (..),
    Sql92SerializableDataTypeSyntax (..),
  )
import Database.Beam.Migrate.SQL.Builder (ConstraintAttributeTiming (..), SqlConstraintAttributesBuilder (..), sqlConstraintAttributesSerialized)
import Database.Beam.Migrate.SQL.SQL92 (ForeignKeyAction (..))
import Database.Beam.Migrate.Serialization
  ( BeamSerializedConstraint,
    BeamSerializedConstraintDefinition,
    BeamSerializedDataType (..),
    BeamSerializedExpression (..),
    BeamSerializedMatchType,
    BeamSerializedReferentialAction,
    fromBeamSerializedConstraintDefinition,
    fromBeamSerializedDataType,
  )
import Database.DuckDB.Simple (Null (Null))

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

instance IsSql92DdlCommandSyntax DuckDBCommandSyntax where
  type Sql92DdlCommandCreateTableSyntax DuckDBCommandSyntax = DuckDBCreateTableSyntax
  type Sql92DdlCommandDropTableSyntax DuckDBCommandSyntax = DuckDBDropTableSyntax
  type Sql92DdlCommandAlterTableSyntax DuckDBCommandSyntax = DuckDBAlterTableSyntax

  createTableCmd = DuckDBCommandSyntax . fromDuckDBCreateTable
  dropTableCmd = DuckDBCommandSyntax . fromDuckDBDropTable
  alterTableCmd = DuckDBCommandSyntax . fromDuckDBAlterTable

instance IsSql92DdlSchemaCommandSyntax DuckDBCommandSyntax where
  type Sql92DdlCommandCreateSchemaSyntax DuckDBCommandSyntax = DuckDBCreateSchemaSyntax
  type Sql92DdlCommandDropSchemaSyntax DuckDBCommandSyntax = DuckDBDropSchemaSyntax

  createSchemaCmd = DuckDBCommandSyntax . fromDuckDBCreateSchema
  dropSchemaCmd = DuckDBCommandSyntax . fromDuckDBDropSchema

newtype DuckDBTableNameSyntax = DuckDBTableNameSyntax {fromDuckDBTableName :: DuckDBSyntax}

-- | DuckDB @SELECT@ syntax
newtype DuckDBSelectSyntax = DuckDBSelectSyntax {fromDuckDBSelect :: DuckDBSyntax}

newtype DuckDBSelectTableSyntax = DuckDBSelectTableSyntax {fromDuckDBSelectTable :: DuckDBSyntax}

newtype DuckDBOrderingSyntax = DuckDBOrderingSyntax {fromDuckDBOrdering :: DuckDBSyntax}

newtype DuckDBExpressionSyntax = DuckDBExpressionSyntax {fromDuckDBExpression :: DuckDBSyntax} deriving (Eq)

newtype DuckDBTableSourceSyntax = DuckDBTableSourceSyntax {fromDuckDBTableSource :: DuckDBSyntax}

newtype DuckDBProjectionSyntax = DuckDBProjectionSyntax {fromDuckDBProjection :: DuckDBSyntax}

newtype DuckDBFromSyntax = DuckDBFromSyntax {fromDuckDBFrom :: DuckDBSyntax}

newtype DuckDBValueSyntax = DuckDBValueSyntax {fromDuckDBValue :: DuckDBSyntax}

newtype DuckDBFieldNameSyntax = DuckDBFieldNameSyntax {fromDuckDBFieldName :: DuckDBSyntax}

newtype DuckDBComparisonQuantifierSyntax = DuckDBComparisonQuantifierSyntax {fromDuckDBComparisonQuantifier :: DuckDBSyntax}

data DuckDBDataTypeSyntax = DuckDBDataTypeSyntax
  { duckDBDataType :: DuckDBSyntax,
    duckDBDataTypeSerialized :: BeamSerializedDataType
  }
  deriving (Show)

-- DDL syntax newtypes used by 'IsSql92DdlCommandSyntax' and friends.

newtype DuckDBCreateTableSyntax = DuckDBCreateTableSyntax {fromDuckDBCreateTable :: DuckDBSyntax}

data DuckDBTableOptionsSyntax = DuckDBTableOptionsSyntax DuckDBSyntax DuckDBSyntax

newtype DuckDBDropTableSyntax = DuckDBDropTableSyntax {fromDuckDBDropTable :: DuckDBSyntax}

newtype DuckDBAlterTableSyntax = DuckDBAlterTableSyntax {fromDuckDBAlterTable :: DuckDBSyntax}

newtype DuckDBAlterTableActionSyntax = DuckDBAlterTableActionSyntax {fromDuckDBAlterTableAction :: DuckDBSyntax}

newtype DuckDBAlterColumnActionSyntax = DuckDBAlterColumnActionSyntax {fromDuckDBAlterColumnAction :: DuckDBSyntax}

newtype DuckDBCreateSchemaSyntax = DuckDBCreateSchemaSyntax {fromDuckDBCreateSchema :: DuckDBSyntax}

newtype DuckDBDropSchemaSyntax = DuckDBDropSchemaSyntax {fromDuckDBDropSchema :: DuckDBSyntax}

newtype DuckDBSchemaNameSyntax = DuckDBSchemaNameSyntax {fromDuckDBSchemaName :: DuckDBSyntax}

newtype DuckDBColumnSchemaSyntax = DuckDBColumnSchemaSyntax {fromDuckDBColumnSchema :: DuckDBSyntax}
  deriving (Show, Eq)

data DuckDBColumnConstraintDefinitionSyntax = DuckDBColumnConstraintDefinitionSyntax
  { fromDuckDBColumnConstraintDefinition :: DuckDBSyntax,
    duckDBColumnConstraintDefinitionSerialized :: BeamSerializedConstraintDefinition
  }
  deriving (Show)

data DuckDBColumnConstraintSyntax = DuckDBColumnConstraintSyntax
  { fromDuckDBColumnConstraint :: DuckDBSyntax,
    duckDBColumnConstraintSerialized :: BeamSerializedConstraint
  }

newtype DuckDBTableConstraintSyntax = DuckDBTableConstraintSyntax {fromDuckDBTableConstraint :: DuckDBSyntax}

data DuckDBMatchTypeSyntax = DuckDBMatchTypeSyntax
  { fromDuckDBMatchType :: DuckDBSyntax,
    duckDBMatchTypeSerialized :: BeamSerializedMatchType
  }

data DuckDBReferentialActionSyntax = DuckDBReferentialActionSyntax
  { fromDuckDBReferentialAction :: DuckDBSyntax,
    duckDBReferentialActionSerialized :: BeamSerializedReferentialAction
  }

newtype DuckDBIndexOptions = DuckDBIndexOptions {duckDBIndexUnique :: Bool}
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

newtype DuckDBExtractFieldSyntax = DuckDBExtractFieldSyntax {fromDuckDBExtractField :: DuckDBSyntax}

newtype DuckDBGroupingSyntax = DuckDBGroupingSyntax {fromDuckDBGrouping :: DuckDBSyntax}

newtype DuckDBSelectSetQuantifierSyntax = DuckDBSelectSetQuantifierSyntax {fromDuckDBSelectSetQuantifier :: DuckDBSyntax}

newtype DuckDBAggregationSetQuantifierSyntax = DuckDBAggregationSetQuantifierSyntax {fromDuckDBAggregationSetQuantifier :: DuckDBSyntax}

newtype DuckDBCommonTableExpressionSyntax = DuckDBCommonTableExpressionSyntax {fromDuckDBCommonTableExpressionSyntax :: DuckDBSyntax}

newtype DuckDBWindowFrameSyntax = DuckDBWindowFrameSyntax {fromDuckDBWindowFrame :: DuckDBSyntax}

newtype DuckDBWindowFrameBoundsSyntax = DuckDBWindowFrameBoundsSyntax {fromDuckDBWindowFrameBounds :: DuckDBSyntax}

newtype DuckDBWindowFrameBoundSyntax = DuckDBWindowFrameBoundSyntax {fromDuckDBWindowFrameBound :: Text -> DuckDBSyntax}

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
        [ emit "SELECT ",
          maybe mempty ((<> emit " ") . fromDuckDBAggregationSetQuantifier) setQuantifier,
          fromDuckDBProjection proj,
          maybe mempty ((emit " FROM " <>) . fromDuckDBFrom) from,
          maybe mempty ((emit " WHERE " <>) . fromDuckDBExpression) where_,
          maybe mempty ((emit " GROUP BY " <>) . fromDuckDBGrouping) grouping,
          maybe mempty ((emit " HAVING " <>) . fromDuckDBExpression) having
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
  weekField = DuckDBExtractFieldSyntax (emit "WEEK")
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

instance IsSql99DataTypeSyntax DuckDBDataTypeSyntax where
  characterLargeObjectType = DuckDBDataTypeSyntax {duckDBDataType = emit "TEXT", duckDBDataTypeSerialized = characterLargeObjectType}
  binaryLargeObjectType = DuckDBDataTypeSyntax {duckDBDataType = emit "BYTEA", duckDBDataTypeSerialized = binaryLargeObjectType}
  booleanType = DuckDBDataTypeSyntax (emit "BOOLEAN") booleanType
  arrayType (DuckDBDataTypeSyntax syntax serialized) sz =
    DuckDBDataTypeSyntax
      (syntax <> emit "[" <> emit (fromString (show sz)) <> emit "]")
      (arrayType serialized sz)
  rowType fields =
    DuckDBDataTypeSyntax
      { duckDBDataType =
          -- Note that DuckDB's support for ROW is effectively STRUCT, as far as I understand
          emit "STRUCT("
            <> sepBy (emit ", ") (map (\(fieldName, DuckDBDataTypeSyntax t _) -> emit fieldName <> emit " " <> t) fields)
            <> emitChar ')',
        duckDBDataTypeSerialized = rowType (map (second duckDBDataTypeSerialized) fields)
      }

instance IsSql2008BigIntDataTypeSyntax DuckDBDataTypeSyntax where
  bigIntType = DuckDBDataTypeSyntax (emit "BIGINT") bigIntType

instance IsSql2003BinaryAndVarBinaryDataTypeSyntax DuckDBDataTypeSyntax where
  binaryType sz =
    DuckDBDataTypeSyntax
      (emit "BLOB" <> optPrec sz)
      (binaryType sz)
  varBinaryType sz =
    DuckDBDataTypeSyntax
      (emit "BLOB" <> optPrec sz)
      (varBinaryType sz)

-- | Two 'DuckDBDataTypeSyntax' values are equal when their rendered SQL is
-- equal. This is good enough for migration purposes, where the serialized
-- JSON form is used as the canonical comparison key by @beam-migrate@.
instance Eq DuckDBDataTypeSyntax where
  DuckDBDataTypeSyntax a _ == DuckDBDataTypeSyntax b _ = a == b

instance Hashable DuckDBDataTypeSyntax where
  hashWithSalt salt (DuckDBDataTypeSyntax a _) = hashWithSalt salt a

instance Sql92DisplaySyntax DuckDBDataTypeSyntax where
  displaySyntax = displaySyntax . duckDBDataType

instance Sql92SerializableDataTypeSyntax DuckDBDataTypeSyntax where
  serializeDataType = fromBeamSerializedDataType . duckDBDataTypeSerialized

-- | DuckDB doesn't allow user-defined domain types, so every data type is
-- always available.
instance HasDataTypeCreatedCheck DuckDBDataTypeSyntax where
  dataTypeHasBeenCreated _ _ = True

instance Sql92DisplaySyntax DuckDBColumnSchemaSyntax where
  displaySyntax = displaySyntax . fromDuckDBColumnSchema

instance Hashable DuckDBColumnSchemaSyntax where
  hashWithSalt salt = hashWithSalt salt . fromDuckDBColumnSchema

instance Eq DuckDBColumnConstraintDefinitionSyntax where
  DuckDBColumnConstraintDefinitionSyntax a _ == DuckDBColumnConstraintDefinitionSyntax b _ = a == b

instance Hashable DuckDBColumnConstraintDefinitionSyntax where
  hashWithSalt salt = hashWithSalt salt . fromDuckDBColumnConstraintDefinition

instance Sql92DisplaySyntax DuckDBColumnConstraintDefinitionSyntax where
  displaySyntax = displaySyntax . fromDuckDBColumnConstraintDefinition

instance Sql92SerializableConstraintDefinitionSyntax DuckDBColumnConstraintDefinitionSyntax where
  serializeConstraint = fromBeamSerializedConstraintDefinition . duckDBColumnConstraintDefinitionSerialized

instance IsSql92SchemaNameSyntax DuckDBSchemaNameSyntax where
  schemaName s = DuckDBSchemaNameSyntax (quotedIdentifier s)

instance IsSql92CreateSchemaSyntax DuckDBCreateSchemaSyntax where
  type Sql92CreateSchemaSchemaNameSyntax DuckDBCreateSchemaSyntax = DuckDBSchemaNameSyntax

  createSchemaSyntax sNm =
    DuckDBCreateSchemaSyntax (emit "CREATE SCHEMA " <> fromDuckDBSchemaName sNm)

instance IsSql92DropSchemaSyntax DuckDBDropSchemaSyntax where
  type Sql92DropSchemaSchemaNameSyntax DuckDBDropSchemaSyntax = DuckDBSchemaNameSyntax

  dropSchemaSyntax sNm =
    DuckDBDropSchemaSyntax (emit "DROP SCHEMA " <> fromDuckDBSchemaName sNm)

instance IsSql92DropTableSyntax DuckDBDropTableSyntax where
  type Sql92DropTableTableNameSyntax DuckDBDropTableSyntax = DuckDBTableNameSyntax

  dropTableSyntax tblNm =
    DuckDBDropTableSyntax (emit "DROP TABLE " <> fromDuckDBTableName tblNm)

instance IsSql92AlterTableSyntax DuckDBAlterTableSyntax where
  type Sql92AlterTableAlterTableActionSyntax DuckDBAlterTableSyntax = DuckDBAlterTableActionSyntax
  type Sql92AlterTableTableNameSyntax DuckDBAlterTableSyntax = DuckDBTableNameSyntax

  alterTableSyntax tblNm action =
    DuckDBAlterTableSyntax $
      emit "ALTER TABLE "
        <> fromDuckDBTableName tblNm
        <> emit " "
        <> fromDuckDBAlterTableAction action

instance IsSql92AlterTableActionSyntax DuckDBAlterTableActionSyntax where
  type Sql92AlterTableAlterColumnActionSyntax DuckDBAlterTableActionSyntax = DuckDBAlterColumnActionSyntax
  type Sql92AlterTableColumnSchemaSyntax DuckDBAlterTableActionSyntax = DuckDBColumnSchemaSyntax

  alterColumnSyntax colNm action =
    DuckDBAlterTableActionSyntax $
      emit "ALTER COLUMN "
        <> quotedIdentifier colNm
        <> emit " "
        <> fromDuckDBAlterColumnAction action

  addColumnSyntax colNm schema =
    DuckDBAlterTableActionSyntax $
      emit "ADD COLUMN "
        <> quotedIdentifier colNm
        <> emit " "
        <> fromDuckDBColumnSchema schema

  dropColumnSyntax colNm =
    DuckDBAlterTableActionSyntax (emit "DROP COLUMN " <> quotedIdentifier colNm)

  renameTableToSyntax newNm =
    DuckDBAlterTableActionSyntax (emit "RENAME TO " <> quotedIdentifier newNm)

  renameColumnToSyntax oldNm newNm =
    DuckDBAlterTableActionSyntax $
      emit "RENAME COLUMN "
        <> quotedIdentifier oldNm
        <> emit " TO "
        <> quotedIdentifier newNm

instance IsSql92AlterColumnActionSyntax DuckDBAlterColumnActionSyntax where
  setNullSyntax = DuckDBAlterColumnActionSyntax (emit "DROP NOT NULL")
  setNotNullSyntax = DuckDBAlterColumnActionSyntax (emit "SET NOT NULL")

instance IsSql92CreateTableSyntax DuckDBCreateTableSyntax where
  type Sql92CreateTableTableNameSyntax DuckDBCreateTableSyntax = DuckDBTableNameSyntax
  type Sql92CreateTableColumnSchemaSyntax DuckDBCreateTableSyntax = DuckDBColumnSchemaSyntax
  type Sql92CreateTableTableConstraintSyntax DuckDBCreateTableSyntax = DuckDBTableConstraintSyntax
  type Sql92CreateTableOptionsSyntax DuckDBCreateTableSyntax = DuckDBTableOptionsSyntax

  createTableSyntax options tblNm fieldTypes constraints =
    let (beforeOptions, afterOptions) =
          case options of
            Nothing -> (emit " ", emit " ")
            Just (DuckDBTableOptionsSyntax before after) ->
              (emit " " <> before <> emit " ", emit " " <> after <> emit " ")
     in DuckDBCreateTableSyntax $
          emit "CREATE"
            <> beforeOptions
            <> emit "TABLE "
            <> fromDuckDBTableName tblNm
            <> emit " ("
            <> sepBy
              (emit ", ")
              ( map (\(nm, ty) -> quotedIdentifier nm <> emit " " <> fromDuckDBColumnSchema ty) fieldTypes
                  <> map fromDuckDBTableConstraint constraints
              )
            <> emit ")"
            <> afterOptions

duckDBForeignKeyAction :: ForeignKeyAction -> DuckDBSyntax
duckDBForeignKeyAction ForeignKeyActionCascade = emit "CASCADE"
duckDBForeignKeyAction ForeignKeyActionSetNull = emit "SET NULL"
duckDBForeignKeyAction ForeignKeyActionSetDefault = emit "SET DEFAULT"
duckDBForeignKeyAction ForeignKeyActionRestrict = emit "RESTRICT"
duckDBForeignKeyAction ForeignKeyNoAction = emit "NO ACTION"

instance IsSql92TableConstraintSyntax DuckDBTableConstraintSyntax where
  primaryKeyConstraintSyntax fieldNames =
    DuckDBTableConstraintSyntax $
      emit "PRIMARY KEY("
        <> sepBy (emit ", ") (map quotedIdentifier (NE.toList fieldNames))
        <> emit ")"
  foreignKeyConstraintSyntax localCols refTbl refCols onUpdate onDelete =
    DuckDBTableConstraintSyntax $
      emit "FOREIGN KEY("
        <> sepBy (emit ", ") (map quotedIdentifier (NE.toList localCols))
        <> emit ") REFERENCES "
        <> quotedIdentifier refTbl
        <> emit "("
        <> sepBy (emit ", ") (map quotedIdentifier (NE.toList refCols))
        <> emit ")"
        <> emit " ON UPDATE "
        <> duckDBForeignKeyAction onUpdate
        <> emit " ON DELETE "
        <> duckDBForeignKeyAction onDelete

instance IsSql92ColumnSchemaSyntax DuckDBColumnSchemaSyntax where
  type Sql92ColumnSchemaColumnTypeSyntax DuckDBColumnSchemaSyntax = DuckDBDataTypeSyntax
  type Sql92ColumnSchemaExpressionSyntax DuckDBColumnSchemaSyntax = DuckDBExpressionSyntax
  type Sql92ColumnSchemaColumnConstraintDefinitionSyntax DuckDBColumnSchemaSyntax = DuckDBColumnConstraintDefinitionSyntax

  columnSchemaSyntax colType defaultClause constraints collation =
    DuckDBColumnSchemaSyntax $
      duckDBDataType colType
        <> maybe mempty (\d -> emit " DEFAULT " <> fromDuckDBExpression d) defaultClause
        <> ( case constraints of
               [] -> mempty
               _ -> foldMap (\c -> emit " " <> fromDuckDBColumnConstraintDefinition c) constraints
           )
        <> maybe mempty (\nm -> emit " COLLATE " <> quotedIdentifier nm) collation

instance IsSql92MatchTypeSyntax DuckDBMatchTypeSyntax where
  fullMatchSyntax = DuckDBMatchTypeSyntax (emit "FULL") fullMatchSyntax
  partialMatchSyntax = DuckDBMatchTypeSyntax (emit "PARTIAL") partialMatchSyntax

instance IsSql92ReferentialActionSyntax DuckDBReferentialActionSyntax where
  referentialActionCascadeSyntax = DuckDBReferentialActionSyntax (emit "CASCADE") referentialActionCascadeSyntax
  referentialActionNoActionSyntax = DuckDBReferentialActionSyntax (emit "NO ACTION") referentialActionNoActionSyntax
  referentialActionSetDefaultSyntax = DuckDBReferentialActionSyntax (emit "SET DEFAULT") referentialActionSetDefaultSyntax
  referentialActionSetNullSyntax = DuckDBReferentialActionSyntax (emit "SET NULL") referentialActionSetNullSyntax

instance IsSql92ColumnConstraintDefinitionSyntax DuckDBColumnConstraintDefinitionSyntax where
  type Sql92ColumnConstraintDefinitionConstraintSyntax DuckDBColumnConstraintDefinitionSyntax = DuckDBColumnConstraintSyntax
  type Sql92ColumnConstraintDefinitionAttributesSyntax DuckDBColumnConstraintDefinitionSyntax = SqlConstraintAttributesBuilder

  constraintDefinitionSyntax nm constraint attrs =
    DuckDBColumnConstraintDefinitionSyntax syntax serialized
    where
      syntax =
        maybe mempty (\n -> emit "CONSTRAINT " <> quotedIdentifier n <> emit " ") nm
          <> fromDuckDBColumnConstraint constraint
          <> maybe mempty (\a -> emit " " <> duckDBSqlConstraintAttributes a) attrs
      serialized =
        constraintDefinitionSyntax nm (duckDBColumnConstraintSerialized constraint) (fmap sqlConstraintAttributesSerialized attrs)

-- | Emit a 'SqlConstraintAttributesBuilder' as DuckDB syntax. DuckDB does not
-- support @DEFERRABLE@/@INITIALLY DEFERRED@ at the time of writing, but we
-- still emit the corresponding SQL keywords so that user-supplied constraint
-- attributes are not silently dropped. If DuckDB rejects them, the user will
-- see an obvious error rather than a subtle behavioural difference.
duckDBSqlConstraintAttributes :: SqlConstraintAttributesBuilder -> DuckDBSyntax
duckDBSqlConstraintAttributes (SqlConstraintAttributesBuilder timing deferrable) =
  maybe mempty timingBuilder timing <> maybe mempty deferrableBuilder deferrable
  where
    timingBuilder InitiallyDeferred = emit "INITIALLY DEFERRED"
    timingBuilder InitiallyImmediate = emit "INITIALLY IMMEDIATE"
    deferrableBuilder False = emit "NOT DEFERRABLE"
    deferrableBuilder True = emit "DEFERRABLE"

instance IsSql92ColumnConstraintSyntax DuckDBColumnConstraintSyntax where
  type Sql92ColumnConstraintMatchTypeSyntax DuckDBColumnConstraintSyntax = DuckDBMatchTypeSyntax
  type Sql92ColumnConstraintReferentialActionSyntax DuckDBColumnConstraintSyntax = DuckDBReferentialActionSyntax
  type Sql92ColumnConstraintExpressionSyntax DuckDBColumnConstraintSyntax = DuckDBExpressionSyntax

  notNullConstraintSyntax = DuckDBColumnConstraintSyntax (emit "NOT NULL") notNullConstraintSyntax
  uniqueColumnConstraintSyntax = DuckDBColumnConstraintSyntax (emit "UNIQUE") uniqueColumnConstraintSyntax
  primaryKeyColumnConstraintSyntax = DuckDBColumnConstraintSyntax (emit "PRIMARY KEY") primaryKeyColumnConstraintSyntax
  checkColumnConstraintSyntax expr =
    DuckDBColumnConstraintSyntax
      (emit "CHECK(" <> fromDuckDBExpression expr <> emit ")")
      ( checkColumnConstraintSyntax
          ( BeamSerializedExpression
              (duckDBRenderSyntaxScript (fromDuckDBExpression expr))
          )
      )
  referencesConstraintSyntax tbl fields matchType onUpdate onDelete =
    DuckDBColumnConstraintSyntax syntax serialized
    where
      syntax =
        emit "REFERENCES "
          <> quotedIdentifier tbl
          <> emit "("
          <> sepBy (emit ", ") (map quotedIdentifier fields)
          <> emit ")"
          <> maybe mempty (\m -> emit " " <> fromDuckDBMatchType m) matchType
          <> maybe mempty (\a -> emit " ON UPDATE " <> fromDuckDBReferentialAction a) onUpdate
          <> maybe mempty (\a -> emit " ON DELETE " <> fromDuckDBReferentialAction a) onDelete
      serialized =
        referencesConstraintSyntax
          tbl
          fields
          (fmap duckDBMatchTypeSerialized matchType)
          (fmap duckDBReferentialActionSerialized onUpdate)
          (fmap duckDBReferentialActionSerialized onDelete)

instance IsSql92CreateDropIndexSyntax DuckDBCommandSyntax where
  type Sql92CreateIndexOptionsSyntax DuckDBCommandSyntax = DuckDBIndexOptions

  defaultIndexOptions = DuckDBIndexOptions {duckDBIndexUnique = False}

  createIndexCmd idxNm tblNm cols opts =
    DuckDBCommandSyntax $
      emit (if duckDBIndexUnique opts then "CREATE UNIQUE INDEX " else "CREATE INDEX ")
        <> quotedIdentifier idxNm
        <> emit " ON "
        <> fromDuckDBTableName tblNm
        <> parens (sepBy (emit ", ") (NE.toList (fmap quotedIdentifier cols)))

  dropIndexCmd idxNm =
    DuckDBCommandSyntax (emit "DROP INDEX " <> quotedIdentifier idxNm)

  serializeIndexOptions opts =
    object ["unique" .= duckDBIndexUnique opts]

  deserializeIndexOptions =
    withObject "DuckDBIndexOptions" $ \v ->
      DuckDBIndexOptions <$> v .: "unique"

instance IsSql92UniqueIndexSyntax DuckDBCommandSyntax where
  setUniqueIndexOptions u opts = opts {duckDBIndexUnique = u}
  indexIsUnique = duckDBIndexUnique

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

instance HasSqlValueSyntax DuckDBValueSyntax Scientific where
  sqlValueSyntax f = DuckDBValueSyntax (emitScientific f)

instance HasSqlValueSyntax DuckDBValueSyntax Bool where
  sqlValueSyntax = DuckDBValueSyntax . emitValue

instance HasSqlValueSyntax DuckDBValueSyntax SqlNull where
  sqlValueSyntax _ = DuckDBValueSyntax (emitValue Null)

instance HasSqlValueSyntax DuckDBValueSyntax String where
  sqlValueSyntax = sqlValueSyntax . Text.pack

instance HasSqlValueSyntax DuckDBValueSyntax Text where
  sqlValueSyntax = DuckDBValueSyntax . emitValue

instance HasSqlValueSyntax DuckDBValueSyntax Lazy.Text where
  sqlValueSyntax = sqlValueSyntax . Lazy.toStrict

instance HasSqlValueSyntax DuckDBValueSyntax Day where
  sqlValueSyntax = DuckDBValueSyntax . emitValue

instance HasSqlValueSyntax DuckDBValueSyntax TimeOfDay where
  sqlValueSyntax = DuckDBValueSyntax . emitValue

instance HasSqlValueSyntax DuckDBValueSyntax LocalTime where
  sqlValueSyntax = DuckDBValueSyntax . emitValue

instance HasSqlValueSyntax DuckDBValueSyntax UTCTime where
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
  quantifierListE vs =
    DuckDBExpressionSyntax $
      emit "(VALUES " <> sepBy (emit ", ") (fmap (parens . coerce) vs) <> emit ")"
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

  ascOrdering e = DuckDBOrderingSyntax (fromDuckDBExpression e <> emit " ASC")
  descOrdering e = DuckDBOrderingSyntax (fromDuckDBExpression e <> emit " DESC")

instance IsSql2003OrderingElementaryOLAPOperationsSyntax DuckDBOrderingSyntax where
  nullsFirstOrdering o = DuckDBOrderingSyntax $ coerce o <> emit " NULLS FIRST"
  nullsLastOrdering o = DuckDBOrderingSyntax $ coerce o <> emit " NULLS LAST"

instance IsSql92SelectSyntax DuckDBSelectSyntax where
  type Sql92SelectSelectTableSyntax DuckDBSelectSyntax = DuckDBSelectTableSyntax
  type Sql92SelectOrderingSyntax DuckDBSelectSyntax = DuckDBOrderingSyntax

  selectStmt tbl ordering limit offset =
    DuckDBSelectSyntax $
      mconcat
        [ fromDuckDBSelectTable tbl,
          case ordering of
            [] -> mempty
            ordering' -> emit " ORDER BY " <> commas (map coerce ordering'),
          maybe mempty (emit . fromString . (" LIMIT " <>) . show) limit,
          maybe mempty (emit . fromString . (" OFFSET " <>) . show) offset
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

instance IsSql99CommonTableExpressionSelectSyntax DuckDBSelectSyntax where
  type Sql99SelectCTESyntax DuckDBSelectSyntax = DuckDBCommonTableExpressionSyntax

  withSyntax ctes (DuckDBSelectSyntax select) =
    DuckDBSelectSyntax $
      emit "WITH "
        <> sepBy (emit ", ") (map fromDuckDBCommonTableExpressionSyntax ctes)
        <> select

instance IsSql99RecursiveCommonTableExpressionSelectSyntax DuckDBSelectSyntax where
  withRecursiveSyntax ctes (DuckDBSelectSyntax select) =
    DuckDBSelectSyntax $
      emit "WITH RECURSIVE "
        <> sepBy (emit ", ") (map fromDuckDBCommonTableExpressionSyntax ctes)
        <> select

instance IsSql99CommonTableExpressionSyntax DuckDBCommonTableExpressionSyntax where
  type Sql99CTESelectSyntax DuckDBCommonTableExpressionSyntax = DuckDBSelectSyntax

  cteSubquerySyntax tbl fields (DuckDBSelectSyntax select) =
    DuckDBCommonTableExpressionSyntax $
      quotedIdentifier tbl
        <> parens (sepBy (emit ",") (map quotedIdentifier fields))
        <> emit " AS "
        <> parens select

instance IsSql99FunctionExpressionSyntax DuckDBExpressionSyntax where
  functionCallE name args =
    DuckDBExpressionSyntax $
      fromDuckDBExpression name
        <> parens (sepBy (emit ", ") (map fromDuckDBExpression args))
  functionNameE nm = DuckDBExpressionSyntax (emit nm)

instance IsSql99ExpressionSyntax DuckDBExpressionSyntax where
  -- The implementation of 'distinctE' is the same as Postgres', but its
  -- use via 'distinct_' creates SQL queries which are not necessarily supported
  distinctE select = DuckDBExpressionSyntax (emit "DISTINCT (" <> fromDuckDBSelect select <> emit ")")

  -- DuckDB doesn't support 'SIMILAR TO' exactly, but rather
  -- provides the 'regexp_matches' function
  similarToE left right =
    DuckDBExpressionSyntax $
      mconcat
        [ emit "regexp_matches(",
          coerce left,
          emit ", ",
          coerce right,
          emitChar ')'
        ]

  instanceFieldE i nm =
    DuckDBExpressionSyntax $
      parens (fromDuckDBExpression i) <> emit "." <> quotedIdentifier nm

  refFieldE i nm =
    DuckDBExpressionSyntax $
      parens (fromDuckDBExpression i) <> emit "->" <> quotedIdentifier nm

instance IsSql99ConcatExpressionSyntax DuckDBExpressionSyntax where
  concatE [] = valueE (sqlValueSyntax ("" :: Text))
  concatE [x] = x
  concatE es =
    DuckDBExpressionSyntax $
      emit "CONCAT"
        <> parens
          ( sepBy
              (emit ", ")
              -- DuckDB's type inference doesn't work reliably inside of CONCAT. I suspect
              -- that this is only for ? parameters for binding.
              -- However, to be extra safe, we cast every element inside the `CONCAT` call
              -- to a VARCHAR
              -- TODO: optimize this to only CAST when the expression is a binding parameter
              (map (\e -> emit "CAST" <> parens (coerce e <> emit " AS VARCHAR")) es)
          )

instance IsSql99AggregationExpressionSyntax DuckDBExpressionSyntax where
  everyE = aggFunc "BOOL_AND" -- DuckDB doesn't implement 'EVERY'

  -- The following two functions are modeled the same way
  -- as for the Postgres backend
  someE = aggFunc "BOOL_OR"
  anyE = aggFunc "BOOL_OR"

instance IsSql2003ExpressionSyntax DuckDBExpressionSyntax where
  type
    Sql2003ExpressionWindowFrameSyntax DuckDBExpressionSyntax =
      DuckDBWindowFrameSyntax

  overE expr frame =
    DuckDBExpressionSyntax $
      fromDuckDBExpression expr <> emit " " <> fromDuckDBWindowFrame frame
  rowNumberE = DuckDBExpressionSyntax $ emit "ROW_NUMBER()"

instance IsSql2003EnhancedNumericFunctionsExpressionSyntax DuckDBExpressionSyntax where
  lnE x = DuckDBExpressionSyntax (emit "LN(" <> fromDuckDBExpression x <> emit ")")
  expE x = DuckDBExpressionSyntax (emit "EXP(" <> fromDuckDBExpression x <> emit ")")
  sqrtE x = DuckDBExpressionSyntax (emit "SQRT(" <> fromDuckDBExpression x <> emit ")")
  ceilE x = DuckDBExpressionSyntax (emit "CEIL(" <> fromDuckDBExpression x <> emit ")")
  floorE x = DuckDBExpressionSyntax (emit "FLOOR(" <> fromDuckDBExpression x <> emit ")")
  powerE x y = DuckDBExpressionSyntax (emit "POWER(" <> fromDuckDBExpression x <> emit ", " <> fromDuckDBExpression y <> emit ")")

instance IsSql2003ExpressionAdvancedOLAPOperationsSyntax DuckDBExpressionSyntax where
  denseRankAggE = DuckDBExpressionSyntax $ emit "DENSE_RANK()"
  percentRankAggE = DuckDBExpressionSyntax $ emit "PERCENT_RANK()"
  cumeDistAggE = DuckDBExpressionSyntax $ emit "CUME_DIST()"

instance IsSql2003ExpressionElementaryOLAPOperationsSyntax DuckDBExpressionSyntax where
  rankAggE = DuckDBExpressionSyntax $ emit "RANK()"
  filterAggE agg f =
    DuckDBExpressionSyntax $
      fromDuckDBExpression agg <> emit " FILTER (WHERE " <> fromDuckDBExpression f <> emit ")"

binAggFunc ::
  Text ->
  Maybe DuckDBAggregationSetQuantifierSyntax ->
  DuckDBExpressionSyntax ->
  DuckDBExpressionSyntax ->
  DuckDBExpressionSyntax
binAggFunc fn q x y =
  DuckDBExpressionSyntax $
    emit fn
      <> emitChar '('
      <> maybe mempty (\inner -> fromDuckDBAggregationSetQuantifier inner <> emitChar ' ') q
      <> fromDuckDBExpression x
      <> emit ", "
      <> fromDuckDBExpression y
      <> emitChar ')'

instance IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax DuckDBExpressionSyntax where
  stddevPopE = aggFunc "STDDEV_POP"
  stddevSampE = aggFunc "STDDEV_SAMP"
  varPopE = aggFunc "VAR_POP"
  varSampE = aggFunc "VAR_SAMP"

  covarPopE = binAggFunc "COVAR_POP"
  covarSampE = binAggFunc "COVAR_SAMP"
  corrE = binAggFunc "CORR"
  regrSlopeE = binAggFunc "REGR_SLOPE"
  regrInterceptE = binAggFunc "REGR_INTERCEPT"
  regrCountE = binAggFunc "REGR_COUNT"
  regrRSquaredE = binAggFunc "REGR_R2"
  regrAvgXE = binAggFunc "REGR_AVGX"
  regrAvgYE = binAggFunc "REGR_AVGY"
  regrSXXE = binAggFunc "REGR_SXX"
  regrSYYE = binAggFunc "REGR_SYY"
  regrSXYE = binAggFunc "REGR_SXY"

instance IsSql2003NtileExpressionSyntax DuckDBExpressionSyntax where
  ntileE x = DuckDBExpressionSyntax (emit "NTILE(" <> fromDuckDBExpression x <> emit ")")

instance IsSql2003LeadAndLagExpressionSyntax DuckDBExpressionSyntax where
  leadE x Nothing Nothing =
    DuckDBExpressionSyntax (emit "LEAD(" <> fromDuckDBExpression x <> emit ")")
  leadE x (Just n) Nothing =
    DuckDBExpressionSyntax (emit "LEAD(" <> fromDuckDBExpression x <> emit ", " <> fromDuckDBExpression n <> emit ")")
  leadE x (Just n) (Just def) =
    DuckDBExpressionSyntax (emit "LEAD(" <> fromDuckDBExpression x <> emit ", " <> fromDuckDBExpression n <> emit ", " <> fromDuckDBExpression def <> emit ")")
  leadE x Nothing (Just def) =
    DuckDBExpressionSyntax (emit "LEAD(" <> fromDuckDBExpression x <> emit ", 1, " <> fromDuckDBExpression def <> emit ")")

  lagE x Nothing Nothing =
    DuckDBExpressionSyntax (emit "LAG(" <> fromDuckDBExpression x <> emit ")")
  lagE x (Just n) Nothing =
    DuckDBExpressionSyntax (emit "LAG(" <> fromDuckDBExpression x <> emit ", " <> fromDuckDBExpression n <> emit ")")
  lagE x (Just n) (Just def) =
    DuckDBExpressionSyntax (emit "LAG(" <> fromDuckDBExpression x <> emit ", " <> fromDuckDBExpression n <> emit ", " <> fromDuckDBExpression def <> emit ")")
  lagE x Nothing (Just def) =
    DuckDBExpressionSyntax (emit "LAG(" <> fromDuckDBExpression x <> emit ", 1, " <> fromDuckDBExpression def <> emit ")")

instance IsSql2003FirstValueAndLastValueExpressionSyntax DuckDBExpressionSyntax where
  firstValueE x = DuckDBExpressionSyntax (emit "FIRST_VALUE(" <> fromDuckDBExpression x <> emit ")")
  lastValueE x = DuckDBExpressionSyntax (emit "LAST_VALUE(" <> fromDuckDBExpression x <> emit ")")

instance IsSql2003NthValueExpressionSyntax DuckDBExpressionSyntax where
  nthValueE x n = DuckDBExpressionSyntax (emit "NTH_VALUE(" <> fromDuckDBExpression x <> emit ", " <> fromDuckDBExpression n <> emit ")")

instance IsSql2003WindowFrameSyntax DuckDBWindowFrameSyntax where
  type Sql2003WindowFrameExpressionSyntax DuckDBWindowFrameSyntax = DuckDBExpressionSyntax
  type Sql2003WindowFrameOrderingSyntax DuckDBWindowFrameSyntax = DuckDBOrderingSyntax
  type Sql2003WindowFrameBoundsSyntax DuckDBWindowFrameSyntax = DuckDBWindowFrameBoundsSyntax

  frameSyntax partition_ ordering_ bounds_ =
    DuckDBWindowFrameSyntax $
      emit "OVER "
        <> parens
          ( maybe mempty (\p -> emit "PARTITION BY " <> sepBy (emit ", ") (map fromDuckDBExpression p)) partition_
              <> maybe mempty (\o -> emit " ORDER BY " <> sepBy (emit ", ") (map fromDuckDBOrdering o)) ordering_
              <> maybe mempty (\b -> emit " ROWS " <> fromDuckDBWindowFrameBounds b) bounds_
          )

instance IsSql2003WindowFrameBoundsSyntax DuckDBWindowFrameBoundsSyntax where
  type Sql2003WindowFrameBoundsBoundSyntax DuckDBWindowFrameBoundsSyntax = DuckDBWindowFrameBoundSyntax

  fromToBoundSyntax from Nothing =
    DuckDBWindowFrameBoundsSyntax (fromDuckDBWindowFrameBound from "PRECEDING")
  fromToBoundSyntax from (Just to) =
    DuckDBWindowFrameBoundsSyntax $
      emit "BETWEEN " <> fromDuckDBWindowFrameBound from "PRECEDING" <> emit " AND " <> fromDuckDBWindowFrameBound to "FOLLOWING"

instance IsSql2003WindowFrameBoundSyntax DuckDBWindowFrameBoundSyntax where
  unboundedSyntax = DuckDBWindowFrameBoundSyntax $ \where_ -> emit "UNBOUNDED " <> emit where_
  nrowsBoundSyntax 0 = DuckDBWindowFrameBoundSyntax $ \_ -> emit "CURRENT ROW"
  nrowsBoundSyntax n = DuckDBWindowFrameBoundSyntax $ \where_ -> emit (fromString (show n)) <> emit " " <> emit where_
