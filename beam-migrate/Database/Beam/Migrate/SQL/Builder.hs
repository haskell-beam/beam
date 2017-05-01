module Database.Beam.Migrate.SQL.Builder where

import Database.Beam.Backend.SQL.Builder
import Database.Beam.Migrate.SQL

import Control.Applicative

import Data.ByteString.Builder (Builder, byteString)
import Data.Monoid

data SqlSyntaxBuilderCreateTableOptions
    = SqlSyntaxBuilderCreateTableOptions
        SqlSyntaxBuilder
        SqlSyntaxBuilder
    deriving Eq

instance IsSql92DdlCommandSyntax SqlSyntaxBuilder where
  type Sql92DdlCommandCreateTableSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92DdlCommandDropTableSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92DdlCommandAlterTableSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  createTableCmd = id
  alterTableCmd = id
  dropTableCmd = id

instance IsSql92DropTableSyntax SqlSyntaxBuilder where
  dropTableSyntax tblNm =
    SqlSyntaxBuilder $
    byteString "DROP TABLE " <> quoteSql tblNm

instance IsSql92AlterTableSyntax SqlSyntaxBuilder where
  type Sql92AlterTableAlterTableActionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  alterTableSyntax tblNm action =
    SqlSyntaxBuilder $
    byteString "ALTER TABLE " <> quoteSql tblNm <> byteString " " <> buildSql action

instance IsSql92AlterTableActionSyntax SqlSyntaxBuilder where
  type Sql92AlterTableAlterColumnActionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92AlterTableColumnSchemaSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  alterColumnSyntax colNm action =
    SqlSyntaxBuilder $
    byteString "ALTER COLUMN " <> quoteSql colNm <> byteString " " <> buildSql action

  addColumnSyntax colNm colSchema =
    SqlSyntaxBuilder $
    byteString "ADD COLUMN " <> quoteSql colNm <> byteString " " <> buildSql colSchema
  dropColumnSyntax colNm =
    SqlSyntaxBuilder $
    byteString "DROP COLUMN " <> quoteSql colNm

instance IsSql92AlterColumnActionSyntax SqlSyntaxBuilder where
  setNotNullSyntax = SqlSyntaxBuilder (byteString "SET NOT NULL")
  setNullSyntax = SqlSyntaxBuilder (byteString "DROP NOT NULL")

instance IsSql92CreateTableSyntax SqlSyntaxBuilder where
  type Sql92CreateTableColumnSchemaSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92CreateTableTableConstraintSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92CreateTableOptionsSyntax SqlSyntaxBuilder = SqlSyntaxBuilderCreateTableOptions

  createTableSyntax tableOptions tableName fieldSchemas constraints =
      SqlSyntaxBuilder $
      byteString "CREATE " <>
      maybe mempty (\b -> buildSql b <> byteString " ") beforeOptions <>
      byteString " TABLE " <>

      quoteSql tableName <>

      byteString "(" <>
      buildSepBy (byteString ", ")
                 (map (\(nm, schema) -> quoteSql nm <> byteString " " <> buildSql schema) fieldSchemas <>
                  map buildSql constraints) <>
      byteString ")" <>

      maybe mempty (\a -> buildSql a <> byteString " ") afterOptions

    where
      (beforeOptions, afterOptions) =
          case tableOptions of
            Just (SqlSyntaxBuilderCreateTableOptions b a) -> (Just b, Just a)
            Nothing -> (Nothing, Nothing)

instance IsSql92TableConstraintSyntax SqlSyntaxBuilder where
  primaryKeyConstraintSyntax fs =
    SqlSyntaxBuilder $
    byteString "PRIMARY KEY(" <> buildSepBy (byteString ", ") (map quoteSql fs) <> byteString ")"

data ConstraintAttributeTiming = InitiallyDeferred | InitiallyImmediate
  deriving (Show, Eq, Ord, Enum, Bounded)

data SqlConstraintAttributesBuilder
  = SqlConstraintAttributesBuilder
  { _sqlConstraintAttributeTiming :: Maybe ConstraintAttributeTiming
  , _sqlConstraintAttributeDeferrable :: Maybe Bool }
  deriving (Show, Eq)

instance Monoid SqlConstraintAttributesBuilder where
  mempty = SqlConstraintAttributesBuilder Nothing Nothing
  mappend a b =
    SqlConstraintAttributesBuilder
      (_sqlConstraintAttributeTiming b <|> _sqlConstraintAttributeTiming a)
      (_sqlConstraintAttributeDeferrable b <|> _sqlConstraintAttributeDeferrable a)

fromSqlConstraintAttributes :: SqlConstraintAttributesBuilder -> Builder
fromSqlConstraintAttributes (SqlConstraintAttributesBuilder timing deferrable) =
  maybe mempty timingBuilder timing <> maybe mempty deferrableBuilder deferrable
  where timingBuilder InitiallyDeferred = byteString "INITIALLY DEFERRED"
        timingBuilder InitiallyImmediate = byteString "INITIALLY IMMEDIATE"
        deferrableBuilder False = byteString "NOT DEFERRABLE"
        deferrableBuilder True = byteString "DEFERRABLE"

instance IsSql92ConstraintAttributesSyntax SqlConstraintAttributesBuilder where
  initiallyDeferredAttributeSyntax = SqlConstraintAttributesBuilder (Just InitiallyDeferred) Nothing
  initiallyImmediateAttributeSyntax = SqlConstraintAttributesBuilder (Just InitiallyImmediate) Nothing
  deferrableAttributeSyntax = SqlConstraintAttributesBuilder Nothing (Just True)
  notDeferrableAttributeSyntax = SqlConstraintAttributesBuilder Nothing (Just False)

instance IsSql92ColumnSchemaSyntax SqlSyntaxBuilder where
  type Sql92ColumnSchemaColumnConstraintDefinitionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ColumnSchemaColumnTypeSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ColumnSchemaExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  columnSchemaSyntax type_ default_ constraints collation =
    SqlSyntaxBuilder $
    buildSql type_ <>
    maybe mempty (\d -> byteString " DEFAULT " <> buildSql d) default_ <>
    (case constraints of
       [] -> mempty
       _ -> foldMap (\c -> byteString " " <> buildSql c) constraints) <>
    maybe mempty (\nm -> byteString " COLLATE " <> quoteSql nm) collation

instance IsSql92ColumnConstraintDefinitionSyntax SqlSyntaxBuilder where
  type Sql92ColumnConstraintDefinitionConstraintSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ColumnConstraintDefinitionAttributesSyntax SqlSyntaxBuilder = SqlConstraintAttributesBuilder

  constraintDefinitionSyntax nm c attrs =
    SqlSyntaxBuilder $
    maybe mempty (\nm -> byteString "CONSTRAINT " <> quoteSql nm <> byteString " ") nm <>
    buildSql c <>
    maybe mempty fromSqlConstraintAttributes attrs

instance IsSql92ColumnConstraintSyntax SqlSyntaxBuilder where
  type Sql92ColumnConstraintMatchTypeSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ColumnConstraintReferentialActionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92ColumnConstraintExpressionSyntax SqlSyntaxBuilder = SqlSyntaxBuilder

  notNullConstraintSyntax = SqlSyntaxBuilder (byteString "NOT NULL")
  uniqueColumnConstraintSyntax = SqlSyntaxBuilder (byteString "UNIQUE")
  primaryKeyColumnConstraintSyntax = SqlSyntaxBuilder (byteString "PRIMARY KEY")
  checkColumnConstraintSyntax e = SqlSyntaxBuilder ("CHECK (" <> buildSql e <> ")")
  referencesConstraintSyntax tbl fields match onUpdate onDelete =
    SqlSyntaxBuilder $
    "REFERENCES " <> quoteSql tbl <> "(" <>
    buildSepBy ", " (map quoteSql fields) <> ")" <>
    maybe mempty (\m -> " " <> buildSql m) match <>
    maybe mempty (\a -> " ON UPDATE " <> buildSql a) onUpdate <>
    maybe mempty (\a -> " ON DELETE " <> buildSql a) onDelete

instance IsSql92MatchTypeSyntax SqlSyntaxBuilder where
  fullMatchSyntax = SqlSyntaxBuilder "FULL"
  partialMatchSyntax = SqlSyntaxBuilder "PARTIAL"

instance IsSql92ReferentialActionSyntax SqlSyntaxBuilder where
  referentialActionCascadeSyntax = SqlSyntaxBuilder "CASCADE"
  referentialActionNoActionSyntax = SqlSyntaxBuilder "NO ACTION"
  referentialActionSetDefaultSyntax = SqlSyntaxBuilder "SET DEFAULT"
  referentialActionSetNullSyntax = SqlSyntaxBuilder "SET NULL"
