module Database.Beam.Migrate.SQL.Builder where

import Database.Beam.Backend.SQL.Builder
import Database.Beam.Migrate.SQL

import Control.Applicative

import Data.ByteString.Builder (byteString)
import Data.Monoid

data SqlSyntaxBuilderCreateTableOptions
    = SqlSyntaxBuilderCreateTableOptions
        SqlSyntaxBuilder
        SqlSyntaxBuilder
    deriving Eq

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

instance IsSql92ConstraintAttributesSyntax SqlConstraintAttributesBuilder where
  initiallyDeferredAttributeSyntax = SqlConstraintAttributesBuilder (Just InitiallyDeferred) Nothing
  initiallyImmediateAttributeSyntax = SqlConstraintAttributesBuilder (Just InitiallyImmediate) Nothing
  deferrableAttributeSyntax = SqlConstraintAttributesBuilder Nothing (Just True)
  notDeferrableAttributeSyntax = SqlConstraintAttributesBuilder Nothing (Just False)
