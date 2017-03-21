module Database.Beam.Migrate.SQL.SQL92 where

import Database.Beam

import Data.Text (Text)

class IsSql92DdlCommand syntax where
  type Sql92DdlCommandCreateTableSyntax syntax :: *

  createTableCmd :: Sql92DdlCommandCreateTableSyntax syntax -> syntax

class IsSql92TableConstraintSyntax (Sql92CreateTableTableConstraintSyntax syntax) =>
    IsSql92CreateTableSyntax syntax where
  type Sql92CreateTableColumnSchemaSyntax syntax :: *
  type Sql92CreateTableTableConstraintSyntax syntax :: *
  type Sql92CreateTableOptionsSyntax syntax :: *

  createTableSyntax :: Maybe (Sql92CreateTableOptionsSyntax syntax)
                    -> Text
                    -> [(Text, Sql92CreateTableColumnSchemaSyntax syntax)]
                    -> [ Sql92CreateTableTableConstraintSyntax syntax ]
                    -> syntax

class IsSql92ColumnSchemaSyntax columnSchema where
  type Sql92ColumnSchemaColumnTypeSyntax columnSchema :: *
  type Sql92ColumnSchemaExpressionSyntax columnSchema :: *
  type Sql92ColumnSchemaColumnConstraintSyntax columnSchema :: *

  columnSchemaSyntax :: Sql92ColumnSchemaColumnTypeSyntax columnSchema
                     -> Maybe (Sql92ColumnSchemaExpressionSyntax columnSchema)
                     -> [ Sql92ColumnSchemaColumnConstraintSyntax columnSchema ]
                     -> Maybe Text
                     -> columnSchema

class IsSql92TableConstraintSyntax constraint where
  primaryKeyConstraintSyntax :: [ Text ] -> constraint
