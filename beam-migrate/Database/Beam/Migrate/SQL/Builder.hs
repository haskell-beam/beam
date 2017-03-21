module Database.Beam.Migrate.SQL.Builder where

import Database.Beam.Backend.SQL.Builder

data SqlSyntaxBuilderCreateTableOptions
    = SqlSyntaxBuilderCreateTableOptions
        SqlSyntaxBuilder
        SqlSyntaxBuilder
    deriving (Show, Eq)

instance IsSql92TableConstraintSyntax SqlSyntaxBuilder where
  type Sql92CreateTableColumnSchemaSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92CreateTableTableConstraintSyntax SqlSyntaxBuilder = SqlSyntaxBuilder
  type Sql92CreateTableOptionsSyntax SqlSyntaxBuilder = SqlSyntaxBuilderCreateTableOptions

  createTableSyntax tableOptions tableName fieldSchemas constraints =
      SqlSyntaxBuilder $
      byteString "CREATE " <>
      maybe mempty (\b -> buildSql b <> byteString " ") beforeOptions <>
      byteString " TABLE " <>

      quotedIdentifier tableName <>

      byteString "(" <>
      buildSepBy (byteString ", ")
                 (map () fieldSchemas <>
                  map buildSql constraints) <>
      byteString ")" <>

      maybe mempty (\a -> buildSql a <> byteString " ") afterOptions

    where
      (beforeOptions, afterOptions) =
          case tableOptions of
            Just (SqlSyntaxBuliderCreateTableOptions b a) -> (Just b, Just a)
            Nothing -> (Nothing, Nothing)
