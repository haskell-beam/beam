module Database.Beam.Migrate.SQL.Tables where

import Database.Beam
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.SQL.SQL92

import Data.Text (Text)

createTable :: ( Beamable tbl, IsSql92CreateTableSyntax (Sql92DdlCommandCreateTableSyntax syntax) ) =>
               Text -> TableSchema (Sql92CreateTableColumnSchemaSyntax (Sql92DdlCommandCreateTableSyntax syntax)) tbl
            -> MigrationStep syntax (DatabaseTable db table)
createTable tblName tblSettings =
    let createTableCommand =
            createTableSyntax Nothing
                              tblName
                              (allBeamValues (\(Columnar' (TableFieldSchema name schema)) -> (name, schema)) tblSettings)
                              [ primaryKeyConstraintSyntax (allBeamValues (\(Columnar' (TableFieldSchema name _)) -> name) tblSettings) ]

        command = createTableCommandSyntax createTableCommand

        tbl' = changeBeamRep (\(Columnar' (TableFieldSchema name _)) -> Columnar' (TableField name)) tblSettings
    in liftF (MigrationStep command Nothing (DatabaseTable Proxy tblName tbl'))
