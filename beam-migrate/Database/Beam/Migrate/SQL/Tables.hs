module Database.Beam.Migrate.SQL.Tables where

import Database.Beam
import Database.Beam.Migrate.Types
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.SQL.SQL92

import Data.Text (Text)
import Data.Proxy (Proxy(..))

createTable :: ( Beamable table, Table table
               , IsSql92DdlCommandSyntax syntax ) =>
               Text -> TableSchema (Sql92CreateTableColumnSchemaSyntax (Sql92DdlCommandCreateTableSyntax syntax)) table
            -> Migration syntax (CheckedDatabaseEntity be db (TableEntity table))
createTable tblName tblSettings =
  do let createTableCommand =
           createTableSyntax Nothing
                             tblName
                             (allBeamValues (\(Columnar' (TableFieldSchema name (FieldSchema schema) _)) -> (name, schema)) tblSettings)
                             [ primaryKeyConstraintSyntax (allBeamValues (\(Columnar' (TableFieldSchema name _ _)) -> name) (primaryKey tblSettings)) ]

         command = createTableCmd createTableCommand

         tbl' = changeBeamRep (\(Columnar' (TableFieldSchema name _ _)) -> Columnar' (TableField name)) tblSettings
--         fieldChecks = concat (allBeamValues (\(Columnar' (TableFieldSchema nm _ cs)) -> map (TableFieldCheck nm) cs) tblSettings)
         tblChecks = []

     upDown command Nothing
     pure (CheckedDatabaseEntity (CheckedDatabaseTable (DatabaseTable tblName tbl') tblChecks) [])
