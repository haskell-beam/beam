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
            -> Migration syntax (DatabaseTable db table)
createTable tblName tblSettings =
  do let createTableCommand =
           createTableSyntax Nothing
                             tblName
                             (allBeamValues (\(Columnar' (TableFieldSchema name (FieldSchema schema))) -> (name, schema)) tblSettings)
                             [ primaryKeyConstraintSyntax (allBeamValues (\(Columnar' (TableFieldSchema name _)) -> name) (primaryKey tblSettings)) ]

         command = createTableCmd createTableCommand

         tbl' = changeBeamRep (\(Columnar' (TableFieldSchema name _)) -> Columnar' (TableField name)) tblSettings

     upDown command Nothing
     pure (DatabaseTable Proxy tblName tbl')
