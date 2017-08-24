module Database.Beam.Migrate.SQL.Tables where

import Database.Beam
import Database.Beam.Schema.Tables

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Checks
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.SQL.SQL92

import Control.Applicative

import Data.Text (Text)

-- * Table manipulation

createTable :: ( Beamable table, Table table
               , IsSql92DdlCommandSyntax syntax ) =>
               Text -> TableSchema (Sql92CreateTableColumnSchemaSyntax (Sql92DdlCommandCreateTableSyntax syntax)) table
            -> Migration syntax (CheckedDatabaseEntity be db (TableEntity table))
createTable newTblName tblSettings =
  do let createTableCommand =
           createTableSyntax Nothing newTblName
                             (allBeamValues (\(Columnar' (TableFieldSchema name (FieldSchema schema) _)) -> (name, schema)) tblSettings)
                             [ primaryKeyConstraintSyntax (allBeamValues (\(Columnar' (TableFieldSchema name _ _)) -> name) (primaryKey tblSettings)) ]

         command = createTableCmd createTableCommand

         tbl' = changeBeamRep (\(Columnar' (TableFieldSchema name _ _)) -> Columnar' (TableField name)) tblSettings

         fieldChecks = changeBeamRep (\(Columnar' (TableFieldSchema _ _ cs)) -> Columnar' (Const cs)) tblSettings

         tblChecks = [ TableCheck (\tblName _ -> SomeDatabasePredicate (TableExistsPredicate tblName)) ] ++
                     primaryKeyCheck

         primaryKeyCheck =
           case allBeamValues (\(Columnar' (TableFieldSchema name _ _)) -> name) (primaryKey tblSettings) of
             [] -> []
             cols -> [ TableCheck (\tblName _ -> SomeDatabasePredicate (TableHasPrimaryKey tblName cols)) ]

     upDown command Nothing
     pure (CheckedDatabaseEntity (CheckedDatabaseTable (DatabaseTable newTblName tbl') tblChecks fieldChecks) [])

dropTable :: IsSql92DdlCommandSyntax syntax
          => CheckedDatabaseEntity be db (TableEntity table)
          -> Migration syntax ()
dropTable (CheckedDatabaseEntity (CheckedDatabaseTable (DatabaseTable tblNm _) _) _) =
  let command = dropTableCmd (dropTableSyntax tblNm)
  in upDown command Nothing

preserve :: CheckedDatabaseEntity be db e
         -> Migration syntax (CheckedDatabaseEntity be db' e)
preserve (CheckedDatabaseEntity desc checks) = pure (CheckedDatabaseEntity desc checks)

-- * Alter table

-- alterTable :: CheckedDatabaseEntity be db (TableEntity table)
--            -> (forall f m. table f -> m (table' f))
--            -> Migration syntax (CheckedDatabaseEntity be db' (TableEntity table'))
-- alterTable _ _  = blah
