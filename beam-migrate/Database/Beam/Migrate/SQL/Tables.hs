{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Beam.Migrate.SQL.Tables
  ( -- * Table manipulation

    -- ** Creation and deletion
    createTable, dropTable
  , preserve

    -- ** @ALTER TABLE@
  , TableMigration(..)
  , ColumnMigration(..)
  , alterTable

  , renameTableTo, renameColumnTo
  , addColumn, dropColumn

    -- * Field specification
  , DefaultValue, Constraint(..), NotNullConstraint

  , field

  , defaultTo_, notNull, unique

    -- ** Internal classes
    --    Provided without documentation for use in type signatures
  , FieldReturnType(..)
  ) where

import Database.Beam
import Database.Beam.Schema.Tables
import Database.Beam.Backend.SQL

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Checks
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.SQL.SQL92

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Writer.Strict
import Control.Monad.State

import Data.Text (Text)
import Data.Typeable
import qualified Data.Kind as Kind (Constraint)

import GHC.TypeLits

-- * Table manipulation

-- | Add a @CREATE TABLE@ statement to this migration
--
--   The first argument is the name of the table.
--
--   The second argument is a table containing a 'FieldSchema' for each field.
--   See documentation on the 'Field' command for more information.c
createTable :: ( Beamable table, Table table
<<<<<<< HEAD
               , BeamMigrateSqlBackend be ) =>
               Text -> TableSchema be  table
=======
               , BeamMigrateSqlBackend be )
            => Text -> TableSchema be table
>>>>>>> 651b2dc3fd84649093759f02cacaa13467bc7a0a
            -> Migration be (CheckedDatabaseEntity be db (TableEntity table))
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

-- | Add a @DROP TABLE@ statement to this migration.
dropTable :: BeamMigrateSqlBackend be
          => CheckedDatabaseEntity be db (TableEntity table)
          -> Migration be ()
dropTable (CheckedDatabaseEntity (CheckedDatabaseTable (DatabaseTable tblNm _) _ _) _) =
  let command = dropTableCmd (dropTableSyntax tblNm)
  in upDown command Nothing

-- | Copy a table schema from one database to another
preserve :: CheckedDatabaseEntity be db e
         -> Migration be (CheckedDatabaseEntity be db' e)
preserve (CheckedDatabaseEntity desc checks) = pure (CheckedDatabaseEntity desc checks)

-- * Alter table

-- | A column in the process of being altered
data ColumnMigration a
  = ColumnMigration
  { columnMigrationFieldName :: Text
  , columnMigrationFieldChecks :: [FieldCheck] }

-- | Monad representing a series of @ALTER TABLE@ statements
newtype TableMigration be a
<<<<<<< HEAD
  = TableMigration (WriterT [Sql92DdlCommandAlterTableSyntax (BeamSqlBackendSyntax be)] (State (Text, [TableCheck])) a)
=======
  = TableMigration (WriterT [BeamSqlBackendAlterTableSyntax be] (State (Text, [TableCheck])) a)
>>>>>>> 651b2dc3fd84649093759f02cacaa13467bc7a0a
  deriving (Monad, Applicative, Functor)

-- | @ALTER TABLE ... RENAME TO@ command
renameTableTo :: BeamMigrateSqlBackend be
              => Text -> table ColumnMigration
              -> TableMigration be (table ColumnMigration)
renameTableTo newName oldTbl = TableMigration $ do
  (curNm, chks) <- get
  tell [ alterTableSyntax curNm (renameTableToSyntax newName) ]
  put (newName, chks)
  return oldTbl

-- | @ALTER TABLE ... RENAME COLUMN ... TO ...@ command
renameColumnTo :: BeamMigrateSqlBackend be
               => Text -> ColumnMigration a
               -> TableMigration be (ColumnMigration a)
renameColumnTo newName column = TableMigration $ do
  (curTblNm, _) <- get
  tell [ alterTableSyntax curTblNm
           (renameColumnToSyntax (columnMigrationFieldName column) newName) ]
  pure column { columnMigrationFieldName = newName }

-- | @ALTER TABLE ... DROP COLUMN ...@ command
dropColumn :: BeamMigrateSqlBackend be
           => ColumnMigration a -> TableMigration be ()
dropColumn column = TableMigration $ do
  (curTblNm, _)<- get
  tell [ alterTableSyntax curTblNm (dropColumnSyntax (columnMigrationFieldName column)) ]

-- | @ALTER TABLE ... ADD COLUMN ...@ command
addColumn :: BeamMigrateSqlBackend be
          => TableFieldSchema be a
          -> TableMigration be (ColumnMigration a)
addColumn (TableFieldSchema nm (FieldSchema fieldSchemaSyntax) checks) =
  TableMigration $
  do (curTblNm, _) <- get
     tell [ alterTableSyntax curTblNm (addColumnSyntax nm fieldSchemaSyntax) ]
     pure (ColumnMigration nm checks)

-- | Compose a series of @ALTER TABLE@ commands
--
--   Example usage
--
-- @
-- migrate (OldDb oldTbl) = do
--   alterTable oldTbl $ \oldTbl' ->
--     field2 <- renameColumnTo "NewNameForField2" (_field2 oldTbl')
--     dropColumn (_field3 oldTbl')
--     renameTableTo "NewTableName"
--     field4 <- addColumn (field "ANewColumn" smallint notNull (defaultTo_ (val_ 0)))
--     return (NewTable (_field1 oldTbl') field2 field4)
-- @
--
--   The above would result in commands like:
--
-- @
-- ALTER TABLE <oldtable> RENAME COLUMN <field2> TO "NewNameForField2";
-- ALTER TABLE <oldtable> DROP COLUMN <field3>;
-- ALTER TABLE <oldtable> RENAME TO "NewTableName";
-- ALTER TABLE "NewTableName" ADD COLUMN "ANewColumn" SMALLINT NOT NULL DEFAULT 0;
-- @
--
<<<<<<< HEAD
alterTable :: forall be db db' table table' syntax
=======
alterTable :: forall be db db' table table'
>>>>>>> 651b2dc3fd84649093759f02cacaa13467bc7a0a
            . (Table table', BeamMigrateSqlBackend be)
           => CheckedDatabaseEntity be db (TableEntity table)
           -> (table ColumnMigration -> TableMigration be (table' ColumnMigration))
           -> Migration be (CheckedDatabaseEntity be db' (TableEntity table'))
alterTable (CheckedDatabaseEntity (CheckedDatabaseTable (DatabaseTable tblNm tbl) tblChecks tblFieldChecks) entityChecks) alterColumns =
 let initialTbl = runIdentity $
                  zipBeamFieldsM
                      (\(Columnar' (TableField nm) :: Columnar' (TableField table) x)
                        (Columnar' (Const checks) :: Columnar' (Const [FieldCheck]) x) ->
                         pure (Columnar' (ColumnMigration nm checks)
                               :: Columnar' ColumnMigration x))
                      tbl tblFieldChecks

     TableMigration alterColumns' = alterColumns initialTbl
     ((newTbl, cmds), (tblNm', tblChecks')) = runState (runWriterT alterColumns') (tblNm, tblChecks)

     fieldChecks' = changeBeamRep (\(Columnar' (ColumnMigration _ checks) :: Columnar' ColumnMigration a) ->
                                     Columnar' (Const checks) :: Columnar' (Const [FieldCheck]) a)
                                  newTbl
     tbl' = changeBeamRep (\(Columnar' (ColumnMigration nm _) :: Columnar' ColumnMigration a) ->
                              Columnar' (TableField nm) :: Columnar' (TableField table') a)
                          newTbl
 in forM_ cmds (\cmd -> upDown (alterTableCmd cmd) Nothing) >>
    pure (CheckedDatabaseEntity (CheckedDatabaseTable (DatabaseTable tblNm' tbl') tblChecks' fieldChecks') entityChecks)

-- * Fields

-- | Build a schema for a field. This function takes the name and type of the
-- field and a variable number of modifiers, such as constraints and default
-- values. GHC will complain at you if the modifiers do not make sense. For
-- example, you cannot apply the 'notNull' constraint to a column with a 'Maybe'
-- type.
--
-- Example of creating a table named "Employee" with three columns: "FirstName",
-- "LastName", and "HireDate"
--
-- @
-- data Employee f =
--   Employee { _firstName :: C f Text
--            , _lastName  :: C f Text
--            , _hireDate  :: C f (Maybe LocalTime)
--            } deriving Generic
-- instance Beamable Employee
--
-- instance Table Employee where
--    data PrimaryKey Employee f = EmployeeKey (C f Text) (C f Text) deriving Generic
--    primaryKey = EmployeeKey \<$\> _firstName \<*\> _lastName
--
-- instance Beamable PrimaryKey Employee f
--
-- data EmployeeDb entity
--     = EmployeeDb { _employees :: entity (TableEntity Employee) }
--     deriving Generic
-- instance Database EmployeeDb
--
-- migration :: IsSql92DdlCommandSyntax syntax => Migration syntax () EmployeeDb
-- migration = do
--   employees <- createTable "EmployeesTable"
--                  (Employee (field "FirstNameField" (varchar (Just 15)) notNull)
--                            (field "last_name" (varchar Nothing) notNull (defaultTo_ (val_ "Smith")))
--                            (field "hiredDate" (maybeType timestamp)))
--   return (EmployeeDb employees)
-- @
field :: ( BeamMigrateSqlBackend be
         , FieldReturnType 'False 'False be resTy a )
      => Text -> DataType be resTy -> a
field name (DataType ty) = field' (Proxy @'False) (Proxy @'False) name ty Nothing Nothing []

-- ** Default values

-- | Represents the default value of a field with a given column schema syntax and type
newtype DefaultValue be a = DefaultValue (BeamSqlBackendExpressionSyntax be)

-- | Build a 'DefaultValue' from a 'QExpr'. GHC will complain if you supply more
-- than one default value.
defaultTo_ :: BeamMigrateSqlBackend be
           => (forall s. QExpr be s a)
           -> DefaultValue be a
defaultTo_ (QExpr e) =
  DefaultValue (e "t")

-- ** Constraints

-- | Represents a constraint in the given column schema syntax
newtype Constraint be
  = Constraint (BeamSqlBackendConstraintSyntax be)

newtype NotNullConstraint be
  = NotNullConstraint (Constraint be)

-- | The SQL92 @NOT NULL@ constraint
notNull :: BeamMigrateSqlBackend be => NotNullConstraint be
notNull = NotNullConstraint (Constraint notNullConstraintSyntax)

-- | SQL @UNIQUE@ constraint
unique :: BeamMigrateSqlBackend be => Constraint be
unique = Constraint uniqueColumnConstraintSyntax

-- ** 'field' variable arity classes

class FieldReturnType (defaultGiven :: Bool) (collationGiven :: Bool) be resTy a | a -> be resTy where
  field' :: BeamMigrateSqlBackend be
         => Proxy defaultGiven -> Proxy collationGiven
         -> Text
         -> BeamSqlBackendDataTypeSyntax be
         -> Maybe (BeamSqlBackendExpressionSyntax be)
         -> Maybe Text -> [ BeamSqlBackendColumnConstraintDefinitionSyntax be ]
         -> a

instance FieldReturnType 'True collationGiven be resTy a =>
  FieldReturnType 'False collationGiven be resTy (DefaultValue be resTy -> a) where
  field' _ collationGiven nm ty _ collation constraints (DefaultValue e) =
    field' (Proxy @'True) collationGiven nm ty (Just e) collation constraints

instance FieldReturnType defaultGiven collationGiven be resTy a =>
  FieldReturnType defaultGiven collationGiven be resTy (Constraint be -> a) where
  field' defaultGiven collationGiven nm ty default_' collation constraints (Constraint e) =
    field' defaultGiven collationGiven nm ty default_' collation (constraints ++ [ constraintDefinitionSyntax Nothing e Nothing ])

instance ( FieldReturnType defaultGiven collationGiven be resTy (Constraint be -> a)
         , IsNotNull resTy ) =>
  FieldReturnType defaultGiven collationGiven be resTy (NotNullConstraint be -> a) where
  field' defaultGiven collationGiven nm ty default_' collation constraints (NotNullConstraint c) =
    field' defaultGiven collationGiven nm ty default_' collation constraints c

instance ( FieldReturnType 'True collationGiven be resTy a
         , TypeError ('Text "Only one DEFAULT clause can be given per 'field' invocation") ) =>
  FieldReturnType 'True collationGiven be resTy (DefaultValue be resTy -> a) where

  field' = error "Unreachable because of GHC Custom Type Errors"

instance ( FieldReturnType defaultGiven collationGiven be resTy a
         , TypeError ('Text "Only one type declaration allowed per 'field' invocation")) =>
  FieldReturnType defaultGiven collationGiven be resTy (DataType be' x -> a) where
  field' = error "Unreachable because of GHC Custom Type Errors"

instance BeamMigrateSqlBackend be =>
  FieldReturnType defaultGiven collationGiven be resTy (TableFieldSchema be resTy) where
  field' _ _ nm ty default_' collation constraints =
    TableFieldSchema nm (FieldSchema (columnSchemaSyntax ty default_' constraints collation)) checks
    where checks = [ FieldCheck (\tbl field'' -> SomeDatabasePredicate (TableHasColumn tbl field'' ty :: TableHasColumn be)) ] ++
                   map (\cns -> FieldCheck (\tbl field'' -> SomeDatabasePredicate (TableColumnHasConstraint tbl field'' cns :: TableColumnHasConstraint be))) constraints

type family IsNotNull (x :: *) :: Kind.Constraint where
  IsNotNull (Maybe x) = TypeError ('Text "You used Database.Beam.Migrate.notNull on a column with type" ':$$:
                                   'ShowType (Maybe x) ':$$:
                                   'Text "Either remove 'notNull' from your migration or 'Maybe' from your table")
  IsNotNull x = ()
