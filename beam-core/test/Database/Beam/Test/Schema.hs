{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database.Beam.Test.Schema
  ( EmployeeT(..), DepartmentT(..)
  , RoleT(..), FunnyT (..)
  , EmployeeDb(..)
  , PrimaryKey(..)

  , DummyBackend

  , employeeDbSettings

  , tests ) where

import           Database.Beam
import           Database.Beam.Schema.Tables
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL.AST

import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Schema Tests"
                  [ basicSchemaGeneration
                  , ruleBasedRenaming
--                  , automaticNestedFieldsAreUnset
--                  , nullableForeignKeysGivenMaybeType
                  , underscoresAreHandledGracefully ]
--                  , dbSchemaGeneration ]
--                  , dbSchemaModification ]

data DummyBackend

instance BeamBackend DummyBackend where
  -- Pretty much everything we can shove in a database satisfies show
  type BackendFromField DummyBackend = Show

data EmployeeT f
  = EmployeeT
  { _employeeFirstName :: Columnar f Text
  , _employeeLastName  :: Columnar f Text
  , _employeePhoneNumber :: Columnar f Text

  , _employeeAge       :: Columnar f Int
  , _employeeSalary    :: Columnar f Double

  , _employeeHireDate  :: Columnar f UTCTime
  , _employeeLeaveDate :: Columnar f (Maybe UTCTime)

  , _employeeCreated :: Columnar f UTCTime
  } deriving Generic
instance Beamable EmployeeT
instance Table EmployeeT where
  data PrimaryKey EmployeeT f = EmployeeId (Columnar f Text) (Columnar f Text) (Columnar f UTCTime)
    deriving Generic
  primaryKey e = EmployeeId (_employeeFirstName e) (_employeeLastName e) (_employeeCreated e)
instance Beamable (PrimaryKey EmployeeT)
deriving instance Show (TableSettings EmployeeT)
deriving instance Eq (TableSettings EmployeeT)
deriving instance (Show (Columnar f Text), Show (Columnar f UTCTime)) => Show (PrimaryKey EmployeeT f)
deriving instance (Eq (Columnar f Text), Eq (Columnar f  UTCTime)) => Eq (PrimaryKey EmployeeT f)

-- * Verify that the schema is generated properly

employeeTableSchema :: TableSettings EmployeeT
employeeTableSchema = defTblFieldSettings

expectedEmployeeTableSchema :: TableSettings EmployeeT
expectedEmployeeTableSchema =
  EmployeeT { _employeeFirstName = TableField "first_name"
            , _employeeLastName = TableField "last_name"
            , _employeePhoneNumber = TableField "phone_number"
            , _employeeAge = TableField "age"
            , _employeeSalary = TableField "salary"
            , _employeeHireDate = TableField "hire_date"
            , _employeeLeaveDate = TableField "leave_date"
            , _employeeCreated = TableField "created"
            }

basicSchemaGeneration :: TestTree
basicSchemaGeneration =
  testCase "Basic Schema Generation" $
  do _employeeFirstName employeeTableSchema @?= _employeeFirstName expectedEmployeeTableSchema
     _employeeLastName employeeTableSchema @?= _employeeLastName expectedEmployeeTableSchema
     _employeePhoneNumber employeeTableSchema @?= _employeePhoneNumber expectedEmployeeTableSchema
     _employeeAge employeeTableSchema @?= _employeeAge expectedEmployeeTableSchema
     _employeeSalary employeeTableSchema @?= _employeeSalary expectedEmployeeTableSchema
     _employeeHireDate employeeTableSchema @?= _employeeHireDate expectedEmployeeTableSchema
     _employeeLeaveDate employeeTableSchema @?= _employeeLeaveDate expectedEmployeeTableSchema
     _employeeCreated employeeTableSchema @?= _employeeCreated expectedEmployeeTableSchema

-- * Ensure that automatic fields are unset when nesting

data RoleT f
  = RoleT
  { _roleForEmployee :: PrimaryKey EmployeeT f
  , _roleName :: Columnar f Text
  , _roleStarted :: Columnar f UTCTime }
  deriving Generic
instance Beamable RoleT
instance Table RoleT where
  data PrimaryKey RoleT f = RoleId (PrimaryKey EmployeeT f) (Columnar f UTCTime)
    deriving Generic
  primaryKey (RoleT e _ s) = RoleId e s
instance Beamable (PrimaryKey RoleT)
deriving instance Show (TableSettings (PrimaryKey RoleT))
deriving instance Eq (TableSettings (PrimaryKey RoleT))

roleTableSchema :: TableSettings RoleT
roleTableSchema = defTblFieldSettings

-- * Ensure that fields of a nullable primary key are given the proper Maybe type

data DepartmentT f
  = DepartmentT
  { _departmentName :: Columnar f Text
  , _departmentHead :: PrimaryKey EmployeeT (Nullable f) -- ^ Departments may currently lack a department head
  } deriving Generic
instance Beamable DepartmentT
instance Table DepartmentT where
  data PrimaryKey DepartmentT f = DepartmentId (Columnar f Text) deriving Generic
  primaryKey (DepartmentT name _) = DepartmentId name
instance Beamable (PrimaryKey DepartmentT)
deriving instance Show (TableSettings DepartmentT)
deriving instance Eq (TableSettings DepartmentT)

departmentTableSchema :: TableSettings DepartmentT
departmentTableSchema = defTblFieldSettings

-- nullableForeignKeysGivenMaybeType :: TestTree
-- nullableForeignKeysGivenMaybeType =
--   testCase "Nullable foreign keys are given maybe type" $
--   do _departmentHead departmentTableSchema @?=
--        EmployeeId (TableField "head__first_name" (DummyField True False (DummyFieldMaybe DummyFieldText)))
--                   (TableField "head__last_name" (DummyField True False (DummyFieldMaybe DummyFieldText)))
--                   (TableField "head__created" (DummyField True False (DummyFieldMaybe DummyFieldUTCTime)))

-- * Ensure that fields with underscores are handled properly

data FunnyT f
  = FunnyT
  { funny_field1 :: Columnar f Text
  , funny_field_2 :: Columnar f Text
  , funny_first_name :: Columnar f Text
  , _funny_lastName :: Columnar f Text
  , _funny_middle_Name :: Columnar f Text
  , ___ :: Columnar f Int }
  deriving Generic
instance Beamable FunnyT
instance Table FunnyT where
  data PrimaryKey FunnyT f = FunnyId (Columnar f Text) deriving Generic
  primaryKey = FunnyId . funny_field1
instance Beamable (PrimaryKey FunnyT)

funnyTableSchema :: TableSettings FunnyT
funnyTableSchema = defTblFieldSettings

underscoresAreHandledGracefully :: TestTree
underscoresAreHandledGracefully =
  testCase "Underscores in field names are handled gracefully" $
  do let fieldNames = allBeamValues (\(Columnar' f) -> _fieldName f) funnyTableSchema
     fieldNames @?= [ "funny_field1"
                    , "funny_field_2"
                    , "funny_first_name"
                    , "funny_lastName"
                    , "funny_middle_Name"
                    , "___" ]

ruleBasedRenaming :: TestTree
ruleBasedRenaming =
  testCase "Rule based renaming works correctly" $
  do let (DatabaseEntity (DatabaseTable _ funny)) = _funny employeeDbSettingsRuleMods
         (DatabaseEntity (DatabaseTable _ departments)) = _departments employeeDbSettingsRuleMods

         funnyFieldNames = allBeamValues (\(Columnar' f) -> _fieldName f) funny
         deptFieldNames = allBeamValues (\(Columnar' f) -> _fieldName f) departments

     funnyFieldNames @?= [ "pfx_funny_field1"
                         , "pfx_funny_field_2"
                         , "pfx_funny_first_name"
                         , "pfx_funny_lastName"
                         , "pfx_funny_middle_Name"
                         , "___" ]

     deptFieldNames @?= [ "name", "head__first_name", "head__last_name", "head__created" ]

-- * Database schema is derived correctly

data EmployeeDb f
  = EmployeeDb
    { _employees   :: f (TableEntity EmployeeT)
    , _departments :: f (TableEntity DepartmentT)
    , _roles       :: f (TableEntity RoleT)
    , _funny       :: f (TableEntity FunnyT) }
    deriving Generic
instance Database be EmployeeDb

employeeDbSettings :: DatabaseSettings be EmployeeDb
employeeDbSettings = defaultDbSettings

employeeDbSettingsRuleMods :: DatabaseSettings be EmployeeDb
employeeDbSettingsRuleMods = defaultDbSettings `withDbModification`
                             renamingFields (\field ->
                                                case T.stripPrefix "funny" field of
                                                  Nothing -> field
                                                  Just fieldNm -> "pfx_" <> field)

-- employeeDbSettingsModified :: DatabaseSettings EmployeeDb
-- employeeDbSettingsModified =
--   defaultDbSettings `withDbModifications`
--   (modifyingDb { _employees = tableModification (\_ -> "emps") tableFieldsModification
--                , _departments = tableModification (\_ -> "depts")
--                                                   (tableFieldsModification
--                                                     { _departmentName = fieldModification (\_ -> "depts_name") id }) })

-- dbSchemaGeneration :: TestTree
-- dbSchemaGeneration =
--   testCase "Database schema generation" $
--   do let names = allTables (\(DatabaseTable _ nm _) -> nm) employeeDbSettings
--      names @?= [ "employees"
--                , "departments"
--                , "roles"
--                , "funny" ]

-- dbSchemaModification :: TestTree
-- dbSchemaModification =
--   testCase "Database schema modification" $
--   do let names = allTables (\(DatabaseTable _ nm _ ) -> nm) employeeDbSettingsModified
--      names @?= [ "emps"
--                , "depts"
--                , "roles"
--                , "funny" ]

--      let DatabaseTable _ _ departmentT = _departments employeeDbSettingsModified
--      departmentT @?= DepartmentT (TableField "depts_name" (DummyField False False DummyFieldText))
--                                  (EmployeeId (TableField "head__first_name" (DummyField True False (DummyFieldMaybe DummyFieldText)))
--                                              (TableField "head__last_name" (DummyField True False (DummyFieldMaybe DummyFieldText)))
--                                              (TableField "head__created" (DummyField True False (DummyFieldMaybe DummyFieldUTCTime))))
