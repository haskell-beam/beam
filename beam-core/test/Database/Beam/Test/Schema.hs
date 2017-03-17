{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database.Beam.Test.Schema
  ( EmployeeT(..), DepartmentT(..)
  , RoleT(..), FunnyT (..)
  , EmployeeDb(..)
  , PrimaryKey(..)

  , DummyBackend, DummyFieldType(..)

  , employeeDbSettings

  , tests ) where

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Backend.SQL.AST

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Proxy

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Schema Tests"
                  [ basicSchemaGeneration
                  , automaticNestedFieldsAreUnset
                  , nullableForeignKeysGivenMaybeType
                  , underscoresAreHandledGracefully
                  , dbSchemaGeneration
                  , dbSchemaModification ]

data DummyBackend
instance SupportedSyntax DummyBackend Select
instance BeamBackend DummyBackend where
  data BackendColumnSchema DummyBackend
    = DummyField
    { dummyNested, dummyAuto :: Bool
    , dummyType :: DummyFieldType }
    deriving (Show, Eq)

data DummyFieldType
  = DummyFieldText
  | DummyFieldInt
  | DummyFieldDouble
  | DummyFieldUTCTime

  | DummyFieldMaybe DummyFieldType
    deriving (Show, Eq)

instance BeamColumnSchema (BackendColumnSchema DummyBackend) where
  maybeFieldSchema field = field { dummyType = DummyFieldMaybe (dummyType field) }
  autoSchema field = field { dummyAuto = True }
  nestedSchema field = field { dummyNested = True, dummyAuto = False }
instance HasDefaultFieldSchema DummyBackend a => HasDefaultFieldSchema DummyBackend (Maybe a) where
  defFieldSchema _ = maybeFieldSchema (defFieldSchema (Proxy @a))
instance HasDefaultFieldSchema DummyBackend Text where
  defFieldSchema _ = DummyField False False DummyFieldText
instance HasDefaultFieldSchema DummyBackend Int where
  defFieldSchema _ = DummyField False False DummyFieldInt
instance HasDefaultFieldSchema DummyBackend Double where
  defFieldSchema _ = DummyField False False DummyFieldDouble
instance HasDefaultFieldSchema DummyBackend UTCTime where
  defFieldSchema _ = DummyField False False DummyFieldUTCTime

data EmployeeT f
  = EmployeeT
  { _employeeFirstName :: Columnar f Text
  , _employeeLastName  :: Columnar f Text
  , _employeePhoneNumber :: Columnar f Text

  , _employeeAge       :: Columnar f Int
  , _employeeSalary    :: Columnar f Double

  , _employeeHireDate  :: Columnar f UTCTime
  , _employeeLeaveDate :: Columnar f (Maybe UTCTime)

  , _employeeCreated :: Columnar f (Auto UTCTime)
  } deriving Generic
instance Beamable EmployeeT
instance Table EmployeeT where
  data PrimaryKey EmployeeT f = EmployeeId (Columnar f Text) (Columnar f Text) (Columnar f (Auto UTCTime))
    deriving Generic
  primaryKey e = EmployeeId (_employeeFirstName e) (_employeeLastName e) (_employeeCreated e)
instance Beamable (PrimaryKey EmployeeT)
deriving instance Show (TableSettings DummyBackend EmployeeT)
deriving instance Eq (TableSettings DummyBackend EmployeeT)
deriving instance (Show (Columnar f Text), Show (Columnar f (Auto UTCTime))) => Show (PrimaryKey EmployeeT f)
deriving instance (Eq (Columnar f Text), Eq(Columnar f (Auto UTCTime))) => Eq (PrimaryKey EmployeeT f)

-- * Verify that the schema is generated properly

employeeTableSchema :: TableSettings DummyBackend EmployeeT
employeeTableSchema = defTblFieldSettings

expectedEmployeeTableSchema :: TableSettings DummyBackend EmployeeT
expectedEmployeeTableSchema =
  EmployeeT { _employeeFirstName = TableField "first_name"
                                              (DummyField False False DummyFieldText)
            , _employeeLastName = TableField "last_name"
                                             (DummyField False False DummyFieldText)
            , _employeePhoneNumber = TableField "phone_number"
                                                (DummyField False False DummyFieldText)
            , _employeeAge = TableField "age" (DummyField False False DummyFieldInt)
            , _employeeSalary = TableField "salary" (DummyField False False DummyFieldDouble)
            , _employeeHireDate = TableField "hire_date" (DummyField False False DummyFieldUTCTime)
            , _employeeLeaveDate = TableField "leave_date" (DummyField False False (DummyFieldMaybe DummyFieldUTCTime))
            , _employeeCreated = TableField "created" (DummyField False True DummyFieldUTCTime)
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
deriving instance Show (TableSettings DummyBackend (PrimaryKey RoleT))
deriving instance Eq (TableSettings DummyBackend (PrimaryKey RoleT))

roleTableSchema :: TableSettings DummyBackend RoleT
roleTableSchema = defTblFieldSettings

automaticNestedFieldsAreUnset :: TestTree
automaticNestedFieldsAreUnset =
  testCase "Automatic fields are unset when nesting" $
  do _roleForEmployee roleTableSchema @?=
       EmployeeId (TableField "for_employee__first_name" (DummyField True False DummyFieldText))
                  (TableField "for_employee__last_name" (DummyField True False DummyFieldText))
                  (TableField "for_employee__created" (DummyField True False DummyFieldUTCTime))

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
deriving instance Show (TableSettings DummyBackend DepartmentT)
deriving instance Eq (TableSettings DummyBackend DepartmentT)

departmentTableSchema :: TableSettings DummyBackend DepartmentT
departmentTableSchema = defTblFieldSettings

nullableForeignKeysGivenMaybeType :: TestTree
nullableForeignKeysGivenMaybeType =
  testCase "Nullable foreign keys are given maybe type" $
  do _departmentHead departmentTableSchema @?=
       EmployeeId (TableField "head__first_name" (DummyField True False (DummyFieldMaybe DummyFieldText)))
                  (TableField "head__last_name" (DummyField True False (DummyFieldMaybe DummyFieldText)))
                  (TableField "head__created" (DummyField True False (DummyFieldMaybe DummyFieldUTCTime)))

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

funnyTableSchema :: TableSettings DummyBackend FunnyT
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

-- * Database schema is derived correctly

data EmployeeDb tbl
  = EmployeeDb
    { _employees   :: tbl EmployeeT
    , _departments :: tbl DepartmentT
    , _roles       :: tbl RoleT
    , _funny       :: tbl FunnyT }
    deriving Generic
instance Database EmployeeDb

employeeDbSettings :: DatabaseSettings DummyBackend EmployeeDb
employeeDbSettings = defaultDbSettings

employeeDbSettingsModified :: DatabaseSettings DummyBackend EmployeeDb
employeeDbSettingsModified =
  defaultDbSettings `withDbModifications`
  (modifyingDb { _employees = tableModification (\_ -> "emps") tableFieldsModification
               , _departments = tableModification (\_ -> "depts")
                                                  (tableFieldsModification
                                                    { _departmentName = fieldModification (\_ -> "depts_name") id }) })

dbSchemaGeneration :: TestTree
dbSchemaGeneration =
  testCase "Database schema generation" $
  do let names = allTables (\(DatabaseTable _ nm _) -> nm) employeeDbSettings
     names @?= [ "employees"
               , "departments"
               , "roles"
               , "funny" ]

dbSchemaModification :: TestTree
dbSchemaModification =
  testCase "Database schema modification" $
  do let names = allTables (\(DatabaseTable _ nm _ ) -> nm) employeeDbSettingsModified
     names @?= [ "emps"
               , "depts"
               , "roles"
               , "funny" ]

     let DatabaseTable _ _ departmentT = _departments employeeDbSettingsModified
     departmentT @?= DepartmentT (TableField "depts_name" (DummyField False False DummyFieldText))
                                 (EmployeeId (TableField "head__first_name" (DummyField True False (DummyFieldMaybe DummyFieldText)))
                                             (TableField "head__last_name" (DummyField True False (DummyFieldMaybe DummyFieldText)))
                                             (TableField "head__created" (DummyField True False (DummyFieldMaybe DummyFieldUTCTime))))
