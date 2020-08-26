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

import           Data.Int
import           Data.List.NonEmpty ( NonEmpty((:|)) )
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Schema Tests"
                  [ basicSchemaGeneration
                  , ruleBasedRenaming
                  , parametricBeamSchemaGeneration
                  , parametricAndFixedNestedBeamsAreEquivalent
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

  , _employeeAge       :: Columnar f Int32
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
  EmployeeT { _employeeFirstName = TableField (pure "_employeeFirstName") "first_name"
            , _employeeLastName = TableField (pure "_employeeLastName") "last_name"
            , _employeePhoneNumber = TableField (pure "_employeePhoneNumber") "phone_number"
            , _employeeAge = TableField (pure "_employeeAge") "age"
            , _employeeSalary = TableField (pure "_employeeSalary") "salary"
            , _employeeHireDate = TableField (pure "_employeeHireDate") "hire_date"
            , _employeeLeaveDate = TableField (pure "_employeeLeaveDate") "leave_date"
            , _employeeCreated = TableField (pure "_employeeCreated") "created"
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
  , ___ :: Columnar f Int32 }
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
  do let fieldNames = allBeamValues (\(Columnar' f) -> (_fieldPath f, _fieldName f)) funnyTableSchema
     fieldNames @?= [ ( pure "funny_field1", "funny_field1")
                    , ( pure "funny_field_2", "funny_field_2")
                    , ( pure "funny_first_name", "funny_first_name")
                    , ( pure "_funny_lastName", "funny_lastName")
                    , ( pure "_funny_middle_Name", "funny_middle_Name")
                    , ( pure "___", "___") ]

ruleBasedRenaming :: TestTree
ruleBasedRenaming =
  testCase "Rule based renaming works correctly" $
  do let (DatabaseEntity (DatabaseTable { dbTableSettings = funny })) = _funny employeeDbSettingsRuleMods
         (DatabaseEntity (DatabaseTable { dbTableSettings = departments })) = _departments employeeDbSettingsRuleMods

         funnyFieldNames = allBeamValues (\(Columnar' f) -> (_fieldPath f, _fieldName f)) funny
         deptFieldNames = allBeamValues (\(Columnar' f) -> (_fieldPath f, _fieldName f)) departments

     funnyFieldNames @?= [ ( pure "funny_field1", "pfx_funny_field1")
                         , ( pure "funny_field_2", "pfx_funny_field_2")
                         , ( pure "funny_first_name", "pfx_funny_first_name")
                         , ( pure "_funny_lastName", "pfx_funny_lastName")
                         , ( pure "_funny_middle_Name", "pfx_funny_middle_Name")
                         , ( pure "___", "___") ]

     deptFieldNames @?= [ (pure "_departmentName", "name")
                        , ( "_departmentHead" :| [ "_employeeFirstName" ], "head__first_name")
                        , ( "_departmentHead" :| [ "_employeeLastName" ], "head__last_name")
                        , ( "_departmentHead" :| [ "_employeeCreated" ], "head__created") ]

-- * Ensure it is possible to use nested paremetric beams
parametricBeamSchemaGeneration :: TestTree
parametricBeamSchemaGeneration =
  testCase "Parametric schema generation" $
  do let (DatabaseEntity (DatabaseTable { dbTableSettings = deptVehiculesA })) = _departmentVehiculesA employeeDbSettings
         deptVehiculesFieldNamesA = allBeamValues (\(Columnar' f) -> (_fieldPath f, _fieldName f)) deptVehiculesA

     deptVehiculesFieldNamesA @?= [ ( "_aDepartament" :| [ "_departmentName" ] , "departament__name")
                                  , ( "_aRelatesTo"   :| [ "_vehiculeId" ], "relates_to__id")
                                  , ( "_aRelatesTo"   :| [ "_vehiculeType" ], "relates_to__type")
                                  , ( "_aRelatesTo"   :| [ "_numberOfWheels" ], "relates_to__of_wheels")
                                  , ( "_aMetaInfo"    :| [ "_price" ], "meta_info__price")
                                  ]

-- * Ensure it doesn't matter whether we abstract over a beam's parameters, or if we them fixed.
parametricAndFixedNestedBeamsAreEquivalent :: TestTree
parametricAndFixedNestedBeamsAreEquivalent =
  testCase "Parametric and fixed nested beams are equivalent" $
  do let (DatabaseEntity (DatabaseTable { dbTableSettings = deptVehiculesA })) = _departmentVehiculesA employeeDbSettings
         (DatabaseEntity (DatabaseTable { dbTableSettings = deptVehiculesB })) = _departmentVehiculesB employeeDbSettings
         deptVehiculesFieldNamesA = allBeamValues (\(Columnar' f) -> _fieldName f) deptVehiculesA
         deptVehiculesFieldNamesB = allBeamValues (\(Columnar' f) -> _fieldName f) deptVehiculesB

     deptVehiculesFieldNamesB @?= deptVehiculesFieldNamesA


-- `ADepartmentVehiculeT` and `BDepartmentVehiculeT` are equivalent, but one was using params while
-- the other had its sub-beams fixed.

type ADepartmentVehiculeT  = DepartamentRelatedT VehiculeInformationT VehiculeT
data DepartamentRelatedT metaInfo prop f = DepartamentProperty
      { _aDepartament  :: PrimaryKey DepartmentT f
      , _aRelatesTo    :: prop f                -- checking we can nest both, nullable and non-nullable beams.
      , _aMetaInfo     :: metaInfo (Nullable f)
      } deriving Generic

data BDepartmentVehiculeT f = BDepartmentVehicule
      { _bDepartament  :: PrimaryKey DepartmentT f
      , _bRelatesTo    :: VehiculeT f
      , _bMetaInfo     :: VehiculeInformationT (Nullable f)
      } deriving Generic

instance (Beamable metaInfo, Beamable  prop) => Beamable (DepartamentRelatedT metaInfo prop)


instance (Table metaInfo, Table  prop) => Table    (DepartamentRelatedT metaInfo prop) where
  data PrimaryKey (DepartamentRelatedT metaInfo prop) f = DepReKeyA (PrimaryKey DepartmentT f)
                                                                    (PrimaryKey prop f)
                                                                    deriving(Generic)
  primaryKey = DepReKeyA <$> _aDepartament <*> (primaryKey._aRelatesTo)

instance (Table metaInfo, Table prop) => Beamable (PrimaryKey (DepartamentRelatedT metaInfo prop))



data VehiculeT f = VehiculeT
      { _vehiculeId     :: C f Text
      , _vehiculeType   :: C f Text
      , _numberOfWheels :: C f Int32
      } deriving Generic

instance Beamable VehiculeT
instance Beamable (PrimaryKey VehiculeT)

instance Table VehiculeT where
     data PrimaryKey VehiculeT f = VehiculeId  (C f Text) deriving(Generic)
     primaryKey = VehiculeId <$> _vehiculeId

data VehiculeInformationT f = VehiculeInformationT
      { _price          :: C f Double
      } deriving Generic


instance Beamable VehiculeInformationT
instance Beamable (PrimaryKey VehiculeInformationT)

instance Table VehiculeInformationT where
     data PrimaryKey VehiculeInformationT f = VehiculeInformationKey  deriving(Generic)
     primaryKey _ = VehiculeInformationKey

instance Beamable BDepartmentVehiculeT
instance Beamable (PrimaryKey BDepartmentVehiculeT)
instance Table BDepartmentVehiculeT where
  data PrimaryKey BDepartmentVehiculeT f = DepReKeyB (PrimaryKey DepartmentT f)
                                                     (PrimaryKey VehiculeT   f)
                                                     deriving(Generic)
  primaryKey = DepReKeyB <$> _bDepartament <*> (primaryKey._bRelatesTo)

data NoPrimaryKeyT f
    = NoPrimaryKeyT
    { _npkField1 :: C f Text
    , _npkField2 :: C f Double
    } deriving Generic
instance Beamable NoPrimaryKeyT
instance Table NoPrimaryKeyT where
    data PrimaryKey NoPrimaryKeyT f = NoPrimaryKey
        deriving Generic
    primaryKey _ = NoPrimaryKey
instance Beamable (PrimaryKey NoPrimaryKeyT)

-- * Database schema is derived correctly

data EmployeeDb f
  = EmployeeDb
    { _employees            :: f (TableEntity EmployeeT)
    , _departments          :: f (TableEntity DepartmentT)
    , _roles                :: f (TableEntity RoleT)
    , _funny                :: f (TableEntity FunnyT)
    , _departmentVehiculesA :: f (TableEntity ADepartmentVehiculeT)
    , _departmentVehiculesB :: f (TableEntity BDepartmentVehiculeT)
    , _noPrimaryKey         :: f (TableEntity NoPrimaryKeyT)
    } deriving Generic
instance Database be EmployeeDb

employeeDbSettings :: DatabaseSettings be EmployeeDb
employeeDbSettings = defaultDbSettings

employeeDbSettingsRuleMods :: DatabaseSettings be EmployeeDb
employeeDbSettingsRuleMods = defaultDbSettings `withDbModification`
                             renamingFields (\field ->
                                                let defName = defaultFieldName field
                                                in case T.stripPrefix "funny" defName of
                                                  Nothing -> defName
                                                  Just _ -> "pfx_" <> defName)

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
