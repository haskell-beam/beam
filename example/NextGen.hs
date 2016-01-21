{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, TypeFamilies, DeriveGeneric, OverloadedStrings, UndecidableInstances #-}
module Main where

import Database.Beam
import Database.Beam.Backend.Sqlite3

import Control.Monad
import Control.Monad.Identity

import Data.Typeable
import Data.Text
import Data.Time.Clock

import GHC.Generics

import Lens.Micro

import System.Environment

-- * Beam schema
data Position = DepartmentLead
              | Analyst
                deriving (Show, Read, Eq, Ord, Enum)

data EmployeeT f = Employee
                 { _employeeId         :: Columnar f AutoId
                 , _employeeFirstName  :: Columnar f Text
                 , _employeeLastName   :: Columnar f Text
                 , _employeeGroup      :: ForeignKey GroupT f

                 , _employeePosition   :: Columnar f (BeamEnum Position) }
                 deriving (Generic)

data DepartmentT f = Department
                   { _deptId       :: Columnar f Text }
                   deriving (Generic)

data GroupT f = Group
              { _groupId       :: Columnar f Text
              , _groupDeptId   :: ForeignKey DepartmentT f
              , _groupLocation :: Columnar f Text }
                deriving (Generic)

Employee (LensFor employeeIdC)
         (LensFor employeeFirstNameC)
         (LensFor employeeLastNameC)
         (ForeignKey (LensFor employeeDeptIdC, LensFor employeeGroupIdC))
         (LensFor employeePositionC) = tableConfigLenses

Department (LensFor deptIdC) = tableConfigLenses

Group (LensFor groupIdC)
      (ForeignKey (PK (LensFor groupDeptIdC)))
      (LensFor groupLocationC) = tableConfigLenses

data EmployeeDatabase q = EmployeeDatabase
                        { _employees   :: q EmployeeT
                        , _departments :: q DepartmentT
                        , _groups      :: q GroupT }
                        deriving Generic

instance Table EmployeeT where
    type PrimaryKey EmployeeT f = PK f AutoId
    primaryKey x = PK (_employeeId x)

    tblFieldSettings = defTblFieldSettings
                       & employeeIdC . fieldName .~ "id"
                       & employeeFirstNameC . fieldName .~ "first_name"
                       & employeeLastNameC . fieldName .~ "last_name"
                       & employeeGroupIdC . fieldName .~ "group_id"
                       & employeeDeptIdC . fieldName .~ "dept_id"
                       & employeePositionC . fieldName .~ "position"
instance Table DepartmentT where
    type PrimaryKey DepartmentT f = PK f Text
    primaryKey x = PK (_deptId x)

    tblFieldSettings = defTblFieldSettings
                        & deptIdC . fieldName .~ "id"
                        & deptIdC . fieldSettings .~ TextFieldSettings (Varchar (Just 32))
instance Table GroupT where
    type PrimaryKey GroupT f = (Columnar f Text, Columnar f Text)
    primaryKey (Group groupId (ForeignKey (PK deptId)) _ ) = (groupId, deptId)

    tblFieldSettings = defTblFieldSettings
                       & groupIdC . fieldName .~ "id"
                       & groupDeptIdC . fieldName .~ "dept_id"
                       & groupLocationC . fieldName .~ "location"

type Employee = EmployeeT Identity
type Department = DepartmentT Identity
type Group = GroupT Identity

deriving instance Show Group
deriving instance Show Employee
deriving instance Show Department

instance Database EmployeeDatabase where
    allTables f (EmployeeDatabase emps depts groups) = [f emps, f depts, f groups]
employeeDb :: DatabaseSettings EmployeeDatabase
employeeDb = EmployeeDatabase (DatabaseTable (Proxy :: Proxy EmployeeT) "employees")
                              (DatabaseTable (Proxy :: Proxy DepartmentT) "departments")
                              (DatabaseTable (Proxy :: Proxy GroupT) "groups")

-- * main functions
main = do [sqliteDbPath] <- getArgs
          beam <- openDatabase employeeDb (Sqlite3Settings sqliteDbPath)
          pure ()

          beamTxn beam $ \(EmployeeDatabase employeesT departmentsT groupsT) ->
            do let departments = [ Department "accounting"
                                 , Department "operations"
                                 , Department "product"
                                 , Department "sales"
                                 , Department "IT" ]

                   employees = [ Employee UnassignedId "James" "Smith" (ForeignKey ("ItGroup1", "IT")) DepartmentLead
                               , Employee UnassignedId "Taylor" "Jones" (ForeignKey ("ItGroup1", "IT")) Analyst
                               , Employee UnassignedId "Samantha" "Nolen" (ForeignKey ("ItGroup2", "IT")) Analyst

                               , Employee UnassignedId "Maurice" "Davies" (ForeignKey ("OpsGroup1", "operations")) DepartmentLead
                               , Employee UnassignedId "Bob" "Lee" (ForeignKey ("OpsGroup2", "operations")) Analyst
                               , Employee UnassignedId "Constantine" "Nevos" (ForeignKey ("OpsGroup3", "operations")) Analyst ]

                   groups = [ Group "ItGroup1" (ForeignKey (PK "IT")) "New York"
                            , Group "ItGroup2" (ForeignKey (PK "IT")) "Los Angeles"
                            , Group "OpsGroup1" (ForeignKey (PK "operations")) "Boston"
                            , Group "OpsGroup2" (ForeignKey (PK "operations")) "Los Angeles"
                            , Group "OpsGroup3" (ForeignKey (PK "operations")) "New York" ]

               mapM_ (insertInto departmentsT) departments
               mapM_ (insertInto groupsT) groups

               employees'@[ jamesSmith, taylorJones, samanthaNolen
                          , mauriceDavies, bobLee, constantineNevos ] <-
                   mapM (insertInto employeesT) employees

               liftIO (putStrLn "Inserted employees")
               liftIO (mapM_ (putStrLn . show) employees')

               liftIO (putStrLn "---- Query 1: All departments")
               q1 <- queryList (all_ departmentsT)
               liftIO (mapM_ (putStrLn . show) q1)

               liftIO (putStrLn "\n---- Query 2: All employees in new york")
               q2 <- queryList $
                     do employee <- all_ employeesT
                        group <- related_ groupsT (_employeeGroup employee)

                        guard_ (_groupLocation group ==# "New York")

                        pure (employee, group)
               liftIO (mapM_ (putStrLn . show) q2)

               liftIO (putStrLn "\n---- Query 3: All employees (and their associated departments) based in Los Angeles")
               q3 <- queryList $
                     do employee <- all_ employeesT
                        group <- related_ groupsT (_employeeGroup employee)
                        guard_ (_groupLocation group ==# "Los Angeles")

                        dept <- related_ departmentsT (_groupDeptId group)
                        pure (employee, dept)
               liftIO (mapM_ (putStrLn . show) q3)

               {-
                 q4 <- queryList $
                       do (orderAmount, region) <- limit_ 1 $
                                                   orderBy (\(amount, location) -> Desc amount) $
                                                   aggregate (\(amount, location) -> (group_ location, sum_ amount)) $
                                                   do order <- all_ ordersT
                                                      employee <- related_ employeesT (_orderTakenBy order)
                                                      group <- related groupsT (_employeeGroup employee)

                                                      guard_ (_orderCreated order >=# val_ yearAgo)

                                                      pure (_orderAmount order, _groupLocation group)
                          employee <- all_ employeesT
                          group <- related groupsT (_employeeGroup employee)
                          guard_ (_groupLocation group ==# region)
                          pure employee
                -}
