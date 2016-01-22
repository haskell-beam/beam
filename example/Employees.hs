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

data OrderT f = Order
              { _orderId  :: Columnar f AutoId
              , _orderTakenBy :: ForeignKey EmployeeT f
              , _orderAmount :: Columnar f Int }
              deriving Generic

Employee (LensFor employeeIdC)
         (LensFor employeeFirstNameC)
         (LensFor employeeLastNameC)
         (ForeignKey (LensFor employeeDeptIdC, LensFor employeeGroupIdC))
         (LensFor employeePositionC) = tableConfigLenses

Department (LensFor deptIdC) = tableConfigLenses

Group (LensFor groupIdC)
      (ForeignKey (PK (LensFor groupDeptIdC)))
      (LensFor groupLocationC) = tableConfigLenses

Order (LensFor orderIdC)
      (ForeignKey (PK (LensFor orderTakenByIdC)))
      (LensFor orderAmountC) = tableConfigLenses

data EmployeeDatabase q = EmployeeDatabase
                        { _employees   :: q EmployeeT
                        , _departments :: q DepartmentT
                        , _groups      :: q GroupT
                        , _orders      :: q OrderT }
                        deriving Generic

instance Table EmployeeT where
    type PrimaryKey EmployeeT f = PK f AutoId
    primaryKey x = PK (_employeeId x)
instance Table DepartmentT where
    type PrimaryKey DepartmentT f = PK f Text
    primaryKey x = PK (_deptId x)

    tblFieldSettings = defTblFieldSettings
                        & deptIdC . fieldSettings .~ TextFieldSettings (Varchar (Just 32))
instance Table GroupT where
    type PrimaryKey GroupT f = (Columnar f Text, Columnar f Text)
    primaryKey (Group groupId (ForeignKey (PK deptId)) _ ) = (groupId, deptId)

    tblFieldSettings = defTblFieldSettings

instance Table OrderT where
    type PrimaryKey OrderT f = PK f AutoId
    primaryKey x = PK (_orderId x)

    tblFieldSettings = defTblFieldSettings

type Employee = EmployeeT Identity
type Department = DepartmentT Identity
type Group = GroupT Identity
type Order = OrderT Identity

deriving instance Show Group
deriving instance Show Employee
deriving instance Show Department
deriving instance Show Order

instance Database EmployeeDatabase
employeeDb :: DatabaseSettings EmployeeDatabase
employeeDb = autoDbSettings

-- * main functions
main = do [sqliteDbPath] <- getArgs
          beam <- openDatabase employeeDb (Sqlite3Settings sqliteDbPath)

          beamTxn beam $ \(EmployeeDatabase employeesT departmentsT groupsT ordersT) ->
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
                               , Employee UnassignedId "Constantine" "Nevos" (ForeignKey ("OpsGroup3", "operations")) Analyst

                               , Employee UnassignedId "Tom" "Jones" (ForeignKey ("Alpha", "sales")) DepartmentLead
                               , Employee UnassignedId "Toby" "Roberts" (ForeignKey ("Alpha", "sales")) Analyst
                               , Employee UnassignedId "Katy" "Barry" (ForeignKey ("Beta", "sales")) Analyst
                               , Employee UnassignedId "Amy" "Zely" (ForeignKey ("Beta", "sales")) Analyst
                               , Employee UnassignedId "Pierre" "Berger" (ForeignKey ("Gamma", "sales")) Analyst
                               , Employee UnassignedId "Blaise" "Solle" (ForeignKey ("Gamma", "sales")) Analyst  ]

                   groups = [ Group "ItGroup1" (ForeignKey (PK "IT")) "New York"
                            , Group "ItGroup2" (ForeignKey (PK "IT")) "Los Angeles"
                            , Group "OpsGroup1" (ForeignKey (PK "operations")) "Boston"
                            , Group "OpsGroup2" (ForeignKey (PK "operations")) "Los Angeles"
                            , Group "OpsGroup3" (ForeignKey (PK "operations")) "New York"

                            , Group "Alpha" (ForeignKey (PK "sales")) "Los Angeles"
                            , Group "Beta" (ForeignKey (PK "sales")) "New York"
                            , Group "Gamma" (ForeignKey (PK "sales")) "Boston"]

               mapM_ (insertInto departmentsT) departments
               mapM_ (insertInto groupsT) groups

               employees'@[ _, _, _, _, _, _
                          , tomJones, tobyRoberts, katyBarry
                          , amyZely, pierreBerger, blaiseSolle ] <-
                   mapM (insertInto employeesT) employees
               liftIO (putStrLn "Inserted employees")
               liftIO (mapM_ (putStrLn . show) employees')

               let orders = [ Order UnassignedId (ref tomJones) 100
                            , Order UnassignedId (ref tomJones) 500
                            , Order UnassignedId (ref tomJones) 2500

                            , Order UnassignedId (ref tobyRoberts) 400
                            , Order UnassignedId (ref tobyRoberts) 550

                            , Order UnassignedId (ref katyBarry) 50
                            , Order UnassignedId (ref katyBarry) 430
                            , Order UnassignedId (ref katyBarry) 80
                            , Order UnassignedId (ref katyBarry) 210

                            , Order UnassignedId (ref amyZely) 200
                            , Order UnassignedId (ref amyZely) 50

                            , Order UnassignedId (ref pierreBerger) 300
                            , Order UnassignedId (ref pierreBerger) 140
                            , Order UnassignedId (ref pierreBerger) 20

                            , Order UnassignedId (ref blaiseSolle) 350
                            , Order UnassignedId (ref blaiseSolle) 1000 ]

               mapM_ (insertInto ordersT) orders

               liftIO (putStrLn "---- Query 1: All departments")
               q1 <- queryList (all_ departmentsT)
               liftIO (mapM_ (putStrLn . show) q1)

               liftIO (putStrLn "\n---- Query 2: All employees in new york")
               q2 <- queryList $
                     do employee <- all_ employeesT
                        group <- related_ groupsT (_employeeGroup employee)

                        guard_ (_groupLocation group ==. "New York")

                        pure (employee, group)
               liftIO (mapM_ (putStrLn . show) q2)

               liftIO (putStrLn "\n---- Query 3: All employees (and their associated departments) based in Los Angeles")
               q3 <- queryList $
                     do employee <- all_ employeesT
                        group <- related_ groupsT (_employeeGroup employee)
                        guard_ (_groupLocation group ==. "Los Angeles")

                        dept <- related_ departmentsT (_groupDeptId group)
                        pure (employee, dept)
               liftIO (mapM_ (putStrLn . show) q3)

               liftIO (putStrLn "\n---- Query 4: Total orders by region")
               q4 <- queryList $
                     aggregate (\(amount, location) -> (group_ location, sum_ amount)) $
                               do order <- all_ ordersT
                                  employee <- related_ employeesT (_orderTakenBy order)
                                  group <- related_ groupsT (_employeeGroup employee)

--                                  guard_ (_orderCreated order >=# val_ yearAgo)

                                  pure (_orderAmount order, _groupLocation group)
               liftIO (mapM_ (putStrLn . show) q4)

               liftIO (putStrLn "\n---- Query 5: All employees in the highest grossing region")
               q5 <- queryList $
                     do (_, highestGrossingRegion) <- subquery_ $
                                                      limit_ 1 $
                                                      orderBy (\(amount, location) -> desc_ amount) $
                                                      aggregate (\(amount, location) -> (sum_ amount, group_ location)) $
                                                      do order <- all_ ordersT
                                                         employee <- related_ employeesT (_orderTakenBy order)
                                                         group <- related_ groupsT (_employeeGroup employee)

                                                         pure (_orderAmount order, _groupLocation group)
                        employee <- all_ employeesT
                        group <- related_ groupsT (_employeeGroup employee)
                        guard_ (_groupLocation group ==. highestGrossingRegion)
                        pure employee
               liftIO (mapM_ (putStrLn . show) q5)
