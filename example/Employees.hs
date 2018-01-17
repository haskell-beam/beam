{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, TypeFamilies, DeriveGeneric, OverloadedStrings, UndecidableInstances #-}
module Main where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL92

import Control.Arrow
import Control.Monad
import Control.Monad.Identity

import Data.Typeable
import Data.Text
import Data.Int
import Data.Time.Clock

import GHC.Generics

import Lens.Micro

<<<<<<< HEAD
import System.Environment (getArgs, getProgName)

import EmployeesData

departments :: [Department]
departments =
    [ Department "accounting"
    , Department "operations"
    , Department "product"
    , Department "sales"
    , Department "IT" ]

employees :: [Employee]
employees =
    [ Employee UnassignedId "James" "Smith" (GroupId "ItGroup1" "IT") DepartmentLead
    , Employee UnassignedId "Taylor" "Jones" (GroupId "ItGroup1" "IT") Analyst
    , Employee UnassignedId "Samantha" "Nolen" (GroupId "ItGroup2" "IT") Analyst

    , Employee UnassignedId "Maurice" "Davies" (GroupId "OpsGroup1" "operations") DepartmentLead
    , Employee UnassignedId "Bob" "Lee" (GroupId "OpsGroup2" "operations") Analyst
    , Employee UnassignedId "Constantine" "Nevos" (GroupId "OpsGroup3" "operations") Analyst

    , Employee UnassignedId "Tom" "Jones" (GroupId "Alpha" "sales") DepartmentLead
    , Employee UnassignedId "Toby" "Roberts" (GroupId "Alpha" "sales") Analyst
    , Employee UnassignedId "Katy" "Barry" (GroupId "Beta" "sales") Analyst
    , Employee UnassignedId "Amy" "Zely" (GroupId "Beta" "sales") Analyst
    , Employee UnassignedId "Pierre" "Berger" (GroupId "Gamma" "sales") Analyst
    , Employee UnassignedId "Blaise" "Solle" (GroupId "Gamma" "sales") Analyst ]

groups :: [Group]
groups =
    [ Group "ItGroup1" (DepartmentId "IT") "New York"
    , Group "ItGroup2" (DepartmentId "IT") "Los Angeles"
    , Group "OpsGroup1" (DepartmentId "operations") "Boston"
    , Group "OpsGroup2" (DepartmentId "operations") "Los Angeles"
    , Group "OpsGroup3" (DepartmentId "operations") "New York"

    , Group "Alpha" (DepartmentId "sales") "Los Angeles"
    , Group "Beta" (DepartmentId "sales") "New York"
    , Group "Gamma" (DepartmentId "sales") "Boston" ]

-- * main functions
main :: IO ()
main = do args <- getArgs
          progName <- getProgName
          let sqliteDbPath = case args of
                               [x] -> x
                               _ -> error $ "Usage: " ++ progName ++ " [path to SQLite3 database]"
          beam <- openDatabaseDebug employeeDb AutoMigrate (Sqlite3Settings sqliteDbPath)

          _ <- beamTxn beam $ \(EmployeeDatabase employeesT departmentsT groupsT ordersT) ->
            do mapM_ (insertInto departmentsT) departments
               mapM_ (insertInto groupsT) groups

               employees'@[ _, _, _, _, _, _
                          , tomJones, tobyRoberts, katyBarry
                          , amyZely, pierreBerger, blaiseSolle ] <-
                   mapM (insertInto employeesT) employees
               liftIO (putStrLn "Inserted employees")
               liftIO (mapM_ print employees')

               let orders =
                       [ Order UnassignedId (pk tomJones) 100
                       , Order UnassignedId (pk tomJones) 500
                       , Order UnassignedId (pk tomJones) 2500

                       , Order UnassignedId (pk tobyRoberts) 400
                       , Order UnassignedId (pk tobyRoberts) 550

                       , Order UnassignedId (pk katyBarry) 50
                       , Order UnassignedId (pk katyBarry) 430
                       , Order UnassignedId (pk katyBarry) 80
                       , Order UnassignedId (pk katyBarry) 210

                       , Order UnassignedId (pk amyZely) 200
                       , Order UnassignedId (pk amyZely) 50

                       , Order UnassignedId (pk pierreBerger) 300
                       , Order UnassignedId (pk pierreBerger) 140
                       , Order UnassignedId (pk pierreBerger) 20

                       , Order UnassignedId (pk blaiseSolle) 350
                       , Order UnassignedId (pk blaiseSolle) 1000 ]
               mapM_ (insertInto ordersT) orders

               liftIO (putStrLn "---- Query 1: All departments")
               q1 <- queryList (all_ departmentsT)
               liftIO (mapM_ print q1)

               liftIO (putStrLn "\n---- Query 2: All employees in new york")
               q2 <- queryList $
                     do employee <- all_ employeesT
                        group <- related_ groupsT (_employeeGroup employee)

                        guard_ (_groupLocation group ==. "New York")

                        pure (employee, group)
               liftIO (mapM_ print q2)

               liftIO (putStrLn "\n---- Query 3: All employees (and their associated departments) based in Los Angeles")
               q3 <- queryList $
                     do employee <- all_ employeesT
                        group <- related_ groupsT (_employeeGroup employee)
                        guard_ (_groupLocation group ==. "Los Angeles")

                        dept <- related_ departmentsT (_groupDeptId group)
                        pure (employee, dept)
               liftIO (mapM_ print q3)

               liftIO (putStrLn "\n---- Query 4: Total orders by region")
               q4 <- queryList $
                     aggregate (sum_ *** group_) $
                               do order <- all_ ordersT
                                  employee <- related_ employeesT (_orderTakenBy order)
     <                             group <- related_ groupsT (_employeeGroup employee)

--                                  guard_ (_orderCreated order >=# val_ yearAgo)

                                  pure (_orderAmount order, _groupLocation group)
               liftIO (mapM_ print q4)

               liftIO (putStrLn "\n---- Query 5: All employees in the highest grossing region")
               q5 <- queryList $
                     do (_, highestGrossingRegion) <- subquery_ $
                                                      limit_ 1 $
                                                      orderBy (desc_ . fst) $
                                                      aggregate (sum_ *** group_) $
                                                      do order <- all_ ordersT
                                                         employee <- related_ employeesT (_orderTakenBy order)
                                                         group <- related_ groupsT (_employeeGroup employee)

                                                         pure (_orderAmount order, _groupLocation group)
                        employee <- all_ employeesT
                        group <- related_ groupsT (_employeeGroup employee)
                        guard_ (_groupLocation group ==. highestGrossingRegion)
                        pure employee
               liftIO (mapM_ print q5)
          return ()
=======
import System.Environment

-- * Beam schema
-- data Position = DepartmentLead
--               | Analyst
--                 deriving (Show, Read, Eq, Ord, Enum)
-- instance BeamSql92Backend be => HasDefaultFieldSchema be Position where
--     defFieldSchema = enumSchema
-- instance (BeamBackend be, FromBackendLiteral be Int32) => FromBackendLiteral be Position where
--     fromBackendLiteral = fromEnumValue
--     toBackendLiteral = makeEnumValue

data EmployeeT f = Employee
                 { _employeeId         :: Columnar f Int32
                 , _employeeFirstName  :: Columnar f Text
                 , _employeeLastName   :: Columnar f Text
                 , _employeeGroup      :: PrimaryKey GroupT f }
                 deriving Generic
instance Beamable EmployeeT

data DepartmentT f = Department
                   { _deptId       :: Columnar f Text }
                   deriving Generic
instance Beamable DepartmentT

data GroupT f = Group
              { _groupId       :: Columnar f Text
              , _groupDeptId   :: PrimaryKey DepartmentT f
              , _groupLocation :: Columnar f Text }
                deriving Generic
instance Beamable GroupT

data OrderT f = Order
              { _orderId  :: Columnar f Int32
              , _orderTakenBy :: PrimaryKey EmployeeT f
              , _orderAmount :: Columnar f Int32 }
              deriving Generic
instance Beamable OrderT

Employee (LensFor employeeIdC)
         (LensFor employeeFirstNameC)
         (LensFor employeeLastNameC)
         (GroupId (LensFor employeeDeptIdC) (LensFor employeeGroupIdC)) =
  tableConfigLenses
--         (LensFor employeePositionC) = tableConfigLenses

Department (LensFor deptIdC) = tableConfigLenses

Group (LensFor groupIdC)
      (DepartmentId (LensFor groupDeptIdC))
      (LensFor groupLocationC) = tableConfigLenses

Order (LensFor orderIdC)
      (EmployeeId (LensFor orderTakenByIdC))
      (LensFor orderAmountC) = tableConfigLenses

data EmployeeDatabase q = EmployeeDatabase
                        { _employees   :: q EmployeeT
                        , _departments :: q DepartmentT
                        , _groups      :: q GroupT
                        , _orders      :: q OrderT }
                        deriving Generic

instance Table EmployeeT where
    data PrimaryKey EmployeeT f = EmployeeId (Columnar f Int32)
                                deriving Generic
    primaryKey = EmployeeId . _employeeId
instance Beamable (PrimaryKey EmployeeT)
instance Table DepartmentT where
    data PrimaryKey DepartmentT f = DepartmentId (Columnar f Text)
                                  deriving Generic
    primaryKey = DepartmentId . _deptId
instance Beamable (PrimaryKey DepartmentT)
instance Table GroupT where
    data PrimaryKey GroupT f = GroupId (Columnar f Text) (Columnar f Text)
                             deriving Generic
    primaryKey (Group groupId (DepartmentId deptId) _ ) = GroupId groupId deptId
instance Beamable (PrimaryKey GroupT)
instance Table OrderT where
    data PrimaryKey OrderT f = OrderId (Columnar f Int32)
                               deriving Generic
    primaryKey = OrderId . _orderId
instance Beamable (PrimaryKey OrderT)

type Employee = EmployeeT Identity
type EmployeeId = PrimaryKey EmployeeT Identity
type Department = DepartmentT Identity
type DepartmentId = PrimaryKey DepartmentT Identity
type Group = GroupT Identity
type GroupId = PrimaryKey GroupT Identity
type Order = OrderT Identity
type OrderId = PrimaryKey OrderT Identity

deriving instance Show Group
deriving instance Show GroupId
deriving instance Show Employee
deriving instance Show EmployeeId
deriving instance Show Department
deriving instance Show DepartmentId
deriving instance Show Order
deriving instance Show OrderId

instance Database EmployeeDatabase

EmployeeDatabase { _departments = TableLens departmentsC } = dbLenses
employeeDb :: DatabaseSettings Postgres EmployeeDatabase
employeeDb = autoDbSettings & departmentsC . tableSettings . deptIdC . fieldSchema .~ varchar (Just 32)
-- * main functions
main = putStrLn "Hello" -- do [sqliteDbPath] <- getArgs
--           beam <- openDatabaseDebug employeeDb AutoMigrate (Sqlite3Settings sqliteDbPath)

--           beamTxn beam $ \(EmployeeDatabase employeesT departmentsT groupsT ordersT) ->
--             do let departments = [ Department "accounting"
--                                  , Department "operations"
--                                  , Department "product"
--                                  , Department "sales"
--                                  , Department "IT" ]

--                    employees = [ Employee UnassignedId "James" "Smith" (GroupId "ItGroup1" "IT") DepartmentLead
--                                , Employee UnassignedId "Taylor" "Jones" (GroupId "ItGroup1" "IT") Analyst
--                                , Employee UnassignedId "Samantha" "Nolen" (GroupId "ItGroup2" "IT") Analyst

--                                , Employee UnassignedId "Maurice" "Davies" (GroupId "OpsGroup1" "operations") DepartmentLead
--                                , Employee UnassignedId "Bob" "Lee" (GroupId "OpsGroup2" "operations") Analyst
--                                , Employee UnassignedId "Constantine" "Nevos" (GroupId "OpsGroup3" "operations") Analyst

--                                , Employee UnassignedId "Tom" "Jones" (GroupId "Alpha" "sales") DepartmentLead
--                                , Employee UnassignedId "Toby" "Roberts" (GroupId "Alpha" "sales") Analyst
--                                , Employee UnassignedId "Katy" "Barry" (GroupId "Beta" "sales") Analyst
--                                , Employee UnassignedId "Amy" "Zely" (GroupId "Beta" "sales") Analyst
--                                , Employee UnassignedId "Pierre" "Berger" (GroupId "Gamma" "sales") Analyst
--                                , Employee UnassignedId "Blaise" "Solle" (GroupId "Gamma" "sales") Analyst  ]

--                    groups = [ Group "ItGroup1" (DepartmentId "IT") "New York"
--                             , Group "ItGroup2" (DepartmentId "IT") "Los Angeles"
--                             , Group "OpsGroup1" (DepartmentId "operations") "Boston"
--                             , Group "OpsGroup2" (DepartmentId "operations") "Los Angeles"
--                             , Group "OpsGroup3" (DepartmentId "operations") "New York"

--                             , Group "Alpha" (DepartmentId "sales") "Los Angeles"
--                             , Group "Beta" (DepartmentId "sales") "New York"
--                             , Group "Gamma" (DepartmentId "sales") "Boston"]

--                mapM_ (insertInto departmentsT) departments
--                mapM_ (insertInto groupsT) groups

--                employees'@[ _, _, _, _, _, _
--                           , tomJones, tobyRoberts, katyBarry
--                           , amyZely, pierreBerger, blaiseSolle ] <-
--                    mapM (insertInto employeesT) employees
--                liftIO (putStrLn "Inserted employees")
--                liftIO (mapM_ (putStrLn . show) employees')

--                let orders = [ Order UnassignedId (pk tomJones) 100
--                             , Order UnassignedId (pk tomJones) 500
--                             , Order UnassignedId (pk tomJones) 2500

--                             , Order UnassignedId (pk tobyRoberts) 400
--                             , Order UnassignedId (pk tobyRoberts) 550

--                             , Order UnassignedId (pk katyBarry) 50
--                             , Order UnassignedId (pk katyBarry) 430
--                             , Order UnassignedId (pk katyBarry) 80
--                             , Order UnassignedId (pk katyBarry) 210

--                             , Order UnassignedId (pk amyZely) 200
--                             , Order UnassignedId (pk amyZely) 50

--                             , Order UnassignedId (pk pierreBerger) 300
--                             , Order UnassignedId (pk pierreBerger) 140
--                             , Order UnassignedId (pk pierreBerger) 20

--                             , Order UnassignedId (pk blaiseSolle) 350
--                             , Order UnassignedId (pk blaiseSolle) 1000 ]

--                mapM_ (insertInto ordersT) orders

--                liftIO (putStrLn "---- Query 1: All departments")
--                q1 <- queryList (all_ departmentsT)
--                liftIO (mapM_ (putStrLn . show) q1)

--                liftIO (putStrLn "\n---- Query 2: All employees in new york")
--                q2 <- queryList $
--                      do employee <- all_ employeesT
--                         group <- related_ groupsT (_employeeGroup employee)

--                         guard_ (_groupLocation group ==. "New York")

--                         pure (employee, group)
--                liftIO (mapM_ (putStrLn . show) q2)

--                liftIO (putStrLn "\n---- Query 3: All employees (and their associated departments) based in Los Angeles")
--                q3 <- queryList $
--                      do employee <- all_ employeesT
--                         group <- related_ groupsT (_employeeGroup employee)
--                         guard_ (_groupLocation group ==. "Los Angeles")

--                         dept <- related_ departmentsT (_groupDeptId group)
--                         pure (employee, dept)
--                liftIO (mapM_ (putStrLn . show) q3)

--                liftIO (putStrLn "\n---- Query 4: Total orders by region")
--                q4 <- queryList $
--                      aggregate (\(amount, location) -> (group_ location, sum_ amount)) $
--                                do order <- all_ ordersT
--                                   employee <- related_ employeesT (_orderTakenBy order)
--                                   group <- related_ groupsT (_employeeGroup employee)

-- --                                  guard_ (_orderCreated order >=# val_ yearAgo)

--                                   pure (_orderAmount order, _groupLocation group)
--                liftIO (mapM_ (putStrLn . show) q4)

--                liftIO (putStrLn "\n---- Query 5: All employees in the highest grossing region")
--                q5 <- queryList $
--                      do (_, highestGrossingRegion) <- subquery_ $
--                                                       limit_ 1 $
--                                                       orderBy (\(amount, location) -> desc_ amount) $
--                                                       aggregate (\(amount, location) -> (sum_ amount, group_ location)) $
--                                                       do order <- all_ ordersT
--                                                          employee <- related_ employeesT (_orderTakenBy order)
--                                                          group <- related_ groupsT (_employeeGroup employee)

--                                                          pure (_orderAmount order, _groupLocation group)
--                         employee <- all_ employeesT
--                         group <- related_ groupsT (_employeeGroup employee)
--                         guard_ (_groupLocation group ==. highestGrossingRegion)
--                         pure employee
--                liftIO (mapM_ (putStrLn . show) q5)
>>>>>>> Separated beam core. Started work on postgres integration
