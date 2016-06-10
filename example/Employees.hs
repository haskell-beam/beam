module Main where

import Database.Beam
import Database.Beam.Backend.Sqlite3

import Control.Arrow
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
                                  group <- related_ groupsT (_employeeGroup employee)

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
