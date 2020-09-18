{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Beam.Test.SQL
  ( tests ) where

import Database.Beam.Test.Schema hiding (tests)

import Database.Beam
import Database.Beam.Backend.SQL (MockSqlBackend)
import Database.Beam.Backend.SQL.AST

import Data.Int
import Data.Time.Clock
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "SQL generation tests"
                  [ simpleSelect
                  , simpleWhere
                  , simpleJoin
                  , selfJoin
                  , leftJoin
                  , leftJoinSingle
                  , aggregates
                  , orderBy

                  , joinHaving

                  , maybeFieldTypes

                  , tableEquality
                  , related
                  , selectCombinators
                  , limitOffset

                  , existsTest

                  , updateCurrent
                  , updateNullable

                  , noEmptyIns

                  -- Regressions for github issues
                  , testGroup "Regression tests"
                    [ gh70OrderByInFirstJoinCausesIncorrectProjection
                    ]
                  ]

selectMock :: Projectible (MockSqlBackend Command) res
           => Q (MockSqlBackend Command) db QBaseScope res -> SqlSelect (MockSqlBackend Command) (QExprToIdentity res)
selectMock = select

updateMock :: Beamable table
           => DatabaseEntity (MockSqlBackend Command) db (TableEntity table)
           -> (forall s. table (QField s) -> QAssignment (MockSqlBackend Command) s)
           -> (forall s. table (QExpr (MockSqlBackend Command) s) -> QExpr (MockSqlBackend Command) s Bool)
           -> SqlUpdate (MockSqlBackend Command) table
updateMock = update

-- | Ensure simple select selects the right fields

simpleSelect :: TestTree
simpleSelect =
  testCase "All fields are present in a simple all_ query" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , .. } <- pure (selectMock (all_ (_employees employeeDbSettings)))

     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectWhere @?= Nothing
     selectLimit @?= Nothing
     selectOffset @?= Nothing
     selectHaving @?= Nothing
     selectQuantifier @?= Nothing

     Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (tblName, Nothing))) <- pure selectFrom

     selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField tblName "first_name"), Just "res0")
                                    , (ExpressionFieldName (QualifiedField tblName "last_name"), Just "res1")
                                    , (ExpressionFieldName (QualifiedField tblName "phone_number"), Just "res2")
                                    , (ExpressionFieldName (QualifiedField tblName "age"), Just "res3")
                                    , (ExpressionFieldName (QualifiedField tblName "salary"), Just "res4")
                                    , (ExpressionFieldName (QualifiedField tblName "hire_date"), Just "res5")
                                    , (ExpressionFieldName (QualifiedField tblName "leave_date"), Just "res6")
                                    , (ExpressionFieldName (QualifiedField tblName "created"), Just "res7") ]

-- | Simple select with WHERE clause

simpleWhere :: TestTree
simpleWhere =
  testCase "guard_ clauses are successfully translated into WHERE statements" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , .. } <- pure $ selectMock $
                                do e <- all_ (_employees employeeDbSettings)
                                   guard_ (_employeeSalary e >. 120202 &&.
                                           _employeeAge e <. 30 &&.
                                           _employeeFirstName e ==. _employeeLastName e)
                                   pure e
     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectLimit @?= Nothing
     selectOffset @?= Nothing
     selectHaving @?= Nothing
     selectQuantifier @?= Nothing

     Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (employees, Nothing))) <- pure selectFrom

     let salaryCond = ExpressionCompOp ">" Nothing (ExpressionFieldName (QualifiedField employees "salary")) (ExpressionValue (Value (120202 :: Double)))
         ageCond = ExpressionCompOp "<" Nothing (ExpressionFieldName (QualifiedField employees "age")) (ExpressionValue (Value (30 :: Int32)))
         nameCond = ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField employees "first_name")) (ExpressionFieldName (QualifiedField employees "last_name"))

     selectWhere @?= Just (ExpressionBinOp "AND" salaryCond (ExpressionBinOp "AND" ageCond nameCond))

-- | Ensure that multiple tables are correctly joined

simpleJoin :: TestTree
simpleJoin =
  testCase "Introducing multiple tables results in an inner join" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , .. } <- pure $ selectMock $
                                do e <- all_ (_employees employeeDbSettings)
                                   r <- all_ (_roles employeeDbSettings)
                                   pure (_employeePhoneNumber e, _roleName r)

     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectWhere @?= Nothing
     selectLimit @?= Nothing
     selectOffset @?= Nothing
     selectHaving @?= Nothing
     selectQuantifier @?= Nothing

     Just (InnerJoin (FromTable (TableNamed (TableName Nothing "employees")) (Just (employees, Nothing)))
                     (FromTable (TableNamed (TableName Nothing "roles")) (Just (roles, Nothing)))
                     Nothing) <- pure selectFrom

     selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField employees "phone_number"), Just "res0" )
                                    , ( ExpressionFieldName (QualifiedField roles "name"), Just "res1") ]

-- | Ensure that multiple joins on the same table are correctly referenced

selfJoin :: TestTree
selfJoin =
  testCase "Table names are unique and properly used in self joins" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , .. } <- pure $ selectMock $
                                do e1 <- all_ (_employees employeeDbSettings)
                                   e2 <- relatedBy_ (_employees employeeDbSettings)
                                                    (\e2 -> _employeeFirstName e1 ==. _employeeLastName e2)
                                   e3 <- relatedBy_ (_employees employeeDbSettings)
                                                    (\e3 -> _employeePhoneNumber e1 ==. _employeeLastName e3 &&. _employeePhoneNumber e3 ==. _employeeFirstName e2)
                                   pure (_employeeFirstName e1, _employeeLastName e2, _employeePhoneNumber e3)

     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectWhere @?= Nothing
     selectLimit @?= Nothing
     selectOffset @?= Nothing
     selectHaving @?= Nothing
     selectQuantifier @?= Nothing

     Just (InnerJoin (InnerJoin (FromTable (TableNamed (TableName Nothing "employees")) (Just (e1, Nothing)))
                                (FromTable (TableNamed (TableName Nothing "employees")) (Just (e2, Nothing)))
                                (Just joinCondition12))
                     (FromTable (TableNamed (TableName Nothing "employees")) (Just (e3, Nothing)))
                     (Just joinCondition123)) <- pure selectFrom

     assertBool "Table names are not unique" (e1 /= e2 && e1 /= e3 && e2 /= e3)
     joinCondition12 @?= ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField e1 "first_name"))
                                                       (ExpressionFieldName (QualifiedField e2 "last_name"))
     joinCondition123 @?= ExpressionBinOp "AND" (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField e1 "phone_number"))
                                                  (ExpressionFieldName (QualifiedField e3 "last_name")))
                            (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField e3 "phone_number")) (ExpressionFieldName (QualifiedField e2 "first_name")))

-- | Ensure that left joins are properly generated

leftJoin :: TestTree
leftJoin =
  testCase "leftJoin_ generates the right join" $
  do SqlSelect Select { selectTable = SelectTable { selectWhere = Nothing, selectFrom } } <-
       pure $ selectMock $
       do r <- all_ (_roles employeeDbSettings)
          e <- leftJoin_ (all_ (_employees employeeDbSettings)) (\e -> primaryKey e ==. _roleForEmployee r)
          pure (e, r)

     Just (LeftJoin (FromTable (TableNamed (TableName Nothing "roles")) (Just (roles, Nothing)))
                    (FromTable (TableNamed (TableName Nothing "employees")) (Just (employees, Nothing)))
                    (Just cond)) <- pure selectFrom

     let andE = ExpressionBinOp "AND"
         eqE = ExpressionCompOp "==" Nothing

         firstNameCond = eqE (ExpressionFieldName (QualifiedField employees "first_name"))
                             (ExpressionFieldName (QualifiedField roles "for_employee__first_name"))
         lastNameCond = eqE (ExpressionFieldName (QualifiedField employees "last_name"))
                            (ExpressionFieldName (QualifiedField roles "for_employee__last_name"))
         createdCond = eqE (ExpressionFieldName (QualifiedField employees "created"))
                           (ExpressionFieldName (QualifiedField roles "for_employee__created"))

     cond @?= andE (andE firstNameCond lastNameCond) createdCond

-- | Ensure that left joins which return a single column are properly typed. The
--   point of this test is to test for compile-time errors. The same query
--   should be generated as above.

leftJoinSingle :: TestTree
leftJoinSingle =
  testCase "leftJoin_ generates the right join (single return value)" $
  do SqlSelect Select { selectTable = SelectTable { selectWhere = Nothing, selectFrom } } <-
       pure $ selectMock $
       do r <- all_ (_roles employeeDbSettings)
          e <- leftJoin_ (do e <- all_ (_employees employeeDbSettings)
                             pure (primaryKey e, _employeeAge e))
                         (\(key, _) -> key ==. _roleForEmployee r)
          pure (e, r)

     Just (LeftJoin (FromTable (TableNamed (TableName Nothing "roles")) (Just (roles, Nothing)))
                    (FromTable (TableNamed (TableName Nothing "employees")) (Just (employees, Nothing)))
                    (Just cond)) <- pure selectFrom

     let andE = ExpressionBinOp "AND"
         eqE = ExpressionCompOp "==" Nothing

         firstNameCond = eqE (ExpressionFieldName (QualifiedField employees "first_name"))
                             (ExpressionFieldName (QualifiedField roles "for_employee__first_name"))
         lastNameCond = eqE (ExpressionFieldName (QualifiedField employees "last_name"))
                            (ExpressionFieldName (QualifiedField roles "for_employee__last_name"))
         createdCond = eqE (ExpressionFieldName (QualifiedField employees "created"))
                           (ExpressionFieldName (QualifiedField roles "for_employee__created"))

     cond @?= andE (andE firstNameCond lastNameCond) createdCond

-- | Ensure that aggregations cause the correct GROUP BY clause to be generated

aggregates :: TestTree
aggregates =
  testGroup "Aggregate support"
    [ basicAggregate
    , basicHaving
    , aggregateInJoin
    , aggregateInJoinReverse
    , aggregateOverTopLevel
    , filterAfterTopLevelAggregate
    , joinTopLevelAggregate
    , joinTopLevelAggregate2 ]
  where
    basicAggregate =
      testCase "Basic aggregate support" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
           do e <- all_ (_employees employeeDbSettings)
              pure e

         Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0, Nothing))) <- pure selectFrom
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0 "age"), Just "res0" )
                                        , ( ExpressionAgg "MAX" Nothing [ ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name")) ], Just "res1") ]
         selectWhere @?= Nothing
         selectGrouping @?= Just (Grouping [ ExpressionFieldName (QualifiedField t0 "age") ])
         selectHaving @?= Nothing

    basicHaving =
      testCase "Basic HAVING support" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           do (age, maxNameLength) <- aggregate_ (\e -> ( group_ (_employeeAge e)
                                                        , fromMaybe_ 0 (max_ (as_ @Int32 (charLength_ (_employeeFirstName e))))) ) $
                                      all_ (_employees employeeDbSettings)
              guard_ (maxNameLength >. 42)
              pure (age, maxNameLength)

         Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0, Nothing))) <- pure selectFrom
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0 "age"), Just "res0" )
                                        , ( ExpressionCoalesce [ ExpressionAgg "MAX" Nothing [ ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name")) ]
                                                               , ExpressionValue (Value (0 :: Int32)) ], Just "res1" ) ]
         selectWhere @?= Nothing
         selectGrouping @?= Just (Grouping [ ExpressionFieldName (QualifiedField t0 "age") ])
         selectHaving @?= Just (ExpressionCompOp ">" Nothing
                                (ExpressionCoalesce [ ExpressionAgg "MAX" Nothing [ ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name")) ]
                                                    , ExpressionValue (Value (0 :: Int32)) ])
                                (ExpressionValue (Value (42 :: Int32))))

    aggregateInJoin =
      testCase "Aggregate in JOIN" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           do (age, maxFirstNameLength) <- aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
                                           all_ (_employees employeeDbSettings)
              role <- all_ (_roles employeeDbSettings)
              pure (age, maxFirstNameLength, _roleName role)

         Just (InnerJoin (FromTable (TableFromSubSelect subselect) (Just (t0, Nothing)))
                         (FromTable (TableNamed (TableName Nothing "roles")) (Just (t1, Nothing)))
                         Nothing) <- pure selectFrom
         selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t0 "res0"), Just "res0")
                                        , (ExpressionFieldName (QualifiedField t0 "res1"), Just "res1")
                                        , (ExpressionFieldName (QualifiedField t1 "name"), Just "res2") ]
         selectWhere @?= Nothing
         selectGrouping @?= Nothing
         selectHaving @?= Nothing

         Select { selectTable = SelectTable { .. }
                , selectLimit = Nothing, selectOffset = Nothing
                , selectOrdering = [] } <- pure subselect
         Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0, Nothing))) <- pure selectFrom
         selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t0 "age"), Just "res0")
                                        , (ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name"))], Just "res1") ]
         selectWhere @?= Nothing
         selectGrouping @?= Just (Grouping [ExpressionFieldName (QualifiedField t0 "age")])
         selectHaving @?= Nothing

    aggregateInJoinReverse =
      testCase "Aggregate in JOIN (reverse order)" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           do role <- all_ (_roles employeeDbSettings)
              (age, maxFirstNameLength) <- aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
                                           all_ (_employees employeeDbSettings)
              pure (age, maxFirstNameLength, _roleName role)

         Just (InnerJoin (FromTable (TableNamed (TableName Nothing "roles")) (Just (t0, Nothing)))
                         (FromTable (TableFromSubSelect subselect) (Just (t1, Nothing)))
                         Nothing) <- pure selectFrom
         selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t1 "res0"), Just "res0")
                                        , (ExpressionFieldName (QualifiedField t1 "res1"), Just "res1")
                                        , (ExpressionFieldName (QualifiedField t0 "name"), Just "res2") ]
         selectWhere @?= Nothing
         selectGrouping @?= Nothing
         selectHaving @?= Nothing

         Select { selectTable = SelectTable { .. }
                , selectLimit = Nothing, selectOffset = Nothing
                , selectOrdering = [] } <- pure subselect
         Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0, Nothing))) <- pure selectFrom
         selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t0 "age"), Just "res0")
                                        , (ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name"))], Just "res1") ]
         selectWhere @?= Nothing
         selectGrouping @?= Just (Grouping [ExpressionFieldName (QualifiedField t0 "age")])
         selectHaving @?= Nothing

    aggregateOverTopLevel =
      testCase "Aggregate over top-level" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
           limit_ 10 (all_ (_employees employeeDbSettings))

         Just (FromTable (TableFromSubSelect subselect) (Just (t0, Nothing))) <- pure selectFrom

         selectProjection @?= ProjExprs [(ExpressionFieldName (QualifiedField t0 "res3"),Just "res0"),(ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "res0"))], Just "res1")]
         selectWhere @?= Nothing
         selectGrouping @?= Just (Grouping [ExpressionFieldName (QualifiedField t0 "res3")])
         selectHaving @?= Nothing

         selectLimit subselect @?= Just 10
         selectOffset subselect @?= Nothing
         selectOrdering subselect @?= []

         SelectTable {selectFrom = Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0', Nothing))), .. } <-
             pure $ selectTable subselect

         selectWhere @?= Nothing
         selectGrouping @?= Nothing
         selectHaving @?= Nothing
         selectProjection @?=
            ProjExprs [ (ExpressionFieldName (QualifiedField t0' "first_name"), Just "res0")
                      , (ExpressionFieldName (QualifiedField t0' "last_name"),  Just "res1")
                      , (ExpressionFieldName (QualifiedField t0' "phone_number"), Just "res2")
                      , (ExpressionFieldName (QualifiedField t0' "age"), Just "res3")
                      , (ExpressionFieldName (QualifiedField t0' "salary"), Just "res4")
                      , (ExpressionFieldName (QualifiedField t0' "hire_date"), Just "res5")
                      , (ExpressionFieldName (QualifiedField t0' "leave_date"), Just "res6")
                      , (ExpressionFieldName (QualifiedField t0' "created"), Just "res7") ]

--         assertFailure ("Select " ++ show s)

    filterAfterTopLevelAggregate =
      testCase "Filter after top-level aggregate" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           filter_ (\(_, l) -> l <. 10 ||. l >. 20) $
           aggregate_ (\e -> ( group_ (_employeeAge e)
                             , fromMaybe_ 0 (max_ (as_ @Int32 (charLength_ (_employeeFirstName e))))) ) $
           limit_ 10 (all_ (_employees employeeDbSettings))
         Just (FromTable (TableFromSubSelect subselect) (Just (t0, Nothing))) <- pure selectFrom

         selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t0 "res3"),Just "res0")
                                        , (ExpressionCoalesce [ ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "res0"))]
                                                              , ExpressionValue (Value (0 :: Int32)) ], Just "res1")
                                        ]
         selectWhere @?= Nothing
         selectGrouping @?= Just (Grouping [ExpressionFieldName (QualifiedField t0 "res3")])
         selectHaving @?= Just (ExpressionBinOp "OR" (ExpressionCompOp "<" Nothing
                                                      (ExpressionCoalesce [ ExpressionAgg "MAX" Nothing [ ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "res0")) ]
                                                                          , ExpressionValue (Value (0 :: Int32)) ])
                                                      (ExpressionValue (Value (10 :: Int32))))
                                                     (ExpressionCompOp ">" Nothing
                                                      (ExpressionCoalesce [ ExpressionAgg "MAX" Nothing [ ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "res0")) ]
                                                                          , ExpressionValue (Value (0 :: Int32)) ])
                                                      (ExpressionValue (Value (20 :: Int32)))))

         selectLimit subselect @?= Just 10
         selectOffset subselect @?= Nothing
         selectOrdering subselect @?= []

         SelectTable {selectFrom = Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0', Nothing))), .. } <-
             pure $ selectTable subselect

         selectWhere @?= Nothing
         selectGrouping @?= Nothing
         selectHaving @?= Nothing
         selectProjection @?=
            ProjExprs [ (ExpressionFieldName (QualifiedField t0' "first_name"), Just "res0")
                      , (ExpressionFieldName (QualifiedField t0' "last_name"),  Just "res1")
                      , (ExpressionFieldName (QualifiedField t0' "phone_number"), Just "res2")
                      , (ExpressionFieldName (QualifiedField t0' "age"), Just "res3")
                      , (ExpressionFieldName (QualifiedField t0' "salary"), Just "res4")
                      , (ExpressionFieldName (QualifiedField t0' "hire_date"), Just "res5")
                      , (ExpressionFieldName (QualifiedField t0' "leave_date"), Just "res6")
                      , (ExpressionFieldName (QualifiedField t0' "created"), Just "res7") ]

    joinTopLevelAggregate =
      testCase "Join against top-level aggregate" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           do (lastName, firstNameLength) <-
                  filter_ (\(_, charLength) -> fromMaybe_ 0 charLength >. 10) $
                  aggregate_ (\e -> (group_ (_employeeLastName e), max_ (as_ @Int32 (charLength_ (_employeeFirstName e))))) $
                  limit_ 10 (all_ (_employees employeeDbSettings))
              role <- relatedBy_ (_roles employeeDbSettings) (\r -> _roleName r ==. lastName)
              pure (firstNameLength, role, lastName)

         Just (InnerJoin (FromTable (TableFromSubSelect subselect) (Just (t0, Nothing)))
                         (FromTable (TableNamed (TableName Nothing "roles")) (Just (t1, Nothing)))
                         (Just joinCond) ) <-
           pure selectFrom

         joinCond @?= ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField t1 "name")) (ExpressionFieldName (QualifiedField t0 "res0"))
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0 "res1"), Just "res0" )
                                        , ( ExpressionFieldName (QualifiedField t1 "for_employee__first_name"), Just "res1" )
                                        , ( ExpressionFieldName (QualifiedField t1 "for_employee__last_name"), Just "res2" )
                                        , ( ExpressionFieldName (QualifiedField t1 "for_employee__created"), Just "res3" )
                                        , ( ExpressionFieldName (QualifiedField t1 "name"), Just "res4" )
                                        , ( ExpressionFieldName (QualifiedField t1 "started"), Just "res5" )
                                        , ( ExpressionFieldName (QualifiedField t0 "res0"), Just "res6" ) ]
         selectWhere @?= Nothing
         selectHaving @?= Nothing
         selectGrouping @?= Nothing

         Select { selectTable = SelectTable { .. }, selectLimit = Nothing
                , selectOffset = Nothing, selectOrdering = [] } <-
           pure subselect
         Just (FromTable (TableFromSubSelect employeesSelect) (Just (t0', Nothing))) <- pure selectFrom
         selectWhere @?= Nothing
         selectHaving @?= Just (ExpressionCompOp ">" Nothing (ExpressionCoalesce [ ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0' "res0"))]
                                                                                 , ExpressionValue (Value (0 :: Int32)) ])
                                                             (ExpressionValue (Value (10 :: Int32))))
         selectGrouping @?= Just (Grouping [ (ExpressionFieldName (QualifiedField t0' "res1")) ])
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0' "res1"), Just "res0" )
                                        , ( ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0' "res0"))], Just "res1" ) ]

         Select { selectTable = SelectTable { .. }, selectLimit = Just 10
                , selectOffset = Nothing, selectOrdering = [] } <-
           pure employeesSelect
         Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0'', Nothing))) <- pure selectFrom

         selectWhere @?= Nothing
         selectHaving @?= Nothing
         selectGrouping @?= Nothing
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0'' "first_name"), Just "res0" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "last_name"), Just "res1" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "phone_number"), Just "res2" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "age"), Just "res3" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "salary"), Just "res4" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "hire_date"), Just "res5" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "leave_date"), Just "res6" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "created"), Just "res7" ) ]

    joinTopLevelAggregate2 =
      testCase "Join against top-level aggregate (places reversed)" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           do role <- all_ (_roles employeeDbSettings)
              (lastName, firstNameLength) <-
                  filter_ (\(_, charLength) -> charLength >. 10) $
                  aggregate_ (\e -> ( group_ (_employeeLastName e)
                                    , fromMaybe_ 0 $ max_ (as_ @Int32 (charLength_ (_employeeFirstName e)))) ) $
                  limit_ 10 (all_ (_employees employeeDbSettings))
              guard_ (_roleName role ==. lastName)
              pure (firstNameLength, role, lastName)

         Just (InnerJoin (FromTable (TableNamed (TableName Nothing "roles")) (Just (t0, Nothing)))
                         (FromTable (TableFromSubSelect subselect) (Just (t1, Nothing)))
                         Nothing) <-
           pure selectFrom
         selectWhere @?= Just (ExpressionBinOp "AND"
                               (ExpressionCompOp ">" Nothing (ExpressionFieldName (QualifiedField t1 "res1")) (ExpressionValue (Value (10 :: Int32))))
                               (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField t0 "name")) (ExpressionFieldName (QualifiedField t1 "res0"))))
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t1 "res1"), Just "res0" )
                                        , ( ExpressionFieldName (QualifiedField t0 "for_employee__first_name"), Just "res1" )
                                        , ( ExpressionFieldName (QualifiedField t0 "for_employee__last_name"), Just "res2" )
                                        , ( ExpressionFieldName (QualifiedField t0 "for_employee__created"), Just "res3" )
                                        , ( ExpressionFieldName (QualifiedField t0 "name"), Just "res4" )
                                        , ( ExpressionFieldName (QualifiedField t0 "started"), Just "res5" )
                                        , ( ExpressionFieldName (QualifiedField t1 "res0"), Just "res6" ) ]
         selectHaving @?= Nothing
         selectGrouping @?= Nothing

         Select { selectTable = SelectTable { .. }, selectLimit = Nothing
                , selectOffset = Nothing, selectOrdering = [] } <-
           pure subselect
         Just (FromTable (TableFromSubSelect employeesSelect) (Just (t0', Nothing))) <- pure selectFrom
         selectWhere @?= Nothing
         selectHaving @?= Nothing
         selectGrouping @?= Just (Grouping [ (ExpressionFieldName (QualifiedField t0' "res1")) ])
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0' "res1"), Just "res0" )
                                        , ( ExpressionCoalesce [ ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0' "res0"))]
                                                               , ExpressionValue (Value (0 :: Int32)) ]
                                          , Just "res1" ) ]

         Select { selectTable = SelectTable { .. }, selectLimit = Just 10
                , selectOffset = Nothing, selectOrdering = [] } <-
           pure employeesSelect
         Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0'', Nothing))) <- pure selectFrom

         selectWhere @?= Nothing
         selectHaving @?= Nothing
         selectGrouping @?= Nothing
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0'' "first_name"), Just "res0" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "last_name"), Just "res1" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "phone_number"), Just "res2" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "age"), Just "res3" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "salary"), Just "res4" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "hire_date"), Just "res5" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "leave_date"), Just "res6" )
                                        , ( ExpressionFieldName (QualifiedField t0'' "created"), Just "res7" ) ]

-- | ORDER BY

orderBy :: TestTree
orderBy =
  testGroup "ORDER BY tests"
            [ simpleOrdering
            , orderCombination
            , orderLimitsOffsets
            , orderJoin
            , orderJoinReversed ]
  where
    simpleOrdering =
      testCase "Simple Ordering" $
      do SqlSelect Select { selectTable = select
                          , selectLimit = Just 100, selectOffset = Just 5
                          , selectOrdering = ordering } <-
           pure $ selectMock $
           limit_ 100 $ offset_ 5 $
           orderBy_ (asc_ . _roleStarted) $
           all_ (_roles employeeDbSettings)
         select @?= SelectTable { selectProjection =
                                    ProjExprs [ ( ExpressionFieldName (QualifiedField "t0" "for_employee__first_name"), Just "res0" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "for_employee__last_name"), Just "res1" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "for_employee__created"), Just "res2" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "name"), Just "res3" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "started"), Just "res4" ) ]
                                , selectFrom = Just (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t0", Nothing)))
                                , selectWhere = Nothing
                                , selectGrouping = Nothing
                                , selectHaving = Nothing
                                , selectQuantifier = Nothing }
         ordering @?= [ OrderingAsc (ExpressionFieldName (QualifiedField "t0" "started")) ]
    orderCombination =
      testCase "Order combined query" $
      do SqlSelect Select { selectTable = select
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = ordering } <-
           pure $ selectMock $
           orderBy_ (asc_ . _roleStarted) $
           (all_ (_roles employeeDbSettings) `unionAll_`
            all_ (_roles employeeDbSettings))

         let expSelect =
               SelectTable { selectProjection =
                                    ProjExprs [ ( ExpressionFieldName (QualifiedField "t0" "for_employee__first_name"), Just "res0" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "for_employee__last_name"), Just "res1" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "for_employee__created"), Just "res2" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "name"), Just "res3" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "started"), Just "res4" ) ]
                           , selectFrom = Just (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t0", Nothing)))
                           , selectWhere = Nothing
                           , selectGrouping = Nothing
                           , selectHaving = Nothing
                           , selectQuantifier = Nothing }
         select @?= UnionTables True expSelect expSelect
         ordering @?= [ OrderingAsc (ExpressionFieldName (UnqualifiedField "res4")) ]

    orderLimitsOffsets =
      testCase "Order after LIMIT/OFFSET" $
      do SqlSelect Select { selectTable = s
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = ordering } <-
           pure $ selectMock $
           orderBy_ (asc_ . _roleStarted) $
           limit_ 100 $ offset_ 5 $
           all_ (_roles employeeDbSettings)

         let rolesSelect =
               SelectTable { selectProjection =
                                    ProjExprs [ ( ExpressionFieldName (QualifiedField "t0" "for_employee__first_name"), Just "res0" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "for_employee__last_name"), Just "res1" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "for_employee__created"), Just "res2" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "name"), Just "res3" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "started"), Just "res4" ) ]
                           , selectFrom = Just (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t0", Nothing)))
                           , selectWhere = Nothing
                           , selectGrouping = Nothing
                           , selectHaving = Nothing
                           , selectQuantifier = Nothing }
         s @?= SelectTable { selectProjection =
                                    ProjExprs [ ( ExpressionFieldName (QualifiedField "t0" "res0"), Just "res0" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "res1"), Just "res1" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "res2"), Just "res2" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "res3"), Just "res3" )
                                              , ( ExpressionFieldName (QualifiedField "t0" "res4"), Just "res4" ) ]
                                , selectFrom = Just (FromTable (TableFromSubSelect (Select { selectTable = rolesSelect, selectLimit = Just 100
                                                                                           , selectOffset = Just 5, selectOrdering = [] })) (Just ("t0", Nothing)))
                                , selectWhere = Nothing
                                , selectGrouping = Nothing
                                , selectHaving = Nothing
                                , selectQuantifier = Nothing }
         ordering @?= [ OrderingAsc (ExpressionFieldName (QualifiedField "t0" "res4")) ]

    orderJoin =
      testCase "Order join" $
      do SqlSelect Select { selectTable = select
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           do oldestEmployees <- limit_ 10 $ orderBy_ ((,) <$> (asc_ <$> _employeeAge) <*> (desc_ <$> (charLength_ <$> _employeeFirstName))) $
                                 all_ (_employees employeeDbSettings)
              role <- relatedBy_ (_roles employeeDbSettings) (\r -> _roleForEmployee r ==. primaryKey oldestEmployees)
              pure (_employeeFirstName oldestEmployees, _employeeLastName oldestEmployees, _employeeAge oldestEmployees, _roleName role)

         selectProjection select @?= ProjExprs [ ( ExpressionFieldName (QualifiedField "t0" "res0"), Just "res0" )
                                               , ( ExpressionFieldName (QualifiedField "t0" "res1"), Just "res1" )
                                               , ( ExpressionFieldName (QualifiedField "t0" "res3"), Just "res2" )
                                               , ( ExpressionFieldName (QualifiedField "t1" "name"), Just "res3" ) ]
         let subselectExp = Select { selectTable = subselectTableExp, selectLimit = Just 10, selectOffset = Nothing
                                   , selectOrdering = [ OrderingAsc (ExpressionFieldName (QualifiedField "t0" "age"))
                                                      , OrderingDesc (ExpressionCharLength (ExpressionFieldName (QualifiedField "t0" "first_name"))) ] }
             subselectTableExp = SelectTable { selectProjection =
                                                 ProjExprs [ ( ExpressionFieldName (QualifiedField "t0" "first_name"), Just "res0" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "last_name"), Just "res1" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "phone_number"), Just "res2" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "age"), Just "res3" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "salary"), Just "res4" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res5" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res6" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "created"), Just "res7" ) ]
                                             , selectFrom = Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing)))
                                             , selectWhere = Nothing
                                             , selectGrouping = Nothing
                                             , selectHaving = Nothing
                                             , selectQuantifier = Nothing }
             joinCondExp = ExpressionBinOp "AND" (ExpressionBinOp "AND" (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField "t1" "for_employee__first_name"))
                                                                                                       (ExpressionFieldName (QualifiedField "t0" "res0")))
                                                                        (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField "t1" "for_employee__last_name"))
                                                                                                       (ExpressionFieldName (QualifiedField "t0" "res1"))))
                                                 (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField "t1" "for_employee__created"))
                                                                                (ExpressionFieldName (QualifiedField "t0" "res7")))
         selectFrom select @?= Just (InnerJoin (FromTable (TableFromSubSelect subselectExp) (Just ("t0", Nothing))) (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t1", Nothing)))
                                               (Just joinCondExp))
         selectWhere select @?= Nothing
         selectGrouping select @?= Nothing
         selectHaving select @?= Nothing

    orderJoinReversed =
      testCase "Order join (reversed)" $
      do SqlSelect Select { selectTable = s
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ selectMock $
           do role <- all_ (_roles employeeDbSettings)
              oldestEmployees <- limit_ 10 $ orderBy_ ((,) <$> (asc_ <$> _employeeAge) <*> (desc_ <$> (charLength_ <$> _employeeFirstName))) $
                                 all_ (_employees employeeDbSettings)
              guard_ (_roleForEmployee role ==. primaryKey oldestEmployees)
              pure (_employeeFirstName oldestEmployees, _employeeLastName oldestEmployees, _employeeAge oldestEmployees, _roleName role)

         selectProjection s @?= ProjExprs [ ( ExpressionFieldName (QualifiedField "t1" "res0"), Just "res0" )
                                          , ( ExpressionFieldName (QualifiedField "t1" "res1"), Just "res1" )
                                          , ( ExpressionFieldName (QualifiedField "t1" "res3"), Just "res2" )
                                          , ( ExpressionFieldName (QualifiedField "t0" "name"), Just "res3" ) ]
         selectWhere s @?= Just (ExpressionBinOp "AND"
                                  (ExpressionBinOp "AND"
                                    (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField "t0" "for_employee__first_name"))
                                      (ExpressionFieldName (QualifiedField "t1" "res0")))
                                    (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField "t0" "for_employee__last_name"))
                                      (ExpressionFieldName (QualifiedField "t1" "res1"))))
                                  (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField "t0" "for_employee__created"))
                                    (ExpressionFieldName (QualifiedField "t1" "res7"))))
         selectGrouping s @?= Nothing
         selectHaving s @?= Nothing

         let subselect = Select { selectTable = subselectTable, selectLimit = Just 10, selectOffset = Nothing
                                , selectOrdering = [ OrderingAsc (ExpressionFieldName (QualifiedField "t0" "age"))
                                                      , OrderingDesc (ExpressionCharLength (ExpressionFieldName (QualifiedField "t0" "first_name"))) ] }
             subselectTable = SelectTable { selectProjection =
                                                 ProjExprs [ ( ExpressionFieldName (QualifiedField "t0" "first_name"), Just "res0" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "last_name"), Just "res1" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "phone_number"), Just "res2" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "age"), Just "res3" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "salary"), Just "res4" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res5" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res6" )
                                                           , ( ExpressionFieldName (QualifiedField "t0" "created"), Just "res7" ) ]
                                             , selectFrom = Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing)))
                                             , selectWhere = Nothing
                                             , selectGrouping = Nothing
                                             , selectHaving = Nothing
                                             , selectQuantifier = Nothing }
         selectFrom s @?= Just (InnerJoin (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t0", Nothing)))
                                          (FromTable (TableFromSubSelect subselect) (Just ("t1", Nothing)))
                                          Nothing)

-- | HAVING clause should not be floated out of a join

joinHaving :: TestTree
joinHaving =
  testCase "HAVING clause should not be floated out of a join" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , selectLimit = Nothing, selectOffset = Nothing
                      , selectOrdering = [] } <-
       pure $ selectMock $
       do (age, maxFirstNameLength) <- filter_ (\(_, nameLength) -> fromMaybe_ 0 nameLength >=. 20) $
                                       aggregate_ (\e -> (group_ (_employeeAge e), max_ (as_ @Int32 (charLength_ (_employeeFirstName e))))) $
                                       all_ (_employees employeeDbSettings)
          role <- all_ (_roles employeeDbSettings)
          pure (age, maxFirstNameLength, _roleName role)

     Just (InnerJoin
            (FromTable (TableFromSubSelect subselect) (Just (t0, Nothing)))
            (FromTable (TableNamed (TableName Nothing "roles")) (Just (t1, Nothing)))
            Nothing) <- pure selectFrom
     selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t0 "res0"), Just "res0")
                                    , (ExpressionFieldName (QualifiedField t0 "res1"), Just "res1")
                                    , (ExpressionFieldName (QualifiedField t1 "name"), Just "res2") ]
     selectWhere @?= Nothing
     selectGrouping @?= Nothing
     selectHaving @?= Nothing

     Select { selectTable = SelectTable { .. }
            , selectLimit = Nothing, selectOffset = Nothing
            , selectOrdering = [] } <- pure subselect
     Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (t0, Nothing))) <- pure selectFrom
     selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t0 "age"), Just "res0")
                                    , (ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name"))], Just "res1") ]
     selectWhere @?= Nothing
     selectGrouping @?= Just (Grouping [ExpressionFieldName (QualifiedField t0 "age")])
     selectHaving @?= Just (ExpressionCompOp ">=" Nothing
                            (ExpressionCoalesce [ ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name"))]
                                                , ExpressionValue (Value (0 :: Int32)) ])
                            (ExpressionValue (Value (20 :: Int32))))

-- | Ensure that isJustE and isNothingE work correctly for simple types

maybeFieldTypes :: TestTree
maybeFieldTypes =
  testCase "Simple maybe field types" $
  do SqlSelect Select { selectTable = SelectTable { selectWhere = Just selectWhere, selectFrom } } <- pure $ selectMock $ do
       e <- all_ (_employees employeeDbSettings)
       guard_ (isNothing_ (_employeeLeaveDate e))
       pure e

     Just (FromTable (TableNamed (TableName Nothing "employees")) (Just (employees, Nothing))) <- pure selectFrom
     selectWhere @?= ExpressionIsNull (ExpressionFieldName (QualifiedField employees "leave_date"))

-- | Ensure isJustE and isNothingE work correctly for table and composite types

-- | Ensure maybeE works for simple types

-- | Ensure equality works for tables

tableEquality :: TestTree
tableEquality =
  testGroup "Equality comparisions among table expressions and table literals"
   [ tableExprToTableExpr, tableExprToTableLiteral ]
 where
   tableExprToTableExpr =
     testCase "Equality comparison between two table expressions" $
     do SqlSelect Select { selectTable = SelectTable { selectWhere = Just selectWhere, selectFrom } } <- pure $ selectMock $ do
          d <- all_ (_departments employeeDbSettings)
          guard_ (d ==. d)
          pure d

        Just (FromTable (TableNamed (TableName Nothing "departments")) (Just (depts, Nothing))) <- pure selectFrom

        let andE = ExpressionBinOp "AND"; orE = ExpressionBinOp "OR"
            eqE = ExpressionCompOp "==" Nothing

            maybeEqE a b = ExpressionCase [ (andE (ExpressionIsNull a) (ExpressionIsNull b), ExpressionValue (Value True))
                                          , (orE (ExpressionIsNull a) (ExpressionIsNull b), ExpressionValue (Value False))
                                          ]
                                          (eqE a b)

            nameCond = eqE (ExpressionFieldName (QualifiedField depts "name"))
                           (ExpressionFieldName (QualifiedField depts "name"))
            firstNameCond = maybeEqE (ExpressionFieldName (QualifiedField depts "head__first_name"))
                                (ExpressionFieldName (QualifiedField depts "head__first_name"))
            lastNameCond = maybeEqE (ExpressionFieldName (QualifiedField depts "head__last_name"))
                                    (ExpressionFieldName (QualifiedField depts "head__last_name"))
            createdCond = maybeEqE (ExpressionFieldName (QualifiedField depts "head__created"))
                                   (ExpressionFieldName (QualifiedField depts "head__created"))
        selectWhere @?= andE (andE (andE nameCond firstNameCond) lastNameCond) createdCond

   tableExprToTableLiteral =
     testCase "Equality comparison between table expression and table literal" $
     do now <- getCurrentTime

        let exp = DepartmentT "Sales" (EmployeeId (Just "Jane") (Just "Smith") (Just now))
        SqlSelect Select { selectTable = SelectTable { selectWhere = Just selectWhere, selectFrom } } <- pure $ selectMock $ do
          d <- all_ (_departments employeeDbSettings)
          guard_ (d ==. val_ exp)
          pure d

        Just (FromTable (TableNamed (TableName Nothing "departments")) (Just (depts, Nothing))) <- pure selectFrom

        let andE = ExpressionBinOp "AND"; orE = ExpressionBinOp "OR"
            eqE = ExpressionCompOp "==" Nothing

            maybeEqE a b = ExpressionCase [ (andE (ExpressionIsNull a) (ExpressionIsNull b), ExpressionValue (Value True))
                                          , (orE (ExpressionIsNull a) (ExpressionIsNull b), ExpressionValue (Value False))
                                          ]
                                          (eqE a b)

            nameCond = eqE (ExpressionFieldName (QualifiedField depts "name"))
                           (ExpressionValue (Value ("Sales" :: Text)))
            firstNameCond = maybeEqE (ExpressionFieldName (QualifiedField depts "head__first_name"))
                                     (ExpressionValue (Value ("Jane" :: Text)))
            lastNameCond = maybeEqE (ExpressionFieldName (QualifiedField depts "head__last_name"))
                                    (ExpressionValue (Value ("Smith" :: Text)))
            createdCond = maybeEqE (ExpressionFieldName (QualifiedField depts "head__created"))
                                  (ExpressionValue (Value now))
        selectWhere @?= andE (andE (andE nameCond firstNameCond) lastNameCond) createdCond

-- | Ensure related_ generates the correct ON conditions

related :: TestTree
related =
  testCase "related_ generate the correct ON conditions" $
  do SqlSelect Select { .. } <-
       pure $ selectMock $
       do r <- all_ (_roles employeeDbSettings)
          e <- related_ (_employees employeeDbSettings) (_roleForEmployee r)
          pure (e, r)

     pure ()

-- | Ensure select can be joined with UNION, INTERSECT, and EXCEPT

selectCombinators :: TestTree
selectCombinators =
  testGroup "UNION, INTERSECT, EXCEPT support"
    [ basicUnion
    , fieldNamesCapturedCorrectly
    , basicIntersect
    , basicExcept
    , equalProjectionsDontHaveSubSelects

    , unionWithGuards ]
  where
    basicUnion =
      testCase "Basic UNION support" $
      do let -- leaveDates, hireDates :: Q _ _ s ( QExpr Expression s Text, QExpr Expression s (Maybe _) )
             hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (as_ @Text $ val_ "hire", just_ (_employeeHireDate e))
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (as_ @Text $ val_ "leave", _employeeLeaveDate e)
         SqlSelect Select { selectTable = UnionTables False a b } <- pure (selectMock (union_ hireDates leaveDates))
         a @?= SelectTable Nothing
                           (ProjExprs [ (ExpressionValue (Value ("hire" :: Text)), Just "res0")
                                      , (ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res1") ])
                           (Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing))))
                           Nothing Nothing Nothing
         b @?= SelectTable Nothing
                           (ProjExprs [ (ExpressionValue (Value ("leave" :: Text)), Just "res0")
                                      , (ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res1") ])
                           (Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing))))
                           (Just (ExpressionIsNotNull (ExpressionFieldName (QualifiedField "t0" "leave_date"))))
                           Nothing Nothing
         pure ()

    fieldNamesCapturedCorrectly =
      testCase "UNION field names are propagated correctly" $
      do let -- leaveDates, hireDates :: Q _ _ s ( QExpr Expression s Text, QExpr Expression s Int32, QExpr Expression s (Maybe _) )
             hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (as_ @Text $ val_ "hire", _employeeAge e, just_ (_employeeHireDate e))
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (as_ @Text $ val_ "leave", _employeeAge e, _employeeLeaveDate e)
         SqlSelect Select { selectTable = SelectTable { .. }, selectLimit = Nothing, selectOffset = Nothing, selectOrdering = [] } <-
           pure (selectMock $ do
                    (type_, age, date) <- limit_ 10 (union_ hireDates leaveDates)
                    guard_ (age <. 22)
                    pure (type_, age + 23, date))

         Just (FromTable (TableFromSubSelect subselect) (Just (subselectTbl, Nothing))) <- pure selectFrom
         selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField subselectTbl "res0"), Just "res0")
                                        , (ExpressionBinOp "+" (ExpressionFieldName (QualifiedField subselectTbl "res1"))
                                                               (ExpressionValue (Value (23 :: Int32))), Just "res1")
                                        , (ExpressionFieldName (QualifiedField subselectTbl "res2"), Just "res2") ]
         selectHaving @?= Nothing
         selectGrouping @?= Nothing
         selectWhere @?= Just (ExpressionCompOp "<" Nothing (ExpressionFieldName (QualifiedField subselectTbl "res1")) (ExpressionValue (Value (22 :: Int32))))

         Select { selectTable = UnionTables False hireDatesQuery leaveDatesQuery
                , selectLimit = Just 10, selectOffset = Nothing
                , selectOrdering = []  } <-
           pure subselect

         hireDatesQuery @?= SelectTable Nothing
                                        (ProjExprs [ ( ExpressionValue (Value ("hire" :: Text)), Just "res0" )
                                                   , ( ExpressionFieldName (QualifiedField "t0" "age"), Just "res1" )
                                                   , ( ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res2" ) ])
                                        (Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing)))) Nothing Nothing Nothing
         leaveDatesQuery @?= SelectTable Nothing
                                         (ProjExprs [ ( ExpressionValue (Value ("leave" :: Text)), Just "res0" )
                                                    , ( ExpressionFieldName (QualifiedField "t0" "age"), Just "res1")
                                                    , ( ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res2") ])
                                         (Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing))))
                                         (Just (ExpressionIsNotNull (ExpressionFieldName (QualifiedField "t0" "leave_date"))))
                                         Nothing Nothing

         pure ()

    basicIntersect =
      testCase "intersect_ generates INTERSECT combination" $
      do let hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (_employeeFirstName e, _employeeLastName e)
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (_employeeFirstName e, _employeeLastName e)
         SqlSelect Select { selectTable = IntersectTables False _ _ } <- pure $ selectMock $ intersect_ hireDates leaveDates
         pure ()

    basicExcept =
      testCase "except_ generates EXCEPT combination" $
      do let hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (_employeeFirstName e, _employeeLastName e)
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (_employeeFirstName e, _employeeLastName e)
         SqlSelect Select { selectTable = ExceptTable False _ _ } <- pure $ selectMock $ except_ hireDates leaveDates
         pure ()

    equalProjectionsDontHaveSubSelects =
      testCase "Equal projections dont have sub-selects" $
      do let -- leaveDates, hireDates :: Q _ _ s ( QExpr Expression s Text, QExpr Expression s Text, QExpr Expression s Int32 )
             hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (_employeeFirstName e, _employeeLastName e, _employeeAge e)
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (_employeeFirstName e, _employeeLastName e, _employeeAge e)
         SqlSelect Select { selectTable = ExceptTable False _ _ } <-
             pure $ selectMock $ do
               (firstName, lastName, age) <- except_ hireDates leaveDates
               pure (firstName, (lastName, age))
         pure ()

    unionWithGuards =
      testCase "UNION with guards" $
      do let -- leaveDates, hireDates :: Q _ _ s ( QExpr Expression s Text, QExpr Expression s Int32, QExpr Expression s (Maybe _) )
             hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (as_ @Text $ val_ "hire", _employeeAge e, just_ (_employeeHireDate e))
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (as_ @Text $ val_ "leave", _employeeAge e, _employeeLeaveDate e)
         SqlSelect Select { selectTable =
                                SelectTable
                                { selectFrom = Just (FromTable (TableFromSubSelect (Select (UnionTables False a b) [] Nothing Nothing)) (Just (t0, Nothing)))
                                , selectProjection = proj
                                , selectWhere = Just where_
                                , selectGrouping = Nothing
                                , selectHaving = Nothing
                                , selectQuantifier = Nothing }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <- pure (selectMock (filter_ (\(_, age, _) -> age <. 40) (union_ hireDates leaveDates)))
         a @?= SelectTable Nothing
                           (ProjExprs [ (ExpressionValue (Value ("hire" :: Text)), Just "res0")
                                      , (ExpressionFieldName (QualifiedField "t0" "age"), Just "res1")
                                      , (ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res2") ])
                           (Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing))))
                           Nothing Nothing Nothing
         b @?= SelectTable Nothing
                           (ProjExprs [ (ExpressionValue (Value ("leave" :: Text)), Just "res0")
                                      , (ExpressionFieldName (QualifiedField "t0" "age"), Just "res1")
                                      , (ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res2") ])
                           (Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing))))
                           (Just (ExpressionIsNotNull (ExpressionFieldName (QualifiedField "t0" "leave_date"))))
                           Nothing Nothing

         proj @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0 "res0"), Just "res0" )
                            , ( ExpressionFieldName (QualifiedField t0 "res1"), Just "res1" )
                            , ( ExpressionFieldName (QualifiedField t0 "res2"), Just "res2" ) ]
         where_ @?= ExpressionCompOp "<" Nothing (ExpressionFieldName (QualifiedField "t0" "res1")) (ExpressionValue (Value (40 :: Int32)))
         pure ()


-- | Ensure simple selects can be used with limit_ and offset_

limitOffset :: TestTree
limitOffset =
  testGroup "LIMIT/OFFSET support"
  [ limitSupport, offsetSupport, limitOffsetSupport

  , limitPlacedOnUnion ]
  where
    limitSupport =
      testCase "Basic LIMIT support" $
      do SqlSelect Select { selectLimit, selectOffset } <-
           pure $ selectMock $ limit_ 100 $ limit_ 20 (all_ (_employees employeeDbSettings))

         selectLimit @?= Just 20
         selectOffset @?= Nothing

    offsetSupport =
      testCase "Basic OFFSET support" $
      do SqlSelect Select { selectLimit, selectOffset } <-
           pure $ selectMock $ offset_ 2 $ offset_ 100 (all_ (_employees employeeDbSettings))

         selectLimit @?= Nothing
         selectOffset @?= Just 102

    limitOffsetSupport =
      testCase "Basic LIMIT .. OFFSET .. support" $
      do SqlSelect Select { selectLimit, selectOffset } <-
           pure $ selectMock $ offset_ 2 $ limit_ 100 (all_ (_roles employeeDbSettings))

         selectLimit @?= Just 98
         selectOffset @?= Just 2

    limitPlacedOnUnion =
      testCase "LIMIT placed on UNION" $
      do SqlSelect Select { selectOffset = Nothing, selectLimit = Just 10
                          , selectOrdering = []
                          , selectTable = UnionTables False a b } <-
             pure $ selectMock $ limit_ 10 $ union_ (filter_ (\e -> _employeeAge e <. 40) (all_ (_employees employeeDbSettings)))
                                                (filter_ (\e -> _employeeAge e >. 50) (do { e <- all_ (_employees employeeDbSettings); pure e { _employeeFirstName = _employeeLastName e, _employeeLastName = _employeeFirstName e} }))

         selectProjection a @?= ProjExprs [ (ExpressionFieldName (QualifiedField "t0" "first_name"), Just "res0")
                                          , (ExpressionFieldName (QualifiedField "t0" "last_name"), Just "res1")
                                          , (ExpressionFieldName (QualifiedField "t0" "phone_number"), Just "res2")
                                          , (ExpressionFieldName (QualifiedField "t0" "age"), Just "res3")
                                          , (ExpressionFieldName (QualifiedField "t0" "salary"), Just "res4")
                                          , (ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res5")
                                          , (ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res6")
                                          , (ExpressionFieldName (QualifiedField "t0" "created"), Just "res7") ]
         selectFrom a @?= Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing)))
         selectWhere a @?= Just (ExpressionCompOp "<" Nothing (ExpressionFieldName (QualifiedField "t0" "age")) (ExpressionValue (Value (40 :: Int32))))
         selectGrouping a @?= Nothing
         selectHaving a @?= Nothing

         selectProjection b @?= ProjExprs [ (ExpressionFieldName (QualifiedField "t0" "last_name"), Just "res0")
                                          , (ExpressionFieldName (QualifiedField "t0" "first_name"), Just "res1")
                                          , (ExpressionFieldName (QualifiedField "t0" "phone_number"), Just "res2")
                                          , (ExpressionFieldName (QualifiedField "t0" "age"), Just "res3")
                                          , (ExpressionFieldName (QualifiedField "t0" "salary"), Just "res4")
                                          , (ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res5")
                                          , (ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res6")
                                          , (ExpressionFieldName (QualifiedField "t0" "created"), Just "res7") ]
         selectFrom b @?= Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing)))
         selectWhere b @?= Just (ExpressionCompOp ">" Nothing (ExpressionFieldName (QualifiedField "t0" "age")) (ExpressionValue (Value (50 :: Int32))))
         selectGrouping b @?= Nothing
         selectHaving b @?= Nothing

-- | Ensures exists predicates generate table names that do not overlap

existsTest :: TestTree
existsTest =
  testGroup "EXISTS() tests"
  [ existsInWhere ]
  where
    existsInWhere =
      testCase "EXISTS() in WHERE" $
      do SqlSelect Select { selectOffset = Nothing, selectLimit = Nothing
                          , selectOrdering = []
                          , selectTable = SelectTable { .. } } <-
           pure  $ selectMock $ do
             role <- all_ (_roles employeeDbSettings)
             guard_ (not_ (exists_ (do dept <- all_ (_departments employeeDbSettings)
                                       guard_ (_departmentName dept ==. _roleName role)
                                       pure (as_ @Int32 1))))
             pure role

         let existsQuery = Select
                         { selectOffset = Nothing, selectLimit = Nothing
                         , selectOrdering = []
                         , selectTable = SelectTable
                                       { selectQuantifier = Nothing
                                       , selectProjection = ProjExprs [ (ExpressionValue (Value (1 :: Int32)), Just "res0") ]
                                       , selectGrouping = Nothing
                                       , selectHaving = Nothing
                                       , selectWhere = Just joinExpr
                                       , selectFrom = Just (FromTable (TableNamed (TableName Nothing "departments")) (Just ("sub_t0", Nothing))) } }
             joinExpr = ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField "sub_t0" "name"))
                                                      (ExpressionFieldName (QualifiedField "t0" "name"))

         selectGrouping @?= Nothing
         selectWhere @?= Just (ExpressionUnOp "NOT" (ExpressionExists existsQuery))
         selectHaving @?= Nothing
         selectQuantifier @?= Nothing
         selectFrom @?= Just (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t0", Nothing)))

-- | UPDATE can correctly get the current value

updateCurrent :: TestTree
updateCurrent =
  testCase "UPDATE can use current value" $
  do SqlUpdate _ (Update { .. }) <-
       pure $ updateMock (_employees employeeDbSettings)
                         (\employee -> _employeeAge employee <-. current_ (_employeeAge employee) + 1)
                         (\employee -> _employeeFirstName employee ==. "Joe")

     updateTable @?= (TableName Nothing "employees")
     updateFields @?= [ (UnqualifiedField "age", ExpressionBinOp "+" (ExpressionFieldName (UnqualifiedField "age")) (ExpressionValue (Value (1 :: Int32)))) ]
     updateWhere @?= Just (ExpressionCompOp "==" Nothing (ExpressionFieldName (UnqualifiedField "first_name")) (ExpressionValue (Value ("Joe" :: String))))

updateNullable :: TestTree
updateNullable =
  testCase "UPDATE can correctly set a nullable field" $
  do curTime <- getCurrentTime

     let employeeKey :: PrimaryKey EmployeeT (Nullable Identity)
         employeeKey = EmployeeId (Just "John") (Just "Smith") (Just curTime)

     SqlUpdate _ (Update { .. }) <-
       pure $ updateMock (_departments employeeDbSettings)
                         (\department -> _departmentHead department <-. val_ employeeKey)
                         (\department -> _departmentName department ==. "Sales")

     updateTable @?= (TableName Nothing "departments")
     updateFields @?= [ (UnqualifiedField "head__first_name", ExpressionValue (Value ("John" :: Text)))
                      , (UnqualifiedField "head__last_name", ExpressionValue (Value ("Smith" :: Text)))
                      , (UnqualifiedField "head__created", ExpressionValue (Value curTime)) ]
     updateWhere @?= Just (ExpressionCompOp "==" Nothing
                             (ExpressionFieldName (UnqualifiedField "name"))
                             (ExpressionValue (Value ("Sales" :: String))))

-- | Ensure empty IN operators transform into FALSE

noEmptyIns :: TestTree
noEmptyIns =
  testCase "Empty INs are transformed to FALSE" $
  do  SqlSelect Select { selectTable = SelectTable {..} } <-
        pure $ selectMock $ do
          e <- all_ (_employees employeeDbSettings)
          guard_ (_employeeFirstName e `in_` [])
          pure e

      selectWhere @?= Just (ExpressionValue (Value False))

-- | ORDER BY in first join causes incorrect projection in subquery

gh70OrderByInFirstJoinCausesIncorrectProjection :: TestTree
gh70OrderByInFirstJoinCausesIncorrectProjection =
  testGroup "#70: ORDER BY as first join causes incorrect projection"
    [ testCase "Simple" simple
    , testCase "Grouping" grouping
    , testCase "Top-level OFFSET 0" topLevelOffs0
    ]
  where
    employees = "t0"; roles = "t1"

    eqE = ExpressionCompOp "==" Nothing
    andE = ExpressionBinOp "AND"

    firstNameCond = eqE (ExpressionFieldName (QualifiedField roles "for_employee__first_name"))
                        (ExpressionFieldName (QualifiedField employees "res0"))
    lastNameCond = eqE (ExpressionFieldName (QualifiedField roles "for_employee__last_name"))
                       (ExpressionFieldName (QualifiedField employees "res1"))
    createdCond = eqE (ExpressionFieldName (QualifiedField roles "for_employee__created"))
                      (ExpressionFieldName (QualifiedField employees "res7"))

    simple =
      do let employeesMakingSixFigures =
               orderBy_ (\e -> desc_ (_employeeAge e)) $
               filter_ (\e -> _employeeSalary e >=. 100000) $
               all_ (_employees employeeDbSettings)

             richEmployeesAndRoles = do
               employee <- employeesMakingSixFigures
               role <- all_ (_roles employeeDbSettings)
               guard_ (_roleForEmployee role `references_` employee)
               pure (_roleName role, employee)

         SqlSelect Select { selectTable = SelectTable { .. }
                          , .. } <-
           pure $ selectMock richEmployeesAndRoles

         selectProjection @?= ProjExprs
             [ (ExpressionFieldName (QualifiedField roles "name")    , Just "res0")
             , (ExpressionFieldName (QualifiedField employees "res0"), Just "res1")
             , (ExpressionFieldName (QualifiedField employees "res1"), Just "res2")
             , (ExpressionFieldName (QualifiedField employees "res2"), Just "res3")
             , (ExpressionFieldName (QualifiedField employees "res3"), Just "res4")
             , (ExpressionFieldName (QualifiedField employees "res4"), Just "res5")
             , (ExpressionFieldName (QualifiedField employees "res5"), Just "res6")
             , (ExpressionFieldName (QualifiedField employees "res6"), Just "res7")
             , (ExpressionFieldName (QualifiedField employees "res7"), Just "res8")
             ]
         selectGrouping @?= Nothing
         selectOrdering @?= []
         selectLimit @?= Nothing
         selectOffset @?= Nothing
         selectHaving @?= Nothing

         selectWhere @?= Just (andE (andE firstNameCond lastNameCond) createdCond)

         Just (InnerJoin (FromTable (TableFromSubSelect subselect) (Just ("t0", Nothing)))
                         (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t1", Nothing)))
                         Nothing) <- pure selectFrom

         Select { selectTable = SelectTable { .. }
                , .. } <- pure subselect

         selectGrouping @?= Nothing
         selectLimit @?= Nothing
         selectOffset @?= Nothing
         selectHaving @?= Nothing
         selectOrdering @?= [ OrderingDesc (ExpressionFieldName (QualifiedField "t0" "age")) ]
         selectWhere @?= Just (ExpressionCompOp ">=" Nothing
                                 (ExpressionFieldName (QualifiedField "t0" "salary"))
                                 (ExpressionValue (Value (100000 :: Double))))

         selectProjection @?= ProjExprs
           [ (ExpressionFieldName (QualifiedField "t0" "first_name")   , Just "res0")
           , (ExpressionFieldName (QualifiedField "t0" "last_name")    , Just "res1")
           , (ExpressionFieldName (QualifiedField "t0" "phone_number") , Just "res2")
           , (ExpressionFieldName (QualifiedField "t0" "age")          , Just "res3")
           , (ExpressionFieldName (QualifiedField "t0" "salary")       , Just "res4")
           , (ExpressionFieldName (QualifiedField "t0" "hire_date")    , Just "res5")
           , (ExpressionFieldName (QualifiedField "t0" "leave_date")   , Just "res6")
           , (ExpressionFieldName (QualifiedField "t0" "created")      , Just "res7")
           ]

    grouping = do
      let employeeNamesWhoMakeSixFiguresOnAverage =
            orderBy_ (\(fn, _) -> desc_ fn) $
            filter_ (\(_, sl) -> sl >=. val_ 100000) $
            aggregate_ (\e -> ( group_ (_employeeFirstName e)
                              , fromMaybe_ 0 $ avg_ (_employeeSalary e) )) $
            all_ (_employees employeeDbSettings)

          richEmployeeNamesAndRoles = do
            (fn, sl) <- employeeNamesWhoMakeSixFiguresOnAverage
            role <- all_ (_roles employeeDbSettings)
            let EmployeeId roleForEmployee_firstName _ _ = _roleForEmployee role
            guard_ (roleForEmployee_firstName ==. fn)
            pure (_roleName role, fn, sl)

      SqlSelect Select { selectTable = SelectTable
                                       { selectQuantifier = Nothing
                                       , selectProjection = ProjExprs
                                           [ (ExpressionFieldName (QualifiedField "t1" "name"), Just "res0")
                                           , (ExpressionFieldName (QualifiedField "t0" "res0"), Just "res1")
                                           , (ExpressionFieldName (QualifiedField "t0" "res1"), Just "res2")
                                           ]
                                       , selectGrouping = Nothing, selectHaving = Nothing
                                       , selectWhere = Just selectWhere
                                       , selectFrom = Just (InnerJoin (FromTable (TableFromSubSelect Select
                                                                                 { selectTable =
                                                                                   SelectTable { selectQuantifier = Nothing
                                                                                               , selectWhere = Nothing
                                                                                               , .. }
                                                                                 , selectLimit = Nothing, selectOffset = Nothing
                                                                                 , selectOrdering = selectOrdering }) (Just ("t0", Nothing)))
                                                                    (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t1", Nothing)))
                                                                    Nothing) }
                       , selectOrdering = []
                       , selectLimit = Nothing , selectOffset = Nothing } <-
        pure $ selectMock $ richEmployeeNamesAndRoles

      selectWhere @?= firstNameCond
      selectOrdering @?= [ OrderingDesc (ExpressionFieldName (QualifiedField "t0" "first_name")) ]
      selectGrouping @?= Just (Grouping [ ExpressionFieldName (QualifiedField "t0" "first_name") ])
      selectHaving @?= Just (ExpressionCompOp ">=" Nothing
                              (ExpressionCoalesce
                               [ ExpressionAgg "AVG" Nothing
                                  [ ExpressionFieldName (QualifiedField "t0" "salary") ]
                               , ExpressionValue (Value (0 :: Double)) ])
                              (ExpressionValue (Value (100000 :: Double))))
      selectProjection @?= ProjExprs
        [ ( ExpressionFieldName (QualifiedField "t0" "first_name"), Just "res0" )
        , ( ExpressionCoalesce
            [ ExpressionAgg "AVG" Nothing
              [ ExpressionFieldName (QualifiedField "t0" "salary") ]
            , ExpressionValue (Value (0 :: Double)) ]
          , Just "res1" )
        ]

    topLevelOffs0 = do
      SqlSelect actual <-
         pure $ selectMock $ do
           e <-  orderBy_ (\e -> desc_ (_employeeAge e)) $ offset_ 0 $
                 filter_ (\e -> _employeeSalary e >=. val_ 100000) $
                 all_ (_employees employeeDbSettings)
           role <- all_ (_roles employeeDbSettings)
           let EmployeeId roleForEmployee_firstName _ _ = _roleForEmployee role
           guard_ (roleForEmployee_firstName ==. _employeeFirstName e)
           pure (_roleName role, _employeeFirstName e)

      let expected =
            Select { selectTable = SelectTable
                     { selectQuantifier = Nothing
                     , selectProjection = ProjExprs
                       [ (ExpressionFieldName (QualifiedField "t1" "name"), Just "res0")
                       , (ExpressionFieldName (QualifiedField "t0" "res0"), Just "res1")
                       ]
                     , selectGrouping = Nothing, selectHaving = Nothing
                     , selectWhere = Just firstNameCond
                     , selectFrom = Just (InnerJoin (FromTable
                                                     (TableFromSubSelect Select
                                                       { selectTable = SelectTable
                                                         { selectQuantifier = Nothing
                                                         , selectProjection = ProjExprs
                                                           [ (ExpressionFieldName (QualifiedField "t0" "first_name")   , Just "res0")
                                                           , (ExpressionFieldName (QualifiedField "t0" "last_name")    , Just "res1")
                                                           , (ExpressionFieldName (QualifiedField "t0" "phone_number") , Just "res2")
                                                           , (ExpressionFieldName (QualifiedField "t0" "age")          , Just "res3")
                                                           , (ExpressionFieldName (QualifiedField "t0" "salary")       , Just "res4")
                                                           , (ExpressionFieldName (QualifiedField "t0" "hire_date")    , Just "res5")
                                                           , (ExpressionFieldName (QualifiedField "t0" "leave_date")   , Just "res6")
                                                           , (ExpressionFieldName (QualifiedField "t0" "created")      , Just "res7")
                                                           ]
                                                         , selectWhere = Just (ExpressionCompOp ">=" Nothing
                                                                               (ExpressionFieldName (QualifiedField "t0" "salary"))
                                                                               (ExpressionValue (Value (100000 :: Double))))
                                                         , selectFrom  = Just (FromTable (TableNamed (TableName Nothing "employees")) (Just ("t0", Nothing)))
                                                         , selectGrouping = Nothing, selectHaving = Nothing }
                                                       , selectLimit = Nothing, selectOffset = Just 0
                                                       , selectOrdering = [ OrderingDesc (ExpressionFieldName (QualifiedField "t0" "age"))] }) (Just ("t0", Nothing)))
                                           (FromTable (TableNamed (TableName Nothing "roles")) (Just ("t1", Nothing)))
                                           Nothing) }
                   , selectOrdering = []
                   , selectLimit = Nothing , selectOffset = Nothing }

      actual @?= expected
