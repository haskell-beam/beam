{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Database.Beam.Test.SQL
  ( tests ) where

import Database.Beam.Test.Schema hiding (tests)

import Database.Beam
import Database.Beam.Query
import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL hiding (leftJoin)
import Database.Beam.Backend.SQL.AST
import Database.Beam.Backend.SQL.Builder

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
                  , aggregates

                  , joinHaving

                  , maybeFieldTypes

                  , tableEquality
                  , related
                  , selectCombinators
                  , limitOffset

                  , updateCurrent ]

-- * Ensure simple select selects the right fields

simpleSelect :: TestTree
simpleSelect =
  testCase "All fields are present in a simple all_ query" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , .. } <- pure (select (all_ (_employees employeeDbSettings)))

     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectWhere @?= Nothing
     selectLimit @?= Nothing
     selectOffset @?= Nothing
     selectHaving @?= Nothing

     Just (FromTable (TableNamed "employees") (Just tblName)) <- pure selectFrom

     selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField tblName "first_name"), Just "res0")
                                    , (ExpressionFieldName (QualifiedField tblName "last_name"), Just "res1")
                                    , (ExpressionFieldName (QualifiedField tblName "phone_number"), Just "res2")
                                    , (ExpressionFieldName (QualifiedField tblName "age"), Just "res3")
                                    , (ExpressionFieldName (QualifiedField tblName "salary"), Just "res4")
                                    , (ExpressionFieldName (QualifiedField tblName "hire_date"), Just "res5")
                                    , (ExpressionFieldName (QualifiedField tblName "leave_date"), Just "res6")
                                    , (ExpressionFieldName (QualifiedField tblName "created"), Just "res7") ]

-- * Simple select with WHERE clause

simpleWhere :: TestTree
simpleWhere =
  testCase "guard_ clauses are successfully translated into WHERE statements" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , .. } <- pure $ select $
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

     Just (FromTable (TableNamed "employees") (Just employees)) <- pure selectFrom

     let salaryCond = ExpressionCompOp ">" Nothing (ExpressionFieldName (QualifiedField employees "salary")) (ExpressionValue (Value (120202 :: Double)))
         ageCond = ExpressionCompOp "<" Nothing (ExpressionFieldName (QualifiedField employees "age")) (ExpressionValue (Value (30 :: Int)))
         nameCond = ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField employees "first_name")) (ExpressionFieldName (QualifiedField employees "last_name"))

     selectWhere @?= Just (ExpressionBinOp "AND" salaryCond (ExpressionBinOp "AND" ageCond nameCond))

-- * Ensure that multiple tables are correctly joined

simpleJoin :: TestTree
simpleJoin =
  testCase "Introducing multiple tables results in an inner join" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , .. } <- pure $ select $
                                do e <- all_ (_employees employeeDbSettings)
                                   r <- all_ (_roles employeeDbSettings)
                                   pure (_employeePhoneNumber e, _roleName r)

     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectWhere @?= Nothing
     selectLimit @?= Nothing
     selectOffset @?= Nothing
     selectHaving @?= Nothing

     Just (InnerJoin (FromTable (TableNamed "employees") (Just employees))
                     (FromTable (TableNamed "roles") (Just roles))
                     Nothing) <- pure selectFrom

     selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField employees "phone_number"), Just "res0" )
                                    , ( ExpressionFieldName (QualifiedField roles "name"), Just "res1") ]

-- * Ensure that multiple joins on the same table are correctly referenced

selfJoin :: TestTree
selfJoin =
  testCase "Table names are unique and properly used in self joins" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , .. } <- pure $ select $
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

     Just (InnerJoin (InnerJoin (FromTable (TableNamed "employees") (Just e1))
                                (FromTable (TableNamed "employees") (Just e2))
                                (Just joinCondition12))
                     (FromTable (TableNamed "employees") (Just e3))
                     (Just joinCondition123)) <- pure selectFrom

     assertBool "Table names are not unique" (e1 /= e2 && e1 /= e3 && e2 /= e3)
     joinCondition12 @?= ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField e1 "first_name"))
                                                       (ExpressionFieldName (QualifiedField e2 "last_name"))
     joinCondition123 @?= ExpressionBinOp "AND" (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField e1 "phone_number"))
                                                  (ExpressionFieldName (QualifiedField e3 "last_name")))
                            (ExpressionCompOp "==" Nothing (ExpressionFieldName (QualifiedField e3 "phone_number")) (ExpressionFieldName (QualifiedField e2 "first_name")))

-- * Ensure that right joins are properly generated

leftJoin :: TestTree
leftJoin =
  testCase "leftJoin_ generates the right join" $
  do SqlSelect Select { selectTable = SelectTable { selectWhere = Nothing, selectFrom } } <-
       pure $ select $
       do r <- all_ (_roles employeeDbSettings)
          e <- leftJoin_ (_employees employeeDbSettings) (\e -> primaryKey e ==. _roleForEmployee r)
          pure (e, r)

     Just (LeftJoin (FromTable (TableNamed "roles") (Just roles))
                    (FromTable (TableNamed "employees") (Just employees))
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

-- * Ensure that aggregations cause the correct GROUP BY clause to be generated

aggregates :: TestTree
aggregates =
  testGroup "Aggregate support"
    [ basicAggregate
    , basicHaving
    , aggregateInJoin
    , aggregateInJoinReverse ]
  where
    basicAggregate =
      testCase "Basic aggregate support" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ select $
           aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
           do e <- all_ (_employees employeeDbSettings)
              pure e

         Just (FromTable (TableNamed "employees") (Just t0)) <- pure selectFrom
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
           pure $ select $
           do (age, maxNameLength) <- aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
                                      all_ (_employees employeeDbSettings)
              guard_ (maxNameLength >. 42)
              pure (age, maxNameLength)

         Just (FromTable (TableNamed "employees") (Just t0)) <- pure selectFrom
         selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField t0 "age"), Just "res0" )
                                        , ( ExpressionAgg "MAX" Nothing [ ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name")) ], Just "res1") ]
         selectWhere @?= Nothing
         selectGrouping @?= Just (Grouping [ ExpressionFieldName (QualifiedField t0 "age") ])
         selectHaving @?= Just (ExpressionCompOp ">" Nothing (ExpressionAgg "MAX" Nothing [ ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name")) ])
                                                             (ExpressionValue (Value (42 :: Int))))

    aggregateInJoin =
      testCase "Aggregate in JOIN" $
      do SqlSelect Select { selectTable = SelectTable { .. }
                          , selectLimit = Nothing, selectOffset = Nothing
                          , selectOrdering = [] } <-
           pure $ select $
           do (age, maxFirstNameLength) <- aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
                                           all_ (_employees employeeDbSettings)
              role <- all_ (_roles employeeDbSettings)
              pure (age, maxFirstNameLength, _roleName role)

         Just (InnerJoin (FromTable (TableFromSubSelect subselect) (Just t0))
                         (FromTable (TableNamed "roles") (Just t1))
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
         Just (FromTable (TableNamed "employees") (Just t0)) <- pure selectFrom
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
           pure $ select $
           do role <- all_ (_roles employeeDbSettings)
              (age, maxFirstNameLength) <- aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
                                           all_ (_employees employeeDbSettings)
              pure (age, maxFirstNameLength, _roleName role)

         Just (InnerJoin (FromTable (TableNamed "roles") (Just t0))
                         (FromTable (TableFromSubSelect subselect) (Just t1))
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
         Just (FromTable (TableNamed "employees") (Just t0)) <- pure selectFrom
         selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t0 "age"), Just "res0")
                                        , (ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name"))], Just "res1") ]
         selectWhere @?= Nothing
         selectGrouping @?= Just (Grouping [ExpressionFieldName (QualifiedField t0 "age")])
         selectHaving @?= Nothing

-- * HAVING clause should not be floated out of a join

joinHaving :: TestTree
joinHaving =
  testCase "HAVING clause should not be floated out of a join" $
  do SqlSelect Select { selectTable = SelectTable { .. }
                      , selectLimit = Nothing, selectOffset = Nothing
                      , selectOrdering = [] } <-
       pure $ select $
       do (age, maxFirstNameLength) <- filter_ (\(_, nameLength) -> nameLength >=. 20) $
                                       aggregate_ (\e -> (group_ (_employeeAge e), max_ (charLength_ (_employeeFirstName e)))) $
                                       all_ (_employees employeeDbSettings)
          role <- all_ (_roles employeeDbSettings)
          pure (age, maxFirstNameLength, _roleName role)

     Just (InnerJoin
            (FromTable (TableFromSubSelect subselect) (Just t0))
            (FromTable (TableNamed "roles") (Just t1))
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
     Just (FromTable (TableNamed "employees") (Just t0)) <- pure selectFrom
     selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField t0 "age"), Just "res0")
                                    , (ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name"))], Just "res1") ]
     selectWhere @?= Nothing
     selectGrouping @?= Just (Grouping [ExpressionFieldName (QualifiedField t0 "age")])
     selectHaving @?= Just (ExpressionCompOp ">=" Nothing (ExpressionAgg "MAX" Nothing [ExpressionCharLength (ExpressionFieldName (QualifiedField t0 "first_name"))]) (ExpressionValue (Value (20 :: Int))))

-- * Ensure that isJustE and isNothingE work correctly for simple types

maybeFieldTypes :: TestTree
maybeFieldTypes =
  testCase "Simple maybe field types" $
  do SqlSelect Select { selectTable = SelectTable { selectWhere = Just selectWhere, selectFrom } } <- pure $ select $ do
       e <- all_ (_employees employeeDbSettings)
       guard_ (isNothing_ (_employeeLeaveDate e))
       pure e

     Just (FromTable (TableNamed "employees") (Just employees)) <- pure selectFrom
     selectWhere @?= ExpressionIsNull (ExpressionFieldName (QualifiedField employees "leave_date"))

-- * Ensure isJustE and isNothingE work correctly for table and composite types

-- * Ensure maybeE works for simple types

-- * Ensure equality works for tables

tableEquality :: TestTree
tableEquality =
  testGroup "Equality comparisions among table expressions and table literals"
   [ tableExprToTableExpr, tableExprToTableLiteral ]
 where
   tableExprToTableExpr =
     testCase "Equality comparison between two table expressions" $
     do SqlSelect Select { selectTable = SelectTable { selectWhere = Just selectWhere, selectFrom } } <- pure $ select $ do
          d <- all_ (_departments employeeDbSettings)
          guard_ (d ==. d)
          pure d

        Just (FromTable (TableNamed "departments") (Just depts)) <- pure selectFrom

        let andE = ExpressionBinOp "AND"
            eqE = ExpressionCompOp "==" Nothing
            nameCond = eqE (ExpressionFieldName (QualifiedField depts "name"))
                           (ExpressionFieldName (QualifiedField depts "name"))
            firstNameCond = eqE (ExpressionFieldName (QualifiedField depts "head__first_name"))
                                (ExpressionFieldName (QualifiedField depts "head__first_name"))
            lastNameCond = eqE (ExpressionFieldName (QualifiedField depts "head__last_name"))
                               (ExpressionFieldName (QualifiedField depts "head__last_name"))
            createdCond = eqE (ExpressionFieldName (QualifiedField depts "head__created"))
                              (ExpressionFieldName (QualifiedField depts "head__created"))
        selectWhere @?= andE (andE (andE nameCond firstNameCond) lastNameCond) createdCond

   tableExprToTableLiteral =
     testCase "Equality comparison between table expression and table literal" $
     do now <- getCurrentTime

        let exp = DepartmentT "Sales" (EmployeeId (Just "Jane") (Just "Smith") (Just (Auto (Just now))))
        SqlSelect Select { selectTable = SelectTable { selectWhere = Just selectWhere, selectFrom } } <- pure $ select $ do
          d <- all_ (_departments employeeDbSettings)
          guard_ (d ==. val_ exp)
          pure d

        Just (FromTable (TableNamed "departments") (Just depts)) <- pure selectFrom

        let andE = ExpressionBinOp "AND"
            eqE = ExpressionCompOp "==" Nothing
            nameCond = eqE (ExpressionFieldName (QualifiedField depts "name"))
                           (ExpressionValue (Value ("Sales" :: Text)))
            firstNameCond = eqE (ExpressionFieldName (QualifiedField depts "head__first_name"))
                                (ExpressionValue (Value (Just ("Jane" :: Text))))
            lastNameCond = eqE (ExpressionFieldName (QualifiedField depts "head__last_name"))
                               (ExpressionValue (Value (Just ("Smith" :: Text))))
            createdCond = eqE (ExpressionFieldName (QualifiedField depts "head__created"))
                              (ExpressionValue (Value (Just (Auto (Just now)))))
        selectWhere @?= andE (andE (andE nameCond firstNameCond) lastNameCond) createdCond

-- * Ensure related_ generates the correct ON conditions

related :: TestTree
related =
  testCase "related_ generate the correct ON conditions" $
  do SqlSelect Select { .. } <-
       pure $ select $
       do r <- all_ (_roles employeeDbSettings)
          e <- related_ (_employees employeeDbSettings) (_roleForEmployee r)
          pure (e, r)

     pure ()

-- * Ensure select can be joined with UNION, INTERSECT, and EXCEPT

selectCombinators :: TestTree
selectCombinators =
  testGroup "UNION, INTERSECT, EXCEPT support"
    [ basicUnion
    , fieldNamesCapturedCorrectly
    , basicIntersect
    , basicExcept
    , equalProjectionsDontHaveSubSelects ]
  where
    basicUnion =
      testCase "Basic UNION support" $
      do let hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (as_ @Text $ val_ "hire", just_ (_employeeHireDate e))
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (as_ @Text $ val_ "leave", _employeeLeaveDate e)
         SqlSelect Select { selectTable = UnionTables False a b } <- pure (select (union_ hireDates leaveDates))
         a @?= SelectTable (ProjExprs [ (ExpressionValue (Value ("hire" :: Text)), Just "res0")
                                      , (ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res1") ])
                           (Just (FromTable (TableNamed "employees") (Just "t0")))
                           Nothing Nothing Nothing
         b @?= SelectTable (ProjExprs [ (ExpressionValue (Value ("leave" :: Text)), Just "res0")
                                      , (ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res1") ])
                           (Just (FromTable (TableNamed "employees") (Just "t0")))
                           (Just (ExpressionIsNotNull (ExpressionFieldName (QualifiedField "t0" "leave_date"))))
                           Nothing Nothing
         pure ()

    fieldNamesCapturedCorrectly =
      testCase "UNION field names are propagated correctly" $
      do let hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (as_ @Text $ val_ "hire", _employeeAge e, just_ (_employeeHireDate e))
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (as_ @Text $ val_ "leave", _employeeAge e, _employeeLeaveDate e)
         SqlSelect Select { selectTable = SelectTable { .. }, selectLimit = Nothing, selectOffset = Nothing, selectOrdering = [] } <-
           pure (select $ do
                    (type_, age, date) <- limit_ 10 (union_ hireDates leaveDates)
                    guard_ (age <. 22)
                    pure (type_, age + 23, date))

         Just (FromTable (TableFromSubSelect subselect) (Just subselectTbl)) <- pure selectFrom
         selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField subselectTbl "res0"), Just "res0")
                                        , (ExpressionBinOp "+" (ExpressionFieldName (QualifiedField subselectTbl "res1"))
                                                               (ExpressionValue (Value (23 :: Int))), Just "res1")
                                        , (ExpressionFieldName (QualifiedField subselectTbl "res2"), Just "res2") ]
         selectHaving @?= Nothing
         selectGrouping @?= Nothing
         selectWhere @?= Just (ExpressionCompOp "<" Nothing (ExpressionFieldName (QualifiedField subselectTbl "res1")) (ExpressionValue (Value (22 :: Int))))

         Select { selectTable = UnionTables False hireDatesQuery leaveDatesQuery
                , selectLimit = Just 10, selectOffset = Nothing
                , selectOrdering = []  } <-
           pure subselect

         hireDatesQuery @?= SelectTable (ProjExprs [ ( ExpressionValue (Value ("hire" :: Text)), Just "res0" )
                                                   , ( ExpressionFieldName (QualifiedField "t0" "age"), Just "res1" )
                                                   , ( ExpressionFieldName (QualifiedField "t0" "hire_date"), Just "res2" ) ])
                                        (Just (FromTable (TableNamed "employees") (Just "t0"))) Nothing Nothing Nothing
         leaveDatesQuery @?= SelectTable (ProjExprs [ ( ExpressionValue (Value ("leave" :: Text)), Just "res0" )
                                                    , ( ExpressionFieldName (QualifiedField "t0" "age"), Just "res1")
                                                    , ( ExpressionFieldName (QualifiedField "t0" "leave_date"), Just "res2") ])
                                         (Just (FromTable (TableNamed "employees") (Just "t0")))
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
         SqlSelect Select { selectTable = IntersectTables False _ _ } <- pure $ select $ intersect_ hireDates leaveDates
         pure ()

    basicExcept =
      testCase "except_ generates EXCEPT combination" $
      do let hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (_employeeFirstName e, _employeeLastName e)
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (_employeeFirstName e, _employeeLastName e)
         SqlSelect Select { selectTable = ExceptTable False _ _ } <- pure $ select $ except_ hireDates leaveDates
         pure ()

    equalProjectionsDontHaveSubSelects =
      testCase "Equal projections dont have sub-selects" $
      do let hireDates = do e <- all_ (_employees employeeDbSettings)
                            pure (_employeeFirstName e, _employeeLastName e, _employeeAge e)
             leaveDates = do e <- all_ (_employees employeeDbSettings)
                             guard_ (isJust_ (_employeeLeaveDate e))
                             pure (_employeeFirstName e, _employeeLastName e, _employeeAge e)
         SqlSelect Select { selectTable = ExceptTable False _ _ } <-
             pure $ select $ do
               (firstName, lastName, age) <- except_ hireDates leaveDates
               pure (firstName, (lastName, age))
         pure ()

-- * Ensure simple selects can be used with limit_ and offset_

limitOffset :: TestTree
limitOffset =
  testGroup "LIMIT/OFFSET support"
  [ limitSupport, offsetSupport, limitOffsetSupport ]
  where
    limitSupport =
      testCase "Basic LIMIT support" $
      do SqlSelect Select { selectLimit, selectOffset } <-
           pure $ select $ limit_ 100 $ limit_ 20 (all_ (_employees employeeDbSettings))

         selectLimit @?= Just 20
         selectOffset @?= Nothing

    offsetSupport =
      testCase "Basic OFFSET support" $
      do SqlSelect Select { selectLimit, selectOffset } <-
           pure $ select $ offset_ 2 $ offset_ 100 (all_ (_employees employeeDbSettings))

         selectLimit @?= Nothing
         selectOffset @?= Just 102

    limitOffsetSupport =
      testCase "Basic LIMIT .. OFFSET .. support" $
      do SqlSelect Select { selectLimit, selectOffset } <-
           pure $ select $ offset_ 2 $ limit_ 100 (all_ (_roles employeeDbSettings))

         selectLimit @?= Just 98
         selectOffset @?= Just 2

-- * Ensure exists_ generates the correct sub-select

-- * Ensure results can be correctly sorted with orderBy

-- * UPDATE can correctly get the current value

updateCurrent :: TestTree
updateCurrent =
  testCase "UPDATE can use current value" $
  do SqlUpdate Update { .. } <-
       pure $ update (_employees employeeDbSettings)
                     (\employee -> [ _employeeAge employee <-. current_ (_employeeAge employee) + 1])
                     (\employee -> _employeeFirstName employee ==. "Joe")

     updateTable @?= "employees"
     updateFields @?= [ (UnqualifiedField "age", ExpressionBinOp "+" (ExpressionFieldName (UnqualifiedField "age")) (ExpressionValue (Value (1 :: Int)))) ]
     updateWhere @?= Just (ExpressionCompOp "==" Nothing (ExpressionFieldName (UnqualifiedField "first_name")) (ExpressionValue (Value ("Joe" :: String))))
