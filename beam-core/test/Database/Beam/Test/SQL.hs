{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database.Beam.Test.SQL
  ( tests ) where

import Database.Beam.Test.Schema hiding (tests)

import Database.Beam
import Database.Beam.Query
import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL92 hiding (rightJoin)
import Database.Beam.Backend.SQL92.AST

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
                  , rightJoin
                  , maybeFieldTypes

                  , tableEquality ]

-- * Ensure simple select selects the right fields

simpleSelect :: TestTree
simpleSelect =
  testCase "All fields are present in a simple all_ query" $
  do SqlSelect Select { .. } <- pure (select (all_ (_employees employeeDbSettings)))

     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectWhere @?= ExpressionValue (Value True)
     selectLimit @?= Nothing
     selectOffset @?= Nothing

     Just (FromTable (TableNamed "employees") (Just tblName)) <- pure selectFrom

     selectProjection @?= ProjExprs [ (ExpressionFieldName (QualifiedField tblName "first_name"), Nothing)
                                    , (ExpressionFieldName (QualifiedField tblName "last_name"), Nothing)
                                    , (ExpressionFieldName (QualifiedField tblName "phone_number"), Nothing)
                                    , (ExpressionFieldName (QualifiedField tblName "age"), Nothing)
                                    , (ExpressionFieldName (QualifiedField tblName "salary"), Nothing)
                                    , (ExpressionFieldName (QualifiedField tblName "hire_date"), Nothing)
                                    , (ExpressionFieldName (QualifiedField tblName "leave_date"), Nothing)
                                    , (ExpressionFieldName (QualifiedField tblName "created"), Nothing) ]

-- * Simple select with WHERE clause

simpleWhere :: TestTree
simpleWhere =
  testCase "guard_ clauses are successfully translated into WHERE statements" $
  do SqlSelect Select { .. } <- pure $ select $
                                do e <- all_ (_employees employeeDbSettings)
                                   guard_ (_employeeSalary e >. 120202 &&.
                                           _employeeAge e <. 30 &&.
                                           _employeeFirstName e ==. _employeeLastName e)
                                   pure e
     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectLimit @?= Nothing
     selectOffset @?= Nothing

     Just (FromTable (TableNamed "employees") (Just employees)) <- pure selectFrom

     let salaryCond = ExpressionBinOp ">" (ExpressionFieldName (QualifiedField employees "salary")) (ExpressionValue (Value (120202 :: Double)))
         ageCond = ExpressionBinOp "<" (ExpressionFieldName (QualifiedField employees "age")) (ExpressionValue (Value (30 :: Int)))
         nameCond = ExpressionBinOp "==" (ExpressionFieldName (QualifiedField employees "first_name")) (ExpressionFieldName (QualifiedField employees "last_name"))

     selectWhere @?= ExpressionBinOp "AND" (ExpressionValue (Value True))
                      (ExpressionBinOp "AND" salaryCond (ExpressionBinOp "AND" ageCond nameCond))

-- * Ensure that multiple tables are correctly joined

simpleJoin :: TestTree
simpleJoin =
  testCase "Introducing multiple tables results in an inner join" $
  do SqlSelect Select { .. } <- pure $ select $
                                do e <- all_ (_employees employeeDbSettings)
                                   r <- all_ (_roles employeeDbSettings)
                                   pure (_employeePhoneNumber e, _roleName r)

     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectWhere @?= ExpressionValue (Value True)
     selectLimit @?= Nothing
     selectOffset @?= Nothing

     Just (InnerJoin (FromTable (TableNamed "employees") (Just employees))
                     (FromTable (TableNamed "roles") (Just roles))
                     Nothing) <- pure selectFrom

     selectProjection @?= ProjExprs [ ( ExpressionFieldName (QualifiedField employees "phone_number"), Nothing )
                                    , ( ExpressionFieldName (QualifiedField roles "name"), Nothing) ]

-- * Ensure that multiple joins on the same table are correctly referenced

selfJoin :: TestTree
selfJoin =
  testCase "Table names are unique and properly used in self joins" $
  do SqlSelect Select { .. } <- pure $ select $
                                do e1 <- all_ (_employees employeeDbSettings)
                                   e2 <- relatedBy_ (_employees employeeDbSettings)
                                                    (\e2 -> _employeeFirstName e1 ==. _employeeLastName e2)
                                   e3 <- relatedBy_ (_employees employeeDbSettings)
                                                    (\e3 -> _employeePhoneNumber e1 ==. _employeeLastName e3 &&. _employeePhoneNumber e3 ==. _employeeFirstName e2)
                                   pure (_employeeFirstName e1, _employeeLastName e2, _employeePhoneNumber e3)

     selectGrouping @?= Nothing
     selectOrdering @?= []
     selectWhere @?= ExpressionValue (Value True)
     selectLimit @?= Nothing
     selectOffset @?= Nothing

     Just (InnerJoin (InnerJoin (FromTable (TableNamed "employees") (Just e1))
                                (FromTable (TableNamed "employees") (Just e2))
                                (Just joinCondition12))
                     (FromTable (TableNamed "employees") (Just e3))
                     (Just joinCondition123)) <- pure selectFrom

     assertBool "Table names are not unique" (e1 /= e2 && e1 /= e3 && e2 /= e3)
     joinCondition12 @?= ExpressionBinOp "==" (ExpressionFieldName (QualifiedField e1 "first_name"))
                                              (ExpressionFieldName (QualifiedField e2 "last_name"))
     joinCondition123 @?= ExpressionBinOp "AND" (ExpressionBinOp "==" (ExpressionFieldName (QualifiedField e1 "phone_number"))
                                                  (ExpressionFieldName (QualifiedField e3 "last_name")))
                            (ExpressionBinOp "==" (ExpressionFieldName (QualifiedField e3 "phone_number")) (ExpressionFieldName (QualifiedField e2 "first_name")))


-- * Ensure that field names are correctly mapped from a subquery

-- * Ensure that right joins are properly generated

rightJoin :: TestTree
rightJoin =
  testCase "rightJoin_ generates the right join" $
  do SqlSelect Select { selectWhere, selectFrom } <-
       pure $ select $
       do r <- all_ (_roles employeeDbSettings)
          e <- leftJoin_ (_employees employeeDbSettings) (\e -> primaryKey e ==. _roleForEmployee r)
          pure (e, r)

     Just (LeftJoin (FromTable (TableNamed "roles") (Just roles))
                    (FromTable (TableNamed "employees") (Just employees))
                    (Just cond)) <- pure selectFrom

     let andE = ExpressionBinOp "AND"
         eqE = ExpressionBinOp "=="
         trueE = ExpressionValue (Value True)

         firstNameCond = eqE (ExpressionFieldName (QualifiedField employees "first_name"))
                             (ExpressionFieldName (QualifiedField roles "for_employee__first_name"))
         lastNameCond = eqE (ExpressionFieldName (QualifiedField employees "last_name"))
                            (ExpressionFieldName (QualifiedField roles "for_employee__last_name"))
         createdCond = eqE (ExpressionFieldName (QualifiedField employees "created"))
                           (ExpressionFieldName (QualifiedField roles "for_employee__created"))

     cond @?= andE (andE (andE trueE firstNameCond) lastNameCond) createdCond
     selectWhere @?= ExpressionValue (Value True)

-- * Ensure that aggregations cause the correct GROUP BY clause to be generated

-- * Ensure that isJustE and isNothingE work correctly for simple types

maybeFieldTypes :: TestTree
maybeFieldTypes =
  testCase "Simple maybe field types" $
  do SqlSelect Select { selectWhere, selectFrom } <- pure $ select $ do
       e <- all_ (_employees employeeDbSettings)
       guard_ (isNothing_ (_employeeLeaveDate e))
       pure e

     Just (FromTable (TableNamed "employees") (Just employees)) <- pure selectFrom
     selectWhere @?= ExpressionBinOp "AND" (ExpressionValue (Value True))
                       (ExpressionIsNothing (ExpressionFieldName (QualifiedField employees "leave_date")))

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
     do SqlSelect Select { selectWhere, selectFrom } <- pure $ select $ do
          d <- all_ (_departments employeeDbSettings)
          guard_ (d ==. d)
          pure d

        Just (FromTable (TableNamed "departments") (Just depts)) <- pure selectFrom

        let andE = ExpressionBinOp "AND"
            eqE = ExpressionBinOp "=="
            nameCond = eqE (ExpressionFieldName (QualifiedField depts "name"))
                           (ExpressionFieldName (QualifiedField depts "name"))
            firstNameCond = eqE (ExpressionFieldName (QualifiedField depts "head__first_name"))
                                (ExpressionFieldName (QualifiedField depts "head__first_name"))
            lastNameCond = eqE (ExpressionFieldName (QualifiedField depts "head__last_name"))
                               (ExpressionFieldName (QualifiedField depts "head__last_name"))
            createdCond = eqE (ExpressionFieldName (QualifiedField depts "head__created"))
                              (ExpressionFieldName (QualifiedField depts "head__created"))
        selectWhere @?= andE (ExpressionValue (Value True))
                             (andE (andE (andE (andE (ExpressionValue (Value True)) nameCond) firstNameCond) lastNameCond) createdCond)

   tableExprToTableLiteral =
     testCase "Equality comparison between table expression and table literal" $
     do now <- getCurrentTime

        let exp = DepartmentT "Sales" (EmployeeId (Just "Jane") (Just "Smith") (Just (Auto (Just now))))
        SqlSelect Select { selectWhere, selectFrom } <- pure $ select $ do
          d <- all_ (_departments employeeDbSettings)
          guard_ (d ==. val_ exp)
          pure d

        Just (FromTable (TableNamed "departments") (Just depts)) <- pure selectFrom

        let andE = ExpressionBinOp "AND"
            eqE = ExpressionBinOp "=="
            nameCond = eqE (ExpressionFieldName (QualifiedField depts "name"))
                           (ExpressionValue (Value ("Sales" :: Text)))
            firstNameCond = eqE (ExpressionFieldName (QualifiedField depts "head__first_name"))
                                (ExpressionValue (Value (Just ("Jane" :: Text))))
            lastNameCond = eqE (ExpressionFieldName (QualifiedField depts "head__last_name"))
                               (ExpressionValue (Value (Just ("Smith" :: Text))))
            createdCond = eqE (ExpressionFieldName (QualifiedField depts "head__created"))
                              (ExpressionValue (Value (Just (Auto (Just now)))))
        selectWhere @?= andE (ExpressionValue (Value True))
                             (andE (andE (andE (andE (ExpressionValue (Value True)) nameCond) firstNameCond) lastNameCond) createdCond)

-- * Ensure related_ and lookup_ generate the correct join conditions

-- * Ensure select can be joined with UNION, INTERSECT, and EXCLUDING

-- * Ensure simple selects can be used with limit_ and offset_

-- * Ensure complex selects can be used with limit_ and offset_

-- * Ensure exists_ generates the correct sub-select

-- * Ensure results can be correctly sorted with orderBy
