{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Sqlite.Test.Select (tests) where

import Data.Int (Int32)

import Database.Beam
import Database.Beam.Sqlite
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Database.Beam.Sqlite.Test

tests :: TestTree
tests = testGroup "Selection tests"
  [ testExceptValues
  , testInRowValues
  , testInSelect
  , testSelectFromValues
  ]

data Pair f = Pair
  { _left :: C f Bool
  , _right :: C f Bool
  } deriving (Generic, Beamable)

testInRowValues :: TestTree
testInRowValues = testCase "IN with row values works" $
  withTestDb $ \conn -> do
    result <- runBeamSqlite conn $ runSelectReturningList $ select $ do
      let p :: forall ctx s. Pair (QGenExpr ctx Sqlite s)
          p = val_ $ Pair False False
      return $ p `in_` [p, p]
    assertEqual "result" [True] result

testInSelect :: TestTree
testInSelect = testCase "IN (SELECT ...) works" $
  withTestDb $ \conn -> do
    result <- runBeamSqlite conn $ runSelectReturningList $ select $ do
      let x  = as_ @Int32 (val_ 1)
      return $ x `inQuery_` (pure (as_ @Int32 $ val_ 1))
    assertEqual "result" [True] result

    result <- runBeamSqlite conn $ runSelectReturningList $ select $ do
      let x  = as_ @Int32 (val_ 1)
      return $ x `inQuery_`  (pure (as_ @Int32 $ val_ 2))
    assertEqual "result" [False] result

testExceptValues :: TestTree
testExceptValues = testCase "EXCEPT with VALUES works" $
  withTestDb $ \conn -> do
    result <- runBeamSqlite conn $ runSelectReturningList $ select $
      values_ [as_ @Bool $ val_ True, val_ False] `except_` values_ [val_ False]
    assertEqual "result" [True] result

testSelectFromValues :: TestTree
testSelectFromValues = testCase "SELECT * FROM (VALUES ...) works by factoring out a common CTE" $
  withTestDb $ \conn -> do
    xs <- runBeamSqlite conn $ runSelectReturningList $ select $ do
            (a, b) <- values_ [(val_ 1, val_ 2), (val_ 2, val_ 3)]
            (c, d) <- values_ [(val_ 2, val_ 4), (val_ 2, val_ 3)]
            guard_ (as_ @Int32 b ==. as_ @Int32 c)
            pure (as_ @Int32 a, as_ @Int32 d)
    assertEqual "result" [(1,4), (1,3)] xs
