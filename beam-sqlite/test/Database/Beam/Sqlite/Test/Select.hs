module Database.Beam.Sqlite.Test.Select (tests) where

import Control.Exception
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Database.Beam
import Database.Beam.Sqlite

import Database.Beam.Sqlite.Test

tests :: TestTree
tests = testGroup "Selection tests"
  [ expectFail testExceptValues
  ]

-- | Regression test for <https://github.com/haskell-beam/beam/issues/326 #326>
testExceptValues :: TestTree
testExceptValues = testCase "EXCEPT with VALUES works" $
  flip catch asFailure $ withTestDb $ \conn -> do
    result <- runBeamSqlite conn $ runSelectReturningList $ select $
      values_ [as_ @Bool $ val_ True, val_ False] `except_` values_ [val_ False]
    assertEqual "result" [True] result
  where asFailure (e :: SomeException) = assertFailure $ show e
