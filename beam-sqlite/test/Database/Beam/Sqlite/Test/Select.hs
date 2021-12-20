module Database.Beam.Sqlite.Test.Select (tests) where

import Database.Beam
import Database.Beam.Sqlite
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Database.Beam.Sqlite.Test

tests :: TestTree
tests = testGroup "Selection tests"
  [ expectFail testExceptValues
  , testInRowValues
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

-- | Regression test for <https://github.com/haskell-beam/beam/issues/326 #326>
testExceptValues :: TestTree
testExceptValues = testCase "EXCEPT with VALUES works" $
  withTestDb $ \conn -> do
    result <- runBeamSqlite conn $ runSelectReturningList $ select $
      values_ [as_ @Bool $ val_ True, val_ False] `except_` values_ [val_ False]
    assertEqual "result" [True] result
