module Database.Beam.Postgres.Test.Select (tests) where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Int
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit

import           Database.Beam
import           Database.Beam.Postgres

import           Database.Beam.Postgres.Test

tests :: IO ByteString -> TestTree
tests getConn = testGroup "Selection Tests"
  [ testPgArrayToJSON getConn
  ]

testPgArrayToJSON :: IO ByteString -> TestTree
testPgArrayToJSON getConn = testCase "array_to_json" $
  withTestPostgres "array_to_json" getConn $ \conn -> do
    let values :: [Int32] = [1, 2, 3]
    actual :: [PgJSON Value] <-
      runBeamPostgres conn $ runSelectReturningList $ select $
        return $ pgArrayToJson $ val_ $ V.fromList values
    assertEqual "JSON list" [PgJSON $ toJSON values] actual
