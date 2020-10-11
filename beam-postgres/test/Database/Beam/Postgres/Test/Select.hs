{-# LANGUAGE LambdaCase #-}

module Database.Beam.Postgres.Test.Select (tests) where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.Text (Text)
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit

import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple (autoMigrate)
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Migrate (migrationBackend)
import           Database.Beam.Postgres.Test
import qualified Database.PostgreSQL.Simple as Pg


import           Test.Tasty
import           Test.Tasty.HUnit

import           Unsafe.Coerce

tests :: IO ByteString -> TestTree
tests getConn = testGroup "Selection Tests"
  [ testReturningMany getConn
  ]

testReturningMany :: IO ByteString -> TestTree
testReturningMany getConn = testCase "runReturningMany (batching via cursor) works" $
  withTestPostgres "run_returning_many_cursor" getConn $ \conn -> do
    result <- runBeamPostgres conn $ runSelectReturningMany
      (select $ pgUnnestArray $ array_ $ (as_ @Int32 . val_) <$> [0..rowCount - 1])
      (\fetch ->
        let count n = fetch >>= \case
              Nothing -> pure n
              Just _  -> count (n + 1)
        in count 0)
    assertEqual "result" rowCount result
 where
  rowCount = 500
  runSelectReturningMany ::
    (FromBackendRow Postgres x) =>
    SqlSelect Postgres x -> (Pg (Maybe x) -> Pg a) -> Pg a
  runSelectReturningMany (SqlSelect s) =
    runReturningMany (selectCmd s)

