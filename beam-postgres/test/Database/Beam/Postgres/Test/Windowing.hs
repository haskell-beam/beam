{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Beam.Postgres.Test.Windowing (tests) where

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.Beam.Postgres.Test

import Control.Exception (SomeException (..), handle)

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)

import Control.Monad (void)
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO ByteString -> TestTree
tests postgresConn =
    testGroup
        "Windowing unit tests"
        [ testLead1 postgresConn
        , testLag1 postgresConn
        , testLead postgresConn
        , testLag postgresConn
        , testLeadWithDefault postgresConn
        , testLagWithDefault postgresConn
        ]

testLead1 :: IO ByteString -> TestTree
testLead1 = testCase "lead1_" . windowingQueryTest query expectation
  where
    query =
        withWindow_
            ( \Person{name} ->
                frame_
                    noPartition_
                    (orderPartitionBy_ (asc_ name))
                    noBounds_
            )
            ( \Person{name} w ->
                (name, lead1_ name `over_` w)
            )
            (all_ $ persons db)
    expectation = [("Alice", Just "Bob"), ("Bob", Just "Claire"), ("Claire", Nothing)]

testLag1 :: IO ByteString -> TestTree
testLag1 = testCase "lag1_" . windowingQueryTest query expectation
  where
    query =
        withWindow_
            ( \Person{name} ->
                frame_
                    noPartition_
                    (orderPartitionBy_ (asc_ name))
                    noBounds_
            )
            ( \Person{name} w ->
                (name, lag1_ name `over_` w)
            )
            (all_ $ persons db)
    expectation = [("Alice", Nothing), ("Bob", Just "Alice"), ("Claire", Just "Bob")]

testLead :: IO ByteString -> TestTree
testLead getConnStr =
    testGroup
        "lead_"
        [ testCase "n=1" $ windowingQueryTest (query 1) [("Alice", Just "Bob"), ("Bob", Just "Claire"), ("Claire", Nothing)] getConnStr
        , testCase "n=2" $ windowingQueryTest (query 2) [("Alice", Just "Claire"), ("Bob", Nothing), ("Claire", Nothing)] getConnStr
        ]
  where
    query n =
        withWindow_
            ( \Person{name} ->
                frame_
                    noPartition_
                    (orderPartitionBy_ (asc_ name))
                    noBounds_
            )
            ( \Person{name} w ->
                (name, lead_ name (val_ (n :: Int32)) `over_` w)
            )
            (all_ $ persons db)
    expectation1 = []

testLag :: IO ByteString -> TestTree
testLag getConnStr =
    testGroup
        "lag_"
        [ testCase "n=1" $ windowingQueryTest (query 1) [("Alice", Nothing), ("Bob", Just "Alice"), ("Claire", Just "Bob")] getConnStr
        , testCase "n=2" $ windowingQueryTest (query 2) [("Alice", Nothing), ("Bob", Nothing), ("Claire", Just "Alice")] getConnStr
        ]
  where
    query n =
        withWindow_
            ( \Person{name} ->
                frame_
                    noPartition_
                    (orderPartitionBy_ (asc_ name))
                    noBounds_
            )
            ( \Person{name} w ->
                (name, lag_ name (val_ (n :: Int32)) `over_` w)
            )
            (all_ $ persons db)
    expectation = []


testLeadWithDefault :: IO ByteString -> TestTree
testLeadWithDefault getConnStr =
    testGroup
        "leadWithDefault_"
        [ testCase "n=1" $ windowingQueryTest (query 1 "default") [("Alice", "Bob"), ("Bob", "Claire"), ("Claire", "default")] getConnStr
        , testCase "n=2" $ windowingQueryTest (query 2 "default") [("Alice", "Claire"), ("Bob", "default"), ("Claire", "default")] getConnStr
        ]
  where
    query n def =
        withWindow_
            ( \Person{name} ->
                frame_
                    noPartition_
                    (orderPartitionBy_ (asc_ name))
                    noBounds_
            )
            ( \Person{name} w ->
                (name, leadWithDefault_ name (val_ (n :: Int32)) (val_ def) `over_` w)
            )
            (all_ $ persons db)
    expectation1 = []


testLagWithDefault :: IO ByteString -> TestTree
testLagWithDefault getConnStr =
    testGroup
        "lagWithDefault_"
        [ testCase "n=1" $ windowingQueryTest (query 1 "default") [("Alice", "default"), ("Bob", "Alice"), ("Claire", "Bob")] getConnStr
        , testCase "n=2" $ windowingQueryTest (query 2 "default") [("Alice", "default"), ("Bob", "default"), ("Claire", "Alice")] getConnStr
        ]
  where
    query n def =
        withWindow_
            ( \Person{name} ->
                frame_
                    noPartition_
                    (orderPartitionBy_ (asc_ name))
                    noBounds_
            )
            ( \Person{name} w ->
                (name, lagWithDefault_ name (val_ (n :: Int32)) (val_ def) `over_` w)
            )
            (all_ $ persons db)
    expectation = []



data PersonT f = Person
    { name :: C f Text
    }
    deriving (Generic)

type Person = PersonT Identity

type PersonExpr s = PersonT (QExpr Postgres s)

deriving instance Show Person
deriving instance Eq Person

instance Beamable PersonT

instance Table PersonT where
    data PrimaryKey PersonT f = PersonKey (C f Text)
        deriving stock (Generic)
        deriving anyclass (Beamable)

    primaryKey Person{name} = PersonKey name

data Db f = Db
    { persons :: f (TableEntity PersonT)
    }
    deriving (Generic)

instance Database Postgres Db

db :: DatabaseSettings Postgres Db
db = defaultDbSettings

windowingQueryTest ::
    (Eq a, Show a, Eq b, Show b, FromBackendRow Postgres a, FromBackendRow Postgres b) =>
    Q Postgres Db QBaseScope (QExpr Postgres s a, QExpr Postgres s b) ->
    [(a, b)] ->
    IO ByteString ->
    Assertion
windowingQueryTest query expectation getConnStr =
    withTestPostgres "db_windowing_psql" getConnStr $
        \conn -> do
            prepareTable conn
            results <-
                runBeamPostgres conn $
                    runSelectReturningList $
                        select query

            assertEqual "Unexpected" expectation results

prepareTable :: Connection -> IO ()
prepareTable conn =
    runBeamPostgres conn $ do
        void $ autoMigrate migrationBackend (defaultMigratableDbSettings @Postgres @Db)
        runInsert $
            insert (persons db) $
                insertValues
                    [ Person "Alice"
                    , Person "Bob"
                    , Person "Claire"
                    ]
