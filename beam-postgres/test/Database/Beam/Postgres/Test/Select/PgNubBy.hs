{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Beam.Postgres.Test.Select.PgNubBy (tests) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorian)
import Database.Beam
import Database.Beam.Migrate (defaultMigratableDbSettings)
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.Beam.Postgres.Test (withTestPostgres)
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO ByteString -> TestTree
tests getConn =
  testGroup
    "pgNubBy_ / nub_ with window functions (issue #746)"
    [ testPgNubByWithLead getConn,
      testNubWithLead getConn
    ]

data PersonT f = Person
  { name :: C f Text,
    validFrom :: C f Day,
    idx :: C f Int32
  }
  deriving (Generic)

type Person = PersonT Identity

deriving instance Show Person

deriving instance Eq Person

instance Beamable PersonT

instance Table PersonT where
  data PrimaryKey PersonT f = PersonKey (C f Text)
    deriving stock (Generic)
    deriving anyclass (Beamable)

  primaryKey Person {name} = PersonKey name

data Db f = Db
  { persons :: f (TableEntity PersonT)
  }
  deriving (Generic)

instance Database Postgres Db

db :: DatabaseSettings Postgres Db
db = defaultDbSettings

day :: Integer -> Int -> Int -> Day
day = fromGregorian

-- | Six rows across three distinct @validFrom@ dates so deduplication is
-- observable, and the window function has something non-trivial to do.
seedRows :: [Person]
seedRows =
  [ Person "A" (day 2025 1 1) 1,
    Person "B" (day 2025 1 1) 1,
    Person "C" (day 2025 1 2) 2,
    Person "D" (day 2025 1 2) 2,
    Person "E" (day 2025 1 3) 3,
    Person "F" (day 2025 1 3) 3
  ]

setupDb :: Connection -> IO ()
setupDb conn = runBeamPostgres conn $ do
  void $ autoMigrate migrationBackend (defaultMigratableDbSettings @Postgres @Db)
  runInsert $ insert (persons db) $ insertValues seedRows

-- | Reproducer for issue #746 with @pgNubBy_@.
--
-- We deduplicate the @validFrom@ column with @pgNubBy_ id@, then pair each
-- distinct date with the next using @lead1_@. The expected behaviour is that
-- @LEAD@ runs over the deduplicated rows, yielding the next distinct date.
--
-- The bug: beam emits @SELECT DISTINCT ON (...) ..., LEAD(...) OVER (...)@ in
-- a single SELECT, so @LEAD@ sees the duplicate input rows and each date ends
-- up paired with itself.
testPgNubByWithLead :: IO ByteString -> TestTree
testPgNubByWithLead getConn = testCase "pgNubBy_ feeding lead1_" $
  withTestPostgres "issue_746_pg_nub_by" getConn $ \conn -> do
    setupDb conn
    results <-
      runBeamPostgres conn $
        runSelectReturningList $
          select $
            withWindow_
              (\vf -> frame_ noPartition_ (orderPartitionBy_ (asc_ vf)) noBounds_)
              (\vf w -> (vf, lead1_ vf `over_` w))
              (pgNubBy_ id $ fmap validFrom $ all_ (persons db))

    let expected =
          [ (day 2025 1 1, Just (day 2025 1 2)),
            (day 2025 1 2, Just (day 2025 1 3)),
            (day 2025 1 3, Nothing)
          ]
    assertEqual "lead1_ over pgNubBy_ should pair each distinct date with the next" expected results

-- | Reproducer for issue #746 with @nub_@.
--
-- Same shape as 'testPgNubByWithLead' but using the backend-agnostic @nub_@.
-- With the bug, @SELECT DISTINCT vf, LEAD(vf) OVER (...) FROM ...@ does not
-- deduplicate (each @(vf, lead)@ tuple is already distinct) so the result has
-- six rows instead of three.
testNubWithLead :: IO ByteString -> TestTree
testNubWithLead getConn = testCase "nub_ feeding lead1_" $
  withTestPostgres "issue_746_nub" getConn $ \conn -> do
    setupDb conn
    results <-
      runBeamPostgres conn $
        runSelectReturningList $
          select $
            withWindow_
              (\vf -> frame_ noPartition_ (orderPartitionBy_ (asc_ vf)) noBounds_)
              (\vf w -> (vf, lead1_ vf `over_` w))
              (nub_ $ fmap validFrom $ all_ (persons db))

    let expected =
          [ (day 2025 1 1, Just (day 2025 1 2)),
            (day 2025 1 2, Just (day 2025 1 3)),
            (day 2025 1 3, Nothing)
          ]
    assertEqual "lead1_ over nub_ should pair each distinct date with the next" expected results
