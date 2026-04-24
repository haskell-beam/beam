module Database.Beam.Sqlite.Test.Insert (tests) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime, Day(..), UTCTime(..), fromGregorian, getCurrentTime, secondsToDiffTime)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Sqlite
import Database.SQLite.Simple (execute_)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Database.Beam.Sqlite.Test

tests :: TestTree
tests = testGroup "Insertion tests"
  [ testInsertReturningColumnOrder
  , testInsertOnlyDefaults
  , expectFail testUpsertOnlyDefaults
  , testInsertSomeDefaults
  ]

data TestTableT f
  = TestTable
  { ttId :: C f Int32
  , ttFirstName :: C f Text
  , ttLastName  :: C f Text
  , ttAge       :: C f Int32
  , ttDateJoined :: C f LocalTime
  , ttDateLoggedIn :: C f UTCTime
  } deriving (Generic, Beamable)

deriving instance Show (TestTableT Identity)
deriving instance Eq (TestTableT Identity)

instance Table TestTableT where
  data PrimaryKey TestTableT f = TestTableKey (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = TestTableKey <$> ttId

data TestTableDb entity
  = TestTableDb
  { dbTestTable :: entity (TableEntity TestTableT)
  } deriving (Generic, Database Sqlite)

testDatabase :: DatabaseSettings be TestTableDb
testDatabase = defaultDbSettings

testInsertReturningColumnOrder :: TestTree
testInsertReturningColumnOrder = testCase "runInsertReturningList with mismatching column order" $ do
  now <- getCurrentTime
  let zeroUtcTime = UTCTime (ModifiedJulianDay 0) 0
  let oneUtcTime = UTCTime (fromGregorian 1 0 0) (secondsToDiffTime 0)

  withTestDb $ \conn -> do
    execute_ conn "CREATE TABLE test_table ( date_joined TIMESTAMP NOT NULL, date_logged_in TIMESTAMP WITH TIME ZONE NOT NULL, first_name TEXT NOT NULL, id INT PRIMARY KEY, age INT NOT NULL, last_name TEXT NOT NULL )"
    inserted <-
      runBeamSqlite conn $ runInsertReturningList $
      insert (dbTestTable testDatabase) $
      insertExpressions [ TestTable 0 (concat_ [ "j", "im" ]) "smith" 19 currentTimestamp_ (val_ zeroUtcTime)
                        , TestTable 1 "sally" "apple" ((val_ 56 + val_ 109) `div_` 5) currentTimestamp_ (val_ oneUtcTime)
                        , TestTable 4 "blah" "blah" (-1) currentTimestamp_ (val_ now) ]

    let dateJoined : _ = ttDateJoined <$> inserted

        expected = [ TestTable 0 "jim" "smith" 19 dateJoined zeroUtcTime
                   , TestTable 1 "sally" "apple" 33 dateJoined oneUtcTime
                   , TestTable 4 "blah" "blah" (-1) dateJoined now ]

    assertEqual "insert values" expected inserted

data WithDefaultsT f = WithDefaults
  { _id :: C f (SqlSerial Int32)
  , _value :: C f Text
  } deriving (Generic, Beamable)

deriving instance Show (WithDefaultsT Identity)
deriving instance Eq (WithDefaultsT Identity)

instance Table WithDefaultsT where
  data PrimaryKey WithDefaultsT f = WithDefaultsKey (C f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = WithDefaultsKey <$> _id

data WithDefaultsDb entity = WithDefaultsDb
  { tblWithDefaults :: entity (TableEntity WithDefaultsT)
  } deriving (Generic, Database Sqlite)

withDefaultsDb :: DatabaseSettings be WithDefaultsDb
withDefaultsDb = defaultDbSettings

-- | Regression test for <https://github.com/haskell-beam/beam/issues/607 #607>
testInsertOnlyDefaults :: TestTree
testInsertOnlyDefaults = testCase "insert only default values" $
  withTestDb $ \conn -> do
    execute_ conn "CREATE TABLE with_defaults (id INTEGER PRIMARY KEY, value TEXT NOT NULL DEFAULT \"unknown\")"
    inserted <- runBeamSqlite conn $ runInsertReturningList $
      insert (tblWithDefaults withDefaultsDb) $ insertExpressions
        [ WithDefaults default_ default_
        , WithDefaults default_ $ val_ "other"
        ]
    assertEqual "inserted values"
      [ WithDefaults 1 "unknown"
      , WithDefaults 2 "other"
      ]
      inserted

testUpsertOnlyDefaults :: TestTree
testUpsertOnlyDefaults = testCase "upsert only default values" $
  withTestDb $ \conn -> do
    execute_ conn "CREATE TABLE with_defaults (id INTEGER PRIMARY KEY, value TEXT NOT NULL DEFAULT \"unknown\")"
    inserted <- runBeamSqlite conn $ runInsertReturningList $
      insertOnConflict (tblWithDefaults withDefaultsDb)
        ( insertExpressions
            [ WithDefaults default_ default_
            , WithDefaults default_ $ val_ "other"
            ]
        )
        anyConflict
        onConflictDoNothing
    assertEqual "inserted values"
      [ WithDefaults 1 "unknown"
      , WithDefaults 2 "other"
      ]
      inserted


data MixTableT f
  = MixTable
  { mtId :: C f (SqlSerial Int32)
  , mtField1 :: C f Int32
  , mtField2 :: C f Int32
  , mtField3 :: C f Int32
  } deriving (Generic, Beamable)

deriving instance Show (MixTableT Identity)
deriving instance Eq (MixTableT Identity)

instance Table MixTableT where
  data PrimaryKey MixTableT f = MixTableKey (C f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey (MixTable { mtId = i }) = MixTableKey i

data MixTableDb entity
  = MixTableDb
  { dbMixTable :: entity (TableEntity MixTableT)
  } deriving (Generic, Database Sqlite)

mixDatabase :: DatabaseSettings be MixTableDb
mixDatabase = defaultDbSettings

-- | Test grouping logic for queries containing different DEFAULT column layouts.
testInsertSomeDefaults :: TestTree
testInsertSomeDefaults = testCase "insert mix of default values" $ do
  withTestDb $ \conn -> do
    execute_ conn "CREATE TABLE mix_table ( id INTEGER PRIMARY KEY AUTOINCREMENT, field1 INT DEFAULT 11, field2 INT DEFAULT 22, field3 INT DEFAULT 33)"
    inserted <-
      runBeamSqlite conn $ runInsertReturningList $
      insert (dbMixTable mixDatabase) $
      insertExpressions [ MixTable default_ default_ 1001     10001
                        , MixTable 202      2        default_ default_
                        , MixTable 303      3        default_ default_
                        , MixTable default_ default_ 4004     40004
                        ]

    let expected = [ MixTable 1   11 1001 10001
                   , MixTable 202 2  22   33
                   , MixTable 303 3  22   33
                   , MixTable 304 11 4004 40004
                   ]

    assertEqual "insert values" expected inserted
