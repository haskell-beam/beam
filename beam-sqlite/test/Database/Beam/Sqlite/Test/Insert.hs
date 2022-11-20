module Database.Beam.Sqlite.Test.Insert (tests) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime, Day(..), UTCTime(..), fromGregorian, getCurrentTime, secondsToDiffTime)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Sqlite hiding (runInsertReturningList)
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

    let dateJoined = ttDateJoined (head inserted)

        expected = [ TestTable 0 "jim" "smith" 19 dateJoined zeroUtcTime
                   , TestTable 1 "sally" "apple" 33 dateJoined oneUtcTime
                   , TestTable 4 "blah" "blah" (-1) dateJoined now ]

    assertEqual "insert values" inserted expected

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
    assertEqual "inserted values" inserted
      [ WithDefaults 1 "unknown"
      , WithDefaults 2 "other"
      ]

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
    assertEqual "inserted values" inserted
      [ WithDefaults 1 "unknown"
      , WithDefaults 2 "other"
      ]
