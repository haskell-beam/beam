{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Beam.Postgres.Test.Select.PgNubBy (tests) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorian)
import Database.Beam
import Database.Beam.Migrate ( defaultMigratableDbSettings )
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.Beam.Postgres.Test (withTestPostgres)
import Database.PostgreSQL.Simple (Query, execute_)
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO ByteString -> TestTree
tests getConn =
    testGroup
        "pgNubBy_"
        [testIssue746 getConn]

data PersonT f = Person
    { name :: C f Text
    , title :: C f Text
    , index :: C f Int32
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

testIssue746 :: IO ByteString -> TestTree
testIssue746 getConn = testCase "pgNubBy_ (issue 746)" $ withTestPostgres "issue_746" getConn $ \conn -> do
    rs <- runBeamPostgresDebug print conn $ do
        void $ autoMigrate migrationBackend (defaultMigratableDbSettings @Postgres @Db)
        runInsert $
            insert (persons db) $
                insertValues
                    [ -- Commit 1
                      Person "A" "Mechanic" 1
                    , Person "B" "Consultant" 1
                    , -- Commit 2
                      Person "C" "Cleaner" 2
                    , Person "D" "Assistant" 2
                    , -- Commit 3
                      Person "E" "CEO" 3
                    , Person "F" "CFO" 3
                    ]

        -- TODO: two layers of using nub_, to ensure that the second nub_ doesn't use
        -- data from the first
        runSelectReturningList $ select $ do
            person <- pgNubBy_ index $ all_ $ persons db
            pure (name person)

    assertEqual mempty ["A", "C", "E"] rs