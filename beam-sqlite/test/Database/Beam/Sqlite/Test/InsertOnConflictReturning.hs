{-# LANGUAGE DerivingStrategies #-}

{-
    This module is based on the bug reproducer from issue #773
    https://github.com/haskell-beam/beam/issues/773
-}
module Database.Beam.Sqlite.Test.InsertOnConflictReturning (tests) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (
    Beamable,
    Columnar,
    Database,
    DatabaseSettings,
    Generic,
    Identity,
    SqlEq ((/=.)),
    Table (..),
    TableEntity,
    current_,
    defaultDbSettings,
    insert,
    insertValues,
    runInsert,
    (<-.),
 )
import Database.Beam.Backend.SQL.BeamExtensions (
    BeamHasInsertOnConflict (
        conflictingFields,
        insertOnConflict,
        onConflictUpdateSetWhere
    ),
    MonadBeamInsertReturning (runInsertReturningList),
 )
import Database.Beam.Sqlite (Sqlite, runBeamSqlite)
import Database.Beam.Sqlite.Test (withTestDb)
import Database.SQLite.Simple (execute_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (
    conflictingFields,
    insertOnConflict,
    onConflictUpdateSetWhere,
    runInsertReturningList,
 )
import Database.Beam.Migrate (defaultMigratableDbSettings)
import Database.Beam.Migrate.Simple (CheckedDatabaseSettings, autoMigrate)
import Database.Beam.Sqlite (Sqlite, runBeamSqliteDebug)
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Database.SQLite.Simple (open)

tests :: TestTree
tests =
    testGroup
        "Insertion on conflict returning tests"
        [testInsertOnConflictReturning]

data TestDb f
    = TestDb
    { usersTable :: f (TableEntity User)
    }
    deriving stock (Generic)

deriving anyclass instance Database be TestDb

testDb :: DatabaseSettings be TestDb
testDb = defaultDbSettings

checkedDb :: CheckedDatabaseSettings Sqlite TestDb
checkedDb = defaultMigratableDbSettings

data User f
    = User
    { userId :: Columnar f Int32
    , userName :: Columnar f Text
    }
    deriving stock (Generic)

deriving stock instance Show (PrimaryKey User Identity)
deriving stock instance Show (User Identity)

instance Table User where
    newtype PrimaryKey User f = UserId (Columnar f Int32)
        deriving stock (Generic)
    primaryKey = UserId . userId

deriving anyclass instance Beamable User
deriving anyclass instance Beamable (PrimaryKey User)

testInsertOnConflictReturning :: TestTree
testInsertOnConflictReturning = testCase "Check that conflicting values are returned by `runInsertReturningList`" $
    withTestDb $ \conn -> do
        conflicts <-
            runBeamSqlite conn $ do
                autoMigrate migrationBackend checkedDb

                runInsert $
                    insert
                        (usersTable testDb)
                        ( insertValues
                            [ User{userId = 0, userName = "user0"}
                            , User{userId = 2, userName = "user2"}
                            , User{userId = 5, userName = "user5"}
                            ]
                        )

                let newUsers =
                        [ User{userId = 1, userName = "user1"}
                        , User{userId = 2, userName = "different_user2"}
                        ]

                runInsertReturningList $
                    insertOnConflict
                        (usersTable testDb)
                        (insertValues newUsers)
                        (conflictingFields userId)
                        ( onConflictUpdateSetWhere
                            ( \(User{userName = fld})
                               (User{userName = excl}) ->
                                    fld <-. excl
                            )
                            ( \(User{userName = fld})
                               (User{userName = excl}) ->
                                    current_ fld /=. excl
                            )
                        )

        -- Expecting that the conflicting user, User id 2, is also returned
        userId <$> conflicts @?= [1, 2]
