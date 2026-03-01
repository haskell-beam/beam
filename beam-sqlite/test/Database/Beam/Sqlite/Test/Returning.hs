{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Sqlite.Test.Returning (tests) where

import Data.Int (Int32)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
  (conflictingFields, onConflictUpdateSet, onConflictUpdateSetWhere)
import Database.Beam.Migrate (defaultMigratableDbSettings)
import Database.Beam.Migrate.Simple (CheckedDatabaseSettings, autoMigrate)
import Database.Beam.Sqlite (Sqlite, runBeamSqlite, insertOnConflictReturning)
import Database.Beam.Sqlite.Migrate (migrationBackend)

import Database.Beam.Sqlite
    ( deleteReturning
    , insertReturning
    , runDeleteReturningList
    , runInsertReturningList
    , runUpdateReturningList
    , updateReturning
    )

import Database.Beam.Sqlite.Test (withTestDb)


tests :: TestTree
tests =
    testGroup
        "SQLite RETURNING statement tests"
        [ testInsertOnConflictReturning
        , testUpdateReturning
        , testDeleteReturning
        ]

-- | Database Schema Definition
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
    { userId    :: !( Columnar f Int32 )
    , userName  :: !( Columnar f Text  )
    , userInfo1 :: !( Columnar f Text  )
    , userInfo2 :: !( Columnar f Int32 )
    }
    deriving stock (Generic)
deriving stock instance (Eq (Columnar f Int32), Eq (Columnar f Text)) => Eq (User f)
deriving stock instance (Show (Columnar f Int32), Show (Columnar f Text)) => Show (User f)

instance Table User where
    newtype PrimaryKey User f = UserId (Columnar f Int32)
        deriving stock (Generic)
    primaryKey = UserId . userId

deriving anyclass instance Beamable User
deriving anyclass instance Beamable (PrimaryKey User)

-- | Test @INSERT .. ON CONFLICT .. RETURNING@
testInsertOnConflictReturning :: TestTree
testInsertOnConflictReturning = testCase "INSERT .. ON CONFLICT .. RETURNING" $
  withTestDb $ \conn -> do
    results <-
      runBeamSqlite conn $ do
        autoMigrate migrationBackend checkedDb

        -- Seed data
        runInsert $
          insert
            (usersTable testDb)
            $ insertValues
              [ User {userId = 1, userName = "initial_user1", userInfo1 = "user1Info", userInfo2 = 11 }
              , User {userId = 3, userName = "initial_user3", userInfo1 = "user3Info", userInfo2 = 33 }
              , User {userId = 4, userName = "initial_user4", userInfo1 = "user4Info", userInfo2 = 44 }
              ]

        let
          newUsers =
            [ User {userId = 1, userName = "upserted_user1", userInfo1 = "user1Info2", userInfo2 = 111 }
            , User {userId = 2, userName = "new_user2"     , userInfo1 = "info1"     , userInfo2 = 222 }
            , User {userId = 3, userName = "upserted_user3", userInfo1 = "info1b"    , userInfo2 = 333 }
            , User {userId = 4, userName = "not_upserted_4", userInfo1 = "ignore"    , userInfo2 = 444 }
            ]

        -- Run 'INSERT .. ON CONFLICT (id) DO UPDATE SET .. WHERE .. RETURNING ..'
        runInsertReturningList $
          insertOnConflictReturning
            (usersTable testDb)
            (insertValues newUsers)
            (conflictingFields userId)
            (onConflictUpdateSetWhere
              ( \(User{userName = fld}) (User{userName = excl}) ->
                  fld <-. excl
              )
              ( \(User{userName = fld}) (User{userName = excl, userInfo1 = exclInfo1 }) ->
                  (current_ fld /=. excl) &&. (exclInfo1 /=. "ignore")
              )
            )
            (\u -> (userId u, userName u, userInfo2 u))

    results @?=
      [ (1, "upserted_user1",  11)
      , (2, "new_user2"     , 222)
      , (3, "upserted_user3",  33)
      ]

-- | Test @UPDATE ... RETURNING@
testUpdateReturning :: TestTree
testUpdateReturning = testCase "UPDATE .. RETURNING" $
  withTestDb $ \conn -> do
    updatedUsers <-
      runBeamSqlite conn $ do
        autoMigrate migrationBackend checkedDb

        -- Seed data
        runInsert $
          insert (usersTable testDb) $
            insertValues
              [ User {userId = 1, userName = "user1", userInfo1 = "user1Info1", userInfo2 = 11 }
              , User {userId = 2, userName = "user2", userInfo1 = "user2Info1", userInfo2 = 22 }
              , User {userId = 3, userName = "user3", userInfo1 = "user3Info1", userInfo2 = 33 }
              ]

        -- Update user 2 and return projected columns from the updated row
        runUpdateReturningList $
          updateReturning
            (usersTable testDb)
            (\u -> userName u <-. val_ "updated_user2")
            (\u -> userId u ==. val_ 2)
            (\u -> (userId u, userName u, userInfo2 u))

    updatedUsers @?= [(2, "updated_user2", 22)]

-- | Test @DELETE .. RETURNING@
testDeleteReturning :: TestTree
testDeleteReturning = testCase "DELETE .. RETURNING" $
  withTestDb $ \conn -> do
    deletedUsers <-
      runBeamSqlite conn $ do
        autoMigrate migrationBackend checkedDb

        -- Seed data
        runInsert $
          insert (usersTable testDb) $
            insertValues
              [ User { userId = 1, userName = "user1", userInfo1 = "user1Info1", userInfo2 = 11 }
              , User { userId = 2, userName = "user2", userInfo1 = "user2Info1", userInfo2 = 22 }
              , User { userId = 3, userName = "user3", userInfo1 = "user3Info1", userInfo2 = 33 }
              ]

        -- Delete user 3 and return projected columns from the deleted row
        runDeleteReturningList $
          deleteReturning
            (usersTable testDb)
            (\u -> userId u ==. val_ 3)
            (\u -> (userName u, userInfo2 u))

    deletedUsers @?= [("user3", 33)]
