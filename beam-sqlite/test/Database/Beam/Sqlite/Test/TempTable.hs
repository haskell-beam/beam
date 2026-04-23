{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Sqlite.Test.TempTable (tests) where

import Data.Int (Int32)
import Data.Kind (Type)
import GHC.Exts (Any)

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (execute_)

import Test.Tasty
import Test.Tasty.HUnit

import Database.Beam.Sqlite.Test

tests :: TestTree
tests = testGroup "Temporary table tests"
  [ testStageFilteredRows
  , testDropAndCreate
  ]

-- | A permanent table of items.
data ItemT f = Item
  { itemId    :: C f Int32
  , itemValue :: C f Int32
  } deriving (Generic, Beamable)

deriving instance Show (ItemT Identity)
deriving instance Eq   (ItemT Identity)

instance Table ItemT where
  data PrimaryKey ItemT f = ItemKey (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = ItemKey . itemId

data ItemDb entity = ItemDb
  { dbItems :: entity (TableEntity ItemT)
  } deriving (Generic, Database Sqlite)

itemDb :: DatabaseSettings be ItemDb
itemDb = defaultDbSettings

testStageFilteredRows :: TestTree
testStageFilteredRows = testCase "sanity test" $
  withTestDb $ \conn -> do
    execute_ conn "CREATE TABLE items (id INT PRIMARY KEY, value INT NOT NULL)"
    result <- runBeamSqlite conn $ do
      runInsert $ insert (dbItems itemDb) $ insertValues
        [ Item 1 5, Item 2 10, Item 3 15, Item 4 20, Item 5 25 ]
      scratch <- runCreateTempTable @_ @Any @ItemT
                   defaultTempTableOptions "high_value_items"
      runInsert $ insert scratch $ insertFrom $
        filter_ (\r -> itemValue r >. val_ 15) $
        all_ (dbItems itemDb)
      runSelectReturningList $ select $
        orderBy_ (asc_ . itemId) $ all_ scratch
    assertEqual "high-value items staged" [Item 4 20, Item 5 25] result

-- | 'DropAndCreate' resets the temp table so a second pass populates a
-- clean table, with no leftovers from the first pass.
testDropAndCreate :: TestTree
testDropAndCreate = testCase "drop-and-create" $
  withTestDb $ \conn -> do
    execute_ conn "CREATE TABLE items (id INT PRIMARY KEY, value INT NOT NULL)"
    result <- runBeamSqlite conn $ do
      runInsert $ insert (dbItems itemDb) $ insertValues
        [ Item 1 5, Item 2 10, Item 3 15, Item 4 20, Item 5 25 ]
      -- First pass: stage low-value items.
      scratch <- runCreateTempTable @_ @Any @ItemT
                   defaultTempTableOptions "scratch"
      runInsert $ insert scratch $ insertFrom $
        filter_ (\r -> itemValue r <=. val_ 15) $
        all_ (dbItems itemDb)
      -- Second pass: drop and recreate, then stage high-value items only.
      scratch' <- runCreateTempTable @_ @Any @ItemT
                    (defaultTempTableOptions { tempTableCreateMode = DropAndCreate })
                    "scratch"
      runInsert $ insert scratch' $ insertFrom $
        filter_ (\r -> itemValue r >. val_ 15) $
        all_ (dbItems itemDb)
      runSelectReturningList $ select $
        orderBy_ (asc_ . itemId) $ all_ scratch'
    assertEqual "only high-value items after reset" [Item 4 20, Item 5 25] result
