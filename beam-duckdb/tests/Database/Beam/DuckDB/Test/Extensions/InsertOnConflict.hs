{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.DuckDB.Test.Extensions.InsertOnConflict (tests) where

import Control.Monad (void)
import Data.Int (Int32)
import qualified Data.List as List
import Data.Text (Text)
import Database.Beam
  ( Beamable,
    Columnar,
    Database,
    DatabaseSettings,
    Generic,
    Identity,
    SqlOrd ((>.)),
    Table (..),
    TableEntity,
    all_,
    asc_,
    current_,
    dbModification,
    defaultDbSettings,
    insert,
    insertValues,
    modifyTableFields,
    orderBy_,
    runInsert,
    runSelectReturningList,
    select,
    tableModification,
    val_,
    withDbModification,
    (<-.),
  )
import Database.Beam.DuckDB
  ( BeamHasInsertOnConflict
      ( anyConflict,
        conflictingFields,
        insertOnConflict,
        onConflictDoNothing,
        onConflictUpdateSet,
        onConflictUpdateSetWhere
      ),
    DuckDB,
    onConflictUpdateAll,
    onConflictUpdateInstead,
    runBeamDuckDB,
    runInsertReturningList,
  )
import Database.DuckDB.Simple (Connection, execute_, withConnection)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "INSERT ... ON CONFLICT"
    [ testAnyConflictDoNothing,
      testConflictingFieldsDoNothing,
      testConflictingFieldsUpdateAll,
      testConflictingFieldsUpdateInstead,
      testConflictingFieldsUpdateSet,
      testConflictingFieldsUpdateSetWhere,
      testInsertOnConflictReturning
    ]

testAnyConflictDoNothing :: TestTree
testAnyConflictDoNothing =
  testCase "anyConflict + onConflictDoNothing leaves existing rows untouched" $
    withTestDb widgetData $ \conn -> do
      runBeamDuckDB conn $
        runInsert $
          insertOnConflict
            (_dbWidgets testDb)
            (insertValues [Widget 2 "Sprocket-collision" 0.0])
            anyConflict
            onConflictDoNothing
      remaining <- queryAllWidgets conn
      remaining @?= List.sortOn _widgetId widgetData

testConflictingFieldsDoNothing :: TestTree
testConflictingFieldsDoNothing =
  testCase "conflictingFields + onConflictDoNothing inserts non-conflicting rows" $
    withTestDb widgetData $ \conn -> do
      runBeamDuckDB conn $
        runInsert $
          insertOnConflict
            (_dbWidgets testDb)
            ( insertValues
                [ Widget 2 "Sprocket-collision" 0.0,
                  Widget 4 "Gear" 7.75
                ]
            )
            (conflictingFields _widgetId)
            onConflictDoNothing
      remaining <- queryAllWidgets conn
      remaining
        @?= [ Widget 1 "Widget" 9.99,
              Widget 2 "Sprocket" 4.50,
              Widget 3 "Cog" 1.25,
              Widget 4 "Gear" 7.75
            ]

testConflictingFieldsUpdateAll :: TestTree
testConflictingFieldsUpdateAll =
  testCase "conflictingFields + onConflictUpdateAll overwrites every column" $
    withTestDb widgetData $ \conn -> do
      runBeamDuckDB conn $
        runInsert $
          insertOnConflict
            (_dbWidgets testDb)
            ( insertValues
                [ Widget 2 "Sprocket-v2" 6.00,
                  Widget 4 "Gear" 7.75
                ]
            )
            (conflictingFields _widgetId)
            onConflictUpdateAll
      remaining <- queryAllWidgets conn
      remaining
        @?= [ Widget 1 "Widget" 9.99,
              Widget 2 "Sprocket-v2" 6.00,
              Widget 3 "Cog" 1.25,
              Widget 4 "Gear" 7.75
            ]

testConflictingFieldsUpdateInstead :: TestTree
testConflictingFieldsUpdateInstead =
  testCase "conflictingFields + onConflictUpdateInstead overwrites a subset" $
    withTestDb widgetData $ \conn -> do
      runBeamDuckDB conn $
        runInsert $
          insertOnConflict
            (_dbWidgets testDb)
            (insertValues [Widget 2 "Sprocket-v3" 100.0])
            (conflictingFields _widgetId)
            (onConflictUpdateInstead _widgetName)
      remaining <- queryAllWidgets conn
      remaining
        @?= [ Widget 1 "Widget" 9.99,
              Widget 2 "Sprocket-v3" 4.50,
              Widget 3 "Cog" 1.25
            ]

testConflictingFieldsUpdateSet :: TestTree
testConflictingFieldsUpdateSet =
  testCase "conflictingFields + onConflictUpdateSet applies a custom assignment" $
    withTestDb widgetData $ \conn -> do
      runBeamDuckDB conn $
        runInsert $
          insertOnConflict
            (_dbWidgets testDb)
            (insertValues [Widget 2 "Sprocket-discount" 0.0])
            (conflictingFields _widgetId)
            ( onConflictUpdateSet
                ( \fields _excluded ->
                    _widgetPrice fields <-. current_ (_widgetPrice fields) - val_ 0.50
                )
            )
      remaining <- queryAllWidgets conn
      remaining
        @?= [ Widget 1 "Widget" 9.99,
              Widget 2 "Sprocket" 4.00,
              Widget 3 "Cog" 1.25
            ]

testConflictingFieldsUpdateSetWhere :: TestTree
testConflictingFieldsUpdateSetWhere =
  testCase "conflictingFields + onConflictUpdateSetWhere only updates when predicate holds" $
    withTestDb widgetData $ \conn -> do
      runBeamDuckDB conn $
        runInsert $
          insertOnConflict
            (_dbWidgets testDb)
            ( insertValues
                [ Widget 1 "Widget-replacement" 1.00,
                  Widget 3 "Cog-replacement" 50.00
                ]
            )
            (conflictingFields _widgetId)
            ( onConflictUpdateSetWhere
                ( \fields excludedRow ->
                    _widgetName fields <-. _widgetName excludedRow
                )
                -- Only overwrite when the incoming price is higher than the existing one
                ( \fields excludedRow ->
                    _widgetPrice excludedRow >. current_ (_widgetPrice fields)
                )
            )
      remaining <- queryAllWidgets conn
      remaining
        @?= [ Widget 1 "Widget" 9.99,
              Widget 2 "Sprocket" 4.50,
              Widget 3 "Cog-replacement" 1.25
            ]

testInsertOnConflictReturning :: TestTree
testInsertOnConflictReturning =
  testCase "INSERT ON CONFLICT ... RETURNING returns inserted-or-updated rows" $
    withTestDb widgetData $ \conn -> do
      returned <-
        runBeamDuckDB conn $
          runInsertReturningList $
            insertOnConflict
              (_dbWidgets testDb)
              ( insertValues
                  [ Widget 2 "Sprocket-v2" 6.00,
                    Widget 4 "Gear" 7.75
                  ]
              )
              (conflictingFields _widgetId)
              onConflictUpdateAll
      List.sort returned
        @?= List.sort
          [ Widget 2 "Sprocket-v2" 6.00,
            Widget 4 "Gear" 7.75
          ]

data WidgetT f = Widget
  { _widgetId :: Columnar f Int32,
    _widgetName :: Columnar f Text,
    _widgetPrice :: Columnar f Double
  }
  deriving (Generic)

type Widget = WidgetT Identity

deriving instance Show Widget

deriving instance Eq Widget

deriving instance Ord Widget

instance Beamable WidgetT

instance Table WidgetT where
  data PrimaryKey WidgetT f = WidgetId (Columnar f Int32)
    deriving (Generic)
  primaryKey = WidgetId . _widgetId

instance Beamable (PrimaryKey WidgetT)

newtype TestDB f = TestDB
  { _dbWidgets :: f (TableEntity WidgetT)
  }
  deriving (Generic, Database be)

testDb :: DatabaseSettings DuckDB TestDB
testDb =
  defaultDbSettings
    `withDbModification` dbModification
      { _dbWidgets =
          modifyTableFields
            tableModification
              { _widgetId = "id",
                _widgetName = "name",
                _widgetPrice = "price"
              }
      }

createTables :: Connection -> IO ()
createTables conn =
  void $
    execute_
      conn
      "CREATE TABLE widgets (\
      \  id INTEGER PRIMARY KEY,\
      \  name TEXT NOT NULL DEFAULT '',\
      \  price DOUBLE NOT NULL DEFAULT 0\
      \)"

seedData :: Connection -> [Widget] -> IO ()
seedData _ [] = pure ()
seedData conn widgets =
  void $
    runBeamDuckDB conn $
      runInsertReturningList $
        insert (_dbWidgets testDb) (insertValues widgets)

withTestDb :: [Widget] -> (Connection -> IO a) -> IO a
withTestDb widgets action =
  withConnection ":memory:" $ \conn -> do
    createTables conn
    seedData conn widgets
    action conn

queryAllWidgets :: Connection -> IO [Widget]
queryAllWidgets conn =
  runBeamDuckDB conn $
    runSelectReturningList $
      select $
        orderBy_ (asc_ . _widgetId) $
          all_ (_dbWidgets testDb)

widgetData :: [Widget]
widgetData =
  [ Widget 1 "Widget" 9.99,
    Widget 2 "Sprocket" 4.50,
    Widget 3 "Cog" 1.25
  ]
