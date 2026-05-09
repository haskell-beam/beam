{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.DuckDB.Test.Extensions.Returning (tests) where

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
    Table (..),
    TableEntity,
    all_,
    asc_,
    current_,
    dbModification,
    defaultDbSettings,
    delete,
    insert,
    insertValues,
    modifyTableFields,
    orderBy_,
    runSelectReturningList,
    save,
    select,
    tableModification,
    update,
    val_,
    withDbModification,
    (<-.),
    (==.),
  )
import Database.Beam.DuckDB
  ( DuckDB,
    runBeamDuckDB,
    runDeleteReturningList,
    runInsertReturningList,
    runUpdateReturningList,
  )
import Database.DuckDB.Simple (Connection, execute_, withConnection)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "RETURNING"
    [ testGroup
        "INSERT ... RETURNING"
        [ testInsertReturningAll,
          testInsertReturningEmpty
        ],
      testGroup
        "UPDATE ... RETURNING"
        [ testUpdateReturningAll,
          testUpdateReturningOnlyMatched
        ],
      testGroup
        "DELETE ... RETURNING"
        [ testDeleteReturningAll,
          testDeleteReturningOnlyMatched
        ]
    ]

testInsertReturningAll :: TestTree
testInsertReturningAll =
  testCase "INSERT returns the inserted rows" $
    withTestDb [] $ \conn -> do
      returned <-
        runBeamDuckDB conn $
          runInsertReturningList $
            insert (_dbWidgets testDb) (insertValues widgetData)
      List.sort returned @?= List.sort widgetData

testInsertReturningEmpty :: TestTree
testInsertReturningEmpty =
  testCase "INSERT with no rows returns []" $
    withTestDb widgetData $ \conn -> do
      returned <-
        runBeamDuckDB conn $
          runInsertReturningList $
            insert (_dbWidgets testDb) (insertValues [])
      returned @?= []
      remaining <- queryAllWidgets conn
      remaining @?= List.sortOn _widgetId widgetData

testUpdateReturningAll :: TestTree
testUpdateReturningAll =
  testCase "UPDATE returns the post-update rows" $
    withTestDb widgetData $ \conn -> do
      returned <-
        runBeamDuckDB conn $
          runUpdateReturningList $
            update
              (_dbWidgets testDb)
              (\w -> _widgetPrice w <-. current_ (_widgetPrice w) + val_ 1.0)
              (\_ -> val_ True)
      List.sort returned
        @?= List.sort
          [ Widget 1 "Widget" 10.99,
            Widget 2 "Sprocket" 5.50,
            Widget 3 "Cog" 2.25
          ]

testUpdateReturningOnlyMatched :: TestTree
testUpdateReturningOnlyMatched =
  testCase "UPDATE with WHERE returns only the matched rows" $
    withTestDb widgetData $ \conn -> do
      returned <-
        runBeamDuckDB conn $
          runUpdateReturningList $
            save (_dbWidgets testDb) (Widget 2 "Sprocket-v2" 4.50)
      returned @?= [Widget 2 "Sprocket-v2" 4.50]
      remaining <- queryAllWidgets conn
      remaining
        @?= [ Widget 1 "Widget" 9.99,
              Widget 2 "Sprocket-v2" 4.50,
              Widget 3 "Cog" 1.25
            ]

testDeleteReturningAll :: TestTree
testDeleteReturningAll =
  testCase "DELETE returns the deleted rows" $
    withTestDb widgetData $ \conn -> do
      returned <-
        runBeamDuckDB conn $
          runDeleteReturningList $
            delete (_dbWidgets testDb) (\_ -> val_ True)
      List.sort returned @?= List.sort widgetData
      remaining <- queryAllWidgets conn
      remaining @?= []

testDeleteReturningOnlyMatched :: TestTree
testDeleteReturningOnlyMatched =
  testCase "DELETE with WHERE returns only the matched rows" $
    withTestDb widgetData $ \conn -> do
      returned <-
        runBeamDuckDB conn $
          runDeleteReturningList $
            delete (_dbWidgets testDb) (\w -> _widgetId w ==. val_ 2)
      returned @?= [Widget 2 "Sprocket" 4.50]
      remaining <- queryAllWidgets conn
      remaining
        @?= [ Widget 1 "Widget" 9.99,
              Widget 3 "Cog" 1.25
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
