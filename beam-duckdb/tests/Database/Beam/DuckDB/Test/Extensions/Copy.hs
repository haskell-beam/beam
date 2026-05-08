{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.DuckDB.Test.Extensions.Copy (tests) where

import Control.Monad (guard, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Char as Char
import Data.Int (Int32)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
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
    dbModification,
    defaultDbSettings,
    guard_,
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
    (>.),
  )
import Database.Beam.Backend.SQL.BeamExtensions
  ( copySelectTo,
    copyTableFrom,
    copyTableTo,
    runCopyFrom,
    runCopyTo,
  )
import Database.Beam.DuckDB
  ( DuckDB,
    DuckDBCSVCopyFromOptions (..),
    DuckDBCSVCopyToOptions (..),
    DuckDBJSONCopyFromOptions (..),
    DuckDBJSONCopyToOptions (..),
    DuckDBParquetCopyToOptions (..),
    JSONCompression (..),
    ParquetCompression (..),
    copyFromCSV,
    copyFromCSVWith,
    copyFromJSONWith,
    copyFromParquet,
    copyFromParquetWith,
    copyToCSV,
    copyToCSVWith,
    copyToJSONWith,
    copyToParquetWith,
    defaultDuckDBCSVCopyFromOptions,
    defaultDuckDBCSVCopyToOptions,
    defaultDuckDBJSONCopyFromOptions,
    defaultDuckDBJSONCopyToOptions,
    defaultDuckDBParquetCopyFromOptions,
    defaultDuckDBParquetCopyToOptions,
    runBeamDuckDB,
  )
import Database.DuckDB.Simple (Connection, execute_, withConnection)
import Hedgehog (Gen, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "COPY statements"
    [ testGroup
        "COPY ... TO"
        [ testCopyAllColumnsToCsv,
          testCopyMultiColumnProjectionToCsv,
          testCopySingleColumnProjectionToCsv,
          testCopySelectToCsv
        ],
      testGroup
        "COPY ... FROM"
        [ testCopyFromCsvAllColumns,
          testCopyFromCsvMultiColumnProjection,
          testCopyFromCsvSingleColumnProjection
        ],
      testGroup
        "options"
        [ testCopyToWithCustomDelimiterAndNoHeader,
          testCopyFromWithCustomDelimiter,
          testRoundTripParquetWithCompression
        ],
      testGroup
        "property-based round-trip"
        [ propRoundTripCsv,
          propRoundTripParquet,
          propRoundTripJson
        ]
    ]

testCopyAllColumnsToCsv :: TestTree
testCopyAllColumnsToCsv = testCase "Copy all columns of a table to a CSV file" $
  withCopyTarget $ \conn path -> do
    runBeamDuckDB conn $
      runCopyTo $
        copyTableTo (_dbWidgets testDb) id (copyToCSV path)

    (header, rows) <- readCsv path
    header @?= "id,name,price"
    rows
      @?= List.sort
        [ "1,Widget,9.99",
          "2,Sprocket,4.5",
          "3,Cog,1.25"
        ]

testCopyMultiColumnProjectionToCsv :: TestTree
testCopyMultiColumnProjectionToCsv =
  testCase "Copy a multi-column projection of a table to a CSV file" $
    withCopyTarget $ \conn path -> do
      runBeamDuckDB conn $
        runCopyTo $
          copyTableTo
            (_dbWidgets testDb)
            (\w -> (_widgetId w, _widgetName w))
            (copyToCSV path)

      (header, rows) <- readCsv path
      header @?= "id,name"
      rows
        @?= List.sort
          [ "1,Widget",
            "2,Sprocket",
            "3,Cog"
          ]

testCopySingleColumnProjectionToCsv :: TestTree
testCopySingleColumnProjectionToCsv =
  testCase "Copy a single column of a table to a CSV file" $
    withCopyTarget $ \conn path -> do
      runBeamDuckDB conn $
        runCopyTo $
          copyTableTo (_dbWidgets testDb) _widgetName (copyToCSV path)

      (header, rows) <- readCsv path
      header @?= "name"
      rows @?= List.sort ["Widget", "Sprocket", "Cog"]

testCopySelectToCsv :: TestTree
testCopySelectToCsv =
  testCase "Copy the result of a SELECT query to a CSV file" $
    withCopyTarget $ \conn path -> do
      runBeamDuckDB conn $
        runCopyTo $
          copySelectTo
            ( select $ do
                w <- all_ (_dbWidgets testDb)
                guard_ (_widgetPrice w >. val_ 4.0)
                pure (_widgetName w)
            )
            (copyToCSV path)

      (_header, rows) <- readCsv path
      -- Only Widget (9.99) and Sprocket (4.5) have price > 4.0; Cog (1.25) is excluded.
      rows @?= List.sort ["Widget", "Sprocket"]

testCopyFromCsvAllColumns :: TestTree
testCopyFromCsvAllColumns =
  testCase "Copy all columns of a table from a CSV file" $
    withCopyFromSource csvAllColumns $ \conn path -> do
      runBeamDuckDB conn $
        runCopyFrom $
          copyTableFrom (_dbWidgets testDb) id (copyFromCSV path)

      rows <- queryAllWidgets conn
      rows
        @?= [ Widget 1 "Widget" 9.99,
              Widget 2 "Sprocket" 4.5,
              Widget 3 "Cog" 1.25
            ]
  where
    csvAllColumns =
      "id,name,price\n\
      \1,Widget,9.99\n\
      \2,Sprocket,4.5\n\
      \3,Cog,1.25\n"

testCopyFromCsvMultiColumnProjection :: TestTree
testCopyFromCsvMultiColumnProjection =
  testCase "Copy a multi-column projection of a table from a CSV file" $
    withCopyFromSource csvIdName $ \conn path -> do
      runBeamDuckDB conn $
        runCopyFrom $
          copyTableFrom
            (_dbWidgets testDb)
            (\w -> (_widgetId w, _widgetName w))
            (copyFromCSV path)

      rows <- queryAllWidgets conn
      -- 'price' was not in the CSV, so it gets the table's DEFAULT value.
      rows
        @?= [ Widget 1 "Widget" 0,
              Widget 2 "Sprocket" 0,
              Widget 3 "Cog" 0
            ]
  where
    csvIdName =
      "id,name\n\
      \1,Widget\n\
      \2,Sprocket\n\
      \3,Cog\n"

testCopyFromCsvSingleColumnProjection :: TestTree
testCopyFromCsvSingleColumnProjection =
  testCase "Copy a single column of a table from a CSV file" $
    withCopyFromSource csvIdOnly $ \conn path -> do
      runBeamDuckDB conn $
        runCopyFrom $
          copyTableFrom (_dbWidgets testDb) _widgetId (copyFromCSV path)

      rows <- queryAllWidgets conn
      -- Only 'id' was in the CSV, so 'name' and 'price' get table DEFAULTs.
      rows
        @?= [ Widget 1 "" 0,
              Widget 2 "" 0,
              Widget 3 "" 0
            ]
  where
    csvIdOnly =
      "id\n\
      \1\n\
      \2\n\
      \3\n"

testCopyToWithCustomDelimiterAndNoHeader :: TestTree
testCopyToWithCustomDelimiterAndNoHeader =
  testCase "Copy a table to a CSV file with custom delimiter and no header" $
    withCopyTarget $ \conn path -> do
      let opts =
            copyToCSVWith
              path
              defaultDuckDBCSVCopyToOptions
                { csvCopyToHeader = Just False,
                  csvCopyToDelimiter = Just "|"
                }
      runBeamDuckDB conn $
        runCopyTo $
          copyTableTo (_dbWidgets testDb) id opts

      contents <- Text.IO.readFile path
      let dataLines = List.sort (filter (not . Text.null) (Text.lines contents))
      dataLines
        @?= List.sort
          [ "1|Widget|9.99",
            "2|Sprocket|4.5",
            "3|Cog|1.25"
          ]

testCopyFromWithCustomDelimiter :: TestTree
testCopyFromWithCustomDelimiter =
  testCase "Copy from a CSV file with a custom delimiter" $
    withCopyFromSource pipeDelimited $ \conn path -> do
      let opts =
            copyFromCSVWith
              path
              defaultDuckDBCSVCopyFromOptions
                { csvCopyFromDelimiter = Just "|"
                }
      runBeamDuckDB conn $
        runCopyFrom $
          copyTableFrom (_dbWidgets testDb) id opts

      rows <- queryAllWidgets conn
      rows
        @?= [ Widget 1 "Widget" 9.99,
              Widget 2 "Sprocket" 4.5,
              Widget 3 "Cog" 1.25
            ]
  where
    pipeDelimited =
      "id|name|price\n\
      \1|Widget|9.99\n\
      \2|Sprocket|4.5\n\
      \3|Cog|1.25\n"

testRoundTripParquetWithCompression :: TestTree
testRoundTripParquetWithCompression =
  testCase "Round-trip data through a ZSTD-compressed Parquet file" $
    withTestDb widgetData $ \conn ->
      withSystemTempFile "beam-duckdb-copy-.parquet" $ \path h -> do
        hClose h
        let toOpts =
              copyToParquetWith
                path
                defaultDuckDBParquetCopyToOptions
                  { parquetCopyToCompression = Just ParquetZstd
                  }
            fromOpts = copyFromParquet path

        runBeamDuckDB conn $
          runCopyTo $
            copyTableTo (_dbWidgets testDb) id toOpts

        -- Wipe the seeded rows, then re-import from the Parquet file we wrote.
        void $ execute_ conn "DELETE FROM widgets"

        runBeamDuckDB conn $
          runCopyFrom $
            copyTableFrom (_dbWidgets testDb) id fromOpts

        rows <- queryAllWidgets conn
        rows
          @?= [ Widget 1 "Widget" 9.99,
                Widget 2 "Sprocket" 4.5,
                Widget 3 "Cog" 1.25
              ]

propRoundTripCsv :: TestTree
propRoundTripCsv =
  testProperty "CSV options round-trip" $
    property $ do
      toOpts <- forAll genCsvToOpts
      let fromOpts = matchedCsvFromOpts toOpts
      rows <- liftIO $ csvRoundTrip toOpts fromOpts
      rows === widgetData
  where
    csvRoundTrip toOpts fromOpts =
      withTestDb widgetData $ \conn ->
        withSystemTempFile "beam-duckdb-prop-csv-.csv" $ \path h -> do
          hClose h
          runBeamDuckDB conn $
            runCopyTo $
              copyTableTo (_dbWidgets testDb) id (copyToCSVWith path toOpts)
          void $ execute_ conn "DELETE FROM widgets"
          runBeamDuckDB conn $
            runCopyFrom $
              copyTableFrom (_dbWidgets testDb) id (copyFromCSVWith path fromOpts)
          queryAllWidgets conn

    genCsvToOpts :: Hedgehog.Gen DuckDBCSVCopyToOptions
    genCsvToOpts = do
      hdr <- Gen.maybe Gen.bool
      delim <- Gen.maybe (Gen.element [",", ";", "|", "\t"])
      nullStr <- Gen.maybe (Gen.element ["NULL", "~~~"])
      quote <- Gen.maybe genCsvSpecialChar
      escape <- Gen.maybe genCsvSpecialChar

      -- quote and escape can't overlap with nullStr and delim,
      -- otherwise the CSV data may be unparseable.
      guard (maybe True not $ Text.elem <$> quote <*> delim)
      guard (quote /= Just ',') -- the default delimiter
      guard (maybe True not $ Text.elem <$> quote <*> nullStr)
      guard (maybe True not $ Text.elem <$> escape <*> delim)
      guard (escape /= Just ',') -- the default delimiter
      guard (maybe True not $ Text.elem <$> escape <*> nullStr)

      pure
        defaultDuckDBCSVCopyToOptions
          { csvCopyToHeader = hdr,
            csvCopyToDelimiter = delim,
            csvCopyToQuote = quote,
            csvCopyToEscape = escape,
            csvCopyToNullStr = nullStr,
            -- The TO-side @overwrite_or_ignore@ field is intentionally fixed to
            -- @Just True@: a property that replays through the same temp file 25
            -- times needs the second iteration onwards to overwrite, and the field
            -- itself is independent of the readable contents we round-trip.
            csvCopyToOverwriteOrIgnore = Just True
          }

    matchedCsvFromOpts :: DuckDBCSVCopyToOptions -> DuckDBCSVCopyFromOptions
    matchedCsvFromOpts o =
      defaultDuckDBCSVCopyFromOptions
        { csvCopyFromHeader = csvCopyToHeader o,
          csvCopyFromDelimiter = csvCopyToDelimiter o,
          csvCopyFromQuote = csvCopyToQuote o,
          csvCopyFromEscape = csvCopyToEscape o,
          csvCopyFromNullStr = csvCopyToNullStr o
        }

    -- Printable single-byte ASCII characters that don't collide with
    -- the seeded 'widgetData' (which is alphanumeric plus '.'), and the delimiter
    genCsvSpecialChar :: Hedgehog.Gen Char
    genCsvSpecialChar =
      Gen.element
        [ c
        | c <- enumFromTo minBound maxBound,
          Char.isPrint c,
          Char.isAscii c,
          not (Char.isAlphaNum c),
          c /= '.'
        ]

propRoundTripParquet :: TestTree
propRoundTripParquet =
  testProperty "Parquet options round-trip" $
    property $ do
      toOpts <- forAll genParquetToOpts
      let fromOpts = defaultDuckDBParquetCopyFromOptions
      rows <- liftIO $ parquetRoundTrip toOpts fromOpts
      rows === widgetData
  where
    parquetRoundTrip toOpts fromOpts =
      withTestDb widgetData $ \conn ->
        withSystemTempFile "beam-duckdb-prop-parquet-.parquet" $ \path h -> do
          hClose h
          runBeamDuckDB conn $
            runCopyTo $
              copyTableTo (_dbWidgets testDb) id (copyToParquetWith path toOpts)
          void $ execute_ conn "DELETE FROM widgets"
          runBeamDuckDB conn $
            runCopyFrom $
              copyTableFrom (_dbWidgets testDb) id (copyFromParquetWith path fromOpts)
          queryAllWidgets conn

    genParquetToOpts :: Hedgehog.Gen DuckDBParquetCopyToOptions
    genParquetToOpts = do
      comp <- Gen.maybe Gen.enumBounded
      pure
        defaultDuckDBParquetCopyToOptions
          { parquetCopyToCompression = comp,
            parquetCopyToOverwriteOrIgnore = Just True
          }

propRoundTripJson :: TestTree
propRoundTripJson =
  testProperty "JSON options round-trip" $
    property $ do
      toOpts <- forAll genJsonToOpts
      fromOpts <- forAll (genJsonFromOpts (jsonCopyToCompression toOpts))
      rows <- liftIO $ jsonRoundTrip toOpts fromOpts
      rows === widgetData
  where
    jsonRoundTrip toOpts fromOpts =
      withTestDb widgetData $ \conn ->
        withSystemTempFile "beam-duckdb-prop-json-.json" $ \path h -> do
          hClose h
          runBeamDuckDB conn $
            runCopyTo $
              copyTableTo (_dbWidgets testDb) id (copyToJSONWith path toOpts)
          void $ execute_ conn "DELETE FROM widgets"
          runBeamDuckDB conn $
            runCopyFrom $
              copyTableFrom (_dbWidgets testDb) id (copyFromJSONWith path fromOpts)
          queryAllWidgets conn

    genJsonToOpts :: Hedgehog.Gen DuckDBJSONCopyToOptions
    genJsonToOpts = do
      comp <- Gen.maybe Gen.enumBounded
      pure
        defaultDuckDBJSONCopyToOptions
          { jsonCopyToCompression = comp,
            jsonCopyToOverwriteOrIgnore = Just True
          }

    genJsonFromOpts :: Maybe JSONCompression -> Hedgehog.Gen DuckDBJSONCopyFromOptions
    genJsonFromOpts comp = do
      auto <- Gen.maybe Gen.bool
      pure
        defaultDuckDBJSONCopyFromOptions
          { jsonCopyFromAutoDetect = auto,
            jsonCopyFromCompression = comp
          }

withCopyTarget :: (Connection -> FilePath -> IO a) -> IO a
withCopyTarget action =
  withTestDb widgetData $ \conn ->
    withSystemTempFile "beam-duckdb-copy-.csv" $ \path h -> do
      -- DuckDB's COPY TO opens the file itself; close our handle first
      -- so it isn't held open while DuckDB writes.
      hClose h
      action conn path

withCopyFromSource :: Text -> (Connection -> FilePath -> IO a) -> IO a
withCopyFromSource csvContent action =
  withTestDb [] $ \conn ->
    withSystemTempFile "beam-duckdb-copy-from-.csv" $ \path h -> do
      Text.IO.hPutStr h csvContent
      hClose h
      action conn path

readCsv :: FilePath -> IO (Text, [Text])
readCsv path = do
  contents <- Text.IO.readFile path
  case Text.lines contents of
    [] -> fail ("expected at least a header line in " <> path)
    (h : ds) -> pure (h, List.sort ds)

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

data WidgetT f = Widget
  { _widgetId :: Columnar f Int32,
    _widgetName :: Columnar f Text,
    _widgetPrice :: Columnar f Double
  }
  deriving (Generic)

type Widget = WidgetT Identity

deriving instance Show Widget

deriving instance Eq Widget

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
seedData conn widgets =
  runBeamDuckDB conn $
    runInsert $
      insert (_dbWidgets testDb) $
        insertValues widgets

withTestDb ::
  [Widget] ->
  (Connection -> IO a) ->
  IO a
withTestDb widgets action =
  withConnection ":memory:" $ \conn -> do
    createTables conn
    seedData conn widgets
    action conn
