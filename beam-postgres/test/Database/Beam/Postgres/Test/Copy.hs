{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.Postgres.Test.Copy (tests) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
  ( MonadBeamCopyFrom (..),
    MonadBeamCopyFromStream (..),
    MonadBeamCopyTo (..),
    MonadBeamCopyToStream (..),
    copySelectTo,
    copySelectToStream,
    copyTableFrom,
    copyTableFromStream,
    copyTableTo,
    copyTableToStream,
  )
import Database.Beam.Postgres
import Database.Beam.Postgres.Test (withTestPostgres)
import Database.PostgreSQL.Simple (Connection, execute_)
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

tests :: IO ByteString -> TestTree
tests getConn =
  testGroup
    "COPY statements"
    [ testGroup
        "COPY ... TO / ... FROM round-trip"
        [ testRoundtripCsvAllColumns getConn,
          testRoundtripCsvMultiColumnProjection getConn,
          testRoundtripCsvSingleColumnProjection getConn,
          testRoundtripCsvFromSelect getConn
        ],
      testGroup
        "options"
        [ testCustomDelimiter getConn,
          testNoHeader getConn,
          testTextFormat getConn
        ],
      testGroup
        "streaming COPY"
        [ testStreamRoundtripCsv getConn,
          testStreamFromSelect getConn
        ],
      testGroup
        "property-based round-trip"
        [ testPropCsvRoundtrip getConn,
          testPropTextRoundtrip getConn
        ]
    ]

-- | The full table is exported to a CSV, the table is truncated, and the CSV
-- is re-imported. The final rows must match the original ones.
testRoundtripCsvAllColumns :: IO ByteString -> TestTree
testRoundtripCsvAllColumns getConn =
  testCopy getConn "copy_all_columns_csv" "/tmp/copy_all_columns.csv" $ \conn path -> do
    seedWidgets conn widgetData
    runBeamPostgres conn $ do
      runCopyTo $ copyTableTo (_dbWidgets testDb) id (copyToCSV path)
    truncateWidgets conn
    runBeamPostgres conn $ do
      runCopyFrom $ copyTableFrom (_dbWidgets testDb) id (copyFromCSV path)
    rows <- queryAllWidgets conn
    assertEqual "round-tripped rows match seed" widgetData rows

-- | Project a subset of columns; the unprojected column gets its DEFAULT on
-- import.
testRoundtripCsvMultiColumnProjection :: IO ByteString -> TestTree
testRoundtripCsvMultiColumnProjection getConn =
  testCopy getConn "copy_multi_column_csv" "/tmp/copy_multi_column.csv" $ \conn path -> do
    seedWidgets conn widgetData
    runBeamPostgres conn $ do
      runCopyTo $
        copyTableTo
          (_dbWidgets testDb)
          (\w -> (_widgetId w, _widgetName w))
          (copyToCSV path)
    truncateWidgets conn
    runBeamPostgres conn $ do
      runCopyFrom $
        copyTableFrom
          (_dbWidgets testDb)
          (\w -> (_widgetId w, _widgetName w))
          (copyFromCSV path)
    rows <- queryAllWidgets conn
    -- 'price' was not in the projection, so it gets its DEFAULT (0).
    let expected = [w {_widgetPrice = 0} | w <- widgetData]
    assertEqual "id+name round-tripped, price defaulted" expected rows

-- | Single-column projection.
testRoundtripCsvSingleColumnProjection :: IO ByteString -> TestTree
testRoundtripCsvSingleColumnProjection getConn =
  testCopy getConn "copy_single_column_csv" "/tmp/copy_single_column.csv" $ \conn path -> do
    seedWidgets conn widgetData
    runBeamPostgres conn $ do
      runCopyTo $ copyTableTo (_dbWidgets testDb) _widgetId (copyToCSV path)
    truncateWidgets conn
    runBeamPostgres conn $ do
      runCopyFrom $ copyTableFrom (_dbWidgets testDb) _widgetId (copyFromCSV path)
    rows <- queryAllWidgets conn
    let expected = [w {_widgetName = "", _widgetPrice = 0} | w <- widgetData]
    assertEqual "id-only round-tripped, name+price defaulted" expected rows

-- | Use copySelectTo with a filtered query, then COPY FROM the result back
-- into the table.
testRoundtripCsvFromSelect :: IO ByteString -> TestTree
testRoundtripCsvFromSelect getConn =
  testCopy getConn "copy_select_csv" "/tmp/copy_select.csv" $ \conn path -> do
    seedWidgets conn widgetData
    runBeamPostgres conn $ do
      runCopyTo $
        copySelectTo
          ( select $ do
              w <- all_ (_dbWidgets testDb)
              guard_ (_widgetPrice w >. val_ 4.0)
              pure w
          )
          (copyToCSV path)
    truncateWidgets conn
    runBeamPostgres conn $ do
      runCopyFrom $ copyTableFrom (_dbWidgets testDb) id (copyFromCSV path)
    rows <- queryAllWidgets conn
    -- Only Widget (9.99) and Sprocket (4.5) have price > 4.0; Cog is excluded.
    let expected = sort [w | w <- widgetData, _widgetPrice w > 4.0]
    assertEqual "select-filtered round-trip" expected rows

-- | Use a custom delimiter on both sides of the round-trip.
testCustomDelimiter :: IO ByteString -> TestTree
testCustomDelimiter getConn =
  testCopy getConn "copy_custom_delim_csv" "/tmp/copy_custom_delim.csv" $ \conn path -> do
    seedWidgets conn widgetData
    let toOpts = copyToCSVWith path defaultPgCSVCopyToOptions {pgCsvCopyToDelimiter = Just '|'}
        fromOpts = copyFromCSVWith path defaultPgCSVCopyFromOptions {pgCsvCopyFromDelimiter = Just '|'}
    runBeamPostgres conn $ do
      runCopyTo $ copyTableTo (_dbWidgets testDb) id toOpts
    truncateWidgets conn
    runBeamPostgres conn $ do
      runCopyFrom $ copyTableFrom (_dbWidgets testDb) id fromOpts
    rows <- queryAllWidgets conn
    assertEqual "round-trip with '|' delimiter" widgetData rows

-- | Toggle HEADER off on TO and on on FROM; round-trip should still work
-- (as long as both sides agree).
testNoHeader :: IO ByteString -> TestTree
testNoHeader getConn =
  testCopy getConn "copy_no_header_csv" "/tmp/copy_no_header.csv" $ \conn path -> do
    seedWidgets conn widgetData
    let toOpts = copyToCSVWith path defaultPgCSVCopyToOptions {pgCsvCopyToHeader = Just False}
        fromOpts = copyFromCSVWith path defaultPgCSVCopyFromOptions {pgCsvCopyFromHeader = Just False}
    runBeamPostgres conn $ do
      runCopyTo $ copyTableTo (_dbWidgets testDb) id toOpts
    truncateWidgets conn
    runBeamPostgres conn $ do
      runCopyFrom $ copyTableFrom (_dbWidgets testDb) id fromOpts
    rows <- queryAllWidgets conn
    assertEqual "round-trip without header line" widgetData rows

-- | The 'text' format with custom delimiter, round-tripped.
testTextFormat :: IO ByteString -> TestTree
testTextFormat getConn =
  testCopy getConn "copy_text_format" "/tmp/copy_text.txt" $ \conn path -> do
    seedWidgets conn widgetData
    let toOpts = copyToTextWith path defaultPgTextCopyToOptions {pgTextCopyToDelimiter = Just '\t'}
        fromOpts = copyFromTextWith path defaultPgTextCopyFromOptions {pgTextCopyFromDelimiter = Just '\t'}
    runBeamPostgres conn $ do
      runCopyTo $ copyTableTo (_dbWidgets testDb) id toOpts
    truncateWidgets conn
    runBeamPostgres conn $ do
      runCopyFrom $ copyTableFrom (_dbWidgets testDb) id fromOpts
    rows <- queryAllWidgets conn
    assertEqual "round-trip via text format" widgetData rows

-- | Round-trip widgets through streaming @COPY ... TO STDOUT@ then
-- @COPY ... FROM STDIN@. The bytes pass through the client connection only;
-- no server-side file is touched.
testStreamRoundtripCsv :: IO ByteString -> TestTree
testStreamRoundtripCsv getConn =
  testCopy getConn "stream_roundtrip_csv" "" $ \conn _ -> do
    seedWidgets conn widgetData
    -- Drain the stream into an IORef.
    chunksRef <- newIORef []
    runBeamPostgres conn $
      runCopyToStream
        (copyTableToStream (_dbWidgets testDb) id copyToCSVStream)
        (\chunk -> modifyIORef' chunksRef (chunk :))
    payload <- BS.concat . reverse <$> readIORef chunksRef
    assertBool "stream produced non-empty payload" (not (BS.null payload))
    -- Replay the captured bytes back into the table.
    truncateWidgets conn
    sourceRef <- newIORef (Just payload)
    runBeamPostgres conn $
      runCopyFromStream
        (copyTableFromStream (_dbWidgets testDb) id copyFromCSVStream)
        ( do
            mchunk <- readIORef sourceRef
            writeIORef sourceRef Nothing
            pure mchunk
        )
    rows <- queryAllWidgets conn
    assertEqual "round-tripped rows match seed" widgetData rows

-- | @COPY (SELECT ...) TO STDOUT@ via 'copySelectToStream'.
testStreamFromSelect :: IO ByteString -> TestTree
testStreamFromSelect getConn =
  testCopy getConn "stream_from_select" "" $ \conn _ -> do
    seedWidgets conn widgetData
    chunksRef <- newIORef []
    runBeamPostgres conn $
      runCopyToStream
        ( copySelectToStream
            ( select $ do
                w <- all_ (_dbWidgets testDb)
                guard_ (_widgetPrice w >. val_ 4.0)
                pure w
            )
            copyToCSVStream
        )
        (\chunk -> modifyIORef' chunksRef (chunk :))
    payload <- BS.concat . reverse <$> readIORef chunksRef
    -- The payload should mention the two widgets with price > 4.0 and not
    -- the one priced 1.25.
    assertBool "Widget present" ("Widget" `BS.isInfixOf` payload)
    assertBool "Sprocket present" ("Sprocket" `BS.isInfixOf` payload)
    assertBool "Cog absent" (not ("Cog" `BS.isInfixOf` payload))

testPropCsvRoundtrip :: IO ByteString -> TestTree
testPropCsvRoundtrip getConn =
  testCopy getConn "prop_csv_roundtrip" "/tmp/prop_csv.csv" $ \conn path -> do
    passes <-
      Hedgehog.check . Hedgehog.property $ do
        toOpts <- Hedgehog.forAll genCsvToOpts
        let fromOpts = matchedCsvFromOpts toOpts
        rows <-
          liftIO $
            roundtripWidgets conn $ \c -> do
              runBeamPostgres c $
                runCopyTo $
                  copyTableTo (_dbWidgets testDb) id (copyToCSVWith path toOpts)
              truncateWidgets c
              runBeamPostgres c $
                runCopyFrom $
                  copyTableFrom (_dbWidgets testDb) id (copyFromCSVWith path fromOpts)
        rows === widgetData
    assertBool "Hedgehog property failed" passes
  where
    genCsvToOpts = do
      delim <- Gen.maybe genDelimiterChar
      hdr <- Gen.maybe Gen.bool
      quote <- Gen.maybe genQuoteChar
      escape <- Gen.maybe genEscapeChar
      nullStr <- Gen.maybe genNullStr
      pure
        defaultPgCSVCopyToOptions
          { pgCsvCopyToDelimiter = delim,
            pgCsvCopyToHeader = hdr,
            pgCsvCopyToQuote = quote,
            pgCsvCopyToEscape = escape,
            pgCsvCopyToNullStr = nullStr
          }

    matchedCsvFromOpts :: PgCSVCopyToOptions -> PgCSVCopyFromOptions
    matchedCsvFromOpts o =
      defaultPgCSVCopyFromOptions
        { pgCsvCopyFromDelimiter = pgCsvCopyToDelimiter o,
          pgCsvCopyFromHeader = pgCsvCopyToHeader o,
          pgCsvCopyFromQuote = pgCsvCopyToQuote o,
          pgCsvCopyFromEscape = pgCsvCopyToEscape o,
          pgCsvCopyFromNullStr = pgCsvCopyToNullStr o
        }

testPropTextRoundtrip :: IO ByteString -> TestTree
testPropTextRoundtrip getConn =
  testCopy getConn "prop_text_roundtrip" "/tmp/prop_text.txt" $ \conn path -> do
    passes <-
      Hedgehog.check . Hedgehog.property $ do
        toOpts <- Hedgehog.forAll genTextToOpts
        let fromOpts = matchedTextFromOpts toOpts
        rows <-
          liftIO $
            roundtripWidgets conn $ \c -> do
              runBeamPostgres c $
                runCopyTo $
                  copyTableTo (_dbWidgets testDb) id (copyToTextWith path toOpts)
              truncateWidgets c
              runBeamPostgres c $
                runCopyFrom $
                  copyTableFrom (_dbWidgets testDb) id (copyFromTextWith path fromOpts)
        rows === widgetData
    assertBool "Hedgehog property failed" passes
  where
    genTextToOpts = do
      delim <- Gen.maybe genDelimiterChar
      hdr <- Gen.maybe Gen.bool
      -- Avoid generating a NULL string that contains the chosen delimiter.
      nullStr <- Gen.maybe (Gen.filter (not . T.any (== ',')) genNullStr)
      pure
        defaultPgTextCopyToOptions
          { pgTextCopyToDelimiter = delim,
            pgTextCopyToHeader = hdr,
            pgTextCopyToNullStr = nullStr
          }

    -- \| Mirror of 'matchedCsvFromOpts' for the @text@ format. 'pgTextCopyFromFreeze'
    -- is intentionally left at @Nothing@: PostgreSQL only accepts @FREEZE@ when
    -- the target table has not been touched in the current (sub)transaction, but
    -- our property test truncates before the FROM, which violates that
    -- precondition.
    matchedTextFromOpts :: PgTextCopyToOptions -> PgTextCopyFromOptions
    matchedTextFromOpts o =
      defaultPgTextCopyFromOptions
        { pgTextCopyFromDelimiter = pgTextCopyToDelimiter o,
          pgTextCopyFromHeader = pgTextCopyToHeader o,
          pgTextCopyFromNullStr = pgTextCopyToNullStr o
        }

-- Single-byte delimiters that never appear in 'widgetData'. Avoiding @.@
-- matters for the @text@ format because the data contains floating-point
-- prices like @9.99@.
genDelimiterChar :: Hedgehog.Gen Char
genDelimiterChar = Gen.element [',', ';', '|', '\t']

genQuoteChar :: Hedgehog.Gen Char
genQuoteChar = Gen.element ['"', '\'']

genEscapeChar :: Hedgehog.Gen Char
genEscapeChar = Gen.element ['"', '\'', '\\']

genNullStr :: Hedgehog.Gen Text
genNullStr = Gen.element ["NULL", "~~~"]

roundtripWidgets :: Connection -> (Connection -> IO ()) -> IO [Widget]
roundtripWidgets conn action = do
  truncateWidgets conn
  seedWidgets conn widgetData
  action conn
  queryAllWidgets conn

testCopy ::
  IO ByteString ->
  String ->
  -- | Server-side file path inside the Postgres container.
  FilePath ->
  (Connection -> FilePath -> Assertion) ->
  TestTree
testCopy getConn name path action = testCase name $
  withTestPostgres name getConn $ \conn -> do
    createWidgetsTable conn
    action conn path

queryAllWidgets :: Connection -> IO [Widget]
queryAllWidgets conn =
  runBeamPostgres conn $
    runSelectReturningList $
      select $
        orderBy_ (asc_ . _widgetId) $
          all_ (_dbWidgets testDb)

createWidgetsTable :: Connection -> IO ()
createWidgetsTable conn =
  void $
    execute_
      conn
      "CREATE TABLE widgets (\
      \  id INTEGER PRIMARY KEY,\
      \  name TEXT NOT NULL DEFAULT '',\
      \  price DOUBLE PRECISION NOT NULL DEFAULT 0\
      \)"

truncateWidgets :: Connection -> IO ()
truncateWidgets conn = void $ execute_ conn "TRUNCATE TABLE widgets"

seedWidgets :: Connection -> [Widget] -> IO ()
seedWidgets conn widgets =
  runBeamPostgres conn $
    runInsert $
      insert (_dbWidgets testDb) (insertValues widgets)

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

testDb :: DatabaseSettings Postgres TestDB
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
