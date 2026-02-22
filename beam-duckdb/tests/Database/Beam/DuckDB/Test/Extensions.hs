{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.DuckDB.Test.Extensions (tests) where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Database.Beam
  ( Beamable,
    Columnar,
    Database,
    DatabaseSettings,
    Generic,
    Identity,
    Table (..),
    aggregate_,
    as_,
    countAll_,
    dbModification,
    defaultDbSettings,
    guard_,
    min_,
    runSelectReturningList,
    runSelectReturningOne,
    select,
    tableModification,
    val_,
    withDbModification,
    (==.),
  )
import Database.Beam.DuckDB
  ( CSVFileEntity,
    CSVFileOptions (..),
    DuckDB,
    IcebergTableEntity,
    ParquetFileEntity,
    allFromCSV_,
    allFromIceberg_,
    allFromParquet_,
    allowMovedPaths,
    csvFileWith,
    defaultCSVFileOptions,
    defaultIcebergTableOptions,
    icebergTableWith,
    modifyCSVFileFields,
    modifyIcebergTableFields,
    modifyParquetFileFields,
    multipleParquetFiles,
    parquetFile,
    runBeamDuckDB,
    singleCSVFile,
    singleParquetFile,
  )
import Database.DuckDB.Simple (Connection, withConnection)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Extensions"
    [ testGroup
        "Parquet"
        [ testCountFromParquet,
          testQueryFromParquet,
          testCountFromMultipleParquetFiles,
          testQueryFromMultipleParquetFiles
        ],
      testGroup
        "Apache Iceberg"
        [ testCountFromIceberg,
          testParseDateFromIceberg
        ],
      testGroup
        "CSV"
        [ testCountFromCSV,
          testQueryFromCSV
        ]
    ]

testCountFromParquet :: TestTree
testCountFromParquet = testCase "Counting records from a Parquet file" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningList $
        select $ do
          exam <- allFromParquet_ (_dbExams testDb)
          pure (_examName exam)
  results @?= ["alice", "bob", "carol", "dave"]

testQueryFromParquet :: TestTree
testQueryFromParquet = testCase "Query from a Parquet file" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningOne $
        select $ do
          exam <- allFromParquet_ (_dbExams testDb)
          guard_ (_examId exam ==. 1)
          pure (_examName exam)
  results @?= Just "alice"

testCountFromMultipleParquetFiles :: TestTree
testCountFromMultipleParquetFiles = testCase "Counting records from multiple Parquet file" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningList $
        select $ do
          exam <- allFromParquet_ (_dbExamsMulti testDb)
          pure (_examName exam)
  results @?= ["alice", "bob", "carol", "dave", "erika", "francis", "genevieve", "hugo"]

testQueryFromMultipleParquetFiles :: TestTree
testQueryFromMultipleParquetFiles = testCase "Query from multiple Parquet files" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningOne $
        select $ do
          exam <- allFromParquet_ (_dbExamsMulti testDb)
          guard_ (_examId exam ==. 5)
          pure (_examName exam)
  results @?= Just "erika"

testCountFromIceberg :: TestTree
testCountFromIceberg = testCase "Counting records from an Apache Iceberg table" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningOne $
        select $
          aggregate_ (\_ -> as_ @Int32 countAll_) (allFromIceberg_ (_dbLineItems testDb))
  results @?= Just 51793 -- From DuckDB's documentation

testParseDateFromIceberg :: TestTree
testParseDateFromIceberg = testCase "Test parsing date columns from Apache Iceberg table" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningOne $
        select $
          aggregate_ (min_ . _lineitemShipdate) (allFromIceberg_ (_dbLineItems testDb))

  results @?= Just (Just (fromGregorian 1992 01 04))

testCountFromCSV :: TestTree
testCountFromCSV = testCase "Counting records from a CSV file" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningOne $
        select $
          aggregate_ (\_ -> as_ @Int32 countAll_) (allFromCSV_ (_dbFlights testDb))
  results @?= Just 3

testQueryFromCSV :: TestTree
testQueryFromCSV = testCase "Query from a CSV file" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningOne $
        select $ do
          flight <- allFromCSV_ (_dbFlights testDb)
          guard_ (_flightDate flight ==. val_ (fromGregorian 1988 01 01))
          pure flight
  results
    @?= Just
      ( Flight
          { _flightDate = fromGregorian 1988 01 01,
            _flightUniqueCarrier = "AA",
            _flightOriginCity = "New York, NY",
            _flightDestCity = "Los Angeles, CA"
          }
      )

data ExamT f = Exam
  { _examId :: Columnar f Int32,
    _examName :: Columnar f Text,
    _examScore :: Columnar f Double,
    _examDate :: Columnar f Day
  }
  deriving (Generic)

type Exam = ExamT Identity

type ExamId = PrimaryKey ExamT Identity

deriving instance Show ExamId

deriving instance Eq ExamId

deriving instance Ord ExamId

deriving instance Show Exam

deriving instance Eq Exam

deriving instance Ord Exam

instance Beamable ExamT

instance Table ExamT where
  data PrimaryKey ExamT f = ExamId (Columnar f Int32)
    deriving (Generic)
  primaryKey = ExamId . _examId

instance Beamable (PrimaryKey ExamT)

data LineItemT f = Lineitem
  { _lineitemOrderkey :: Columnar f Int32,
    _lineitemPartkey :: Columnar f Int32,
    _lineitemSuppkey :: Columnar f Int32,
    _lineitemLinenumber :: Columnar f Int32,
    _lineitemQuantity :: Columnar f Double,
    _lineitemExtendedprice :: Columnar f Double,
    _lineitemDiscount :: Columnar f Double,
    _lineitemTax :: Columnar f Double,
    _lineitemReturnflag :: Columnar f Text,
    _lineitemLinestatus :: Columnar f Text,
    _lineitemShipdate :: Columnar f Day,
    _lineitemCommitdate :: Columnar f Day,
    _lineitemReceiptdate :: Columnar f Day,
    _lineitemShipinstruct :: Columnar f Text,
    _lineitemShipmode :: Columnar f Text,
    _lineitemComment :: Columnar f Text
  }
  deriving (Generic)

type Lineitem = LineItemT Identity

deriving instance Show Lineitem

deriving instance Eq Lineitem

instance Beamable LineItemT

instance Table LineItemT where
  data PrimaryKey LineItemT f
    = LineItemKey (Columnar f Int32) (Columnar f Int32)
    deriving (Generic)
  primaryKey li =
    LineItemKey (_lineitemOrderkey li) (_lineitemLinenumber li)

instance Beamable (PrimaryKey LineItemT)

deriving instance Show (PrimaryKey LineItemT Identity)

deriving instance Eq (PrimaryKey LineItemT Identity)

data FlightT f = Flight
  { _flightDate :: Columnar f Day,
    _flightUniqueCarrier :: Columnar f Text,
    _flightOriginCity :: Columnar f Text,
    _flightDestCity :: Columnar f Text
  }
  deriving (Generic)

type Flight = FlightT Identity

deriving instance Show Flight

deriving instance Eq Flight

instance Beamable FlightT

instance Table FlightT where
  data PrimaryKey FlightT f
    = FlightKey
        (Columnar f Day)
        (Columnar f Text)
        (Columnar f Text)
        (Columnar f Text)
    deriving (Generic)
  primaryKey fl =
    FlightKey
      (_flightDate fl)
      (_flightUniqueCarrier fl)
      (_flightOriginCity fl)
      (_flightDestCity fl)

instance Beamable (PrimaryKey FlightT)

deriving instance Show (PrimaryKey FlightT Identity)

deriving instance Eq (PrimaryKey FlightT Identity)

data TestDB f = TestDB
  { _dbExams :: f (ParquetFileEntity ExamT),
    _dbExamsMulti :: f (ParquetFileEntity ExamT), -- set up with multiple parquet files
    _dbLineItems :: f (IcebergTableEntity LineItemT),
    _dbFlights :: f (CSVFileEntity FlightT)
  }
  deriving (Generic, Database DuckDB)

testDb :: DatabaseSettings DuckDB TestDB
testDb =
  defaultDbSettings
    `withDbModification` (dbModification @_ @DuckDB)
      { _dbExams =
          parquetFile (singleParquetFile "tests/data/test1.parquet")
            <> modifyParquetFileFields
              tableModification
                { _examId = "id",
                  _examName = "name",
                  _examScore = "score",
                  _examDate = "exam_date"
                },
        _dbExamsMulti =
          parquetFile
            ( multipleParquetFiles
                ("tests/data/test1.parquet" :| ["tests/data/test2.parquet"])
            )
            <> modifyParquetFileFields
              tableModification
                { _examId = "id",
                  _examName = "name",
                  _examScore = "score",
                  _examDate = "exam_date"
                },
        _dbLineItems =
          icebergTableWith
            "tests/data/lineitem_iceberg"
            (defaultIcebergTableOptions {allowMovedPaths = Just True})
            <> modifyIcebergTableFields
              tableModification
                { _lineitemOrderkey = "l_orderkey",
                  _lineitemPartkey = "l_partkey",
                  _lineitemSuppkey = "l_suppkey",
                  _lineitemLinenumber = "l_linenumber",
                  _lineitemQuantity = "l_quantity",
                  _lineitemExtendedprice = "l_extendedprice",
                  _lineitemDiscount = "l_discount",
                  _lineitemTax = "l_tax",
                  _lineitemReturnflag = "l_returnflag",
                  _lineitemLinestatus = "l_linestatus",
                  _lineitemShipdate = "l_shipdate",
                  _lineitemCommitdate = "l_commitdate",
                  _lineitemReceiptdate = "l_receiptdate",
                  _lineitemShipinstruct = "l_shipinstruct",
                  _lineitemShipmode = "l_shipmode",
                  _lineitemComment = "l_comment"
                },
        _dbFlights =
          csvFileWith
            (singleCSVFile "tests/data/flights.csv")
            (defaultCSVFileOptions {header = Just True, comment = Just '#', delim = Just "|", ignoreErrors = Just False})
            <> modifyCSVFileFields
              tableModification
                { _flightDate = "FlightDate",
                  _flightUniqueCarrier = "UniqueCarrier",
                  _flightOriginCity = "OriginCityName",
                  _flightDestCity = "DestCityName"
                }
      }

withTestDb ::
  (Connection -> IO a) ->
  IO a
withTestDb =
  withConnection ":memory:"
