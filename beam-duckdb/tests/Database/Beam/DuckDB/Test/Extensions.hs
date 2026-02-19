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
import Data.Text (Text)
import Data.Time (Day)
import Data.List.NonEmpty(NonEmpty((:|)))
import Database.Beam
  ( Beamable,
    Columnar,
    Database,
    DatabaseSettings,
    Generic,
    Identity,
    Table (..),
    dbModification,
    defaultDbSettings,
    runSelectReturningList,
    select,
    tableModification,
    withDbModification,
  )
import Database.Beam.DuckDB (DuckDB, ParquetFileEntity, allFromParquet_, modifyParquetFileFields, multipleParquetFiles, parquetFile, runBeamDuckDB, singleParquetFile)
import Database.DuckDB.Simple (Connection, withConnection)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Extensions"
    [ testGroup
        "Parquet"
        [ testSelectFromParquet,
          testSelectFromMultipleParquetFiles
        ]
    ]

testSelectFromParquet :: TestTree
testSelectFromParquet = testCase "Selecting records from a Parquet file" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningList $
        select $ do
          exam <- allFromParquet_ (_dbExams testDb)
          pure (_examName exam)
  results @?= ["alice", "bob", "carol", "dave"]

testSelectFromMultipleParquetFiles :: TestTree
testSelectFromMultipleParquetFiles = testCase "Selecting records from multiple Parquet file" $ do
  results <- withTestDb $ \conn ->
    runBeamDuckDB conn $
      runSelectReturningList $
        select $ do
          exam <- allFromParquet_ (_dbExamsMulti testDb)
          pure (_examName exam)
  results @?= ["alice", "bob", "carol", "dave", "erika", "francis", "genevieve", "hugo"]

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

data TestDB f = TestDB
  { _dbExams :: f (ParquetFileEntity ExamT),
    _dbExamsMulti :: f (ParquetFileEntity ExamT) -- set up with multiple parquet files
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
                }
      }

withTestDb ::
  (Connection -> IO a) ->
  IO a
withTestDb =
  withConnection ":memory:"
