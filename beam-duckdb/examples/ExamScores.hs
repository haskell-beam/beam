#!/usr/bin/env cabal
{- cabal:
  build-depends: base >= 4, beam-core, beam-duckdb, duckdb-simple, time, text
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Int
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Time
import Database.Beam
import Database.Beam.DuckDB
import Database.DuckDB.Simple
import Data.Function

data ExamT f = Exam
  { _examId :: Columnar f Int32,
    _examName :: Columnar f Text,
    _examScore :: Columnar f Double,
    _examDate :: Columnar f Day
  } deriving (Generic, Beamable)

type Exam = ExamT Identity

deriving instance Show (ExamT Identity)
deriving instance Eq (ExamT Identity)

instance Table ExamT where
  data PrimaryKey ExamT f = ExamKey (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = ExamKey <$> _examId

data ScoresDB f = ScoresDB
  { _scores :: f (DataSourceEntity ExamT) }
  deriving (Generic)

deriving instance Database DuckDB ScoresDB

scoresDb :: DatabaseSettings DuckDB ScoresDB
scoresDb =
  defaultDbSettings
    `withDbModification` (dbModification @_ @DuckDB)
      { _scores =
          dataSource (parquet (NonEmpty.singleton "./tests/data/test1.parquet"))
            <> modifyDataSourceFields
              tableModification
                { _examId = "id",
                  _examName = "name",
                  _examScore = "score",
                  _examDate = "exam_date"
                }
      }

main = do
  Just maxScore <- withConnection ":memory:"
    $ \conn -> runBeamDuckDB conn
      $ runSelectReturningOne
        $ select
          $ aggregate_
              (max_ . _examScore)
              (allFromDataSource_ (_scores scoresDb))

  putStrLn "Max exam score: "
  print maxScore
