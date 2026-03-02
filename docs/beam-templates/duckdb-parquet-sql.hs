{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

-- ! BUILD_COMMAND: runhaskell -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XDeriveAnyClass -XPartialTypeSignatures -fno-warn-partial-type-signatures
-- ! BUILD_DIR: beam-sqlite/examples/
-- ! FORMAT: sql
module Main where

import Database.Beam
import Database.Beam.DuckDB hiding (runBeamDuckDBDebug)
import qualified Database.Beam.DuckDB as DuckDB
import Database.DuckDB.Simple (withConnection)
import Database.Beam.Backend.Types

import Control.Monad
import Control.Exception

import qualified Data.List.NonEmpty as NonEmpty
import Data.Int
import Data.Text
import Data.Time (Day)

import System.Environment (getEnv)

data BeamDone = BeamDone
  deriving (Show)
instance Exception BeamDone

data ExamT f = Exam
  { _examId :: Columnar f Int32,
    _examName :: Columnar f Text,
    _examScore :: Columnar f Double,
    _examDate :: Columnar f Day
  }
  deriving (Generic)

type Exam = ExamT Identity
deriving instance Show Exam
deriving instance Eq Exam

instance Beamable ExamT
instance Table ExamT where
  data PrimaryKey ExamT f = ExamId (Columnar f Int32) deriving (Generic)
  primaryKey = ExamId . _examId
instance Beamable (PrimaryKey ExamT)

data SchoolDB f = SchoolDB
  { _exams :: f (DataSourceEntity ExamT)
  }
  deriving (Generic, Database DuckDB)

main :: IO ()
main = do
  beamSource <- getEnv "BEAM_SOURCE"
  let parquetPath = beamSource <> "/docs/.beam-query-cache/data/exams.parquet"

      schoolDB :: DatabaseSettings DuckDB SchoolDB
      schoolDB =
        defaultDbSettings
          `withDbModification` (dbModification @_ @DuckDB)
            { _exams =
                dataSource (parquet (NonEmpty.singleton parquetPath))
                  <> modifyDataSourceFields
                    tableModification
                      { _examId = "id",
                        _examName = "name",
                        _examScore = "score",
                        _examDate = "exam_date"
                      }
            }

  withConnection ":memory:" $ \conn -> do
    let runBeamDuckDBDebug _ = DuckDB.runBeamDuckDBDebugString putStrLn

    ( do
        -- Don't print the result
        let print :: Show a => a -> IO ()
            print _ = pure ()

        BEAM_PLACEHOLDER
      )