{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

-- ! BUILD_OPTIONS: -XTypeFamilies -XOverloadedStrings -XPartialTypeSignatures -XTypeApplications -XStandaloneDeriving -XFlexibleInstances -XMultiParamTypeClasses -XDeriveGeneric -XFlexibleContexts -fno-warn-partial-type-signatures
-- ! BUILD_DIR: beam-duckdb/examples/
module Main where

import Database.Beam
import Database.Beam.DuckDB (DuckDB)
import qualified Database.Beam.DuckDB as DuckDB
import Database.DuckDB.Simple (withConnection)
import Database.Beam.Backend.Types
import qualified Database.Beam.Query.Adhoc as Adhoc
BEAM_MODULE_IMPORT

import Control.Monad
import Control.Exception

import Data.IORef
import Data.Monoid ((<>))
import Data.Int
import Data.Text



import System.IO

data BeamDone = BeamDone
  deriving (Show)
instance Exception BeamDone

BEAM_BACKEND_EXTRA

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
  { _exams :: f (DataSourceEntity ExamT),
  }
  deriving (Generic, Database DuckDB)

schoolDB :: DatabaseSettings DuckDB SchoolDB
schoolDB =
  defaultDbSettings
    `withDbModification` (dbModification @_ @DuckDB)
      { _exams =
          dataSource (parquet (NonEmpty.singleton "exams.parquet"))
            <> modifyDataSourceFields
              tableModification
                { _examId = "id",
                  _examName = "name",
                  _examScore = "score",
                  _examDate = "exam_date"
                }
      }

main :: IO ()
main = withConnection ":memory:" $ \conn -> do
    let runBeamDuckDBDebug _ = DuckDB.runBeamDuckDB

    BEAM_PLACEHOLDER
