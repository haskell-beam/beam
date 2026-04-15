{-# LANGUAGE OverloadedStrings #-}

-- | Row-reader benchmarks comparing @beam-duckdb@ against
-- @duckdb-simple@ on a synthetic @imdb_names@ dataset.
--
-- An in-memory DuckDB database is created, populated with a
-- configurable number of rows (env var @BEAM_BENCH_ROWS@,
-- default 10000), and then two variants of @SELECT *@ are run:
--
--  * @duckdb-simple@: 'query_' returning @[StrictImdbName]@
--  * @beam-duckdb@: 'runBeamDuckDB' + 'runSelectReturningList'
module Main (main) where

import BenchSchema
  ( ImdbDb (imdbNames),
    ImdbName,
    StrictImdbName,
    generateRow,
    imdbDb,
  )
import Control.Monad (void)
import Data.List.Split (chunksOf)
import Database.Beam (all_, runSelectReturningList, select)
import Database.Beam.DuckDB (runBeamDuckDB)
import Database.DuckDB.Simple
  ( Connection,
    close,
    executeMany,
    execute_,
    open,
    query_,
  )
import System.Environment (lookupEnv)
import Test.Tasty (withResource)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nfIO)

main :: IO ()
main = do
  nrows <- readRowCount
  putStrLn $ "Starting benchmark with " <> show nrows <> " rows"
  putStrLn "(set BEAM_BENCH_ROWS to override)"
  defaultMain
    [ withResource (initConn nrows) close $ \getConn ->
        bgroup
          ("SELECT * FROM imdb_names (" <> show nrows <> " rows)")
          [ bench "duckdb-simple" $
              nfIO (getConn >>= selectAllSimple),
            bench "beam-duckdb" $
              nfIO (getConn >>= selectAllBeam)
          ]
    ]
  where
    initConn :: Int -> IO Connection
    initConn nrows = do
      conn <- open ":memory:"

      void $
        execute_
          conn
          "CREATE TABLE imdb_names (\
          \  nconst              TEXT NOT NULL,\
          \  primary_name        TEXT,\
          \  birth_year          INTEGER,\
          \  death_year          INTEGER,\
          \  primary_profession  TEXT,\
          \  known_for_titles    TEXT\
          \)"

      -- DuckDB handles bulk inserts efficiently when batched. We keep
      -- batches at 5,000 rows so the generated INSERT statements stay
      -- modestly sized.
      let rows = map generateRow [1 .. nrows]
          batchSize = 5000
          batches = chunksOf batchSize rows
          insertSql =
            "INSERT INTO imdb_names \
            \  (nconst, primary_name, birth_year, death_year, \
            \   primary_profession, known_for_titles) \
            \VALUES (?, ?, ?, ?, ?, ?)"
      mapM_ (executeMany conn insertSql) batches

      pure conn

selectAllBeam :: Connection -> IO [ImdbName]
selectAllBeam conn =
  runBeamDuckDB conn $
    runSelectReturningList $
      select $
        all_ (imdbNames imdbDb)

selectAllSimple :: Connection -> IO [StrictImdbName]
selectAllSimple conn =
  query_
    conn
    "SELECT nconst, primary_name, birth_year, death_year, \
    \primary_profession, known_for_titles FROM imdb_names"

-- | Number of synthetic rows to insert. Read from the env var
--  @BEAM_BENCH_ROWS@; defaults to 10,000 for a balance of signal
--  vs. benchmark wall-time.
defaultRowCount :: Int
defaultRowCount = 10000

readRowCount :: IO Int
readRowCount = do
  mbStr <- lookupEnv "BEAM_BENCH_ROWS"
  pure $ case mbStr of
    Just s | [(n, "")] <- reads s -> n
    _ -> defaultRowCount
