{-# LANGUAGE OverloadedStrings #-}

-- | Row-reader benchmarks comparing @beam-sqlite@ against
-- @sqlite-simple@ on a synthetic @imdb_names@ dataset.
--
-- An in-memory SQLite database is created, populated with a
-- configurable number of rows (env var @BEAM_BENCH_ROWS@,
-- default 10000), and then two variants of @SELECT *@ are run:
--
--  * @sqlite-simple@: 'query_' returning @[StrictImdbName]@
--  * @beam-sqlite@: 'runBeamSqlite' + 'runSelectReturningList'
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
import Data.String (fromString)
import Database.Beam (all_, runSelectReturningList, select)
import Database.Beam.Sqlite (runBeamSqlite)
import Database.SQLite.Simple
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
          [ bench "sqlite-simple" $
              nfIO (getConn >>= selectAllSimple),
            bench "beam-sqlite" $
              nfIO (getConn >>= selectAllBeam)
          ]
    ]
  where
    initConn :: Int -> IO Connection
    initConn nrows = do
      conn <- open ":memory:"

      void $
        execute_ conn $
          fromString $
            unwords
              [ "CREATE TABLE imdb_names (",
                "  nconst              TEXT NOT NULL,",
                "  primary_name        TEXT,",
                "  birth_year          INTEGER,",
                "  death_year          INTEGER,",
                "  primary_profession  TEXT,",
                "  known_for_titles    TEXT",
                ")"
              ]

      -- Wrap the bulk insert in a single transaction; without this every
      -- INSERT would commit separately, which for in-memory SQLite still
      -- costs a lot of housekeeping per statement.
      let rows = map generateRow [1 .. nrows]
          -- SQLite's default SQLITE_LIMIT_VARIABLE_NUMBER is 32,766; six
          -- columns per row → 5,000 rows = 30,000 parameters keeps us
          -- safely under the limit.
          batchSize = 5000
          batches = chunksOf batchSize rows
          insertSql =
            fromString $
              unwords
                [ "INSERT INTO imdb_names",
                  "  (nconst, primary_name, birth_year, death_year,",
                  "   primary_profession, known_for_titles)",
                  "VALUES (?, ?, ?, ?, ?, ?)"
                ]
      void $ execute_ conn "BEGIN TRANSACTION"
      mapM_ (executeMany conn insertSql) batches
      void $ execute_ conn "COMMIT"

      pure conn

selectAllBeam :: Connection -> IO [ImdbName]
selectAllBeam conn =
  runBeamSqlite conn $
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
