{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Monad (void)
import Data.List.Split (chunksOf)
import Data.String (fromString)
import Test.Tasty (withResource)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nfIO)

import Database.Beam (all_, runSelectReturningList, select)
import Database.Beam.Bench (
  ImdbDb (imdbNames),
  ImdbName,
  StrictImdbName (..),
  createTableSql,
  generateRow,
  imdbDb,
  insertSql,
  readRowCount,
  selectSql,
 )
import Database.Beam.DuckDB (runBeamDuckDB)

import Database.DuckDB.Simple (
  Connection,
  close,
  executeMany,
  execute_,
  open,
  query_,
 )
import Database.DuckDB.Simple.FromRow (FromRow (..), field)
import Database.DuckDB.Simple.ToField (toField)
import Database.DuckDB.Simple.ToRow (ToRow (..))

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
              nfIO (getConn >>= selectAllSimple)
          , bench "beam-duckdb" $
              nfIO (getConn >>= selectAllBeam)
          ]
    ]
 where
  initConn :: Int -> IO Connection
  initConn nrows = do
    conn <- open ":memory:"
    void $ execute_ conn (fromString createTableSql)
    -- DuckDB handles bulk inserts efficiently when batched. Keep
    -- batches at 5,000 rows so the generated INSERT statements stay
    -- modestly sized.
    let batches = chunksOf 5000 (map generateRow [1 .. nrows])
    mapM_ (executeMany conn (fromString insertSql)) batches
    pure conn

selectAllBeam :: Connection -> IO [ImdbName]
selectAllBeam conn =
  runBeamDuckDB conn $
    runSelectReturningList $
      select $
        all_ (imdbNames imdbDb)

selectAllSimple :: Connection -> IO [StrictImdbName]
selectAllSimple conn = query_ conn (fromString selectSql)

instance FromRow StrictImdbName where
  fromRow =
    StrictImdbName
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToRow StrictImdbName where
  toRow x =
    [ toField (siNconst x)
    , toField (siPrimaryName x)
    , toField (siBirthYear x)
    , toField (siDeathYear x)
    , toField (siPrimaryProfession x)
    , toField (siKnownForTitles x)
    ]