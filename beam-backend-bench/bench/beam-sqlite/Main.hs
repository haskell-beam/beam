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
import Database.Beam.Sqlite (runBeamSqlite)

import Database.SQLite.Simple (
  Connection,
  close,
  executeMany,
  execute_,
  open,
  query_,
 )
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToField (toField)
import Database.SQLite.Simple.ToRow (ToRow (..))

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
              nfIO (getConn >>= selectAllSimple)
          , bench "beam-sqlite" $
              nfIO (getConn >>= selectAllBeam)
          ]
    ]
 where
  initConn :: Int -> IO Connection
  initConn nrows = do
    conn <- open ":memory:"
    void $ execute_ conn (fromString createTableSql)
    -- Wrap the bulk insert in a single transaction; without this
    -- every INSERT commits separately which costs meaningful
    -- housekeeping per statement even for an in-memory DB.
    --
    -- SQLite's default SQLITE_LIMIT_VARIABLE_NUMBER is 32,766;
    -- six columns * 5,000 rows = 30,000 parameters keeps us safely
    -- under the limit.
    let batches = chunksOf 5000 (map generateRow [1 .. nrows])
    void $ execute_ conn "BEGIN TRANSACTION"
    mapM_ (executeMany conn (fromString insertSql)) batches
    void $ execute_ conn "COMMIT"
    pure conn

selectAllBeam :: Connection -> IO [ImdbName]
selectAllBeam conn =
  runBeamSqlite conn $
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
