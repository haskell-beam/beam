{-# LANGUAGE OverloadedStrings #-}

{- | Row-reader benchmarks comparing @beam-postgres@ against
@postgresql-simple@ on a synthetic @imdb_names@ dataset.

The benchmark spins up a temporary @postgres:16.4@ container via
testcontainers, populates it with a configurable number of rows
(env var @BEAM_BENCH_ROWS@, default 10000), and then runs three
variants of @SELECT *@:

  * @beam-postgres@: 'runBeamPostgres' + 'runSelectReturningList'
  * @postgresql-simple@: 'query_' returning @[StrictImdbName]@
  * @postgresql-simple (Vector)@: 'PSV.query_' returning @Vector StrictImdbName@
-}
module Main (main) where

import Control.Monad (void)

import Data.ByteString (ByteString)
import Data.List.Split (chunksOf)
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Text.Lazy as TL
import System.Environment (lookupEnv)
import Test.Tasty (withResource)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nfIO)
import qualified TestContainers.Tasty as TC
import Database.Beam (all_, runSelectReturningList, select)
import Database.Beam.Postgres (runBeamPostgres)

import Database.PostgreSQL.Simple (
  ConnectInfo (..),
  Connection,
  close,
  connectPostgreSQL,
  defaultConnectInfo,
  executeMany,
  execute_,
  postgreSQLConnectionString,
  query_,
 )

import BenchSchema (
  ImdbDb (imdbNames),
  ImdbName,
  StrictImdbName,
  generateRow,
  imdbDb,
 )

main :: IO ()
main = do
  nrows <- readRowCount
  putStrLn $ "Starting benchmark with " <> show nrows <> " rows"
  putStrLn "(set BEAM_BENCH_ROWS to override)"
  defaultMain
    [ TC.withContainers setupContainer $ \getConnStr ->
        withResource (initConn nrows getConnStr) close $ \getConn ->
          bgroup
            ("SELECT * FROM imdb_names (" <> show nrows <> " rows)")
            [ bench "postgresql-simple" $
                nfIO (getConn >>= selectAllSimple)
            , bench "beam-postgres" $
                nfIO (getConn >>= selectAllBeam)
            ]
    ]
 where
  initConn :: Int -> IO ByteString -> IO Connection
  initConn nrows getConnStr = do
    conn <- connectPostgreSQL =<< getConnStr

    void $
      execute_ conn $
        fromString $
          unwords
            [ "CREATE TABLE imdb_names ("
            , "  nconst              TEXT NOT NULL,"
            , "  primary_name        TEXT,"
            , "  birth_year          INTEGER,"
            , "  death_year          INTEGER,"
            , "  primary_profession  TEXT,"
            , "  known_for_titles    TEXT"
            , ")"
            ]

    let rows = map generateRow [1 .. nrows]
        -- Postgres has a 65,535 parameter limit per query, and each row has 6 columns, so a batch
        -- size of 5,000 rows = 30,000 parameters keeps us well under it.
        batchSize = 5000
        batches = chunksOf batchSize rows
        insertSql =
          fromString $
            unwords
              [ "INSERT INTO imdb_names"
              , "  (nconst, primary_name, birth_year, death_year,"
              , "   primary_profession, known_for_titles)"
              , "VALUES (?, ?, ?, ?, ?, ?)"
              ]
    mapM_ (executeMany conn insertSql) batches

    pure conn

selectAllBeam :: Connection -> IO [ImdbName]
selectAllBeam conn =
  runBeamPostgres conn $
    runSelectReturningList $
      select $
        all_ (imdbNames imdbDb)

selectAllSimple :: Connection -> IO [StrictImdbName]
selectAllSimple conn =
  query_
    conn
    "SELECT nconst, primary_name, birth_year, death_year, \
    \primary_profession, known_for_titles FROM imdb_names"

{- | Number of synthetic rows to insert. Read from the env var
  @BEAM_BENCH_ROWS@; defaults to 10,000 for a balance of signal
  vs. benchmark wall-time.
-}
defaultRowCount :: Int
defaultRowCount = 10000

readRowCount :: IO Int
readRowCount = do
  mbStr <- lookupEnv "BEAM_BENCH_ROWS"
  pure $ case mbStr of
    Just s | [(n, "")] <- reads s -> n
    _ -> defaultRowCount

setupContainer :: (TC.MonadDocker m) => m ByteString
setupContainer = do
  let user = "postgres"
      pw = "root"
      db = "benchdb"
      port = 5432

  c <-
    TC.run $
      TC.containerRequest (TC.fromTag "postgres:16.4")
        TC.& TC.setExpose [port]
        TC.& TC.setEnv
          [ ("POSTGRES_USER", user)
          , ("POSTGRES_PASSWORD", pw)
          , ("POSTGRES_DB", db)
          ]
        TC.& TC.setWaitingFor
          ( TC.waitForLogLine
              TC.Stderr
              ( "database system is ready to accept connections"
                  `TL.isInfixOf`
              )
          )

  pure $
    postgreSQLConnectionString
      defaultConnectInfo
        { connectHost = "localhost"
        , connectUser = unpack user
        , connectPassword = unpack pw
        , connectDatabase = unpack db
        , connectPort = fromIntegral $ TC.containerPort c port
        }
