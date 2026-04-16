{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.List.Split (chunksOf)
import Data.String (fromString)
import Data.Text (unpack)
import qualified Data.Text.Lazy as TL
import Test.Tasty (withResource)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nfIO)
import qualified TestContainers.Tasty as TC

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
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))

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
    void $ execute_ conn (fromString createTableSql)
    -- Postgres has a 65,535 parameter limit per query; six columns per
    -- row * 5,000 rows = 30,000 parameters is safely under it.
    let batches = chunksOf 5000 (map generateRow [1 .. nrows])
    mapM_ (executeMany conn (fromString insertSql)) batches
    pure conn

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

selectAllBeam :: Connection -> IO [ImdbName]
selectAllBeam conn =
  runBeamPostgres conn $
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