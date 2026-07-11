module Database.Beam.Postgres.Test where

import qualified Database.PostgreSQL.Simple as Pg

import           Control.Exception (bracket)

import           Control.Monad (void)

import           Data.ByteString (ByteString)
import           Data.String

withTestPostgres :: String -> IO ByteString -> (Pg.Connection -> IO a) -> IO a
withTestPostgres dbName getConnStr action = do
  connStr <- getConnStr

  -- Create and drop isolated test databases from the administrative postgres
  -- database. Connecting to template1 while cloning it is rejected by recent
  -- PostgreSQL releases because the template database is already in use.
  let connStrAdmin = connStr <> " dbname=postgres"
      connStrDb = connStr <> " dbname=" <> fromString dbName

      withAdmin :: (Pg.Connection -> IO b) -> IO b
      withAdmin = bracket (Pg.connectPostgreSQL connStrAdmin) Pg.close

      createDatabase = withAdmin $ \c -> do
                         void $ Pg.execute_ c (fromString ("CREATE DATABASE " <> dbName))

                         Pg.connectPostgreSQL connStrDb
      dropDatabase c = do
        Pg.close c
        withAdmin $ \c' -> void $
          Pg.execute_ c' (fromString ("DROP DATABASE " <> dbName))

  bracket createDatabase dropDatabase action
