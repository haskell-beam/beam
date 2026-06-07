module Database.Beam.Postgres.Test where

import qualified Database.PostgreSQL.Simple as Pg

import           Control.Exception (bracket)

import           Control.Monad (void)

import           Data.ByteString (ByteString)
import           Data.String

withTestPostgres :: String -> IO ByteString -> (Pg.Connection -> IO a) -> IO a
withTestPostgres dbName getConnStr action = do
  connStr <- getConnStr

  let connStrTemplate1 = connStr <> " dbname=template1"
      connStrDb = connStr <> " dbname=" <> fromString dbName

      withTemplate1 :: (Pg.Connection -> IO b) -> IO b
      withTemplate1 = bracket (Pg.connectPostgreSQL connStrTemplate1) Pg.close

      createDatabase = withTemplate1 $ \c -> do
                         void $ Pg.execute_ c (fromString ("CREATE DATABASE " <> dbName))

                         Pg.connectPostgreSQL connStrDb
      dropDatabase c = do
        Pg.close c
        withTemplate1 $ \c' -> void $
          Pg.execute_ c' (fromString ("DROP DATABASE " <> dbName))

  bracket createDatabase dropDatabase action
