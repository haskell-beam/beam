module Database.Beam.Postgres.Test where

import qualified Database.PostgreSQL.Simple as Pg

import Control.Exception (SomeException(..), bracket, catch)
import Control.Concurrent (threadDelay)

import Data.ByteString (ByteString)
import Data.Semigroup
import Data.String

import Network.Socket

import System.IO.Temp
import System.Process

withTestPostgres :: String -> IO ByteString -> (Pg.Connection -> IO a) -> IO a
withTestPostgres dbName getConnStr action = do
  connStr <- getConnStr

  let connStrTemplate1 = connStr <> " dbname=template1"
      connStrDb = connStr <> " dbname=" <> fromString dbName

      withTemplate1 :: (Pg.Connection -> IO b) -> IO b
      withTemplate1 = bracket (Pg.connectPostgreSQL connStrTemplate1) Pg.close

      createDatabase = withTemplate1 $ \c -> do
                         Pg.execute_ c (fromString ("CREATE DATABASE " <> dbName))

                         Pg.connectPostgreSQL connStrDb
      dropDatabase c = do
        Pg.close c
        withTemplate1 $ \c' -> do
            Pg.execute_ c' (fromString ("DROP DATABASE " <> dbName))
            pure ()

  bracket createDatabase dropDatabase action

startTempPostgres :: IO (ByteString, IO ())
startTempPostgres = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 0 0)
  SockAddrInet port _ <- getSocketName s

  let portNumber = fromIntegral port :: Int

  tmpDir <- getCanonicalTemporaryDirectory
  pgDataDir <- createTempDirectory tmpDir "postgres-data"

  callProcess "pg_ctl" [ "init", "-D", pgDataDir ]

  close s
  pgHdl <- spawnProcess "postgres"
                        [ "-i", "-D", pgDataDir
                        , "-p", show portNumber
                        , "-h", "localhost" ]

  let waitForPort 10 = fail "Could not connect to postgres"
      waitForPort n = do
        s <- socket AF_INET Stream defaultProtocol
        ( connect s (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1))) >>
          close s )
          `catch` (\(SomeException {}) -> threadDelay 1000000 >> waitForPort (n + 1))

  waitForPort 0

  pure ( fromString ("port=" <> show portNumber <> " host=localhost")
       , interruptProcessGroupOf pgHdl )
