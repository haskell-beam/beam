module Database.Beam.Postgres.Test where

import qualified Database.PostgreSQL.Simple as Pg

import Control.Exception (SomeException(..), bracket, catch)
import Control.Concurrent (threadDelay)

import Data.ByteString (ByteString)
import Data.Semigroup
import Data.String

import Network.Socket

import System.IO
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
  putStrLn "Starting postgres"
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 0 0)
  SockAddrInet port _ <- getSocketName s

  let portNumber = fromIntegral port :: Int

  hPutStrLn stderr "startTempPostgres: creating directories"

  tmpDir <- getCanonicalTemporaryDirectory
  pgDataDir <- createTempDirectory tmpDir "postgres-data"
  pgHostDir <- createTempDirectory tmpDir "postgres-host"

  hPutStrLn stderr ("startTempPostgres: created dirs " ++ show (pgDataDir, pgHostDir))
  callProcess "pg_ctl" [ "init", "-D", pgDataDir ]

  close s
  hPutStrLn stderr ("startTempPostgres: spawning postgres")
  pgHdl <- spawnProcess "postgres"
                        [ "-i", "-D", pgDataDir
                        , "-p", show portNumber
                        , "-h", "localhost"
                        , "-k", pgHostDir ]

  hPutStrLn stderr ("startTempPostgres: waiting for postgres")
  let waitForPort 10 = do
        hPutStrLn stderr ("startTempPostgres: could not connect to postgres")
        fail "Could not connect to postgres"
      waitForPort n = do
        if n == 0 then
          hPutStrLn stderr ("startTempPostgres: trying to connect")
          else hPutStrLn stderr "startTempPostgres: connecting again"
        s <- socket AF_INET Stream defaultProtocol
        ( connect s (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1))) >>
          close s )
          `catch` (\(SomeException {}) -> threadDelay 1000000 >> waitForPort (n + 1))

  waitForPort 0

  pure ( fromString ("port=" <> show portNumber <> " host=localhost")
       , interruptProcessGroupOf pgHdl )
