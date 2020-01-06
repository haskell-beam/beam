{-# LANGUAGE CPP #-}
module Database.Beam.Postgres.Test where

#if MIN_VERSION_base(4,12,0)
import           Prelude hiding (fail)
#endif

import qualified Database.PostgreSQL.Simple as Pg

import           Control.Exception (SomeException(..), bracket, catch)
import           Control.Concurrent (threadDelay)

import           Control.Monad (void)
#if MIN_VERSION_base(4,12,0)
import           Control.Monad.Fail (MonadFail(..))
#endif

import           Data.ByteString (ByteString)
import           Data.Semigroup
import           Data.String

import qualified Hedgehog

import           System.IO.Temp
import           System.Process
import           System.Exit
import           System.FilePath
import           System.Directory

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
  tmpDir <- getCanonicalTemporaryDirectory
  pgDataDir <- createTempDirectory tmpDir "postgres-data"

  callProcess "pg_ctl" [ "init", "-D", pgDataDir ]

  -- Use 'D' because otherwise, the path is too long on OS X
  pgHdl <- spawnProcess "postgres"
                        [ "-D", pgDataDir
                        , "-k", pgDataDir, "-h", "" ]

  putStrLn ("Using " ++ pgDataDir ++ " as postgres host")

  let waitForPort 10 = fail "Could not connect to postgres"
      waitForPort n = do
        (code, stdout, stderr) <- readProcessWithExitCode "pg_ctl" [ "status", "-D", pgDataDir ] ""
        case code of
          ExitSuccess -> waitForSocket 0
          ExitFailure _ -> threadDelay 1000000 >> waitForPort (n + 1)

      waitForSocket 10 = fail "Could not connect to postgres (waitForSocket)"
      waitForSocket n = do
        skExists <- doesFileExist (pgDataDir </> ".s.PGSQL.5432")
        if skExists then pure () else threadDelay 1000000 >> waitForSocket (n + 1)

  waitForPort 0
  putStrLn "Completed waiting for postgres"

  pure ( fromString ("host=" ++ pgDataDir)
       , void (callProcess "pg_ctl" [ "stop", "-D", pgDataDir ]))

#if MIN_VERSION_base(4,12,0)
#if !MIN_VERSION_hedgehog(1,0,0)
-- TODO orphan instances are bad
-- Would be easier to say 'build-depends: hedgehog >= 1.0',
-- but it's difficult to propagate to older Stackage snapshots
instance Monad m => MonadFail (Hedgehog.PropertyT m) where
    fail _ = Hedgehog.failure
#endif
#endif
