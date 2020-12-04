{-# LANGUAGE CPP #-}
module Database.Beam.Postgres.Test where

#if MIN_VERSION_base(4,12,0)
import           Prelude hiding (fail)
#endif

import qualified Database.PostgreSQL.Simple as Pg

import           Control.Exception (bracket)

import           Control.Monad (void)
#if MIN_VERSION_base(4,12,0)
import           Control.Monad.Fail (MonadFail(..))
#endif

import           Data.ByteString (ByteString)
import           Data.String

import qualified Hedgehog


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

#if MIN_VERSION_base(4,12,0)
#if !MIN_VERSION_hedgehog(1,0,0)
-- TODO orphan instances are bad
-- Would be easier to say 'build-depends: hedgehog >= 1.0',
-- but it's difficult to propagate to older Stackage snapshots
instance Monad m => MonadFail (Hedgehog.PropertyT m) where
    fail _ = Hedgehog.failure
#endif
#endif
