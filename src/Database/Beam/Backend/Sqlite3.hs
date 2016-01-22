module Database.Beam.Backend.Sqlite3 where

import Database.Beam.Types
import Database.Beam.Internal
import Database.Beam.Backend
import Database.Beam.SQL.Types

import Control.Monad.Trans

import Database.HDBC.Sqlite3
import Database.HDBC

import Data.Text (Text, unpack)

-- * Sqlite3 support

data Sqlite3Settings = Sqlite3Settings FilePath
                       deriving Show

instance BeamBackend Sqlite3Settings where
    openBeam dbSettings (Sqlite3Settings fp) =
        do conn <- liftIO (connectSqlite3 fp)

           return Beam { beamDbSettings = dbSettings
                       , beamDebug = False
                       , closeBeam = liftIO (disconnect conn)
                       , compareSchemas = defaultBeamCompareSchemas
                       , adjustColDescForBackend =
                           \cs -> cs { csConstraints = filter (/=SQLAutoIncrement) (csConstraints cs) }
                       , getLastInsertedRow = getLastInsertedRow' conn
                       , withHDBCConnection = \f -> f conn }

getLastInsertedRow' :: MonadIO m => Connection -> Text -> m [SqlValue]
getLastInsertedRow' conn tblName = do
  [res] <- liftIO (quickQuery conn (concat ["SELECT * FROM ", unpack tblName, " WHERE ROWID=(SELECT last_insert_rowid()) limit 1"]) [])
  return res
