module Database.Beam.Backend.Sqlite3 where

import Database.Beam.Types
import Database.Beam.Backend

import Control.Monad.Trans

import Database.HDBC.Sqlite3
import Database.HDBC

-- * Sqlite3 support

data Sqlite3Settings = Sqlite3Settings FilePath
                       deriving Show

instance BeamBackend Sqlite3Settings where
    openBeam (Sqlite3Settings fp) =
        do conn <- liftIO (connectSqlite3 fp)

           return Beam { closeBeam = liftIO (disconnect conn)
                       , compareSchemas = defaultBeamCompareSchemas
                       , adjustColDescForBackend = id
                       , withHDBCConnection = \f -> f conn }