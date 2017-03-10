module Database.Beam.Backend.Sqlite3 where

import Database.Beam.Query.Internal
import Database.Beam.Query.Combinators
import Database.Beam.Backend
import Database.Beam.SQL.Types
import Database.Beam.Schema.Tables
import Database.Beam.Schema.Fields

import Control.Monad.Trans

import Database.HDBC.Sqlite3
import Database.HDBC

import Data.Convertible
import Data.Time
import Data.Data
import Data.Text (Text, unpack)

-- * Sqlite3 support

deriving instance Data SqlValue
data Sqlite3Settings = Sqlite3Settings FilePath
                       deriving (Show, Data)

instance BeamBackend Sqlite3Settings where
    newtype BeamBackendValue Sqlite3Settings = Sqlite3Value SqlValue deriving (Eq, Show, Data)

    openBeam dbSettings (Sqlite3Settings fp) =
        do conn <- liftIO (connectSqlite3 fp)

           return Beam { beamDbSettings = dbSettings
                       , beamDebug = False
                       , closeBeam = liftIO (disconnect conn)
                       , compareSchemas = defaultBeamCompareSchemas
                       , adjustColDescForBackend =
                           \cs -> cs { csConstraints = filter (/=SQLAutoIncrement) (csConstraints cs) }
                       , getLastInsertedRow = \name -> map Sqlite3Value <$> getLastInsertedRow' conn name

                       , beamExecute = \s vals ->
                                       liftIO $
                                       do stmt <- prepare conn s
                                          execute stmt (map (\(Sqlite3Value v) -> v) vals)
                                          pure (Right ((map Sqlite3Value <$>) <$> fetchRow stmt))
                       , beamDescribeDatabase = hdbcSchema conn
                       , beamCommit = liftIO (commit conn)
                       , beamRollback = liftIO (rollback conn) }

    sqlNull = Sqlite3Value SqlNull
    sqlInteger = Sqlite3Value . SqlInteger
    sqlString = Sqlite3Value . SqlString
    sqlUTCTime = Sqlite3Value . SqlUTCTime
    sqlBool = Sqlite3Value . SqlBool

    fromBackendValue (Sqlite3Value SqlNull) = Just BeamNull
    fromBackendValue (Sqlite3Value (SqlUTCTime t)) = Just (BeamUTCTime t)
    fromBackendValue (Sqlite3Value i)
        | Right x <- safeFromSql i = Just (BeamInteger x)
    fromBackendValue (Sqlite3Value s)
        | Right s <- safeFromSql s = Just (BeamString s)
    fromBackendValue _ = Nothing

    backendAsBool (Sqlite3Value s) =
        case safeFromSql s of
          Right x -> x
          Left _ -> False

getLastInsertedRow' :: MonadIO m => Connection -> Text -> m [SqlValue]
getLastInsertedRow' conn tblName = do
  [res] <- liftIO (quickQuery conn (concat ["SELECT * FROM ", unpack tblName, " WHERE ROWID=(SELECT last_insert_rowid()) limit 1"]) [])
  return res

(++.) :: QExpr Sqlite3Settings s Text -> QExpr Sqlite3Settings s Text -> QExpr Sqlite3Settings s Text
QExpr a ++. QExpr b = QExpr (SQLBinOpE "||" a b)

instance HasDefaultFieldSchema Sqlite3Settings a => HasDefaultFieldSchema Sqlite3Settings (Maybe a) where
    defFieldSchema be = maybeFieldSchema (defFieldSchema be)
instance HasDefaultFieldSchema Sqlite3Settings Int where
    defFieldSchema _ = intSchema
instance HasDefaultFieldSchema Sqlite3Settings AutoId where
    defFieldSchema _ = autoIdSchema
instance HasDefaultFieldSchema Sqlite3Settings Text where
    defFieldSchema _ = defaultTextSchema
instance HasDefaultFieldSchema Sqlite3Settings UTCTime where
    defFieldSchema _ = dateTimeSchema

instance Convertible a SqlValue => SqlValable (QExpr Sqlite3Settings s a) where
    val_ = QExpr . SQLValE . Sqlite3Value . convert
