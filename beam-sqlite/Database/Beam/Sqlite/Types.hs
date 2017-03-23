module Database.Beam.Sqlite.Types where

import           Database.Beam
import           Database.Beam.Backend.Types
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.Builder

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Time (UTCTime, LocalTime, Day)
import           Data.Word (Word8, Word16, Word32, Word64)

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Types as Sql

data Sqlite = Sqlite

instance BeamBackend Sqlite where
  type BackendFromField Sqlite = Sql.FromField

instance FromBackendRow Sqlite Bool
instance FromBackendRow Sqlite Double
instance FromBackendRow Sqlite Float
instance FromBackendRow Sqlite Int
instance FromBackendRow Sqlite Int8
instance FromBackendRow Sqlite Int16
instance FromBackendRow Sqlite Int32
instance FromBackendRow Sqlite Int64
instance FromBackendRow Sqlite Integer
instance FromBackendRow Sqlite Word
instance FromBackendRow Sqlite Word8
instance FromBackendRow Sqlite Word16
instance FromBackendRow Sqlite Word32
instance FromBackendRow Sqlite Word64
instance FromBackendRow Sqlite ByteString
instance FromBackendRow Sqlite BL.ByteString
instance FromBackendRow Sqlite Text
instance FromBackendRow Sqlite TL.Text
instance FromBackendRow Sqlite UTCTime
instance FromBackendRow Sqlite Day
instance FromBackendRow Sqlite Sql.Null

instance Sql.FromField x => Sql.FromField (Auto x) where
  fromField field = fmap (Auto . Just) (Sql.fromField field)

instance BeamSqlBackend Sqlite
instance BeamSql92Backend Sqlite
