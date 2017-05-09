{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Sqlite.Types where

import           Database.Beam
import           Database.Beam.Backend.Types
import           Database.Beam.Backend.SQL

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Monoid ((<>))
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time (UTCTime, LocalTime, Day, utcToLocalTime, utc)
import           Data.Word (Word8, Word16, Word32, Word64)

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Types as Sql
import qualified Database.SQLite.Simple as Sql

import           Text.Read (readMaybe)

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
instance FromBackendRow Sqlite LocalTime where
  fromBackendRow = utcToLocalTime utc <$> fromBackendRow
instance FromBackendRow Sqlite Scientific where
  fromBackendRow = unSqliteScientific <$> fromBackendRow
instance FromBackendRow Sqlite SqliteScientific

instance Sql.FromField x => Sql.FromField (Auto x) where
  fromField field = fmap (Auto . Just) (Sql.fromField field)

newtype SqliteScientific = SqliteScientific { unSqliteScientific :: Scientific }
instance Sql.FromField SqliteScientific where
  fromField field =
    SqliteScientific <$>
    case Sql.fieldData field of
      Sql.SQLInteger i -> pure (fromIntegral i)
      Sql.SQLFloat d -> pure . fromRational . toRational $ d
      Sql.SQLText t -> tryRead (T.unpack t)
      Sql.SQLBlob b -> tryRead (BS.unpack b)
      Sql.SQLNull -> Sql.returnError Sql.UnexpectedNull field "null"
    where
      tryRead s = case readMaybe s of
                    Nothing -> Sql.returnError Sql.ConversionFailed field $ "No conversion to Scientific for '" <> s <> "'"
                    Just s'  -> pure s'

instance BeamSqlBackend Sqlite
instance BeamSql92Backend Sqlite
