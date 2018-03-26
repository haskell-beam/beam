{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Beam.Backend.SQL.Types where

import Database.Beam.Backend.Types

import qualified Data.Aeson as Json
import           Data.Bits

class ( BeamBackend be ) =>
      BeamSqlBackend be where

data SqlNull = SqlNull
  deriving (Show, Eq, Ord, Bounded, Enum)
newtype SqlBitString = SqlBitString Integer
  deriving (Show, Eq, Ord, Enum, Bits)

newtype SqlSerial a = SqlSerial { unSerial :: a }
  deriving (Show, Read, Eq, Ord, Num)
instance FromBackendRow be x => FromBackendRow be (SqlSerial x) where
  fromBackendRow = SqlSerial <$> fromBackendRow
instance Json.FromJSON a => Json.FromJSON (SqlSerial a) where
  parseJSON a = SqlSerial <$> Json.parseJSON a
instance Json.ToJSON a => Json.ToJSON (SqlSerial a) where
  toJSON (SqlSerial a) = Json.toJSON a
  toEncoding (SqlSerial a) = Json.toEncoding a

