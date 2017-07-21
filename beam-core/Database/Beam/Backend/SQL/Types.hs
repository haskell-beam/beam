{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Beam.Backend.SQL.Types where

import Database.Beam.Backend.Types

import Data.Bits

class ( BeamBackend be ) =>
      BeamSqlBackend be where

data SqlNull = SqlNull
  deriving (Show, Eq, Ord, Bounded, Enum)
newtype SqlBitString = SqlBitString Integer
  deriving (Show, Eq, Ord, Enum, Bits)

newtype SqlSerial a = SqlSerial { unSerial :: a }
  deriving (Show, Read, Eq, Ord)
instance FromBackendRow be x => FromBackendRow be (SqlSerial x) where
  fromBackendRow = SqlSerial <$> fromBackendRow
