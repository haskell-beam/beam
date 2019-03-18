{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Beam.Backend.SQL.Types where

import qualified Data.Aeson as Json
import           Data.Bits

data SqlNull = SqlNull
  deriving (Show, Eq, Ord, Bounded, Enum)
newtype SqlBitString = SqlBitString Integer
  deriving (Show, Eq, Ord, Enum, Bits)

newtype SqlSerial a = SqlSerial { unSerial :: a }
  deriving (Show, Read, Eq, Ord, Num, Integral, Real, Enum)

instance Json.FromJSON a => Json.FromJSON (SqlSerial a) where
  parseJSON a = SqlSerial <$> Json.parseJSON a
instance Json.ToJSON a => Json.ToJSON (SqlSerial a) where
  toJSON (SqlSerial a) = Json.toJSON a
  toEncoding (SqlSerial a) = Json.toEncoding a

