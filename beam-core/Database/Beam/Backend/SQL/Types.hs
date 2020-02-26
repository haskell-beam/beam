{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Beam.Backend.SQL.Types where

import qualified Data.Aeson as Json
import           Data.Bits
import           Data.Functor.Contravariant (contramap)

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
instance Json.FromJSONKey a => Json.FromJSONKey (SqlSerial a) where
  fromJSONKey = SqlSerial <$> Json.fromJSONKey
  fromJSONKeyList = fmap SqlSerial <$> Json.fromJSONKeyList
instance Json.ToJSONKey a => Json.ToJSONKey (SqlSerial a) where
  toJSONKey = contramap unSerial Json.toJSONKey
  toJSONKeyList = contramap (fmap unSerial) Json.toJSONKeyList
