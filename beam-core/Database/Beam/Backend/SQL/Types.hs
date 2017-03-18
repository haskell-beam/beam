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
