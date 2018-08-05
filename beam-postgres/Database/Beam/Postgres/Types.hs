{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Postgres.Types
  ( Postgres(..) ) where

import           Database.Beam
import           Database.Beam.Backend.SQL

import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.HStore as Pg (HStoreMap, HStoreList)
import qualified Database.PostgreSQL.Simple.Types as Pg
import qualified Database.PostgreSQL.Simple.Range as Pg (PGRange)
import qualified Database.PostgreSQL.Simple.Time as Pg (Date, UTCTimestamp, ZonedTimestamp, LocalTimestamp)

import           Data.Aeson (Value)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive (CI)
import           Data.Int
import           Data.Ratio (Ratio)
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Time (UTCTime, Day, TimeOfDay, LocalTime, ZonedTime(..))
import           Data.UUID.Types (UUID)
import           Data.Vector (Vector)
import           Data.Word

-- | The Postgres backend type, used to parameterize 'MonadBeam'. See the
-- definitions there for more information. The corresponding query monad is
-- 'Pg'. See documentation for 'MonadBeam' and the
-- <https://tathougies.github/beam/ user guide> for more information on using
-- this backend.
data Postgres
  = Postgres

instance BeamBackend Postgres where
  type BackendFromField Postgres = Pg.FromField

instance Pg.FromField SqlNull where
  fromField field d = fmap (\Pg.Null -> SqlNull) (Pg.fromField field d)

fromScientificOrIntegral :: forall a. (Bounded a, Integral a) => FromBackendRowA Postgres (Result a)
fromScientificOrIntegral =
  parseAlternative (maybe (Error $ BeamRowError "fromScientificOrIntegral: downcasting would result in loss of data") Result <$> toBoundedInteger)
                   (Result . (fromIntegral :: Integer -> a))

fromPgIntegral :: forall a. (Pg.FromField a, Integral a) => FromBackendRowA Postgres (Result a)
fromPgIntegral = parseAlternative Result f
  where
    f :: Integer -> Result a
    f x =
      let x' = fromIntegral x
      in if x == fromIntegral x'
           then Result x'
           else Error $ BeamRowError "fromPgIntegral: downcasting would result in loss of data"

-- Default FromBackendRow instances for all postgresql-simple FromField instances
instance FromBackendRow Postgres SqlNull
instance FromBackendRow Postgres Bool
instance FromBackendRow Postgres Char
instance FromBackendRow Postgres Double
instance FromBackendRow Postgres Int where
  fromBackendRow = fromPgIntegral
instance FromBackendRow Postgres Int16 where
  fromBackendRow = fromPgIntegral
instance FromBackendRow Postgres Int32 where
  fromBackendRow = fromPgIntegral
instance FromBackendRow Postgres Int64 where
  fromBackendRow = fromPgIntegral
-- Word values are serialized as SQL @NUMBER@ types to guarantee full domain coverage.
-- However, we wan them te be serialized/deserialized as whichever type makes sense
instance FromBackendRow Postgres Word where
  fromBackendRow = fromScientificOrIntegral
instance FromBackendRow Postgres Word16 where
  fromBackendRow = fromScientificOrIntegral
instance FromBackendRow Postgres Word32 where
  fromBackendRow = fromScientificOrIntegral
instance FromBackendRow Postgres Word64 where
  fromBackendRow = fromScientificOrIntegral
instance FromBackendRow Postgres Integer
instance FromBackendRow Postgres ByteString
instance FromBackendRow Postgres Scientific
instance FromBackendRow Postgres BL.ByteString
instance FromBackendRow Postgres Text
instance FromBackendRow Postgres UTCTime
instance FromBackendRow Postgres Value
instance FromBackendRow Postgres TL.Text
instance FromBackendRow Postgres Pg.Oid
instance FromBackendRow Postgres LocalTime where
  fromBackendRow = parseAlternative Result (Result . zonedTimeToLocalTime)
instance FromBackendRow Postgres TimeOfDay
instance FromBackendRow Postgres Day
instance FromBackendRow Postgres UUID
instance FromBackendRow Postgres Pg.Null
instance FromBackendRow Postgres Pg.Date
instance FromBackendRow Postgres Pg.ZonedTimestamp
instance FromBackendRow Postgres Pg.UTCTimestamp
instance FromBackendRow Postgres Pg.LocalTimestamp
instance FromBackendRow Postgres Pg.HStoreMap
instance FromBackendRow Postgres Pg.HStoreList
instance FromBackendRow Postgres [Char]
instance FromBackendRow Postgres (Ratio Integer)
instance FromBackendRow Postgres (CI Text)
instance FromBackendRow Postgres (CI TL.Text)
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Vector a) where
  fromBackendRow = f <$> parseOneField
    where
      f Null = Result mempty
      f x = x
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Pg.PGArray a)
instance FromBackendRow Postgres (Pg.Binary ByteString)
instance FromBackendRow Postgres (Pg.Binary BL.ByteString)
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Pg.PGRange a)
instance (Pg.FromField a, Pg.FromField b) => FromBackendRow Postgres (Either a b)

instance BeamSqlBackend Postgres
instance BeamSql92Backend Postgres
