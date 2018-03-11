{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Postgres.Types where

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
import           Data.Time (UTCTime, Day, TimeOfDay, LocalTime, ZonedTime)
import           Data.UUID (UUID)
import           Data.Vector (Vector)
import           Data.Word

data Postgres
  = Postgres

instance BeamBackend Postgres where
  type BackendFromField Postgres = Pg.FromField

instance Pg.FromField SqlNull where
  fromField field d = fmap (\Pg.Null -> SqlNull) (Pg.fromField field d)

fromScientificOrIntegral :: (Bounded a, Integral a) => FromBackendRowM Postgres a
fromScientificOrIntegral = do
  sciVal <- fmap (toBoundedInteger =<<) peekField
  case sciVal of
    Just sciVal' -> do
      -- If the parse succeeded, consume the field
      _ <- parseOneField @Postgres @Scientific
      pure sciVal'
    Nothing -> fromIntegral <$> fromBackendRow @Postgres @Integer

-- | Deserialize integral fields, possibly downcasting from a larger integral
-- type, but only if we won't lose data
fromPgIntegral :: forall a
                . (Pg.FromField a, Integral a)
               => FromBackendRowM Postgres a
fromPgIntegral = do
  val <- peekField
  case val of
    Just val' -> do
      _ <- parseOneField @Postgres @a
      pure val'
    Nothing -> do
      val' <- parseOneField @Postgres @Integer
      let val'' = fromIntegral val'
      if fromIntegral val'' == val'
        then pure val''
        else fail (concat [ "Data loss while downsizing Integral type. "
                          , "Make sure your Haskell types are wide enough for your data" ])

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
instance FromBackendRow Postgres LocalTime
instance FromBackendRow Postgres ZonedTime
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
    fromBackendRow = do
      isNull <- peekField
      case isNull of
        Just SqlNull -> pure mempty
        Nothing -> parseOneField @Postgres @(Vector a)
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Pg.PGArray a)
instance FromBackendRow Postgres (Pg.Binary ByteString)
instance FromBackendRow Postgres (Pg.Binary BL.ByteString)
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Pg.PGRange a)
instance (Pg.FromField a, Pg.FromField b) => FromBackendRow Postgres (Either a b)

instance BeamSqlBackend Postgres
instance BeamSql92Backend Postgres

-- * Types for postgres specific extensions

newtype PgTableSamplingMethod
  = PgTableSamplingMethod { pgTableSamplingMethodAsText :: Text }
  deriving (Show, Eq, Ord)

pgBernoulliSamplingMethod, pgSystemSamplingMethod :: PgTableSamplingMethod
pgBernoulliSamplingMethod = PgTableSamplingMethod "BERNOULLI"
pgSystemSamplingMethod = PgTableSamplingMethod "SYSTEM"
