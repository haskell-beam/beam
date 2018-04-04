{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Postgres.Types () where

import Data.Typeable
import           Database.Beam.Backend
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
import           Data.UUID (UUID)
import           Data.Vector (Vector)
import           Data.Word

-- instance BeamBackend Postgres where
--   type BackendFromField Postgres = Pg.FromField

instance Pg.FromField SqlNull where
  fromField field d = fmap (\Pg.Null -> SqlNull) (Pg.fromField field d)

fromScientificOrIntegral :: (Bounded a, Integral a) => FromBackendRowM a
fromScientificOrIntegral = do
  sciVal <- fmap (toBoundedInteger =<<) peekField
  case sciVal of
    Just sciVal' -> do
      -- If the parse succeeded, consume the field
      _ <- parseOneField @Scientific
      pure sciVal'
    Nothing -> fromIntegral <$> fromBackendRow @Integer

-- | Deserialize integral fields, possibly downcasting from a larger integral
-- type, but only if we won't lose data
fromPgIntegral :: forall a
                . (Pg.FromField a, Integral a)
               => FromBackendRowM a
fromPgIntegral = do
  val <- peekField
  case val of
    Just val' -> do
      _ <- parseOneField @a
      pure val'
    Nothing -> do
      val' <- parseOneField @Integer
      let val'' = fromIntegral val'
      if fromIntegral val'' == val'
        then pure val''
        else fail (concat [ "Data loss while downsizing Integral type. "
                          , "Make sure your Haskell types are wide enough for your data" ])

-- Default FromBackendRow instances for all postgresql-simple FromField instances
instance FromBackendRow SqlNull
instance FromBackendRow Bool
instance FromBackendRow Char
instance FromBackendRow Double
instance FromBackendRow Int where
  fromBackendRow = fromPgIntegral
instance FromBackendRow Int16 where
  fromBackendRow = fromPgIntegral
instance FromBackendRow Int32 where
  fromBackendRow = fromPgIntegral
instance FromBackendRow Int64 where
  fromBackendRow = fromPgIntegral
-- Word values are serialized as SQL @NUMBER@ types to guarantee full domain coverage.
-- However, we wan them te be serialized/deserialized as whichever type makes sense
instance FromBackendRow Word where
  fromBackendRow = fromScientificOrIntegral
instance FromBackendRow Word16 where
  fromBackendRow = fromScientificOrIntegral
instance FromBackendRow Word32 where
  fromBackendRow = fromScientificOrIntegral
instance FromBackendRow Word64 where
  fromBackendRow = fromScientificOrIntegral
instance FromBackendRow Integer
instance FromBackendRow ByteString
instance FromBackendRow Scientific
instance FromBackendRow BL.ByteString
instance FromBackendRow Text
instance FromBackendRow UTCTime
instance FromBackendRow Value
instance FromBackendRow TL.Text
instance FromBackendRow Pg.Oid
instance FromBackendRow LocalTime where
  fromBackendRow =
    peekField >>=
    \case
      Just (_ :: LocalTime) -> parseOneField

      -- Also accept 'TIMESTAMP WITH TIME ZONE'. Considered as
      -- 'LocalTime', because postgres always returns times in the
      -- server timezone, regardless of type.
      Nothing ->
        peekField >>=
        \case
          Just (_ :: ZonedTime) -> zonedTimeToLocalTime <$> parseOneField
          Nothing -> fail "'TIMESTAMP WITH TIME ZONE' or 'TIMESTAMP WITHOUT TIME ZONE' required for LocalTime"
instance FromBackendRow TimeOfDay
instance FromBackendRow Day
instance FromBackendRow UUID
instance FromBackendRow Pg.Null
instance FromBackendRow Pg.Date
instance FromBackendRow Pg.ZonedTimestamp
instance FromBackendRow Pg.UTCTimestamp
instance FromBackendRow Pg.LocalTimestamp
instance FromBackendRow Pg.HStoreMap
instance FromBackendRow Pg.HStoreList
instance FromBackendRow [Char]
instance FromBackendRow (Ratio Integer)
instance FromBackendRow (CI Text)
instance FromBackendRow (CI TL.Text)
instance (Pg.FromField a, Typeable a) => FromBackendRow (Vector a) where
    fromBackendRow = do
      isNull <- peekField
      case isNull of
        Just SqlNull -> pure mempty
        Nothing -> parseOneField @(Vector a)
instance (Pg.FromField a, Typeable a) => FromBackendRow (Pg.PGArray a)
instance FromBackendRow (Pg.Binary ByteString)
instance FromBackendRow (Pg.Binary BL.ByteString)
instance (Pg.FromField a, Typeable a) => FromBackendRow (Pg.PGRange a)
instance (Pg.FromField a, Pg.FromField b) => FromBackendRow (Either a b)

-- instance BeamSqlBackend Postgres
-- instance BeamSql92Backend Postgres
