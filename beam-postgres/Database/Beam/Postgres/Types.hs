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
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Database.PostgreSQL.Simple.HStore as Pg (HStoreMap, HStoreList)
import qualified Database.PostgreSQL.Simple.Types as Pg
import qualified Database.PostgreSQL.Simple.Range as Pg (PGRange)
import qualified Database.PostgreSQL.Simple.Time as Pg (Date, UTCTimestamp, ZonedTimestamp, LocalTimestamp)

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import           Data.Int
import           Data.Aeson (Value)
import           Data.UUID (UUID)
import           Data.CaseInsensitive (CI)
import           Data.Ratio (Ratio)
import           Data.Scientific (Scientific)
import           Data.Vector (Vector)
import           Data.Time (UTCTime, Day, TimeOfDay, LocalTime, ZonedTime)

data Postgres
  = Postgres

instance BeamBackend Postgres where
  type BackendFromField Postgres = Pg.FromField

instance Pg.FromField x => Pg.FromField (Auto x) where
  fromField field d = fmap (Auto . Just) (Pg.fromField field d)
instance Pg.ToField x => Pg.ToField (Auto x) where
  toField (Auto Nothing) = Pg.Plain "DEFAULT"
  toField (Auto (Just x)) = Pg.toField x
instance Pg.FromField SqlNull where
  fromField field d = fmap (\Pg.Null -> SqlNull) (Pg.fromField field d)
instance Pg.ToField SqlNull where
  toField _ = Pg.toField Pg.Null

-- Default FromBackendRow instances for all postgresql-simple FromField instances
instance FromBackendRow Postgres SqlNull
instance FromBackendRow Postgres Bool
instance FromBackendRow Postgres Char
instance FromBackendRow Postgres Double
instance FromBackendRow Postgres Int
instance FromBackendRow Postgres Int16
instance FromBackendRow Postgres Int32
instance FromBackendRow Postgres Int64
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
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Vector a)
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
