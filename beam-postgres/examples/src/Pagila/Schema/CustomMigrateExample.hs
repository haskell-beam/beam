{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module contains a minimal example of how to use
-- a custom data type (``ShippingCarrier`` in this example) in migration
-- Other than the use of ``ShippingCarrier`` as a part of ``AddressT``,
-- this is just a stripped down version of Pagila.Schema.V0001
module Pagila.Schema.CustomMigrateExample where

import Database.Beam
    ( Generic,
      maybeType,
      timestamp,
      varchar,
      FromBackendRow(fromBackendRow),
      DataType(..),
      Beamable,
      Columnar,
      Database,
      Table(..),
      TableEntity,
      Identity )
import Database.Beam.Postgres
    ( smallserial,
      now_,
      PgCommandSyntax,
      Postgres,
      ResultError(ConversionFailed) )
import Database.Beam.Postgres.Syntax
    ( pgTextType, PgColumnSchemaSyntax, PgDataTypeSyntax )
import Database.Beam.Migrate
    ( createTable,
      defaultTo_,
      field,
      notNull,
      TableFieldSchema,
      Migration,
      CheckedDatabaseSettings )
import Database.Beam.Backend.SQL
    ( HasSqlValueSyntax(..),
      BeamSqlBackend,
      SqlSerial,
      autoSqlValueSyntax )
import Database.Beam.Migrate.SQL ()

import Data.Int ( Int32 )
import qualified Data.Text as T
import           Data.Time.LocalTime (LocalTime)
import Database.PostgreSQL.Simple.FromField
    ( returnError, FromField(..) )
import Text.Read ( readMaybe )

data ShippingCarrier = USPS | FedEx | UPS | DHL
  deriving (Show, Read, Eq, Ord, Enum)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax

instance FromField ShippingCarrier where
  fromField f bs = do
    mCarrier <- readMaybe <$> fromField f bs
    case mCarrier of
      Nothing -> returnError ConversionFailed f "Could not 'read' value for 'ShippingCarrier'"
      Just x -> pure x

-- | An explicit definition of ``fromBackendRow`` is required for each custom type
instance (BeamSqlBackend be, FromBackendRow be T.Text) => FromBackendRow be ShippingCarrier where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: T.Text of
      "usps" -> pure USPS
      "fedex" -> pure FedEx
      "ups"  -> pure UPS
      "dhl"  -> pure DHL
      _ -> fail ("Invalid value for ShippingCarrier: " ++ T.unpack val)

-- | The shipping carrier type that is used in migration
-- In this case, we want to store it as the postgres type ``text``
-- Look into the module Database.Beam.Postgres.Syntax for a list of other
-- postgres types that your custom type can take
shippingCarrierType :: DataType Postgres a
shippingCarrierType = DataType pgTextType


-- | Address table
data AddressT f
  = AddressT
  { addressId         :: Columnar f (SqlSerial Int32)
  , addressAddress1   :: Columnar f T.Text
  , addressAddress2   :: Columnar f (Maybe T.Text)
  , addressDistrict   :: Columnar f T.Text
  , addressShipper    :: Columnar f ShippingCarrier
  , addressPostalCode :: Columnar f T.Text
  , addressPhone      :: Columnar f T.Text
  , addressLastUpdate :: Columnar f LocalTime
  } deriving Generic
type Address = AddressT Identity
deriving instance Show Address
deriving instance Eq Address

instance Table AddressT where
  data PrimaryKey AddressT f = AddressId (Columnar f (SqlSerial Int32)) deriving Generic
  primaryKey = AddressId . addressId
type AddressId = PrimaryKey AddressT Identity
deriving instance Show AddressId
deriving instance Eq AddressId


-- | Pagila db
data PagilaDb f
  = PagilaDb
  {
    address    :: f (TableEntity AddressT)
  } deriving Generic
instance Database Postgres PagilaDb

-- Beamable instances

instance Beamable (PrimaryKey AddressT)
instance Beamable AddressT

-- lastUpdateField :: TableFieldSchema PgColumnSchemaSyntax LocalTime
lastUpdateField :: TableFieldSchema Postgres LocalTime
lastUpdateField = field "last_update" timestamp  (defaultTo_ now_) notNull

-- migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres PagilaDb)
migration () = do
--  year_ <- createDomain "year" integer (check (\yr -> yr >=. 1901 &&. yr <=. 2155))
  PagilaDb <$> createTable "address"
                 (AddressT
                    (field "address_id" smallserial)
                    (field "address" (varchar (Just 50)) notNull)
                    (field "address2" (maybeType $ varchar (Just 50)))
                    (field "district" (varchar (Just 20)) notNull)
                    (field "shipper" shippingCarrierType)
                    (field "postal_code" (varchar (Just 10)))
                    (field "phone" (varchar (Just 20)) notNull) lastUpdateField)
