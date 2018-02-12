{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pagila.Schema.CustomMigrateExample where

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Migrate
import           Database.Beam.Postgres.Migrate
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.SQL.Types (TableFieldSchema(..), DataType(..))
import           Database.Beam.Backend.SQL.Types (SqlSerial)

import qualified Data.Text as T
import           Data.Time.LocalTime (LocalTime)
import           Database.PostgreSQL.Simple.FromField
import           Text.Read

data ShippingCarrier = USPS | FedEx | UPS | DHL
  deriving (Show, Read, Eq, Ord, Enum)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax
instance (IsSql92ColumnSchemaSyntax be) => HasDefaultSqlDataTypeConstraints be ShippingCarrier

instance FromField ShippingCarrier where
  fromField f bs = do
    x <- readMaybe <$> fromField f bs
    case x of
      Nothing -> returnError ConversionFailed f "Could not 'read' value for 'ShippingCarrier'"
      Just x -> pure x

instance (BeamBackend be) => FromBackendRow be ShippingCarrier

shippingCarrierType :: DataType PgDataTypeSyntax ShippingCarrier
shippingCarrierType = DataType pgTextType

-- Address table

data AddressT f
  = AddressT
  { addressId         :: Columnar f (SqlSerial Int)
  , addressAddress1   :: Columnar f Text
  , addressAddress2   :: Columnar f (Maybe Text)
  , addressDistrict   :: Columnar f Text
  , addressShipper    :: C f ShippingCarrier
  , addressPostalCode :: Columnar f Text
  , addressPhone      :: Columnar f Text
  , addressLastUpdate :: Columnar f LocalTime
  } deriving Generic
type Address = AddressT Identity
deriving instance Show Address
deriving instance Eq Address

instance Table AddressT where
  data PrimaryKey AddressT f = AddressId (Columnar f (SqlSerial Int)) deriving Generic
  primaryKey = AddressId . addressId
type AddressId = PrimaryKey AddressT Identity
deriving instance Show AddressId
deriving instance Eq AddressId

-- Pagila db

data PagilaDb f
  = PagilaDb
  {
    address    :: f (TableEntity AddressT)
  } deriving Generic
instance Database PagilaDb

-- Beamable instances

instance Beamable (PrimaryKey AddressT)
instance Beamable AddressT

lastUpdateField :: TableFieldSchema PgColumnSchemaSyntax LocalTime
lastUpdateField = field "last_update" timestamp (defaultTo_ now_) notNull

migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres PagilaDb)
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
