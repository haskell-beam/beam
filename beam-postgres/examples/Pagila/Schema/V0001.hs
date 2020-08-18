{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Pagila.Schema.V0001 where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres (PgSyntax(..))
import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.Types hiding (migrateScript)
import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Backend.SQL.Types (SqlSerial)
import qualified Database.PostgreSQL.Simple as Pg

import qualified Control.Exception as E

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Time.LocalTime (LocalTime)
import Data.Scientific (Scientific)

-- Address table

data AddressT f
  = AddressT
  { addressId         :: Columnar f (SqlSerial Int32)
  , addressAddress1   :: Columnar f Text
  , addressAddress2   :: Columnar f (Maybe Text)
  , addressDistrict   :: Columnar f Text
  , addressCity       :: PrimaryKey CityT f
  , addressPostalCode :: Columnar f Text
  , addressPhone      :: Columnar f Text
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

-- City table

data CityT f
  = CityT
  { cityId         :: Columnar f Int32
  , cityName       :: Columnar f Text
  , cityCountryId  :: PrimaryKey CountryT f
  , cityLastUpdate :: Columnar f LocalTime
  } deriving Generic
type City = CityT Identity
deriving instance Show City
deriving instance Eq City

instance Table CityT where
  data PrimaryKey CityT f = CityId (Columnar f Int32) deriving Generic
  primaryKey = CityId . cityId
type CityId = PrimaryKey CityT Identity
deriving instance Show CityId
deriving instance Eq CityId

-- Country table

data CountryT f
  = CountryT
  { countryId          :: Columnar f Int32
  , countryName        :: Columnar f Text
  , countryLastUpdated :: Columnar f LocalTime
  } deriving Generic
type Country = CountryT Identity
deriving instance Show Country
deriving instance Eq Country

instance Table CountryT where
  data PrimaryKey CountryT f = CountryId (Columnar f Int32) deriving Generic
  primaryKey = CountryId . countryId
type CountryId = PrimaryKey CountryT Identity
deriving instance Show CountryId
deriving instance Eq CountryId

-- Actor

data ActorT f
  = ActorT
  { actorId :: Columnar f (SqlSerial Int32)
  , actorFirstName :: Columnar f Text
  , actorLastName :: Columnar f Text
  , actorLastUpdate :: Columnar f LocalTime
  } deriving Generic
type Actor = ActorT Identity
deriving instance Show Actor; deriving instance Eq Actor

instance Table ActorT where
  data PrimaryKey ActorT f = ActorId (Columnar f (SqlSerial Int23))
                             deriving Generic
  primaryKey = ActorId . actorId
type ActorId = PrimaryKey ActorT Identity
deriving instance Show ActorId; deriving instance Eq ActorId

-- Category

data CategoryT f
  = CategoryT
  { categoryId :: Columnar f Int32
  , categoryName :: Columnar f Text
  , categoryLastUpdate :: Columnar f LocalTime
  } deriving Generic
type Category = CategoryT Identity
deriving instance Show Category; deriving instance Eq Category

instance Table CategoryT where
  data PrimaryKey CategoryT f = CategoryId (Columnar f Int32) deriving Generic
  primaryKey = CategoryId . categoryId
type CategoryId = PrimaryKey CategoryT Identity
deriving instance Show CategoryId; deriving instance Eq CategoryId

-- Customer

data CustomerT f
  = CustomerT
  { customerId        :: Columnar f (SqlSerial Int32)
  , customerStore     :: PrimaryKey StoreT f
  , customerFirstName :: Columnar f Text
  , customerLastName  :: Columnar f Text
  , customerEmail     :: Columnar f Text
  , customerAddress   :: PrimaryKey AddressT f
  , customerActive    :: Columnar f Bool
  , customerCreateDate :: Columnar f LocalTime
  , customerLastUpdate :: Columnar f LocalTime
  } deriving Generic
type Customer = CustomerT Identity
deriving instance Show Customer; deriving instance Eq Customer

instance Table CustomerT where
  data PrimaryKey CustomerT f = CustomerId (Columnar f (SqlSerial Int32))
                                deriving Generic
  primaryKey = CustomerId . customerId
type CustomerId = PrimaryKey CustomerT Identity
deriving instance Show CustomerId; deriving instance Eq CustomerId

-- Store

data StoreT f
  = StoreT
  { storeId      :: Columnar f Int32
  , storeManager :: PrimaryKey StaffT f
  , storeAddress :: PrimaryKey AddressT f
  , lastUpdate   :: Columnar f LocalTime
  } deriving Generic
type Store = StoreT Identity
deriving instance Show Store; deriving instance Eq Store

instance Table StoreT where
  data PrimaryKey StoreT f = StoreId (Columnar f Int32) deriving Generic
  primaryKey = StoreId . storeId
type StoreId = PrimaryKey StoreT Identity
deriving instance Show StoreId; deriving instance Eq StoreId

-- Staff

data StaffT f
  = StaffT
  { staffId        :: Columnar f Int32
  , staffFirstName :: Columnar f Text
  , staffLastName  :: Columnar f Text
  , staffAddress   :: PrimaryKey AddressT f
  , staffEmail     :: Columnar f Text
  , staffStore     :: PrimaryKey StoreT f
  , staffActive    :: Columnar f Bool
  , staffUsername  :: Columnar f Text
  , staffPassword  :: Columnar f ByteString
  , staffLastUpdate :: Columnar f LocalTime
  , staffPicture   :: Columnar f (Maybe ByteString)
  } deriving Generic
type Staff = StaffT Identity
deriving instance Eq Staff; deriving instance Show Staff

instance Table StaffT where
  data PrimaryKey StaffT f = StaffId (Columnar f Int32) deriving Generic
  primaryKey = StaffId . staffId
type StaffId = PrimaryKey StaffT Identity
deriving instance Eq StaffId; deriving instance Show StaffId

-- Film

data FilmT f
  = FilmT
  { filmId             :: Columnar f (SqlSerial Int32)
  , filmTitle          :: Columnar f Text
  , filmDescription    :: Columnar f Text
  , filmReleaseYear    :: Columnar f Int32
  , filmLanguage       :: PrimaryKey LanguageT f
  , filmOriginalLanguage :: PrimaryKey LanguageT f
  , filmRentalDuration :: Columnar f Int32
  , filmRentalRate     :: Columnar f Scientific
  , filmLength         :: Columnar f Int32
  , filmReplacementCost :: Columnar f Scientific
  , filmRating         :: Columnar f Text
  , filmLastUpdate     :: Columnar f LocalTime
  } deriving Generic
type Film = FilmT Identity
deriving instance Eq Film; deriving instance Show Film

instance Table FilmT where
  data PrimaryKey FilmT f = FilmId (Columnar f (SqlSerial Int32))
                            deriving Generic
  primaryKey = FilmId . filmId
type FilmId = PrimaryKey FilmT Identity
deriving instance Eq FilmId; deriving instance Show FilmId

-- Film category

data FilmCategoryT f
  = FilmCategoryT
  { filmCategoryFilm       :: PrimaryKey FilmT f
  , filmCategoryCategory   :: PrimaryKey CategoryT f
  , filmCategoryLastUpdate :: Columnar f LocalTime
  } deriving Generic
type FilmCategory = FilmCategoryT Identity
deriving instance Eq FilmCategory; deriving instance Show FilmCategory

instance Table FilmCategoryT where
  data PrimaryKey FilmCategoryT f = FilmCategoryId (PrimaryKey CategoryT f) (PrimaryKey FilmT f)
    deriving Generic
  primaryKey = FilmCategoryId <$> filmCategoryCategory <*> filmCategoryFilm
type FilmCategoryId = PrimaryKey FilmCategoryT Identity
deriving instance Eq FilmCategoryId; deriving instance Show FilmCategoryId

-- Language

data LanguageT f
  = LanguageT
  { languageId   :: Columnar f (SqlSerial Int32)
  , languageName :: Columnar f Text
  , languageLastUpdate :: Columnar f LocalTime
  }  deriving Generic
type Language = LanguageT Identity
deriving instance Eq Language; deriving instance Show Language

instance Table LanguageT where
  data PrimaryKey LanguageT f = LanguageId (Columnar f (SqlSerial Int32))
                                deriving Generic
  primaryKey = LanguageId . languageId
type LanguageId = PrimaryKey LanguageT Identity
deriving instance Eq LanguageId; deriving instance Show LanguageId

-- Pagila db

data PagilaDb f
  = PagilaDb
  { actor      :: f (TableEntity ActorT)
  , address    :: f (TableEntity AddressT)
  , city       :: f (TableEntity CityT)
  , country    :: f (TableEntity CountryT)
  , category   :: f (TableEntity CategoryT)
  , customer   :: f (TableEntity CustomerT)
  , film       :: f (TableEntity FilmT)
  , filmCategory :: f (TableEntity FilmCategoryT)
  , language   :: f (TableEntity LanguageT)
  , store      :: f (TableEntity StoreT)
  , staff      :: f (TableEntity StaffT)
  } deriving Generic
instance Database Postgres PagilaDb

-- Beamable instances

instance Beamable (PrimaryKey AddressT)
instance Beamable AddressT
instance Beamable (PrimaryKey CityT)
instance Beamable CityT
instance Beamable (PrimaryKey CountryT)
instance Beamable CountryT
instance Beamable (PrimaryKey ActorT)
instance Beamable ActorT
instance Beamable (PrimaryKey CategoryT)
instance Beamable CategoryT
instance Beamable (PrimaryKey CustomerT)
instance Beamable CustomerT
instance Beamable (PrimaryKey StoreT)
instance Beamable StoreT
instance Beamable (PrimaryKey StaffT)
instance Beamable StaffT
instance Beamable (PrimaryKey FilmT)
instance Beamable FilmT
instance Beamable (PrimaryKey FilmCategoryT)
instance Beamable FilmCategoryT
instance Beamable (PrimaryKey LanguageT)
instance Beamable LanguageT

lastUpdateField :: TableFieldSchema PgColumnSchemaSyntax LocalTime
lastUpdateField = field "last_update" timestamp (defaultTo_ now_) notNull

migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres PagilaDb)
migration () = do
--  year_ <- createDomain "year" integer (check (\yr -> yr >=. 1901 &&. yr <=. 2155))
  PagilaDb <$> createTable "actor"
                 (ActorT (field "actor_id" serial)
                         (field "first_name" (varchar (Just 45)) notNull)
                         (field "last_name" (varchar (Just 45)) notNull unique)
                         lastUpdateField)
           <*> createTable "address"
                 (AddressT (field "address_id" smallserial)
                           (field "address" (varchar (Just 50)) notNull)
                           (field "address2" (maybeType $ varchar (Just 50)))
                           (field "district" (varchar (Just 20)) notNull)
                           (CityId (field "city_id" smallint notNull))
                           (field "postal_code" (varchar (Just 10)))
                           (field "phone" (varchar (Just 20)) notNull) lastUpdateField)
           <*> createTable "city"
                 (CityT (field "city_id" smallint)
                        (field "city" (varchar (Just 50)) notNull)
                        (CountryId (field "country_id" smallint notNull))
                        lastUpdateField)
           <*> createTable "country"
                 (CountryT (field "country_id" smallint)
                           (field "country" (varchar (Just 50)) notNull)
                           lastUpdateField)
           <*> createTable "category"
                 (CategoryT (field "category_id" smallint)
                            (field "name" (varchar (Just 25)) notNull)
                            lastUpdateField)
           <*> createTable "customer"
                 (CustomerT (field "customer_id" serial)
                            (StoreId (field "store_id" smallint notNull))
                            (field "first_name" (varchar (Just 45)) notNull)
                            (field "last_name" (varchar (Just 45)) notNull)
                            (field "email" (varchar (Just 50)))
                            (AddressId (field "address_id" serial notNull))
                            (field "activebool" boolean (defaultTo_ (val_ True)) notNull)
                            (field "create_date" date (defaultTo_ now_) notNull)
                            lastUpdateField)
           <*> createTable "film"
                 (FilmT     (field "film_id" smallserial)
                            (field "title" (varchar (Just 255)) notNull)
                            (field "description" text)
                            (field "release_year" smallint {- TODO year -})
                            (LanguageId (field "language_id" smallserial notNull))
                            (LanguageId (field "original_language_id" smallserial))
                            (field "rental_duration" smallint notNull)
                            (field "rental_rate" (numeric (Just (4, Just 2))))
                            (field "length" smallint)
                            (field "replacement_cost" (numeric (Just (5, Just 2))))
                            (field "rating_text" text)
                            lastUpdateField)
           <*> createTable "film_category"
                 (FilmCategoryT (FilmId (field "film_id" smallserial notNull))
                                (CategoryId (field "category_id" smallint))
                                lastUpdateField)
           <*> createTable "language"
                 (LanguageT (field "language_id" smallserial)
                            (field "name" (char (Just 20)) notNull)
                            lastUpdateField)
           <*> createTable "store"
                 (StoreT (field "store_id" smallint)
                         (StaffId (field "manager_staff_id" smallint notNull))
                         (AddressId (field "address_id" serial notNull))
                         lastUpdateField)
           <*> createTable "staff"
                 (StaffT (field "staff_id" smallint)
                         (field "first_name" (varchar (Just 45)) notNull)
                         (field "last_name" (varchar (Just 45)) notNull)
                         (AddressId (field "address_id" serial notNull))
                         (field "email" (varchar (Just 50)))
                         (StoreId (field "store_id" smallint notNull))
                         (field "active" boolean (defaultTo_ (val_ True)) notNull)
                         (field "username" (varchar (Just 16)) notNull)
                         (field "password" binaryLargeObject)
                         lastUpdateField
                         (field "picture" (maybeType bytea)))
