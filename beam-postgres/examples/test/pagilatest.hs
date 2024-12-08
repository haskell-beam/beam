{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Pagila.Schema

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres (PgSyntax(..))
import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.Types hiding (migrateScript)
import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Migrate.SQL.Types

import qualified Database.PostgreSQL.Simple as Pg

import qualified Control.Exception as E

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Time.LocalTime (LocalTime)

import Data.Conduit ((=$=), runConduit)
import qualified Data.Conduit.List as CL (mapM_)

testQuery conn q = runConduit (runSelect conn (select q) =$= CL.mapM_ (putStrLn . show))

main :: IO ()
main = pure ()
  -- E.bracket (Pg.connectPostgreSQL "dbname=pagila") Pg.close $ \conn ->
  -- do let q = do AddressT { .. } <- all_ (addresses db)
  --               CityT { .. } <- related_ (cities db) addressCity
  --               CountryT { .. } <- related_ (countries db) cityCountryId
  --               guard_ (isJust_ addressAddress2)
  --               pure (addressAddress1, addressDistrict, cityName, addressPostalCode, countryName, addressPhone, addressLastUpdate)
  --    runConduit (runSelect conn (select q) =$= CL.mapM_ (putStrLn . show))

  --    let q = do store@StoreT { .. } <- all_ (stores db)
  --               manager <- related_ (staff db) storeManager
  --               pure (store, manager)
  --    runConduit (runSelect conn (select q) =$= CL.mapM_ (putStrLn . show))

  --    BL.putStrLn . BL.concat . migrateScript $ migration
