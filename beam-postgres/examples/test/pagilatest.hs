{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Pagila.Schema

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres (PgSyntax(..))
import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.Types hiding (migrateScript)
import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Migrate.SQL

import qualified Database.PostgreSQL.Simple as Pg

import qualified Control.Exception as E

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Time.LocalTime (LocalTime)

import Data.Conduit ((.|), runConduitRes)
import qualified Data.Conduit.List as CL (mapM_)
import Database.Beam.Postgres.Conduit (streamingRunSelect)

testQuery :: ( FromBackendRow Postgres (QExprToIdentity res)
             , Projectible Postgres res, Show (QExprToIdentity res)
             ) => Connection
               -> Q Postgres db QBaseScope res
               -> IO ()
testQuery conn q
  = runConduitRes (
         streamingRunSelect conn (select q)
      .| CL.mapM_ (liftIO . putStrLn . show)
   )

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
