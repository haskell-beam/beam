{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pagila.Schema
  ( module Pagila.Schema.V0002
  , allMigrationSteps, migrateDB, dbSettings, dbSettings' ) where

import           Database.PostgreSQL.Simple

import Pagila.Schema.V0002 hiding (migration)

import qualified Pagila.Schema.V0001 as V0001
import qualified Pagila.Schema.V0002 as V0002

import Control.Arrow ( (>>>) )

import Test.QuickCheck.Gen (Gen, sample')
import Test.QuickCheck.Arbitrary (arbitrary)

import Database.Beam (DatabaseSettings, liftIO, insert, insertValues, runInsert)
import Database.Beam.Migrate.Types ( CheckedDatabaseSettings, MigrationSteps, unCheckDatabase
                                   , evaluateDatabase, migrationStep)
import Database.Beam.Postgres (Postgres, runBeamPostgresDebug)
import Database.Beam.Migrate.Simple (BringUpToDateHooks, bringUpToDateWithHooks, defaultUpToDateHooks, runIrreversibleHook)
import qualified Database.Beam.Postgres.Migrate as Pg

firstMigrationStep :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V0001.PagilaDb)
firstMigrationStep = migrationStep "Initial commit" V0001.migration

secondMigrationStep :: MigrationSteps Postgres (CheckedDatabaseSettings Postgres V0001.PagilaDb) (CheckedDatabaseSettings Postgres V0002.PagilaDb)
secondMigrationStep = migrationStep "Add film actor, inventory, rental table" V0002.migration

allMigrationSteps :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V0002.PagilaDb)
allMigrationSteps = firstMigrationStep >>> secondMigrationStep

dbSettings :: DatabaseSettings Postgres V0001.PagilaDb
dbSettings = unCheckDatabase (evaluateDatabase firstMigrationStep)

dbSettings' :: DatabaseSettings Postgres V0002.PagilaDb
dbSettings' = unCheckDatabase (evaluateDatabase allMigrationSteps)

allowDestructive :: (MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = return True
    }

{- |
  Run two migrations: V0001 and V0002.
  After V0001 migration, insert randomly generated countries and staff.
  This demonstrates the V0002 migration will not delete that data.
-}
migrateDB :: Connection -> IO (Maybe (CheckedDatabaseSettings Postgres V0002.PagilaDb))
migrateDB conn = runBeamPostgresDebug putStrLn conn $ do

  -- Run migration V0001
  mx :: Maybe (CheckedDatabaseSettings Postgres V0001.PagilaDb) <- bringUpToDateWithHooks allowDestructive Pg.migrationBackend firstMigrationStep

  case mx of
    -- if migration V0001 succeeded, proceed.
    Just (_ :: CheckedDatabaseSettings Postgres V0001.PagilaDb) -> do
      -- generate random countries
      randomCountries :: [V0001.Country] <- liftIO
        . fmap (zipWith (\i country -> country { V0001.countryId = i }) [1..])
        $ sample' (arbitrary :: Gen V0001.Country)
      runInsert $ insert (V0001.country dbSettings) $ insertValues randomCountries

      -- generate random V0001 Staff
      randomStaff :: [V0001.Staff] <- 
        liftIO
        . fmap (zipWith (\i staff -> staff { V0001.staffId = i }) [1..])
        . fmap (fmap (\staffMember -> staffMember { V0001.staffPicture = Nothing } ))  -- overwrite picture with null
        $ sample' (arbitrary :: Gen V0001.Staff)

      runInsert $ insert (V0001.staff dbSettings) $ insertValues randomStaff

      {- Run migrations V0001 (redundantly) and V0002.
         The V0002 migration will add staff `salary` field, among other changes.
         See 'Pagila.Schema.V0002.migrateToNewStaffWithSalary'.
      -}
      bringUpToDateWithHooks allowDestructive Pg.migrationBackend allMigrationSteps
    Nothing ->
      pure Nothing
