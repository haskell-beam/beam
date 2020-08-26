{-# LANGUAGE AllowAmbiguousTypes #-}
module Pagila.Test where

import Control.Arrow

import Data.Int
import Data.Proxy
import Data.Text(Text)

import Database.Beam
import Database.Beam.Backend.Types
import Database.Beam.Migrate.Types ( CheckedDatabaseSettings, MigrationSteps, unCheckDatabase
                                   , evaluateDatabase, migrationStep)
import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Postgres (Postgres, PgCommandSyntax)
import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.Types hiding (migrateScript)
import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Migrate.SQL.Types

data SimpleTbl f
  = SimpleTbl
  { simpletblField1 :: Columnar f Int32
  , simpletblField2 :: Columnar f (Maybe Text) }
  deriving Generic
instance Beamable SimpleTbl

instance Table SimpleTbl where
  data PrimaryKey SimpleTbl f = SimpleTblId (Columnar f Int32) deriving Generic
  primaryKey = SimpleTblId <$> simpletblField1
instance Beamable (PrimaryKey SimpleTbl)

data MyDb f =
  MyDb { mydbSimpleTbl :: f (TableEntity SimpleTbl) } deriving Generic
instance Database be MyDb

myDbMigratable :: forall syntax be
                . IsSql92DdlCommandSyntax syntax
               => CheckedDatabaseSettings be MyDb
myDbMigratable = defaultMigratableDbSettings @syntax
