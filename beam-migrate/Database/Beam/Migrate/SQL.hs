-- | Manual alternative to the 'CheckedDatabaseSettings' mechanism.
--
--   Database schemas are given as sequences of DDL commands expressed in a
--   @beam-migrate@ DSL. The 'runMigrationSilenced' function can be used to
--   recover the 'CheckedDatabaseSettings' that represents the database settings
--   as well as the database predicates corresponding to the sequence of DDL
--   commands.
--
--   This is often a more concise way of specifying a database schema when your
--   database names are wildly different from the defaults beam assigns or you
--   multiple constraints that make modifying the auto-generated schema too
--   difficult.

module Database.Beam.Migrate.SQL
  ( module Database.Beam.Migrate.SQL.SQL92
  , module Database.Beam.Migrate.SQL.Tables
  , module Database.Beam.Migrate.SQL.Types ) where

import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Migrate.SQL.Types

