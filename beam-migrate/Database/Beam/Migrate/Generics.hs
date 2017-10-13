{-# LANGUAGE AllowAmbiguousTypes #-}
module Database.Beam.Migrate.Generics
 ( module Database.Beam.Migrate.Generics.Tables
 , module Database.Beam.Migrate.Generics.Types

 , defaultMigratableDbSettings ) where

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.Generics.Types

import Data.Proxy

import GHC.Generics

defaultMigratableDbSettings
  :: forall syntax be db.
   ( Generic (CheckedDatabaseSettings be db)
   , GAutoMigratableDb syntax (Rep (CheckedDatabaseSettings be db)) )
  => CheckedDatabaseSettings be db
defaultMigratableDbSettings =
  to (defaultMigratableDbSettings' (Proxy @syntax) :: Rep (CheckedDatabaseSettings be db) ())

