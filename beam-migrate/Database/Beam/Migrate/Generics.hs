{-# LANGUAGE AllowAmbiguousTypes #-}
module Database.Beam.Migrate.Generics where

import Database.Beam.Migrate.Types
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
