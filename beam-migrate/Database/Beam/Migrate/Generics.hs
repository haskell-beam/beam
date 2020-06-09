{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Support for creating checked databases from Haskell ADTs, using 'Generic's.
--
-- For more information, see
-- <https://haskell-beam.github.io/beam/schema-guide/migrations/ the manual>

module Database.Beam.Migrate.Generics
 ( -- * Default checked database settings
   defaultMigratableDbSettings

 -- * Extending the defaulting sytem
 , HasDefaultSqlDataType(..)
 , HasNullableConstraint, NullableStatus
 ) where

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.Generics.Types

import Data.Proxy

import GHC.Generics

-- | Produce a checked database for the given Haskell database type
--
-- See <https://haskell-beam.github.io/beam/schema-guide/migrations/ the manual>
-- for more information on the defaults.
defaultMigratableDbSettings
  :: forall be db.
   ( Generic (CheckedDatabaseSettings be db)
   , GAutoMigratableDb be (Rep (CheckedDatabaseSettings be db)) )
  => CheckedDatabaseSettings be db
defaultMigratableDbSettings =
  to (defaultMigratableDbSettings' (Proxy @be) :: Rep (CheckedDatabaseSettings be db) ())

