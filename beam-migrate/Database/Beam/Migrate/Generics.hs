{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Support for creating checked databases from Haskell ADTs, using 'Generic's.
--
-- For more information, see
-- <http://tathougies.github.io/beam/schema-guide/migrations/ the manual>

module Database.Beam.Migrate.Generics
 ( -- * Default checked database settings
   defaultMigratableDbSettings
 , withDbIndices

 -- * Extending the defaulting sytem
 , HasDefaultSqlDataType(..)
 , HasNullableConstraint, NullableStatus
 ) where

import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.Generics.Types
import Database.Beam.Migrate.Types
import Database.Beam.Schema.Indices
import Database.Beam.Schema.Tables

import Data.Functor.Identity
import Data.Proxy

import GHC.Generics

-- | Produce a checked database for the given Haskell database type
--
-- See <http://tathougies.github.io/beam/schema-guide/migrations/ the manual>
-- for more information on the defaults.
defaultMigratableDbSettings
  :: forall be db.
   ( Generic (CheckedDatabaseSettings be db)
   , GAutoMigratableDb be (Rep (CheckedDatabaseSettings be db)) )
  => CheckedDatabaseSettings be db
defaultMigratableDbSettings =
  to (defaultMigratableDbSettings' (Proxy @be) :: Rep (CheckedDatabaseSettings be db) ())

-- | Attach checks which require the checked database to contain the given
-- indices.
--
-- This function nicely composes with 'withDbModification' when used
-- in its infix form:
--
-- @
-- defaultMigratableDbSettings
--     `withDbModification` dbModification{ ... }
--     `withDbIndices` dbIndices{ ... }
-- @
withDbIndices
    :: forall be db.
       Database be db
    => CheckedDatabaseSettings be db
    -> DatabaseIndices be db
    -> CheckedDatabaseSettings be db
withDbIndices checkedDbSettings indices =
    runIdentity $
    zipTables (Proxy @be)
        (\(CheckedDatabaseEntity dbSettings dbPredicates) indexEntity ->
          pure $ CheckedDatabaseEntity (addIndexChecks dbSettings indexEntity) dbPredicates
        )
        checkedDbSettings indices
