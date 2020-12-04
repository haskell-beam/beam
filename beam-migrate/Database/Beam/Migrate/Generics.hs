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

import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.Generics.Types
import Database.Beam.Migrate.Types.CheckedEntities

import Data.Proxy

import GHC.Generics
