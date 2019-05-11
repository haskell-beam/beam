{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Support for creating checked databases from Haskell ADTs, using 'Generic's.
--
-- For more information, see
-- <http://tathougies.github.io/beam/schema-guide/migrations/ the manual>

module Database.Beam.Migrate.Generics
 ( -- * Default checked database settings
   defaultMigratableDbSettings

 -- * Extending the defaulting sytem
 , HasDefaultSqlDataType(..)
 , HasNullableConstraint, NullableStatus
 ) where

import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.Generics.Types
