{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Support for creating checked databases from Haskell ADTs, using 'Generic's.
--
-- For more information, see
-- <http://tathougies.github.io/beam/schema-guide/migrations/ the manual>

module Database.Beam.Migrate.Generics where
--  ( -- * Default checked database settings
--    defaultMigratableDbSettings

--  -- * Extending the defaulting sytem
--  , HasDefaultSqlDataType(..), HasDefaultSqlDataTypeConstraints(..)
--  , Sql92HasDefaultDataType
--  ) where

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.Generics.Types

import Data.Proxy

import GHC.Generics

-- -- | Produce a checked database for the given Haskell database type
-- --
-- -- See <http://tathougies.github.io/beam/schema-guide/migrations/ the manual>
-- -- for more information on the defaults.
-- defaultMigratableDbSettings
--   :: forall syntax be db.
--    ( Generic (CheckedDatabaseSettings be db)
--    , GAutoMigratableDb syntax (Rep (CheckedDatabaseSettings be db)) )
--   => CheckedDatabaseSettings be db
-- defaultMigratableDbSettings =
--   to (defaultMigratableDbSettings' (Proxy @syntax) :: Rep (CheckedDatabaseSettings be db) ())

