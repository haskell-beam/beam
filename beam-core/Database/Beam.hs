{-# LANGUAGE GADTs #-}
-- | Top-level Beam module. This module re-exports all the symbols
--   necessary for most common user operations.
--
--   The most interesting modules are "Database.Beam.Schema" and "Database.Beam.Query".
--
--   This is mainly reference documentation. Most users will want to consult the
--   [manual](https://haskell-beam.github.io/beam).
module Database.Beam
     ( module Database.Beam.Query
     , module Database.Beam.Schema
     , MonadBeam(..)
     , FromBackendRow(..)

       -- * Re-exports
     , MonadIO(..), Typeable
     , Generic, Identity ) where

import Database.Beam.Query
import Database.Beam.Schema
import Database.Beam.Backend

import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO(..))

import Data.Typeable

import GHC.Generics
