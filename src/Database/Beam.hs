{-# LANGUAGE GADTs #-}
-- | Top-level Beam module. This module re-exports all the symbols
--   necessary for most common user operations.
--
--   The most interesting modules are 'Database.Beam.Schema' and 'Database.Beam.Query'.
--
--   'Database.Beam.SQL' contains an internal representation of SQL
--   that can be easily converted to textual
--   SQL. 'Database.Beam.Backend' offers functions to run Beam queries
--   and data manipulation commands within specific SQL backends.
--
--   For a series of tutorials, see my series of blog posts at
--
--   * [Beam tutorial (part 1)](http://travis.athougies.net/posts/2016-01-21-beam-tutorial-1.html)
--   * [Beam tutorial (part 2)](http://travis.athougies.net/posts/2016-01-22-beam-tutorial-part-2.html)
--   * [Beam tutorial (part 3)](http://travis.athougies.net/posts/2016-01-25-beam-tutorial-part-3.html)
module Database.Beam
     ( module Database.Beam.Query
     , module Database.Beam.Schema
     , module Database.Beam.Backend

     , Typeable, Generic, Identity

     , liftIO

     , Beam, BeamBackend, BeamT, BeamResult(..), BeamRollbackReason(..) ) where

import Database.Beam.Internal
import Database.Beam.SQL
import Database.Beam.Query
import Database.Beam.Schema
import Database.Beam.Backend

import Control.Monad.Trans
import Control.Monad.Identity

import Data.Typeable
import Data.Conduit

import GHC.Generics
