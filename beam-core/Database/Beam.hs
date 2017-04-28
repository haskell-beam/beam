{-# LANGUAGE GADTs #-}
-- | Top-level Beam module. This module re-exports all the symbols
--   necessary for most common user operations.
--
--   The most interesting modules are 'Database.Beam.Schema' and 'Database.Beam.Query'.
--
--   This is mainly reference documentation. Most users will want to consult the
--   [manual](https://tathougies.github.io/beam).
--
--   Older versions of beam were accompanied by a series of tutorials, still available at
--
--   * [Beam tutorial (part 1)](http://travis.athougies.net/posts/2016-01-21-beam-tutorial-1.html)
--   * [Beam tutorial (part 2)](http://travis.athougies.net/posts/2016-01-22-beam-tutorial-part-2.html)
--   * [Beam tutorial (part 3)](http://travis.athougies.net/posts/2016-01-25-beam-tutorial-part-3.html)
--
--   The API has mostly stayed the same, but not all the examples given there compile.
module Database.Beam
     ( module Database.Beam.Query
     , module Database.Beam.Schema

       -- * Re-exports for convenience
     , Typeable, Generic, Identity ) where

import Database.Beam.Query
import Database.Beam.Schema

import Control.Monad.Identity

import Data.Typeable

import GHC.Generics
