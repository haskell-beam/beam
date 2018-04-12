-- | Some functionality is useful enough to be provided across backends, but is
-- not standardized. For example, many RDBMS systems provide ways of fetching
-- auto-incrementing or defaulting fields on INSERT or UPDATE.
--
-- Beam provides type classes that some backends instantiate that provide this
-- support. This uses direct means on sufficiently advanced backends and is
-- emulated on others.
module Database.Beam.Backend.SQL.BeamExtensions
  ( MonadBeamInsertReturning(..)
  , MonadBeamUpdateReturning(..)
  , MonadBeamDeleteReturning(..)

  , SqlSerial(..)
  ) where

import Database.Beam.Backend
import Database.Beam.Query
import Database.Beam.Schema

import Control.Monad.Identity

--import GHC.Generics

-- | 'MonadBeam's that support returning the newly created rows of an @INSERT@ statement.
--   Useful for discovering the real value of a defaulted value.
class MonadBeam be handle m =>
  MonadBeamInsertReturning be handle m | m -> be handle where
  runInsertReturningList
    :: ( Beamable table
       , Projectible be (table (QExpr be ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues be (table (QExpr be s))
    -> m [table Identity]

-- | 'MonadBeam's that support returning the updated rows of an @UPDATE@ statement.
--   Useful for discovering the new values of the updated rows.
class MonadBeam be handle m =>
  MonadBeamUpdateReturning be handle m | m -> be handle where
  runUpdateReturningList
    :: ( Beamable table
       , Projectible be (table (QExpr be ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> (forall s. table (QField s) -> [ QAssignment be s ])
    -> (forall s. table (QExpr be s) -> QExpr be s Bool)
    -> m [table Identity]

-- | 'MonadBeam's that suppert returning rows that will be deleted by the given
-- @DELETE@ statement. Useful for deallocating resources based on the value of
-- deleted rows.
class MonadBeam be handle m =>
  MonadBeamDeleteReturning be handle m | m -> be handle where
  runDeleteReturningList
    :: ( Beamable table
       , Projectible be (table (QExpr be ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> (forall s. table (QExpr be s) -> QExpr be s Bool)
    -> m [table Identity]
