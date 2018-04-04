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
import Database.Beam.Backend.SQL
import Database.Beam.Query
import Database.Beam.Query.Internal
import Database.Beam.Schema

import Control.Monad.Identity

import GHC.Generics

-- | 'MonadBeam's that support returning the newly created rows of an @INSERT@ statement.
--   Useful for discovering the real value of a defaulted value.
class MonadBeam handle m =>
  MonadBeamInsertReturning handle m | m -> handle, handle -> m where
  runInsertReturningList
    :: ( Beamable table
       , Projectible (table (QExpr ()))
       , FromBackendRow (table Identity) )
    => DatabaseEntity db (TableEntity table)
    -> SqlInsertValues (table (QExpr s))
    -> m [table Identity]

-- | 'MonadBeam's that support returning the updated rows of an @UPDATE@ statement.
--   Useful for discovering the new values of the updated rows.
class MonadBeam handle m =>
  MonadBeamUpdateReturning handle m | m -> handle, handle -> m where
  runUpdateReturningList
    :: ( Beamable table
       , Projectible (table (QExpr ()))
       , FromBackendRow (table Identity) )
    => DatabaseEntity db (TableEntity table)
    -> (forall s. table (QField s) -> [ QAssignment s ])
    -> (forall s. table (QExpr s) -> QExpr s Bool)
    -> m [table Identity]

-- | 'MonadBeam's that suppert returning rows that will be deleted by the given
-- @DELETE@ statement. Useful for deallocating resources based on the value of
-- deleted rows.
class MonadBeam handle m =>
  MonadBeamDeleteReturning handle m | m -> handle, handle -> m where
  runDeleteReturningList
    :: ( Beamable table
       , Projectible (table (QExpr ()))
       , FromBackendRow (table Identity) )
    => DatabaseEntity db (TableEntity table)
    -> (forall s. table (QExpr s) -> QExpr s Bool)
    -> m [table Identity]
