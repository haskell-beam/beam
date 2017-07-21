module Database.Beam.Backend.SQL.BeamExtensions where

import Database.Beam.Backend
import Database.Beam.Backend.SQL
import Database.Beam.Query
import Database.Beam.Schema

import Control.Monad.Identity

--import GHC.Generics

-- | 'MonadBeam's that support returning the results of an insert statement.
--   Useful for discovering the real value of a defaulted value.
--
--   Unfortunately, SQL has no standard way of doing this, so it is provided as
--   a beam extension.
class MonadBeam syntax be handle m =>
  MonadBeamInsertReturning syntax be handle m | m -> syntax be handle, be -> m, handle -> m where
  runInsertReturningList
    :: ( Beamable table
       , Projectible (Sql92ExpressionSyntax syntax) (table (QExpr (Sql92ExpressionSyntax syntax) ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues (Sql92InsertValuesSyntax (Sql92InsertSyntax syntax)) table
    -> m [table Identity]
