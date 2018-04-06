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

--import GHC.Generics

-- | 'MonadBeam's that support returning the newly created rows of an @INSERT@ statement.
--   Useful for discovering the real value of a defaulted value.
class MonadBeam syntax be m =>
  MonadBeamInsertReturning syntax be m | m -> syntax be where
  runInsertReturningList
    :: ( Beamable table
       , Projectible (Sql92ExpressionSyntax syntax) (table (QExpr (Sql92ExpressionSyntax syntax) ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues (Sql92InsertValuesSyntax (Sql92InsertSyntax syntax))
                       (table (QExpr (Sql92InsertExpressionSyntax (Sql92InsertSyntax syntax)) s))
    -> m [table Identity]

-- | 'MonadBeam's that support returning the updated rows of an @UPDATE@ statement.
--   Useful for discovering the new values of the updated rows.
class MonadBeam syntax be m =>
  MonadBeamUpdateReturning syntax be m | m -> syntax be where
  runUpdateReturningList
    :: ( Beamable table
       , Projectible (Sql92ExpressionSyntax syntax) (table (QExpr (Sql92ExpressionSyntax syntax) ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> (forall s. table (QField s) -> [ QAssignment (Sql92UpdateFieldNameSyntax (Sql92UpdateSyntax syntax)) (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax syntax)) s ])
    -> (forall s. table (QExpr (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax syntax)) s) -> QExpr (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax syntax)) s Bool)
    -> m [table Identity]

-- | 'MonadBeam's that suppert returning rows that will be deleted by the given
-- @DELETE@ statement. Useful for deallocating resources based on the value of
-- deleted rows.
class MonadBeam syntax be m =>
  MonadBeamDeleteReturning syntax be m | m -> syntax be where
  runDeleteReturningList
    :: ( Beamable table
       , Projectible (Sql92ExpressionSyntax syntax) (table (QExpr (Sql92ExpressionSyntax syntax) ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> (forall s. table (QExpr (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax syntax)) s) -> QExpr (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax syntax)) s Bool)
    -> m [table Identity]
