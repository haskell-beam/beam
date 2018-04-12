{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Backend.SQL
  ( module Database.Beam.Backend.SQL.SQL2003
  , module Database.Beam.Backend.SQL.Types

  , MonadBeam(..)

  , BeamSqlBackend(..)
  , MockSqlBackend

  , BeamSqlBackendIsString

  , BeamSql99ExpressionBackend
  , BeamSql99AggregationBackend
  , BeamSql99ConcatExpressionBackend
  , BeamSql2003ExpressionBackend

  , BeamSqlT611Backend
  , BeamSqlT612Backend
  , BeamSqlT614Backend
  , BeamSqlT615Backend
  , BeamSqlT616Backend
  , BeamSqlT618Backend
  , BeamSqlT621Backend

  , BeamSqlBackendSupportsOuterJoin

  , BeamSqlBackendSelectSyntax
  , BeamSqlBackendInsertSyntax
  , BeamSqlBackendInsertValuesSyntax
  , BeamSqlBackendUpdateSyntax
  , BeamSqlBackendDeleteSyntax
  , BeamSqlBackendSelectTableSyntax
  , BeamSqlBackendAggregationQuantifierSyntax
  , BeamSqlBackendSetQuantifierSyntax
  , BeamSqlBackendFromSyntax

  , BeamSqlBackendExpressionSyntax
  , BeamSqlBackendFieldNameSyntax
  , BeamSqlBackendExpressionQuantifierSyntax
  , BeamSqlBackendValueSyntax
  , BeamSqlBackendOrderingSyntax
  , BeamSqlBackendGroupingSyntax

  , BeamSqlBackendWindowFrameSyntax
  , BeamSqlBackendWindowFrameBoundsSyntax
  , BeamSqlBackendWindowFrameBoundSyntax

  , BeamSqlBackendCanSerialize
  , BeamSqlBackendCanDeserialize
  , BeamSqlBackendSupportsDataType
  ) where

import Database.Beam.Backend.SQL.SQL2003
import Database.Beam.Backend.SQL.Types
import Database.Beam.Backend.Types

import Control.Monad.IO.Class

import Data.Tagged (Tagged)
import Data.Text (Text)

-- * MonadBeam class

-- | A class that ties together a monad with a particular backend
--
--   Intuitively, this allows you to write code that performs database commands
--   without having to know the underlying API. As long as you have an
--   appropriate handle from a database library that Beam can use, you can use
--   the 'MonadBeam' methods to execute the query.
--
--   Provided here is a low-level interface. Most often, you'll only need the
--   'withDatabase' and 'withDatabaseDebug' function. The 'run*' functions are
--   wrapped by the appropriate functions in "Database.Beam.Query".
--
--   This interface is very high-level and isn't meant to expose the full power
--   of the underlying database. Namely, it only supports simple data retrieval
--   strategies. More complicated strategies (for example, Postgres's @COPY@)
--   are supported in individual backends. See the documentation of those
--   backends for more details.
class (BeamBackend be, MonadIO m) =>
  MonadBeam be handle m | m -> be handle where

  {-# MINIMAL withDatabaseDebug, runReturningMany #-}

  -- | Run a database action, and log debugging information about statements
  --   executed using the specified 'IO' action.
  withDatabaseDebug :: (String -> IO ()) -- ^ Database statement logging function
                    -> handle            -- ^ The database connection handle against which to execute the action
                    -> m a               -- ^ The database action
                    -> IO a
  withDatabase :: handle -> m a -> IO a

  -- | Run a database action, but don't report any debug information
  withDatabase = withDatabaseDebug (\_ -> pure ())

  -- | Run a query determined by the given syntax, providing an action that will
  --   be called to consume the results from the database (if any). The action
  --   will get a reader action that can be used to fetch the next row. When
  --   this reader action returns 'Nothing', there are no rows left to consume.
  --   When the reader action returns, the database result is freed.
  runReturningMany :: FromBackendRow be x
                   => BeamSqlBackendSyntax be
                      -- ^ The query to run
                   -> (m (Maybe x) -> m a)
                       -- ^ Reader action that will be called with a function to
                       -- fetch the next row
                   -> m a

  -- | Run the given command and don't consume any results. Useful for DML
  --   statements like INSERT, UPDATE, and DELETE, or DDL statements.
  runNoReturn :: BeamSqlBackendSyntax be -> m ()
  runNoReturn cmd =
      runReturningMany cmd $ \(_ :: m (Maybe ())) -> pure ()

  -- | Run the given command and fetch the unique result. The result is
  --   'Nothing' if either no results are returned or more than one result is
  --   returned.
  runReturningOne :: FromBackendRow be x => BeamSqlBackendSyntax be -> m (Maybe x)
  runReturningOne cmd =
      runReturningMany cmd $ \next ->
        do a <- next
           case a of
             Nothing -> pure Nothing
             Just x -> do
               a' <- next
               case a' of
                 Nothing -> pure (Just x)
                 Just _ -> pure Nothing

  -- | Run the given command, collect all the results, and return them as a
  --   list. May be more convenient than 'runReturningMany', but reads the entire
  --   result set into memory.
  runReturningList :: FromBackendRow be x => BeamSqlBackendSyntax be -> m [x]
  runReturningList cmd =
      runReturningMany cmd $ \next ->
          let collectM acc = do
                a <- next
                case a of
                  Nothing -> pure (acc [])
                  Just x -> collectM (acc . (x:))
          in collectM id

-- * BeamSqlBackend

-- | Class for all Beam SQL backends
class ( -- Every SQL backend must be a beam backend
        BeamBackend be

        -- Every SQL backend must have a reasonable SQL92 semantics
      , IsSql92Syntax (BeamSqlBackendSyntax be)
      , Sql92SanityCheck (BeamSqlBackendSyntax be)

        -- Needed for several combinators
      , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) Bool
      , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) SqlNull

        -- Needed for the Eq instance on QGenExpr
      , Eq (BeamSqlBackendExpressionSyntax be)
      ) => BeamSqlBackend be where

  type BeamSqlBackendSyntax be :: *

-- | Fake backend that cannot deserialize anything, but is useful for testing
data MockSqlBackend syntax

class Trivial a
instance Trivial a

instance BeamBackend (MockSqlBackend syntax) where
  type BackendFromField (MockSqlBackend syntax) = Trivial

instance ( IsSql92Syntax syntax
         , Sql92SanityCheck syntax

           -- Needed for several combinators
         , HasSqlValueSyntax (Sql92ValueSyntax syntax) Bool
         , HasSqlValueSyntax (Sql92ValueSyntax syntax) SqlNull

           -- Needed for the Eq instance on QGenExpr
         , Eq (Sql92ExpressionSyntax syntax)
         ) => BeamSqlBackend (MockSqlBackend syntax) where
  type BeamSqlBackendSyntax (MockSqlBackend syntax) = syntax

-- | Type class for things which are text-like in this backend
class BeamSqlBackendIsString be text
instance BeamSqlBackendIsString be t => BeamSqlBackendIsString be (Tagged tag t)
instance BeamSqlBackendIsString (MockSqlBackend cmd) Text
instance BeamSqlBackendIsString (MockSqlBackend cmd) [Char]

type BeamSql99ExpressionBackend be = IsSql99ExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSql99ConcatExpressionBackend be = IsSql99ConcatExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSql99AggregationBackend be = IsSql99AggregationExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSql2003ExpressionBackend be = ( IsSql2003ExpressionSyntax (BeamSqlBackendExpressionSyntax be)
                                       , Sql2003SanityCheck (BeamSqlBackendSyntax be) )

type BeamSqlBackendSupportsOuterJoin be = IsSql92FromOuterJoinSyntax (BeamSqlBackendFromSyntax be)

type BeamSqlT611Backend be = IsSql2003ExpressionElementaryOLAPOperationsSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT612Backend be = IsSql2003ExpressionAdvancedOLAPOperationsSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT614Backend be = IsSql2003NtileExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT615Backend be = IsSql2003LeadAndLagExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT616Backend be = IsSql2003FirstValueAndLastValueExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT618Backend be = IsSql2003NthValueExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT621Backend be =
  ( IsSql2003EnhancedNumericFunctionsExpressionSyntax (BeamSqlBackendExpressionSyntax be)
  , IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax (BeamSqlBackendExpressionSyntax be) )

type BeamSqlBackendSelectSyntax be = Sql92SelectSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendInsertSyntax be = Sql92InsertSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendInsertValuesSyntax be = Sql92InsertValuesSyntax (BeamSqlBackendInsertSyntax be)
type BeamSqlBackendExpressionSyntax be = Sql92ExpressionSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendFieldNameSyntax be = Sql92ExpressionFieldNameSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlBackendUpdateSyntax be = Sql92UpdateSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendDeleteSyntax be = Sql92DeleteSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendExpressionQuantifierSyntax be = Sql92ExpressionQuantifierSyntax (Sql92ExpressionSyntax (BeamSqlBackendSyntax be))
type BeamSqlBackendValueSyntax be = Sql92ValueSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendSetQuantifierSyntax be = Sql92SelectTableSetQuantifierSyntax (BeamSqlBackendSelectTableSyntax be)
type BeamSqlBackendAggregationQuantifierSyntax be = Sql92AggregationSetQuantifierSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlBackendSelectTableSyntax be = Sql92SelectSelectTableSyntax (BeamSqlBackendSelectSyntax be)
type BeamSqlBackendFromSyntax be = Sql92SelectFromSyntax (BeamSqlBackendSelectSyntax be)
type BeamSqlBackendOrderingSyntax be = Sql92SelectOrderingSyntax (BeamSqlBackendSelectSyntax be)
type BeamSqlBackendGroupingSyntax be = Sql92SelectTableGroupingSyntax (BeamSqlBackendSelectTableSyntax be)

type BeamSqlBackendWindowFrameSyntax be = Sql2003ExpressionWindowFrameSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlBackendWindowFrameBoundsSyntax be = Sql2003WindowFrameBoundsSyntax (BeamSqlBackendWindowFrameSyntax be)
type BeamSqlBackendWindowFrameBoundSyntax be = Sql2003WindowFrameBoundsBoundSyntax (BeamSqlBackendWindowFrameBoundsSyntax be)

type BeamSqlBackendCanSerialize be = HasSqlValueSyntax (BeamSqlBackendValueSyntax be)
type BeamSqlBackendCanDeserialize be = FromBackendRow be
type BeamSqlBackendSupportsDataType be x =
  ( BeamSqlBackendCanDeserialize be x
  , BeamSqlBackendCanSerialize be x )
