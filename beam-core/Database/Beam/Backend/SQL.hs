module Database.Beam.Backend.SQL
  ( module Database.Beam.Backend.SQL.SQL2003
  , module Database.Beam.Backend.SQL.Types
  , module Database.Beam.Backend.Types

  , MonadBeam(..) ) where

import Database.Beam.Backend.SQL.SQL2003
import Database.Beam.Backend.SQL.Types
import Database.Beam.Backend.Types

import Control.Monad.IO.Class

-- * MonadBeam class

-- | A class that ties together a Sql syntax, backend, handle, and monad type.
--
--   Functional dependencies mean that only the backend type or the handle need
--   to be specified.
--
--   Intuitively, this allows you to write code that performs database commands
--   without having to know the underlying API. As long as you have an
--   appropriate handle from a database library that Beam can use, you can use
--   the 'MonadBeam' methods to execute the query.
--
--   Provided here is a low-level interface. Most often, you'll only need the
--   'withDatabase' and 'withDatabaseDebug' function. The 'run*' functions are
--   wrapped by the appropriate functions in 'Database.Beam.Query'.
--
--   This interface is very high-level and isn't meant to expose the full power
--   of the underlying database. Namely, it only supports simple data retrieval
--   strategies. More complicated strategies (for example, Postgres's @COPY@)
--   are supported in individual backends. See the documentation of those
--   backends for more details.
class (BeamBackend be, Monad m, MonadIO m, Sql92SanityCheck syntax) =>
  MonadBeam syntax be handle m | m -> syntax be handle, be -> m, handle -> m where

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
                   => syntax               -- ^ The query to run
                   -> (m (Maybe x) -> m a) -- ^ Reader action that will be called with a function to fetch the next row
                   -> m a

  -- | Run the given command and don't consume any results. Useful for DML
  --   statements like INSERT, UPDATE, and DELETE, or DDL statements.
  runNoReturn :: syntax -> m ()
  runNoReturn cmd =
      runReturningMany cmd $ \(_ :: m (Maybe ())) -> pure ()

  -- | Run the given command and fetch the unique result. The result is
  --   'Nothing' if either no results are returned or more than one result is
  --   returned.
  runReturningOne :: FromBackendRow be x => syntax -> m (Maybe x)
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
  runReturningList :: FromBackendRow be x => syntax -> m [x]
  runReturningList cmd =
      runReturningMany cmd $ \next ->
          let collectM acc = do
                a <- next
                case a of
                  Nothing -> pure (acc [])
                  Just x -> collectM (acc . (x:))
          in collectM id
