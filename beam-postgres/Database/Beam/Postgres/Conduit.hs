{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | More efficient query execution functions for @beam-postgres@. These
-- functions use the @conduit@ package, to execute @beam-postgres@ statements in
-- an arbitrary 'MonadIO'. These functions may be more efficient for streaming
-- operations than 'MonadBeam'.
module Database.Beam.Postgres.Conduit
  ( streamingRunSelect
  , runInsert
  , streamingRunInsertReturning
  , runUpdate
  , streamingRunUpdateReturning
  , runDelete
  , streamingRunDeleteReturning
  , executeStatement
  , streamingRunQueryReturning
  -- * Deprecated streaming variants
  , runSelect
  , runInsertReturning
  , runUpdateReturning
  , runDeleteReturning
  , runQueryReturning
  ) where

import           Database.Beam hiding (runInsert, runUpdate, runDelete)
import           Database.Beam.Postgres.Connection
import           Database.Beam.Postgres.Full
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import           Control.Concurrent.MVar (takeMVar, putMVar)
import           Control.Exception.Base (bracket, throwIO)
import           Control.Exception.Lifted (finally)
import qualified Control.Exception.Lifted as Lifted
import qualified Control.Concurrent.MVar.Lifted as Lifted
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Database.PostgreSQL.LibPQ as Pg hiding
  (Connection, escapeStringConn, escapeIdentifier, escapeByteaConn, exec)
import qualified Database.PostgreSQL.LibPQ as Pq
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Internal as Pg
import           Database.PostgreSQL.Simple.Internal (connectionHandle)
import qualified Database.PostgreSQL.Simple.Types as Pg (Query(..))

import qualified Conduit as C
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import qualified Control.Monad.Fail as Fail

#if MIN_VERSION_conduit(1,3,0)
#define CONDUIT_TRANSFORMER C.ConduitT
#else
#define CONDUIT_TRANSFORMER C.ConduitM
#endif

-- * @SELECT@

-- | Run a PostgreSQL @SELECT@ statement in any 'C.MonadResource'.
streamingRunSelect :: ( C.MonadResource m, Fail.MonadFail m, FromBackendRow Postgres a )
                   => Pg.Connection -> SqlSelect Postgres a
                   -> CONDUIT_TRANSFORMER () a m ()
streamingRunSelect conn (SqlSelect (PgSelectSyntax syntax)) =
  streamingRunQueryReturning conn syntax

-- | Run a PostgreSQL @SELECT@ statement in any 'MonadIO'.
runSelect :: ( MonadIO m, Fail.MonadFail m, MonadBaseControl IO m, FromBackendRow Postgres a )
          => Pg.Connection -> SqlSelect Postgres a
          -> (CONDUIT_TRANSFORMER () a m () -> m b) -> m b
runSelect conn (SqlSelect (PgSelectSyntax syntax)) withSrc =
  runQueryReturning conn syntax withSrc
{-# DEPRECATED runSelect "Use streamingRunSelect" #-}


-- * @INSERT@

-- | Run a PostgreSQL @INSERT@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runInsert :: MonadIO m
          => Pg.Connection -> SqlInsert Postgres tbl -> m Int64
runInsert _ SqlInsertNoRows = pure 0
runInsert conn (SqlInsert _ (PgInsertSyntax i)) =
  executeStatement conn i

-- | Run a PostgreSQL @INSERT ... RETURNING ...@ statement in any 'C.MonadResource' and
-- get a 'C.Source' of the newly inserted rows.
streamingRunInsertReturning :: ( C.MonadResource m, Fail.MonadFail m, FromBackendRow Postgres a )
                            => Pg.Connection
                            -> PgInsertReturning a
                            -> CONDUIT_TRANSFORMER () a m ()
streamingRunInsertReturning _ PgInsertReturningEmpty = pure ()
streamingRunInsertReturning conn (PgInsertReturning i) =
    streamingRunQueryReturning conn i

-- | Run a PostgreSQL @INSERT ... RETURNING ...@ statement in any 'MonadIO' and
-- get a 'C.Source' of the newly inserted rows.
runInsertReturning :: ( MonadIO m, Fail.MonadFail m, MonadBaseControl IO m, FromBackendRow Postgres a )
                   => Pg.Connection
                   -> PgInsertReturning a
                   -> (CONDUIT_TRANSFORMER () a m () -> m b)
                   -> m b
runInsertReturning _ PgInsertReturningEmpty withSrc = withSrc (pure ())
runInsertReturning conn (PgInsertReturning i) withSrc =
    runQueryReturning conn i withSrc
{-# DEPRECATED runInsertReturning "Use streamingRunInsertReturning" #-}

-- * @UPDATE@

-- | Run a PostgreSQL @UPDATE@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runUpdate :: MonadIO m
          => Pg.Connection -> SqlUpdate Postgres tbl -> m Int64
runUpdate _ SqlIdentityUpdate = pure 0
runUpdate conn (SqlUpdate _ (PgUpdateSyntax i)) =
    executeStatement conn i

-- | Run a PostgreSQL @UPDATE ... RETURNING ...@ statement in any 'C.MonadResource' and
-- get a 'C.Source' of the newly updated rows.
streamingRunUpdateReturning :: ( C.MonadResource m, Fail.MonadFail m, FromBackendRow Postgres a)
                            => Pg.Connection
                            -> PgUpdateReturning a
                            -> CONDUIT_TRANSFORMER () a m ()
streamingRunUpdateReturning _ PgUpdateReturningEmpty = pure ()
streamingRunUpdateReturning conn (PgUpdateReturning u) =
  streamingRunQueryReturning conn u

-- | Run a PostgreSQL @UPDATE ... RETURNING ...@ statement in any 'MonadIO' and
-- get a 'C.Source' of the newly updated rows.
runUpdateReturning :: ( MonadIO m, Fail.MonadFail m, MonadBaseControl IO m, FromBackendRow Postgres a)
                   => Pg.Connection
                   -> PgUpdateReturning a
                   -> (CONDUIT_TRANSFORMER () a m () -> m b)
                   -> m b
runUpdateReturning _ PgUpdateReturningEmpty withSrc = withSrc (pure ())
runUpdateReturning conn (PgUpdateReturning u) withSrc =
  runQueryReturning conn u withSrc
{-# DEPRECATED runUpdateReturning "Use streamingRunUpdateReturning" #-}

-- * @DELETE@

-- | Run a PostgreSQL @DELETE@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runDelete :: MonadIO m
          => Pg.Connection -> SqlDelete Postgres tbl
          -> m Int64
runDelete conn (SqlDelete _ (PgDeleteSyntax d)) =
    executeStatement conn d

-- | Run a PostgreSQl @DELETE ... RETURNING ...@ statement in any
-- 'C.MonadResource' and get a 'C.Source' of the deleted rows.
streamingRunDeleteReturning :: ( C.MonadResource m, Fail.MonadFail m, FromBackendRow Postgres a )
                            => Pg.Connection -> PgDeleteReturning a
                            -> CONDUIT_TRANSFORMER () a m ()
streamingRunDeleteReturning conn (PgDeleteReturning d) =
  streamingRunQueryReturning conn d

-- | Run a PostgreSQl @DELETE ... RETURNING ...@ statement in any
-- 'MonadIO' and get a 'C.Source' of the deleted rows.
runDeleteReturning :: ( MonadIO m, Fail.MonadFail m, MonadBaseControl IO m, FromBackendRow Postgres a )
                   => Pg.Connection -> PgDeleteReturning a
                   -> (CONDUIT_TRANSFORMER () a m () -> m b) -> m b
runDeleteReturning conn (PgDeleteReturning d) withSrc =
  runQueryReturning conn d withSrc
{-# DEPRECATED runDeleteReturning "Use streamingRunDeleteReturning" #-}

-- * Convenience functions

-- | Run any DML statement. Return the number of rows affected
executeStatement ::  MonadIO m => Pg.Connection -> PgSyntax -> m Int64
executeStatement conn x =
  liftIO $ do
    syntax <- pgRenderSyntax conn x
    Pg.execute_ conn (Pg.Query syntax)


-- | Runs any query that returns a set of values
streamingRunQueryReturning
  :: ( C.MonadResource m, Fail.MonadFail m, FromBackendRow Postgres r )
  => Pg.Connection -> PgSyntax
  -> CONDUIT_TRANSFORMER () r m ()
streamingRunQueryReturning (conn@Pg.Connection {connectionHandle}) x = do
  syntax <- liftIO $ pgRenderSyntax conn x
  -- We need to own the connection for the duration of the conduit's
  -- lifetime, since it will be in a streaming state until we clean up
  C.bracketP
    (takeMVar connectionHandle)
    (putMVar connectionHandle)
    (\conn' -> do
      success <- liftIO $
        if Pg.isNullConnection conn'
        then throwIO Pg.disconnectedError
        else Pg.sendQuery conn' syntax

      if success
        then do
          singleRowModeSet <- liftIO $ Pg.setSingleRowMode conn'
          if singleRowModeSet
            then
              C.bracketP
                (pure ())
                (\_ -> gracefulShutdown conn')
                (\_ -> streamResults conn conn' Nothing)
            else Fail.fail "Could not enable single row mode"
        else do
          errMsg <- fromMaybe "No libpq error provided" <$> liftIO (Pg.errorMessage conn')
          Fail.fail (show errMsg))

streamResults :: (Fail.MonadFail m, FromBackendRow Postgres r, MonadIO m) => Pg.Connection -> Pq.Connection -> Maybe [Pg.Field] -> C.ConduitT i r m ()
streamResults (conn@Pg.Connection {connectionHandle}) conn' fields = do
  nextRow <- liftIO (Pg.getResult conn')
  case nextRow of
    Nothing -> pure ()
    Just row ->
      liftIO (Pg.resultStatus row) >>=
      \case
        Pg.SingleTuple ->
          do fields' <- liftIO (maybe (getFields row) pure fields)
             parsedRow <- liftIO $ bracket
               (putMVar connectionHandle conn')
               (\_ -> takeMVar connectionHandle)
               (\_ -> runPgRowReader conn 0 row fields' fromBackendRow)
             case parsedRow of
               Left err -> liftIO (bailEarly conn' row ("Could not read row: " <> show err))
               Right parsedRow' ->
                 do C.yield parsedRow'
                    streamResults conn conn' (Just fields')
        Pg.TuplesOk -> liftIO (finishQuery conn')
        Pg.EmptyQuery -> Fail.fail "No query"
        Pg.CommandOk -> pure ()
        status@Pg.BadResponse -> liftIO (Pg.throwResultError "streamResults" row status)
        status@Pg.NonfatalError -> liftIO (Pg.throwResultError "streamResults" row status)
        status@Pg.FatalError -> liftIO (Pg.throwResultError "streamResults" row status)
        _ -> do errMsg <- liftIO (Pg.resultErrorMessage row)
                Fail.fail ("Postgres error: " <> show errMsg)

bailEarly :: Pq.Connection -> Pg.Result -> String -> IO a
bailEarly conn' row errorString = do
  Pg.unsafeFreeResult row
  cancelQuery conn'
  Fail.fail errorString

cancelQuery :: Pq.Connection -> IO ()
cancelQuery conn' = do
  cancel <- Pg.getCancel conn'
  case cancel of
    Nothing -> pure ()
    Just cancel' -> do
      res <- Pg.cancel cancel'
      case res of
        Right () -> liftIO (finishQuery conn')
        Left err -> Fail.fail ("Could not cancel: " <> show err)

finishQuery :: Pq.Connection -> IO ()
finishQuery conn' = do
  nextRow <- Pg.getResult conn'
  case nextRow of
    Nothing -> pure ()
    Just _ -> finishQuery conn'

gracefulShutdown :: Pq.Connection -> IO ()
gracefulShutdown conn' = do
  sts <- Pg.transactionStatus conn'
  case sts of
    Pg.TransIdle -> pure ()
    Pg.TransInTrans -> pure ()
    Pg.TransInError -> pure ()
    Pg.TransUnknown -> pure ()
    Pg.TransActive -> cancelQuery conn'

-- | Runs any query that returns a set of values
runQueryReturning
  :: ( MonadIO m, Fail.MonadFail m, MonadBaseControl IO m, Functor m, FromBackendRow Postgres r )
  => Pg.Connection -> PgSyntax
  -> (CONDUIT_TRANSFORMER () r m () -> m b)
  -> m b
runQueryReturning (conn@Pg.Connection {connectionHandle}) x withSrc = do
  syntax <- liftIO $ pgRenderSyntax conn x

  Lifted.bracket
    (Lifted.takeMVar connectionHandle)
    (Lifted.putMVar connectionHandle)
    (\conn' -> do
      success <- liftIO $ Pg.sendQuery conn' syntax
      if success
        then do
          singleRowModeSet <- liftIO (Pg.setSingleRowMode conn')
          if singleRowModeSet
             then withSrc (streamResults conn conn' Nothing) `finally` (liftIO $ gracefulShutdown conn')
             else Fail.fail "Could not enable single row mode"
        else do
          errMsg <- fromMaybe "No libpq error provided" <$> liftIO (Pg.errorMessage conn')
          Fail.fail (show errMsg))
{-# DEPRECATED runQueryReturning "Use streamingRunQueryReturning" #-}
