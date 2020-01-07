{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | More efficient query execution functions for @beam-postgres@. These
-- functions use the @conduit@ package, to execute @beam-postgres@ statements in
-- an arbitrary 'MonadIO'. These functions may be more efficient for streaming
-- operations than 'MonadBeam'.
module Database.Beam.Postgres.Conduit where

import           Database.Beam
import           Database.Beam.Postgres.Connection
import           Database.Beam.Postgres.Full
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import           Control.Exception.Lifted (finally)
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Database.PostgreSQL.LibPQ as Pg hiding
  (Connection, escapeStringConn, escapeIdentifier, escapeByteaConn, exec)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Internal as Pg (withConnection)
import qualified Database.PostgreSQL.Simple.Types as Pg (Query(..))

import qualified Data.Conduit as C
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

-- | Run a PostgreSQL @SELECT@ statement in any 'MonadIO'.
runSelect :: ( MonadIO m, Fail.MonadFail m, MonadBaseControl IO m, FromBackendRow Postgres a )
          => Pg.Connection -> SqlSelect Postgres a
          -> (CONDUIT_TRANSFORMER () a m () -> m b) -> m b
runSelect conn (SqlSelect (PgSelectSyntax syntax)) withSrc =
  runQueryReturning conn syntax withSrc

-- * @INSERT@

-- | Run a PostgreSQL @INSERT@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runInsert :: MonadIO m
          => Pg.Connection -> SqlInsert Postgres tbl -> m Int64
runInsert _ SqlInsertNoRows = pure 0
runInsert conn (SqlInsert _ (PgInsertSyntax i)) =
  executeStatement conn i

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

-- * @UPDATE@

-- | Run a PostgreSQL @UPDATE@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runUpdate :: MonadIO m
          => Pg.Connection -> SqlUpdate Postgres tbl -> m Int64
runUpdate _ SqlIdentityUpdate = pure 0
runUpdate conn (SqlUpdate _ (PgUpdateSyntax i)) =
    executeStatement conn i

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

-- * @DELETE@

-- | Run a PostgreSQL @DELETE@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runDelete :: MonadIO m
          => Pg.Connection -> SqlDelete Postgres tbl
          -> m Int64
runDelete conn (SqlDelete _ (PgDeleteSyntax d)) =
    executeStatement conn d

-- | Run a PostgreSQl @DELETE ... RETURNING ...@ statement in any
-- 'MonadIO' and get a 'C.Source' of the deleted rows.
runDeleteReturning :: ( MonadIO m, Fail.MonadFail m, MonadBaseControl IO m, FromBackendRow Postgres a )
                   => Pg.Connection -> PgDeleteReturning a
                   -> (CONDUIT_TRANSFORMER () a m () -> m b) -> m b
runDeleteReturning conn (PgDeleteReturning d) withSrc =
  runQueryReturning conn d withSrc

-- * Convenience functions

-- | Run any DML statement. Return the number of rows affected
executeStatement ::  MonadIO m => Pg.Connection -> PgSyntax -> m Int64
executeStatement conn x =
  liftIO $ do
    syntax <- pgRenderSyntax conn x
    Pg.execute_ conn (Pg.Query syntax)

-- | Runs any query that returns a set of values
runQueryReturning
  :: ( MonadIO m, Fail.MonadFail m, MonadBaseControl IO m, Functor m, FromBackendRow Postgres r )
  => Pg.Connection -> PgSyntax
  -> (CONDUIT_TRANSFORMER () r m () -> m b)
  -> m b
runQueryReturning conn x withSrc = do
  success <- liftIO $ do
    syntax <- pgRenderSyntax conn x

    Pg.withConnection conn (\conn' -> Pg.sendQuery conn' syntax)

  if success
    then do
      singleRowModeSet <- liftIO (Pg.withConnection conn Pg.setSingleRowMode)
      if singleRowModeSet
         then withSrc (streamResults Nothing) `finally` gracefulShutdown
         else Fail.fail "Could not enable single row mode"
    else do
      errMsg <- fromMaybe "No libpq error provided" <$> liftIO (Pg.withConnection conn Pg.errorMessage)
      Fail.fail (show errMsg)

  where
    streamResults fields = do
      nextRow <- liftIO (Pg.withConnection conn Pg.getResult)
      case nextRow of
        Nothing -> pure ()
        Just row ->
          liftIO (Pg.resultStatus row) >>=
          \case
            Pg.SingleTuple ->
              do fields' <- liftIO (maybe (getFields row) pure fields)
                 parsedRow <- liftIO (runPgRowReader conn 0 row fields' fromBackendRow)
                 case parsedRow of
                   Left err -> liftIO (bailEarly row ("Could not read row: " <> show err))
                   Right parsedRow' ->
                     do C.yield parsedRow'
                        streamResults (Just fields')
            Pg.TuplesOk -> liftIO (Pg.withConnection conn finishQuery)
            Pg.EmptyQuery -> Fail.fail "No query"
            Pg.CommandOk -> pure ()
            _ -> do errMsg <- liftIO (Pg.resultErrorMessage row)
                    Fail.fail ("Postgres error: " <> show errMsg)

    bailEarly row errorString = do
      Pg.unsafeFreeResult row
      Pg.withConnection conn $ cancelQuery
      Fail.fail errorString

    cancelQuery conn' = do
      cancel <- Pg.getCancel conn'
      case cancel of
        Nothing -> pure ()
        Just cancel' -> do
          res <- Pg.cancel cancel'
          case res of
            Right () -> liftIO (finishQuery conn')
            Left err -> Fail.fail ("Could not cancel: " <> show err)

    finishQuery conn' = do
      nextRow <- Pg.getResult conn'
      case nextRow of
        Nothing -> pure ()
        Just _ -> finishQuery conn'

    gracefulShutdown =
      liftIO . Pg.withConnection conn $ \conn' ->
      do sts <- Pg.transactionStatus conn'
         case sts of
           Pg.TransIdle -> pure ()
           Pg.TransInTrans -> pure ()
           Pg.TransInError -> pure ()
           Pg.TransUnknown -> pure ()
           Pg.TransActive -> cancelQuery conn'
