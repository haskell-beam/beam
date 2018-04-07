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

import qualified Database.PostgreSQL.LibPQ as Pg hiding
  (Connection, escapeStringConn, escapeIdentifier, escapeByteaConn, exec)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Internal as Pg (withConnection)
import qualified Database.PostgreSQL.Simple.Types as Pg (Query(..))

import qualified Data.Conduit as C
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))

-- * @SELECT@

-- | Run a PostgreSQL @SELECT@ statement in any 'MonadIO'.
runSelect :: ( MonadIO m, Functor m, FromBackendRow Postgres a ) =>
             Pg.Connection -> SqlSelect PgSelectSyntax a -> C.ConduitT () a m ()
runSelect conn (SqlSelect (PgSelectSyntax syntax)) = runQueryReturning conn syntax

-- * @INSERT@

-- | Run a PostgreSQL @INSERT@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runInsert :: ( MonadIO m, Functor m ) => Pg.Connection -> SqlInsert PgInsertSyntax -> m Int64
runInsert _ SqlInsertNoRows = pure 0
runInsert conn (SqlInsert (PgInsertSyntax i)) =
  executeStatement conn i

-- | Run a PostgreSQL @INSERT ... RETURNING ...@ statement in any 'MonadIO' and
-- get a 'C.Source' of the newly inserted rows.
runInsertReturning :: ( MonadIO m, Functor m, FromBackendRow Postgres a)
                   => Pg.Connection
                   -> PgInsertReturning a
                   -> C.ConduitT () a m ()
runInsertReturning _ PgInsertReturningEmpty = pure ()
runInsertReturning conn (PgInsertReturning i) =
    runQueryReturning conn i

-- * @UPDATE@

-- | Run a PostgreSQL @UPDATE@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runUpdate :: ( MonadIO m, Functor m ) => Pg.Connection -> SqlUpdate PgUpdateSyntax tbl -> m Int64
runUpdate _ SqlIdentityUpdate = pure 0
runUpdate conn (SqlUpdate (PgUpdateSyntax i)) =
    executeStatement conn i

-- | Run a PostgreSQL @UPDATE ... RETURNING ...@ statement in any 'MonadIO' and
-- get a 'C.Source' of the newly updated rows.
runUpdateReturning :: ( MonadIO m, Functor m, FromBackendRow Postgres a)
                   => Pg.Connection
                   -> PgUpdateReturning a
                   -> C.ConduitT () a m ()
runUpdateReturning _ PgUpdateReturningEmpty = pure ()
runUpdateReturning conn (PgUpdateReturning u) =
  runQueryReturning conn u

-- * @DELETE@

-- | Run a PostgreSQL @DELETE@ statement in any 'MonadIO'. Returns the number of
-- rows affected.
runDelete :: MonadIO m
          => Pg.Connection -> SqlDelete PgDeleteSyntax tbl
          -> m Int64
runDelete conn (SqlDelete (PgDeleteSyntax d)) =
    executeStatement conn d

-- | Run a PostgreSQl @DELETE ... RETURNING ...@ statement in any
-- 'MonadIO' and get a 'C.Source' of the deleted rows.
runDeleteReturning :: ( MonadIO m, Functor m, FromBackendRow Postgres a)
                   => Pg.Connection -> PgDeleteReturning a
                   -> C.ConduitT () a m ()
runDeleteReturning conn (PgDeleteReturning d) =
  runQueryReturning conn d

-- * Convenience functions

-- | Run any DML statement. Return the number of rows affected
executeStatement ::  MonadIO m => Pg.Connection -> PgSyntax -> m Int64
executeStatement conn x =
  liftIO $ do
    syntax <- pgRenderSyntax conn x
    Pg.execute_ conn (Pg.Query syntax)

-- | Runs any query that returns a set of values
runQueryReturning ::
    ( MonadIO m, Functor m, FromBackendRow Postgres r ) =>
    Pg.Connection -> PgSyntax -> C.ConduitT () r m ()
runQueryReturning conn x = do
  success <- liftIO $ do
    syntax <- pgRenderSyntax conn x

    Pg.withConnection conn (\conn' -> Pg.sendQuery conn' syntax)

  if success
    then do
      singleRowModeSet <- liftIO (Pg.withConnection conn Pg.setSingleRowMode)
      if singleRowModeSet then streamResults Nothing
         else fail "Could not enable single row mode"
    else do
      errMsg <- fromMaybe "No libpq error provided" <$> liftIO (Pg.withConnection conn Pg.errorMessage)
      fail (show errMsg)

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
            Pg.EmptyQuery -> fail "No query"
            Pg.CommandOk -> pure ()
            _ -> do errMsg <- liftIO (Pg.resultErrorMessage row)
                    fail ("Postgres error: " <> show errMsg)

    bailEarly row errorString = do
      Pg.unsafeFreeResult row
      cancelQuery
      fail errorString

    cancelQuery =
      Pg.withConnection conn $ \conn' -> do
      cancel <- Pg.getCancel conn'
      case cancel of
        Nothing -> pure ()
        Just cancel' -> do
          res <- Pg.cancel cancel'
          case res of
            Right () -> liftIO (finishQuery conn')
            Left err -> fail ("Could not cancel: " <> show err)

    finishQuery conn' = do
      nextRow <- Pg.getResult conn'
      case nextRow of
        Nothing -> pure ()
        Just _ -> finishQuery conn'
