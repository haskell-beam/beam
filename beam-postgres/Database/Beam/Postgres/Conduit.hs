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

import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))

-- * Functions to query

runSelect :: ( MonadIO m, Functor m, FromBackendRow Postgres a ) =>
             Pg.Connection -> SqlSelect PgSelectSyntax a -> C.Source m a
runSelect conn (SqlSelect (PgSelectSyntax syntax)) = runQueryReturning conn syntax

-- * INSERT INTO

runInsert :: ( MonadIO m, Functor m ) => Pg.Connection -> SqlInsert PgInsertSyntax -> m ()
runInsert _ SqlInsertNoRows = pure ()
runInsert conn (SqlInsert (PgInsertSyntax i)) =
    C.runConduit (runInsertReturning conn (PgInsertReturning i :: PgInsertReturning ()) C.=$=
                  C.sinkNull)

runInsertReturning :: ( MonadIO m, Functor m, FromBackendRow Postgres a)
                   => Pg.Connection
                   -> PgInsertReturning a
                   -> C.Source m a
runInsertReturning _ PgInsertReturningEmpty = pure ()
runInsertReturning conn (PgInsertReturning i) =
    runQueryReturning conn i

runUpdateReturning :: ( MonadIO m, Functor m, FromBackendRow Postgres a)
                   => Pg.Connection
                   -> PgUpdateReturning a
                   -> C.Source m a
runUpdateReturning _ PgUpdateReturningEmpty = pure ()
runUpdateReturning conn (PgUpdateReturning u) =
  runQueryReturning conn u

runDeleteReturning :: ( MonadIO m, Functor m, FromBackendRow Postgres a)
                   => Pg.Connection -> PgDeleteReturning a
                   -> C.Source m a
runDeleteReturning conn (PgDeleteReturning d) =
  runQueryReturning conn d


-- | Runs any query that returns a set of values
runQueryReturning ::
    ( MonadIO m, Functor m, FromBackendRow Postgres r ) =>
    Pg.Connection -> PgSyntax -> C.Source m r
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
                     do C.yieldOr parsedRow' (liftIO bailAfterParse)
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

    bailAfterParse = cancelQuery

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
