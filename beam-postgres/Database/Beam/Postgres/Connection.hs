{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Database.Beam.Postgres.Connection
  ( PgRowReadError(..), PgError(..)
  , Pg(..), PgF(..)

  , runBeamPostgres, runBeamPostgresDebug

  , pgRenderSyntax, runPgRowReader, getFields

  , withPgDebug

  , postgresUriSyntax ) where

import           Control.Applicative.Free
import           Control.Exception (Exception, throwIO)
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

import           Database.Beam hiding (runDelete, runUpdate, runInsert, insert)
import           Database.Beam.Schema.Tables
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Backend.URI
import           Database.Beam.Query.Types (QGenExpr(..))

import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Full
import           Database.Beam.Postgres.Types

import qualified Database.PostgreSQL.LibPQ as Pg hiding
  (Connection, escapeStringConn, escapeIdentifier, escapeByteaConn, exec)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.Internal as Pg
  ( Field(..), RowParser(..)
  , escapeStringConn, escapeIdentifier, escapeByteaConn
  , exec, throwResultError )
import qualified Database.PostgreSQL.Simple.Internal as PgI
import qualified Database.PostgreSQL.Simple.Ok as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg (Query(..))

import           Control.Monad.Reader
import           Control.Monad.State

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (toLazyByteString, byteString)
import qualified Data.ByteString.Lazy as BL
import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import           Foreign.C.Types

import           Network.URI (uriToString)

-- | Errors that may arise while using the 'Pg' monad.
data PgError
  = PgRowParseError PgRowReadError
  | PgInternalError String
  deriving Show
instance Exception PgError

data PgStream a = PgStreamDone     (Either PgError a)
                | PgStreamContinue (Maybe PgI.Row -> IO (PgStream a))

-- | 'BeamURIOpeners' for the standard @postgresql:@ URI scheme. See the
-- postgres documentation for more details on the formatting. See documentation
-- for 'BeamURIOpeners' for more information on how to use this with beam
postgresUriSyntax :: c PgCommandSyntax Postgres Pg.Connection Pg
                  -> BeamURIOpeners c
postgresUriSyntax =
    mkUriOpener "postgresql:"
        (\uri -> do
            let pgConnStr = fromString (uriToString id uri "")
            hdl <- Pg.connectPostgreSQL pgConnStr
            pure (hdl, Pg.close hdl))

-- * Syntax rendering

pgRenderSyntax ::
  Pg.Connection -> PgSyntax -> IO ByteString
pgRenderSyntax conn (PgSyntax mkQuery) =
  renderBuilder <$> runF mkQuery finish step mempty
  where
    renderBuilder = BL.toStrict . toLazyByteString

    step (EmitBuilder b next) a = next (a <> b)
    step (EmitByteString b next) a = next (a <> byteString b)
    step (EscapeString b next) a = do
      res <- wrapError "EscapeString" (Pg.escapeStringConn conn b)
      next (a <> byteString res)
    step (EscapeBytea b next) a = do
      res <- wrapError "EscapeBytea" (Pg.escapeByteaConn conn b)
      next (a <> byteString res)
    step (EscapeIdentifier b next) a = do
      res <- wrapError "EscapeIdentifier" (Pg.escapeIdentifier conn b)
      next (a <> byteString res)

    finish _ = pure

    wrapError step' go = do
      res <- go
      case res of
        Right res' -> pure res'
        Left res' -> fail (step' <> ": " <> show res')

-- * Run row readers

-- | An error that may occur while parsing a row
data PgRowReadError
    = PgRowReadNotEnoughColumns !CInt !CInt
      -- ^ We attempted to read more columns than postgres returned. First
      -- argument is the number of columns required to successfully parse
      -- the result, and the second is the number of columns we got.
    | PgRowCouldNotParseField !CInt
      -- ^ There was an error while parsing the field. The first argument gives
      -- the zero-based index of the column that could not have been
      -- parsed. This is usually caused by your Haskell schema type being
      -- incompatible with the one in the database.
    deriving Show

instance Exception PgRowReadError

getFields :: Pg.Result -> IO [Pg.Field]
getFields res = do
  Pg.Col colCount <- Pg.nfields res

  let getField col =
        Pg.Field res (Pg.Col col) <$> Pg.ftype res (Pg.Col col)

  mapM getField [0..colCount - 1]

runPgRowReader ::
  Pg.Connection -> Pg.Row -> Pg.Result -> [Pg.Field] -> FromBackendRowA Postgres a -> IO (Either PgRowReadError a)
runPgRowReader conn rowIdx res fields readRow = do
  (Pg.Col colCount) <- Pg.nfields res
  let needCols = fromIntegral $ valuesNeeded readRow
  if needCols <= colCount
    then do
      runExceptT $ evalStateT (runAp step readRow) $ zip [0..(colCount - 1)] fields
    else
      pure $ Left $ PgRowReadNotEnoughColumns needCols colCount
  where
    step :: FromBackendRowF Postgres a -> StateT [(CInt, Pg.Field)] (ExceptT PgRowReadError IO) a
    step (ParseOneField next) = do
      (curCol, field):fs <- get
      put fs
      isNull <- liftIO $ Pg.getisnull res rowIdx (Pg.Col curCol)
      case isNull of
        True -> pure $ next $ Nothing
        False -> do
          fv <- liftIO $ Pg.getvalue res rowIdx (Pg.Col curCol)
          r <- liftIO $ Pg.runConversion (Pg.fromField field fv) conn
          case r of
            Pg.Ok x -> pure $ next $ Just x
            Pg.Errors _ -> lift $ throwE $ PgRowCouldNotParseField curCol
    step (ParseAlternative f g next) = do
      (curCol, field):fs <- get
      put fs
      isNull <- liftIO $ Pg.getisnull res rowIdx (Pg.Col curCol)
      case isNull of
        True -> pure $ next $ Nothing
        False -> do
          fv1 <- liftIO $ Pg.getvalue res rowIdx (Pg.Col curCol)
          r1 <- liftIO $ Pg.runConversion (Pg.fromField field fv1) conn
          case r1 of
            Pg.Ok x -> pure $ next $ f x
            _ -> do
              fv2 <- liftIO $ Pg.getvalue res rowIdx (Pg.Col curCol)
              r2 <- liftIO $ Pg.runConversion (Pg.fromField field fv2) conn
              case r2 of
                Pg.Ok x -> pure $ next $ g x
                Pg.Errors _ -> lift $ throwE $ PgRowCouldNotParseField curCol

withPgDebug :: (String -> IO ()) -> Pg.Connection -> Pg a -> IO (Either PgError a)
withPgDebug dbg conn (Pg action) =
  let finish x = pure (Right x)
      step (PgLiftIO io next) = io >>= next
      step (PgLiftWithHandle withConn next) = withConn conn >>= next
      step (PgFetchNext next) = next Nothing
      step (PgRunReturning (PgCommandSyntax PgCommandTypeQuery syntax)
                           (mkProcess :: Pg (Maybe x) -> Pg a')
                           next) =
        do query <- pgRenderSyntax conn syntax
           let Pg process = mkProcess (Pg (liftF (PgFetchNext id)))
           dbg (T.unpack (decodeUtf8 query))
           action' <- runF process finishProcess stepProcess Nothing
           case action' of
             PgStreamDone (Right x) -> Pg.execute_ conn (Pg.Query query) >> next x
             PgStreamDone (Left err) -> pure (Left err)
             PgStreamContinue nextStream ->
               let finishUp (PgStreamDone (Right x)) = next x
                   finishUp (PgStreamDone (Left err)) = pure (Left err)
                   finishUp (PgStreamContinue next') = next' Nothing >>= finishUp

                   columnCount = fromIntegral $ valuesNeeded (fromBackendRow :: FromBackendRowA Postgres (Maybe x))
               in Pg.foldWith_ (Pg.RP (put columnCount >> ask)) conn (Pg.Query query) (PgStreamContinue nextStream) runConsumer >>= finishUp
      step (PgRunReturning (PgCommandSyntax PgCommandTypeDataUpdateReturning syntax) mkProcess next) =
        do query <- pgRenderSyntax conn syntax
           dbg (T.unpack (decodeUtf8 query))

           res <- Pg.exec conn query
           sts <- Pg.resultStatus res
           case sts of
             Pg.TuplesOk -> do
               let Pg process = mkProcess (Pg (liftF (PgFetchNext id)))
               runF process (\x _ -> Pg.unsafeFreeResult res >> next x) (stepReturningList res) 0
             _ -> Pg.throwResultError "No tuples returned to Postgres update/insert returning"
                                      res sts
      step (PgRunReturning (PgCommandSyntax _ syntax) mkProcess next) =
        do query <- pgRenderSyntax conn syntax
           dbg (T.unpack (decodeUtf8 query))
           _ <- Pg.execute_ conn (Pg.Query query)

           let Pg process = mkProcess (Pg (liftF (PgFetchNext id)))
           runF process next stepReturningNone

      stepReturningNone :: forall a. PgF (IO (Either PgError a)) -> IO (Either PgError a)
      stepReturningNone (PgLiftIO action' next) = action' >>= next
      stepReturningNone (PgLiftWithHandle withConn next) = withConn conn >>= next
      stepReturningNone (PgFetchNext next) = next Nothing
      stepReturningNone (PgRunReturning _ _ _) = pure (Left (PgInternalError "Nested queries not allowed"))

      stepReturningList :: forall a. Pg.Result -> PgF (CInt -> IO (Either PgError a)) -> CInt -> IO (Either PgError a)
      stepReturningList _   (PgLiftIO action' next) rowIdx = action' >>= \x -> next x rowIdx
      stepReturningList res (PgFetchNext next) rowIdx =
        do fields <- getFields res
           Pg.Row rowCount <- Pg.ntuples res
           if rowIdx >= rowCount
             then next Nothing rowIdx
             else runPgRowReader conn (Pg.Row rowIdx) res fields fromBackendRow >>= \case
                    Left err -> pure (Left (PgRowParseError err))
                    Right r -> next r (rowIdx + 1)
      stepReturningList _   (PgRunReturning _ _ _) _ = pure (Left (PgInternalError "Nested queries not allowed"))
      stepReturningList _   (PgLiftWithHandle {}) _ = pure (Left (PgInternalError "Nested queries not allowed"))

      finishProcess :: forall a. a -> Maybe PgI.Row -> IO (PgStream a)
      finishProcess x _ = pure (PgStreamDone (Right x))

      stepProcess :: forall a. PgF (Maybe PgI.Row -> IO (PgStream a)) -> Maybe PgI.Row -> IO (PgStream a)
      stepProcess (PgLiftIO action' next) row = action' >>= flip next row
      stepProcess (PgFetchNext next) Nothing =
        pure . PgStreamContinue $ \res ->
        case res of
          Nothing -> next Nothing Nothing
          Just (PgI.Row rowIdx res') ->
            getFields res' >>= \fields ->
            runPgRowReader conn rowIdx res' fields fromBackendRow >>= \case
              Left err -> pure (PgStreamDone (Left (PgRowParseError err)))
              Right r -> next r Nothing
      stepProcess (PgFetchNext next) (Just (PgI.Row rowIdx res)) =
        getFields res >>= \fields ->
        runPgRowReader conn rowIdx res fields fromBackendRow >>= \case
          Left err -> pure (PgStreamDone (Left (PgRowParseError err)))
          Right r -> pure (PgStreamContinue (next r))
      stepProcess (PgRunReturning _ _ _) _ = pure (PgStreamDone (Left (PgInternalError "Nested queries not allowed")))
      stepProcess (PgLiftWithHandle _ _) _ = pure (PgStreamDone (Left (PgInternalError "Nested queries not allowed")))

      runConsumer :: forall a. PgStream a -> PgI.Row -> IO (PgStream a)
      runConsumer s@(PgStreamDone {}) _ = pure s
      runConsumer (PgStreamContinue next) row = next (Just row)
  in runF action finish step

-- * Beam Monad class

data PgF next where
    PgLiftIO :: IO a -> (a -> next) -> PgF next
    PgRunReturning ::
        FromBackendRow Postgres x =>
        PgCommandSyntax -> (Pg (Maybe x) -> Pg a) -> (a -> next) -> PgF next
    PgFetchNext ::
        FromBackendRow Postgres x =>
        (Maybe x -> next) -> PgF next
    PgLiftWithHandle :: (Pg.Connection -> IO a) -> (a -> next) -> PgF next
deriving instance Functor PgF

-- | 'MonadBeam' in which we can run Postgres commands. See the documentation
-- for 'MonadBeam' on examples of how to use.
--
-- @beam-postgres@ also provides functions that let you run queries without
-- 'MonadBeam'. These functions may be more efficient and offer a conduit
-- API. See "Database.Beam.Postgres.Conduit" for more information.
newtype Pg a = Pg { runPg :: F PgF a }
    deriving (Monad, Applicative, Functor, MonadFree PgF)

instance MonadIO Pg where
    liftIO x = liftF (PgLiftIO x id)

runBeamPostgresDebug :: (String -> IO ()) -> Pg.Connection -> Pg a -> IO a
runBeamPostgresDebug dbg conn action =
    withPgDebug dbg conn action >>= either throwIO pure

runBeamPostgres :: Pg.Connection -> Pg a -> IO a
runBeamPostgres = runBeamPostgresDebug (\_ -> pure ())

instance MonadBeam PgCommandSyntax Postgres Pg.Connection Pg where
    withDatabase = runBeamPostgres
    withDatabaseDebug = runBeamPostgresDebug

    runReturningMany cmd consume =
        liftF (PgRunReturning cmd consume id)

instance MonadBeamInsertReturning PgCommandSyntax Postgres Pg.Connection Pg where
    runInsertReturningList tbl values = do
        let insertReturningCmd' =
                insertReturning tbl values onConflictDefault
                                (Just (changeBeamRep (\(Columnar' (QExpr s) :: Columnar' (QExpr PgExpressionSyntax PostgresInaccessible) ty) ->
                                                              Columnar' (QExpr s) :: Columnar' (QExpr PgExpressionSyntax ()) ty)))

        -- Make savepoint
        case insertReturningCmd' of
          PgInsertReturningEmpty ->
            pure []
          PgInsertReturning insertReturningCmd ->
            runReturningList (PgCommandSyntax PgCommandTypeDataUpdateReturning insertReturningCmd)

instance MonadBeamUpdateReturning PgCommandSyntax Postgres Pg.Connection Pg where
    runUpdateReturningList tbl mkAssignments mkWhere = do
        let updateReturningCmd' =
                updateReturning tbl mkAssignments mkWhere
                                (changeBeamRep (\(Columnar' (QExpr s) :: Columnar' (QExpr PgExpressionSyntax PostgresInaccessible) ty) ->
                                                        Columnar' (QExpr s) :: Columnar' (QExpr PgExpressionSyntax ()) ty))

        case updateReturningCmd' of
          PgUpdateReturningEmpty ->
            pure []
          PgUpdateReturning updateReturningCmd ->
            runReturningList (PgCommandSyntax PgCommandTypeDataUpdateReturning updateReturningCmd)

instance MonadBeamDeleteReturning PgCommandSyntax Postgres Pg.Connection Pg where
    runDeleteReturningList tbl mkWhere = do
        let PgDeleteReturning deleteReturningCmd =
                deleteReturning tbl mkWhere
                                (changeBeamRep (\(Columnar' (QExpr s) :: Columnar' (QExpr PgExpressionSyntax PostgresInaccessible) ty) ->
                                                        Columnar' (QExpr s) :: Columnar' (QExpr PgExpressionSyntax ()) ty))

        runReturningList (PgCommandSyntax PgCommandTypeDataUpdateReturning deleteReturningCmd)
