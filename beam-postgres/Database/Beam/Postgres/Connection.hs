{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-partial-type-signatures #-}

{-# LANGUAGE PartialTypeSignatures #-}
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
  ( Pg(..), PgF(..)

  , runBeamPostgres, runBeamPostgresDebug

  , pgRenderSyntax, runPgRowReader, getFields

  , withPgDebug

  , postgresUriSyntax ) where

import           Control.Exception (SomeException(..), throwIO)
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class

import           Database.Beam hiding (runDelete, runUpdate, runInsert, insert)
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Backend.SQL.Row ( FromBackendRowF(..), FromBackendRowM(..)
                                               , BeamRowReadError(..), ColumnParseError(..) )
import           Database.Beam.Backend.URI
import           Database.Beam.Query.Types (QGenExpr(..))
import           Database.Beam.Schema.Tables

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
import           Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (toLazyByteString, byteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Proxy
import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Typeable (cast)
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import           Foreign.C.Types

import           Network.URI (uriToString)

data PgStream a = PgStreamDone     (Either BeamRowReadError a)
                | PgStreamContinue (Maybe PgI.Row -> IO (PgStream a))

-- | 'BeamURIOpeners' for the standard @postgresql:@ URI scheme. See the
-- postgres documentation for more details on the formatting. See documentation
-- for 'BeamURIOpeners' for more information on how to use this with beam
postgresUriSyntax :: c Postgres Pg.Connection Pg
                  -> BeamURIOpeners c
postgresUriSyntax =
    mkUriOpener runBeamPostgres "postgresql:"
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

getFields :: Pg.Result -> IO [Pg.Field]
getFields res = do
  Pg.Col colCount <- Pg.nfields res

  let getField col =
        PgI.resultToField res (Pg.Col col)

  return $ map getField [0..colCount - 1]

runPgRowReader ::
  Pg.Connection -> Pg.Row -> Pg.Result -> [Pg.Field] -> FromBackendRowM Postgres a -> IO (Either BeamRowReadError a)
runPgRowReader conn rowIdx res fields (FromBackendRowM readRow) =
  Pg.nfields res >>= \(Pg.Col colCount) ->
  runF readRow finish step 0 colCount fields
  where

    step :: forall x. FromBackendRowF Postgres (CInt -> CInt -> [PgI.Field] -> IO (Either BeamRowReadError x))
         -> CInt -> CInt -> [PgI.Field] -> IO (Either BeamRowReadError x)
    step (ParseOneField _) curCol colCount [] = pure (Left (BeamRowReadError (Just (fromIntegral curCol)) (ColumnNotEnoughColumns (fromIntegral colCount))))
    step (ParseOneField _) curCol colCount _
      | curCol >= colCount = pure (Left (BeamRowReadError (Just (fromIntegral curCol)) (ColumnNotEnoughColumns (fromIntegral colCount))))
    step (ParseOneField (next' :: next -> _)) curCol colCount (field:remainingFields) =
      do fieldValue <- Pg.getvalue res rowIdx (Pg.Col curCol)
         res' <- Pg.runConversion (Pg.fromField field fieldValue) conn
         case res' of
           Pg.Errors errs ->
             let err = fromMaybe (ColumnErrorInternal "Column parse failed with unknown exception") $
                       listToMaybe $
                       do SomeException e <- errs
                          Just pgErr <- pure (cast e)
                          case pgErr of
                            Pg.ConversionFailed { Pg.errSQLType = sql
                                                , Pg.errHaskellType = hs
                                                , Pg.errMessage = msg } ->
                              pure (ColumnTypeMismatch hs sql msg)
                            Pg.Incompatible { Pg.errSQLType = sql
                                            , Pg.errHaskellType = hs
                                            , Pg.errMessage = msg } ->
                              pure (ColumnTypeMismatch hs sql msg)
                            Pg.UnexpectedNull {} ->
                              pure ColumnUnexpectedNull
             in pure (Left (BeamRowReadError (Just (fromIntegral curCol)) err))
           Pg.Ok x -> next' x (curCol + 1) colCount remainingFields

    step (Alt (FromBackendRowM a) (FromBackendRowM b) next) curCol colCount cols =
      do aRes <- runF a (\x curCol' colCount' cols' -> pure (Right (next x curCol' colCount' cols'))) step curCol colCount cols
         case aRes of
           Right next' -> next'
           Left aErr -> do
             bRes <- runF b (\x curCol' colCount' cols' -> pure (Right (next x curCol' colCount' cols'))) step curCol colCount cols
             case bRes of
               Right next' -> next'
               Left {} -> pure (Left aErr)

    step (FailParseWith err) _ _ _ =
      pure (Left err)

    finish x _ _ _ = pure (Right x)

withPgDebug :: (String -> IO ()) -> Pg.Connection -> Pg a -> IO (Either BeamRowReadError a)
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

                   columnCount = fromIntegral $ valuesNeeded (Proxy @Postgres) (Proxy @x)
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

      stepReturningNone :: forall a. PgF (IO (Either BeamRowReadError a)) -> IO (Either BeamRowReadError a)
      stepReturningNone (PgLiftIO action' next) = action' >>= next
      stepReturningNone (PgLiftWithHandle withConn next) = withConn conn >>= next
      stepReturningNone (PgFetchNext next) = next Nothing
      stepReturningNone (PgRunReturning _ _ _) = pure (Left (BeamRowReadError Nothing (ColumnErrorInternal  "Nested queries not allowed")))

      stepReturningList :: forall a. Pg.Result -> PgF (CInt -> IO (Either BeamRowReadError a)) -> CInt -> IO (Either BeamRowReadError a)
      stepReturningList _   (PgLiftIO action' next) rowIdx = action' >>= \x -> next x rowIdx
      stepReturningList res (PgFetchNext next) rowIdx =
        do fields <- getFields res
           Pg.Row rowCount <- Pg.ntuples res
           if rowIdx >= rowCount
             then next Nothing rowIdx
             else runPgRowReader conn (Pg.Row rowIdx) res fields fromBackendRow >>= \case
                    Left err -> pure (Left err)
                    Right r -> next (Just r) (rowIdx + 1)
      stepReturningList _   (PgRunReturning _ _ _) _ = pure (Left (BeamRowReadError Nothing (ColumnErrorInternal "Nested queries not allowed")))
      stepReturningList _   (PgLiftWithHandle {}) _ = pure (Left (BeamRowReadError Nothing (ColumnErrorInternal "Nested queries not allowed")))

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
              Left err -> pure (PgStreamDone (Left err))
              Right r -> next (Just r) Nothing
      stepProcess (PgFetchNext next) (Just (PgI.Row rowIdx res)) =
        getFields res >>= \fields ->
        runPgRowReader conn rowIdx res fields fromBackendRow >>= \case
          Left err -> pure (PgStreamDone (Left err))
          Right r -> pure (PgStreamContinue (next (Just r)))
      stepProcess (PgRunReturning _ _ _) _ = pure (PgStreamDone (Left (BeamRowReadError Nothing (ColumnErrorInternal "Nested queries not allowed"))))
      stepProcess (PgLiftWithHandle _ _) _ = pure (PgStreamDone (Left (BeamRowReadError Nothing (ColumnErrorInternal "Nested queries not allowed"))))

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

instance MonadFail Pg where
    fail e = fail $ "Internal Error with: " <> show e

instance MonadIO Pg where
    liftIO x = liftF (PgLiftIO x id)

runBeamPostgresDebug :: (String -> IO ()) -> Pg.Connection -> Pg a -> IO a
runBeamPostgresDebug dbg conn action =
    withPgDebug dbg conn action >>= either throwIO pure

runBeamPostgres :: Pg.Connection -> Pg a -> IO a
runBeamPostgres = runBeamPostgresDebug (\_ -> pure ())

instance MonadBeam Postgres Pg where
    runReturningMany cmd consume =
        liftF (PgRunReturning cmd consume id)

instance MonadBeamInsertReturning Postgres Pg where
    runInsertReturningList i = do
        let insertReturningCmd' = i `returning`
              changeBeamRep (\(Columnar' (QExpr s) :: Columnar' (QExpr Postgres PostgresInaccessible) ty) ->
                Columnar' (QExpr s) :: Columnar' (QExpr Postgres ()) ty)

        -- Make savepoint
        case insertReturningCmd' of
          PgInsertReturningEmpty ->
            pure []
          PgInsertReturning insertReturningCmd ->
            runReturningList (PgCommandSyntax PgCommandTypeDataUpdateReturning insertReturningCmd)

instance MonadBeamUpdateReturning Postgres Pg where
    runUpdateReturningList u = do
        let updateReturningCmd' = u `returning`
              changeBeamRep (\(Columnar' (QExpr s) :: Columnar' (QExpr Postgres PostgresInaccessible) ty) ->
                Columnar' (QExpr s) :: Columnar' (QExpr Postgres ()) ty)

        case updateReturningCmd' of
          PgUpdateReturningEmpty ->
            pure []
          PgUpdateReturning updateReturningCmd ->
            runReturningList (PgCommandSyntax PgCommandTypeDataUpdateReturning updateReturningCmd)

instance MonadBeamDeleteReturning Postgres Pg where
    runDeleteReturningList d = do
        let PgDeleteReturning deleteReturningCmd = d `returning`
              changeBeamRep (\(Columnar' (QExpr s) :: Columnar' (QExpr Postgres PostgresInaccessible) ty) ->
                Columnar' (QExpr s) :: Columnar' (QExpr Postgres ()) ty)

        runReturningList (PgCommandSyntax PgCommandTypeDataUpdateReturning deleteReturningCmd)
