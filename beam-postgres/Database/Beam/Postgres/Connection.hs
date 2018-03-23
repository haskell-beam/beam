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

module Database.Beam.Postgres.Connection
  ( PgRowReadError(..), PgError(..)
  , Pg(..), PgF(..)

  , pgRenderSyntax, runPgRowReader, getFields

  , withPgDebug

  , postgresUriSyntax ) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class

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
import qualified Database.PostgreSQL.Simple.Types as Pg (Null(..), Query(..))

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Exception (bracket)

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (toLazyByteString, byteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import           Data.Proxy
import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

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
        (\uri action -> do
            let pgConnStr = fromString (uriToString id uri "")
            bracket (Pg.connectPostgreSQL pgConnStr) Pg.close action)

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
    = PgRowReadNoMoreColumns !CInt !CInt
      -- ^ We attempted to read more columns than postgres returned. First
      -- argument is the zero-based index of the column we attempted to read,
      -- and the second is the total number of columns
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
  Pg.Connection -> Pg.Row -> Pg.Result -> [Pg.Field] -> FromBackendRowM Postgres a -> IO (Either PgRowReadError a)
runPgRowReader conn rowIdx res fields readRow =
  Pg.nfields res >>= \(Pg.Col colCount) ->
  runF readRow finish step 0 colCount fields
  where
    step (ParseOneField _) curCol colCount [] = pure (Left (PgRowReadNoMoreColumns curCol colCount))
    step (ParseOneField _) curCol colCount _
      | curCol >= colCount = pure (Left (PgRowReadNoMoreColumns curCol colCount))
    step (ParseOneField next) curCol colCount remainingFields =
      let next' Nothing _ _ _ = pure (Left (PgRowCouldNotParseField curCol))
          next' (Just {}) _ _ [] = fail "Internal error"
          next' (Just x) curCol' colCount' (_:remainingFields') = next x (curCol' + 1) colCount' remainingFields'
      in step (PeekField next') curCol colCount remainingFields

    step (PeekField next) curCol colCount [] = next Nothing curCol colCount []
    step (PeekField next) curCol colCount remainingFields
      | curCol >= colCount = next Nothing curCol colCount remainingFields
    step (PeekField next) curCol colCount remainingFields@(field:_) =
      do fieldValue <- Pg.getvalue res rowIdx (Pg.Col curCol)
         res' <- Pg.runConversion (Pg.fromField field fieldValue) conn
         case res' of
           Pg.Errors {} -> next Nothing curCol colCount remainingFields
           Pg.Ok x -> next (Just x) curCol colCount remainingFields

    step (CheckNextNNull n next) curCol colCount remainingFields =
      doCheckNextN (fromIntegral n) (curCol :: CInt) (colCount :: CInt) remainingFields >>= \yes ->
      next yes (curCol + if yes then fromIntegral n else 0) colCount (if yes then drop (fromIntegral n) remainingFields else remainingFields)

    doCheckNextN 0 _ _ _ = pure False
    doCheckNextN n curCol colCount remainingFields
      | curCol + n > colCount = pure False
      | otherwise =
        let fieldsInQuestion = zip [curCol..] (take (fromIntegral n) remainingFields)
        in readAndCheck fieldsInQuestion

    readAndCheck [] = pure True
    readAndCheck ((i, field):xs) =
      do fieldValue <- Pg.getvalue res rowIdx (Pg.Col i)
         res' <- Pg.runConversion (Pg.fromField field fieldValue) conn
         case res' of
           Pg.Errors _ -> pure False
           Pg.Ok Pg.Null -> readAndCheck xs

    finish x _ _ _ = pure (Right x)

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
                    Right r -> next (Just r) (rowIdx + 1)
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
          Right r -> pure (PgStreamContinue (next (Just r)))
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

instance MonadBeam PgCommandSyntax Postgres Pg.Connection Pg where
    withDatabase conn action =
      withPgDebug (\_ -> pure ()) conn action >>= either throwIO pure
    withDatabaseDebug dbg conn action =
      withPgDebug dbg conn action >>= either throwIO pure

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
