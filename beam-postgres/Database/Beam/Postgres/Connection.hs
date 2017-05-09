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
  ( Pg.Connection
  , Pg.ResultError(..), Pg.SqlError(..)
  , Pg.ExecStatus(..)
  , Pg(..), PgF(..)

  , Pg.ConnectInfo(..), Pg.defaultConnectInfo

  , Pg.postgreSQLConnectionString

  , Pg.connectPostgreSQL, Pg.connect
  , Pg.close

  -- * Beam-specific calls
  , RowReadError(..), PgError(..)

  , runSelect, runInsert, runInsertReturning
  , Q.select

  , Q.insertValues, Q.insertFrom

  , pgRenderSyntax, runPgRowReader, getFields

  , withPgDebug ) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class

import           Data.ByteString (ByteString)

import           Database.Beam hiding (runInsert, insert)
import           Database.Beam.Backend.SQL
import qualified Database.Beam.Query as Q

import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import qualified Database.PostgreSQL.LibPQ as Pg hiding
  (Connection, escapeStringConn, escapeIdentifier, escapeByteaConn)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.Internal as Pg
  ( Field(..), RowParser(..)
  , withConnection, escapeStringConn, escapeIdentifier, escapeByteaConn)
import qualified Database.PostgreSQL.Simple.Internal as PgI
import qualified Database.PostgreSQL.Simple.Ok as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg (Null(..))

import           Control.Monad.Reader
import           Control.Monad.State

import           Data.ByteString.Builder (toLazyByteString, byteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.String

import           Foreign.C.Types

-- | We can use this to parameterize the thread parameters in 'Q'.
 --  This guarantees that library users can't use arbitrary 'QExpr's in a 'Q'.

-- data PostgresInaccessible

data PgError
  = PgRowParseError RowReadError
  | PgInternalError String
  deriving Show
instance Exception PgError
data PgStream a = PgStreamDone     (Either PgError a)
                | PgStreamContinue (Maybe PgI.Row -> IO (PgStream a))

-- * Functions to query

runSelect :: ( MonadIO m, Functor m, FromBackendRow Postgres a ) =>
             Pg.Connection -> SqlSelect PgSelectSyntax a -> C.Source m a
runSelect conn (SqlSelect (PgSelectSyntax syntax)) = runQueryReturning conn syntax

-- * INSERT INTO

runInsert :: ( MonadIO m, Functor m ) => Pg.Connection -> SqlInsert PgInsertSyntax -> m ()
runInsert conn (SqlInsert (PgInsertSyntax i)) =
    C.runConduit (runInsertReturning conn (PgInsertReturning i :: PgInsertReturning ()) C.=$=
                  C.sinkNull)

runInsertReturning :: ( MonadIO m, Functor m, FromBackendRow Postgres a)
                   => Pg.Connection
                   -> PgInsertReturning a
                   -> C.Source m a
runInsertReturning conn (PgInsertReturning i) =
    runQueryReturning conn i

-- insertFrom :: ( Beamable tbl, IsQuery q ) =>
--               q PgSyntax db PostgresInaccessible (tbl (QExpr PgSyntax PostgresInaccessible))
--            -> PgInsertValuesSyntax tbl
-- insertFrom q =
--     let (_, _, x) = buildSql92Query (Proxy @PgSyntax) (toQ q) 0
--     in PgInsertValuesSyntax x

-- insert :: ( MonadIO m, Functor m ) =>
--           Pg.Connection -> DatabaseTable Postgres db table
--        -> PgInsertValuesSyntax table -> PgInsertOnConflictSyntax
--        -> m ()
-- insert conn tbl values onConflict =
--     C.runConduit ((insertReturning @()) conn tbl values onConflict Nothing C.=$= C.sinkNull)

-- insertReturning
--     :: forall a m table db.
--        ( FromBackendRow Postgres (QExprToIdentity a)
--        , MonadIO m, Functor m, Projectible PgSyntax a ) =>
--        Pg.Connection -> DatabaseTable Postgres db table
--     -> PgInsertValuesSyntax table -> PgInsertOnConflictSyntax
--     -> Maybe (table (QExpr PgSyntax PostgresInaccessible) -> a)
--     -> C.Source m (QExprToIdentity a)
-- insertReturning conn (DatabaseTable _ tblNm tblSettings)
--                 (PgInsertValuesSyntax insertValues)
--                 (PgInsertOnConflictSyntax onConflict)
--                 returning =
--     runQueryReturning conn $
--     emit "INSERT INTO " <> pgQuotedIdentifier tblNm <> emit "(" <>
--     pgSepBy (emit ", ") (allBeamValues (\(Columnar' f) -> pgQuotedIdentifier (_fieldName f)) tblSettings) <>
--     emit ") " <> insertValues <> emit " " <> onConflict <>
--     (case returning of
--        Nothing -> mempty
--        Just mkProjection ->
--            let tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (unqualifiedFieldE (Proxy @PgSyntax) (_fieldName f)))) tblSettings
--            in emit " RETURNING " <>
--               pgSepBy (emit ", ") (project (Proxy @PgSyntax) (mkProjection tblQ)))

-- * UPDATE statements

-- * DELETE statements

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

data RowReadError
    = RowReadNoMoreColumns !CInt !CInt
    | RowCouldNotParseField !CInt
    deriving Show

instance Exception RowReadError

getFields :: Pg.Result -> IO [Pg.Field]
getFields res = do
  Pg.Col colCount <- Pg.nfields res

  let getField col =
        Pg.Field res (Pg.Col col) <$> Pg.ftype res (Pg.Col col)

  mapM getField [0..colCount - 1]

runPgRowReader ::
  Pg.Connection -> Pg.Row -> Pg.Result -> [Pg.Field] -> FromBackendRowM Postgres a -> IO (Either RowReadError a)
runPgRowReader conn rowIdx res fields readRow =
  Pg.nfields res >>= \(Pg.Col colCount) ->
  runF readRow finish step 0 colCount fields
  where
    step (ParseOneField _) curCol colCount [] = pure (Left (RowReadNoMoreColumns curCol colCount))
    step (ParseOneField _) curCol colCount _
      | curCol >= colCount = pure (Left (RowReadNoMoreColumns curCol colCount))
    step (ParseOneField next) curCol colCount remainingFields =
      let next' Nothing _ _ _ = pure (Left (RowCouldNotParseField curCol))
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
      step (PgFetchNext next) = next Nothing
      step (PgRunReturning (PgCommandSyntax syntax)
                           (mkProcess :: Pg (Maybe x) -> Pg a')
                           next) =
        do query <- pgRenderSyntax conn syntax
           let Pg process = mkProcess (Pg (liftF (PgFetchNext id)))
           dbg (BS.unpack query)
           action' <- runF process finishProcess stepProcess Nothing
           case action' of
             PgStreamDone (Right x) -> Pg.execute_ conn (fromString (BS.unpack query)) >> next x
             PgStreamDone (Left err) -> pure (Left err)
             PgStreamContinue nextStream ->
               let finishUp (PgStreamDone (Right x)) = next x
                   finishUp (PgStreamDone (Left err)) = pure (Left err)
                   finishUp (PgStreamContinue next') = next' Nothing >>= finishUp

                   columnCount = fromIntegral $ valuesNeeded (Proxy @Postgres) (Proxy @x)
               in Pg.foldWith_ (Pg.RP (put columnCount >> ask)) conn (fromString (BS.unpack query)) (PgStreamContinue nextStream) runConsumer >>= finishUp

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
deriving instance Functor PgF

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
