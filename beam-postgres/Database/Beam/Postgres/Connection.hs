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
  , RowReadError(..)

  , runSelect, runInsert, runInsertReturning
  , Q.select

  , Q.insertValues, Q.insertFrom

  , pgRenderSyntax, runPgRowReader, getFields ) where

import           Control.Exception (Exception)
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Query.Internal
import qualified Database.Beam.Query as Q

import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Database.PostgreSQL.LibPQ as Pg hiding
  (Connection, escapeStringConn, escapeIdentifier, escapeByteaConn)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.Internal as Pg
  ( Field(..)
  , withConnection, escapeStringConn, escapeIdentifier, escapeByteaConn)
import qualified Database.PostgreSQL.Simple.Ok as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg (Null(..))

import           Control.Monad.Reader

import           Data.ByteString.Builder (toLazyByteString, byteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy

import           Foreign.C.Types

-- | We can use this to parameterize the thread parameters in 'Q'.
 --  This guarantees that library users can't use arbitrary 'QExpr's in a 'Q'.
data PostgresInaccessible

-- * Functions to query

runSelect :: ( MonadIO m, Functor m, FromBackendRow Postgres a ) =>
             Pg.Connection -> SqlSelect PgSelectSyntax a -> C.Source m a
runSelect conn (SqlSelect (PgSelectSyntax syntax)) = runQueryReturning conn syntax

-- * INSERT INTO

runInsert :: ( MonadIO m, Functor m ) => Pg.Connection -> SqlInsert PgInsertSyntax -> m ()
runInsert conn (SqlInsert (PgInsertSyntax insert)) =
    C.runConduit (runInsertReturning conn (PgInsertReturning insert :: PgInsertReturning ()) C.=$=
                  C.sinkNull)

runInsertReturning :: ( MonadIO m, Functor m, FromBackendRow Postgres a)
                   => Pg.Connection
                   -> PgInsertReturning a
                   -> C.Source m a
runInsertReturning conn (PgInsertReturning insert) =
    runQueryReturning conn insert

-- insertValues :: ( Beamable table, MakeSqlLiterals PgSyntax table ) =>
--                 [ table Identity ] -> PgSyntax1 table
-- insertValues = insertValuesGeneric (Proxy @PgSyntax)
-- insertValues tbls = PgInsertValuesSyntax (emit "VALUES " <>
--                                           pgSepBy (emit ", ") (map oneRow tbls))
--     where oneRow tbl = emit "(" <>
--                        pgSepBy (emit ", ")
--                                (allBeamValues (\(Columnar' (WithConstraint x :: WithConstraint (HasSqlValueSyntax PgSyntax) x)) ->
--                                                    sqlValueSyntax (Proxy @PgSyntax) x)
--                                               (makeSqlLiterals tbl)) <>
--                        emit ")"

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
    putStrLn ("Going to run " <> BC.unpack syntax)
    Pg.withConnection conn (\conn -> Pg.sendQuery conn syntax)

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
                   Right parsedRow ->
                     do C.yieldOr parsedRow (liftIO bailAfterParse)
                        streamResults (Just fields')
            Pg.TuplesOk -> liftIO (Pg.withConnection conn finishQuery)
            Pg.EmptyQuery -> fail "No query"
            Pg.CommandOk -> pure ()
            _ -> do errMsg <- liftIO (Pg.resultErrorMessage row)
                    fail ("Postgres error: " <> show errMsg)

    bailEarly row error = do
      Pg.unsafeFreeResult row
      cancelQuery
      fail error

    bailAfterParse = cancelQuery

    cancelQuery =
      Pg.withConnection conn $ \conn -> do
      cancel <- Pg.getCancel conn
      case cancel of
        Nothing -> pure ()
        Just cancel -> do
          res <- Pg.cancel cancel
          case res of
            Right () -> liftIO (finishQuery conn)
            Left err -> fail ("Could not cancel: " <> show err)

    finishQuery conn = do
      nextRow <- Pg.getResult conn
      case nextRow of
        Nothing -> pure ()
        Just _ -> putStrLn "REading another result" >> finishQuery conn

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

    finish x a = pure a

    wrapError step go = do
      res <- go
      case res of
        Right res -> pure res
        Left res -> fail (step <> ": " <> show res)

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
    step (ParseOneField next) curCol colCount fields@(_:fields') =
      let next' Nothing _ _ _ = pure (Left (RowCouldNotParseField curCol))
          next' (Just x) _ _ [] = fail "Internal error"
          next' (Just x) curCol colCount (_:fields) = next x (curCol + 1) colCount fields
      in step (PeekField next') curCol colCount fields

    step (PeekField next) curCol colCount [] = next Nothing curCol colCount []
    step (PeekField next) curCol colCount fields
      | curCol >= colCount = next Nothing curCol colCount fields
    step (PeekField next) curCol colCount fields@(field:_) =
      do fieldValue <- Pg.getvalue res rowIdx (Pg.Col curCol)
         res <- Pg.runConversion (Pg.fromField field fieldValue) conn
         case res of
           Pg.Errors xs -> do putStrLn ("Got errors " <> show xs)
                              next Nothing curCol colCount fields
           Pg.Ok x -> next (Just x) curCol colCount fields

    step (CheckNextNNull n next) curCol colCount fields =
      doCheckNextN (fromIntegral n) (curCol :: CInt) (colCount :: CInt) fields >>= \yes ->
      next yes curCol colCount fields

    doCheckNextN 0 _ _ _ = pure False
    doCheckNextN n curCol colCount fields
      | curCol + n > colCount = pure False
      | otherwise =
        let fieldsInQuestion = zip [curCol..] (take (fromIntegral n) fields)
        in readAndCheck fieldsInQuestion

    readAndCheck [] = pure True
    readAndCheck ((i, field):xs) =
      do fieldValue <- Pg.getvalue res rowIdx (Pg.Col i)
         res <- Pg.runConversion (Pg.fromField field fieldValue) conn
         case res of
           Pg.Errors xs -> pure False
           Pg.Ok Pg.Null -> readAndCheck xs

    finish x _ _ _ = pure (Right x)

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

instance MonadBeam PgCommandSyntax Postgres Pg where
    runReturningMany cmd consume =
        liftF (PgRunReturning cmd consume id)
