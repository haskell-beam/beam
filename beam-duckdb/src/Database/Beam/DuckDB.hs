{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.DuckDB
  ( DuckDB,
    runBeamDuckDB,
    runBeamDuckDBDebug,
  )
where

import Control.Exception (SomeException (..))
import Control.Monad (void)
import Control.Monad.Free.Church (runF)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad.Trans.State.Strict (StateT (..), get, put)
import Data.ByteString (ByteString)
import qualified Data.DList as DL
import Data.Data (Proxy (Proxy), cast)
import Data.Functor (($>))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Beam (HasQBuilder, HasSqlEqualityCheck, HasSqlInTable (..), HasSqlQuantifiedEqualityCheck)
import Database.Beam.Backend
  ( BeamBackend (..),
    BeamRowReadError (..),
    BeamSqlBackend,
    BeamSqlBackendIsString,
    BeamSqlBackendSyntax,
    ColumnParseError (..),
    FromBackendRow,
    FromBackendRowF (..),
    FromBackendRowM (..),
    MonadBeam,
    SqlNull (..),
    parseOneField,
  )
import Database.Beam.Backend.SQL (FromBackendRow (..), MonadBeam (..))
import Database.Beam.DuckDB.Syntax
  ( DuckDBCommandSyntax (..),
    DuckDBExpressionSyntax (..),
  )
import Database.Beam.DuckDB.Syntax.Builder
  ( DuckDBSyntax (..),
    SomeField (..),
    commas,
    emit,
    parens,
    withPlaceholder,
  )
import Database.Beam.Query.SQL92 (buildSql92Query')
import Database.Beam.Query.Types (HasQBuilder (..))
import Database.DuckDB.Simple (Connection, FromRow, Null, Query (Query), ResultError (..), RowParser, ToField (toField), ToRow (toRow), bind, execute, nextRow, withStatement)
import Database.DuckDB.Simple.FromField (FromField)
import Database.DuckDB.Simple.FromRow (FromRow (..), RowParser (..), field)
import Database.DuckDB.Simple.Ok (Ok (..))

-- | 'MonadBeam' instance inside which DuckDB queries are run. See the
-- <https://haskell-beam.github.io/beam/ user guide> for more information
newtype DuckDBM a
  = DuckDBM
  { -- | Run an IO action with access to a DuckDB connection and a debug logging
    --  function, called or each query submitted on the connection.
    runDuckDBM :: ReaderT (Text -> IO (), Connection) IO a
  }
  deriving (Monad, Functor, Applicative, MonadIO, MonadFail)

runBeamDuckDB :: Connection -> DuckDBM a -> IO a
runBeamDuckDB = runBeamDuckDBDebug (\_ -> pure ())

runBeamDuckDBDebug :: (Text -> IO ()) -> Connection -> DuckDBM a -> IO a
runBeamDuckDBDebug debug conn action =
  runReaderT (runDuckDBM action) (debug, conn)

type instance BeamSqlBackendSyntax DuckDB = DuckDBCommandSyntax

newtype BeamDuckDBParams = BeamDuckDBParams [SomeField]

instance ToRow BeamDuckDBParams where
  toRow (BeamDuckDBParams x) = map (\(SomeField f) -> toField f) x

instance MonadBeam DuckDB DuckDBM where
  runNoReturn (DuckDBCommandSyntax (DuckDBSyntax cmd vals)) =
    DuckDBM $ do
      (logger, conn) <- ask
      let cmdString = Text.Lazy.toStrict (Builder.toLazyText (withPlaceholder cmd))
      liftIO (logger (cmdString <> ";\n-- With values: " <> Text.pack (show (DL.toList vals))))
      liftIO (void $ execute conn (Query cmdString) (BeamDuckDBParams (DL.toList vals)))

  runReturningMany (DuckDBCommandSyntax (DuckDBSyntax cmd vals)) action =
    DuckDBM $ do
      (logger, conn) <- ask
      let cmdString = Text.Lazy.toStrict (Builder.toLazyText (withPlaceholder cmd))
      liftIO $ do
        logger (cmdString <> ";\n-- With values: " <> Text.pack (show (DL.toList vals)))
        withStatement conn (Query cmdString) $ \stmt ->
          do
            bind stmt (map toField (DL.toList vals))
            let nextRow' =
                  liftIO (nextRow stmt) >>= \case
                    Nothing -> pure Nothing
                    Just (BeamDuckDBRow row) -> pure row
            runReaderT (runDuckDBM (action nextRow')) (logger, conn)

newtype BeamDuckDBRow a = BeamDuckDBRow a

instance (FromBackendRow DuckDB a) => FromRow (BeamDuckDBRow a) where
  fromRow = BeamDuckDBRow <$> runF fromBackendRow' finish step
    where
      FromBackendRowM fromBackendRow' = fromBackendRow

      translateErrors :: Maybe Int -> SomeException -> Maybe SomeException
      translateErrors col (SomeException e) =
        case cast e of
          Just
            ( ConversionFailed
                { errSQLType = typeString,
                  errHaskellType = hsString,
                  errMessage = msg
                }
              ) ->
              Just
                ( SomeException
                    ( BeamRowReadError
                        col
                        ( ColumnTypeMismatch
                            (Text.unpack hsString)
                            (Text.unpack typeString)
                            ("conversion failed: " ++ Text.unpack msg)
                        )
                    )
                )
          Just (UnexpectedNull {}) ->
            Just (SomeException (BeamRowReadError col ColumnUnexpectedNull))
          Just
            ( Incompatible
                { errSQLType = typeString,
                  errHaskellType = hsString,
                  errMessage = msg
                }
              ) ->
              Just
                ( SomeException
                    ( BeamRowReadError
                        col
                        ( ColumnTypeMismatch
                            (Text.unpack hsString)
                            (Text.unpack typeString)
                            ("incompatible: " ++ Text.unpack msg)
                        )
                    )
                )
          Nothing -> Nothing

      finish = pure

      step :: forall a'. FromBackendRowF DuckDB (RowParser a') -> RowParser a'
      step (ParseOneField next) =
        RowParser $ ReaderT $ \ro -> StateT $ \st@(col, _) ->
          case runStateT (runReaderT (runRowParser field) ro) st of
            Ok (x, st') -> runStateT (runReaderT (runRowParser (next x)) ro) st'
            Errors errs -> Errors (mapMaybe (translateErrors (Just col)) errs)
      step (Alt (FromBackendRowM a) (FromBackendRowM b) next) = do
        RowParser $ do
          let RowParser a' = runF a finish step
              RowParser b' = runF b finish step

          st <- lift get
          ro <- ask
          case runStateT (runReaderT a' ro) st of
            Ok (ra, st') -> do
              lift $ put st'
              runRowParser (next ra)
            Errors aErrs ->
              case runStateT (runReaderT b' ro) st of
                Ok (rb, st') -> do
                  lift $ put st'
                  runRowParser (next rb)
                Errors bErrs ->
                  lift (lift (Errors (aErrs ++ bErrs)))
      step (FailParseWith err) = RowParser (lift (lift (Errors [SomeException err])))

data DuckDB

instance BeamSqlBackend DuckDB

instance HasQBuilder DuckDB where
  buildSqlQuery = buildSql92Query' True

instance HasSqlInTable DuckDB where
  inRowValuesE Proxy e es =
    DuckDBExpressionSyntax $
      mconcat
        [ parens $ fromDuckDBExpression e,
          emit " IN ",
          parens $ emit "VALUES " <> commas (map fromDuckDBExpression es)
        ]

instance BeamSqlBackendIsString DuckDB Text

instance BeamSqlBackendIsString DuckDB String

instance BeamBackend DuckDB where
  type BackendFromField DuckDB = FromField

instance FromBackendRow DuckDB SqlNull where
  fromBackendRow = parseOneField @DuckDB @Null $> SqlNull

instance FromBackendRow DuckDB Bool

instance FromBackendRow DuckDB Float

instance FromBackendRow DuckDB Double

instance FromBackendRow DuckDB Integer

instance FromBackendRow DuckDB Int

instance FromBackendRow DuckDB Int8

instance FromBackendRow DuckDB Int16

instance FromBackendRow DuckDB Int32

instance FromBackendRow DuckDB Int64

instance FromBackendRow DuckDB Word

instance FromBackendRow DuckDB Word8

instance FromBackendRow DuckDB Word16

instance FromBackendRow DuckDB Word32

instance FromBackendRow DuckDB Word64

instance FromBackendRow DuckDB Text

instance FromBackendRow DuckDB ByteString

instance FromBackendRow DuckDB UUID

instance FromBackendRow DuckDB Day

instance FromBackendRow DuckDB TimeOfDay

instance FromBackendRow DuckDB LocalTime

instance FromBackendRow DuckDB UTCTime

instance HasSqlEqualityCheck DuckDB Bool

instance HasSqlEqualityCheck DuckDB Float

instance HasSqlEqualityCheck DuckDB Double

instance HasSqlEqualityCheck DuckDB Integer

instance HasSqlEqualityCheck DuckDB Int

instance HasSqlEqualityCheck DuckDB Int8

instance HasSqlEqualityCheck DuckDB Int16

instance HasSqlEqualityCheck DuckDB Int32

instance HasSqlEqualityCheck DuckDB Int64

instance HasSqlEqualityCheck DuckDB Word

instance HasSqlEqualityCheck DuckDB Word8

instance HasSqlEqualityCheck DuckDB Word16

instance HasSqlEqualityCheck DuckDB Word32

instance HasSqlEqualityCheck DuckDB Word64

instance HasSqlEqualityCheck DuckDB Text

instance HasSqlEqualityCheck DuckDB ByteString

instance HasSqlEqualityCheck DuckDB UUID

instance HasSqlEqualityCheck DuckDB Day

instance HasSqlEqualityCheck DuckDB TimeOfDay

instance HasSqlEqualityCheck DuckDB LocalTime

instance HasSqlEqualityCheck DuckDB UTCTime

instance HasSqlQuantifiedEqualityCheck DuckDB Bool

instance HasSqlQuantifiedEqualityCheck DuckDB Float

instance HasSqlQuantifiedEqualityCheck DuckDB Double

instance HasSqlQuantifiedEqualityCheck DuckDB Integer

instance HasSqlQuantifiedEqualityCheck DuckDB Int

instance HasSqlQuantifiedEqualityCheck DuckDB Int8

instance HasSqlQuantifiedEqualityCheck DuckDB Int16

instance HasSqlQuantifiedEqualityCheck DuckDB Int32

instance HasSqlQuantifiedEqualityCheck DuckDB Int64

instance HasSqlQuantifiedEqualityCheck DuckDB Word

instance HasSqlQuantifiedEqualityCheck DuckDB Word8

instance HasSqlQuantifiedEqualityCheck DuckDB Word16

instance HasSqlQuantifiedEqualityCheck DuckDB Word32

instance HasSqlQuantifiedEqualityCheck DuckDB Word64

instance HasSqlQuantifiedEqualityCheck DuckDB Text

instance HasSqlQuantifiedEqualityCheck DuckDB ByteString

instance HasSqlQuantifiedEqualityCheck DuckDB UUID

instance HasSqlQuantifiedEqualityCheck DuckDB Day

instance HasSqlQuantifiedEqualityCheck DuckDB TimeOfDay

instance HasSqlQuantifiedEqualityCheck DuckDB LocalTime

instance HasSqlQuantifiedEqualityCheck DuckDB UTCTime
