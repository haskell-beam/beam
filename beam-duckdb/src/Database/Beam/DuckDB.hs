{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | DuckDB is a powerful in-process database specialized in analytical processing workloads.
--
-- The @beam-duckdb@ library is built atop of @duckdb-simple@, which is
-- used for connection management, transaction support, serialization, and
-- deserialization.
--
-- @beam-duckdb@ supports most beam features as well as many DuckDB-specific
-- features, such as support for reading Parquet files and Apache Iceberg tables.
module Database.Beam.DuckDB
  ( -- * Executing DuckDB queries
    runBeamDuckDB,
    runBeamDuckDBDebug,

    -- * Backend datatype
    DuckDB,
    DuckDBM,

    -- * DuckDB-specific functionality

    -- ** Parquet support

    -- *** Specifying Parquet files as sources of data
    parquetFile,
    modifyParquetFileFields,
    ParquetFileEntity,

    -- *** Specifying the source of Parquet-encoded data
    ParquetSource,
    singleParquetFile,
    multipleParquetFiles,

    -- *** Querying data from a Parquet file
    allFromParquet_,

    -- ** Apache Iceberg tables support

    -- *** Specifying Iceberg tables as part of the database
    icebergTable,
    icebergTableWith,
    modifyIcebergTableFields,
    IcebergTableEntity,

    -- *** Iceberg table options
    IcebergTableOptions (..),
    defaultIcebergTableOptions,

    -- *** Querying data from a Parquet file
    allFromIceberg_,
  )
where

import Control.Exception (SomeException (..))
import Control.Monad (void)
import Control.Monad.Free.Church (runF)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad.Trans.State.Strict (StateT (..), get, put)
import qualified Data.DList as DL
import Data.Data (cast)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Database.Beam.Backend
  ( BeamRowReadError (..),
    ColumnParseError (..),
    FromBackendRow,
    FromBackendRowF (..),
    FromBackendRowM (..),
    MonadBeam,
  )
import Database.Beam.Backend.SQL (FromBackendRow (..), MonadBeam (..))
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Syntax
  ( DuckDBCommandSyntax (..),
  )
import Database.Beam.DuckDB.Syntax.Builder
  ( DuckDBSyntax (..),
    SomeField (..),
    withPlaceholder,
  )
import Database.Beam.DuckDB.Syntax.Extensions
import Database.DuckDB.Simple (Connection, FromRow, Query (Query), ResultError (..), RowParser, ToField (toField), ToRow (toRow), bind, execute, nextRow, withStatement)
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

-- | Execute queries on a DuckDB 'Connection'.
--
-- To trace the exact SQL statements being generated,
-- use 'runBeamDuckDBDebug' instead.
runBeamDuckDB :: Connection -> DuckDBM a -> IO a
runBeamDuckDB = runBeamDuckDBDebug (\_ -> pure ())

runBeamDuckDBDebug :: (Text -> IO ()) -> Connection -> DuckDBM a -> IO a
runBeamDuckDBDebug debug conn action =
  runReaderT (runDuckDBM action) (debug, conn)

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
