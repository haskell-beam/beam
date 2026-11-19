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

-- | Internal module providing the 'DuckDBM' monad and core 'MonadBeam'
-- instances for the DuckDB backend. Re-exported from "Database.Beam.DuckDB".
module Database.Beam.DuckDB.Connection
  ( DuckDBM (..),
    runBeamDuckDB,
    runBeamDuckDBDebug,
    runBeamDuckDBDebugString,
    liftIOWithHandle,
    BeamDuckDBParams (..),
    BeamDuckDBRow (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException (..))
import Control.Monad (void)
import Control.Monad.Free.Church (runF)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT)
import Control.Monad.Trans.State.Strict (gets, mapStateT)
import Data.Coerce (coerce)
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
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamCopyFrom (..), MonadBeamCopyTo (..), SqlCopyFrom (..), SqlCopyTo (..))
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Syntax (DuckDBCommandSyntax (..))
import Database.Beam.DuckDB.Syntax.Builder
  ( DuckDBSyntax (..),
    SomeField (..),
    withPlaceholder,
  )
import Database.Beam.DuckDB.Syntax.Extensions.Copy
  ( DuckDBCopyFromSyntax (..),
    DuckDBCopyToSyntax (..),
  )
import Database.DuckDB.Simple (Connection, FromRow, Query (Query), ResultError (..), RowParser, ToField (toField), ToRow (toRow), bind, execute, nextRow, withStatement)
import Database.DuckDB.Simple.Conversion (conversionError, mapConversionErrors)
import Database.DuckDB.Simple.FromRow (FromRow (..), RowParser (..), field)

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

-- | Like 'runBeamDuckDBDebug', but accepts a 'String' argument instead of 'Text'.
--
-- This is provided for compatibility with other backends
runBeamDuckDBDebugString :: (String -> IO ()) -> Connection -> DuckDBM a -> IO a
runBeamDuckDBDebugString debug = runBeamDuckDBDebug (debug . Text.unpack)

-- | Run an IO action with access to the underlying DuckDB 'Connection'.
-- This is mostly useful for migrate-backend implementors who need to issue
-- raw queries against the connection.
liftIOWithHandle :: (Connection -> IO a) -> DuckDBM a
liftIOWithHandle f = DuckDBM $ ReaderT $ \(_, conn) -> f conn

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

instance MonadBeamCopyTo DuckDB DuckDBM where
  runCopyTo SqlCopyToNoColumns = pure ()
  runCopyTo (SqlCopyTo syntax) = runNoReturn (coerce syntax)

instance MonadBeamCopyFrom DuckDB DuckDBM where
  runCopyFrom SqlCopyFromNoColumns = pure ()
  runCopyFrom (SqlCopyFrom syntax) = runNoReturn (coerce syntax)

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
            Just (SomeException (BeamRowReadError {brreColumn = col, brreError = ColumnUnexpectedNull}))
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
      step (ParseOneField next) = do
        col <- RowParser (lift (gets fst))
        x <-
          RowParser $
            mapReaderT
              (mapStateT (mapConversionErrors (mapMaybe (translateErrors (Just col)))))
              (runRowParser field)
        next x
      step (Alt (FromBackendRowM a) (FromBackendRowM b) next) =
        (runF a finish step <|> runF b finish step) >>= next
      step (FailParseWith err) = RowParser (lift (lift (conversionError err)))
