{-# LANGUAGE UndecidableInstances #-}

-- | Streaming-mode @COPY@: the data flows through the client connection
-- rather than to or from a file on the database server.
--
-- For the file-mode variant, see
-- "Database.Beam.Backend.SQL.BeamExtensions.Copy.File".
module Database.Beam.Backend.SQL.BeamExtensions.Copy.Stream
  ( -- * Building a streaming COPY statement
    copyTableToStream,
    copySelectToStream,
    SqlCopyToStream (..),
    copyTableFromStream,
    SqlCopyFromStream (..),

    -- * Streaming statement-level syntax classes
    IsSqlCopyToStreamSyntax (..),
    IsSqlCopyFromStreamSyntax (..),
    BeamSqlBackendCopyToStreamSyntax,
    BeamSqlBackendCopyFromStreamSyntax,

    -- * Runner classes
    MonadBeamCopyToStream (..),
    MonadBeamCopyFromStream (..),
  )
where

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans (lift)
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Database.Beam.Backend.SQL (BeamSqlBackendSelectSyntax, MonadBeam)
import Database.Beam.Backend.SQL.BeamExtensions.Copy.File
  ( IsSqlCopyFromSourceSyntax (..),
    IsSqlCopyToSourceSyntax (..),
    projection,
  )
import Database.Beam.Query (QField, SqlSelect (..))
import Database.Beam.Query.Internal (AnyType, ProjectibleWithPredicate)
import Database.Beam.Schema (TableEntity)
import Database.Beam.Schema.Tables
  ( DatabaseEntity (..),
    DatabaseEntityDescriptor (DatabaseTable),
    IsDatabaseEntity (dbEntityName, dbEntitySchema),
  )
import Lens.Micro ((^.))

-- | Statement-level syntax for backends that support streaming
-- @COPY ... TO@ (data leaving the database through the client connection).
--
-- Mirrors 'Database.Beam.Backend.SQL.BeamExtensions.Copy.File.IsSqlCopyToSyntax',
-- but the params value carries only format options — no destination path,
-- since chunks travel through the wire.
class (IsSqlCopyToSourceSyntax (SqlCopyToStreamSourceSyntax cmd)) => IsSqlCopyToStreamSyntax cmd where
  -- | The syntax for the source of the streaming copy. As with file-mode
  --  COPY, this can be a table (perhaps with a projection) or a @SELECT@
  --  query.
  type SqlCopyToStreamSourceSyntax cmd :: Type

  -- | All backend-specific options which determine HOW the streaming copy
  --  is performed (format, delimiter, etc.). Unlike the file-mode params,
  --  there is no destination — chunks flow through the connection.
  type SqlCopyToStreamParams cmd :: Type

  -- | Combine a source and a parameters value into a complete
  -- streaming @COPY ... TO@ stream statement.
  copyToStreamStmt ::
    SqlCopyToStreamSourceSyntax cmd ->
    SqlCopyToStreamParams cmd ->
    cmd

-- | Statement-level syntax for backends that support streaming
-- @COPY ... FROM@ (data entering the database through the client connection).
--
-- Mirrors 'Database.Beam.Backend.SQL.BeamExtensions.Copy.File.IsSqlCopyFromSyntax'.
class (IsSqlCopyFromSourceSyntax (SqlCopyFromStreamSourceSyntax cmd)) => IsSqlCopyFromStreamSyntax cmd where
  -- | The syntax for the destination table of the streaming copy.
  type SqlCopyFromStreamSourceSyntax cmd :: Type

  -- | All backend-specific options which determine HOW the streaming copy
  --  is performed. Unlike the file-mode params, there is no source path —
  --  chunks flow through the connection.
  type SqlCopyFromStreamParams cmd :: Type

  -- | Combine a destination and a parameters value into a complete
  -- streaming @COPY ... FROM@ stream statement.
  copyFromStreamStmt ::
    SqlCopyFromStreamSourceSyntax cmd ->
    SqlCopyFromStreamParams cmd ->
    cmd

-- | Type-family selector for the backend's streaming @COPY ... TO@ syntax.
-- A backend instance binds this to the concrete syntax type that
-- implements 'IsSqlCopyToStreamSyntax'.
type family BeamSqlBackendCopyToStreamSyntax be :: Type

-- | Type-family selector for the backend's streaming @COPY ... FROM@ syntax.
-- See 'BeamSqlBackendCopyToStreamSyntax'.
type family BeamSqlBackendCopyFromStreamSyntax be :: Type

-- | A built streaming-mode @COPY ... TO@ statement, ready to be executed by
-- 'runCopyToStream'.
data SqlCopyToStream be a
  = SqlCopyToStream !(BeamSqlBackendCopyToStreamSyntax be)
  | -- | A projection covering zero columns. 'runCopyToStream' should treat
    --    this as a no-op (it must still call the sink zero times) rather
    --    than emit an empty @COPY tbl () TO STDOUT@ statement.
    SqlCopyToStreamNoColumns

-- | A built streaming-mode @COPY ... FROM@ statement, ready to be executed
-- by 'runCopyFromStream'.
data SqlCopyFromStream be a
  = SqlCopyFromStream !(BeamSqlBackendCopyFromStreamSyntax be)
  | -- | A projection covering zero columns. 'runCopyFromStream' should
    --    treat this as a no-op (it should not pull from the source) rather
    --    than emit an empty @COPY tbl () FROM STDIN@ statement.
    SqlCopyFromStreamNoColumns

-- | Express a streaming copy from a table, the data flowing through the
-- client connection rather than to a server-side file.
--
-- To stream the result of a @SELECT@ query instead, see 'copySelectToStream'.
--
-- @since 0.11.1.0
copyTableToStream ::
  ( IsSqlCopyToStreamSyntax (BeamSqlBackendCopyToStreamSyntax be),
    ProjectibleWithPredicate AnyType () Text proj
  ) =>
  DatabaseEntity be db (TableEntity table) ->
  -- | Projection from table to columns. If you want
  --  to copy the entire table, use 'id'.
  (table (QField s) -> proj) ->
  -- | Backend-specific options.
  SqlCopyToStreamParams (BeamSqlBackendCopyToStreamSyntax be) ->
  SqlCopyToStream be proj
copyTableToStream (DatabaseEntity dt@(DatabaseTable {})) mkProj options =
  case nonEmpty (projection dt mkProj) of
    Nothing -> SqlCopyToStreamNoColumns
    Just cols ->
      let source = copyTableToSyntax (dt ^. dbEntitySchema) (dt ^. dbEntityName) (Just cols)
       in SqlCopyToStream
            (copyToStreamStmt source options)

-- | Express a streaming copy from the result of a @SELECT@ statement.
--
-- To stream a table, or a subset of columns, see 'copyTableToStream'.
--
-- @since 0.11.1.0
copySelectToStream ::
  ( IsSqlCopyToStreamSyntax (BeamSqlBackendCopyToStreamSyntax be),
    SqlCopyToSourceSelectSyntax (SqlCopyToStreamSourceSyntax (BeamSqlBackendCopyToStreamSyntax be))
      ~ BeamSqlBackendSelectSyntax be
  ) =>
  SqlSelect be a ->
  SqlCopyToStreamParams (BeamSqlBackendCopyToStreamSyntax be) ->
  SqlCopyToStream be a
copySelectToStream (SqlSelect selectSyntax) options =
  let source = copySelectToSyntax selectSyntax
   in SqlCopyToStream (copyToStreamStmt source options)

-- | Express a streaming copy into a table, the data flowing through the
-- client connection rather than from a server-side file.
--
-- @since 0.11.1.0
copyTableFromStream ::
  ( IsSqlCopyFromStreamSyntax (BeamSqlBackendCopyFromStreamSyntax be),
    ProjectibleWithPredicate AnyType () Text proj
  ) =>
  DatabaseEntity be db (TableEntity table) ->
  -- | Projection from which to copy columns. Other columns
  -- will have their default value inserted.
  --
  -- To copy the stream into the entire table, use 'id'.
  (table (QField s) -> proj) ->
  SqlCopyFromStreamParams (BeamSqlBackendCopyFromStreamSyntax be) ->
  SqlCopyFromStream be proj
copyTableFromStream (DatabaseEntity dt@(DatabaseTable {})) mkProj options =
  case nonEmpty (projection dt mkProj) of
    Nothing -> SqlCopyFromStreamNoColumns
    Just cols ->
      let source = copyTableFromSyntax (dt ^. dbEntitySchema) (dt ^. dbEntityName) (Just cols)
       in SqlCopyFromStream
            (copyFromStreamStmt source options)

-- | 'MonadBeam's that support streaming data out of a database through the
-- client connection (e.g. PostgreSQL's @COPY ... TO STDOUT@).
--
-- The supplied @ByteString -> IO ()@ callback is invoked once per chunk
-- received. The 'runCopyToStream' call blocks until the COPY completes; on
-- failure it raises the underlying backend's exception.
--
-- See 'MonadBeamCopyFromStream' for the inverse operation.
--
-- @since 0.11.1.0
class (MonadBeam be m) => MonadBeamCopyToStream be m | m -> be where
  -- | Execute a built streaming @COPY ... TO@ stream statement. The supplied sink
  -- is invoked from 'IO' once per chunk emitted by the server, in order;
  -- 'runCopyToStream' returns once the server signals end of stream.
  runCopyToStream ::
    SqlCopyToStream be a ->
    -- | Sink. Called once for each chunk of bytes the server emits.
    (ByteString -> IO ()) ->
    m ()

instance (MonadBeamCopyToStream be m) => MonadBeamCopyToStream be (ExceptT e m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

instance (MonadBeamCopyToStream be m) => MonadBeamCopyToStream be (ContT r m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

instance (MonadBeamCopyToStream be m) => MonadBeamCopyToStream be (ReaderT r m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

instance (MonadBeamCopyToStream be m) => MonadBeamCopyToStream be (Lazy.StateT r m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

instance (MonadBeamCopyToStream be m) => MonadBeamCopyToStream be (Strict.StateT r m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

instance (MonadBeamCopyToStream be m, Monoid r) => MonadBeamCopyToStream be (Lazy.WriterT r m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

instance (MonadBeamCopyToStream be m, Monoid r) => MonadBeamCopyToStream be (Strict.WriterT r m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

instance (MonadBeamCopyToStream be m, Monoid w) => MonadBeamCopyToStream be (Lazy.RWST r w s m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

instance (MonadBeamCopyToStream be m, Monoid w) => MonadBeamCopyToStream be (Strict.RWST r w s m) where
  runCopyToStream s cb = lift (runCopyToStream s cb)

-- | 'MonadBeam's that support streaming data into a database through the
-- client connection (e.g. PostgreSQL's @COPY ... FROM STDIN@).
--
-- The supplied @IO (Maybe ByteString)@ source is pulled repeatedly until it
-- returns 'Nothing', signalling end of data. 'runCopyFromStream' blocks
-- until the COPY commits; on failure it raises the underlying backend's
-- exception.
--
-- See 'MonadBeamCopyToStream' for the inverse operation.
--
-- @since 0.11.1.0
class (MonadBeam be m) => MonadBeamCopyFromStream be m | m -> be where
  -- | Execute a built streaming @COPY ... FROM@ statement. The supplied
  -- producer is pulled from 'IO' until it returns 'Nothing'; each 'Just'
  -- chunk is forwarded to the server in order. 'runCopyFromStream' returns
  -- once the server has acknowledged the end of stream.
  runCopyFromStream ::
    SqlCopyFromStream be a ->
    -- | Source. Called repeatedly. 'Nothing' signals end of data.
    IO (Maybe ByteString) ->
    m ()

instance (MonadBeamCopyFromStream be m) => MonadBeamCopyFromStream be (ExceptT e m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)

instance (MonadBeamCopyFromStream be m) => MonadBeamCopyFromStream be (ContT r m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)

instance (MonadBeamCopyFromStream be m) => MonadBeamCopyFromStream be (ReaderT r m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)

instance (MonadBeamCopyFromStream be m) => MonadBeamCopyFromStream be (Lazy.StateT r m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)

instance (MonadBeamCopyFromStream be m) => MonadBeamCopyFromStream be (Strict.StateT r m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)

instance (MonadBeamCopyFromStream be m, Monoid r) => MonadBeamCopyFromStream be (Lazy.WriterT r m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)

instance (MonadBeamCopyFromStream be m, Monoid r) => MonadBeamCopyFromStream be (Strict.WriterT r m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)

instance (MonadBeamCopyFromStream be m, Monoid w) => MonadBeamCopyFromStream be (Lazy.RWST r w s m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)

instance (MonadBeamCopyFromStream be m, Monoid w) => MonadBeamCopyFromStream be (Strict.RWST r w s m) where
  runCopyFromStream s producer = lift (runCopyFromStream s producer)
