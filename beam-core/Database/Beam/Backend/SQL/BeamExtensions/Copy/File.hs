{-# LANGUAGE UndecidableInstances #-}

-- | File-mode @COPY@: the data lives on the database server's filesystem.
--
-- For the variant that streams data through the client connection instead, see
-- "Database.Beam.Backend.SQL.BeamExtensions.Copy.Stream".
module Database.Beam.Backend.SQL.BeamExtensions.Copy.File
  ( -- * Building a COPY statement
    copyTableTo,
    copySelectTo,
    SqlCopyTo (..),
    copyTableFrom,
    SqlCopyFrom (..),

    -- * Source-syntax classes (shared with streaming COPY)
    IsSqlCopyToSourceSyntax (..),
    IsSqlCopyFromSourceSyntax (..),

    -- * File-mode statement-level syntax classes
    IsSqlCopyToSyntax (..),
    IsSqlCopyFromSyntax (..),
    BeamSqlBackendCopyToSyntax,
    BeamSqlBackendCopyFromSyntax,

    -- * Runner classes
    MonadBeamCopyTo (..),
    MonadBeamCopyFrom (..),

    -- * Internal — exposed for use by sibling modules
    projection,
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
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Database.Beam.Backend.SQL (BeamSqlBackendSelectSyntax, MonadBeam (..))
import Database.Beam.Query (QField, SqlSelect (..))
import Database.Beam.Query.Internal (AnyType, ProjectibleWithPredicate, QField (..), project')
import Database.Beam.Schema (TableEntity)
import Database.Beam.Schema.Tables
  ( Beamable,
    Columnar' (..),
    DatabaseEntity (..),
    DatabaseEntityDescriptor (DatabaseTable, dbTableSettings),
    IsDatabaseEntity (dbEntityName, dbEntitySchema),
    changeBeamRep,
    fieldName,
  )
import Lens.Micro ((^.))

-- | This class allows to express the source of a `COPY ... TO` statement.
-- The options are either from a table (with a possible projection),
-- or from the result of a select query.
--
-- Reused by both file-mode and streaming COPY: the source shape (a table or
-- a @SELECT@) is the same regardless of where the data ends up.
class IsSqlCopyToSourceSyntax syntax where
  -- | Expected to be equal to `BeamSqlBackendSelectSyntax be`
  type SqlCopyToSourceSelectSyntax syntax :: Type

  -- Copy an entire table, perhaps with some column projections
  copyTableToSyntax ::
    Maybe Text -> -- schema (Nothing = default search path)
    Text -> -- table name
    Maybe (NonEmpty Text) -> -- column list; `Nothing` means "all columns"
    syntax

  -- Copy the result of a select statement
  copySelectToSyntax ::
    SqlCopyToSourceSelectSyntax syntax ->
    syntax

-- | This class allows to express the source of a `COPY ... FROM` statement.
-- Reused by both file-mode and streaming COPY.
class IsSqlCopyFromSourceSyntax syntax where
  -- Copy data into a table, perhaps with some column projections
  copyTableFromSyntax ::
    Maybe Text -> -- schema (Nothing = default search path)
    Text -> -- table name
    Maybe (NonEmpty Text) -> -- column list; `Nothing` means "all columns"
    syntax

-- | Statement-level syntax for backends that support @COPY ... TO@ file.
class (IsSqlCopyToSourceSyntax (SqlCopyToSourceSyntax cmd)) => IsSqlCopyToSyntax cmd where
  -- | The syntax for the source of the copy. For example, in Postgres,
  --  this can be either a table (and optional projection), or a select statement.
  type SqlCopyToSourceSyntax cmd :: Type

  -- | All backend-specific options which determine HOW the copy is performed,
  --  including the destination of the data (e.g. a filepath).
  type SqlCopyToParams cmd :: Type

  -- | Combine a source and a parameters value into a complete
  -- @COPY ... TO ...@ statement.
  copyToStmt ::
    SqlCopyToSourceSyntax cmd ->
    SqlCopyToParams cmd ->
    cmd

-- | Statement-level syntax for backends that support file-mode @COPY ... FROM@ file.
--
-- Symmetric to 'IsSqlCopyToSyntax', but the data flows in the opposite
-- direction: the params value supplies the source path and any
-- format-specific options.
class (IsSqlCopyFromSourceSyntax (SqlCopyFromSourceSyntax cmd)) => IsSqlCopyFromSyntax cmd where
  -- | The syntax for the destination table of the copy.
  type SqlCopyFromSourceSyntax cmd :: Type

  -- | All backend-specific options which determine HOW the copy is performed,
  --  including the source of the data (e.g. a filepath).
  type SqlCopyFromParams cmd :: Type

  -- | Combine a destination and a parameters value into a complete
  -- @COPY ... FROM ...@ statement.
  copyFromStmt ::
    SqlCopyFromSourceSyntax cmd ->
    SqlCopyFromParams cmd ->
    cmd

-- | Type-family selector for the backend's file-mode @COPY ... TO@ syntax.
-- A backend instance binds this to the concrete syntax type that implements
-- 'IsSqlCopyToSyntax'.
type family BeamSqlBackendCopyToSyntax be :: Type

-- | Type-family selector for the backend's file-mode @COPY ... FROM@ syntax.
-- See 'BeamSqlBackendCopyToSyntax'.
type family BeamSqlBackendCopyFromSyntax be :: Type

-- | A built file-mode @COPY ... TO@ statement, ready to be executed by
-- 'runCopyTo'. Construct via 'copyTableTo' or 'copySelectTo'; the @table@
-- and @proj@ phantom parameters track which table the statement applies to
-- and which columns it projects.
data SqlCopyTo be (table :: (Type -> Type) -> Type) proj
  = SqlCopyTo !(BeamSqlBackendCopyToSyntax be)
  | -- | A projection covering zero columns. 'runCopyTo' should treat this as a
    --    no-op rather than emit an empty @COPY tbl () TO ...@ statement.
    SqlCopyToNoColumns

-- | A built file-mode @COPY ... FROM@ statement, ready to be executed by
-- 'runCopyFrom'. Construct via 'copyTableFrom'.
data SqlCopyFrom be (table :: (Type -> Type) -> Type) proj
  = SqlCopyFrom !(BeamSqlBackendCopyFromSyntax be)
  | -- | A projection covering zero columns. 'runCopyFrom' should treat this as
    --    a no-op rather than emit an empty @COPY tbl () FROM ...@ statement.
    SqlCopyFromNoColumns

-- | Express the copy from a table, to a destination (typically a file).
--
-- To copy the result of a @SELECT@ query instead, see `copySelectTo`.
--
-- @since 0.11.1.0
copyTableTo ::
  ( IsSqlCopyToSyntax (BeamSqlBackendCopyToSyntax be),
    ProjectibleWithPredicate AnyType () Text proj
  ) =>
  DatabaseEntity be db (TableEntity table) ->
  -- | Projection from table to columns. If you want
  --  to copy the entire table, use 'id'.
  (table (QField s) -> proj) ->
  -- | Backend-specific options. The output is also determined by this value
  SqlCopyToParams (BeamSqlBackendCopyToSyntax be) ->
  SqlCopyTo be table proj
copyTableTo (DatabaseEntity dt@(DatabaseTable {})) mkProj options =
  case nonEmpty (projection dt mkProj) of
    Nothing -> SqlCopyToNoColumns
    Just cols ->
      let source = copyTableToSyntax (dt ^. dbEntitySchema) (dt ^. dbEntityName) (Just cols)
       in SqlCopyTo
            (copyToStmt source options)

-- | Express the copy from the result of a @SELECT@ statement to a destination
-- (typically a file). Use this when the source rows are produced by a query
-- rather than read directly from a table.
--
-- To copy a table, or a subset of columns, see `copyTableTo`.
--
-- @since 0.11.1.0
copySelectTo ::
  ( IsSqlCopyToSyntax (BeamSqlBackendCopyToSyntax be),
    SqlCopyToSourceSelectSyntax (SqlCopyToSourceSyntax (BeamSqlBackendCopyToSyntax be))
      ~ BeamSqlBackendSelectSyntax be
  ) =>
  SqlSelect be a ->
  -- | Backend-specific options. The format is pinned by this value
  SqlCopyToParams (BeamSqlBackendCopyToSyntax be) ->
  SqlCopyTo be table proj
copySelectTo (SqlSelect selectSyntax) options =
  let source = copySelectToSyntax selectSyntax
   in SqlCopyTo (copyToStmt source options)

-- | Express the copy to a table, from an external source (typically a file).
--
-- @since 0.11.1.0
copyTableFrom ::
  ( IsSqlCopyFromSyntax (BeamSqlBackendCopyFromSyntax be),
    ProjectibleWithPredicate AnyType () Text proj
  ) =>
  DatabaseEntity be db (TableEntity table) ->
  -- | Projection from table to columns. If you want
  --  to copy into the entire table, use 'id'.
  (table (QField s) -> proj) ->
  -- | Backend-specific options. The format is pinned by this value
  SqlCopyFromParams (BeamSqlBackendCopyFromSyntax be) ->
  SqlCopyFrom be table proj
copyTableFrom (DatabaseEntity dt@(DatabaseTable {})) mkProj options =
  case nonEmpty (projection dt mkProj) of
    Nothing -> SqlCopyFromNoColumns
    Just cols ->
      let source = copyTableFromSyntax (dt ^. dbEntitySchema) (dt ^. dbEntityName) (Just cols)
       in SqlCopyFrom
            (copyFromStmt source options)

-- | Walk a projection and collect the names of the columns it touches.
--
-- This is the building block 'copyTableTo' / 'copyTableFrom' (and their
-- streaming counterparts) use to derive the column list for the emitted
-- @COPY@ statement. Exposed here so that the streaming submodule can reuse
-- the same logic without duplication.
projection ::
  (ProjectibleWithPredicate AnyType () Text proj, Beamable table) =>
  DatabaseEntityDescriptor be (TableEntity table) ->
  (table (QField s) -> proj) ->
  [Text]
projection dt mkProj =
  Strict.execWriter
    ( project'
        (Proxy @AnyType)
        (Proxy @((), Text))
        (\_ _ f -> Strict.tell [f] >> pure f)
        (mkProj tblFields)
    )
  where
    tblFields =
      changeBeamRep
        (\(Columnar' fd) -> Columnar' (QField False (dt ^. dbEntityName) (fd ^. fieldName)))
        (dbTableSettings dt)

-- | 'MonadBeam's that support copying data out of a database, to some other location.
--
-- See 'MonadBeamCopyFrom' for the inverse operation.
--
-- @since 0.11.1.0
class (MonadBeam be m) => MonadBeamCopyTo be m | m -> be where
  -- | Execute a built @COPY ... TO@ file statement.
  runCopyTo :: SqlCopyTo be table proj -> m ()

instance (MonadBeamCopyTo be m) => MonadBeamCopyTo be (ExceptT e m) where
  runCopyTo = lift . runCopyTo

instance (MonadBeamCopyTo be m) => MonadBeamCopyTo be (ContT r m) where
  runCopyTo = lift . runCopyTo

instance (MonadBeamCopyTo be m) => MonadBeamCopyTo be (ReaderT r m) where
  runCopyTo = lift . runCopyTo

instance (MonadBeamCopyTo be m) => MonadBeamCopyTo be (Lazy.StateT r m) where
  runCopyTo = lift . runCopyTo

instance (MonadBeamCopyTo be m) => MonadBeamCopyTo be (Strict.StateT r m) where
  runCopyTo = lift . runCopyTo

instance (MonadBeamCopyTo be m, Monoid r) => MonadBeamCopyTo be (Lazy.WriterT r m) where
  runCopyTo = lift . runCopyTo

instance (MonadBeamCopyTo be m, Monoid r) => MonadBeamCopyTo be (Strict.WriterT r m) where
  runCopyTo = lift . runCopyTo

instance (MonadBeamCopyTo be m, Monoid w) => MonadBeamCopyTo be (Lazy.RWST r w s m) where
  runCopyTo = lift . runCopyTo

instance (MonadBeamCopyTo be m, Monoid w) => MonadBeamCopyTo be (Strict.RWST r w s m) where
  runCopyTo = lift . runCopyTo

-- | 'MonadBeam's that support copying data into database, from other location.
--
-- See 'MonadBeamCopyTo' for the inverse operation.
--
-- @since 0.11.1.0
class (MonadBeam be m) => MonadBeamCopyFrom be m | m -> be where
  -- | Execute a built @COPY ... FROM@ file statement.
  runCopyFrom :: SqlCopyFrom be table proj -> m ()

instance (MonadBeamCopyFrom be m) => MonadBeamCopyFrom be (ExceptT e m) where
  runCopyFrom = lift . runCopyFrom

instance (MonadBeamCopyFrom be m) => MonadBeamCopyFrom be (ContT r m) where
  runCopyFrom = lift . runCopyFrom

instance (MonadBeamCopyFrom be m) => MonadBeamCopyFrom be (ReaderT r m) where
  runCopyFrom = lift . runCopyFrom

instance (MonadBeamCopyFrom be m) => MonadBeamCopyFrom be (Lazy.StateT r m) where
  runCopyFrom = lift . runCopyFrom

instance (MonadBeamCopyFrom be m) => MonadBeamCopyFrom be (Strict.StateT r m) where
  runCopyFrom = lift . runCopyFrom

instance (MonadBeamCopyFrom be m, Monoid r) => MonadBeamCopyFrom be (Lazy.WriterT r m) where
  runCopyFrom = lift . runCopyFrom

instance (MonadBeamCopyFrom be m, Monoid r) => MonadBeamCopyFrom be (Strict.WriterT r m) where
  runCopyFrom = lift . runCopyFrom

instance (MonadBeamCopyFrom be m, Monoid w) => MonadBeamCopyFrom be (Lazy.RWST r w s m) where
  runCopyFrom = lift . runCopyFrom

instance (MonadBeamCopyFrom be m, Monoid w) => MonadBeamCopyFrom be (Strict.RWST r w s m) where
  runCopyFrom = lift . runCopyFrom
