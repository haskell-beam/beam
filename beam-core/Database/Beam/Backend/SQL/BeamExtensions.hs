{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Some functionality is useful enough to be provided across backends, but is
-- not standardized. For example, many RDBMS systems provide ways of fetching
-- auto-incrementing or defaulting fields on INSERT or UPDATE.
--
-- Beam provides type classes that some backends instantiate that provide this
-- support. This uses direct means on sufficiently advanced backends and is
-- emulated on others.
module Database.Beam.Backend.SQL.BeamExtensions
  ( MonadBeamInsertReturning(..)
  , runInsertReturningList
  , MonadBeamUpdateReturning(..)
  , runUpdateReturningList
  , MonadBeamDeleteReturning(..)
  , runDeleteReturningList
  , BeamHasInsertOnConflict(..)

  -- * Support for copying to external files
  , module Database.Beam.Backend.SQL.BeamExtensions.Copy.File
  -- * Support for streaming copy
  , module Database.Beam.Backend.SQL.BeamExtensions.Copy.Stream

  , SqlSerial(..)
  , onConflictUpdateInstead
  , onConflictUpdateAll
  ) where

import           Database.Beam.Backend
import           Database.Beam.Query
import           Database.Beam.Query.Internal
import           Database.Beam.Schema
import           Database.Beam.Schema.Tables

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import           Data.Functor.Const
import           Data.Kind (Type)
import           Data.Proxy
import           Data.Semigroup
import           Database.Beam.Backend.SQL.BeamExtensions.Copy.File hiding (projection) -- internal function
import           Database.Beam.Backend.SQL.BeamExtensions.Copy.Stream

--import GHC.Generics

-- | 'MonadBeam's that support returning data from the newly created rows of an
--   @INSERT@ statement. Useful for discovering the real value of a defaulted
--   field (such as a serial primary key).
class MonadBeam be m =>
  MonadBeamInsertReturning be m | m -> be where
  -- | Execute an @INSERT@ statement and return a list of values projected from
  --   the inserted rows. The projection function selects which columns are
  --   returned: pass 'id' to return the full row, or supply a custom
  --   projection to return a subset.
  runInsertReturningListWith
    :: ( Beamable table
       , Projectible be a
       , FromBackendRow be (QExprToIdentity a) )
    => SqlInsert be table
    -> (table (QExpr be ()) -> a)
    -> m [QExprToIdentity a]

-- | Execute an @INSERT@ statement and return the inserted rows in full.
--   A convenience around 'runInsertReturningListWith' that uses 'id' as the
--   projection.
runInsertReturningList
  :: ( MonadBeamInsertReturning be m
     , Beamable table
     , Projectible be (table (QExpr be ()))
     , FromBackendRow be (table Identity) )
  => SqlInsert be table
  -> m [table Identity]
runInsertReturningList sql = runInsertReturningListWith sql id

instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ExceptT e m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ContT r m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ReaderT r m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (Lazy.StateT r m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (Strict.StateT r m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj
instance (MonadBeamInsertReturning be m, Monoid r)
    => MonadBeamInsertReturning be (Lazy.WriterT r m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj
instance (MonadBeamInsertReturning be m, Monoid r)
    => MonadBeamInsertReturning be (Strict.WriterT r m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj
instance (MonadBeamInsertReturning be m, Monoid w)
    => MonadBeamInsertReturning be (Lazy.RWST r w s m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj
instance (MonadBeamInsertReturning be m, Monoid w)
    => MonadBeamInsertReturning be (Strict.RWST r w s m) where
    runInsertReturningListWith sql proj = lift $ runInsertReturningListWith sql proj

-- | 'MonadBeam's that support returning data from the updated rows of an
--   @UPDATE@ statement. Useful for observing the post-update values of the
--   affected rows.
class MonadBeam be m =>
  MonadBeamUpdateReturning be m | m -> be where
  -- | Execute an @UPDATE@ statement and return a list of values projected from
  --   the updated rows. The projection function selects which columns are
  --   returned: pass 'id' to return the full row, or supply a custom
  --   projection to return a subset.
  runUpdateReturningListWith
    :: ( Beamable table
       , Projectible be a
       , FromBackendRow be (QExprToIdentity a) )
    => SqlUpdate be table
    -> (table (QExpr be ()) -> a)
    -> m [QExprToIdentity a]

-- | Execute an @UPDATE@ statement and return the updated rows in full.
--   A convenience around 'runUpdateReturningListWith' that uses 'id' as the
--   projection.
runUpdateReturningList
  :: ( MonadBeamUpdateReturning be m
     , Beamable table
     , Projectible be (table (QExpr be ()))
     , FromBackendRow be (table Identity) )
  => SqlUpdate be table
  -> m [table Identity]
runUpdateReturningList sql = runUpdateReturningListWith sql id

instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ExceptT e m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ContT r m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ReaderT r m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (Lazy.StateT r m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (Strict.StateT r m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj
instance (MonadBeamUpdateReturning be m, Monoid r)
    => MonadBeamUpdateReturning be (Lazy.WriterT r m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj
instance (MonadBeamUpdateReturning be m, Monoid r)
    => MonadBeamUpdateReturning be (Strict.WriterT r m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj
instance (MonadBeamUpdateReturning be m, Monoid w)
    => MonadBeamUpdateReturning be (Lazy.RWST r w s m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj
instance (MonadBeamUpdateReturning be m, Monoid w)
    => MonadBeamUpdateReturning be (Strict.RWST r w s m) where
    runUpdateReturningListWith sql proj = lift $ runUpdateReturningListWith sql proj

-- | 'MonadBeam's that support returning data from rows that will be deleted by
--   the given @DELETE@ statement. Useful for deallocating resources based on
--   the value of deleted rows.
class MonadBeam be m =>
  MonadBeamDeleteReturning be m | m -> be where
  -- | Execute a @DELETE@ statement and return a list of values projected from
  --   the deleted rows. The projection function selects which columns are
  --   returned: pass 'id' to return the full row, or supply a custom
  --   projection to return a subset.
  runDeleteReturningListWith
    :: ( Beamable table
       , Projectible be a
       , FromBackendRow be (QExprToIdentity a) )
    => SqlDelete be table
    -> (table (QExpr be ()) -> a)
    -> m [QExprToIdentity a]

-- | Execute a @DELETE@ statement and return the deleted rows in full.
--   A convenience around 'runDeleteReturningListWith' that uses 'id' as the
--   projection.
runDeleteReturningList
  :: ( MonadBeamDeleteReturning be m
     , Beamable table
     , Projectible be (table (QExpr be ()))
     , FromBackendRow be (table Identity) )
  => SqlDelete be table
  -> m [table Identity]
runDeleteReturningList sql = runDeleteReturningListWith sql id

instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ExceptT e m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ContT r m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ReaderT r m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (Lazy.StateT r m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (Strict.StateT r m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj
instance (MonadBeamDeleteReturning be m, Monoid r)
    => MonadBeamDeleteReturning be (Lazy.WriterT r m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj
instance (MonadBeamDeleteReturning be m, Monoid r)
    => MonadBeamDeleteReturning be (Strict.WriterT r m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj
instance (MonadBeamDeleteReturning be m, Monoid w)
    => MonadBeamDeleteReturning be (Lazy.RWST r w s m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj
instance (MonadBeamDeleteReturning be m, Monoid w)
    => MonadBeamDeleteReturning be (Strict.RWST r w s m) where
    runDeleteReturningListWith sql proj = lift $ runDeleteReturningListWith sql proj

class BeamSqlBackend be => BeamHasInsertOnConflict be where
  -- | Specifies the kind of constraint that must be violated for the action to occur
  data SqlConflictTarget be (table :: (Type -> Type) -> Type) :: Type
  -- | What to do when an @INSERT@ statement inserts a row into the table @tbl@
  -- that violates a constraint.
  data SqlConflictAction be (table :: (Type -> Type) -> Type) :: Type

  insertOnConflict
    :: Beamable table
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues be (table (QExpr be s))
    -> SqlConflictTarget be table
    -> SqlConflictAction be table
    -> SqlInsert be table

  anyConflict :: SqlConflictTarget be table
  conflictingFields
    :: Projectible be proj
    => (table (QExpr be QInternal) -> proj)
    -> SqlConflictTarget be table
  conflictingFieldsWhere
    :: Projectible be proj
    => (table (QExpr be QInternal) -> proj)
    -> (forall s. table (QExpr be s) -> QExpr be s Bool)
    -> SqlConflictTarget be table

  onConflictDoNothing :: SqlConflictAction be table
  onConflictUpdateSet
    :: Beamable table
    => (forall s. table (QField s) -> table (QExpr be s) -> QAssignment be s)
    -> SqlConflictAction be table
  onConflictUpdateSetWhere
    :: Beamable table
    => (forall s. table (QField s) -> table (QExpr be s) -> QAssignment be s)
    -> (forall s. table (QField s) -> table (QExpr be s) -> QExpr be s Bool)
    -> SqlConflictAction be table

newtype InaccessibleQAssignment be = InaccessibleQAssignment
  { unInaccessibleQAssignment :: [(BeamSqlBackendFieldNameSyntax be, BeamSqlBackendExpressionSyntax be)]
  } deriving (Data.Semigroup.Semigroup, Monoid)

onConflictUpdateInstead
  :: forall be table proj
  .  ( BeamHasInsertOnConflict be
     , Beamable table
     , ProjectibleWithPredicate AnyType () (InaccessibleQAssignment be) proj
     )
  => (table (Const (InaccessibleQAssignment be)) -> proj)
  -> SqlConflictAction be table
onConflictUpdateInstead mkProj = onConflictUpdateSet mkAssignments
  where
    mkAssignments
      :: forall s
      .  table (QField s)
      -> table (QExpr be s)
      -> QAssignment be s
    mkAssignments table excluded = QAssignment $ unInaccessibleQAssignment $
      Strict.execWriter $ project'
        (Proxy @AnyType)
        (Proxy @((), InaccessibleQAssignment be))
        (\_ _ a -> Strict.tell a >> return a)
        (mkProj $ runIdentity $ zipBeamFieldsM mkAssignment table excluded)
    mkAssignment
      :: forall s a
      .  Columnar' (QField s) a
      -> Columnar' (QExpr be s) a
      -> Identity (Columnar' (Const (InaccessibleQAssignment be)) a)
    mkAssignment (Columnar' field) (Columnar' value) =
      Identity $ Columnar' $ Const $
        InaccessibleQAssignment $ unQAssignment $ field <-. value

onConflictUpdateAll
  :: forall be table
  .  ( BeamHasInsertOnConflict be
     , Beamable table
     )
  => SqlConflictAction be table
onConflictUpdateAll = onConflictUpdateInstead id
