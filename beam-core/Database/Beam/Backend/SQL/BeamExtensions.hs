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
  , MonadBeamUpdateReturning(..)
  , MonadBeamDeleteReturning(..)
  , BeamHasInsertOnConflict(..)

  , SqlSerial(..)
  , onConflictUpdateInstead
  , onConflictUpdateAll
  ) where

import Database.Beam.Backend
import Database.Beam.Query
import Database.Beam.Query.Internal
import Database.Beam.Schema
import Database.Beam.Schema.Tables

import Data.Functor.Const
import Data.Proxy
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Except
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Strict as Strict

--import GHC.Generics

-- | 'MonadBeam's that support returning the newly created rows of an @INSERT@ statement.
--   Useful for discovering the real value of a defaulted value.
class MonadBeam be m =>
  MonadBeamInsertReturning be m | m -> be where
  runInsertReturningList
    :: ( Beamable table
       , Projectible be (table (QExpr be ()))
       , FromBackendRow be (table Identity) )
    => SqlInsert be table
    -> m [table Identity]

instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ExceptT e m) where
    runInsertReturningList = lift . runInsertReturningList
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ContT r m) where
    runInsertReturningList = lift . runInsertReturningList
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ReaderT r m) where
    runInsertReturningList = lift . runInsertReturningList
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (Lazy.StateT r m) where
    runInsertReturningList = lift . runInsertReturningList
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (Strict.StateT r m) where
    runInsertReturningList = lift . runInsertReturningList
instance (MonadBeamInsertReturning be m, Monoid r)
    => MonadBeamInsertReturning be (Lazy.WriterT r m) where
    runInsertReturningList = lift . runInsertReturningList
instance (MonadBeamInsertReturning be m, Monoid r)
    => MonadBeamInsertReturning be (Strict.WriterT r m) where
    runInsertReturningList = lift . runInsertReturningList
instance (MonadBeamInsertReturning be m, Monoid w)
    => MonadBeamInsertReturning be (Lazy.RWST r w s m) where
    runInsertReturningList = lift . runInsertReturningList
instance (MonadBeamInsertReturning be m, Monoid w)
    => MonadBeamInsertReturning be (Strict.RWST r w s m) where
    runInsertReturningList = lift . runInsertReturningList

-- | 'MonadBeam's that support returning the updated rows of an @UPDATE@ statement.
--   Useful for discovering the new values of the updated rows.
class MonadBeam be m =>
  MonadBeamUpdateReturning be m | m -> be where
  runUpdateReturningList
    :: ( Beamable table
       , Projectible be (table (QExpr be ()))
       , FromBackendRow be (table Identity) )
    => SqlUpdate be table
    -> m [table Identity]

instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ExceptT e m) where
    runUpdateReturningList = lift . runUpdateReturningList
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ContT r m) where
    runUpdateReturningList = lift . runUpdateReturningList
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ReaderT r m) where
    runUpdateReturningList = lift . runUpdateReturningList
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (Lazy.StateT r m) where
    runUpdateReturningList = lift . runUpdateReturningList
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (Strict.StateT r m) where
    runUpdateReturningList = lift . runUpdateReturningList
instance (MonadBeamUpdateReturning be m, Monoid r)
    => MonadBeamUpdateReturning be (Lazy.WriterT r m) where
    runUpdateReturningList = lift . runUpdateReturningList
instance (MonadBeamUpdateReturning be m, Monoid r)
    => MonadBeamUpdateReturning be (Strict.WriterT r m) where
    runUpdateReturningList = lift . runUpdateReturningList
instance (MonadBeamUpdateReturning be m, Monoid w)
    => MonadBeamUpdateReturning be (Lazy.RWST r w s m) where
    runUpdateReturningList = lift . runUpdateReturningList
instance (MonadBeamUpdateReturning be m, Monoid w)
    => MonadBeamUpdateReturning be (Strict.RWST r w s m) where
    runUpdateReturningList = lift . runUpdateReturningList

-- | 'MonadBeam's that suppert returning rows that will be deleted by the given
-- @DELETE@ statement. Useful for deallocating resources based on the value of
-- deleted rows.
class MonadBeam be m =>
  MonadBeamDeleteReturning be m | m -> be where
  runDeleteReturningList
    :: ( Beamable table
       , Projectible be (table (QExpr be ()))
       , FromBackendRow be (table Identity) )
    => SqlDelete be table
    -> m [table Identity]

instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ExceptT e m) where
    runDeleteReturningList = lift . runDeleteReturningList
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ContT r m) where
    runDeleteReturningList = lift . runDeleteReturningList
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ReaderT r m) where
    runDeleteReturningList = lift . runDeleteReturningList
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (Lazy.StateT r m) where
    runDeleteReturningList = lift . runDeleteReturningList
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (Strict.StateT r m) where
    runDeleteReturningList = lift . runDeleteReturningList
instance (MonadBeamDeleteReturning be m, Monoid r)
    => MonadBeamDeleteReturning be (Lazy.WriterT r m) where
    runDeleteReturningList = lift . runDeleteReturningList
instance (MonadBeamDeleteReturning be m, Monoid r)
    => MonadBeamDeleteReturning be (Strict.WriterT r m) where
    runDeleteReturningList = lift . runDeleteReturningList
instance (MonadBeamDeleteReturning be m, Monoid w)
    => MonadBeamDeleteReturning be (Lazy.RWST r w s m) where
    runDeleteReturningList = lift . runDeleteReturningList
instance (MonadBeamDeleteReturning be m, Monoid w)
    => MonadBeamDeleteReturning be (Strict.RWST r w s m) where
    runDeleteReturningList = lift . runDeleteReturningList

class BeamSqlBackend be => BeamHasInsertOnConflict be where
  type SqlConflictTarget be (table :: (* -> *) -> *) :: *
  type SqlConflictAction be (table :: (* -> *) -> *) :: *

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
    -> (forall s. table (QExpr be s) -> QExpr be s Bool)
    -> SqlConflictAction be table

newtype InaccessibleQAssignment be = InaccessibleQAssignment
  { unInaccessibleQAssignment :: [(BeamSqlBackendFieldNameSyntax be, BeamSqlBackendExpressionSyntax be)]
  } deriving (Semigroup, Monoid)

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
onConflictUpdateAll =
  onConflictUpdateInstead (id @(table (Const (InaccessibleQAssignment be))))
