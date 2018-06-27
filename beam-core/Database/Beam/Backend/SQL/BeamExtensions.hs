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

  , SqlSerial(..)
  ) where

import Database.Beam.Backend
import Database.Beam.Query
import Database.Beam.Schema

import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
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
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues be (table (QExpr be s))
    -> m [table Identity]

instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ExceptT e m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ListT m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ContT r m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (ReaderT r m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (Lazy.StateT r m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance MonadBeamInsertReturning be m => MonadBeamInsertReturning be (Strict.StateT r m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance (MonadBeamInsertReturning be m, Monoid r)
    => MonadBeamInsertReturning be (Lazy.WriterT r m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance (MonadBeamInsertReturning be m, Monoid r)
    => MonadBeamInsertReturning be (Strict.WriterT r m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance (MonadBeamInsertReturning be m, Monoid w)
    => MonadBeamInsertReturning be (Lazy.RWST r w s m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)
instance (MonadBeamInsertReturning be m, Monoid w)
    => MonadBeamInsertReturning be (Strict.RWST r w s m) where
    runInsertReturningList e v = lift (runInsertReturningList e v)

-- | 'MonadBeam's that support returning the updated rows of an @UPDATE@ statement.
--   Useful for discovering the new values of the updated rows.
class MonadBeam be m =>
  MonadBeamUpdateReturning be m | m -> be where
  runUpdateReturningList
    :: ( Beamable table
       , Projectible be (table (QExpr be ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> (forall s. table (QField s) -> [ QAssignment be s ])
    -> (forall s. table (QExpr be s) -> QExpr be s Bool)
    -> m [table Identity]

instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ExceptT e m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ListT m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ContT r m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (ReaderT r m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (Lazy.StateT r m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance MonadBeamUpdateReturning be m => MonadBeamUpdateReturning be (Strict.StateT r m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance (MonadBeamUpdateReturning be m, Monoid r)
    => MonadBeamUpdateReturning be (Lazy.WriterT r m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance (MonadBeamUpdateReturning be m, Monoid r)
    => MonadBeamUpdateReturning be (Strict.WriterT r m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance (MonadBeamUpdateReturning be m, Monoid w)
    => MonadBeamUpdateReturning be (Lazy.RWST r w s m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)
instance (MonadBeamUpdateReturning be m, Monoid w)
    => MonadBeamUpdateReturning be (Strict.RWST r w s m) where
    runUpdateReturningList e a w = lift (runUpdateReturningList e a w)

-- | 'MonadBeam's that suppert returning rows that will be deleted by the given
-- @DELETE@ statement. Useful for deallocating resources based on the value of
-- deleted rows.
class MonadBeam be m =>
  MonadBeamDeleteReturning be m | m -> be where
  runDeleteReturningList
    :: ( Beamable table
       , Projectible be (table (QExpr be ()))
       , FromBackendRow be (table Identity) )
    => DatabaseEntity be db (TableEntity table)
    -> (forall s. table (QExpr be s) -> QExpr be s Bool)
    -> m [table Identity]

instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ExceptT e m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ListT m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ContT r m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (ReaderT r m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (Lazy.StateT r m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance MonadBeamDeleteReturning be m => MonadBeamDeleteReturning be (Strict.StateT r m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance (MonadBeamDeleteReturning be m, Monoid r)
    => MonadBeamDeleteReturning be (Lazy.WriterT r m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance (MonadBeamDeleteReturning be m, Monoid r)
    => MonadBeamDeleteReturning be (Strict.WriterT r m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance (MonadBeamDeleteReturning be m, Monoid w)
    => MonadBeamDeleteReturning be (Lazy.RWST r w s m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
instance (MonadBeamDeleteReturning be m, Monoid w)
    => MonadBeamDeleteReturning be (Strict.RWST r w s m) where
    runDeleteReturningList e v = lift (runDeleteReturningList e v)
