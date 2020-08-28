{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Backend.SQL
  ( module Database.Beam.Backend.SQL.Row
  , module Database.Beam.Backend.SQL.SQL2003
  , module Database.Beam.Backend.SQL.Types

  , MonadBeam(..)

  , BeamSqlBackend
  , BeamSqlBackendSyntax
  , MockSqlBackend

  , BeamSqlBackendIsString

  , BeamSql99ExpressionBackend
  , BeamSql99AggregationBackend
  , BeamSql99ConcatExpressionBackend
  , BeamSql99CommonTableExpressionBackend
  , BeamSql99RecursiveCTEBackend
  , BeamSql2003ExpressionBackend

  , BeamSqlT021Backend
  , BeamSqlT071Backend
  , BeamSqlT611Backend
  , BeamSqlT612Backend
  , BeamSqlT614Backend
  , BeamSqlT615Backend
  , BeamSqlT616Backend
  , BeamSqlT618Backend
  , BeamSqlT621Backend
  , BeamSql99DataTypeBackend

  , BeamSqlBackendSupportsOuterJoin

  , BeamSqlBackendSelectSyntax
  , BeamSqlBackendInsertSyntax
  , BeamSqlBackendInsertValuesSyntax
  , BeamSqlBackendUpdateSyntax
  , BeamSqlBackendDeleteSyntax
  , BeamSqlBackendCastTargetSyntax
  , BeamSqlBackendSelectTableSyntax
  , BeamSqlBackendAggregationQuantifierSyntax
  , BeamSqlBackendSetQuantifierSyntax
  , BeamSqlBackendFromSyntax
  , BeamSqlBackendTableNameSyntax

  , BeamSqlBackendExpressionSyntax
  , BeamSqlBackendDataTypeSyntax
  , BeamSqlBackendFieldNameSyntax
  , BeamSqlBackendExpressionQuantifierSyntax
  , BeamSqlBackendValueSyntax
  , BeamSqlBackendOrderingSyntax
  , BeamSqlBackendGroupingSyntax

  , BeamSqlBackendWindowFrameSyntax
  , BeamSqlBackendWindowFrameBoundsSyntax
  , BeamSqlBackendWindowFrameBoundSyntax

  , BeamSql99BackendCTESyntax

  , BeamSqlBackendCanSerialize
  , BeamSqlBackendCanDeserialize
  , BeamSqlBackendSupportsDataType
  ) where

import           Database.Beam.Backend.SQL.SQL2003
import           Database.Beam.Backend.SQL.Row
import           Database.Beam.Backend.SQL.Types
import           Database.Beam.Backend.Types

import           Control.Monad.Cont
import           Control.Monad.Except
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Strict as Strict

import           Data.Kind (Type)
import           Data.Tagged (Tagged)
import           Data.Text (Text)

-- * MonadBeam class

-- | A class that ties together a monad with a particular backend
--
--   Provided here is a low-level interface for executing commands. The 'run*'
--   functions are wrapped by the appropriate functions in 'Database.Beam.Query'.
--
--   This interface is very high-level and isn't meant to expose the full power
--   of the underlying database. Namely, it only supports simple data retrieval
--   strategies. More complicated strategies (for example, Postgres's @COPY@)
--   are supported in individual backends. See the documentation of those
--   backends for more details.
class (BeamBackend be, Monad m) =>
  MonadBeam be m | m -> be where
  {-# MINIMAL runReturningMany #-}

  -- | Run a query determined by the given syntax, providing an action that will
  --   be called to consume the results from the database (if any). The action
  --   will get a reader action that can be used to fetch the next row. When
  --   this reader action returns 'Nothing', there are no rows left to consume.
  --   When the reader action returns, the database result is freed.
  runReturningMany :: FromBackendRow be x
                   => BeamSqlBackendSyntax be
                      -- ^ The query to run
                   -> (m (Maybe x) -> m a)
                       -- ^ Reader action that will be called with a function to
                       -- fetch the next row
                   -> m a

  -- | Run the given command and don't consume any results. Useful for DML
  --   statements like INSERT, UPDATE, and DELETE, or DDL statements.
  runNoReturn :: BeamSqlBackendSyntax be -> m ()
  runNoReturn cmd =
      runReturningMany cmd $ \(_ :: m (Maybe ())) -> pure ()

  -- | Run the given command and fetch the unique result. The result is
  --   'Nothing' if either no results are returned or more than one result is
  --   returned.
  runReturningOne :: FromBackendRow be x => BeamSqlBackendSyntax be -> m (Maybe x)
  runReturningOne cmd =
      runReturningMany cmd $ \next ->
        do a <- next
           case a of
             Nothing -> pure Nothing
             Just x -> do
               a' <- next
               case a' of
                 Nothing -> pure (Just x)
                 Just _ -> pure Nothing

  -- | Run the given command, collect all the results, and return them as a
  --   list. May be more convenient than 'runReturningMany', but reads the entire
  --   result set into memory.
  runReturningList :: FromBackendRow be x => BeamSqlBackendSyntax be -> m [x]
  runReturningList cmd =
      runReturningMany cmd $ \next ->
          let collectM acc = do
                a <- next
                case a of
                  Nothing -> pure (acc [])
                  Just x -> collectM (acc . (x:))
          in collectM id

instance MonadBeam be m => MonadBeam be (ExceptT e m) where
    runReturningMany s a = ExceptT $ runReturningMany s (\nextRow -> runExceptT (a (lift nextRow)))
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

instance MonadBeam be m => MonadBeam be (ContT r m) where
    runReturningMany s a = ContT $ \r ->
                           runReturningMany s (\nextRow -> runContT (a (lift nextRow)) r)
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

instance MonadBeam be m => MonadBeam be (ReaderT r m) where
    runReturningMany s a = ReaderT $ \r ->
                           runReturningMany s (\nextRow -> runReaderT (a (lift nextRow)) r)
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

instance MonadBeam be m => MonadBeam be (Lazy.StateT s m) where
    runReturningMany s a = Lazy.StateT $ \st ->
                           runReturningMany s (\nextRow -> Lazy.runStateT (a (lift nextRow)) st)
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

instance MonadBeam be m => MonadBeam be (Strict.StateT s m) where
    runReturningMany s a = Strict.StateT $ \st ->
                           runReturningMany s (\nextRow -> Strict.runStateT (a (lift nextRow)) st)
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

instance (MonadBeam be m, Monoid s) => MonadBeam be (Lazy.WriterT s m) where
    runReturningMany s a = Lazy.WriterT $
                           runReturningMany s (\nextRow -> Lazy.runWriterT (a (lift nextRow)))
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

instance (MonadBeam be m, Monoid s) => MonadBeam be (Strict.WriterT s m) where
    runReturningMany s a = Strict.WriterT $
                           runReturningMany s (\nextRow -> Strict.runWriterT (a (lift nextRow)))
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

instance (MonadBeam be m, Monoid w) => MonadBeam be (Lazy.RWST r w s m) where
    runReturningMany s a = Lazy.RWST $ \r st ->
                           runReturningMany s (\nextRow -> Lazy.runRWST (a (lift nextRow)) r st)
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

instance (MonadBeam be m, Monoid w) => MonadBeam be (Strict.RWST r w s m) where
    runReturningMany s a = Strict.RWST $ \r st ->
                           runReturningMany s (\nextRow -> Strict.runRWST (a (lift nextRow)) r st)
    runNoReturn = lift . runNoReturn
    runReturningOne = lift . runReturningOne
    runReturningList = lift . runReturningList

-- * BeamSqlBackend

-- | Class for all Beam SQL backends
class ( -- Every SQL backend must be a beam backend
        BeamBackend be

        -- Every SQL backend must have a reasonable SQL92 semantics
      , IsSql92Syntax (BeamSqlBackendSyntax be)
      , Sql92SanityCheck (BeamSqlBackendSyntax be)

        -- Needed for several combinators
      , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) Bool
      , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) SqlNull

        -- Needed for the Eq instance on QGenExpr
      , Eq (BeamSqlBackendExpressionSyntax be)
      ) => BeamSqlBackend be

type family BeamSqlBackendSyntax be :: Type

-- | Fake backend that cannot deserialize anything, but is useful for testing
data MockSqlBackend syntax

class Trivial a
instance Trivial a

instance BeamBackend (MockSqlBackend syntax) where
  type BackendFromField (MockSqlBackend syntax) = Trivial

instance ( IsSql92Syntax syntax
         , Sql92SanityCheck syntax

           -- Needed for several combinators
         , HasSqlValueSyntax (Sql92ValueSyntax syntax) Bool
         , HasSqlValueSyntax (Sql92ValueSyntax syntax) SqlNull

           -- Needed for the Eq instance on QGenExpr
         , Eq (Sql92ExpressionSyntax syntax)
         ) => BeamSqlBackend (MockSqlBackend syntax)
type instance BeamSqlBackendSyntax (MockSqlBackend syntax) = syntax

-- | Type class for things which are text-like in this backend
class BeamSqlBackendIsString be text
instance BeamSqlBackendIsString be t => BeamSqlBackendIsString be (Tagged tag t)
instance BeamSqlBackendIsString (MockSqlBackend cmd) Text
instance BeamSqlBackendIsString (MockSqlBackend cmd) [Char]

type BeamSql99ExpressionBackend be = IsSql99ExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSql99ConcatExpressionBackend be = IsSql99ConcatExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSql99CommonTableExpressionBackend be =
    ( BeamSqlBackend be
    , IsSql99CommonTableExpressionSelectSyntax (BeamSqlBackendSelectSyntax be)
    , IsSql99CommonTableExpressionSyntax (BeamSql99BackendCTESyntax be)
    , Sql99CTESelectSyntax (BeamSql99BackendCTESyntax be) ~ BeamSqlBackendSelectSyntax be )
type BeamSql99RecursiveCTEBackend be=
    ( BeamSql99CommonTableExpressionBackend be
    , IsSql99RecursiveCommonTableExpressionSelectSyntax (BeamSqlBackendSelectSyntax be) )
type BeamSql99AggregationBackend be = IsSql99AggregationExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSql2003ExpressionBackend be = ( IsSql2003ExpressionSyntax (BeamSqlBackendExpressionSyntax be)
                                       , Sql2003SanityCheck (BeamSqlBackendSyntax be) )

type BeamSqlBackendSupportsOuterJoin be = IsSql92FromOuterJoinSyntax (BeamSqlBackendFromSyntax be)

type BeamSqlT021Backend be = IsSql2003BinaryAndVarBinaryDataTypeSyntax (BeamSqlBackendCastTargetSyntax be)
type BeamSqlT071Backend be = IsSql2008BigIntDataTypeSyntax (BeamSqlBackendCastTargetSyntax be)
type BeamSqlT611Backend be = IsSql2003ExpressionElementaryOLAPOperationsSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT612Backend be = IsSql2003ExpressionAdvancedOLAPOperationsSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT614Backend be = IsSql2003NtileExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT615Backend be = IsSql2003LeadAndLagExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT616Backend be = IsSql2003FirstValueAndLastValueExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT618Backend be = IsSql2003NthValueExpressionSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlT621Backend be =
  ( IsSql2003EnhancedNumericFunctionsExpressionSyntax (BeamSqlBackendExpressionSyntax be)
  , IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax (BeamSqlBackendExpressionSyntax be) )

type BeamSql99DataTypeBackend be =
    ( BeamSqlBackend be
    , IsSql99DataTypeSyntax (BeamSqlBackendCastTargetSyntax be) )

type BeamSqlBackendSelectSyntax be = Sql92SelectSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendInsertSyntax be = Sql92InsertSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendInsertValuesSyntax be = Sql92InsertValuesSyntax (BeamSqlBackendInsertSyntax be)
type BeamSqlBackendExpressionSyntax be = Sql92ExpressionSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendDataTypeSyntax be = Sql92ExpressionCastTargetSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlBackendFieldNameSyntax be = Sql92ExpressionFieldNameSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlBackendUpdateSyntax be = Sql92UpdateSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendDeleteSyntax be = Sql92DeleteSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendCastTargetSyntax be
    = Sql92ExpressionCastTargetSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlBackendExpressionQuantifierSyntax be = Sql92ExpressionQuantifierSyntax (Sql92ExpressionSyntax (BeamSqlBackendSyntax be))
type BeamSqlBackendValueSyntax be = Sql92ValueSyntax (BeamSqlBackendSyntax be)
type BeamSqlBackendSetQuantifierSyntax be = Sql92SelectTableSetQuantifierSyntax (BeamSqlBackendSelectTableSyntax be)
type BeamSqlBackendAggregationQuantifierSyntax be = Sql92AggregationSetQuantifierSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlBackendSelectTableSyntax be = Sql92SelectSelectTableSyntax (BeamSqlBackendSelectSyntax be)
type BeamSqlBackendFromSyntax be = Sql92SelectFromSyntax (BeamSqlBackendSelectSyntax be)
type BeamSqlBackendTableNameSyntax be =  Sql92TableSourceTableNameSyntax (Sql92FromTableSourceSyntax (BeamSqlBackendFromSyntax be))
type BeamSqlBackendOrderingSyntax be = Sql92SelectOrderingSyntax (BeamSqlBackendSelectSyntax be)
type BeamSqlBackendGroupingSyntax be = Sql92SelectTableGroupingSyntax (BeamSqlBackendSelectTableSyntax be)

type BeamSqlBackendWindowFrameSyntax be = Sql2003ExpressionWindowFrameSyntax (BeamSqlBackendExpressionSyntax be)
type BeamSqlBackendWindowFrameBoundsSyntax be = Sql2003WindowFrameBoundsSyntax (BeamSqlBackendWindowFrameSyntax be)
type BeamSqlBackendWindowFrameBoundSyntax be = Sql2003WindowFrameBoundsBoundSyntax (BeamSqlBackendWindowFrameBoundsSyntax be)

type BeamSql99BackendCTESyntax be = Sql99SelectCTESyntax (BeamSqlBackendSelectSyntax be)

type BeamSqlBackendCanSerialize be = HasSqlValueSyntax (BeamSqlBackendValueSyntax be)
type BeamSqlBackendCanDeserialize be = FromBackendRow be
type BeamSqlBackendSupportsDataType be x =
  ( BeamSqlBackendCanDeserialize be x
  , BeamSqlBackendCanSerialize be x )
