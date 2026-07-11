{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}

-- This module deliberately contains expressions which must not type-check.
-- Deferred type errors are isolated here so the positive CTE tests retain
-- normal, strict type checking.
module Database.Beam.Postgres.Test.CTENegative
  ( invalidNestedDelete
  , invalidNestedInsert
  , invalidNestedUpdate
  , invalidNestedSelectThenDelete
  , invalidNestedDeleteThenSelect
  , invalidNestedEmptyInsert
  , invalidNestedIdentityUpdate
  , invalidCoercedPlacement
  , invalidRecursiveInsert
  ) where

import qualified Data.Coerce as Coerce
import Data.Int (Int32)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Query.CTE as CTE

data NegativeCteRowT f = NegativeCteRow
  { negativeCteId :: C f Int32
  , negativeCteValue :: C f Text
  } deriving (Generic, Beamable)

deriving instance Show (NegativeCteRowT Identity)
deriving instance Eq (NegativeCteRowT Identity)

instance Table NegativeCteRowT where
  data PrimaryKey NegativeCteRowT f = NegativeCteRowKey (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = NegativeCteRowKey . negativeCteId

newtype NegativeCteDb entity = NegativeCteDb
  { negativeCteRows :: entity (TableEntity NegativeCteRowT)
  } deriving (Generic, Database Postgres)

negativeCteDb :: DatabaseSettings Postgres NegativeCteDb
negativeCteDb = defaultDbSettings

-- Each of the following three expressions attempts to put a modifying CTE in
-- pgSelectWith. They must fail with CteTopLevelOnly versus CteNestedAllowed,
-- independently of which data-modifying command produced the CTE.
invalidNestedDelete :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedDelete = select $ Pg.pgSelectWith $ do
  deleted <- topLevelDeleteCte
  pure (reuse deleted)

invalidNestedInsert :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedInsert = select $ Pg.pgSelectWith $ do
  inserted <- Pg.cteInsertReturning
    (negativeCteRows negativeCteDb)
    (insertValues [NegativeCteRow 2 "inserted"])
    Pg.onConflictDefault
    id
  case inserted of
    Nothing -> pure $ all_ (negativeCteRows negativeCteDb)
    Just inserted' -> pure (reuse inserted')

invalidNestedUpdate :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedUpdate = select $ Pg.pgSelectWith $ do
  updated <- Pg.cteUpdateReturning
    (negativeCteRows negativeCteDb)
    (\row -> negativeCteValue row <-. val_ "updated")
    (\row -> negativeCteId row ==. val_ 1)
    id
  case updated of
    Nothing -> pure $ all_ (negativeCteRows negativeCteDb)
    Just updated' -> pure (reuse updated')

-- Placement is a property of the whole With block. Reordering a normal SELECT
-- CTE around the DELETE must not weaken the top-level-only requirement.
invalidNestedSelectThenDelete :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedSelectThenDelete = select $ Pg.pgSelectWith $ do
  _ <- nestedSelectCte
  deleted <- topLevelDeleteCte
  pure (reuse deleted)

invalidNestedDeleteThenSelect :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedDeleteThenSelect = select $ Pg.pgSelectWith $ do
  deleted <- topLevelDeleteCte
  _ <- nestedSelectCte
  pure (reuse deleted)

-- The result is conservatively top-level-only even when a value-level check
-- later discovers that the INSERT or UPDATE emits no statement. The placement
-- invariant cannot depend on runtime values.
invalidNestedEmptyInsert :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedEmptyInsert = select $ Pg.pgSelectWith $ do
  inserted <- Pg.cteInsertReturning
    (negativeCteRows negativeCteDb)
    SqlInsertValuesEmpty
    Pg.onConflictDefault
    id
  case inserted of
    Nothing -> pure $ all_ (negativeCteRows negativeCteDb)
    Just inserted' -> pure (reuse inserted')

invalidNestedIdentityUpdate :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedIdentityUpdate = select $ Pg.pgSelectWith $ do
  updated <- Pg.cteUpdateReturning
    (negativeCteRows negativeCteDb)
    (const mempty)
    (const (val_ True))
    id
  case updated of
    Nothing -> pure $ all_ (negativeCteRows negativeCteDb)
    Just updated' -> pure (reuse updated')

-- With has nominal roles and an abstract constructor, so Data.Coerce cannot be
-- used to relabel a top-level-only block as nested-safe.
invalidCoercedPlacement :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidCoercedPlacement = select $ Pg.pgSelectWith $ coercePlacement $ do
  deleted <- topLevelDeleteCte
  pure (reuse deleted)

-- MonadFix exists only for CteNestedAllowed. This prevents an INSERT CTE from
-- reading its own RETURNING rows recursively, which PostgreSQL rejects.
invalidRecursiveInsert :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidRecursiveInsert = selectWith $ mdo
  ~(Just inserted) <- Pg.cteInsertReturning
    (negativeCteRows negativeCteDb)
    (insertFrom (reuse inserted))
    Pg.onConflictDefault
    id
  pure (reuse inserted)

coercePlacement
  :: With Postgres NegativeCteDb 'CteTopLevelOnly a
  -> With Postgres NegativeCteDb 'CteNestedAllowed a
coercePlacement = Coerce.coerce

nestedSelectCte
  :: With Postgres NegativeCteDb placement
       (ReusableQ Postgres NegativeCteDb
         (NegativeCteRowT (QExpr Postgres CTE.QAnyScope)))
nestedSelectCte = selecting $ all_ (negativeCteRows negativeCteDb)

topLevelDeleteCte
  :: With Postgres NegativeCteDb 'CteTopLevelOnly
       (ReusableQ Postgres NegativeCteDb
         (NegativeCteRowT (QExpr Postgres CTE.QAnyScope)))
topLevelDeleteCte = Pg.cteDeleteReturning
  (negativeCteRows negativeCteDb)
  (\row -> negativeCteId row ==. val_ 1)
  id
