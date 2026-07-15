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
  , invalidNestedSideEffectDelete
  , invalidCoercedPlacement
  , invalidRecursiveInsert
  , invalidReuseSideEffect
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
-- pgSelectWithNested. They must fail with PgCteTopLevelOnly versus
-- PgCteNestedAllowed,
-- independently of which data-modifying command produced the CTE.
invalidNestedDelete :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedDelete = select $ Pg.pgSelectWithNested $ do
  deleted <- topLevelDeleteCte
  pure (reuse deleted)

invalidNestedInsert :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedInsert = select $ Pg.pgSelectWithNested $ do
  inserted <- Pg.cteInsertReturning
    (negativeCteRows negativeCteDb)
    (insertValues [NegativeCteRow 2 "inserted"])
    Pg.onConflictDefault
    id
  case inserted of
    Nothing -> pure $ all_ (negativeCteRows negativeCteDb)
    Just inserted' -> pure (reuse inserted')

invalidNestedUpdate :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedUpdate = select $ Pg.pgSelectWithNested $ do
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
invalidNestedSelectThenDelete = select $ Pg.pgSelectWithNested $ do
  _ <- nestedSelectCte
  deleted <- topLevelDeleteCte
  pure (reuse deleted)

invalidNestedDeleteThenSelect :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedDeleteThenSelect = select $ Pg.pgSelectWithNested $ do
  deleted <- topLevelDeleteCte
  _ <- nestedSelectCte
  pure (reuse deleted)

-- The result is conservatively top-level-only even when the supplied values or
-- assignments make the INSERT or UPDATE a no-op. The placement index cannot
-- vary with that value-level outcome.
invalidNestedEmptyInsert :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedEmptyInsert = select $ Pg.pgSelectWithNested $ do
  inserted <- Pg.cteInsertReturning
    (negativeCteRows negativeCteDb)
    SqlInsertValuesEmpty
    Pg.onConflictDefault
    id
  case inserted of
    Nothing -> pure $ all_ (negativeCteRows negativeCteDb)
    Just inserted' -> pure (reuse inserted')

invalidNestedIdentityUpdate :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedIdentityUpdate = select $ Pg.pgSelectWithNested $ do
  updated <- Pg.cteUpdateReturning
    (negativeCteRows negativeCteDb)
    (const mempty)
    (const (val_ True))
    id
  case updated of
    Nothing -> pure $ all_ (negativeCteRows negativeCteDb)
    Just updated' -> pure (reuse updated')

-- A no-RETURNING modifying CTE has the same top-level placement requirement as
-- its returning counterpart, even though it exposes no relation.
invalidNestedSideEffectDelete :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidNestedSideEffectDelete = select $ Pg.pgSelectWithNested $ do
  Pg.cteDelete
    (negativeCteRows negativeCteDb)
    (\row -> negativeCteId row ==. val_ 1)
  pure $ all_ (negativeCteRows negativeCteDb)

-- PgWith has nominal roles and an abstract constructor, so Data.Coerce cannot
-- be used to relabel a top-level-only block as nested-safe.
invalidCoercedPlacement :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidCoercedPlacement = select $ Pg.pgSelectWithNested $ coercePlacement $ do
  deleted <- topLevelDeleteCte
  pure (reuse deleted)

-- MonadFix exists only for PgCteNestedAllowed. This prevents an INSERT CTE from
-- reading its own RETURNING rows recursively, which PostgreSQL rejects.
invalidRecursiveInsert :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidRecursiveInsert = Pg.pgSelectWithTopLevel $ mdo
  ~(Just inserted) <- Pg.cteInsertReturning
    (negativeCteRows negativeCteDb)
    (insertFrom (reuse inserted))
    Pg.onConflictDefault
    id
  pure (reuse inserted)

-- Side-effect-only CTEs deliberately return unit because a DML statement
-- without RETURNING forms no temporary relation in PostgreSQL.
invalidReuseSideEffect :: SqlSelect Postgres (NegativeCteRowT Identity)
invalidReuseSideEffect = Pg.pgSelectWithTopLevel $ do
  deleted <- Pg.cteDelete
    (negativeCteRows negativeCteDb)
    (\row -> negativeCteId row ==. val_ 1)
  let impossible
        :: ReusableQ Postgres NegativeCteDb
             (NegativeCteRowT (QExpr Postgres CTE.QAnyScope))
      impossible = deleted
  pure (reuse impossible)

coercePlacement
  :: Pg.PgWith NegativeCteDb 'Pg.PgCteTopLevelOnly a
  -> Pg.PgWith NegativeCteDb 'Pg.PgCteNestedAllowed a
coercePlacement = Coerce.coerce

nestedSelectCte
  :: Pg.PgWith NegativeCteDb placement
       (ReusableQ Postgres NegativeCteDb
         (NegativeCteRowT (QExpr Postgres CTE.QAnyScope)))
nestedSelectCte = Pg.pgSelecting $ all_ (negativeCteRows negativeCteDb)

topLevelDeleteCte
  :: Pg.PgWith NegativeCteDb 'Pg.PgCteTopLevelOnly
       (ReusableQ Postgres NegativeCteDb
         (NegativeCteRowT (QExpr Postgres CTE.QAnyScope)))
topLevelDeleteCte = Pg.cteDeleteReturning
  (negativeCteRows negativeCteDb)
  (\row -> negativeCteId row ==. val_ 1)
  id
