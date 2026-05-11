{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.DuckDB.Syntax.Extensions.InsertOnConflict
  ( -- * Compile-time-blocked variant of 'conflictingFieldsWhere'

    -- DuckDB does not support partial-index conflict targets, so calling
    -- the class method 'Database.Beam.Backend.SQL.BeamExtensions.conflictingFieldsWhere'
    -- against DuckDB results in a DuckDB binder error at execute time. The
    -- 'conflictingFieldsWhere' exported here shadows the class method so
    -- that any DuckDB user of @Database.Beam.DuckDB@ gets a /compile-time/
    -- error instead — pointing them at 'onConflictUpdateSetWhere'.
    conflictingFieldsWhere,
    UnsupportedConflictingFieldsWhereOnDuckDB,
  )
where

import Data.Proxy (Proxy (..))
import Database.Beam (Beamable, TableEntity)
import Database.Beam.Backend.SQL
  ( IsSql92ExpressionSyntax (..),
    IsSql92FieldNameSyntax (..),
    IsSql92InsertSyntax (..),
    IsSql92TableNameSyntax (..),
  )
import Database.Beam.Backend.SQL.BeamExtensions
  ( BeamHasInsertOnConflict (SqlConflictAction, SqlConflictTarget),
  )
import qualified Database.Beam.Backend.SQL.BeamExtensions as Beam
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Syntax
  ( DuckDBExpressionSyntax (..),
    DuckDBFieldNameSyntax (..),
    DuckDBInsertSyntax (..),
  )
import Database.Beam.DuckDB.Syntax.Builder
  ( DuckDBSyntax,
    commas,
    emit,
    parens,
  )
import Database.Beam.Query (SqlInsert (..), SqlInsertValues (..))
import Database.Beam.Query.Internal
  ( Projectible,
    QAssignment (..),
    QExpr,
    QField (..),
    QGenExpr (..),
    QInternal,
    project,
  )
import Database.Beam.Schema.Tables
  ( Columnar' (..),
    DatabaseEntity (..),
    DatabaseEntityDescriptor (..),
    TableField,
    allBeamValues,
    changeBeamRep,
    _fieldName,
  )
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | @since 0.3.1.0
instance Beam.BeamHasInsertOnConflict DuckDB where
  newtype SqlConflictTarget DuckDB table = DuckDBConflictTarget
    {unDuckDBConflictTarget :: table (QExpr DuckDB QInternal) -> DuckDBSyntax}
  newtype SqlConflictAction DuckDB table = DuckDBConflictAction
    {unDuckDBConflictAction :: forall s. table (QField s) -> DuckDBSyntax}

  insertOnConflict ::
    forall table db s.
    (Beamable table) =>
    DatabaseEntity DuckDB db (TableEntity table) ->
    SqlInsertValues DuckDB (table (QExpr DuckDB s)) ->
    SqlConflictTarget DuckDB table ->
    SqlConflictAction DuckDB table ->
    SqlInsert DuckDB table
  insertOnConflict (DatabaseEntity dt) values target action = case values of
    SqlInsertValuesEmpty -> SqlInsertNoRows
    SqlInsertValues vs ->
      let tblSettings = dbTableSettings dt
          mkField ::
            forall a.
            Columnar' (TableField table) a ->
            Columnar' (QField QInternal) a
          mkField (Columnar' fd) = Columnar' $ QField False (dbTableCurrentName dt) (_fieldName fd)
          tblFields = changeBeamRep mkField tblSettings
          tblExprs =
            changeBeamRep
              ( \(Columnar' (QField _ _ nm)) ->
                  Columnar' (QExpr (\_ -> fieldE (unqualifiedField nm)))
              )
              tblFields
          fieldList = allBeamValues (\(Columnar' f) -> _fieldName f) tblSettings
          DuckDBInsertSyntax baseInsert =
            insertStmt (tableName (dbTableSchema dt) (dbTableCurrentName dt)) fieldList vs
       in SqlInsert tblSettings $
            DuckDBInsertSyntax $
              baseInsert
                <> emit " ON CONFLICT "
                <> unDuckDBConflictTarget target tblExprs
                <> unDuckDBConflictAction action tblFields

  anyConflict = DuckDBConflictTarget (const mempty)

  conflictingFields makeProjection =
    DuckDBConflictTarget $ \tbl ->
      parens
        ( commas $
            map fromDuckDBExpression $
              project (Proxy @DuckDB) (makeProjection tbl) "t"
        )
        <> emit " "

  -- \| DuckDB does not support partial-index conflict targets, so calling this is always a bug.
  -- The supported way to invoke this method from @Database.Beam.DuckDB@ goes through the shadowing
  -- 'conflictingFieldsWhere' below, which produces a /compile-time/ error pointing users at 'onConflictUpdateSetWhere'.
  conflictingFieldsWhere _ _ =
    error
      "Database.Beam.DuckDB: `conflictingFieldsWhere` is not supported on DuckDB. \
      \DuckDB does not support partial-index conflict targets — use \
      \`onConflictUpdateSetWhere` to filter the rows that are updated instead."

  onConflictDoNothing = DuckDBConflictAction (const (emit "DO NOTHING"))

  onConflictUpdateSet mkAssignments =
    DuckDBConflictAction $ \tbl ->
      let QAssignment assignments = mkAssignments tbl (excluded tbl)
          assignmentSyntaxes =
            [ fromDuckDBFieldName fieldNm
                <> emit "="
                <> parens (fromDuckDBExpression expr)
            | (fieldNm, expr) <- assignments
            ]
       in emit "DO UPDATE SET " <> commas assignmentSyntaxes

  onConflictUpdateSetWhere mkAssignments where_ =
    DuckDBConflictAction $ \tbl ->
      let QAssignment assignments = mkAssignments tbl (excluded tbl)
          QExpr where_' = where_ tbl (excluded tbl)
          assignmentSyntaxes =
            [ fromDuckDBFieldName fieldNm
                <> emit "="
                <> parens (fromDuckDBExpression expr)
            | (fieldNm, expr) <- assignments
            ]
       in emit "DO UPDATE SET "
            <> commas assignmentSyntaxes
            <> emit " WHERE "
            <> fromDuckDBExpression (where_' "t")

-- | Build a table reference whose fields point to the special @excluded@
-- pseudo-table available inside DuckDB's @ON CONFLICT DO UPDATE@ — i.e. the
-- row that would have been inserted had there been no conflict.
excluded ::
  (Beamable table) =>
  table (QField s) ->
  table (QExpr DuckDB s)
excluded =
  changeBeamRep
    ( \(Columnar' (QField _ _ nm)) ->
        Columnar' (QExpr (\_ -> fieldE (qualifiedField "excluded" nm)))
    )

-- | A class with a single 'TypeError'-guarded instance, used purely to
-- defer firing of the type error until a /use site/ of
-- 'conflictingFieldsWhere'. The instance context is only discharged
-- when GHC actually selects this instance, which means defining a
-- method body in terms of it (as @'conflictingFieldsWhere' = …@) does
-- not trip the error at module load time.
--
-- = References
--
-- DuckDB's binder unconditionally rejects @ON CONFLICT (cols) WHERE …@:
-- see @GenerateMergeInto@ in
-- <https://github.com/duckdb/duckdb/blob/main/src/planner/binder/statement/bind_insert.cpp duckdb/duckdb \@ src\/planner\/binder\/statement\/bind_insert.cpp>,
-- which throws @"ON CONFLICT WHERE clause is only supported in DO UPDATE
-- SET ... WHERE ... The WHERE clause after the conflict columns is used
-- for partial indexes which are not supported."@. The user-facing
-- <https://duckdb.org/docs/stable/sql/statements/insert INSERT docs>
-- describe both @WHERE@ positions, but only the trailing-update form
-- actually binds; the index-predicate form (used in PostgreSQL to
-- arbitrate between /partial/ unique indexes) has nothing to match
-- against in DuckDB.
--
-- @since 0.3.1.0
class UnsupportedConflictingFieldsWhereOnDuckDB where
  -- | A 'conflictingFieldsWhere' that always fails to compile when
  -- used against DuckDB. Re-exported from "Database.Beam.DuckDB" so
  -- that it shadows the class method of the same name from
  -- "Database.Beam.Backend.SQL.BeamExtensions".
  --
  -- @since 0.3.1.0
  conflictingFieldsWhere ::
    forall table proj.
    (Projectible DuckDB proj, Beamable table) =>
    (table (QExpr DuckDB QInternal) -> proj) ->
    (forall s. table (QExpr DuckDB s) -> QExpr DuckDB s Bool) ->
    SqlConflictTarget DuckDB table

instance
  (TypeError DuckDBConflictingFieldsWhereError) =>
  UnsupportedConflictingFieldsWhereOnDuckDB
  where
  conflictingFieldsWhere _ _ = error "unreachable: TypeError fires first"

type DuckDBConflictingFieldsWhereError =
  'Text "`conflictingFieldsWhere` is not supported on the DuckDB backend."
    ':$$: 'Text "DuckDB does not support partial-index conflict targets"
    ':$$: 'Text "(the WHERE clause between the conflict columns and DO)."
    ':$$: 'Text "Use `onConflictUpdateSetWhere` to filter the rows that are updated instead."
