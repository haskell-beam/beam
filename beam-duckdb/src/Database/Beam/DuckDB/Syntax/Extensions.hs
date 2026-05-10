{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.DuckDB.Syntax.Extensions () where

import Data.Proxy (Proxy (..))
import Database.Beam (Beamable, TableSettings)
import Database.Beam.Backend.SQL
  ( IsSql92ExpressionSyntax (..),
    IsSql92FieldNameSyntax (..),
    MonadBeam (runReturningList),
  )
import Database.Beam.Backend.SQL.BeamExtensions
  ( MonadBeamDeleteReturning (..),
    MonadBeamInsertReturning (..),
    MonadBeamUpdateReturning (..),
  )
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Connection (DuckDBM)
import Database.Beam.DuckDB.Syntax
  ( DuckDBCommandSyntax (..),
    DuckDBDeleteSyntax (..),
    DuckDBExpressionSyntax (..),
    DuckDBInsertSyntax (..),
    DuckDBUpdateSyntax (..),
  )
import Database.Beam.DuckDB.Syntax.Builder (DuckDBSyntax, commas, emit)
import Database.Beam.Query (SqlDelete (..), SqlInsert (..), SqlUpdate (..))
import Database.Beam.Query.Internal (Projectible, QExpr, QGenExpr (..), project)
import Database.Beam.Schema.Tables (Columnar' (..), changeBeamRep, _fieldName)

instance MonadBeamInsertReturning DuckDB DuckDBM where
  -- \| @since 0.3.1.0
  runInsertReturningListWith SqlInsertNoRows _ = pure []
  runInsertReturningListWith (SqlInsert tblSettings (DuckDBInsertSyntax syntax)) mkProjection =
    runReturningList
      (DuckDBCommandSyntax (syntax <> returningClause tblSettings mkProjection))

instance MonadBeamUpdateReturning DuckDB DuckDBM where
  -- \| @since 0.3.1.0
  runUpdateReturningListWith SqlIdentityUpdate _ = pure []
  runUpdateReturningListWith (SqlUpdate tblSettings (DuckDBUpdateSyntax syntax)) mkProjection =
    runReturningList
      (DuckDBCommandSyntax (syntax <> returningClause tblSettings mkProjection))

instance MonadBeamDeleteReturning DuckDB DuckDBM where
  -- \| @since 0.3.1.0
  runDeleteReturningListWith (SqlDelete tblSettings (DuckDBDeleteSyntax syntax)) mkProjection =
    runReturningList
      (DuckDBCommandSyntax (syntax <> returningClause tblSettings mkProjection))

-- | Build @RETURNING e1, e2, ...@ for a projection over the given table's
-- fields, using unqualified field references (DuckDB does not require a
-- table prefix in @RETURNING@ projections).
returningClause ::
  forall table a.
  (Beamable table, Projectible DuckDB a) =>
  TableSettings table ->
  (table (QExpr DuckDB ()) -> a) ->
  DuckDBSyntax
returningClause tblSettings mkProjection =
  emit " RETURNING "
    <> commas
      ( map
          fromDuckDBExpression
          (project (Proxy @DuckDB) (mkProjection tblQ) "t")
      )
  where
    tblQ :: table (QExpr DuckDB ())
    tblQ =
      changeBeamRep
        ( \(Columnar' f) ->
            Columnar' (QExpr (\_ -> fieldE (unqualifiedField (_fieldName f))))
        )
        tblSettings
