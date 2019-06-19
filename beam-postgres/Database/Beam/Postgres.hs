{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Postgres is a popular, open-source RDBMS. It is fairly standards compliant
-- and supports many advanced features and data types.
--
-- The @beam-postgres@ module is built atop of @postgresql-simple@, which is
-- used for connection management, transaction support, serialization, and
-- deserialization.
--
-- @beam-postgres@ supports most beam features as well as many postgres-specific
-- features. For example, @beam-postgres@ provides support for full-text search,
-- @DISTINCT ON@, JSON handling, postgres @ARRAY@s, @RANGE@s, and the @MONEY@ type.
--
-- The documentation for @beam-postgres@ functionality below indicates which
-- postgres function each function or type wraps. Postgres maintains its own
-- in-depth documentation. Please refer to that for more detailed information on
-- <https://www.postgresql.org/docs/current/static/index.html behavior>.
--
-- For examples on how to use @beam-postgres@ usage, see
-- <http://tathougies.github.io/beam/user-guide/backends/beam-postgres/ its manual>.

module Database.Beam.Postgres
  (  -- * Beam Postgres backend
    Postgres(..), Pg

    -- ** Postgres syntax
  , PgCommandSyntax, PgSyntax
  , PgSelectSyntax, PgInsertSyntax
  , PgUpdateSyntax, PgDeleteSyntax

    -- * Beam URI support
  , postgresUriSyntax

    -- * Postgres-specific features
    -- ** Postgres-specific data types

  , json, jsonb, uuid, money
  , tsquery, tsvector, text, bytea
  , unboundedArray

    -- *** @SERIAL@ support
  , smallserial, serial, bigserial

  , module Database.Beam.Postgres.PgSpecific

  , runBeamPostgres, runBeamPostgresDebug

    -- ** Postgres extension support
  , PgExtensionEntity, IsPgExtension(..)
  , pgCreateExtension, pgDropExtension
  , getPgExtension

    -- ** Debug support

  , PgDebugStmt
  , pgTraceStmtIO, pgTraceStmtIO'
  , pgTraceStmt

  -- * @postgresql-simple@ re-exports

  , Pg.ResultError(..), Pg.SqlError(..)

  , Pg.Connection, Pg.ConnectInfo(..)
  , Pg.defaultConnectInfo

  , Pg.connectPostgreSQL, Pg.connect
  , Pg.close

  ) where

import Data.Proxy (Proxy(..))

import Database.Beam.Backend
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Query hiding (insert)
import Database.Beam.Query.Internal
import Database.Beam.Schema.Tables

import Database.Beam.Postgres.Connection
import Database.Beam.Postgres.Full
import Database.Beam.Postgres.Syntax
import Database.Beam.Postgres.Types
import Database.Beam.Postgres.PgSpecific
import Database.Beam.Postgres.Migrate ( tsquery, tsvector, text, bytea, unboundedArray
                                      , json, jsonb, uuid, money, smallserial, serial
                                      , bigserial)
import Database.Beam.Postgres.Extensions ( PgExtensionEntity, IsPgExtension(..)
                                         , pgCreateExtension, pgDropExtension
                                         , getPgExtension )
import Database.Beam.Postgres.Debug

import qualified Database.PostgreSQL.Simple as Pg

instance BeamHasInsertOnConflict Postgres where
  type SqlConflictTarget Postgres table = PgInsertOnConflictTarget table
  type SqlConflictAction Postgres table = PgConflictAction table

  insertOnConflict tbl vs target action = insert tbl vs $ onConflict target action

  -- | Perform the conflict action when any constraint or index conflict occurs.
  -- Syntactically, this is the @ON CONFLICT@ clause, without any /conflict target/.
  anyConflict = PgInsertOnConflictTarget (\_ -> PgInsertOnConflictTargetSyntax mempty)

  -- | The Postgres @DO NOTHING@ action
  onConflictDoNothing = PgConflictAction $ \_ -> PgConflictActionSyntax (emit "DO NOTHING")

  -- | The Postgres @DO UPDATE SET@ action, without the @WHERE@ clause. The
  -- argument takes an updatable row (like the one used in 'update') and the
  -- conflicting row. Use 'current_' on the first argument to get the current
  -- value of the row in the database.
  onConflictUpdateSet mkAssignments =
    PgConflictAction $ \tbl ->
    let QAssignment assignments = mkAssignments tbl tblExcluded
        tblExcluded = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField "excluded" nm)))) tbl

        assignmentSyntaxes =
          [ fromPgFieldName fieldNm <> emit "=" <> pgParens (fromPgExpression expr)
          | (fieldNm, expr) <- assignments ]
    in PgConflictActionSyntax $
       emit "DO UPDATE SET " <> pgSepBy (emit ", ") assignmentSyntaxes

  -- | The Postgres @DO UPDATE SET@ action, with the @WHERE@ clause. This is like
  -- 'onConflictUpdateSet', but only rows satisfying the given condition are
  -- updated. Sometimes this results in more efficient locking. See the Postgres
  -- <https://www.postgresql.org/docs/current/static/sql-insert.html manual> for
  -- more information.
  onConflictUpdateSetWhere mkAssignments where_ =
    PgConflictAction $ \tbl ->
    let QAssignment assignments = mkAssignments tbl tblExcluded
        QExpr where_' = where_ (changeBeamRep (\(Columnar' f) -> Columnar' (current_ f)) tbl)
        tblExcluded = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField "excluded" nm)))) tbl

        assignmentSyntaxes =
          [ fromPgFieldName fieldNm <> emit "=" <> pgParens (fromPgExpression expr)
          | (fieldNm, expr) <- assignments ]
    in PgConflictActionSyntax $
       emit "DO UPDATE SET " <> pgSepBy (emit ", ") assignmentSyntaxes <> emit " WHERE " <> fromPgExpression (where_' "t")

  -- | Perform the conflict action only when these fields conflict. The first
  -- argument gets the current row as a table of expressions. Return the conflict
  -- key. For more information, see the @beam-postgres@ manual.
  conflictingFields makeProjection =
    PgInsertOnConflictTarget $ \tbl ->
    PgInsertOnConflictTargetSyntax $
    pgParens (pgSepBy (emit ", ") $
              map fromPgExpression $
              project (Proxy @Postgres) (makeProjection tbl) "t") <>
    emit " "

  -- | Like 'conflictingFields', but only perform the action if the condition
  -- given in the second argument is met. See the postgres
  -- <https://www.postgresql.org/docs/current/static/sql-insert.html manual> for
  -- more information.
  conflictingFieldsWhere makeProjection makeWhere =
    PgInsertOnConflictTarget $ \tbl ->
    PgInsertOnConflictTargetSyntax $
    pgParens (pgSepBy (emit ", ") $
              map fromPgExpression (project (Proxy @Postgres)
                                            (makeProjection tbl) "t")) <>
    emit " WHERE " <>
    pgParens (let QExpr mkE = makeWhere tbl
                  PgExpressionSyntax e = mkE "t"
              in e) <>
    emit " "
