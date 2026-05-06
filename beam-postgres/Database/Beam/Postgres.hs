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
-- <https://haskell-beam.github.io/beam/user-guide/backends/beam-postgres/ its manual>.
module Database.Beam.Postgres
  ( -- * Beam Postgres backend
    Postgres (..),
    Pg,
    liftIOWithHandle,

    -- ** Executing actions against the backend
    runBeamPostgres,
    runBeamPostgresDebug,

    -- ** Postgres syntax
    PgCommandSyntax,
    PgSyntax,
    PgSelectSyntax,
    PgInsertSyntax,
    PgUpdateSyntax,
    PgDeleteSyntax,

    -- ** COPY support

    --
    -- File-mode @COPY@ support for Postgres. Use 'copyToText' / 'copyToCSV' (or 'copyFromText' /
    -- 'copyFromCSV') for default options, and the @*With@ variants to override
    -- format-specific fields.

    -- *** text format
    copyToText,
    copyToTextWith,
    PgTextCopyToOptions (..),
    defaultPgTextCopyToOptions,
    copyFromText,
    copyFromTextWith,
    PgTextCopyFromOptions (..),
    defaultPgTextCopyFromOptions,

    -- *** CSV format
    copyToCSV,
    copyToCSVWith,
    PgCSVCopyToOptions (..),
    defaultPgCSVCopyToOptions,
    copyFromCSV,
    copyFromCSVWith,
    PgCSVCopyFromOptions (..),
    defaultPgCSVCopyFromOptions,

    -- *** Top-level options sums
    PgCopyToOptions,
    PgCopyFromOptions,

    -- ** Streaming COPY support

    --
    -- Streaming @COPY ... TO STDOUT@ / @COPY ... FROM STDIN@.
    -- Use 'copyToTextStream' / 'copyToCSVStream' (or 'copyFromTextStream' /
    -- 'copyFromCSVStream') with the 'runCopyToStream' / 'runCopyFromStream'
    -- runners from "Database.Beam.Backend.SQL.BeamExtensions".

    -- *** text format
    copyToTextStream,
    copyToTextStreamWith,
    copyFromTextStream,
    copyFromTextStreamWith,

    -- *** CSV format
    copyToCSVStream,
    copyToCSVStreamWith,
    copyFromCSVStream,
    copyFromCSVStreamWith,

    -- *** Top-level options sums
    PgCopyToStreamOptions,
    PgCopyFromStreamOptions,

    -- * Beam URI support
    postgresUriSyntax,

    -- * Postgres-specific features

    -- ** Postgres-specific data types
    json,
    jsonb,
    uuid,
    money,
    tsquery,
    tsvector,
    text,
    bytea,
    unboundedArray,

    -- *** @SERIAL@ support
    smallserial,
    serial,
    bigserial,
    module Database.Beam.Postgres.PgSpecific,
    module Database.Beam.Postgres.TempTable,

    -- ** Postgres extension support
    PgExtensionEntity,
    IsPgExtension (..),
    pgCreateExtension,
    pgDropExtension,
    getPgExtension,

    -- ** Utilities for defining custom instances
    fromPgIntegral,
    fromPgScientificOrIntegral,

    -- ** Debug support
    PgDebugStmt,
    pgTraceStmtIO,
    pgTraceStmtIO',
    pgTraceStmt,

    -- * @postgresql-simple@ re-exports
    Pg.ResultError (..),
    Pg.SqlError (..),
    Pg.Connection,
    Pg.ConnectInfo (..),
    Pg.defaultConnectInfo,
    Pg.connectPostgreSQL,
    Pg.connect,
    Pg.close,
  )
where

import Database.Beam.Postgres.Connection
-- for BeamHasInsertOnConflict instance

import Database.Beam.Postgres.Debug
import Database.Beam.Postgres.Extensions
  ( IsPgExtension (..),
    PgExtensionEntity,
    getPgExtension,
    pgCreateExtension,
    pgDropExtension,
  )
import Database.Beam.Postgres.Extensions.Copy.File
  ( PgCSVCopyFromOptions (..),
    PgCSVCopyToOptions (..),
    PgCopyFromOptions,
    PgCopyToOptions,
    PgTextCopyFromOptions (..),
    PgTextCopyToOptions (..),
    copyFromCSV,
    copyFromCSVWith,
    copyFromText,
    copyFromTextWith,
    copyToCSV,
    copyToCSVWith,
    copyToText,
    copyToTextWith,
    defaultPgCSVCopyFromOptions,
    defaultPgCSVCopyToOptions,
    defaultPgTextCopyFromOptions,
    defaultPgTextCopyToOptions,
  )
import Database.Beam.Postgres.Extensions.Copy.Stream
  ( PgCopyFromStreamOptions,
    PgCopyToStreamOptions,
    copyFromCSVStream,
    copyFromCSVStreamWith,
    copyFromTextStream,
    copyFromTextStreamWith,
    copyToCSVStream,
    copyToCSVStreamWith,
    copyToTextStream,
    copyToTextStreamWith,
  )
import Database.Beam.Postgres.Full ()
import Database.Beam.Postgres.Migrate
  ( bigserial,
    bytea,
    json,
    jsonb,
    money,
    serial,
    smallserial,
    text,
    tsquery,
    tsvector,
    unboundedArray,
    uuid,
  )
import Database.Beam.Postgres.PgSpecific
import Database.Beam.Postgres.Syntax
  ( PgCommandSyntax,
    PgDeleteSyntax,
    PgInsertSyntax,
    PgSelectSyntax,
    PgSyntax,
    PgUpdateSyntax,
  )
import Database.Beam.Postgres.TempTable
import Database.Beam.Postgres.Types
  ( Postgres (..),
    fromPgIntegral,
    fromPgScientificOrIntegral,
  )
import qualified Database.PostgreSQL.Simple as Pg
