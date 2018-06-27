-- | Postgres is a popular, open-source RDBMS. It is fairly standards compliant
-- and supports many advanced features and data types.
--
-- The @beam-postgres@ module is built atop of @postgresql-simple@, which is
-- used for connection management, transaction support, serialization, and
-- deserialization.
--
-- @beam-postgres@ supports most beam features as well as many postgres-specific
-- features. For example, @beam-postgres@ provides support for full-text search,
-- @DISTINCT ON@, JSON handling, postgres @ARRAY@s, and the @MONEY@ type.
--
-- The documentation for @beam-postgres@ functionality below indicates which
-- postgres function each function or type wraps. Postgres maintains its own
-- in-depth documentation. Please refer to that for more detailed information on
-- <https://www.postgresql.org/docs/current/static/index.html behavior>.
--
-- For examples on how to use @beam-postgres@ usage, see
-- <http://tathougies.github.io/beam/user-guide/backends/beam-postgres/ its manual>.

module Database.Beam.Postgres
  (
    -- * @beam-postgres@ errors
    -- @beam-postgres@ query and data manipulation functions may throw any error
    -- thrown by @postgresql-simple@ as well as the following two beam-specific
    -- errors.
    PgRowReadError(..), PgError(..)

    -- * Beam Postgres backend
  , Postgres(..), Pg

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

  -- * @postgresql-simple@ re-exports

  , Pg.ResultError(..), Pg.SqlError(..)

  , Pg.Connection, Pg.ConnectInfo(..)
  , Pg.defaultConnectInfo

  , Pg.connectPostgreSQL, Pg.connect
  , Pg.close

  ) where

import Database.Beam.Postgres.Connection
import Database.Beam.Postgres.Syntax hiding (PostgresInaccessible)
import Database.Beam.Postgres.Types
import Database.Beam.Postgres.PgSpecific
import Database.Beam.Postgres.Migrate ( tsquery, tsvector, text, bytea, unboundedArray
                                      , json, jsonb, uuid, money, smallserial, serial
                                      , bigserial)
import Database.Beam.Postgres.Extensions ( PgExtensionEntity, IsPgExtension(..)
                                         , pgCreateExtension, pgDropExtension
                                         , getPgExtension )

import qualified Database.PostgreSQL.Simple as Pg
