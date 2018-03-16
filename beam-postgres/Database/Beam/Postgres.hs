module Database.Beam.Postgres
  ( Postgres

    -- Only expose the 'Pg' type, not the constructors
  , BeamPostgres.Pg

    -- * @beam-postgres@ errors
  , RowReadError(..), PgError(..)
  , Pg.ResultError(..), Pg.SqlError(..)

    -- * Postgres URI Syntax
  , postgresUriSyntax

    -- * Postgres-specific features

  , module Database.Beam.Postgres.PgSpecific

    -- ** Postgres extensions
  , PgExtensionEntity, IsPgExtension(..)
  , pgCreateExtension, pgDropExtension
  , getPgExtension

  -- * @postgresql-simple@ Re-exports
  , Pg.Connection
  , Pg.ExecStatus(..)

  , Pg.ConnectInfo(..), Pg.defaultConnectInfo

  , Pg.postgreSQLConnectionString

  , Pg.connectPostgreSQL, Pg.connect
  , Pg.close ) where

import Database.Beam.Postgres.Connection hiding (Pg(..), PgF(..), pgRenderSyntax, runPgRowReader, getFields)
import Database.Beam.Postgres.Types (Postgres)
import Database.Beam.Postgres.PgSpecific
import Database.Beam.Postgres.Extensions ( PgExtensionEntity, IsPgExtension(..)
                                         , pgCreateExtension, pgDropExtension
                                         , getPgExtension )

import qualified Database.Beam.Postgres.Connection as BeamPostgres

import qualified Database.PostgreSQL.Simple as Pg
