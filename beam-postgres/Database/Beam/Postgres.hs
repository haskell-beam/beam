module Database.Beam.Postgres
  ( module Database.Beam.Postgres.Connection
  , module Database.Beam.Postgres.Types
  , module Database.Beam.Postgres.Syntax
  , module Database.Beam.Postgres.PgSpecific

  , PgExtensionEntity, IsPgExtension(..)
  , pgCreateExtension, pgDropExtension
  , getPgExtension

    -- Only exposed the 'Pg' type, not the constructors
  , Pg.Pg ) where

import Database.Beam.Postgres.Connection hiding (Pg(..), PgF(..), pgRenderSyntax, runPgRowReader, getFields)
import Database.Beam.Postgres.Syntax hiding (PostgresInaccessible)
import Database.Beam.Postgres.Types
import Database.Beam.Postgres.PgSpecific
import Database.Beam.Postgres.Extensions ( PgExtensionEntity, IsPgExtension(..)
                                         , pgCreateExtension, pgDropExtension
                                         , getPgExtension )

import qualified Database.Beam.Postgres.Connection as Pg
