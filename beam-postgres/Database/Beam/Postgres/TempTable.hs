{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.Postgres.TempTable
  ( -- * Temporary tables

    -- ** @CREATE TEMPORARY TABLE@
    runCreateTempTable

    -- ** Options
  , TempTableCreateMode(..)
  , TempTableOptions(..)
  , defaultTempTableOptions

    -- ** Schema constraint
  , HasDefaultPostgresTempTableSchema

  ) where

import           Database.Beam.Migrate.TempTable
  ( BeamHasTempTables(..)
  , HasDefaultTempTableSchema
  , TempTableCreateMode(..)
  , TempTableOptions(..)
  , defaultTempTableOptions
  , runCreateTempTable
  )
import           Database.Beam.Postgres.Types (Postgres)
import           Database.Beam.Postgres.Syntax
  ( PgCommandSyntax(..), PgCommandType(..)
  , fromPgColumnSchema
  , emit, pgQuotedIdentifier, pgSepBy, pgParens
  )

--------------------------------------------------------------------------------

-- | Tables for which @CREATE TEMPORARY TABLE@ statements can be emitted
-- automatically.
type HasDefaultPostgresTempTableSchema tbl = HasDefaultTempTableSchema Postgres tbl

instance BeamHasTempTables Postgres where
  createTempTableCmd nm colDefs pkCols ifNotExists =
    PgCommandSyntax PgCommandTypeDdl $
      emit "CREATE TEMPORARY TABLE "
        <> (if ifNotExists then emit "IF NOT EXISTS " else mempty)
        <> pgQuotedIdentifier nm
        <> pgParens (pgSepBy (emit ", ") (colDefPieces ++ pkPiece))
    where
      colDefPieces =
        map (\(n, s) -> pgQuotedIdentifier n <> emit " " <> fromPgColumnSchema s)
            colDefs

      pkPiece
        | null pkCols = []
        | otherwise =
            [ emit "PRIMARY KEY "
                <> pgParens (pgSepBy (emit ", ") (map pgQuotedIdentifier pkCols)) ]

  dropTempTableIfExistsCmd nm =
    PgCommandSyntax PgCommandTypeDdl $
      emit "DROP TABLE IF EXISTS " <> pgQuotedIdentifier nm
