{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.Sqlite.TempTable
  ( -- * Temporary tables

    -- ** @CREATE TEMPORARY TABLE@
    runCreateTempTable

    -- ** Options
  , TempTableCreateMode(..)
  , TempTableOptions(..)
  , defaultTempTableOptions

    -- ** Schema constraint
  , HasDefaultSqliteTempTableSchema

  ) where

import           Database.Beam.Migrate.TempTable
  ( BeamHasTempTables(..)
  , HasDefaultTempTableSchema
  , TempTableCreateMode(..)
  , TempTableOptions(..)
  , defaultTempTableOptions
  , runCreateTempTable
  )
import           Database.Beam.Sqlite.Connection (Sqlite)
import           Database.Beam.Sqlite.Syntax
  ( SqliteCommandSyntax(..)
  , fromSqliteColumnSchema, sqliteIsSerialColumn
  , emit, quotedIdentifier, parens, commas
  )

--------------------------------------------------------------------------------

-- | Tables for which @CREATE TEMPORARY TABLE@ statements can be emitted
-- automatically.
type HasDefaultSqliteTempTableSchema tbl = HasDefaultTempTableSchema Sqlite tbl

instance BeamHasTempTables Sqlite where
  createTempTableCmd nm colDefs pkCols ifNotExists =
    SqliteCommandSyntax $
      emit "CREATE TEMPORARY TABLE "
        <> (if ifNotExists then emit "IF NOT EXISTS " else mempty)
        <> quotedIdentifier nm
        <> parens (commas (colDefPieces ++ pkPiece))
    where
      colDefPieces =
        map (\(n, s) -> quotedIdentifier n <> emit " " <> fromSqliteColumnSchema s)
            colDefs

      -- SQLite's INTEGER PRIMARY KEY AUTOINCREMENT already encodes the
      -- primary key constraint, so a separate PRIMARY KEY constraint would
      -- constitute a syntax error.
      hasSerial = any (sqliteIsSerialColumn . snd) colDefs
      pkPiece
        | hasSerial || null pkCols = []
        | otherwise =
            [ emit "PRIMARY KEY "
                <> parens (commas (map quotedIdentifier pkCols)) ]

  dropTempTableIfExistsCmd nm =
    SqliteCommandSyntax $
      emit "DROP TABLE IF EXISTS " <> quotedIdentifier nm
