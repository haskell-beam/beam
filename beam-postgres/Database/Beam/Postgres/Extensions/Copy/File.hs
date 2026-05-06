{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.Postgres.Extensions.Copy.File
  ( PgCopyToSyntax (..),
    PgCopyToSourceSyntax (..),
    PgCopyFromSyntax (..),
    PgCopyFromSourceSyntax (..),

    -- * COPY options
    PgCopyToOptions,
    PgCopyFromOptions,

    -- ** text
    copyToText,
    copyToTextWith,
    PgTextCopyToOptions (..),
    defaultPgTextCopyToOptions,
    copyFromText,
    copyFromTextWith,
    PgTextCopyFromOptions (..),
    defaultPgTextCopyFromOptions,

    -- ** CSV
    copyToCSV,
    copyToCSVWith,
    PgCSVCopyToOptions (..),
    defaultPgCSVCopyToOptions,
    copyFromCSV,
    copyFromCSVWith,
    PgCSVCopyFromOptions (..),
    defaultPgCSVCopyFromOptions,

    -- * Internal — exposed for use by sibling modules
    emitOptionList,
    textCopyToFields,
    csvCopyToFields,
    textCopyFromFields,
    csvCopyFromFields,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam.Backend.SQL.BeamExtensions
  ( IsSqlCopyFromSourceSyntax (..),
    IsSqlCopyFromSyntax (..),
    IsSqlCopyToSourceSyntax (..),
    IsSqlCopyToSyntax (..),
  )
import Database.Beam.Postgres.Syntax
  ( PgSelectSyntax (..),
    PgSyntax,
    emit,
    pgBoolLit,
    pgCharLit,
    pgParens,
    pgQuotedIdentifier,
    pgSepBy,
    pgStringLit,
  )

-- | PostgreSQL @COPY ... TO@ source syntax. Wraps the table name (with
-- optional column list) or a @SELECT@ subquery.
--
-- @since 0.6.1.0
newtype PgCopyToSourceSyntax = PgCopyToSourceSyntax {fromPgCopyToSource :: PgSyntax}

-- | PostgreSQL @COPY ... TO@ statement syntax.
--
-- @since 0.6.1.0
newtype PgCopyToSyntax = PgCopyToSyntax {fromPgCopyTo :: PgSyntax}

-- | PostgreSQL @COPY ... FROM@ source syntax. Just the table name plus
-- optional column list — Postgres @COPY ... FROM@ does not accept a SELECT.
--
-- @since 0.6.1.0
newtype PgCopyFromSourceSyntax = PgCopyFromSourceSyntax {fromPgCopyFromSource :: PgSyntax}

-- | PostgreSQL @COPY ... FROM@ statement syntax.
--
-- @since 0.6.1.0
newtype PgCopyFromSyntax = PgCopyFromSyntax {fromPgCopyFrom :: PgSyntax}

instance IsSqlCopyToSourceSyntax PgCopyToSourceSyntax where
  type SqlCopyToSourceSelectSyntax PgCopyToSourceSyntax = PgSelectSyntax

  copyTableToSyntax mSchema tableName' mColumns =
    PgCopyToSourceSyntax $
      maybe mempty (\schema -> pgQuotedIdentifier schema <> emit ".") mSchema
        <> pgQuotedIdentifier tableName'
        <> case mColumns of
          Nothing -> mempty
          Just cols -> pgParens $ pgSepBy (emit ", ") (map pgQuotedIdentifier (NE.toList cols))

  copySelectToSyntax (PgSelectSyntax select) = PgCopyToSourceSyntax $ pgParens select

instance IsSqlCopyToSyntax PgCopyToSyntax where
  type SqlCopyToSourceSyntax PgCopyToSyntax = PgCopyToSourceSyntax
  type SqlCopyToParams PgCopyToSyntax = PgCopyToOptions

  copyToStmt (PgCopyToSourceSyntax source) options =
    let (filepath, optionsSyntax) = emitPgCopyToOptions options
     in PgCopyToSyntax $
          mconcat
            [ emit "COPY ",
              source,
              emit " TO ",
              pgStringLit (T.pack filepath),
              optionsSyntax
            ]

instance IsSqlCopyFromSourceSyntax PgCopyFromSourceSyntax where
  copyTableFromSyntax mSchema tableName' mColumns =
    PgCopyFromSourceSyntax $
      maybe mempty (\schema -> pgQuotedIdentifier schema <> emit ".") mSchema
        <> pgQuotedIdentifier tableName'
        <> case mColumns of
          Nothing -> mempty
          Just cols -> pgParens $ pgSepBy (emit ", ") (map pgQuotedIdentifier (NE.toList cols))

instance IsSqlCopyFromSyntax PgCopyFromSyntax where
  type SqlCopyFromSourceSyntax PgCopyFromSyntax = PgCopyFromSourceSyntax
  type SqlCopyFromParams PgCopyFromSyntax = PgCopyFromOptions

  copyFromStmt (PgCopyFromSourceSyntax source) options =
    let (filepath, optionsSyntax) = emitPgCopyFromOptions options
     in PgCopyFromSyntax $
          mconcat
            [ emit "COPY ",
              source,
              emit " FROM ",
              pgStringLit (T.pack filepath),
              optionsSyntax
            ]

-- | Options for PostgreSQL's @COPY ... TO@ statement, tagged by the output
-- format. Use 'copyToText' or 'copyToCSV' to construct.
--
-- See <https://www.postgresql.org/docs/current/sql-copy.html the PostgreSQL COPY documentation>
-- for the full set of options each format supports.
--
-- @since 0.6.1.0
data PgCopyToOptions
  = PgCopyToText !FilePath !PgTextCopyToOptions
  | PgCopyToCSV !FilePath !PgCSVCopyToOptions
  deriving (Eq, Show)

-- | Copy to a text file at the given path with default options.
-- Use 'copyToTextWith' to override.
--
-- @since 0.6.1.0
copyToText :: FilePath -> PgCopyToOptions
copyToText path = PgCopyToText path defaultPgTextCopyToOptions

-- | Copy to a text file at the given path with the given options.
--
-- @since 0.6.1.0
copyToTextWith :: FilePath -> PgTextCopyToOptions -> PgCopyToOptions
copyToTextWith = PgCopyToText

-- | Copy to a CSV file at the given path with default options.
-- Use 'copyToCSVWith' to override.
--
-- @since 0.6.1.0
copyToCSV :: FilePath -> PgCopyToOptions
copyToCSV path = PgCopyToCSV path defaultPgCSVCopyToOptions

-- | Copy to a CSV file at the given path with the given options.
--
-- @since 0.6.1.0
copyToCSVWith :: FilePath -> PgCSVCopyToOptions -> PgCopyToOptions
copyToCSVWith = PgCopyToCSV

-- | Options for PostgreSQL's @COPY ... FROM@ statement, tagged by the input
-- format. Use 'copyFromText' or 'copyFromCSV' to construct.
--
-- @since 0.6.1.0
data PgCopyFromOptions
  = PgCopyFromText !FilePath !PgTextCopyFromOptions
  | PgCopyFromCSV !FilePath !PgCSVCopyFromOptions
  deriving (Eq, Show)

-- | Copy from a text file at the given path with default options.
-- Use 'copyFromTextWith' to override.
--
-- @since 0.6.1.0
copyFromText :: FilePath -> PgCopyFromOptions
copyFromText path = PgCopyFromText path defaultPgTextCopyFromOptions

-- | Copy from a text file at the given path with the given options.
--
-- @since 0.6.1.0
copyFromTextWith :: FilePath -> PgTextCopyFromOptions -> PgCopyFromOptions
copyFromTextWith = PgCopyFromText

-- | Copy from a CSV file at the given path with default options.
-- Use 'copyFromCSVWith' to override.
--
-- @since 0.6.1.0
copyFromCSV :: FilePath -> PgCopyFromOptions
copyFromCSV path = PgCopyFromCSV path defaultPgCSVCopyFromOptions

-- | Copy from a CSV file at the given path with the given options.
--
-- @since 0.6.1.0
copyFromCSVWith :: FilePath -> PgCSVCopyFromOptions -> PgCopyFromOptions
copyFromCSVWith = PgCopyFromCSV

-- | Options for the @text@ format on @COPY ... TO@.
--
-- @since 0.6.1.0
data PgTextCopyToOptions = PgTextCopyToOptions
  { -- | @DELIMITER@: single-character column delimiter. Defaults to TAB.
    pgTextCopyToDelimiter :: Maybe Char,
    -- | @NULL@: token written for SQL @NULL@ values. Defaults to @\\N@.
    pgTextCopyToNullStr :: Maybe Text,
    -- | @HEADER@: emit a header row with column names.
    pgTextCopyToHeader :: Maybe Bool,
    -- | @ENCODING@: client-encoding override for the file.
    pgTextCopyToEncoding :: Maybe Text
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'PgTextCopyToOptions'; a sensible starting point for
-- record-update overrides.
--
-- @since 0.6.1.0
defaultPgTextCopyToOptions :: PgTextCopyToOptions
defaultPgTextCopyToOptions =
  PgTextCopyToOptions
    { pgTextCopyToDelimiter = Nothing,
      pgTextCopyToNullStr = Nothing,
      pgTextCopyToHeader = Nothing,
      pgTextCopyToEncoding = Nothing
    }

-- | Options for the @csv@ format on @COPY ... TO@.
--
-- @since 0.6.1.0
data PgCSVCopyToOptions = PgCSVCopyToOptions
  { -- | @DELIMITER@: single-character column delimiter. Defaults to a comma.
    pgCsvCopyToDelimiter :: Maybe Char,
    -- | @NULL@: token written for SQL @NULL@ values. Defaults to the empty
    -- string.
    pgCsvCopyToNullStr :: Maybe Text,
    -- | @HEADER@: emit a header row with column names.
    pgCsvCopyToHeader :: Maybe Bool,
    -- | @QUOTE@: single-character quote character. Defaults to @\"@.
    pgCsvCopyToQuote :: Maybe Char,
    -- | @ESCAPE@: single-character escape character. Defaults to the
    -- value of 'pgCsvCopyToQuote'.
    pgCsvCopyToEscape :: Maybe Char,
    -- | @ENCODING@: client-encoding override for the file.
    pgCsvCopyToEncoding :: Maybe Text
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'PgCSVCopyToOptions'.
--
-- @since 0.6.1.0
defaultPgCSVCopyToOptions :: PgCSVCopyToOptions
defaultPgCSVCopyToOptions =
  PgCSVCopyToOptions
    { pgCsvCopyToDelimiter = Nothing,
      pgCsvCopyToNullStr = Nothing,
      pgCsvCopyToHeader = Nothing,
      pgCsvCopyToQuote = Nothing,
      pgCsvCopyToEscape = Nothing,
      pgCsvCopyToEncoding = Nothing
    }

-- | Options for the @text@ format on @COPY ... FROM@.
--
-- @since 0.6.1.0
data PgTextCopyFromOptions = PgTextCopyFromOptions
  { -- | @DELIMITER@: single-character column delimiter. Defaults to TAB.
    pgTextCopyFromDelimiter :: Maybe Char,
    -- | @NULL@: token to interpret as SQL @NULL@. Defaults to @\\N@.
    pgTextCopyFromNullStr :: Maybe Text,
    -- | @HEADER@: skip the first line as a header row.
    pgTextCopyFromHeader :: Maybe Bool,
    -- | @ENCODING@: client-encoding override for the file.
    pgTextCopyFromEncoding :: Maybe Text,
    -- | @FREEZE@: skip WAL when loading; only legal under specific
    -- conditions (see the PostgreSQL @COPY@ documentation).
    pgTextCopyFromFreeze :: Maybe Bool
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'PgTextCopyFromOptions'.
--
-- @since 0.6.1.0
defaultPgTextCopyFromOptions :: PgTextCopyFromOptions
defaultPgTextCopyFromOptions =
  PgTextCopyFromOptions
    { pgTextCopyFromDelimiter = Nothing,
      pgTextCopyFromNullStr = Nothing,
      pgTextCopyFromHeader = Nothing,
      pgTextCopyFromEncoding = Nothing,
      pgTextCopyFromFreeze = Nothing
    }

-- | Options for the @csv@ format on @COPY ... FROM@.
--
-- @since 0.6.1.0
data PgCSVCopyFromOptions = PgCSVCopyFromOptions
  { -- | @DELIMITER@: single-character column delimiter. Defaults to a comma.
    pgCsvCopyFromDelimiter :: Maybe Char,
    -- | @NULL@: token to interpret as SQL @NULL@. Defaults to the empty
    -- string.
    pgCsvCopyFromNullStr :: Maybe Text,
    -- | @HEADER@: skip the first line as a header row.
    pgCsvCopyFromHeader :: Maybe Bool,
    -- | @QUOTE@: single-character quote character. Defaults to @\"@.
    pgCsvCopyFromQuote :: Maybe Char,
    -- | @ESCAPE@: single-character escape character. Defaults to the
    -- value of 'pgCsvCopyFromQuote'.
    pgCsvCopyFromEscape :: Maybe Char,
    -- | @ENCODING@: client-encoding override for the file.
    pgCsvCopyFromEncoding :: Maybe Text,
    -- | @FREEZE@: skip WAL when loading; only legal under specific
    -- conditions (see the PostgreSQL @COPY@ documentation).
    pgCsvCopyFromFreeze :: Maybe Bool
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'PgCSVCopyFromOptions'; a sensible starting point for
-- record-update overrides.
--
-- @since 0.6.1.0
defaultPgCSVCopyFromOptions :: PgCSVCopyFromOptions
defaultPgCSVCopyFromOptions =
  PgCSVCopyFromOptions
    { pgCsvCopyFromDelimiter = Nothing,
      pgCsvCopyFromNullStr = Nothing,
      pgCsvCopyFromHeader = Nothing,
      pgCsvCopyFromQuote = Nothing,
      pgCsvCopyFromEscape = Nothing,
      pgCsvCopyFromEncoding = Nothing,
      pgCsvCopyFromFreeze = Nothing
    }

-- * Emission

emitPgCopyToOptions :: PgCopyToOptions -> (FilePath, PgSyntax)
emitPgCopyToOptions (PgCopyToText fp o) = (fp, emitOptionList (emit "text") (textCopyToFields o))
emitPgCopyToOptions (PgCopyToCSV fp o) = (fp, emitOptionList (emit "csv") (csvCopyToFields o))

emitPgCopyFromOptions :: PgCopyFromOptions -> (FilePath, PgSyntax)
emitPgCopyFromOptions (PgCopyFromText fp o) = (fp, emitOptionList (emit "text") (textCopyFromFields o))
emitPgCopyFromOptions (PgCopyFromCSV fp o) = (fp, emitOptionList (emit "csv") (csvCopyFromFields o))

-- | Render @ (FORMAT FMT, OPT1 …, OPT2 …)@. The @FORMAT@ entry is always
-- present since the constructor of the options value pins it.
--
-- @since 0.6.1.0
emitOptionList :: PgSyntax -> [PgSyntax] -> PgSyntax
emitOptionList fmt items =
  emit " ("
    <> pgSepBy (emit ", ") ((emit "FORMAT " <> fmt) : items)
    <> emit ")"

-- | Render the per-option items for a 'PgTextCopyToOptions' value.
--
-- @since 0.6.1.0
textCopyToFields :: PgTextCopyToOptions -> [PgSyntax]
textCopyToFields o =
  catMaybes
    [ fmap (\c -> emit "DELIMITER " <> pgCharLit c) (pgTextCopyToDelimiter o),
      fmap (\s -> emit "NULL " <> pgStringLit s) (pgTextCopyToNullStr o),
      fmap (\b -> emit "HEADER " <> pgBoolLit b) (pgTextCopyToHeader o),
      fmap (\s -> emit "ENCODING " <> pgStringLit s) (pgTextCopyToEncoding o)
    ]

-- | Render the per-option items for a 'PgCSVCopyToOptions' value.
--
-- @since 0.6.1.0
csvCopyToFields :: PgCSVCopyToOptions -> [PgSyntax]
csvCopyToFields o =
  catMaybes
    [ fmap (\c -> emit "DELIMITER " <> pgCharLit c) (pgCsvCopyToDelimiter o),
      fmap (\s -> emit "NULL " <> pgStringLit s) (pgCsvCopyToNullStr o),
      fmap (\b -> emit "HEADER " <> pgBoolLit b) (pgCsvCopyToHeader o),
      fmap (\c -> emit "QUOTE " <> pgCharLit c) (pgCsvCopyToQuote o),
      fmap (\c -> emit "ESCAPE " <> pgCharLit c) (pgCsvCopyToEscape o),
      fmap (\s -> emit "ENCODING " <> pgStringLit s) (pgCsvCopyToEncoding o)
    ]

-- | Render the per-option items for a 'PgTextCopyFromOptions' value.
--
-- @since 0.6.1.0
textCopyFromFields :: PgTextCopyFromOptions -> [PgSyntax]
textCopyFromFields o =
  catMaybes
    [ fmap (\c -> emit "DELIMITER " <> pgCharLit c) (pgTextCopyFromDelimiter o),
      fmap (\s -> emit "NULL " <> pgStringLit s) (pgTextCopyFromNullStr o),
      fmap (\b -> emit "HEADER " <> pgBoolLit b) (pgTextCopyFromHeader o),
      fmap (\s -> emit "ENCODING " <> pgStringLit s) (pgTextCopyFromEncoding o),
      fmap (\b -> emit "FREEZE " <> pgBoolLit b) (pgTextCopyFromFreeze o)
    ]

-- | Render the per-option items for a 'PgCSVCopyFromOptions' value.
--
-- @since 0.6.1.0
csvCopyFromFields :: PgCSVCopyFromOptions -> [PgSyntax]
csvCopyFromFields o =
  catMaybes
    [ fmap (\c -> emit "DELIMITER " <> pgCharLit c) (pgCsvCopyFromDelimiter o),
      fmap (\s -> emit "NULL " <> pgStringLit s) (pgCsvCopyFromNullStr o),
      fmap (\b -> emit "HEADER " <> pgBoolLit b) (pgCsvCopyFromHeader o),
      fmap (\c -> emit "QUOTE " <> pgCharLit c) (pgCsvCopyFromQuote o),
      fmap (\c -> emit "ESCAPE " <> pgCharLit c) (pgCsvCopyFromEscape o),
      fmap (\s -> emit "ENCODING " <> pgStringLit s) (pgCsvCopyFromEncoding o),
      fmap (\b -> emit "FREEZE " <> pgBoolLit b) (pgCsvCopyFromFreeze o)
    ]
