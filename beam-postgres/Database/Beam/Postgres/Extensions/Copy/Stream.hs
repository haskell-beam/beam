{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.Postgres.Extensions.Copy.Stream
  ( PgCopyToStreamSyntax (..),
    PgCopyFromStreamSyntax (..),

    -- * COPY options
    PgCopyToStreamOptions,
    PgCopyFromStreamOptions,

    -- ** text
    copyToTextStream,
    copyToTextStreamWith,
    copyFromTextStream,
    copyFromTextStreamWith,

    -- ** CSV
    copyToCSVStream,
    copyToCSVStreamWith,
    copyFromCSVStream,
    copyFromCSVStreamWith,
  )
where

import Database.Beam.Backend.SQL.BeamExtensions
  ( IsSqlCopyFromStreamSyntax (..),
    IsSqlCopyToStreamSyntax (..),
  )
import Database.Beam.Postgres.Extensions.Copy.File
  ( PgCSVCopyFromOptions (..),
    PgCSVCopyToOptions (..),
    PgCopyFromSourceSyntax (..),
    PgCopyToSourceSyntax (..),
    PgTextCopyFromOptions (..),
    PgTextCopyToOptions (..),
    csvCopyFromFields,
    csvCopyToFields,
    defaultPgCSVCopyFromOptions,
    defaultPgCSVCopyToOptions,
    defaultPgTextCopyFromOptions,
    defaultPgTextCopyToOptions,
    emitOptionList,
    textCopyFromFields,
    textCopyToFields,
  )
import Database.Beam.Postgres.Syntax (PgSyntax, emit)

-- | PostgreSQL streaming @COPY ... TO STDOUT@ statement syntax.
--
-- @since 0.6.1.0
newtype PgCopyToStreamSyntax = PgCopyToStreamSyntax {fromPgCopyToStream :: PgSyntax}

-- | PostgreSQL streaming @COPY ... FROM STDIN@ statement syntax.
--
-- @since 0.6.1.0
newtype PgCopyFromStreamSyntax = PgCopyFromStreamSyntax {fromPgCopyFromStream :: PgSyntax}

instance IsSqlCopyToStreamSyntax PgCopyToStreamSyntax where
  type SqlCopyToStreamSourceSyntax PgCopyToStreamSyntax = PgCopyToSourceSyntax
  type SqlCopyToStreamParams PgCopyToStreamSyntax = PgCopyToStreamOptions

  copyToStreamStmt (PgCopyToSourceSyntax source) options =
    PgCopyToStreamSyntax $
      mconcat
        [ emit "COPY ",
          source,
          emit " TO STDOUT",
          emitPgCopyToStreamOptions options
        ]

instance IsSqlCopyFromStreamSyntax PgCopyFromStreamSyntax where
  type SqlCopyFromStreamSourceSyntax PgCopyFromStreamSyntax = PgCopyFromSourceSyntax
  type SqlCopyFromStreamParams PgCopyFromStreamSyntax = PgCopyFromStreamOptions

  copyFromStreamStmt (PgCopyFromSourceSyntax source) options =
    PgCopyFromStreamSyntax $
      mconcat
        [ emit "COPY ",
          source,
          emit " FROM STDIN",
          emitPgCopyFromStreamOptions options
        ]

-- | Options for PostgreSQL's streaming @COPY ... TO STDOUT@ statement,
-- tagged by the output format.
--
-- See <https://www.postgresql.org/docs/current/sql-copy.html the PostgreSQL COPY documentation>
-- for the full set of options each format supports.
--
-- @since 0.6.1.0
data PgCopyToStreamOptions
  = PgCopyToStreamText !PgTextCopyToOptions
  | PgCopyToStreamCSV !PgCSVCopyToOptions
  deriving (Eq, Show)

-- | Stream out using the @text@ format with default options.
-- Use 'copyToTextStreamWith' to override.
--
-- @since 0.6.1.0
copyToTextStream :: PgCopyToStreamOptions
copyToTextStream = PgCopyToStreamText defaultPgTextCopyToOptions

-- | Stream out using the @text@ format with the given options.
--
-- @since 0.6.1.0
copyToTextStreamWith :: PgTextCopyToOptions -> PgCopyToStreamOptions
copyToTextStreamWith = PgCopyToStreamText

-- | Stream out using the @csv@ format with default options.
-- Use 'copyToCSVStreamWith' to override.
--
-- @since 0.6.1.0
copyToCSVStream :: PgCopyToStreamOptions
copyToCSVStream = PgCopyToStreamCSV defaultPgCSVCopyToOptions

-- | Stream out using the @csv@ format with the given options.
--
-- @since 0.6.1.0
copyToCSVStreamWith :: PgCSVCopyToOptions -> PgCopyToStreamOptions
copyToCSVStreamWith = PgCopyToStreamCSV

-- | Options for PostgreSQL's streaming @COPY ... FROM STDIN@ statement,
-- tagged by the input format.
--
-- @since 0.6.1.0
data PgCopyFromStreamOptions
  = PgCopyFromStreamText !PgTextCopyFromOptions
  | PgCopyFromStreamCSV !PgCSVCopyFromOptions
  deriving (Eq, Show)

-- | Stream in using the @text@ format with default options.
-- Use 'copyFromTextStreamWith' to override.
--
-- @since 0.6.1.0
copyFromTextStream :: PgCopyFromStreamOptions
copyFromTextStream = PgCopyFromStreamText defaultPgTextCopyFromOptions

-- | Stream in using the @text@ format with the given options.
--
-- @since 0.6.1.0
copyFromTextStreamWith :: PgTextCopyFromOptions -> PgCopyFromStreamOptions
copyFromTextStreamWith = PgCopyFromStreamText

-- | Stream in using the @csv@ format with default options.
-- Use 'copyFromCSVStreamWith' to override.
--
-- @since 0.6.1.0
copyFromCSVStream :: PgCopyFromStreamOptions
copyFromCSVStream = PgCopyFromStreamCSV defaultPgCSVCopyFromOptions

-- | Stream in using the @csv@ format with the given options.
--
-- @since 0.6.1.0
copyFromCSVStreamWith :: PgCSVCopyFromOptions -> PgCopyFromStreamOptions
copyFromCSVStreamWith = PgCopyFromStreamCSV

-- | Render the @WITH (FORMAT ..., ...)@ tail of a streaming
-- @COPY ... TO STDOUT@ statement.
--
-- @since 0.6.1.0
emitPgCopyToStreamOptions :: PgCopyToStreamOptions -> PgSyntax
emitPgCopyToStreamOptions (PgCopyToStreamText o) = emitOptionList (emit "text") (textCopyToFields o)
emitPgCopyToStreamOptions (PgCopyToStreamCSV o) = emitOptionList (emit "csv") (csvCopyToFields o)

-- | Render the @WITH (FORMAT ..., ...)@ tail of a streaming
-- @COPY ... FROM STDIN@ statement.
--
-- @since 0.6.1.0
emitPgCopyFromStreamOptions :: PgCopyFromStreamOptions -> PgSyntax
emitPgCopyFromStreamOptions (PgCopyFromStreamText o) = emitOptionList (emit "text") (textCopyFromFields o)
emitPgCopyFromStreamOptions (PgCopyFromStreamCSV o) = emitOptionList (emit "csv") (csvCopyFromFields o)
