{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Database.Beam.DuckDB.Syntax.Extensions.Copy
  ( DuckDBCopyToSyntax (..),
    DuckDBCopyFromSyntax (..),

    -- * COPY options
    DuckDBCopyToOptions,
    DuckDBCopyFromOptions,

    -- ** CSV
    copyToCSV,
    copyToCSVWith,
    DuckDBCSVCopyToOptions (..),
    defaultDuckDBCSVCopyToOptions,
    copyFromCSV,
    copyFromCSVWith,
    DuckDBCSVCopyFromOptions (..),
    defaultDuckDBCSVCopyFromOptions,

    -- ** Parquet
    copyToParquet,
    copyToParquetWith,
    DuckDBParquetCopyToOptions (..),
    ParquetCompression (..),
    defaultDuckDBParquetCopyToOptions,
    copyFromParquet,
    copyFromParquetWith,
    DuckDBParquetCopyFromOptions (..),
    defaultDuckDBParquetCopyFromOptions,

    -- ** JSON
    copyToJSON,
    copyToJSONWith,
    DuckDBJSONCopyToOptions (..),
    JSONCompression (..),
    defaultDuckDBJSONCopyToOptions,
    copyFromJSON,
    copyFromJSONWith,
    DuckDBJSONCopyFromOptions (..),
    defaultDuckDBJSONCopyFromOptions,
  )
where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.DuckDB.Syntax (DuckDBSelectSyntax (..))
import Database.Beam.DuckDB.Syntax.Builder (DuckDBSyntax, commas, emit, emit', emitChar, emitCharLit, emitIntegral, emitTextLit, parens, quotedIdentifier, sepBy)

-- | DuckDB @COPY ... TO@ source syntax. Wraps the table name (with
-- optional column list) or a @SELECT@ subquery.
newtype DuckDBCopyToSourceSyntax = DuckDBCopyToSourceSyntax {fromDuckDBCopyToSource :: DuckDBSyntax}

-- | DuckDB @COPY ... TO@ statement syntax.
newtype DuckDBCopyToSyntax = DuckDBCopyToSyntax {fromDuckDBCopyTo :: DuckDBSyntax}

instance IsSqlCopyToSourceSyntax DuckDBCopyToSourceSyntax where
  type SqlCopyToSourceSelectSyntax DuckDBCopyToSourceSyntax = DuckDBSelectSyntax

  copyTableToSyntax mSchema tableName' mColumns =
    DuckDBCopyToSourceSyntax $
      maybe (emit tableName') (\schema -> emit schema <> emitChar '.' <> emit tableName') mSchema
        <> case mColumns of
          Nothing -> mempty
          Just cols -> parens $ commas (map quotedIdentifier (NE.toList cols))

  copySelectToSyntax (DuckDBSelectSyntax select) = DuckDBCopyToSourceSyntax $ parens select

instance IsSqlCopyToSyntax DuckDBCopyToSyntax where
  type SqlCopyToSourceSyntax DuckDBCopyToSyntax = DuckDBCopyToSourceSyntax
  type SqlCopyToParams DuckDBCopyToSyntax = DuckDBCopyToOptions

  copyToStmt source options =
    let (filepath, optionsSyntax) = emitDuckDBCopyToOptions options
     in DuckDBCopyToSyntax $
          mconcat
            [ emit "COPY ",
              coerce source,
              emit " TO ",
              emitChar '\'',
              emit (Text.pack filepath),
              emitChar '\'',
              optionsSyntax
            ]

-- | DuckDB @COPY ... FROM@ source syntax. Just the table name plus optional
-- column list.
newtype DuckDBCopyFromSourceSyntax = DuckDBCopyFromSourceSyntax {fromDuckDBCopyFromSource :: DuckDBSyntax}

-- | DuckDB @COPY ... FROM@ statement syntax.
newtype DuckDBCopyFromSyntax = DuckDBCopyFromSyntax {fromDuckDBCopyFrom :: DuckDBSyntax}

instance IsSqlCopyFromSourceSyntax DuckDBCopyFromSourceSyntax where
  copyTableFromSyntax mSchema tableName' mColumns =
    DuckDBCopyFromSourceSyntax $
      maybe (emit tableName') (\schema -> emit schema <> emitChar '.' <> emit tableName') mSchema
        <> case mColumns of
          Nothing -> mempty
          Just cols -> parens $ commas (map quotedIdentifier (NE.toList cols))

instance IsSqlCopyFromSyntax DuckDBCopyFromSyntax where
  type SqlCopyFromSourceSyntax DuckDBCopyFromSyntax = DuckDBCopyFromSourceSyntax
  type SqlCopyFromParams DuckDBCopyFromSyntax = DuckDBCopyFromOptions

  copyFromStmt source options =
    let (filepath, optionsSyntax) = emitDuckDBCopyFromOptions options
     in DuckDBCopyFromSyntax $
          mconcat
            [ emit "COPY ",
              coerce source,
              emit " FROM ",
              emitChar '\'',
              emit (Text.pack filepath),
              emitChar '\'',
              optionsSyntax
            ]

-- | Options for DuckDB's @COPY ... TO@ statement, tagged by the output format.
--
-- Use 'copyToCSV', 'copyToParquet', and 'copyToJSON' to construct values of type 'DuckDBCopyToOptions'.
--
-- See <https://duckdb.org/docs/sql/statements/copy.html the DuckDB COPY documentation>
-- for the full set of options each format supports.
--
-- @since 0.3.1.0
data DuckDBCopyToOptions
  = DuckDBCopyToCSV !FilePath !DuckDBCSVCopyToOptions
  | DuckDBCopyToParquet !FilePath !DuckDBParquetCopyToOptions
  | DuckDBCopyToJSON !FilePath !DuckDBJSONCopyToOptions
  deriving (Eq, Show)

-- | Copy to a CSV file at the given path with default options.
-- Use 'copyToCSVWith' to override.
--
-- @since 0.3.1.0
copyToCSV :: FilePath -> DuckDBCopyToOptions
copyToCSV path = DuckDBCopyToCSV path defaultDuckDBCSVCopyToOptions

-- | Copy to a CSV file at the given path with the given options.
--
-- @since 0.3.1.0
copyToCSVWith :: FilePath -> DuckDBCSVCopyToOptions -> DuckDBCopyToOptions
copyToCSVWith = DuckDBCopyToCSV

-- | Copy to a Parquet file at the given path with default options.
-- Use 'copyToParquetWith' to override.
--
-- @since 0.3.1.0
copyToParquet :: FilePath -> DuckDBCopyToOptions
copyToParquet path = DuckDBCopyToParquet path defaultDuckDBParquetCopyToOptions

-- | Copy to a Parquet file at the given path with the given options.
--
-- @since 0.3.1.0
copyToParquetWith :: FilePath -> DuckDBParquetCopyToOptions -> DuckDBCopyToOptions
copyToParquetWith = DuckDBCopyToParquet

-- | Copy to a JSON file at the given path with default options.
-- Use 'copyToJSONWith' to override.
--
-- @since 0.3.1.0
copyToJSON :: FilePath -> DuckDBCopyToOptions
copyToJSON path = DuckDBCopyToJSON path defaultDuckDBJSONCopyToOptions

-- | Copy to a JSON file at the given path with the given options.
--
-- @since 0.3.1.0
copyToJSONWith :: FilePath -> DuckDBJSONCopyToOptions -> DuckDBCopyToOptions
copyToJSONWith = DuckDBCopyToJSON

-- | CSV-specific options for @COPY ... TO@.
--
-- @since 0.3.1.0
data DuckDBCSVCopyToOptions = DuckDBCSVCopyToOptions
  { csvCopyToHeader :: Maybe Bool,
    csvCopyToDelimiter :: Maybe Text,
    csvCopyToQuote :: Maybe Char,
    csvCopyToEscape :: Maybe Char,
    csvCopyToNullStr :: Maybe Text,
    csvCopyToDateformat :: Maybe Text,
    csvCopyToTimestampformat :: Maybe Text,
    csvCopyToOverwriteOrIgnore :: Maybe Bool
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'DuckDBCSVCopyToOptions'; a sensible starting point for
-- record-update overrides.
--
-- @since 0.3.1.0
defaultDuckDBCSVCopyToOptions :: DuckDBCSVCopyToOptions
defaultDuckDBCSVCopyToOptions =
  DuckDBCSVCopyToOptions
    { csvCopyToHeader = Nothing,
      csvCopyToDelimiter = Nothing,
      csvCopyToQuote = Nothing,
      csvCopyToEscape = Nothing,
      csvCopyToNullStr = Nothing,
      csvCopyToDateformat = Nothing,
      csvCopyToTimestampformat = Nothing,
      csvCopyToOverwriteOrIgnore = Nothing
    }

-- | Parquet-specific options for @COPY ... TO@.
--
-- @since 0.3.1.0
data DuckDBParquetCopyToOptions = DuckDBParquetCopyToOptions
  { parquetCopyToCompression :: Maybe ParquetCompression,
    parquetCopyToOverwriteOrIgnore :: Maybe Bool
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'DuckDBParquetCopyToOptions'; a sensible starting point
-- for record-update overrides.
--
-- @since 0.3.1.0
defaultDuckDBParquetCopyToOptions :: DuckDBParquetCopyToOptions
defaultDuckDBParquetCopyToOptions =
  DuckDBParquetCopyToOptions
    { parquetCopyToCompression = Nothing,
      parquetCopyToOverwriteOrIgnore = Nothing
    }

-- | JSON-specific options for @COPY ... TO@.
--
-- @since 0.3.1.0
data DuckDBJSONCopyToOptions = DuckDBJSONCopyToOptions
  { jsonCopyToCompression :: Maybe JSONCompression,
    jsonCopyToOverwriteOrIgnore :: Maybe Bool
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'DuckDBJSONCopyToOptions'; a sensible starting point for
-- record-update overrides.
--
-- @since 0.3.1.0
defaultDuckDBJSONCopyToOptions :: DuckDBJSONCopyToOptions
defaultDuckDBJSONCopyToOptions =
  DuckDBJSONCopyToOptions
    { jsonCopyToCompression = Nothing,
      jsonCopyToOverwriteOrIgnore = Nothing
    }

-- | Options for DuckDB's @COPY ... FROM@ statement, tagged by the input format.
--
-- Use 'copyFromCSV', 'copyFromParquet', and 'copyFromJSON' to construct values of type 'DuckDBCopyFromOptions'.
--
-- See <https://duckdb.org/docs/sql/statements/copy.html the DuckDB COPY documentation>
-- for the full set of options each format supports.
--
-- @since 0.3.1.0
data DuckDBCopyFromOptions
  = DuckDBCopyFromCSV !FilePath !DuckDBCSVCopyFromOptions
  | DuckDBCopyFromParquet !FilePath !DuckDBParquetCopyFromOptions
  | DuckDBCopyFromJSON !FilePath !DuckDBJSONCopyFromOptions
  deriving (Eq, Show)

-- | Copy from a CSV file at the given path with default options.
-- Use 'copyFromCSVWith' to override.
--
-- @since 0.3.1.0
copyFromCSV :: FilePath -> DuckDBCopyFromOptions
copyFromCSV path = DuckDBCopyFromCSV path defaultDuckDBCSVCopyFromOptions

-- | Copy from a CSV file at the given path with the given options.
--
-- @since 0.3.1.0
copyFromCSVWith :: FilePath -> DuckDBCSVCopyFromOptions -> DuckDBCopyFromOptions
copyFromCSVWith = DuckDBCopyFromCSV

-- | Copy from a Parquet file at the given path with default options.
-- Use 'copyFromParquetWith' to override.
--
-- @since 0.3.1.0
copyFromParquet :: FilePath -> DuckDBCopyFromOptions
copyFromParquet path = DuckDBCopyFromParquet path defaultDuckDBParquetCopyFromOptions

-- | Copy from a Parquet file at the given path with the given options.
--
-- @since 0.3.1.0
copyFromParquetWith :: FilePath -> DuckDBParquetCopyFromOptions -> DuckDBCopyFromOptions
copyFromParquetWith = DuckDBCopyFromParquet

-- | Copy from a JSON file at the given path with default options.
-- Use 'copyFromJSONWith' to override.
--
-- @since 0.3.1.0
copyFromJSON :: FilePath -> DuckDBCopyFromOptions
copyFromJSON path = DuckDBCopyFromJSON path defaultDuckDBJSONCopyFromOptions

-- | Copy from a JSON file at the given path with the given options.
--
-- @since 0.3.1.0
copyFromJSONWith :: FilePath -> DuckDBJSONCopyFromOptions -> DuckDBCopyFromOptions
copyFromJSONWith = DuckDBCopyFromJSON

-- | CSV-specific options for @COPY ... FROM@.
--
-- @since 0.3.1.0
data DuckDBCSVCopyFromOptions = DuckDBCSVCopyFromOptions
  { csvCopyFromHeader :: Maybe Bool,
    csvCopyFromDelimiter :: Maybe Text,
    csvCopyFromQuote :: Maybe Char,
    csvCopyFromEscape :: Maybe Char,
    csvCopyFromNullStr :: Maybe Text,
    csvCopyFromDateformat :: Maybe Text,
    csvCopyFromTimestampformat :: Maybe Text,
    csvCopyFromAutoDetect :: Maybe Bool,
    csvCopyFromIgnoreErrors :: Maybe Bool,
    csvCopyFromSkip :: Maybe Int,
    csvCopyFromAllVarchar :: Maybe Bool
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'DuckDBCSVCopyFromOptions'; a sensible starting point for
-- record-update overrides.
--
-- @since 0.3.1.0
defaultDuckDBCSVCopyFromOptions :: DuckDBCSVCopyFromOptions
defaultDuckDBCSVCopyFromOptions =
  DuckDBCSVCopyFromOptions
    { csvCopyFromHeader = Nothing,
      csvCopyFromDelimiter = Nothing,
      csvCopyFromQuote = Nothing,
      csvCopyFromEscape = Nothing,
      csvCopyFromNullStr = Nothing,
      csvCopyFromDateformat = Nothing,
      csvCopyFromTimestampformat = Nothing,
      csvCopyFromAutoDetect = Nothing,
      csvCopyFromIgnoreErrors = Nothing,
      csvCopyFromSkip = Nothing,
      csvCopyFromAllVarchar = Nothing
    }

-- | Parquet-specific options for @COPY ... FROM@. DuckDB's Parquet reader has
-- almost no read-time options; this is here so the Parquet format can be
-- selected with consistent defaults.
--
-- @since 0.3.1.0
data DuckDBParquetCopyFromOptions = DuckDBParquetCopyFromOptions
  {
  }
  deriving (Eq, Show)

-- | The (only) 'DuckDBParquetCopyFromOptions' value.
--
-- @since 0.3.1.0
defaultDuckDBParquetCopyFromOptions :: DuckDBParquetCopyFromOptions
defaultDuckDBParquetCopyFromOptions = DuckDBParquetCopyFromOptions

-- | JSON-specific options for @COPY ... FROM@.
--
-- @since 0.3.1.0
data DuckDBJSONCopyFromOptions = DuckDBJSONCopyFromOptions
  { jsonCopyFromAutoDetect :: Maybe Bool,
    -- | @COMPRESSION@: how the input file is compressed. DuckDB's JSON
    -- reader normally auto-detects this from the file extension, but
    -- when the path doesn't carry the usual @.gz@ / @.zst@ suffix the
    -- compression has to be specified explicitly so it can be
    -- decompressed on read. Round-tripping a compressed JSON write
    -- requires this to match the matching @TO@-side compression.
    jsonCopyFromCompression :: Maybe JSONCompression
  }
  deriving (Eq, Show)

-- | All-'Nothing' 'DuckDBJSONCopyFromOptions'; a sensible starting point for
-- record-update overrides.
--
-- @since 0.3.1.0
defaultDuckDBJSONCopyFromOptions :: DuckDBJSONCopyFromOptions
defaultDuckDBJSONCopyFromOptions =
  DuckDBJSONCopyFromOptions
    { jsonCopyFromAutoDetect = Nothing,
      jsonCopyFromCompression = Nothing
    }

-- | Compression algorithms accepted by DuckDB's Parquet writer (and reader).
--
-- See <https://duckdb.org/docs/current/sql/statements/copy DuckDB's COPY documentation>;
-- the JSON format accepts a strictly smaller set of values, modelled
-- separately by 'JSONCompression'.
--
-- @since 0.3.1.0
data ParquetCompression
  = ParquetUncompressed
  | ParquetSnappy
  | ParquetGzip
  | ParquetZstd
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Compression algorithms accepted by DuckDB's JSON writer (and reader).
--
-- DuckDB's JSON format does not support @SNAPPY@, so 'JSONCompression'
-- omits it. See <https://duckdb.org/docs/current/sql/statements/copy DuckDB's COPY documentation>.
--
-- @since 0.3.1.0
data JSONCompression
  = JSONUncompressed
  | JSONGzip
  | JSONZstd
  deriving (Eq, Show, Ord, Enum, Bounded)

emitDuckDBCopyToOptions :: DuckDBCopyToOptions -> (FilePath, DuckDBSyntax)
emitDuckDBCopyToOptions (DuckDBCopyToCSV fp o) = (fp, emitOptionList (emit "CSV") (csvCopyToFields o))
emitDuckDBCopyToOptions (DuckDBCopyToParquet fp o) = (fp, emitOptionList (emit "PARQUET") (parquetCopyToFields o))
emitDuckDBCopyToOptions (DuckDBCopyToJSON fp o) = (fp, emitOptionList (emit "JSON") (jsonCopyToFields o))

emitDuckDBCopyFromOptions :: DuckDBCopyFromOptions -> (FilePath, DuckDBSyntax)
emitDuckDBCopyFromOptions (DuckDBCopyFromCSV fp o) = (fp, emitOptionList (emit "CSV") (csvCopyFromFields o))
emitDuckDBCopyFromOptions (DuckDBCopyFromParquet fp o) = (fp, emitOptionList (emit "PARQUET") (parquetCopyFromFields o))
emitDuckDBCopyFromOptions (DuckDBCopyFromJSON fp o) = (fp, emitOptionList (emit "JSON") (jsonCopyFromFields o))

-- | Render a non-empty list of options like @ (FORMAT FMT, OPT1 …, OPT2 …)@.
-- The @FORMAT@ entry is always present, since the constructor of the options
-- value pins it.
emitOptionList :: DuckDBSyntax -> [DuckDBSyntax] -> DuckDBSyntax
emitOptionList fmt items =
  emit " ("
    <> sepBy (emit ", ") ((emit "FORMAT " <> fmt) : items)
    <> emitChar ')'

csvCopyToFields :: DuckDBCSVCopyToOptions -> [DuckDBSyntax]
csvCopyToFields opts =
  catMaybes
    [ fmap (\x -> emit "HEADER " <> emit' x) (csvCopyToHeader opts),
      fmap (\x -> emit "DELIMITER " <> emitTextLit x) (csvCopyToDelimiter opts),
      fmap (\x -> emit "QUOTE " <> emitCharLit x) (csvCopyToQuote opts),
      fmap (\x -> emit "ESCAPE " <> emitCharLit x) (csvCopyToEscape opts),
      fmap (\x -> emit "NULLSTR " <> emitTextLit x) (csvCopyToNullStr opts),
      fmap (\x -> emit "DATEFORMAT " <> emitTextLit x) (csvCopyToDateformat opts),
      fmap (\x -> emit "TIMESTAMPFORMAT " <> emitTextLit x) (csvCopyToTimestampformat opts),
      fmap (\x -> emit "OVERWRITE_OR_IGNORE " <> emit' x) (csvCopyToOverwriteOrIgnore opts)
    ]

parquetCopyToFields :: DuckDBParquetCopyToOptions -> [DuckDBSyntax]
parquetCopyToFields opts =
  catMaybes
    [ fmap (\x -> emit "COMPRESSION " <> emitParquetCompression x) (parquetCopyToCompression opts),
      fmap (\x -> emit "OVERWRITE_OR_IGNORE " <> emit' x) (parquetCopyToOverwriteOrIgnore opts)
    ]

jsonCopyToFields :: DuckDBJSONCopyToOptions -> [DuckDBSyntax]
jsonCopyToFields opts =
  catMaybes
    [ fmap (\x -> emit "COMPRESSION " <> emitJSONCompression x) (jsonCopyToCompression opts),
      fmap (\x -> emit "OVERWRITE_OR_IGNORE " <> emit' x) (jsonCopyToOverwriteOrIgnore opts)
    ]

csvCopyFromFields :: DuckDBCSVCopyFromOptions -> [DuckDBSyntax]
csvCopyFromFields opts =
  catMaybes
    [ fmap (\x -> emit "HEADER " <> emit' x) (csvCopyFromHeader opts),
      fmap (\x -> emit "DELIMITER " <> emitTextLit x) (csvCopyFromDelimiter opts),
      fmap (\x -> emit "QUOTE " <> emitCharLit x) (csvCopyFromQuote opts),
      fmap (\x -> emit "ESCAPE " <> emitCharLit x) (csvCopyFromEscape opts),
      fmap (\x -> emit "NULLSTR " <> emitTextLit x) (csvCopyFromNullStr opts),
      fmap (\x -> emit "DATEFORMAT " <> emitTextLit x) (csvCopyFromDateformat opts),
      fmap (\x -> emit "TIMESTAMPFORMAT " <> emitTextLit x) (csvCopyFromTimestampformat opts),
      fmap (\x -> emit "AUTO_DETECT " <> emit' x) (csvCopyFromAutoDetect opts),
      fmap (\x -> emit "IGNORE_ERRORS " <> emit' x) (csvCopyFromIgnoreErrors opts),
      fmap (\x -> emit "SKIP " <> emitIntegral x) (csvCopyFromSkip opts),
      fmap (\x -> emit "ALL_VARCHAR " <> emit' x) (csvCopyFromAllVarchar opts)
    ]

parquetCopyFromFields :: DuckDBParquetCopyFromOptions -> [DuckDBSyntax]
parquetCopyFromFields _ = []

jsonCopyFromFields :: DuckDBJSONCopyFromOptions -> [DuckDBSyntax]
jsonCopyFromFields opts =
  catMaybes
    [ fmap (\x -> emit "AUTO_DETECT " <> emit' x) (jsonCopyFromAutoDetect opts),
      fmap (\x -> emit "COMPRESSION " <> emitJSONCompression x) (jsonCopyFromCompression opts)
    ]

emitPartitionBy :: NonEmpty Text -> DuckDBSyntax
emitPartitionBy cols =
  emit "PARTITION_BY ("
    <> commas (map quotedIdentifier (NE.toList cols))
    <> emitChar ')'

emitParquetCompression :: ParquetCompression -> DuckDBSyntax
emitParquetCompression ParquetUncompressed = emit "UNCOMPRESSED"
emitParquetCompression ParquetSnappy = emit "SNAPPY"
emitParquetCompression ParquetGzip = emit "GZIP"
emitParquetCompression ParquetZstd = emit "ZSTD"

emitJSONCompression :: JSONCompression -> DuckDBSyntax
emitJSONCompression JSONUncompressed = emit "UNCOMPRESSED"
emitJSONCompression JSONGzip = emit "GZIP"
emitJSONCompression JSONZstd = emit "ZSTD"
