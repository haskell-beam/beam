{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | DuckDB is a powerful in-process database specialized in analytical processing workloads.
--
-- The @beam-duckdb@ library is built atop of @duckdb-simple@, which is
-- used for connection management, transaction support, serialization, and
-- deserialization.
--
-- @beam-duckdb@ supports most beam features as well as many DuckDB-specific
-- features, such as support for reading Parquet files and Apache Iceberg tables.
module Database.Beam.DuckDB
  ( -- * Executing DuckDB queries
    runBeamDuckDB,

    -- ** Executing DuckDB queries with debugging
    runBeamDuckDBDebug,
    runBeamDuckDBDebugString,

    -- * Backend datatype
    DuckDB,
    DuckDBM,

    -- * @RETURNING@ support

    -- | The 'MonadBeamInsertReturning', 'MonadBeamUpdateReturning', and
    -- 'MonadBeamDeleteReturning' instances let DuckDB users capture the
    -- newly-inserted, updated, or deleted rows of a DML statement, which is
    -- useful for retrieving defaulted/serial column values.
    MonadBeamInsertReturning (..),
    runInsertReturningList,
    MonadBeamUpdateReturning (..),
    runUpdateReturningList,
    MonadBeamDeleteReturning (..),
    runDeleteReturningList,

    -- * DuckDB-specific functionality

    -- ** Data sources
    DataSource,
    DataSourceEntity,
    dataSource,
    modifyDataSourceFields,
    allFromDataSource_,

    -- *** Parquet
    parquet,

    -- *** Apache Iceberg tables
    icebergTable,
    icebergTableWith,
    IcebergTableOptions (..),
    defaultIcebergTableOptions,

    -- *** CSV
    csv,
    csvWith,
    CSVOptions (..),
    defaultCSVOptions,

    -- ** COPY support
    DuckDBCopyToOptions,
    DuckDBCopyFromOptions,

    -- *** CSV
    copyToCSV,
    copyToCSVWith,
    DuckDBCSVCopyToOptions (..),
    defaultDuckDBCSVCopyToOptions,
    copyFromCSV,
    copyFromCSVWith,
    DuckDBCSVCopyFromOptions (..),
    defaultDuckDBCSVCopyFromOptions,

    -- *** Parquet
    copyToParquet,
    copyToParquetWith,
    DuckDBParquetCopyToOptions (..),
    ParquetCompression (..),
    defaultDuckDBParquetCopyToOptions,
    copyFromParquet,
    copyFromParquetWith,
    DuckDBParquetCopyFromOptions (..),
    defaultDuckDBParquetCopyFromOptions,

    -- *** JSON
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

import Database.Beam.Backend.SQL.BeamExtensions
  ( MonadBeamDeleteReturning (..),
    MonadBeamInsertReturning (..),
    MonadBeamUpdateReturning (..),
    runDeleteReturningList,
    runInsertReturningList,
    runUpdateReturningList,
  )
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Connection
  ( DuckDBM,
    runBeamDuckDB,
    runBeamDuckDBDebug,
    runBeamDuckDBDebugString,
  )
import Database.Beam.DuckDB.Syntax.Extensions ()
import Database.Beam.DuckDB.Syntax.Extensions.Copy
  ( DuckDBCSVCopyFromOptions (..),
    DuckDBCSVCopyToOptions (..),
    DuckDBCopyFromOptions,
    DuckDBCopyToOptions,
    DuckDBJSONCopyFromOptions (..),
    DuckDBJSONCopyToOptions (..),
    DuckDBParquetCopyFromOptions (..),
    DuckDBParquetCopyToOptions (..),
    JSONCompression (..),
    ParquetCompression (..),
    copyFromCSV,
    copyFromCSVWith,
    copyFromJSON,
    copyFromJSONWith,
    copyFromParquet,
    copyFromParquetWith,
    copyToCSV,
    copyToCSVWith,
    copyToJSON,
    copyToJSONWith,
    copyToParquet,
    copyToParquetWith,
    defaultDuckDBCSVCopyFromOptions,
    defaultDuckDBCSVCopyToOptions,
    defaultDuckDBJSONCopyFromOptions,
    defaultDuckDBJSONCopyToOptions,
    defaultDuckDBParquetCopyFromOptions,
    defaultDuckDBParquetCopyToOptions,
  )
import Database.Beam.DuckDB.Syntax.Extensions.DataSource
