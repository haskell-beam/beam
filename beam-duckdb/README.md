# `beam-duckdb`: Beam backend for the DuckDB analytics database

`beam-duckdb` is a beam backend for
the [DuckDB analytics database](https://duckdb.org/). 

`beam-duckdb` extends the set of capabilities provided by `beam-core` in a few key
ways. Most importantly, sources of data _other_ than database tables can be
loaded by DuckDB and used for queries like a regular table. Currently, `beam-duckdb`
supports loading Parquet files, Apache Iceberg tables, and CSV files.

`beam-duckdb` is the most recent backend for beam; do not hesitate to raise an issue
if you'd like us to add support for something that isn't currently covered!

## Installation

The underlying duckdb-ffi needs to link to libduckdb. By default the installation assumes:

* The duckdb files are in `/usr/lib/duckdb`
* You're using duckdb 1.4.*

For Linux you can install a blessed version of duckdb in the correct path by cloining the duckdb-hasekll repo and running `make install` in the [duckdb-ffi cbits folder](https://github.com/Tritlo/duckdb-haskell/tree/main/duckdb-ffi/cbits).

## Example

From the project root directory, execute `cabal run ./examples/ExamScores.hs`.
