# `beam-duckdb`: Beam backend for the DuckDB analytics database

`beam-duckdb` is a beam backend for
the [DuckDB analytics database](https://duckdb.org/). 

`beam-duckdb` extends the set of capabilities provided by `beam-core` in a few key
ways. Most importantly, sources of data _other_ than database tables can be
loaded by DuckDB and used for queries like a regular table. Currently, `beam-duckdb`
supports loading Parquet files, Apache Iceberg tables, and CSV files.

`beam-duckdb` is the most recent backend for beam; do not hesitate to raise an issue
if you'd like us to add support for something that isn't currently covered!
