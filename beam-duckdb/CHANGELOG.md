# Revision history for beam-duckdb

## 0.2.0.0 -- 2026-03-07

* Support for the SQL99 feature set, including support for regex matching via `similarTo_` and common table expressions
  (both non-recursive and recursive);
* Support for the SQL2003 feature set, including OLAP functionality such as windowed aggregation and `FILTER`;
* Exposed the `DataSource` type, but without constructors. Use `parquet`, `csv`, or any of the other
  helper functions to construct a `DataSource`;
* Fixed an issue where the SQL queries generated via `allIn_` and `anyIn_` were not supported by DuckDB.
* Fixed an issue with `HasSqlValueSyntax DuckDB` instances overlapping. Users must now derive instances manually for
  custom types. This is a breaking change.

## 0.1.1.0 -- 2026-03-04

* Fixed an issue with modeling boolean conditions, whereby, for example, checking if something was true was modeled as `... IS 1` (as for Sqlite), rather than `... IS TRUE` (like Postgres).
* Added a `FromBackendRow DuckDB Scientific`, `HasSqlEqualityCheck DuckDB Scientific`, and `HasSqlQuantifiedEqualityCheck DuckDB Scientific` instances, which are required to build the documentation.

## 0.1.0.0 -- 2026-02-26

* First version. Released on an unsuspecting world.
