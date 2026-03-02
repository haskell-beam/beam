# Revision history for beam-duckdb

## 0.1.1.0 -- unreleased

* Fixed an issue with modeling boolean conditions, whereby, for example, checking if something was true was modeled as `... IS 1` (as for Sqlite), rather than `... IS TRUE` (like Postgres).
* Added a `FromBackendRow DuckDB Scientific`, `HasSqlEqualityCheck DuckDB Scientific`, and `HasSqlQuantifiedEqualityCheck DuckDB Scientific` instances, which are required to build the documentation.

## 0.1.0.0 -- 2026-02-26

* First version. Released on an unsuspecting world.
