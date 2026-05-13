# Revision history for beam-duckdb

## 0.3.1.0 -- 2026-05-13

* Added support for `beam-migrate`: the new `Database.Beam.DuckDB.Migrate`
  module exposes a `migrationBackend :: BeamMigrationBackend DuckDB DuckDBM`
  value, along with other utilities. This brings DuckDB on par with `beam-postgres`
  and `beam-sqlite` for schema verification and migration script generation.
* Added file-mode `COPY ... TO 'file'` / `COPY ... FROM 'file'` support
  via the new `MonadBeamCopyTo` / `MonadBeamCopyFrom` instances on
  `DuckDBM`. Smart constructors `copyToCSV` / `copyToParquet` /
  `copyToJSON` (and the symmetric `copyFrom*` plus `*With` variants)
  build the per-format options records. See
  `Database.Beam.DuckDB.Syntax.Extensions.Copy`.
* Added instances for `MonadBeamInsertReturning` / `MonadBeamUpdateReturning` / `MonadBeamDeleteReturning`;
* Added a `BeamHasInsertOnConflict DuckDB` instance, exposing
  `insertOnConflict` (and the `anyConflict` / `conflictingFields` /
  `onConflictDoNothing` / `onConflictUpdateSet` /
  `onConflictUpdateSetWhere` / `onConflictUpdateAll` /
  `onConflictUpdateInstead` combinators) for `INSERT ... ON CONFLICT`
  on DuckDB. DuckDB does not support partial-index conflict targets
  ([INSERT docs](https://duckdb.org/docs/stable/sql/statements/insert),
  so `conflictingFieldsWhere` is *not* re-exported from
  `Database.Beam.DuckDB`; the `conflictingFieldsWhere` in that module
  is a shadowing shim that produces a compile-time `TypeError`
  pointing users at `onConflictUpdateSetWhere`. Importing
  `conflictingFieldsWhere` directly from
  `Database.Beam.Backend.SQL.BeamExtensions` still type-checks but
  raises an error at runtime.

## 0.3.0.0 -- 2026-04-28

* Added a `weekField` definition to the `IsSql92ExtractFieldSyntax` instance
  for `DuckDBExtractFieldSyntax`, in support of the new backend-agnostic
  `week_` extract field from `beam-core`.
* Bumped the lower bound on `beam-core` to `0.11` and the lower bound on
  `beam-migrate` to `0.6`.

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
