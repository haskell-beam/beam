# 0.6.1.0

## Added features

* Added file-mode `COPY ... TO 'file'` / `COPY ... FROM 'file'` support
  via the new `MonadBeamCopyTo` / `MonadBeamCopyFrom` instances on `Pg`.
  Smart constructors `copyToText` / `copyToCSV` (and `copyFromText` /
  `copyFromCSV`, plus `*With` variants) build the per-format options
  records. Note that this requires the `pg_write_server_files` /
  `pg_read_server_files` role (or superuser) on the connecting role —
  see `Database.Beam.Postgres.Extensions.Copy.File`.
* Added streaming `COPY ... TO STDOUT` / `COPY ... FROM STDIN` support
  via the new `MonadBeamCopyToStream` / `MonadBeamCopyFromStream`
  instances on `Pg`. Smart constructors `copyToTextStream` /
  `copyToCSVStream` / `copyFromTextStream` / `copyFromCSVStream` build
  the per-format options. Streaming COPY does not require any special
  role attribute and is the appropriate choice when the client and
  server are on different hosts — see
  `Database.Beam.Postgres.Extensions.Copy.Stream`.

# 0.6.0.0

## Interface changes

* Removed `week_` from `Database.Beam.Postgres.PgSpecific`. The same
  functionality is now available in `beam-core` as a backend-agnostic
  `week_` extract field; import it from `Database.Beam.Query.Extract` (or
  re-exported through `Database.Beam`) instead.
* Replaced the single `BeamSqlBackendHasSerial Postgres` instance with three
  width-specific instances `BeamSqlBackendHasSerial Int16/Int32/Int64
  Postgres`, mapping respectively to `smallserial`, `serial`, and
  `bigserial`. Existing code using `genericSerial` for a `SqlSerial Int32`
  column continues to work; other widths are now also supported (#534).

## Added features

* Implemented the new `runInsertReturningListWith` /
  `runUpdateReturningListWith` / `runDeleteReturningListWith` class methods
  on the `Pg` monad. These let callers project a subset of columns from the
  affected rows of an `INSERT` / `UPDATE` / `DELETE ... RETURNING` (#801).
* Implemented `weekField` for `PgExtractFieldSyntax`, supporting the new
  backend-agnostic `week_` extract field from `beam-core`.

## Updated dependencies

* Bumped the lower bound on `beam-core` to `0.11`.

# 0.5.6.1

## Bug fixes

* Fixed a critical bug introduced by the performance improvements of 0.5.6.0, which would
  result in unexpected type errors when executing database queries.
  While the bug has been fixed, performance remains as good as the 0.5.6.0 release (#803)

# 0.5.6.0

## Added features

* Support for temporary tables.

* `getDbConstraintsForSchemas` now discovers foreign key constraints
  via `pg_constraint`, including `ON DELETE` / `ON UPDATE` actions.

## Performance improvements

* By minimizing redundant work in the hot loop, the performance of `beam-postgres`
  when fetching data from a database has been improved by 30%. The performance
  of `beam-postgres` is now within 10% of raw queries via `postgresql-simple` (#797).

# 0.5.5.0

## Added features

* Add support for creating secondary indices, supporting both `CREATE INDEX` and
  `CREATE UNIQUE INDEX`. `getDbConstraintsForSchemas` now discovers user-created
  secondary indices via `pg_index` (excluding primary keys and
  constraint-backing indices).

# 0.5.4.4

## Added features

* Added the array functions `arrayAppend_`, `arrayPrepend_`, `arrayRemove_`, `arrayReplace_`, `arrayShuffle_`, `arraySample_`, `arrayToString_`, and `arrayToStringWithNull_` (#770)

## Updated dependencies

* Updated the upper bound on `time` to include `time-1.14`

# 0.5.4.3

## Added features

 * Added `pgSelectWith`, a combinator like `selectWith` which allows to nest common table expressions in subqueries (#720).

## Bug fixes

 * Added the ability to migrate Postgres' array types (#354).
 * Remove dependency on `haskell-src-exts`, which was not in use anymore.

# 0.5.4.2

## Bug fixes

 * Fixed an issue where columns of type `Maybe (Vector a)` did not marshall correctly from the database. In particular, querying a `Nothing` would return `Just (Vector.fromList [])` instead (#692).

# 0.5.4.1

## Bug fixes

 * Fixed an issue where inexact numeric literals (e.g. Haskell type `Double`) were implicitly converted to Postgres `NUMERIC`, triggering a runtime conversion error (#700).

# 0.5.4.0

## Added features

 * Better error messages on column type mismatches (#696).
 * Added support for creating and dropping database schemas and associated tables with `createDatabaseSchema`, `dropDatabaseSchema`, and `createTableWithSchema` (#716).

## Documentation

 * Make `runBeamPostgres` and `runBeamPostgresDebug` easier to find (#663).

# 0.5.3.1

## Added features

 * Loosen some version bounds

# 0.5.3.0

## Bug fixes

 * Make sure lateral join names do not overlap
 * Fix `bool_or`

## Addded features

 * Add `runSelectReturningFirst`
 * `IN (SELECT ...)` syntax via `inQuery_`

# 0.5.2.1

## Added features

 * Aeson 2.0 support

# 0.5.2.0

## Added features

 * New `conduit` streaming variants which work directly in `MonadResource`
 * Heterogeneous variant of `ilike_`: `ilike_'`
 * Postgres-specific `EXTRACT` fields
 * GHC 9.2 and 9.0 support

## Bug fixes

 * Throw correct exception for row errors in `conduit` implementation
 * Support emitting UUID values in context where type cannot be inferred by Postgres

# 0.5.1.0

## Added features

 * `MonadBase` and `MonadBaseControl` instances for `Pg`

## Bug fixes

 * Fix possible memory corruption by copying row data
 * Remove invalid parentheses emitted by `pgUnnest`

# 0.5.0.0

## Interface changes

 * Removed instances for machine-dependent ambiguous integer types `Int` and `Word`
 * Fixed types for some functions that only work with `jsonb` and not `json`

## Added features

 * Support for `in_` on row values
 * Various Postgres regex functions
 * Expose `fromPgIntegral` and `fromPgScientificOrIntegral`
 * Add `liftIOWithHandle :: (Connection -> IO a) -> Pg a`
 * Add `getDbConstraintsForSchemas` to get constraints without relying on the state of the connection
 * Poly-kinded instances for `Data.Tagged.Tagged`
 * Add `HasDefaultDatatype` for `UTCTime`
 * Support for specifically-sized `SqlSerial` integers (`smallserial`, `serial`, `bigserial`)
 * Predicate detection for extensions
 * `pgArrayToJson` for `array_to_json`
 * Extension definition and all functions provided by `uuid-ossp`
 * GHC 8.8 support

## Bug fixes

 * Only detect primary keys of tables in visible schemas
 * Fix emitting of `DECIMAL` type
 * Report JSON correct decoding errors instead of throwing `UnexpectedNull`

## Behavior changes

 * `runReturningOne` and `runResturningList` now fetch all rows at once instead of using cursors

# 0.4.0.0

# 0.3.2.0

Add `Semigroup` instances to prepare for GHC 8.4 and Stackage nightly

# 0.3.1.0

Add `runBeamPostgres` and `runBeamPostgresDebug` functions.

# 0.3.0.0

Initial hackage beam-postgres
