# Unreleased

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
