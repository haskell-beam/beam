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
