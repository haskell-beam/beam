# 0.5.5.0

## Added features

* `runInsertReturningList` now uses SQLite's relatively new `RETURNING` clause.

## Bux fixes

* Fixed an issue where values inserted with conflicts did not return then when using `runInsertReturningList` (#774) 

## Updated dependencies

* Updated the lower bound of `direct-sqlite` to `2.3.27`.
* Updated the upper bound on `time` to include `time-1.14`.

# 0.5.4.1

## Dependencies

* Ensure that beam-sqlite uses sqlite-3.24+, which is the minimum supported version (#589).

# 0.5.4.0

## Added features

 * Removed the reliance on either the `unix` or `windows` package, which should enable (#738)
   `beam-sqlite` to be buildable on a wider variety of platforms.

# 0.5.3.1

## Added features

 * Replaced use of deprecated functions.

# 0.5.3.0

## Added features

 * Loosen some version bounds
 * `HasSqlEqualityCheck` instance for `Day`

# 0.5.2.0

## Bug fixes

 * Fix encoding for `UTCTime`

## Addded features

 * `IN (SELECT ...)` syntax via `inQuery_`

# 0.5.1.2

## Added features

 * Aeson 2.0 support

# 0.5.1.1

## Added features

 * GHC 9.2 and 9.0 support

## Bug fixes

 * Support inserting default values for all columns (except with upsert)

# 0.5.1.0

## Added features

 * `MonadBase` and `MonadBaseControl` instances for `SqliteM`

# 0.5.0.0

## Interface changes

 * Removed instances for machine-dependent ambiguous integer types `Int` and `Word`

## Added features

 * Support for `in_` on row values
 * Upsert support using `HasInsertOnConflict`
 * Fix build on Android and OpenBSD

## Bug fixes

 * Fix emitting and detection of `DECIMAL` and `DOUBLE PRECISION` types
 * Fix `bitLength`, `charLength_`, and `octectLength_` by emulating with `CAST` and `LENGTH`
 * Fix `runInsertReturningList` for when the database column order and beam column order disagree.

# 0.4.0.0

# 0.3.2.0

Add `Semigroup` instances to prepare for GHC 8.4 and Stackage nightly

# 0.3.1.0

Add `runBeamSqlite` and `runBeamSqliteDebug` functions

# 0.3.0.0

* Re-introduce backend parameter to `Database` class

# 0.2.0.0

First split of sqlite backend from main beam package
