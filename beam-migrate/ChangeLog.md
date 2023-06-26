# 0.5.2.1

# Added features

 * Loosen some version bounds

# 0.5.2.0

# Addded features

 * `IN (SELECT ...)` syntax via `inSelectE`

# 0.5.1.2

## Added features

 * Aeson 2.0 support

# 0.5.1.1

## Added features

 * GHC 9.2 and 9.0 support

# 0.5.1.0

## Added features

 * Expose `IsNotNull` class

## Bug fixes

 * Order log entries when verifying migration status

# 0.5.0.0

## Interface changes

 * Removed instances for machine-dependent ambiguous integer types `Int` and `Word`
 * Require `MonadFail` for `BeamMigrationBackend`

## Added features

 * GHC 8.8 support
 * `checkSchema`: Like `verifySchema`, but detects and returns unexpected
   predicates found in the live database

## Bug fixes

 * Map `Int16` to `smallIntType` instead of `intType`
 * Suppress creation of primary key constraints for tables with no primary keys

## 0.4.0.0

## 0.3.2.0

Added `haskellSchema` shortcut

## 0.3.1.0

Add `Semigroup` instances to prepare for GHC 8.4 and Stackage nightly

## 0.3.0.0

* Re-introduce backend parameter as `Database` type class
* Move beam migration log schema to beam-migrate, since many
  applications will want to easily manage a database using the
  haskell-based migrations
* Add `bringUpToDate` and `bringUpToDateWithHooks` to
  `Database.Beam.Migrate.Simple`, which can be used to bring a
  database up to date with the given migration.

## 0.2.0.0  -- 2018-01-20

* First version. Released on an unsuspecting world.
