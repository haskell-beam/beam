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
