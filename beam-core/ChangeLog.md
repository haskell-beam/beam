# 0.9.0.0

## Removal of machine-dependent `Int`/`Word` instances

Beam now mandates that you use unambiguous integer types like `Int32`, `Int64`, or `Integer` instead of the machine-dependent `Int` or `Word`.
Custom type errors have been added to guide migration where required.

Combinators which previously returned `Int`, such as `countAll_` and `rowNumber_`, now match functions such as `count_` in returning any `Integral` type.
The type of these functions vary across databases and doesn't in general correspond to the `INTEGER` type.
(For example Postgres uses `bigint` for these.)

## `in_` on row values

Beam now supports using `in_` on row values, for backends which support it.
This fulfills the often requested ability to use `in_` on `PrimaryKey`s, e.g. ``primaryKey row `in_` [ ... ]``.

## Miscellaneous added features

 * Support for ad-hoc queries on tables which don't have a corresponding `Beamable` type
 * `HasInsertOnConflict` class for backends which support functionality similar to Postgres and SQLite's `INSERT ... ON CONFLICT`
 * Convenience functions `setEntitySchema` and `modifyEntitySchema`
 * Haskell-style conditionals `ifThenElse_` and `bool_`
 * Poly-kinded instances for `Data.Tagged.Tagged`
 * Variants of update functions which use tri-value `SqlBool`: `update'`, `save'`, `updateRow'`, `updateTableRow'`, and corresponding combinator `references'`
 * GHC 8.8 support

## Minor interface changes

 * Split `WithConstraint` apart, to support strict fields
 * `zipTables` supports `Applicative` actions instead of `Monad`

## Bug fixes

 * Database definition fields can be made strict
 * `decimalType` properly emits SQL 92 `DECIMAL` instead of `DOUBLE`

# 0.8.0.0

## Common table expressions

Beam now supports common table expressions on some backends, using the
`With` monad. Currently, only `SELECT` statements are supported.

## Changes to field name modification

`EntityModification` is now a `Monoid`.

Instead of taking the beam-determined name, the `renamingFields`
function instead takes a `Data.List.NonEmpty` value containing the
names of each Haskell record selector that led to this field.

For example, in the following

```haskell
data Embedded f =
  Embedded { _field1 :: Columnar f Text
           , _field2 :: Columnar f Int }

data Table1 f =
  Table1 { _tbl1FieldA :: Columnar f Text
         , _tbl1Embedded :: Embedded f }

db = defaultDbSettings `withDbModification`
     dbModification { table1 = renamingFields f }
```

`f` would be called with `["_tbl1FieldA"]`, `["_tbl1Embedded", "_field1"]`
and `["_tbl1Embedded", "_field2"]`.


## Simplified types

Every beam SQL backend is now an instance of `BeamSqlBackend` and has an
associated syntax via an associated type family. This means types are much simpler.

Another benefit is that `MonadBeam` now has a simpler type and can be used with
monad transformers. For example, writing a computation that may call out to a
postgres database is as simple as

```haskell
dbComputation :: MonadBeam Postgres m => m result
```

versus before

```haskell
dbComputation :: MonadBeam PgCommandSyntax Postgres Pg.Connection m => m result
```

Things become simpler if you want to write database agnostic computations. You can now do

```haskell
dbComputation :: (BeamSqlBackend be, MonadBeam be m) => m result
```

versus before

```haskell
dbComputation :: ( Sql92SanityCheck syntax, MonadBeam syntax be hdl m ) => m result
```

## Removal of `HasDefaultSqlDataTypeConstraints`

The changes above make a separate `HasDefaultSqlDataTypeConstraints`
class unnecessary. The `defaultSqlDataTypeConstraints` method is now
included within the `HasDefaultSqlDataType` class.

## Changes to parseOneField and peekField

Formerly, the `peekField` function would attempt to parse a field
without advancing the column pointer, regardless of whether a field
was successfully parsed. In order to support more efficient parsing,
this has been changed. When `peekField` returns a `Just` value, then
the column pointer is advanced to the next pointer. This means you do
not need to call `parseOneField` again to advance the pointer.

You can still chain `peekField` calls together by using the new
`Alternative` instance for `FromBackendRowM`.

# 0.7.2.0

Add compatibility with GHC 8.4 and stack nightly

# 0.7.1.0

Note '0.7.1.0' was released because the signature of `delete` was too specific
in '0.7.0.0' due to an error when uploading the package.

# 0.7.0.0

## Weaker functional dependencies on `MonadBeam`

The functional dependency on `MonadBeam` is now just `m -> syntax be
handle`. This allows us to define `MonadBeam` instances atop monad transformers
(although we don't have any yet!).

## Correct boolean handling

Previous versions of beam used the SQL `=` operator to compare potentially
`NULL` values. This is incorrect, as `NULL = NULL => UNKNOWN` in ANSI-compliant
implementations. Beam has changed its emitted SQL to produce proper comparisons,
but this can dramatically affect performance in some backends. Particularly,
proper JOIN index usage in Postgres requires an exact match on an equality
constructor, which may not be what you get when using the proper boolean
handling.

If you are okay using SQL null handling, you can use the new `==?.` and `/=?.`
operators which produce an expression with type `SqlBool` instead. `SqlBool` is
a type that can represent the SQL `BOOL` type in all its gritty glory. Note
however, that these operators do not compare for haskell equality, only SQL
equality, so please understand what that means before using them.

Correspondingly, many functions that took `Bool` expressions now have
corresponding versions that take `SqlBool`. For example, to use `guard_` with a
`SqlBool` expression use `guard_'` (note the prime).

(Note: I don't really like that we have to do this, but this is the only way
unless we introspect user expressions. Beam's philosophy is to be as direct as
possible. The `==.` operator corresponds to haskell `==`, and so produces the
boolean we would expect as Haskell programmers. The `==?.` operator is a new
operator that users must explicitly opt in to. Both produce the most direct code
possible on each backend.)

## Aggregations return `Maybe` types

In previous versions of beam, aggregations such as `avg_`, `sum_`, etc
returned the an expression of the same type as its inputs. However,
this does not match standard SQL behavior, where these aggregates can
return NULL if no rows are selected for the aggregation. This breaks
older code, but is more correct. To restore the older behavior, use
the `fromMaybe_` function to supply a default value.

## Miscellaneous name changes

The `Database.Beam.Query.lookup` function was renamed to `lookup_` to
avoid overlap with the `Prelude` function of the same name.

## Reintroduce explicit backends to `Database` class

Some database entites only work on particular backends. For example,
beam-postgres extension support only works in beam-postgres. The lack
of a backend parameter on the `Database` type class essentially
mandated that every database entity worked on every backend. By
introducing a backend parameter to `Database`, we allow the user to
restrict which backends a database can work on.

The old behavior is still easily recovered. Whereas before you'd write

```haskell
instance Database MyDatabase
```

Now write

```haskell
instance Database be MyDatabase
```

## Require backends to explicitly declare types that can be compared for equality

Beam previously allowed any two types to be compared for SQL
equality. This is no longer the case. Rather, only types that are
instances of `HasSqlEqualityCheck` for the given expression syntax can
be checked for equality. Correspondingly, only types that are
instances of `HasSqlQuantifiedEqualityCheck` can be checked for
quantified equality.

This change is somewhat invasive, as the relationship and join
operators depend on the ability to check primary keys for
equality. You may have to add appropriate class constraints to your
queries. In order to assert that a table can be compared for equality,
you can use the `HasTableEquality` constraint synonym.

### For Backends

Backend implementors should establish instances of
`HasSqlEqualityCheck` and `HasSqlQuantifiedEqualityCheck` for every
type that can be compared in their syntax. You may choose to implement
a custom equality and inequality operator. Alternatively, you can
leave the instances empty to use the defaults, which match the old
behavior.

## Properly deal with NULL values in equality

Previous versions of Beam would use SQL `=` and `<>` operators to
compare potentially `NULL` values. However, `NULL = NULL` is always
false, according to the SQL standard, so this behavior is incorrect.

Now, Beam will generate a `CASE .. WHEN ..` statement to explicitly
handle mismatching `NULL`s. This is the 'expected' behavior from the
Haskell perspective, but does not match what one may expect in
SQL. Note that it is always better to explicitly handle `NULL`s using
`maybe_`, and beam recommends this approach in robust code.

## Remove `Auto` for fields with default values

`Auto` was a convenience type for dealing with tables where some
columns have been given a default value. `Auto` worked well enough but
it was a very leaky abstraction. Moreover, it was
unnecessary. Everything you can do with `Auto` can be done more safely
with `default_`.

For example, instead of using

```haskell
insertValues [ Table1 (Auto Nothing) "Field Value" "Another Field Value" ]
```

use

```haskell
insertExpressions [ Table1 default_ (val_ "Field Value") (val_ "Another Field Value") ]
```

# 0.6.0.0

* Mostly complete SQL92, SQL99 support
* Piecemeal support for SQL2003 and SQL2008 features
* Completely modular backends
* Various bug improvements and fixes

# 0.5.0.0

* Move to using finally tagless style for SQL generation
* Split out backends from `beam-core`
* Allow non-table entities to be stored in databases
* Basic migrations support

