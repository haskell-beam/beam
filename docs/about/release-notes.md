# Beam Release Notes

## 0.7.0.0

### Correct boolean handling

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

### Aggregations return `Maybe` types

In previous versions of beam, aggregations such as `avg_`, `sum_`, etc
returned the an expression of the same type as its inputs. However,
this does not match standard SQL behavior, where these aggregates can
return NULL if no rows are selected for the aggregation. This breaks
older code, but is more correct. To restore the older behavior, use
the `fromMaybe_` function to supply a default value.

### Miscellaneous name changes

The `Database.Beam.Query.lookup` function was renamed to `lookup_` to
avoid overlap with the `Prelude` function of the same name.

### Reintroduce explicit backends to `Database` class

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


### Require backends to explicitly declare types that can be compared for equality

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

#### For Backends

Backend implementors should establish instances of
`HasSqlEqualityCheck` and `HasSqlQuantifiedEqualityCheck` for every
type that can be compared in their syntax. You may choose to implement
a custom equality and inequality operator. Alternatively, you can
leave the instances empty to use the defaults, which match the old
behavior.

### Properly deal with NULL values in equality

Previous versions of Beam would use SQL `=` and `<>` operators to
compare potentially `NULL` values. However, `NULL = NULL` is always
false, according to the SQL standard, so this behavior is incorrect.

Now, Beam will generate a `CASE .. WHEN ..` statement to explicitly
handle mismatching `NULL`s. This is the 'expected' behavior from the
Haskell perspective, but does not match what one may expect in
SQL. Note that it is always better to explicitly handle `NULL`s using
`maybe_`, and beam recommends this approach in robust code.

### Remove `Auto` for fields with default values

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

## 0.6.0.0

* Mostly complete SQL92, SQL99 support
* Piecemeal support for SQL2003 and SQL2008 features
* Completely modular backends
* Various bug improvements and fixes

## 0.5.0.0

* Move to using finally tagless style for SQL generation
* Split out backends from `beam-core`
* Allow non-table entities to be stored in databases
* Basic migrations support
