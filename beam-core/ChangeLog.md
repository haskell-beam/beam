# 0.7.0.0

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
>>>>>>> master

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

