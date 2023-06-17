In the User Guide we saw how to declare a schema for an already created database
and use it to perform queries. Beam can also manage a database schema based on
Haskell datatypes you feed it.

The migrations framework is meant to be a robust and modular way of
managing schema changes. It is an optional part of beam provided in
the `beam-migrate` package.

Install the migrations framework by running.

```
$ cabal install beam-migrate
# or
$ stack install beam-migrate
```

## Basic concepts

In the user guide, we saw how we can use `defaultDbSettings` to generate default
metadata that can be used to access the database. This default metadata is
enough to query, but not enough for `beam-migrate`. `beam-migrate` offers the
`defaultMigratableDbSettings` function, which annotates the database schema with
additional information. Whereas `defaultDbSettings` yields a value of type
`DatabaseSettings be db`, `defaultMigratableDbSettings` yields a value of type
`CheckedDatabaseSettings be db`. You can recover a `DatabaseSettings be db` from
a `CheckedDatabaseSettings be db` value by applying the `unCheckDatabase`
function.

The `CheckedDatabaseSettings` value contains the original `DatabaseSettings`
along with a series of *predicates*. Each *predicate* describes one aspect of
the database. As far as `beam-migrate` is concerned, each database schema is
fully specified by the set of predicates that apply to it. `beam-migrate` calls
this the *checked type* of the database.

For example, a database schema that consists of one table named `table` with no
fields is represented uniquely by the *checked type* of `[TableExistsPredicate
"table"]`. If you add a field `field1` of type `INT` to the table, then the
checked type becomes `[TableExistsPredicate "table", TableHasColumn "table"
"field1" intType]`.

!!! note "Note"
    The types are a bit more complicated than what they appear. In particular, a
    predicate can be of any type that satisfies the `DatabasePredicate` type
    class. The predicates can be stored in a
    ([heterogenous](https://wiki.haskell.org/Heterogenous_collections#Existential_types))
    list because they are wrapped in the `SomeDatabasePredicate` GADT that holds
    the type class instance as well.

### Automatic migration generation

Given two `CheckedDatabaseSettings` values, `beam-migrate` can generate a set of
SQL steps that will transform one schema to another. The generation of such
steps is an exceedingly difficult problem in general. `beam-migrate` can
automatically handle most common cases, but it will not always succeed. In this
case, it can present to you a list of steps it thinks are best as well as what
remains to be solved.

The migration generation is implemented as a proof search in linear logic[^1]. In
particular, `beam-migrate` views a migration as a linear logic proof of the form
`a ⊸ b`, where `a` is the set of predicates of the original schema and `b` is
the set of predicates in the target schema. `beam-migrate` ships with a set of
default proof steps. Backends can add to these steps for backend-specific
predicates.

!!! note "Note"
    At this time, Haskell does not allow the expression of linear programs (this
    will change with the introduction of linear types). Thus, migrations written
    in Haskell are not checked by GHC for linear-ness, but `beam-migrate` will
    validate such migrations at run-time to the best of its ability.

The migration prover may not be able to find a migration in a sufficiently short
period of time. `beam-migrate`'s algorithm is designed to terminate, but this
may take a while. Additionally, the prover will not automatically generate steps
for some migrations. For example, `beam-migrate` will never rename a table
without explicit instructions.

### Advantages of checked migrations

Unlike other database migration frameworks, the checking process allows
`beam-migrate` to be sure that the migration you specify will result in the
database type you want. Also, checked migrations allow the programmer to verify
that the database they are accessing indeed matches what their schema expects.

[^1]:
    Linear logic is a type of logic first described by Jean-Yves Gerard. In
    particular, it constrains the weakening and strengthening rules of classical
    logic. Intuitively, you can think of it as forcing the rule that each
    assumption is used exactly once to produce the result. Read
    more [on Wikipedia](https://en.wikipedia.org/wiki/Linear_logic).
