<img src="img/logo.svg" width="250px"/>

Beam is a highly-general library for accessing any kind of database with
Haskell. Currently, it supports two SQL backends, Postgres and SQLite. Work is
underway for an official MySQL backend (contributions appreciated and
accepted!).

Beam is highly extensible and other backends can be shipped independently
without requiring any changes in the core libraries. For example, there is an
independently maintained [Firebird](https://github.com/gibranrosa/beam-firebird)
backend. For information on creating additional SQL backends, see
the [manual section](user-guide/custom-backends.md) for more.

Beam features

* __Easy schema generation__ from existing databases
* __A basic migration infrastructure__ for working with multiple versions of your
  database schema.
* __Support for most SQL92, SQL99, and SQL2003 features__ across backends that
  support them, including aggregations, subqueries, and window functions.
* __A straightforward Haskell-friendly query syntax__. You can use Beam's `Q`
  monad much like you would interact with the `[]` monad.
* __No Template Haskell__ Beam uses the GHC Haskell type system and nothing else.
  The types have been designed to be easily-inferrable by the compiler, and
  appropriate functions to refine types have been provided for the where the
  compiler may need more help.

## How to install

Beam is available via both [Hackage](https://hackage.haskell.org/packages/beam)
and [Stack](https://www.stackage.org/package/beam). For most Haskell
installations, one of the two commands should work.

```bash
cabal install beam-core beam-migrate <backend>
```

or

```
stack install beam-core beam-migrate <backend>
```

You will also need to install an appropriate backend. Available backends are

* `beam-postgres` -- A feature-complete backend for the Postgres RDBMS.
  See [the beam-postgres documentation](user-guide/backends/beam-postgres.md)
  for more information.
  
* `beam-sqlite` -- An almost feature-complete backend for the Sqlite library.
  Note that SQLite does not support all SQL92 features, so some of the examples
  may not work. Refer
  to [the beam-sqlite documentation](user-guide/backends/beam-sqlite.md) for
  more information on compatibility.
  
## Quick Start Guide

For those looking to get started with beam, we first recommend you go through
the [tutorial](tutorials/tutorial1.md). The [user guide](user-guide/models.md)
contains much more detailed reference-like information. Finally, the
documentation on hackage is always available (although the types may seem
obtuse).

If you're interested if beam supports your favorite database feature, refer to
the documentation for your backend or take a look at
the [compatibility matrix](about/compatibility.md).


<!-- If you already have a database schema, you can use the `beam-migrate` command to -->
<!-- automatically generate appropriate Beam data definitions. -->

<!-- ```bash -->
<!-- beam-migrate --backend <backend-module> <backend-options> new <output-module-name> -->
<!-- ``` -->

<!-- For example, to generate a schema for the Postgres database `employees` at -->
<!-- `localhost:5000` with the user `beam`, run the following command. -->

<!-- ```bash -->
<!-- beam-migrate --backend Database.Beam.Postgres.Migrate --pgconnect postgres://beam@localhost:5000/employees new BeamTutorial.Schema -->
<!-- ``` -->

<!-- This will generate a -->

## How to Contribute

We always welcome contributions, especially to cover more database features or
to add support for a new backend. Help is available on the `beam-users` Google
Group. The following is a quick step-by-step guide of contributing a new feature:

1. Fork the github repository at `https://github.com/tathougies/beam`
   and clone the fork to a local directory.
2. Work on your feature on your own branch, or pick
   an [issue](https://github.com/tathougies/beam/issues).
3. When you feel ready to contribute the feature back to `beam-core`, send a
   Pull Request on Github, with an explanation of what your patch does and
   whether it breaks the API.
4. Respond to community comments and rework your patch.
5. When the maintainer feels comfortable with your patch, he will commit it to
   the `master` branch and it will be included in the next minor version.
   API-breaking changes will not be included until the next major version.
   
!!! tip "Tip"
    Be sure to add your name to
    the
    [`CONTRIBUTORS`](https://github.com/tathougies/beam/blob/master/CONTRIBUTORS) file
    for eternal fame and glory!

## Questions, Feedback, Discussion

* For frequently asked questions, see the [FAQ](about/faq.md).
* For general questions, feedback on patches, support, or other concerns, please
  write to the mailing list
* For bugs or feature requests,
  please [open an issue](https://github.com/tathougies/beam/issues)

## Why Beam?

Beam is the most feature-complete, turnkey Haskell database solution out there.
It supports the entire breadth of the SQL92, SQL99, SQL2003, SQL2006, SQL2008,
SQL2011, and SQL2016 specifications, as well as the entire breadth of features
of each of its backends. See the [compatibility matrix](about/compatibility.md).
You will rarely be forced to write a SQL query 'by hand' when using Beam
(but [you can](user-guide/extensibility.md)).

Additionally, Beam plays nice with the rest of the Haskell ecosystem, the
standard Beam backends are all implemented in terms of the underlying
`<db>-simple` packages. Beam provides only minimum support for querying data
across multiple databases. It is assumed that you have chosen you RDBMS with
much care, and we want to support you in that. Beam's main purpose is to marshal
data back and forth, to serve as the source of truth for the DB schema, and to
generate properly formed SQL from Haskell expressions.
