# Beam: a type-safe, non-TH Haskell relational database library and ORM

[![Build status](https://github.com/haskell-beam/beam/workflows/Build/badge.svg)](https://github.com/haskell-beam/beam/workflows/Build/badge.svg)

If you use beam commercially, please consider a donation to make this project possible: https://liberapay.com/tathougies

Beam is a Haskell interface to relational databases. Beam uses the Haskell type
system to verify that queries are type-safe before sending them to the database
server. Queries are written in a straightforward, natural monadic syntax.
Combinators are provided for all standard SQL92 features, and a significant
subset of SQL99, SQL2003, and SQL2008 features. For your convenience a thorough
compatibility matrix is
maintained [here](https://haskell-beam.github.io/beam/about/compatibility/).

Beam is standards compliant but not naive. We recognize that different database
backends provide different guarantees, syntaxes, and advantages. To reflect
this, beam maintains a modular design. While the core package provides standard
functionality, beam is split up into a variety of *backends* which provide a
means to interface Beam's data query and update DSLs with particular RDBMS
backends. Backends can be written and maintained independently of this
repository. For example,
the [beam-mysql](https://github.com/tathougies/beam-mysql)
and [beam-firebird](https://github.com/gibranrosa/beam-firebird) backends are
packaged independently.

Recognizing that over-abstraction frequently means caving in to the
lowest common denominator, Beam does not do connection or transaction
management. Rather, the user is free to perform these functions using the
appropriate Haskell interface library for their backend of choice. Additionally,
beam backends provide a significant portion of backend-specific functionality
which seamlessly fits into the beam ecosystem.

For example, the `beam-postgres` backend is built off of the `postgresql-simple`
interface library. When using `beam-postgres`, the user manages connections and
transactions with `postgresql-simple`. The user is free to issue queries
directly with `postgresql-simple`, only using beam when desired. Postgres offers
a number of rich data types on top of the standard SQL data types. To reflect
this, `beam-postgres` offers pluggable support for postgres-specific data types
and features.

For more information, see the [user guide](https://haskell-beam.github.io/beam).

For questions, feel free to join
our [mailing list](https://groups.google.com/forum/#!forum/beam-discussion) or
head over to `#haskell-beam` on freenode.

## A word on testing

`beam-core` has in-depth unit tests to test query generation over an idealized
ANSI SQL-compliant backend. You may be concerned that there are no tests in
either `beam-sqlite` or `beam-postgres`. Do not be alarmed. The documentation
contains many, many examples of queries written over the sample Chinook
database, the schema for which can be found at
`beam-sqlite/examples/Chinook/Schema.hs`. The included `mkdocs` configuration
and custom `beam_query` python Markdown extension automatically run every query
in the documentation against a live database connection. Any errors in
serializion/deserialization or invalid syntax are caught while building the
documentation. Feel free to open pull-requests with additional examples/tests.

Tests are written

~~~markdown
!beam-query
```haskell
!example <template-name> <requirements>
do x <- all_ (customer chinookDb) -- chinookDb available under chinook and chinookdml examples
   pure x
```
~~~

The `!beam-query` declaration indicates this is markdown code block that
contains beam query code. The `!example` declaration indicates that this example
should be built against applicable backends and included in the code. The
`template_name` is either `chinook` or `chinookdml` (depending on whether you
have quest a query or a DML statement). For `chinook`, the included code should
produce a `Q` query. For `chinookdml`, the included code should be a monadic
action in a `MonadBeam`. The `requirements` can be used to select which backends
to run this against. See the documentation for examples.

## Building the documentation

Beam uses `mkdocs` for its documentation generation. The included
`build-docs.sh` script can take care of building the documentation and serving
it locally. In order to use the tool though, make sure you have a python
installation with the `mkdocs` module installed. You can do this by creating a
virtualenv, and pip installing `mkdocs`, or by using the supplied `shell.nix`
script.

The documentation uses a custom Markdown preprocessor to automatically build
examples against the canonical Chinook database. By default, beam will build
examples for *every* beam backend it knows about, including ones not in the main
source tree (see `docs/beam.yaml` for the full configuration). This means you
will need to have an instance of all these database servers running and
available. This is usually not what you want.

To only build examples for a particular backend, modify `mkdocs.yaml` and set
the `enabled_backends` configuration setting for the `docs.markdown.beam_query`
preprocessor. For example, to only build docs for `beam-sqlite`, change

```yaml
  - docs.markdown.beam_query:
      template_dir: 'docs/beam-templates'
      cache_dir: 'docs/.beam-query-cache'
      conf: 'docs/beam.yaml'
      base_dir: '.'
```

to

```yaml
  - docs.markdown.beam_query:
      template_dir: 'docs/beam-templates'
      cache_dir: 'docs/.beam-query-cache'
      conf: 'docs/beam.yaml'
      base_dir: '.'
      enabled_backends:
        - beam-sqlite
```
