# Beam: a type-safe, non-TH Haskell relational database library and ORM

![Build status](https://api.travis-ci.org/tathougies/beam.svg?branch=master)

Beam is a Haskell interface to relational databases. Beam uses the Haskell type
system to verify that queries are type-safe before sending them to the database
server. Queries are written in a straightforward, natural monadic syntax.
Combinators are provided for all standard SQL92 features, and a significant
subset of SQL99, SQL2003, and SQL2008 features. For your convenience a thorough
compatibility matrix is
maintained [here](http://tathougies.github.io/beam/about/compatibility/).

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

For more information, see the [user guide](https://tathougies.github.io/beam).

For questions, feel free to join
our [mailing list](https://groups.google.com/forum/#!forum/beam-discussion) or
head over to `#haskell-beam` on freenode.
