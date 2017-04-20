SQLite is a lightweight RDBMS meant for embedding in larger applications.
Because it is not designed to be full-featured, not all Beam queries will work
with SQLite. The module `Database.Beam.SQLite.Checked` provides many symbols
usually imported from the `Database.Beam` module that enforce extra checks on
queries to assure compliance with SQLite. Use this module in code that is SQLite
specific for maximal compile-time safety. Note that this module should be
imported instead of `Database.Beam` to avoid name clashes.

## Compatibility

SQLite is compatible enough with Beam's query syntax, that adapting to its
quirks is pretty straightforwards. The main special case for SQLite is its
handling of nested set operations. On most backends, beam can output these
directly, but SQLite requires us to generate subqueries.
