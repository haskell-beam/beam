# `beam-migrate`: Migrations and SQL DDL support for Beam

This package provides type classes to allow backends to implement SQL DDL
support for beam. This allows you to use beam syntax to write type-safe schema
generation code.

The package also provides features to introspect beam schemas, and support for
automatic generation of migrations in SQL and Haskell formats.

This is mostly a low-level support library. Most often, this library is used to
write tooling to support DDL manipulation in your project, or to enable
migrations support in beam backends.
