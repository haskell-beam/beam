# `beam-sqlite`: Beam backend for the SQLite embedded database

`beam-sqlite` is a beam backend for
the [SQLite embedded database](https://sqlite.org/). 

SQLite is mostly standards compliant, but there are a few cases that beam-sqlite
cannot handle. These cases may result in run-time errors. For more information,
see
[the documentation](https://haskell-beam.github.io/beam/user-guide/backends/beam-sqlite/).
Due to SQLite's embedded nature, there are currently no plans to get rid of
these. However, proposals and PRs to fix these corner cases are welcome, where
appropriate.

## Migration support

`beam-sqlite` supports schema introspection and verification via
`Database.Beam.Sqlite.Migrate`, including:

- Tables, columns, and their types,
- `NOT NULL` constraints,
- Primary keys,
- User-created secondary indices (via `PRAGMA index_list` / `PRAGMA index_info`),
- Foreign key constraints (via `PRAGMA foreign_key_list`), including
  `ON DELETE` / `ON UPDATE` actions.

### Declaring foreign keys

Use `addTableForeignKey` from `beam-migrate` to declare a foreign key on a
checked table entity:

```haskell
let db = defaultMigratableDbSettings `withDbModification`
          (dbModification @_ @Sqlite)
            { _orders =
                addTableForeignKey (_customers db)
                  (\t -> selectorColumnName _order_customer_id t NE.:| [])
                  primaryKeyColumns
                  ForeignKeyNoAction      -- ON UPDATE NO ACTION (default)
                  ForeignKeyActionCascade -- ON DELETE CASCADE
            }
```

> **Note:** SQLite disables foreign key enforcement by default. Issue
> `PRAGMA foreign_keys = ON` on each connection (or set it in your open hook)
> to enable it at runtime.
