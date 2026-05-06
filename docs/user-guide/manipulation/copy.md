SQL `COPY` is a non-standard, but widely supported, statement for moving rows
between a database table and some external resource.

Beam exposes the `COPY` statement within `Database.Beam.Backend.SQL.BeamExtensions`:

* `MonadBeamCopyTo be m` — backends that can execute `COPY ... TO 'file'`,
  exporting rows to a server-side file.
* `MonadBeamCopyFrom be m` — backends that can execute
  `COPY ... FROM 'file'`, importing rows from a server-side file.

`beam-duckdb` and `beam-postgres` currently provide instances.

## Building a COPY statement

In general, there are four ways to copy data OUT of the database (`COPY ... TO`):

* `copyTableTo`: Copy an entire table, or a subset of its columns, to an external file;
* `copySelectTo`: Copy the result of a `SELECT` query to an external file;
* `copyTableToStream`: Stream an entire table, or a subset of its columns;
* `copySelectToStream`: Stream the result of a `SELECT` query.

To copy data INTO the database:

* `copyTableFrom`: Copy data from an external file, to a table (or a subset of its columns).
* `copyTableFromStream`: Copy data from an incoming stream, to a table (or a subset of its columns).

The type signature of these functions may look gnarly. This is because input parameters are
highly backend-dependent. For example, DuckDB can export data in/out of Parquet files, while
Postgres generally cannot. Each backend supplies its own set of smart constructors and options records:

* **PostgreSQL** — `text` and `csv` formats. See
  [the beam-postgres page](../backends/beam-postgres.md#copy-support) for
  format options and the role-permission requirement.
* **DuckDB** — `CSV`, `Parquet`, and `JSON` formats. See
  [the beam-duckdb page](../backends/beam-duckdb.md#copy-support) for format
  coverage, compression, and partition-by support.

### Direct table copy to a file

For example, exporting every Chinook playlist to a CSV in DuckDB:

!beam-query
```haskell
!example chinookdml only:DuckDB
--! import Database.Beam.Backend.SQL.BeamExtensions
runCopyTo $
  copyTableTo
    (playlist chinookDb)
    id -- no projection: export entire table
    (DuckDB.copyToCSV "/tmp/beam-docs-playlists.csv")
```

The same example written for PostgreSQL is identical except for the backend
qualifier:

!beam-query
```haskell
!example chinookdml only:Postgres
--! import Database.Beam.Backend.SQL.BeamExtensions
runCopyTo $
  copyTableTo
    (playlist chinookDb)
    id -- no projection: export entire table
    (Pg.copyToCSV "/tmp/beam-docs-playlists.csv")
```

### Copying a query to a file

Here's an example of exporting the result of a query where playlists are filtered by name before
exporting:

!beam-query
```haskell
!example chinookdml only:DuckDB
--! import Database.Beam.Backend.SQL.BeamExtensions
runCopyTo $
  copySelectTo
    ( select $ do
        p <- all_ (playlist chinookDb)
        guard_ (playlistName p ==. just_ (val_ "Music"))
        pure p
    )
    (DuckDB.copyToCSV "/tmp/beam-docs-music-playlist.csv")
```

### Importing rows from a file

The example below first exports the playlists to a CSV and then re-imports
the same file via `copyTableFrom`:

!beam-query
```haskell
!example chinookdml only:DuckDB
--! import Database.Beam.Backend.SQL.BeamExtensions

-- Export rows to a CSV.
runCopyTo $
  copyTableTo
    (playlist chinookDb)
    id
    (DuckDB.copyToCSV "/tmp/beam-docs-playlists-roundtrip.csv")

-- Clear the playlist table (and its dependents) so the re-import doesn't
-- conflict on primary keys. The example runs inside a rolled-back
-- transaction, so the chinook database stays unchanged.
runDelete $ delete (playlistTrack chinookDb) (\_ -> val_ True)
runDelete $ delete (playlist chinookDb) (\_ -> val_ True)

-- Re-import the rows we just wrote out.
runCopyFrom $
  copyTableFrom
    (playlist chinookDb)
    id
    (DuckDB.copyFromCSV "/tmp/beam-docs-playlists-roundtrip.csv")
```

### Streaming COPY

The `COPY` runners discussed above all read from / write to a file on the
**database server**. This makes sense for an in-process database like DuckDB, but
not so much for a database server like PostgreSQL.

`beam-postgres` currently provides instances; see
[the beam-postgres page](../backends/beam-postgres.md#streaming-copy) for
the Postgres-specific smart constructors and a worked example.
`beam-duckdb` does not yet implement the streaming classes.
