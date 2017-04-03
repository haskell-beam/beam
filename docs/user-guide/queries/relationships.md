Relational databases are so-named because they're good at expressing relations
among data and providing related data in queries. Beam exposes these features in
its DSL.

For these examples, we're going to use the `beam-sqlite` backend with the
provided sample Chinook database.

First, create a SQLite database from the included example.

```
> sqlite3 chinook.db < beam-sqlite/examples/chinook.sql
```

Now, load the chinook database schema in GHCi.

```haskell
Prelude Database.Beam.Sqlite> :load beam-sqlite/examples/Chinook/Schema.hs
Prelude Chinook.Schema> chinook <- open "chinook.db"
```

One more thing, before we explore how beam handles relationships. In the
following example we will be using the `withConnectionTutorial` function. This
is defined as

```haskell
withConnectionTutorial = withConnectionDebug putStrLn
```

This function prints each of our queries to standard output before running them.
Using this function will let us see what SQL is executing.

## Joins



### Joining via a primary key

## Semi-joins

### Left and right joins

### Outer joins

## Subqueries



