Beam is backend-agnostic and doesn't provide any means to connect to a
database. Beam backend libraries usually use well-used Haskell
libraries to provide database connectivity. For example, the
`beam-sqlite` backend uses the `sqlite-simple` backend.

Beam distinguishes each backend via type indexes. Each backend defines
a type that is used to enable backend-specific behavior. For example,
the `beam-sqlite` backend ships with the `Sqlite` type that is used to
distinguish sqlite specific constructs with generic or other
backend-specific ones.

Each backend can have one or more 'syntaxes', which are particular
ways to query the database. While the `beam-core` library ships with a
standard ANSI SQL builder, few real-world database implementations
fully follow the standard. Most backends use their own custom syntax
type. Internally, beam uses a finally-tagless representation for
syntax trees that allow straightforward construction against any
backend.

Beam offers backend-generic functions for the most common operations
against databases. These functions are meant to fit the lowest common
denominator. For example, no control is offered over streaming results
from SELECT statements. While these backend-generic functions are
useful for ad-hoc querying and development, it is wisest to use
backend-specific functions in production for maximum control. Refer to
backend-specific documentation for more information.

For our examples, we will use the `beam-sqlite` backend and demonstrate
usage of the beam standard query functions.

## Connecting to a database

Okay, so we can print out a SQL statement, but how do we execute it against a
database? Beam provides a convenient `MonadBeam` type class that allows us to
write queries in a backend agnostic manner. This is good-enough for most
applications and preserves portability across databases. However, `MonadBeam`
does not support features specific to each backend, nor does it guarantee the
highest-performance. Most backends provide additional methods to query a
database, and you should prefer these if you've committed to a particular
backend. For tutorial purposes, we will use the `beam-sqlite` backend.

First, install `beam-sqlite` with `cabal` or `stack`:

```
$ cabal install beam-sqlite
# or
$ stack install beam-sqlite
```

Now, load `beam-sqlite` in GHCi. 

```
Prelude> import Database.Beam.Sqlite
Prelude Database.Beam.Sqlite> 
```

Now, in another terminal, load the example database provided. 

```
$ sqlite3 basics.db < beam-sqlite/examples/basics.sql
```

Now, back in GHCi, we can create a connection to this database.

```
Prelude Database.Beam.Sqlite> basics <- open "basics.db"
Prelude Database.Beam.Sqlite> withDatabase basics $ runSelectReturningList (select (all_ (persons exampleDb)))
[ .. ]
```

The `runSelectReturningList` function takes a `SqlSelect` for the given syntax
and returns the results via a list.

Voilà! We've successfully created our first query and run it against an example
database. We have now seen the major functionalities of the beam library. In the
next section we'll explore more advanced querying and using relationships
between tables.


## Inserting data

First, let's connect to a sqlite database, and create our schema. The
`beam-core` does not offer any support for the SQL DDL language. There
is a separate core library `beam-migrate` that offers complete support
for ANSI-standard SQL DDL operations, as well as tools to manipulate
database schemas. See the section on migrations for more information.

For our example, we will simply issue a `CREATE TABLE` command
directly against the database using `sqlite-simple` functionality:

```text
Prelude Schema> execute_ conn "CREATE TABLE persons ( first_name TEXT NOT NULL, last_name TEXT NOT NULL, age INT NOT NULL, PRIMARY KEY(first_name, last_name) )"
```

Now we can insert some data into our database. Beam ships with a
function `withDatabase`, with the following signature:

```haskell
withDatabase :: MonadBeam syntax be hdl m => hdl -> m a -> IO a
```

`MonadBeam` is a type class that relates a particular SQL syntax (`syntax`) to a
backend (`be`), command monad (`m`), and database handle (`hdl`) type. Inside
the `m` monad, we can execute data query and manipulation commands. The `hdl`
type is usually the type of the connection in the underlying backend library.

For example, `beam-sqlite` uses the `sqlite-simple` library, so its handle type
is `Connection` from `Database.SQLite.Simple`.

Let's insert some data into our database. We are going to use the `runInsert`
function from `MonadBeam`. INSERTs are discussed in more detail in
the [data manipulation guide](manipulation/insert.md).

```text
Prelude Schema> :{
Prelude Schema| withDatabase conn $ do
Prelude Schema|   runInsert $ insert (persons exampleDb) $
Prelude Schema|               insertValues [ Person "Bob" "Smith" 50
Prelude Schema|                            , Person "Alice" "Wong" 55
Prelude Schema|                            , Person "John" "Quincy" 30 ]
Prelude Schema| :}
```

The `runInsert` function has the type signature

```haskell
runInsert :: MonadBeam syntax be hdl m => SqlInsert syntax -> m ()
```

`SqlInsert syntax` represents a SQL `INSERT` command in the given
`syntax`. We construct this value using the `insert` function from
`Database.Beam.Query`.

```haskell
insert :: IsSql92InsertSyntax syntax =>
          DatabaseEntity be db (TableEntity table)
       -> Sql92InsertValuesSyntax syntax
       -> SqlInsert syntax
```

Intuitively, `insert` takes a database table descriptor and some
values (particular to the given syntax) and returns a statement to
insert these values. `Sql92InsertValuesSyntax syntax` always
implements the `IsSql92InsertValuesSyntax` typeclass, which is where
we get the `insertValues` function from. `IsSql92InsertValuesSyntax`
also defines the `insertSelect` function for inserting values from the
result of a `SELECT` statement. Other backends may provide other ways
of specifying the source of values.

Now, we can query the database, using the `runSelect` function. Like `runInsert`
and `insert`, we use the `select` function to construct a value of type
`SqlSelect syntax`, which can be run inside `MonadBeam`.

We can use the `withDatabaseDebug` function to install a hook that beam will
call with every SQL command it is about to run. In the following example, beam
will print its query to stdout via `putStrLn`. You can use this functionality to hook beam in to a logging framework.

```text
Prelude Schema> withDatabaseDebug putStrLn conn $ runSelect (select (all_ (persons exampleDb)))
[ Person { personFirstName = "Bob", personLastName="Smith", personAge=50 }, ... ]
```
