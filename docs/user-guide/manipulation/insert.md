SQL `INSERT` expressions allow you to insert rows in the database.

There is a lot of variety in how you can provide new data, and Beam supports all
standard ways.

The `insert` function from `Database.Beam.Query` can be used to insert rows into
a particular table. `insert` takes a table and a source of values, represented
by `SqlInsertValues`, and returns a `SqlInsert` object that can be run in a
`MonadBeam` with `runInsert`.

The `SqlInsertValues` type takes two type parameters. The first is the
underlying database syntax, and the second is the shape of the data it carries,
specified as a beam table type. For example, a source of values in Postgres that
can be inserted in the Chinook customers table would have the type
`SqlInsertValues PgInsertValuesSyntax CustomerT`. This abstracts over where
those values actually are. The values may be explicit haskell values,
expressions returning customers, a query returning customers, or something
else. Either way, they can all be used in the same way with the `insert`
function.

## Inserting explicit new values

If you have a record of explicit Haskell values, use the `insertValues`
function. For example, to insert a new playlist into our chinook database

!beam-query
```haskell
!example chinookdml
runInsert $ insert (playlist chinookDb) $
  insertValues [ Playlist 700 (Just "My New Playlist")
               , Playlist 701 (Just "Another Playlist")
               , Playlist 702 (Just "Look... more playlists") ]

insertedPlaylists <-
  runSelectReturningList $
  select $ filter_ (\p -> playlistId p >=. 700) $
  all_ (playlist chinookDb)

putStrLn "Inserted playlists:"
forM_ insertedPlaylists $ \p ->
  putStrLn (show p)
```

## Inserting calculated values

Inserting explicit values is all well and good, but sometimes we want to defer
some processing to the database. For example, perhaps we want to create a new
invoice and use the current time as the invoice date. We could grab the current
time using `getCurrentTime` and then use this to construct an explicit Haskell
value, but this may cause synchronization issues for our application. To do
this, beam allows us to specify arbitrary expressions as a source of values
using the `insertExpressions` function.

!beam-query
```haskell
!example chinookdml
runInsert $ insert (invoice chinookDb) $
  insertExpressions [ Invoice (val_ 800) (CustomerId (val_ 1)) currentTimestamp_
                              (val_ (Address (Just "123 My Street") (Just "Buenos Noches") (Just "Rio") (Just "Mozambique") (Just "ABCDEF")))
                              (val_ 1000) ]

Just newInvoice <-
  runSelectReturningOne $
  lookup_ (invoice chinookDb) (InvoiceId 800)

putStrLn ("Inserted invoice: " ++ show newInvoice)
```

`insertExpressions` is strictly more general than `insertValues`. We can turn
any `insertValues` to an `insertExpressions` by running every table value
through the `val_` function to convert a Haskell literal to an expression.

For example, we can write the playlist example above as

!beam-query
```haskell
!example chinookdml
runInsert $ insert (playlist chinookDb) $
  insertExpressions [ val_ $ Playlist 700 (Just "My New Playlist")
                    , val_ $ Playlist 701 (Just "Another Playlist")
                    , val_ $ Playlist 702 (Just "Look... more playlists") ]

insertedPlaylists <-
  runSelectReturningList $
  select $ filter_ (\p -> playlistId p >=. 700) $
  all_ (playlist chinookDb)

putStrLn "Inserted playlists:"
forM_ insertedPlaylists $ \p ->
  putStrLn (show p)
```

One common use of `insertExpressions_` is when adding new rows to tables where
one field needs to be set to the default value. For example, auto-incrementing
keys or random UUIDs are a common way to assign primary keys to rows. You can
use `insertExpressions_` using the `default_` expression for each column that
you want to use the default value for.

For example, the query below adds a new invoice asking the database to assign a
new id.

!beam-query
```haskell
!example chinookdml !on:Sqlite
runInsert $ insert (invoice chinookDb) $
  insertExpressions [ Invoice default_ -- Ask the database to give us a default id
                              (val_ (CustomerId 1)) currentTimestamp_
                              (val_ (Address (Just "123 My Street") (Just "Buenos Noches") (Just "Rio") (Just "Mozambique") (Just "ABCDEF")))
                              (val_ 1000) ]
```

!!! warning "Warning"
    SQLite is a great little backend, but it doesn't support some standard SQL
    features, like the `DEFAULT` keyword in inserts. You can retrieve the same
    functionality by only inserting into a subset of columns. See the section on
    [that](#choosing-a-subset-of-columns) below.

### Retrieving the rows inserted

However, now we have no way of knowing *what* value the database
assigned. Unfortunately, there is no database-agnostic solution to this
problem. However, it's a common enough use case that beam provides a
backend-agnostic way for some backends. Backends that provide this functionality
provide an instance of `MonadBeamInsertReturning`. In order to use this class,
you'll need to explicitly import
`Database.Beam.Backend.SQL.BeamExtensions`. Below, we've imported this module
qualified.

!beam-query
```haskell
!example chinookdml
[newInvoice] <-
  BeamExtensions.runInsertReturningList (invoice chinookDb) $
  insertExpressions [ Invoice default_ -- Ask the database to give us a default id
                              (val_ (CustomerId 1)) currentTimestamp_
                              (val_ (Address (Just "123 My Street") (Just "Buenos Noches") (Just "Rio") (Just "Mozambique") (Just "ABCDEF")))
                              (val_ 1000) ]

putStrLn ("We inserted a new invoice, and the result was " ++ show newInvoice)
```

The pattern match on the single `newInvoice` is safe, even though its
partial. In general, you can expect the same amount of rows returned as
specified in your `SqlInsertValues`. If you know what this is statically, then
you can feel free to pattern match directly. Otherwise (if you used
`insertFrom`, for example), you'll need to handle the possibility that nothing
was inserted.

!!! tip "Tip"
    Note that unlike the standard beam `INSERT` functionality, which can run any
    `SqlInsert`, `runInsertReturningList` requires that we supply a table and a
    `SqlInsertValues`.

    This is because this functionality is emulated on some backends. Some
    backends (such as postgres) provide a more advanced `INSERT .. RETURNING`
    statement that can be used more like `SqlInsert`. See the backend
    documentation for more details.

!!! note "Note"
    Although SQLite has no support for the `DEFAULT` clause,
    `MonadBeamInsertReturning` in `beam-sqlite` inserts rows one at a time and
    will detect usage of the `DEFAULT` keyword. The beam authors consider this
    okay. While most beam statements are guaranteed to translate directly to the
    underlying DBMS system, `runInsertReturningList` is explicitly marked as
    emulated functionality.

## Inserting from the result of a `SELECT` statement

Sometimes you want to use existing data to insert values. For example, perhaps
we want to give every customer their own playlist, titled "<name>'s playlist".

We can use the `insertFrom` function to make a `SqlInsertValues` corresponding
to the result of a query. Make sure to return a projection with the same 'shape'
as your data. If not, you'll get a compile time error.

For example, to create the playlists as above

!beam-query
```haskell
!example chinookdml

runInsert $ insert (playlist chinookDb) $
  insertFrom $ do
    customer <- all_ (customer chinookDb)
    pure (Playlist (customerId customer + 1000) (just_ (concat_ [ customerFirstName customer, "'s Playlist" ])))

playlists <- runSelectReturningList $ select $ limit_ 10 $
             orderBy_ (\p -> asc_ (playlistId p)) $
             filter_ (\p -> playlistId p >=. 1000) $
             all_ (playlist chinookDb)

putStrLn "Inserted playlists"
forM_ playlists $ \playlist ->
  putStrLn ("  - " ++ show playlist)
```

## Choosing a subset of columns

## Inserting nothing

Oftentimes, the values to be inserted are generated automatically by some
Haskell function, and you just insert the resulting list. Sometimes, these lists
may be empty. If you blindly translated this into SQL, you'd end up with
`INSERT`s with empty `VALUE` clauses, which are illegal. Beam actually handles
this gracefully. If a `SqlInsertValues` has no rows to insert, the `SqlInsert`
returned by `insert` will know that it is empty. Running this `SqlInsert`
results in nothing being sent to the database, which you can verify below.

!beam-query
```haskell
!example chinookdml

let superComplicatedAction = pure [] -- Hopefully, you're more creative!

valuesToInsert <- superComplicatedAction

putStrLn "The following runInsert will send no commands to the database"
runInsert $ insert (playlist chinookDb) $
  insertValues valuesToInsert
putStrLn "See! I told you!"
```