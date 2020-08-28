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
!example chinookdml !on:MySQL
[newInvoice] <-
  BeamExtensions.runInsertReturningList $ insert (invoice chinookDb) $
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
    c <- all_ (customer chinookDb)
    pure (Playlist (customerId c + 1000) (just_ (concat_ [ customerFirstName c, "'s Playlist" ])))

playlists <- runSelectReturningList $ select $ limit_ 10 $
             orderBy_ (\p -> asc_ (playlistId p)) $
             filter_ (\p -> playlistId p >=. 1000) $
             all_ (playlist chinookDb)

putStrLn "Inserted playlists"
forM_ playlists $ \playlist ->
  putStrLn ("  - " ++ show playlist)
```

## Choosing a subset of columns

Above, we used the `default_` clause to set a column to a default
value. Unfortunately, not all backends support `default_` (SQLite being a
notable exception). Moreover, some `INSERT` forms simply can't use `default_`,
such as `insertFrom_` (you can't return `default_` from a query). The standard
SQL tool used in these cases is limiting the inserted data to specific
columns. For example, suppose we want to insert new invoices for every customer
with today's date. We can use the `insertOnly` function to project which field's
are being inserted.

!beam-query
```haskell
!example chinookdml

runInsert $
  insertOnly (invoice chinookDb)
             (\i -> ( invoiceCustomer i, invoiceDate i, invoiceBillingAddress i, invoiceTotal i ) ) $
  insertFrom $ do
    c <- all_ (customer chinookDb)

    -- We'll just charge each customer $10 to be mean!
    pure (primaryKey c, currentTimestamp_, customerAddress c, as_ @Scientific $ val_ 10)
```

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

## `ON CONFLICT`

Several backends (such as Postgres and SQLite) support `ON CONFLICT` subexpressions that specify
what action to take when an `INSERT` statement conlicts with already present data.

Beam support backend-agnostic `ON CONFLICT` statements via the `BeamHasInsertOnConflict`syntax. This
class contains a new function to generate an `SqlInsert`. The `insertOnConflict` function can be
used to attach `ON CONFLICT` actions to a `SqlInsert`.

```haskell
  insertOnConflict
    :: Beamable table
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues be (table (QExpr be s))
    -> SqlConflictTarget be table
    -> SqlConflictAction be table
    -> SqlInsert be table
```

The `SqlConflictTarget` specifies on which kinds of conflicts the action should run. You have a few options

* `anyConflict` - run the action on any conflict
* `conflictingFields` - run the action only when certain fields conflict
* `conflictingFieldsWhere` - run the action only when certain fields conflict and a particular
  expression evaluates to true.

The `SqlConflictAction` specifies what to do when a conflict happens.

* `onConflictDoNothing` - this cancels the insertion
* `onConflictUpdateSet` - sets fields to new values based on the current values
* `onConflictUpdateSetWhere` - sets fields to new values if a particular condition holds


### Acting on any conflict

A common use case of `ON CONFLICT` is to *upsert* rows into a database. *Upsertion* refers to only
inserting a row if another conflicting row does not already exist. For example, if you have a new
customer with primary key 42, and you don't know if it's in the database or not, but you want to
insert it if not, you can use the `insertOnConflict` function with the `anyConflict` target.

!beam-query
```haskell
!example chinookdml on:Sqlite on:Postgres
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  insertOnConflict (customer chinookDb) (insertValues [newCustomer])
     anyConflict
     onConflictDoNothing
```

### Acting only on certain conflicts

Sometimes you only want to perform an action if a certain constraint is violated. If the conflicting
index or constraint is on a field you can specify which fields with the function `conflictingFields`.

!beam-query
```haskell
!example chinookdml !on:MySQL
--! import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict(..))
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  insertOnConflict (customer chinookDb) (insertValues [newCustomer])
    (conflictingFields (\tbl -> primaryKey tbl))
    (onConflictUpdateSet (\fields oldValues -> fields <-. val_ newCustomer))
```

!!! tip "Tip"
    To specify a conflict on the primary keys, use `conflictingFields primaryKey`.

You can also specify how to change the record should it not match. For example, to append the e-mail
as an alternate when you insert an existing row, you can use the `oldValues` argument to get access
to the old value.

!beam-query
```haskell
!example chinookdml !on:MySQL
--! import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict(..))
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  insertOnConflict (customer chinookDb) (insertValues [newCustomer])
    (conflictingFields (\tbl -> primaryKey tbl))
    (onConflictUpdateSet (\fields oldValues -> customerEmail fields <-. concat_ [ customerEmail oldValues, ";", val_ (customerEmail newCustomer)]))
```

If you want to be even more particular and only do this transformation on rows corresponding to
customers from one state, use `conflictingFieldsWhere`.

!beam-query
```haskell
!example chinookdml !on:MySQL
--! import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict(..))
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  insertOnConflict (customer chinookDb) (insertValues [newCustomer])
    (conflictingFieldsWhere (\tbl -> primaryKey tbl) (\tbl -> addressState (customerAddress tbl) ==. val_ (Just "CA")))
    (onConflictUpdateSet (\fields oldValues -> customerEmail fields <-. concat_ [ customerEmail oldValues, ";", val_ (customerEmail newCustomer)]))
```
