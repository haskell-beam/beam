We've seen how to create simple queries from our schema. Beam supports other
clauses in the SQL SELECT statement.

For these examples, we're going to use the `beam-sqlite` backend with the
provided sample Chinook database. The Chinook database schema is modeled after a
fictional record store. It provides several tables containing information on the
music as well as the billing operations. Thus, it provides a good 'real-world'
demonstration of beam's capabalities.

First, create a SQLite database from the included example.

```console
$ sqlite3 chinook.db < beam-sqlite/examples/chinook.sql
```

Now, load the chinook database schema in GHCi.

```haskell
Prelude Database.Beam.Sqlite> :load beam-sqlite/examples/Chinook/Schema.hs
Prelude Chinook.Schema> chinook <- open "chinook.db"
```

One more thing, before we see more complex examples, let's define a quick
utility function.

```haskell
Prelude Chinook.Schema> let withConnectionTutorial = runBeamSqliteDebug putStrLn chinook
```

Let's test it!

We can run all our queries like:

```haskell
withConnectionTutorial $ runSelectReturningList $ select $ <query>
```

Let's select all the tracks.

```haskell
withConnectionTutorial $ runSelectReturningList $ select $ all_ (track chinookDb)
```

For the rest of the guide, we will also show the generated SQL code for both
sqlite and postgres.

!beam-query
```haskell
!example chinook
all_ (track chinookDb)
```

## Returning a subset of columns

Oftentimes we only care about the value of a few columns, rather than every
column in the table. Beam fully supports taking projections of tables. As said
before, `Q` is a `Monad`. Thus, we can use monadic `do` notation to only select a
certain subset of columns. For example, to fetch *only* the name of every track:

!beam-query
```haskell
!example chinook
do tracks <- all_ (track chinookDb)
   pure (trackName tracks)
```

Notice that beam has properly written the `SELECT` projection to only include
the `Name` field.

We can also return multiple fields, by returning a tuple. Perhaps we would also
like to know the composer:

!beam-query
```haskell
!example chinook
do tracks <- all_ (track chinookDb)
   pure (trackName tracks, trackComposer tracks)
```

You can also return arbitrary expressions in the projection. For example to
return the name, composer, unit price, and length in seconds (where the database stores it in milliseconds):

!beam-query
```haskell
!example chinook
do tracks <- all_ (track chinookDb)
   pure (trackName tracks, trackComposer tracks, trackMilliseconds tracks `div_` 1000)
```

Beam includes instances to support returning up to 6-tuples. To return more,
feel free to nest tuples. As an example, we can write the above query as

!beam-query
```haskell
!example chinook
do tracks <- all_ (track chinookDb)
   pure ((trackName tracks, trackComposer tracks), trackMilliseconds tracks `div_` 1000)
```

Notice that the nesting of tuples does not affect the generated SQL projection.
The tuple structure is only used when reading back the row from the database.

The `Q` monad is perfectly rule-abiding, which means it also implements a valid
`Functor` instance. Thus the above could more easily be written.

!beam-query
```haskell
!example chinook
fmap (\tracks -> (trackName tracks, trackComposer tracks, trackMilliseconds tracks `div_` 1000)) $
all_ (track chinookDb)
```

## `WHERE` clause

We've seen how to use `all_` to select all rows of a table. Sometimes, you would
like to filter results based on the result of some condition. For example,
perhaps you would like to fetch all customers whose names start with "Jo". We
can filter over results using the `filter_` function.

!beam-query
```haskell
!example chinook
filter_ (\customer -> customerFirstName customer `like_` "Jo%") $
all_ (customer chinookDb)
```

You can use `(&&.)` and `(||.)` to combine boolean expressions, as you'd expect.
For example, to select all customers whose first name begins with "Jo", last
name begins with "S", and who live in either California or Washington:

!beam-query
```haskell
!example chinook
filter_ (\customer -> ((customerFirstName customer `like_` "Jo%") &&. (customerLastName customer `like_` "S%")) &&.
                      (addressState (customerAddress customer) ==. just_ "CA" ||. addressState (customerAddress customer) ==. just_ "WA")) $
        all_ (customer chinookDb)
```

!!! note "Note"
    We had to use the `just_` function above to compare
    `addressState (customerAddress customer)`. This is because `addressState
    (customerAddress customer)` represents a nullable column which beam types as
    `Maybe Text`. Just as in Haskell, we need to explicitly unwrap the `Maybe`
    type. This is an example of beam offering stronger typing than SQL itself.

## `LIMIT`/`OFFSET` support

The `limit_` and `offset_` functions can be used to truncate the result set at a
certain length and fetch different portions of the result. They correspond to
the `LIMIT` and `OFFSET` SQL constructs.

!beam-query
```haskell
!example chinook
limit_ 10 $ offset_ 100 $
filter_ (\customer -> ((customerFirstName customer `like_` "Jo%") &&. (customerLastName customer `like_` "S%")) &&.
                      (addressState (customerAddress customer) ==. just_ "CA" ||. addressState (customerAddress customer) ==. just_ "WA")) $
        all_ (customer chinookDb)
```

!!! note "Note"
    Nested `limit_`s and `offset_`s compose in the way you'd expect without
    generating extraneous subqueries.

!!! warning "Warning"
    Note that the order of the `limit_` and `offset_` functions matter.
    Offseting an already limited result is not the same as limiting an offseted
    result. For example, if you offset three rows into a limited set of five
    results, you will get at most two rows. On the other hand, if you offset
    three rows and then limit the result to the next five, you may get up to
    five. Beam will generate exactly the query you specify. Notice the
    difference below, where the order of the clauses made beam generate a query
    that returns no results.

!beam-query
```haskell
!example chinook
offset_ 100 $ limit_ 10 $
filter_ (\customer -> ((customerFirstName customer `like_` "Jo%") &&. (customerLastName customer `like_` "S%")) &&.
                      (addressState (customerAddress customer) ==. just_ "CA" ||. addressState (customerAddress customer) ==. just_ "WA")) $
        all_ (customer chinookDb)
```

Backends often differ as to how they implement LIMIT/OFFSET. For example, SQLite
requires that `LIMIT` always be given if an `OFFSET` is provided. Beam correctly
handles this behavior.

!beam-query
```haskell
!example chinook
offset_ 100 $
filter_ (\customer -> ((customerFirstName customer `like_` "Jo%") &&. (customerLastName customer `like_` "S%")) &&.
                      (addressState (customerAddress customer) ==. just_ "CA" ||. addressState (customerAddress customer) ==. just_ "WA")) $
        all_ (customer chinookDb)
```

Notice that the SQLite query output has provided a dummy `LIMIT -1` clause,
while the Postgres query has not.

## `DISTINCT` support

SQL can only return unique results from a query through the `SELECT DISTINCT`
statement. Beam supports this using the `nub_` command. For example, to get all
the unique postal codes where our customers live.

!beam-query
```haskell
!example chinook
nub_ $ fmap (addressPostalCode . customerAddress) $
  all_ (customer chinookDb)
```

## `VALUES` support

Sometimes you want to select from an explicit group of values. This is
most helpful if you want to join against a set of values that isn't in
the database.

For example, to get all customers we know to be in New York, California, and Texas.

!beam-query
```haskell
!example chinook !on:Sqlite !on:MySQL
do c <- all_ (customer chinookDb)
   st <- values_ [ "NY", "CA", "TX" ]
   guard_' (just_ st ==?. addressState (customerAddress c))
   pure c
```

!!! note "Note"
    `beam-sqlite` does not support `VALUES` clauses anywhere within a
    query, but only within a common table expression.

## Ad-hoc queries

Sometimes you want to quickly query a database without having to write
out all the boilerplate. Beam supports this with a feature called
'ad-hoc' queries.

For example, to get all the names of customers, without having to use
`chinookDb` at all.

To use this functionality, import `Database.Beam.Query.Adhoc`. Note
that we use `TypeApplications` to give each field an explicit type. If
you don't, GHC will often infer the type, but it's nice to be explicit.

!beam-query
```haskell
!example chinook-adhoc
-- import qualified Database.Beam.Query.Adhoc as Adhoc
do ( cId, firstName, lastName )
     <- Adhoc.table_ Nothing {- Schema Name -}
          "Customer"
          ( Adhoc.field_ @Int32 "CustomerId"
          , Adhoc.field_ @Text "FirstName"
          , Adhoc.field_ @Text "LastName" )
   guard_ (firstName `like_` "Jo%")
   return (cId, firstName, lastName)
```
