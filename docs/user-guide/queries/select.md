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
Prelude Chinook.Schema> let withConnectionTutorial = withDatabaseDebug putStrLn chinook
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
!chinook sqlite3
!chinookpg postgres
all_ (track chinookDb)
```

## `WHERE` clause

We've seen how to use `all_` to select all rows of a table. Sometimes, you would
like to filter results based on the result of some condition. For example,
perhaps you would like to fetch all customers whose names start with "Jo". We
can filter over results using the `filter_` function.

!beam-query
```haskell
!chinook sqlite3
!chinookpg postgres
filter_ (\customer -> customerFirstName customer `like_` "Jo%") $
all_ (customer chinookDb)
```

You can use `(&&.)` and `(||.)` to combine boolean expressions, as you'd expect.
For example, to select all customers whose first name begins with "Jo", last
name begins with "S", and who live in either California or Washington:

!beam-query
```haskell
!chinook sqlite3
!chinookpg postgres
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

