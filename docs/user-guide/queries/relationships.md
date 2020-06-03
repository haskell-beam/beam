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

One more thing, before we explore how beam handles relationships. Before we do, let's define a quick utility function.

```haskell
Prelude Chinook.Schema> let withConnectionTutorial = runBeamSqliteDebug putStrLn chinook
```

This function prints each of our queries to standard output before running them.
Using this function will let us see what SQL is executing.

## Full inner joins

Recall that the `Q` type is a monad. In many respects, `Q` operates like the
list monad. For those unfamiliar, the monadic bind operator for `[]` is defined
as `concatMap`. Thus,

```haskell
do a <- [1,2,3]
   b <- [4,5,6]
   return (a, b)
```

is equivalent to `[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]`.

This operation is similar to the cartesian product from set theory or the *inner
join* from relational algebra. The `Q` monad fully supports this notion of join,
and in fact, every other join is built off of this primitive.

For example, to get every row from the invoice table and every row from the
invoice line table, with no attention paid to any relationship between the two:

!beam-query
```haskell
!example chinook
do i <- all_ (invoice chinookDb)
   ln <- all_ (invoiceLine chinookDb)
   pure (i, ln)
```

Of course, most of the time you only want to fetch relevant rows. Going back to
the list monad example, suppose we only want to fetch pairs where the second
number is less than or equal to twice the first. In Haskell, we'd use the
`guard` function.

```haskell
do a <- [1,2,3]
   b <- [4,5,6]
   guard (b <= a * 2)
   return (a, b)
```

This would return `[(2,4),(3,4),(3,5)]`.

Beam offers a similar function for the `Q` monad, named `guard_`. Note that
whereas the `Q` bind operator is the same as the Haskell monadic bind, the
corresponding `guard` function is not from `MonadZero`, as it is in Haskell. The
technical reason is that the argument to `guard_` in `Q` represents a SQL
expression returning a boolean, rather than a Haskell boolean itself.

Going back to our invoice line example above, we can fetch every invoice along
with only those invoice lines corresponding to that invoice.

!beam-query
```haskell
!example chinook
do i <- all_ (invoice chinookDb)
   ln <- all_ (invoiceLine chinookDb)
   guard_ (invoiceLineInvoice ln `references_` i)
   pure (i, ln)
```

Note that beam has floated the `guard_` expression into the `WHERE` clause,
rather than the `ON` clause, This is fine for most inner joins on most database
engines, as the query optimizer will execute both queries similarly. However,
some backends are more temperamental, so Beam offers several more idiomatic ways
to express joins which more closely reflect the underlying SQL. In practice,
most users will use the methods below to express JOINs, but it is nevertheless
important to understand that joining is fundamental to the structure of the `Q`
moand.

## One-to-many

Beam supports querying for one-to-many joins. For example, to get every
`InvoiceLine` for each `Invoice`, use the `oneToMany_` combinator.

!beam-query
```haskell
!example chinook
do i <- all_ (invoice chinookDb)
   ln <- oneToMany_ (invoiceLine chinookDb) invoiceLineInvoice i
   pure (i, ln)
```

Or, if you have an actual `Invoice` (called `oneInvoice`) and you want all the
associated `InvoiceLine`s, you can use `val_` to convert `oneInvoice` to the SQL
expression level.

```haskell
oneToMany_ (invoiceLine chinookDb) invoiceLineInvoice (val_ oneInvoice)
```

If you find yourself repeating yourself constantly, you can define a helper.

```haskell
invoiceLines_ :: OneToMany InvoiceT InvoiceLineT
invoiceLines_ = oneToMany_ (invoiceLine chinookDb) invoiceLineInvoice
```

Then the above queries become

```haskell
do i <- all_ (invoice chinookDb)
   ln <- invoiceLines_ i
```

and

```haskell
invoiceLines (val_ i)
```

Notice that, instead of floating the join condition to the `WHERE` clause, beam
generates an `INNER JOIN ... ON` expression. These statements are equivalent,
although the `ON` expression is more idiomatic.

### Nullable columns

If you have a nullable foreign key in your many table, you can use
`oneToManyOptional_` and `OneToManyOptional`, respectively. For example, 

### One-to-one

One to one relationships are a special case of one to many relationships, save
for a unique constraint on one column. Thus, there are no special constructs for
one-to-one relationships.

For convenience, `oneToOne_` and `OneToOne` are equivalent to `oneToMany_` and
`OneToMany`. Additionally, `oneToMaybe_` and `OneToMaybe` correspond to
`oneToManyOptional_` and `OneToManyOptional`.

## Many-to-many

Many to many relationships require a linking table, with foreign keys to each
table part of the relationship.

The `manyToMany_` construct can be used to fetch both, one, or no sides of a
many-to-many relationship.

```haskell
manyToMany_
  :: ( Database be db, Table joinThrough
     , Table left, Table right
     , Sql92SelectSanityCheck syntax
     , IsSql92SelectSyntax syntax

     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) )
  => DatabaseEntity be db (TableEntity joinThrough)
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s)) -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s), right (QExpr (Sql92SelectExpressionSyntax syntax) s))
```

This reads: for any database `db`; tables `joinThrough`, `left`, and `right`;
and sane select syntax `syntax`, where the primary keys of `left` and `right`
are comparable as value expressions and we have some way of extracting a primary
key of `left` and `right` from `joinThrough`, associate all entries of `left`
with those of `right` through `joinThrough` and return the results of `left` and
`right`.

The Chinook database associates multiple tracks with a playlist via the
`playlist_track` table. For example, to get all tracks from the playlists named
either "Movies" or "Music".

!beam-query
```haskell
!example chinook
manyToMany_ (playlistTrack chinookDb)
            playlistTrackPlaylistId playlistTrackTrackId

            (filter_ (\p -> playlistName p ==. just_ (val_ "Music") ||.
                            playlistName p ==. just_ (val_ "Movies"))
                     (all_ (playlist chinookDb)))

            (all_ (track chinookDb))
```

### Many-to-many with arbitrary data

Sometimes you want to have additional data for each relationship. For this, use
`manyToManyPassthrough_`.

```haskell
manyToManyPassthrough_
  :: ( Database be db, Table joinThrough
     , Table left, Table right
     , Sql92SelectSanityCheck syntax
     , IsSql92SelectSyntax syntax

     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) )
  => DatabaseEntity be db (TableEntity joinThrough)
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s ( joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s)
                   , left (QExpr (Sql92SelectExpressionSyntax syntax) s)
                   , right (QExpr (Sql92SelectExpressionSyntax syntax) s))
```

Under the hood `manyToMany_` is defined simply as

```haskell
manyToMany_ = fmap (\(_, left, right) -> (left, right)) manyToManyPassthrough_
```

### Declaring many-to-many relationships

Like one-to-many relationships, beam allows you to extract commonly used
many-to-many relationships, via the `ManyToMany` type.

For example, the playlist/track relationship above can be defined as follows

```haskell
playlistTrackRelationship :: ManyToMany ChinookDb PlaylistT TrackT
playlistTrackRelationship =
  manyToMany_ (playlistTrack chinookDb) playlistTrackPlaylistId playlistTrackTrackId
```

And we can use it as expected:

!beam-query
```haskell
!example chinook
playlistTrackRelationship
    (filter_ (\p -> playlistName p ==. just_ (val_ "Music") ||.
                    playlistName p ==. just_ (val_ "Movies"))
             (all_ (playlist chinookDb)))

    (all_ (track chinookDb))
```

`ManyToManyThrough` is the equivalent for `manyToManyThrough_`, except it takes
another table parameter for the 'through' table.

## Arbitrary Joins

Joins with arbitrary conditions can be specified using the `join_` construct.
For example, `oneToMany_` is implemented as

```haskell
oneToMany_ rel getKey tbl =
  join_ rel (\rel -> getKey rel ==. pk tbl)
```

Thus, the invoice example above could be rewritten. For example, instead of

```haskell
do i <- all_ (invoice chinookDb)
   ln <- oneToMany_ (invoiceLine chinookDb) invoiceLineInvoice i
   pure (i, ln)
```

We could write

!beam-query
```haskell
!example chinook
do i <- all_ (invoice chinookDb)
   ln <- join_ (invoiceLine chinookDb) (\line -> invoiceLineInvoice line ==. primaryKey i)
   pure (i, ln)
```

## Outer joins

### Left and right joins

Left joins with arbitrary conditions can be specified with the `leftJoin_`
construct. `leftJoin_` takes an arbitrary query and a join condition. It
associates each result record with a record of the table given or a fully NULL
row of that table in case no row matches. For this reason, the result of
`leftJoin_` has an extra `Nullable` column tag, which converts each field into
the corresponding `Maybe` type.

!!! note "Note"
    The table parameter passed in as the join condition does not have a
    `Nullable` column tag. The join condition should be written as if a
    concrete row from that table exists.

For example, to get every artist along with their albums, but always including
every artist, use `leftJoin_` as follows.

!beam-query
```haskell
!example chinook
do artist <- all_ (artist chinookDb)
   album  <- leftJoin_ (all_ (album chinookDb)) (\album -> albumArtist album ==. primaryKey artist)
   pure (artist, album)
```

Right joins are not yet supported. They can always be rewritten as left joins.
If you have a compelling use case, please file an issue!

### Handling SQL `NULL`s

`NULL` is a value that SQL treats as an 'unknown' value. Unfortunately, this can
cause a lot of unexpected issues. Beam tries to normalize the handling of NULLs
to some extent, but it ultimately cannot save you from the database. One thing
you can be sure of is that -- assuming your beam schema matches that of the
database -- any beam expression that does not yield a `Maybe` type cannot be
`NULL` at run-time.

Also, beam treats equality between `Maybe` types correctly using the standard
`==.` and `/=.` operators. This means that beam will sometimes generate obtuse
`CASE` expressions. This is because beam's philosophy is that SQL operators be
named after their equivalent Haskell ones, suffixed by a `.`, and that these
operators should follow Haskell semantics.

Sometimes though, this care isn't necessary. When you are okay with SQL
equality, you can use the `(==?.)` and `(/=?.)` operators. These work the same
as the `(==.)` and `(/=.)`, except they return a `SqlBool` instead of
`Bool`. `SqlBool` can only occur as the result of a SQL expression, and it
cannot be deserialized directly into Haskell on any backend. A `SqlBool` value
can contain `TRUE`, `FALSE`, and `UNKNOWN` (the third SQL boolean value). You
can marshal between `SqlBool` and `Bool` using `isTrue_`, `isFalse_`, or
`isUnknown_` to determine which value a `SqlBool` contains. The `unknownAs_`
function takes a default Haskell `Bool` and SQL expression returning
`SqlBool`. It returns the given Haskell `Bool` value in the case the SQL
expression is indeterminate.

You can also convert any expression returning `Bool` to one returning `SqlBool`
by using the `sqlBool_` function.

The various beam functions that deal with `Bool` also have corresponding
versions that operate on `SqlBool`. For example, whereas `leftJoin_` expects its
join condition to be a `Bool`, the corresponding `leftJoin_'` (notice the prime)
method takes a `SqlBool`. There are corresponding `guard_'`, `join_'`, etc
methods. Boolean operators, such as `(&&.)` and `(||.)`, have `SqlBool`
equivalents suffixed with `?` (`(&&?.)` and `(||?.)` for `SqlBool` `AND` and
`OR` respectively).

One place where this can really bite is when generating `ON` conditions. Many
RDBMSes use a rather unintelligent means of choosing which indices to use, by
directly matching on syntaxes. For example, postgres determines index usage by
directly seeing if two columns are compared. If you wrap the comparison in the
`IS TRUE` operator, the index is no longer used. In these cases, using the
proper boolean handling can severely impact performance. For example, to get
every customer along with employees in their area, we can left join the customer
table with employees on their city.

!beam-query
```haskell
!example chinook
do c <- all_ (customer chinookDb)
   e <- leftJoin_ (all_ (employee chinookDb)) (\e -> addressCity (employeeAddress e) ==. addressCity (customerAddress c))
   pure (c, e)
```

Notice that the join condition is not just a simple `=`. This will cause
postgres to ignore any index on these columns. We can instead use `leftJoin_'`
and `==?.` to be more direct.

!beam-query
```haskell
!example chinook
do c <- all_ (customer chinookDb)
   e <- leftJoin_' (all_ (employee chinookDb)) (\e -> addressCity (employeeAddress e) ==?. addressCity (customerAddress c))
   pure (c, e)
```

Now postgres will use an index.

### Full Outer joins

Outer joins are supported with the `outerJoin_` function. `outerJoin_` takes two
queries and a join condition and returns a `Q` that represents the `FULL OUTER
JOIN` of the two queries. Because either table may be nullable, the output of
the result has an additional `Nullable` tag.

!!! note "NOTE"
    Outer joins are only supported in backends whose SQL `FROM` syntax
    implements `IsSql92FromOuterJoinSyntax`. Notably, this does not include
    SQLite.

For example, to get join all employees with customers with the same first name
but including all employees and customers, we can run the query

!beam-query
```haskell
!example chinook outer-join
outerJoin_ (all_ (employee chinookDb)) (all_ (customer chinookDb)) (\(employee, customer) -> employeeFirstName employee ==. customerFirstName customer)
```

## Subqueries

Sometimes you want to join against a *subquery* rather than a table. For the
most part, beam will automatically figure out when certain queries need to be
written using subqueries. For example, to join two result sets cointaining a SQL
LIMIT, you would normally have to write both queries as subqueries. In beam, you
can write such queries as you'd expect. The library takes care of creating
subqueries as expected.

For example, the following query generates the code you'd expect.

!beam-query
```haskell
!example chinook
do i <- limit_ 10 $ all_ (invoice chinookDb)
   line <- invoiceLines i
   pure (i, line)
```

If you need to (for efficiency for example), you can also generate subqueries
explicitly, using `subselect_`. The `subselect_` will force a new query to be
output in most cases. For simple queries, such as `all_`, `subselect_` will have
no effect.

!beam-query
```haskell
!example chinook
-- Same as above, but with explicit sub select
do i <- subselect_ $ limit_ 10 $ all_ (invoice chinookDb)
   line <- invoiceLines i
   pure (i, line)
```
