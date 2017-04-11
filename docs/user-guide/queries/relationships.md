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
Prelude Chinook.Schema> let withConnectionTutorial = withDatabaseDebug putStrLn chinook
```

This function prints each of our queries to standard output before running them.
Using this function will let us see what SQL is executing.

## One-to-many

Beam supports querying for one-to-many joins. For example, to get every
`InvoiceLine` for each `Invoice`, use the `oneToMany_` combinator.

```haskell
do i <- all_ (invoice chinookDb)
   ln <- oneToMany_ (invoiceLine chinookDb) invoiceLineInvoice i
   pure (i, ln)
```

Or, if you have an actual `Invoice` (called `oneInvoice`) and you want all the
associated `InvoiceLine`s, you can use `val_` to convert `oneInvoice` to the SQL
expression level.

```haskell
oneToMany_ (invoiceLine chinookDb) invoiceLineInvoice (val_ i)
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

### One-to-one

One to one relationships are a special case of one to many relationships, save
for a unique constraint on one column. Thus, there are no special constructs for
one-to-one relationships.

For convenience, `oneToOne_` and `OneToOne` are equivalent to `oneToMany_` and
`OneToMany`.

## Many-to-many

Many to many relationships require a linking table, with foreign keys to each
table part of the relationship.

The `manyToMany_` construct can be used to fetch both, one, or no sides of a
many-to-many relationship.

```haskell
manyToMany_ :: ( Database db, Table joinThrough
               , Table left, Table right
               , Sql92SelectSanityCheck syntax
               , IsSql92SelectSyntax syntax

               , SqlOrd (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left g)
               , SqlOrd (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right h) )
            => DatabaseEntity be db (TableEntity joinThrough)
            -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey left g)
            -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey right h)
            -> Q syntax db s (left g) -> Q syntax db s (right h)
            -> Q syntax db s (left g, right h)
```

This reads: for any database `db`; tables `joinThrough`, `left`, and `right`;
and sane select syntax `syntax`, where the primary keys of `left` and `right`
are comparable as value expressions and we have some way of extracting a primary
key of `left` and `right` from `joinThrough`, associate all entries of `left`
with those of `right` through `joinThrough` and return the results of `left` and
`right`.

For example, 

### Many-to-many with arbitrary data

Sometimes you want to have additional data for each relationship. For this, use
`manyToManyPassthrough_`.

```haskell
manyToManyPassthrough_ 
    :: ( Database db, Table joinThrough
       , Table left, Table right
       , Sql92SelectSanityCheck syntax
       , IsSql92SelectSyntax syntax

       , SqlOrd (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left g)
       , SqlOrd (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right h) )
    => DatabaseEntity be db (TableEntity joinThrough)
    -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey left g)
    -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey right h)
    -> Q syntax db s (left g) -> Q syntax db s (right h)
    -> Q syntax db s (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s), left g, right h)
```

Under the hood `manyToMany_` is defined simply as 

```haskell
manyToMany_ = fmap (\(_, left, right) -> (left, right)) manyToManyPassthrough_
```

## Arbitrary Joins

Joins with arbitrary conditions can be specified using the `join_` construct.

## Outer joins

### Left and right joins

Left joins with arbitrary conditions can be specified with the `leftJoin_` construct.

Right joins are supported (albeit awkwardly) with the `rightJoin_` construct.

### Outer joins

Full outer joins are supported via the `outerJoin_` construct.

## Subqueries

Sometimes you want to join against a *subquery* rather than a table. For the
most part, beam will automatically figure out when certain queries need to be
written using subqueries. For example, to join two result sets cointaining a SQL
LIMIT, you would normally have to write both queries as subqueries. In beam, you
can write such queries as you'd expect. The library takes care of creating
subqueries as expected.

For example,

```haskell
do i <- limit_ 10 $ all_ (invoice chinookDb)
   line <- limit_ 10 $ invoiceLInes_ i
   pure (i, line)
```

generates

```sql
-- TODO 
```

If you need to (for efficiency for example), you can also generate subqueries
explicitly, using `subselect_`.
