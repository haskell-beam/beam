Given our database definition and database descriptor, we can query database
entities and retrieve data. Before we discuss writing queries, we will take a
look at some of the important query types.

## Data types

### The `Q` data type

Beam queries are built using the `Q` data type. `Q`'s signature is as follows

```haskell
data Q syntax db s a
```

In this definition

* `syntax` is the particular dialect of SQL this `Q` monad will evaluate to.
  Often times, this is any instance of `IsSql92SelectSyntax`, but sometimes you
  use syntax-specific features. For example, if you want to use named windows in
  postgres, you'll likely have to specialize this to `PgSelectSyntax` from
  `Database.Beam.Postgres.Syntax`.
  
* `db` is the type of the database (as we defined above). This is used to ensure
  you only query database entities that are in scope in this database.
  
* `s` is the scope parameter. For the most part, you'll write your queries so
  that they work over all `s`. Beam manipulates this parameter internally to
  ensure that the fields in your expressions are always in scope at run-time.
  
* `a` is the type of the result of the query.

### The `QGenExpr` type

While `Q` represents the result of whole queries (entire `SELECT`s for example),
`QGenExpr` represents the type of SQL expressions. `QGenExpr` also takes some
type parameters:

```haskell
data QGenExpr context syntax s a
```

* `context` is the particular way in which this expression is being used. For
  example, expressions containing aggregates have `context ~ QAggregateContext`.
  Expressions returning scalar values have `context ~ QValueContext`.
  
* `syntax` is the particular SQL dialect this expression is written in. Note
  that this is usually different than the `syntax` for `Q`, because `Q`'s syntax
  refers to a particular syntax for `SELECT` expressions (a type implementing
  `IsSql92SelectSyntax`), while `QGenExpr`'s syntax usually refers to an
  expression syntax (a type implementing `IsSql92ExpressionSyntax`). Of course,
  since syntaxes are related, you can get from a `Q` `SELECT` syntax to a
  `QGenExpr` `syntax` with the `Sql92SelectExpressionSyntax` type family.
  
  Thus, a `QGenExpr` with syntax `Sql92SelectExpressionSyntax select` can be
  used in the `FILTER` clause of a query with type `Q select db s a`.

* `s` is a scoping parameter, which will match the `s` in `Q`.

* `a` is the type of this expression. For example, expressions returning SQL
  `int` values, will have Haskell type `Int`. This ensures that your SQL query
  won't fail at run-time with a type error.
  
Beam defines some specializations of `QGenExpr` for common uses.

```haskell
type QExpr = QGenExpr QValueContext
type QAgg = QGenExpr QAggregateContext
type QOrd = QGenExpr QOrderingContext
type QWindowExpr = QGenExpr QWindowingContext
type QWindowFrame = QGenExpr QWindowFrameContext
type QGroupExpr = QGenExpr QGroupingContext
```

Thus, value expressions can be given the simpler type of `QExpr syntax s a`.
Expressions containing aggregates are typed as `QAgg syntax s a`.
  
### A note on type inference

These types may seem incredibly complicated. Indeed, the safety that beam tries
to provide requires these scary-looking types.

But alas, do not fear! Beam is also designed to assist type inference. For the
most part, you will rarely need to annotate these types in your code.
Occassionally you will need to provide a type for the result of an expression.
For example, `SELECT`ing just the literal `1` may cause an ambiguity, because
the compiler won't know which `Integral` type to use. Beam provides an easy
utility function `as_` for this. With `-XTypeApplications` enabled,

```haskell
as_ @Int (ambiguous expression)
```

ensures that `ambiguous expression` has the type `QGenExpr ctxt syntax s Int`
with the `ctxt`, `syntax`, and `s` types appropriately inferred.

## Simple queries

The easiest query is simply getting all the rows in a specific table. If you
have a database object (something with type `DatabaseSettings be db`) with some
table or view entities, you can use the `all_` function to retrieve all rows in
a specific table or view.

For example, to retrieve all `PersonT` entries in the `exampleDb` we defined in
the last section, we can say 

```haskell
all_ (persons exampleDb) :: Q syntax ExampleDb s (PersonT (QExpr s))
```

!!! note "Note"
    We give the full type of the query here for illustrative purposes only. There 
    is no need to do so in your own code

Two things to note. Firstly, here `PersonT` is parameterized over the `QExpr s`
higher-kinded type. This means that each field in `PersonT` now contains a SQL
expression instead of a Haskell value. This is the magic that our parameterized
types allow.

Thus,

```haskell
personFirstName (all_ (persons exampleDb)) :: QExpr s Text
```

and

```haskell
personFirstName (Person "John" "Smith" 23 "john.smith@example.com" "8888888888" :: Person) :: Text
```

Secondly, the field type has the same scope variable as the entire query. This
means, it can only be used in the scope of this query. You will never be able to
inspect the type of `s` from outside `Q`.

Once we have a query in terms of `Q`, we can use the `select` function from
`Database.Beam.Query` to turn it into a select statement that can be run against
the backend. `select` takes an expression of type `Q`, and converts it into a
SQL statement, ready to be executed against the database.

The output of the query passed to `select` must follow some conventions, so that
beam knows how to serialize, deserialize, and project the appropriate values
from the query. In particular, the return type of your query must be either

* a plain expression (i.e., type `QExpr`),
* a `Beamable` type (i.e., a table or primary key, defined as above), or
* any combination of tuples of the above (Beam supports up to 8-tuples by
  default). Higher-order tuples can be formed by nested tuples. For example, for
  16 return values, you can return a 2-tuple of 8-tuples or an 8-tuple of
  2-tuples or a 4-tuple of 4-tuples, etc.
  
With this in mind, we can use `select` to get a query statement against our
database. The return type of `all_` is just the table we ask for. In this case,
we're interested in the `persons` table. The `persons` table has the `Beamable`
type `PersonT`. As expected, the `SqlSelect` will return us concrete `Person`
values (recall that `Person` is equivalent to `PersonT Identity`).

```haskell
select (all_ (persons exampleDb)) :: (...) => SqlSelect syntax Person
```

The `...` in the context represents a bunch of requirements for `syntax` that
GHC will generate.

Normally, you'd ship this select statement off to a backend to run, but for the
purposes of this tutorial, we can also ask beam to dump what the standard SQL
expression this query encodes.

```haskell
dumpSqlSelect (all_ (persons exampleDb))
SELECT "t0"."first_name" AS "res0", "t0"."last_name" AS "res1", "t0"."age" AS "res2", "t0"."email" AS "res3", "t0"."phone" AS "res4" FROM "person" AS "t0"
```

Internally, `dumpSqlSelect` uses a `beam-core` provided syntax to generate
standard ANSI SQL expressions. Note that these expressions should not be shipped
to a backend directly, as they may not be escaped properly. Still, it is useful
to see what would run.

!!! tip "Tip"
    `all_` only works for `TableEntity`s. Use `allFromView_` for `ViewEntity`s.

## A note on composability

All beam queries are *composable*. This means that you can freely mix values of
type `Q` in whichever way typechecks and expect a reasonable SQL query. This
differs from the behavior of SQL, where the syntax for composing queries depends
on the structure of that query.

For example, suppose you wanted to fetch all rows of a table, filter them by a
condition, limit the amount of rows returned and then join these rows with
another table. In SQL, you'd have to write explicit subselects, take care of
handling projections, etc. This is because this query doesn't fit into the
'standard' SQL query structure.

However, in beam, you can simply write this query. Beam will take care of
generating explicit subselects and handling projections. Scoping rules enforced
by the Haskell type system ensure that the query is constructed correctly.

For example, we can write the following (meaningless) query, and things will work as expected.

!beam-query
```haskell
!chinook sqlite3
!chinookpg postgres
do tbl1 <- 
     limit_ 10 $
     filter_ (\customer -> ((customerFirstName customer `like_` "Jo%") &&. (customerLastName customer `like_` "S%")) &&.
                           (addressState (customerAddress customer) ==. just_ "CA" ||. addressState (customerAddress customer) ==. just_ "WA")) $
             all_ (customer chinookDb)
   tbl2 <- all_ (track chinookDb)
   pure (tbl1, tbl2)
```

This allows you to easily factor out queries. This means you can build a query
library in your application and then freely mix and match these queries as
necessary. This allows you to offload as much processing to the database as
possible, rather than shipping data to your application pre-processing.

!beam-query
```haskell
!chinook sqlite3
!chinookpg postgres
-- 'complicatedQuery' could be declared and imported from an external module here. The generated query is the same regardless
let complicatedQuery = 
       filter_ (\customer -> ((customerFirstName customer `like_` "Jo%") &&. (customerLastName customer `like_` "S%")) &&.
                             (addressState (customerAddress customer) ==. just_ "CA" ||. addressState (customerAddress customer) ==. just_ "WA")) $
               all_ (customer chinookDb)
in do tbl1 <- limit_ 10 $ complicatedQuery
      tbl2 <- all_ (track chinookDb)
      pure (tbl1, tbl2)
```

