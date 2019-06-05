## How does `beam` compare with `<x>`?

### `opaleye`

`opaleye` has similar aims as beam. However, `beam`'s DSL is monadic,
and eschews the use of arrows. We use a phantom scope parameter to
achieve the same result (it's the `s` parameter in the `Q` and `QExpr`
types). This allows you to write queries in the same way you'd use the
list monad. Some people think this is more intuitive than arrows.

Beam also uses higher-kinded types to allow the use of 'normal'
haskell data types, rather than a fully polymorphic type. For example,
in opaleye you may have to write

```haskell
data Table column1 column2 column3 =
  Table { tblColumn1 :: column1
        , tblColumn2 :: column2
        , tblColumn3 :: column3
        }
```

This can get tiring when you have dozens of columns. In beam, types
need only take one polymorphic parameter.

```haskell
data Table f =
  Table { tblColumn1 :: C f Column1Type
        , tblColumn2 :: C f Column2Type
        , tblColumn3 :: C f Column3Type
        } deriving (Generic, Beamable)
```

Moreover, all beam instances and type synonyms are easily written by
hand. There is no Template Haskell magic here. What you see is what
you get.

Beam is also fully polymorphic over the backend. That is to say
that a beam query can be written once and used across multiple
backends, so long as those backends support the features used in the
query. Feature constraints are written as class constraints. For
example, if you write a query that uses the SQL standard `regr_slope`
function, you can make that query polymorphic over a choice in backend
by using the
`IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax`
class. You can freely mix and match backends at any time (well, within
the realms of possibility in terms of Haskell polymorphism). For
example, the `beam-migrate` CLI tool loads backends at run-time and
issues queries against them, without knowing the specifics of any
particular backend.

Finally, beam produces readable queries. Here is what opaleye produces on a left join:

```
personBirthdayLeftJoin :: Query ((Column PGText, Column PGInt4, Column PGText),
                                 ColumnNullableBirthday)
personBirthdayLeftJoin = leftJoin personQuery birthdayQuery eqName
    where eqName ((name, _, _), birthdayRow) = name .== bdName birthdayRow

The generated SQL is
ghci> printSql personBirthdayLeftJoin
SELECT result1_0_3 as result1,
       result1_1_3 as result2,
       result1_2_3 as result3,
       result2_0_3 as result4,
       result2_1_3 as result5
FROM (SELECT *
      FROM (SELECT name0_1 as result1_0_3,
                   age1_1 as result1_1_3,
                   address2_1 as result1_2_3,
                   name0_2 as result2_0_3,
                   birthday1_2 as result2_1_3
            FROM
            (SELECT *
             FROM (SELECT name as name0_1,
                          age as age1_1,
                          address as address2_1
                   FROM personTable as T1) as T1) as T1
            LEFT OUTER JOIN
            (SELECT *
             FROM (SELECT name as name0_2,
                          birthday as birthday1_2
                   FROM birthdayTable as T1) as T1) as T2
            ON
            (name0_1) = (name0_2)) as T1) as T1
```

A similar query in beam:

!beam-query
```haskell
!example chinook
do artist <- all_ (artist chinookDb)
   album  <- leftJoin_ (all_ (album chinookDb)) (\album -> albumArtist album ==. primaryKey artist)
   pure (artist, album)
```


### `persistent`/`esqueleto`

Persistent is a simple relational mapper for Haskell. Like beam, it
also supports multiple backends. However, unlike `beam`, it does not
offer a DSL for expressing joins or complex queries. Many people use
the `esqueleto` library to add these features to persistent. However,
`esqueleto` allows a number of constructs to compile that lead to
run-time errors. In particular, join `ON` conditions need to match the
order specified in the `FROM` clause, but this is not checked at
compile time. In contrast, beam handles this for you. If the query
compiles, it will generate proper code.

Moreover, beam's approach is more flexible. `Esqueleto` relies on
pre-defined algebraic data types. Beam uses a finally tagless encoding
so that external packages can provide completely new
functionality. For example, `beam-postgres` is packaged independently
of `beam-core` and offers several advanced features, such as row-level
locking, JSON support, etc, without requiring any changes to core
modules. An OpenGIS package is also in the works, and beam's approach
means this will be packaged separately from core.

### `groundhog`

Groundhog is a fork of `persistent` and it shares many of the same
goals as well as restrictions. It, also, does not offer a DSL for
expressing joins or complex queries. Moreover it currently lacks
a companion library similar to `esqueleto`.

Thus, `groundhog` can be useful for making some basic queries safe,
while more complex things must be handled through a raw query escape
hatch or directly through a backend library such as `postgresql-simple`.

### `hasql`

`hasql` is a library offering compile-time checking of queries using a
quasiquoter. It's really great if you want to write your own SQL query
and embed it in your Haskell source. However, it cannot check that the
shape of the data returned by the query matches what your code
expects. For example, if you issue a command `SELECT a, b, c, d, e`,
but then unpack a 6-tuple in your Haskell code, this will lead to a
run-time error. Beam is much more heavy weight but guarantees that the
shape of data matches. Also, beam allows you to write and compose
queries in a very straightforward Haskell style, that is more
expressive than vanilla SQL.

### `selda`

`selda` has similar aims as beam. However, beam encourages the use of
Haskell record types, whereas selda has its own type-level combinators
for constructing table types. This makes it more straightforward to
use beam types in pre-existing applications.

Beam also has more robust support for migrations. Beam backends
typically map more features than selda ones.

### `squeal`


## Help! The type checker keeps complaining about `Syntax` types

Suppose you had the following code to run a query over an arbitrary backend that
supported the SQL92 syntax.

```haskell
listEmployees :: (IsSql92Syntax cmd, MonadBeam cmd be m) => m [Employee]
listEmployees = runSelectReturningList $ select (all_ (employees employeeDb))
```

You may get an error message like the following

```
MyQueries.hs:1:1: error:
    * Could not deduce: Sql92ProjectionExpressionSyntax
                          (Sql92SelectTableProjectionSyntax
                             (Sql92SelectSelectTableSyntax (Sql92SelectSyntax cmd)))
                        ~
                        Sql92SelectTableExpressionSyntax
                          (Sql92SelectSelectTableSyntax (Sql92SelectSyntax cmd))
        arising from a use of 'select'
```

Beam uses a [finally-tagless](http://okmij.org/ftp/tagless-final/JFP.pdf)
encoding for syntaxes. This means we never deal with concrete syntax types
internally, just types that fulfill certain constraints (in this case, being a
valid description of a SQL92 syntax). This works really nicely for
extensibility, but makes the types slightly confusing. Here, the type checker is
complaining that it cannot prove that the type of expressions used in
projections is the same as the type of expressions used in `WHERE` and `HAVING`
clauses. Of course, any sane SQL92 syntax would indeed meet this criteria, but
this criteria is difficult to enforce at the type class level (it leads to
cycles in superclasses, which requires the scary-looking
`UndecidableSuperclasses` extension in GHC).

Nevertheless, we can avoid all this hullabaloo by using the `Sql92SanityCheck`
constraint synonym. This takes a command syntax and asserts all the type
equalities that a sane SQL92 syntax would support. Thus the code above becomes.

```haskell
listEmployees :: ( IsSql92Syntax cmd, Sql92SanityCheck cmd
                 , MonadBeam cmd be m)
              => m [Employee]
listEmployees = runSelectReturningList $ select (all_ (employees employeeDb))
```

## Other database mappers simulate features on databases that lack support, why not beam?

Beam assumes that the developer has picked their RDBMS for a reason. Beam does
not try to take on features of the RDBMS, because often there is no reasonable
and equally performant substitution that can be made. Beam tries to follow the
principle of least surprise -- the SQL queries beam generates should be easily
guessable from the Haskell query DSL (modulo aliasing). Generating complicated
emulation code which can result in unpredictable performance would violate this
principle.
