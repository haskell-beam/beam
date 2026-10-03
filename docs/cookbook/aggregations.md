These recipes show how to summarize data with `GROUP BY` and aggregate
functions, using the [blog example database](./index.md#the-example-database).
The [aggregates guide](../user-guide/queries/aggregates.md) covers the
machinery in depth.

## Count the rows of a table

`aggregate_` introduces aggregation; with no `group_`, all rows form a single
group. `countAll_` is SQL's `COUNT(*)`.

!beam-query
```haskell
!postgres-blog-sql sql
Just postCount <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningOne
      $ select
        $ aggregate_ (\_ -> as_ @Int32 countAll_)
          $ all_ (_blogPosts blogDb)

print postCount
```

!!! tip "Tip"
    `countAll_` can return any `Integral` type, which makes the result type
    ambiguous. The `as_ @Int32` annotation resolves the ambiguity.

## Group and count

`group_` marks the expressions to group by. Here is the number of comments on
each commented post:

!beam-query
```haskell
!postgres-blog-sql sql
commentCounts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ aggregate_ (\comment -> ( group_ (_commentPost comment)
                                  , as_ @Int32 countAll_ ))
          $ all_ (_blogComments blogDb)

mapM_ print commentCounts
```

With the seed data: two comments each on posts 1 and 3, one each on posts 2
and 5 — and no row at all for post 4, which has no comments. The next recipe
fixes that.

## Count children, including those with zero

To include parents with no children in the counts, start from a `LEFT JOIN`
and count a column of the child table with `count_`. Unlike `COUNT(*)`,
`COUNT(column)` skips `NULL`s, so the all-`NULL` rows produced by the left
join count for zero:

!beam-query
```haskell
!postgres-blog-sql sql
commentCounts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ aggregate_ (\(title, commentId) -> ( group_ title
                                             , as_ @Int32 (count_ commentId) ))
        $ do post    <- all_ (_blogPosts blogDb)
             comment <- leftJoin_ (all_ (_blogComments blogDb))
                                  (\comment -> _commentPost comment ==. primaryKey post)
             pure (_postTitle post, _commentId comment)

mapM_ print commentCounts
```

This time *Thoughts on the future* appears, with a count of 0.

## Filter on an aggregate (HAVING)

Beam has no dedicated `HAVING` syntax: simply `filter_` the result of an
`aggregate_`, and beam emits a `HAVING` clause (or an equivalent subquery)
as appropriate. Here are the posts with at least two comments:

!beam-query
```haskell
!postgres-blog-sql sql
popularPosts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\(_, commentCount) -> commentCount >=. 2)
          $ aggregate_ (\comment -> ( group_ (_commentPost comment)
                                    , as_ @Int32 countAll_ ))
            $ all_ (_blogComments blogDb)

mapM_ print popularPosts
```

## MIN and MAX

The usual SQL aggregates are available with an underscore suffix: `sum_`,
`avg_`, `min_`, `max_`. Aggregating over no rows yields `NULL` in SQL, so
these return `Maybe` values. Here is the date of the latest comment:

!beam-query
```haskell
!postgres-blog-sql sql
Just latestActivity <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningOne
      $ select
        $ aggregate_ (\comment -> max_ (_commentPostedOn comment))
          $ all_ (_blogComments blogDb)

print latestActivity
```

Note the two layers of `Maybe`: `runSelectReturningOne` accounts for the
query returning no row, while the inner `Maybe` (from `max_`) accounts for
`MAX` over an empty table.

## Group by an expression

Groups need not be plain columns. Here is how many comments are attributed
vs. anonymous, grouping on a computed boolean:

!beam-query
```haskell
!postgres-blog-sql sql
attribution <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ aggregate_ (\comment ->
                        let AuthorId author = _commentAuthor comment
                        in ( group_ (isNothing_ author)
                           , as_ @Int32 countAll_ ))
          $ all_ (_blogComments blogDb)

mapM_ print attribution
```

With the seed data, this returns `(False, 4)` and `(True, 2)`: four
attributed comments and two anonymous ones.
