Window functions compute a value over a *window* of related rows — without
collapsing the rows the way `GROUP BY` does. These recipes use the
[blog example database](./index.md#the-example-database); the
[window functions guide](../user-guide/queries/window-functions.md) explains
the machinery in detail.

!!! note "Note"
    Window functions are supported by `beam-postgres` and `beam-duckdb`, but
    not `beam-sqlite`.

## Rank rows within a group

`withWindow_` introduces the window frames, then the projection function
combines each row with expressions computed `over_` a frame. Here is each
post ranked by publication date *within its author's posts*:

!beam-query
```haskell
!postgres-blog-sql sql
rankedPosts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ withWindow_ (\post -> frame_ (partitionBy_ (_postAuthor post))
                                       (orderPartitionBy_ (asc_ (_postPublishedOn post)))
                                       noBounds_)
                      (\post window -> ( _postTitle post
                                       , as_ @Int32 rank_ `over_` window ))
          $ all_ (_blogPosts blogDb)

mapM_ print rankedPosts
```

With the seed data, Grace Hopper's *Compilers from scratch* ranks 1 and
*Debugging stories* ranks 2; Ada Lovelace's unpublished draft sorts after her
published post, because PostgreSQL places `NULL`s last when ordering in
ascending order.

## Running totals

An ordered window with no partition runs over the whole table. Here is the
cumulative number of comments on the site over time:

!beam-query
```haskell
!postgres-blog-sql sql
commentActivity <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ withWindow_ (\comment -> frame_ noPartition_
                                          (orderPartitionBy_ (asc_ (_commentPostedOn comment)))
                                          noBounds_)
                      (\comment window -> ( _commentPostedOn comment
                                          , as_ @Int32 countAll_ `over_` window ))
          $ all_ (_blogComments blogDb)

mapM_ print commentActivity
```

Each comment is paired with the number of comments posted up to and including
its date.

## Compare each row to its group's aggregate

A window aggregate makes "this row vs. its group" comparisons easy, since the
per-group value is available on every row. Here is each comment alongside the
total number of comments on the same post:

!beam-query
```haskell
!postgres-blog-sql sql
commentsWithContext <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ withWindow_ (\comment -> frame_ (partitionBy_ (_commentPost comment))
                                          noOrder_
                                          noBounds_)
                      (\comment window -> ( _commentContent comment
                                          , as_ @Int32 countAll_ `over_` window ))
          $ all_ (_blogComments blogDb)

mapM_ print commentsWithContext
```
