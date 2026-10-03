These recipes cover the most common ways of getting rows out of a database.
All of them run against the [blog example database](./index.md#the-example-database).

## Fetch all rows from a table

Use `all_` to select every row of a table, and `runSelectReturningList` to
fetch the results.

!beam-query
```haskell
!postgres-blog-sql sql
authors <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ all_ (_blogAuthors blogDb)

mapM_ print authors
```

## Fetch a row by primary key

Use `lookup_` to fetch a single row by its primary key. Since the row may not
exist, use `runSelectReturningOne`, which returns a `Maybe`.

!beam-query
```haskell
!postgres-blog-sql sql
maybeAuthor <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningOne
      $ lookup_ (_blogAuthors blogDb) (AuthorId 1)

print maybeAuthor
```

## Filter rows with a condition

Use `filter_` to add a `WHERE` clause. Comparison operators are spelled with a
trailing dot (`==.`, `>.`, `<=.`, ...) to distinguish them from their Haskell
counterparts. Here we look for posts published after February 1st, 2024.
Because `published_on` is nullable, the comparison is between `Maybe Day`
values, and we lift the literal with `just_`:

!beam-query
```haskell
!postgres-blog-sql sql
recentPosts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\post -> _postPublishedOn post >. just_ (val_ (fromGregorian 2024 2 1)))
          $ all_ (_blogPosts blogDb)

mapM_ print recentPosts
```

With the seed data, this returns posts 3 and 5. Note that the draft (post 4)
is excluded automatically: its `published_on` is `NULL`, and `NULL > '2024-02-01'`
is not true.

## Return only some columns

Queries are monadic: bind the rows of a table and `pure` the columns (or
arbitrary expressions) you want. Beam narrows the `SELECT` projection
accordingly.

!beam-query
```haskell
!postgres-blog-sql sql
titles <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do post <- all_ (_blogPosts blogDb)
             pure (_postTitle post, _postPublishedOn post)

mapM_ print titles
```

## Sort and limit

Use `orderBy_` with `asc_`/`desc_` for `ORDER BY`, and `limit_`/`offset_` for
`LIMIT`/`OFFSET`. Here are the three most recent comments:

!beam-query
```haskell
!postgres-blog-sql sql
latestComments <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ limit_ 3
          $ orderBy_ (desc_ . _commentPostedOn)
            $ all_ (_blogComments blogDb)

mapM_ print latestComments
```

!!! note "Note"
    `limit_` must be applied *outside* `orderBy_` (limit-then-order would be
    a different query!). Beam composes these in the order you write them.

## Select distinct values

Use `nub_` for `SELECT DISTINCT`. For example, the set of days on which a
comment was posted:

!beam-query
```haskell
!postgres-blog-sql sql
days <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ nub_
          $ fmap _commentPostedOn
            $ all_ (_blogComments blogDb)

mapM_ print days
```
