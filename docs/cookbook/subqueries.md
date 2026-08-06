These recipes nest one query inside another, using the
[blog example database](./index.md#the-example-database).

## EXISTS: rows with at least one related row

`exists_` embeds a whole query as a boolean expression. Here are the authors
who have at least one unpublished draft:

!beam-query
```haskell
!postgres-blog-sql sql
authorsWithDrafts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\author ->
                     exists_ (filter_ (\post -> _postAuthor post ==. primaryKey author
                                            &&. isNothing_ (_postPublishedOn post))
                                      (all_ (_blogPosts blogDb))))
          $ all_ (_blogAuthors blogDb)

mapM_ print authorsWithDrafts
```

With the seed data, only Ada Lovelace has a draft. Note that the inner query
freely refers to `author`, bound in the outer query — a *correlated*
subquery.

## NOT EXISTS: rows with no related row (anti-join)

Negate `exists_` with `not_` to find rows *without* a match. Here are the
posts nobody has commented on:

!beam-query
```haskell
!postgres-blog-sql sql
quietPosts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\post ->
                     not_ (exists_ (filter_ (\comment -> _commentPost comment ==. primaryKey post)
                                            (all_ (_blogComments blogDb)))))
          $ all_ (_blogPosts blogDb)

mapM_ print quietPosts
```

This is the `NOT EXISTS` spelling of an *anti-join*. The same result can be
obtained with a `LEFT JOIN` and an `IS NULL` filter, but `NOT EXISTS`
states the intent more directly.

## Scalar subqueries

When a query returns a single value, `subquery_` turns it into an expression
usable inside another query. Here are the posts published after the very
first comment on the site:

!beam-query
```haskell
!postgres-blog-sql sql
posts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\post ->
                     _postPublishedOn post
                       >=. subquery_ (aggregate_ (min_ . _commentPostedOn)
                                                 (all_ (_blogComments blogDb))))
          $ all_ (_blogPosts blogDb)

mapM_ print posts
```

The types line up because both sides are `Maybe Day`: `published_on` is
nullable, and `MIN` over an empty table would be `NULL`. As usual, the draft
post is excluded by the `NULL` comparison semantics.

## Greatest-n-per-group: the latest comment on each post

A classic: for each post, fetch the most recent comment *row* (not just its
date). Compute the latest date per post in an aggregation subquery, then join
the comments table against it:

!beam-query
```haskell
!postgres-blog-sql sql
latestComments <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do (postKey, lastCommentedOn) <-
               aggregate_ (\comment -> ( group_ (_commentPost comment)
                                       , max_ (_commentPostedOn comment) ))
                 $ all_ (_blogComments blogDb)
             comment <- join_ (_blogComments blogDb)
                              (\comment -> _commentPost comment ==. postKey
                                       &&. just_ (_commentPostedOn comment) ==. lastCommentedOn)
             pure comment

mapM_ print latestComments
```

With the seed data, this returns comments 2, 3, 5 and 6 — the newest comment
on each of the four commented posts.

!!! note "Note"
    If two comments on the same post shared the same date, both would be
    returned. Breaking such ties requires ranking rows; see
    [window functions](./window-functions.md).
