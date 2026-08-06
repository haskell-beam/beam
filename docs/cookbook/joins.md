These recipes show how to combine tables, using the
[blog example database](./index.md#the-example-database). For a full
discussion of relationships in beam, see the
[relationships guide](../user-guide/queries/relationships.md).

## Inner join via a foreign key

Use `related_` to follow a foreign key to the row it references. Beam
generates the join condition for you. Here is every post with the name of its
author:

!beam-query
```haskell
!postgres-blog-sql sql
postsAndAuthors <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do post   <- all_ (_blogPosts blogDb)
             author <- related_ (_blogAuthors blogDb) (_postAuthor post)
             pure (_postTitle post, _authorName author)

mapM_ print postsAndAuthors
```

## Join several tables

Joins compose monadically, so a three-table join is just more binds. Here is
every attributed comment, with the title of the post it was left on and the
name of the commenter:

!beam-query
```haskell
!postgres-blog-sql sql
commentsInContext <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do comment   <- all_ (_blogComments blogDb)
             post      <- related_ (_blogPosts blogDb) (_commentPost comment)
             commenter <- join_ (_blogAuthors blogDb)
                                (\author -> _commentAuthor comment ==. just_ (primaryKey author))
             pure (_postTitle post, _authorName commenter, _commentContent comment)

mapM_ print commentsInContext
```

Since `comments.author__id` is a *nullable* foreign key, we cannot use
`related_` here; instead, `join_` takes an explicit join condition, where
`just_` lifts the non-nullable primary key for comparison against the
nullable one. Anonymous comments do not satisfy the join condition, so they
are absent from the result.

## Walk a relationship in the other direction

The template defines relationship accessors with `oneToMany_`, for example
`authorPosts` and `postComments` (see the
[relationships guide](../user-guide/queries/relationships.md)). These make
"all the children of this parent" queries read naturally. Here is every
comment left on a post written by Ada Lovelace:

!beam-query
```haskell
!postgres-blog-sql sql
commentsOnAda <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do author  <- filter_ (\a -> _authorName a ==. val_ "Ada Lovelace")
                          $ all_ (_blogAuthors blogDb)
             post    <- authorPosts author
             comment <- postComments post
             pure (_postTitle post, _commentContent comment)

mapM_ print commentsOnAda
```

## LEFT JOIN via a nullable foreign key

`leftJoin_` keeps every row on the left side, pairing it with a matching row
on the right *or* an all-`NULL` row. The joined table therefore comes back
with `Nullable` columns. Here is every post with the name of its editor, if
any:

!beam-query
```haskell
!postgres-blog-sql sql
postsAndEditors <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do post   <- all_ (_blogPosts blogDb)
             editor <- leftJoin_ (all_ (_blogAuthors blogDb))
                                 (\editor -> _postEditor post ==. just_ (primaryKey editor))
             pure (_postTitle post, _authorName editor)

mapM_ print postsAndEditors
```

The second component of each result is a `Maybe Text`: it is `Nothing` for
the posts which have no editor (posts 2 and 4 in the seed data).

## LEFT JOIN to keep parents without children

The same construct keeps rows that have no children at all. Here is every
post paired with its comments — including post 4, which has none:

!beam-query
```haskell
!postgres-blog-sql sql
postsAndComments <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do post    <- all_ (_blogPosts blogDb)
             comment <- leftJoin_ (all_ (_blogComments blogDb))
                                  (\comment -> _commentPost comment ==. primaryKey post)
             pure (_postTitle post, _commentContent comment)

mapM_ print postsAndComments
```

To *count* the comments per post instead, see
[counting children, including zero](./aggregations.md#count-children-including-those-with-zero).

## Self-join

Nothing prevents a table from being joined with itself. Here is every pair of
distinct posts written by the same author:

!beam-query
```haskell
!postgres-blog-sql sql
pairs <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do post  <- all_ (_blogPosts blogDb)
             other <- filter_ (\other -> _postAuthor other ==. _postAuthor post
                                     &&. _postId other >. _postId post)
                        $ all_ (_blogPosts blogDb)
             pure (_postTitle post, _postTitle other)

mapM_ print pairs
```

The `>.` on the primary keys avoids pairing a post with itself, and avoids
returning each pair twice.

## Compare two foreign keys on the same row

A related trick: comparing key fields on a single row. Here is every post
whose editor is a *different* person from its author:

!beam-query
```haskell
!postgres-blog-sql sql
reviewed <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\post -> _postEditor post /=. just_ (_postAuthor post))
          $ all_ (_blogPosts blogDb)

mapM_ print reviewed
```

Posts without an editor are excluded automatically: their `editor__id` is
`NULL`, and `NULL <> x` is not true in SQL.
