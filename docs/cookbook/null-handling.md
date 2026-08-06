SQL `NULL` is a frequent source of surprises. These recipes show how beam
deals with nullable columns and nullable foreign keys, using the
[blog example database](./index.md#the-example-database): `authors.bio` and
`posts.published_on` are nullable columns, while `posts.editor__id` and
`comments.author__id` are nullable foreign keys.

## Find rows where a column IS NULL

Nullable columns have Haskell type `Maybe a`, and `isNothing_` generates
`IS NULL`. For example, drafts are posts that have not been published yet:

!beam-query
```haskell
!postgres-blog-sql sql
drafts <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\post -> isNothing_ (_postPublishedOn post))
          $ all_ (_blogPosts blogDb)

mapM_ print drafts
```

With the seed data, this returns post 4, *Thoughts on the future*. The
converse, `isJust_`, generates `IS NOT NULL` and would return all published
posts.

!!! warning "Warning"
    Do not write `_postPublishedOn post ==. val_ Nothing` to look for NULLs.
    This generates `published_on = NULL`, which is *never* true in SQL —
    `NULL` is not equal to anything, not even itself. Always use `isNothing_`
    and `isJust_`.

## Supply a default for NULL values (COALESCE)

Use `fromMaybe_` (or `coalesce_` for more than one fallback) to replace `NULL`
with a default value:

!beam-query
```haskell
!postgres-blog-sql sql
bios <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ do author <- all_ (_blogAuthors blogDb)
             pure ( _authorName author
                  , fromMaybe_ (val_ "No bio provided") (_authorBio author))

mapM_ print bios
```

Grace Hopper has no bio in the seed data, so her row reads
`("Grace Hopper", "No bio provided")` — and note that the result type is
`Text`, not `Maybe Text`: the default proves to the type system that the
value cannot be missing.

## Find rows where a foreign key IS NULL

A nullable foreign key is represented as a `PrimaryKey` over `Nullable f`,
so its columns are `Maybe` values. Unwrap the key with a pattern match and
test the column with `isNothing_`. For example, anonymous comments are
comments without an author:

!beam-query
```haskell
!postgres-blog-sql sql
anonymousComments <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\comment ->
                     let AuthorId author = _commentAuthor comment
                     in isNothing_ author)
          $ all_ (_blogComments blogDb)

mapM_ print anonymousComments
```

With the seed data, this returns comments 2 and 5.

!!! warning "Warning"
    Beam lets you write `_commentAuthor comment ==. nothing_`, but just as
    with `==. val_ Nothing` above, the generated SQL is `author__id = NULL`,
    which never holds. Unwrap the key and use `isNothing_`.

## Find rows where a foreign key IS NOT NULL

The same pattern with `isJust_` finds posts that went through an editor:

!beam-query
```haskell
!postgres-blog-sql sql
editedPosts' <-
  runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
      $ select
        $ filter_ (\post ->
                     let AuthorId editor = _postEditor post
                     in isJust_ editor)
          $ all_ (_blogPosts blogDb)

mapM_ print editedPosts'
```

With the seed data, this returns posts 1, 3 and 5. To go further and fetch
the editors themselves, see
[joining against a nullable foreign key](./joins.md#left-join-via-a-nullable-foreign-key).
