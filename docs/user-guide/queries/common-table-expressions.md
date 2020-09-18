Common table expressions are a SQL99 feature that allow you to reuse common
subqueries in your queries. There is often no semantic difference between CTEs
and reusing Beam queries, but backends sometimes have optimizations that will
only fire when using CTEs. Common table expressions are also necessary to write
*recursive* queries.

## Let's start with an example

Common table expressions are complicated, so let's start with an example.

In the [window function](./window-functions.md) section, we saw how to
find the genre representing the most tracks in a particular album.

!beam-query
```haskell
!example chinook window
let albumGenreCnts = aggregate_ (\t -> ( group_ (trackAlbumId t)
                                , group_ (trackGenreId t)
                                , as_ @Int32 countAll_ )) $
                     all_ (track chinookDb)

    withMaxCounts = withWindow_ (\(albumId, _, _) -> frame_ (partitionBy_ albumId) noOrder_ noBounds_)
                                (\(albumId, genreId, trackCnt) albumWindow ->
                                    (albumId, genreId, trackCnt, max_ trackCnt `over_` albumWindow)) $
                    albumGenreCnts
in filter_' (\(_, _, trackCnt, maxTrackCntPerAlbum) -> just_ trackCnt ==?. maxTrackCntPerAlbum) withMaxCounts
```

Now, suppose that, instead of the album and genre ids, we wanted the names,
along with the name of the artist who produced the album. We can do this by just
joining over the above

!beam-query
```haskell
!example chinook window
let albumGenreCnts = aggregate_ (\t -> ( group_ (trackAlbumId t)
                                , group_ (trackGenreId t)
                                , as_ @Int32 countAll_ )) $
                     all_ (track chinookDb)

    withMaxCounts = withWindow_ (\(albumId, _, _) -> frame_ (partitionBy_ albumId) noOrder_ noBounds_)
                                (\(albumId, genreId, trackCnt) albumWindow ->
                                    (albumId, genreId, trackCnt, max_ trackCnt `over_` albumWindow)) $
                    albumGenreCnts

    albumAndRepresentativeGenres = filter_' (\(_, _, trackCnt, maxTrackCntPerAlbum) -> just_ trackCnt ==?. maxTrackCntPerAlbum) withMaxCounts

in do
  (albumId, genreId, _, _) <- albumAndRepresentativeGenres

  genre_ <- join_' (genre chinookDb) (\g -> genreId ==?. just_ (primaryKey g))
  album_ <- join_' (album chinookDb) (\a -> albumId ==?. just_ (primaryKey a))

  artistName <- fmap artistName $
                join_ (artist chinookDb) (\a -> albumArtist album_ `references_` a)

  pure ( artistName, albumTitle album_, genreName genre_ )
```

Because we're using Beam, and Beam is just Haskell, we are free to compose
queries arbitrarily, and get the right results. However, the generated SQL is a
bit dense, and some backends may not optimize it well. Moreover, if we plan on
using `albumAndRepresentativeGenres` more than once, then we'd like to express
this syntactically, rather than repeating the query wherever we need it. In
other words, we want to be able to signal the concept of re-use to the database
system.

## The `selectWith` function

We can rewrite the above query using common table expressions. Firstly, we'll
have to tell Beam that we want to write a `SELECT` statement with a `WITH`
expression. We can do this by using `selectWith` instead of
`select`. `selectWith` takes one argument, which is a monadic action returning a
query. The monad expected is the `With` monad from
`Database.Beam.Query.CTE`. Within this monad, bindings represent queries we
would like bound at the top-level.

So, let's start building our query using `selectWith`. We'll want to return a
list, so we'll use `runSelectReturningList`.

```haskell
runSelectReturningList $ selectWith $ do
```

## Binding subqueries with `selecting`

Now, we're in the `With` monad, so we can start binding common table
expressions. We can bind multiple different types of results here. On all
backends that support CTEs, you can bind the results of `SELECT`, but in some
backends, you can bind the results of `INSERT`, `DELETE`, `UPDATE`, etc.

In our case, we would like to bind the results of a `SELECT` statement, so we
can use the `selecting` function. This function takes a query (represented by a
value of type `Q`) and returns a `ReusableQ`, which is a query value that can be
re-used elsewhere.

A `ReusableQ` is parameterized by three paramaters

```haskell
data ReusableQ be db res
```

* `be` -- This is the backend that the query is in. For example, `Postgres` for
  `beam-postgres` or `Sqlite` for `beam-sqlite`.
* `db` -- This is the type of the database the query is written over
* `res` -- This is the type of each row returned by the query.

Notice that `ReusableQ` has no scoping parameter like regular `Q`
expressions. This is because `ReusableQ` values can be rescoped at any
level. We'll get to this in the next section.

We can introduce the `albumAndRepresentativeGenres` query into the `With` monad
using `selecting`.

```haskell
  albumAndRepresentativeGenres <-
    selecting $
    let albumGenreCnts = aggregate_ (\t -> ( group_ (trackAlbumId t)
                                    , group_ (trackGenreId t)
                                    , as_ @Int32 countAll_ )) $
                         all_ (track chinookDb)

        withMaxCounts = withWindow_ (\(albumId, _, _) -> frame_ (partitionBy_ albumId) noOrder_ noBounds_)
                                    (\(albumId, genreId, trackCnt) albumWindow ->
                                        (albumId, genreId, trackCnt, max_ trackCnt `over_` albumWindow)) $
                        albumGenreCnts
    in filter_' (\(_, _, trackCnt, maxTrackCntPerAlbum) -> just_ trackCnt ==?. maxTrackCntPerAlbum) withMaxCounts
```

## Using the query with `reuse`

Now that we have taken that query out of scope, we'll need the ability to refer
to its result. Queries are in the `Q` monad, but `albumAndRepresentativeGenres`
has type `ReusableQ`. In order to use the value in the `Q` monad, we can use the
`reuse` function. This utility function ensures that the query is able to be
used at any nesting level.

Also note that since we're in the `With` monad, we'll need to inject our query
into that using `pure`.

```haskell
  pure $ do
    (albumId@(AlbumId albumIdColumn), genreId, _, _) <-
       reuse albumGenreCountQ
    genre_ <- join_' (genre chinookDb) (\g -> genreId ==?. just_ (primaryKey g))
    album_ <- join_' (album chinookDb) (\a -> albumId ==?. just_ (primaryKey a))
    artist_ <- join_ (artist chinookDb) (\a -> albumArtist album_ `references_` a)

    pure ( artistName artist_, albumTitle album_, genreName genre_ )
```

Phew! Putting all of that together, and executing, we get...

!beam-query
```haskell
!example chinookdml cte window
void $ runSelectReturningList $ selectWith $ do
  albumAndRepresentativeGenres <-
    selecting $
    let albumGenreCnts = aggregate_ (\t -> ( group_ (trackAlbumId t)
                                    , group_ (trackGenreId t)
                                    , as_ @Int32 countAll_ )) $
                         all_ (track chinookDb)

        withMaxCounts = withWindow_ (\(albumId, _, _) -> frame_ (partitionBy_ albumId) noOrder_ noBounds_)
                                    (\(albumId, genreId, trackCnt) albumWindow ->
                                        (albumId, genreId, trackCnt, max_ trackCnt `over_` albumWindow)) $
                        albumGenreCnts
    in filter_' (\(_, _, trackCnt, maxTrackCntPerAlbum) -> just_ trackCnt ==?. maxTrackCntPerAlbum) withMaxCounts

  pure $ do
    (albumId, genreId, _, _) <-
       reuse albumAndRepresentativeGenres
    genre_ <- join_' (genre chinookDb) (\g -> genreId ==?. just_ (primaryKey g))
    album_ <- join_' (album chinookDb) (\a -> albumId ==?. just_ (primaryKey a))
    artist_ <- join_ (artist chinookDb) (\a -> albumArtist album_ `references_` a)

    pure ( artistName artist_, albumTitle album_, genreName genre_ )
```

### Refining our query

Notice that the `filter_'` condition on the common table expression resulted in
a nasty subselect. We can get rid of that by introducing the `filter_'` in the
outer query.

!beam-query
```haskell
!example chinookdml cte window
void $ runSelectReturningList $ selectWith $ do
  albumAndRepresentativeGenres <-
    selecting $
    let albumGenreCnts = aggregate_ (\t -> ( group_ (trackAlbumId t)
                                    , group_ (trackGenreId t)
                                    , as_ @Int32 countAll_ )) $
                         all_ (track chinookDb)

    in withWindow_ (\(albumId, _, _) -> frame_ (partitionBy_ albumId) noOrder_ noBounds_)
                   (\(albumId, genreId, trackCnt) albumWindow ->
                       (albumId, genreId, trackCnt, max_ trackCnt `over_` albumWindow)) $
       albumGenreCnts

  pure $ do
    (albumId, genreId, _, _) <-
       filter_' (\(_, _, trackCnt, maxTrackCntInAlbum) ->
                     just_ trackCnt ==?. maxTrackCntInAlbum) $
       reuse albumAndRepresentativeGenres
    genre_ <- join_' (genre chinookDb) (\g -> genreId ==?. just_ (primaryKey g))
    album_ <- join_' (album chinookDb) (\a -> albumId ==?. just_ (primaryKey a))
    artist_ <- join_ (artist chinookDb) (\a -> albumArtist album_ `references_` a)

    pure ( artistName artist_, albumTitle album_, genreName genre_ )
```

To demonstrate that we can still use all beam's features, let's only return
results for albums with tracks of more than one genre. We can do this by using a
quantified comparison operator on the album id column, and a subquery to find
all albums with more than one genre.

!beam-query
```haskell
!example chinookdml cte window
void $ runSelectReturningList $ selectWith $ do
  albumAndRepresentativeGenres <-
    selecting $
    let albumGenreCnts = aggregate_ (\t -> ( group_ (trackAlbumId t)
                                    , group_ (trackGenreId t)
                                    , as_ @Int32 countAll_ )) $
                         all_ (track chinookDb)

    in withWindow_ (\(albumId, _, _) -> frame_ (partitionBy_ albumId) noOrder_ noBounds_)
                   (\(albumId, genreId, trackCnt) albumWindow ->
                       (albumId, genreId, trackCnt, max_ trackCnt `over_` albumWindow)) $
       albumGenreCnts

  pure $ do
    (albumId@(AlbumId albumIdColumn), genreId, _, _) <-
       filter_' (\(_, _, trackCnt, maxTrackCntInAlbum) ->
                     just_ trackCnt ==?. maxTrackCntInAlbum) $
       reuse albumAndRepresentativeGenres
    genre_ <- join_' (genre chinookDb) (\g -> genreId ==?. just_ (primaryKey g))
    album_ <- join_' (album chinookDb) (\a -> albumId ==?. just_ (primaryKey a))
    artist_ <- join_ (artist chinookDb) (\a -> albumArtist album_ `references_` a)

    -- Filter out all albums with tracks of only one genre
    guard_' (albumIdColumn ==*.
             anyOf_ (orderBy_ asc_ $ fmap (\(AlbumId albumIdRaw, _) -> albumIdRaw) $
                     filter_ (\(_, genreCntByAlbum) -> genreCntByAlbum >. 1) $
                     aggregate_ (\t -> let GenreId genreId = trackGenreId t
                                       in ( group_ (trackAlbumId t)
                                          , as_ @Int32 (countOver_ distinctInGroup_ genreId))) $
                     all_ (track chinookDb)))

    pure ( artistName artist_, albumTitle album_, genreName genre_ )
```

without CTEs,


!beam-query
```haskell
!example chinook cte window
do let albumGenreCnts = aggregate_ (\t -> ( group_ (trackAlbumId t)
                                   , group_ (trackGenreId t)
                                   , as_ @Int32 countAll_ )) $
                        all_ (track chinookDb)

       albumAndRepresentativeGenres =
         withWindow_ (\(albumId, _, _) -> frame_ (partitionBy_ albumId) noOrder_ noBounds_)
                     (\(albumId, genreId, trackCnt) albumWindow ->
                         (albumId, genreId, trackCnt, max_ trackCnt `over_` albumWindow)) $
           albumGenreCnts


   (albumId@(AlbumId albumIdColumn), genreId, _, _) <-
      filter_' (\(_, _, trackCnt, maxTrackCntInAlbum) ->
                    just_ trackCnt ==?. maxTrackCntInAlbum) $
      albumAndRepresentativeGenres
   genre_ <- join_' (genre chinookDb) (\g -> genreId ==?. just_ (primaryKey g))
   album_ <- join_' (album chinookDb) (\a -> albumId ==?. just_ (primaryKey a))
   artist_ <- join_ (artist chinookDb) (\a -> albumArtist album_ `references_` a)

   -- Filter out all albums with tracks of only one genre
   guard_' (albumIdColumn ==*.
            anyOf_ (orderBy_ asc_ $ fmap (\(AlbumId albumIdRaw, _) -> albumIdRaw) $
                    filter_ (\(_, genreCntByAlbum) -> genreCntByAlbum >. 1) $
                    aggregate_ (\t -> let GenreId genreId = trackGenreId t
                                      in ( group_ (trackAlbumId t)
                                         , as_ @Int32 (countOver_ distinctInGroup_ genreId))) $
                    all_ (track chinookDb)))

   pure ( artistName artist_, albumTitle album_, genreName genre_ )
```

## Reusing queries multiple times

Suppose we wanted to ask the question, "which albums have tracks under the same
genre?". One easy way of doing this is to do multiple self-joins.

We can also use a common table expression.

Let's call albums `A` and `B` "genre-related" if `A` and `B` contain at least
one track under the same genre. Then, the queries above answer if `A` and `B`
are "genre-related". A natural extension of the question above then is "Which
albums are genre-related to the same album?". For example, suppose `A` has Jazz
and Rock tracks, `B` has Rock and Country tracks, and `C` has Country and Blues
tracks. Then, `(A, B)` and `(B, C)` will both be results of the query above, but
`(A, C)` will not. We can output tuples like `(A, C)` by self-joining on the
results of the same CTE.

## Recursive queries

Okay, so the next natural extension is to extend this relation by relating any
two albums `A` and `D` where there exist `B` and `C` such that `A` is related to
`B`, `B` is related to `C`, and `C` is related to `D`. We can keep extending
this query forever, but the queries above require that we specify all lengths
we're interested in. Conceptually, we could express in Haskell an infinite query:

However, the query generator will loop when serializing this query, because
database systems don't operate on laziness they way Haskell does!

To solve this, some RDBMS systems offer "recursive" queries [^1]. The canonical
example of a recursive query is the Fibonacci sequence:

!beam-query
```haskell
!example chinookdml cte window
void $ runSelectReturningList $ selectWith $ do
  rec fib <- selecting (pure (as_ @Int32 0, as_ @Int32 1) `union_`
                        (do (a, b) <- reuse fib
                            guard_ (b <. 1000)
                            pure (b, a + b)))
  pure (reuse fib)
```

[^1]:
   "Recursive" here is in quotes, because this is not true, general recursion,
   but rather iteration. Still the term "recursive" has stuck around, so Beam
   adopts the convention.

