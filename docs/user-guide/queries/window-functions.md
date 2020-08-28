Window functions allow you to calculate aggregates over portions of your result
set. They are defined in SQL2003. Some databases use the alternative
nomenclature *analytic functions*. They are expressed in SQL with the `OVER`
clause. The
[Postgres documentation](https://www.postgresql.org/docs/9.1/static/tutorial-window.html)
offers a good overview of window functions.

## The `withWindow_` function

When you want to add windows to a query, use the `withWindow_` function to
introduce your frames, and compute the projection. You may notice that this is a
departure from SQL syntax, where you can define window expressions inline. Beam
seeks to be type-safe. Queries with window functions follow slightly different
rules. Wrapping such a query with a special function allows beam to enforce
these rules.

For example, to get each invoice along with the average invoice total by each
customer, use `withWindow_` as follows.

!beam-query
```haskell
!example chinook window
withWindow_ (\i -> frame_ (partitionBy_ (invoiceCustomer i)) noOrder_ noBounds_)
            (\i w -> (i, avg_ (invoiceTotal i) `over_` w))
            (all_ (invoice chinookDb))
```

Or to get each invoice along with the ranking of each invoice by total per
customer *and* the overall ranking,

!beam-query
```haskell
!example chinook window
withWindow_ (\i -> ( frame_ noPartition_ (orderPartitionBy_ (asc_ (invoiceTotal i))) noBounds_
                   , frame_ (partitionBy_ (invoiceCustomer i)) (orderPartitionBy_ (asc_ (invoiceTotal i))) noBounds_ ))
            (\i (allInvoices, customerInvoices) -> (i, as_ @Int32 rank_ `over_` allInvoices, as_ @Int32 rank_ `over_` customerInvoices))
            (all_ (invoice chinookDb))
```

!!! note "Note"
    `rank_` is only available in backends that implement the optional SQL2003
    T611 feature "Elementary OLAP operations". Beam syntaxes that implement this
    functionality implement the
    `IsSql2003ExpressionElementaryOLAPOperationsSyntax` type class.

Notice that aggregates over the result of the window expression work as you'd
expect. Beam automatically generates a subquery once a query has been windowed.
For example, to get the sum of the totals of the invoices, by rank.

!beam-query
```haskell
!example chinook window
orderBy_ (\(rank, _) -> asc_ rank) $
aggregate_ (\(i, rank) -> (group_ rank, sum_ $ invoiceTotal i)) $
withWindow_ (\i -> frame_ (partitionBy_ (invoiceCustomer i)) (orderPartitionBy_ (asc_ (invoiceTotal i))) noBounds_)
            (\i w -> (i, as_ @Int32 rank_ `over_` w))
            (all_ (invoice chinookDb))
```

## More examples

Windows and aggregates can be combined freely. For example, suppose
you wanted to find, for each album, the single genre that represented
most of the tracks on that album.

We can begin by finding the number of tracks in a given genre on a
given album using `countAll_` and `aggregate_`, like so

!beam-query
```haskell
!example chinook
aggregate_ (\t -> ( group_ (trackAlbumId t)
                  , group_ (trackGenreId t)
                  , as_ @Int32 countAll_ )) $
all_ (track chinookDb)
```

Now, we want to find, for each album, which genre has the most
tracks. We can do this by windowing over the album ID.

!beam-query
```haskell
!example chinook window
let albumGenreCnts = aggregate_ (\t -> ( group_ (trackAlbumId t)
                                , group_ (trackGenreId t)
                                , as_ @Int32 countAll_ )) $
                     all_ (track chinookDb)
in withWindow_ (\(albumId, _, _) -> frame_ (partitionBy_ albumId) noOrder_ noBounds_)
               (\(albumId, genreId, trackCnt) albumWindow ->
                   (albumId, genreId, trackCnt, max_ trackCnt `over_` albumWindow)) $
   albumGenreCnts
```

We're almost there. Now, our query returns tuples of

1. an album ID,
2. a genre ID,
3. the number of tracks in that genre for that album, and
4. the number of tracks for the genre with the most tracks in that album

To get just the genre with the most tracks, we have to find the genres
wher the number of tracks (#3) matches #4. We can do this using
`filter_`. Because `max_` can return `NULL` if there are no items in
the window, we use `filter_'` and the nullable ordering operators.

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

## Frame syntax

The `frame_` function takes a partition, ordering, and bounds parameter, all of
which are optional. To specify no partition, use `noPartition_`. For no
ordering, use `noOrder_`. For no bounds, use `noBounds_`.

To specify a partition, use `partitionBy_` with an expression or a tuple of
expressions. To specify an ordering use `orderPartitionBy_` with an ordering
expression or a tuple of ordering expressions. Ordering expressions are scalar
expressions passed to either `asc_` or `desc_`. Finally, to specify bounds, use
`bounds_` or `fromBound_`. `fromBound_` starts the window at the specified
position, which can be `unbounded_` (the default) to include all rows seen thus
far. `bounds_` lets you specify an optional ending bound, which can be `Nothing`
(the default), `Just unbounded_` (the semantic default, but producing an
explicit bound syntactically), or `Just (nrows_ x)`, where `x` is an integer
expression, specifying the number of rows before or after to include in the
calculation.

The following query illustrates some of these features. Along with each invoice, it returns

* The average total of all invoices, given by the frame with no partition, ordering, and bounds.
* The average total of all invoices, by customer.
* The rank of each invoice over all the rows, when ordered by total.
* The average of the totals of the invoices starting at the two immediately
  preceding and ending with the two immediately succeeding invoices, when
  ordered by date.

!beam-query
```haskell
!example chinook window
withWindow_ (\i -> ( frame_ noPartition_ noOrder_ noBounds_
                   , frame_ (partitionBy_ (invoiceCustomer i)) noOrder_ noBounds_
                   , frame_ noPartition_ (orderPartitionBy_ (asc_ (invoiceTotal i))) noBounds_
                   , frame_ noPartition_ (orderPartitionBy_ (asc_ (invoiceDate i))) (bounds_ (nrows_ 2) (Just (nrows_ 2)))))
            (\i (allRows_, sameCustomer_, totals_, fourInvoicesAround_) ->
                 ( i
                 , avg_ (invoiceTotal i) `over_` allRows_
                 , avg_ (invoiceTotal i) `over_` sameCustomer_
                 , as_ @Int32 rank_ `over_` totals_
                 , avg_ (invoiceTotal i) `over_` fourInvoicesAround_ ))
            (all_ (invoice chinookDb))
```
