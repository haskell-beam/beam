You can use the `aggregate_` function to group your result set and compute
aggregates within the group. You can think of `aggregate_` as a souped up
version of Haskell's `groupBy`.

You use `aggregate_` by specifying an underlying query to run and a function
that produces an aggregation projection. An aggregation projection is either a
value of type `QAgg syntax s a`, a value of type `QGroupExpr syntax s a`, or a
tuple of such values. Any `QGenExpr` that uses an aggregate function is
automatically assigned the `QAgg syntax s a` type. Any `QGenExpr` that contains
the `group_` combinator is given the type `QGroupExpr`.

During query generation, the expressions of type `QGroupExpr` are added to the
`GROUP BY` clause, and expressions of type `QAgg` are treated as aggregation to
be computed.

The result of the `aggregate_` lifts all the `QAgg`s and `QGroupExpr`s to
'regular' value-level `QExpr`s, so the result of `aggregate_` can be used in
expressions as usual.

## Simple aggregate usage

Suppose we wanted to count the number of genres in our database.

!beam-query
```haskell
!example chinook
aggregate_ (\_ -> as_ @Int32 countAll_) (all_ (genre chinookDb))
```

### Adding a GROUP BY clause

Above, SQL used the default grouping, which puts all rows in one group. We can
also specify columns and expressions to group by. For example, if we wanted to
count the number of tracks for each genre, we can use the `group_` function to
group by the genre.

!beam-query
```haskell
!example chinook
aggregate_ (\(genre, track) -> (group_ genre, as_ @Int32 $ count_ (trackId track))) $ do
  g <- all_ (genre chinookDb)
  t <- genreTracks g
  pure (g, t)
```

!!! tip "Tip"
    `count_` can return any `Integral` type. Adding the explicit `as_ @Int32` above
    prevents an ambiguous type error.

## SQL compatibility

Above, we demonstrated the use of `count_` and `countAll_` which map to the
appropriate SQL aggregates. Beam supports all of the other standard SQL92
aggregates.

In general, SQL aggregates are named similarly in beam and SQL. As usual, the
aggregate function in beam is suffixed by an underscore. For example, `sum_`
corresponds to the SQL aggregate `SUM`.

SQL also allows you to specify set quantifiers for each aggregate. Beam supports
these as well. By convention, versions of aggregates that take in an optional
set quantifier are suffixed by `Over`. For example `SUM(DISTINCT x)` can be
written `sumOver_ distinctInGroup_ x`. The universally quantified version of
each aggregate is obtained by using the `allInGroup_` quantifier. Thus, `sum_ ==
sumOver_ allInGroup_`. Because `ALL` is the default set quantifier, beam does
not typically generate it in queries. If, for some reason, you would like beam
to be explicit about it, you can use the `allInGroupExplicitly_` quantifier.

!beam-query
```haskell
!example chinook
aggregate_ (\(genre, track) ->
              ( group_ genre
              , as_ @Int32 $ countOver_ distinctInGroup_ (trackUnitPrice track)
              , fromMaybe_ 0 (sumOver_ allInGroupExplicitly_ (fromMaybe_ 0 (trackMilliseconds track))) `div_` 1000)) $ do
  g <- all_ (genre chinookDb)
  t <- genreTracks g
  pure (g, t)
```

!!! tip "Tip"
    Most Beam aggregates (`count_` and `countAll_` being an exception) return a
    `Maybe` value, because aggregating over no rows in SQL returns a `NULL`
    value. Use `fromMaybe_` or `coalesce_` to supply a default value in this case.

The `beam-core` library supports the standard SQL aggregation functions.
Individual backends are likely to support the full range of aggregates available
on that backend (if not, please send a bug report).


| SQL Aggregate       | Relevant standard | Unquantified beam function   | Quantified beam function   |
| :------------------ | :---------------: | :--------------------------- | :------------------------- |
| SUM                 | SQL92             | `sum_`                       | `sumOver_`                 |
| MIN                 | SQL92             | `min_`                       | `minOver_`                 |
| MAX                 | SQL92             | `max_`                       | `maxOver_`                 |
| AVG                 | SQL92             | `avg_`                       | `avgOver_`                 |
| COUNT(x)            | SQL92             | `count_`                     | `countOver_`               |
| COUNT(*)            | SQL92             | `countAll_`                  | N/A                        |
| EVERY(x)            | SQL99             | `every_`                     | `everyOver_`               |
| ANY(x)/SOME(x)      | SQL99             | `any_`, `some_`              | `anyOver_`, `someOver_`    |

### The `HAVING` clause

SQL allows users to specify a `HAVING` condition to filter results based on the
computed result of an aggregate. Beam fully supports `HAVIVG` clauses, but does
not use any special syntax. Simply use `filter_` or `guard_` as usual, and beam
will add a `HAVING` clause if it forms legal SQL. Otherwise, beam will create a
subselect and add a `WHERE` clause. Either way, this is transparent to the user.

!beam-query
```haskell
!example chinook
-- Only return results for genres whose total track length is over 5 minutes
filter_ (\(genre, distinctPriceCount, totalTrackLength) -> totalTrackLength >=. 300000) $
aggregate_ (\(genre, track) ->
              ( group_ genre
              , as_ @Int32 $ countOver_ distinctInGroup_ (trackUnitPrice track)
              , fromMaybe_ 0 (sumOver_ allInGroupExplicitly_ (trackMilliseconds track)) `div_` 1000 )) $
           ((,) <$> all_ (genre chinookDb) <*> all_ (track chinookDb))
```

Beam will also handle the `filter_` correctly in the presence of more
complicated queries. For example, we can now join our aggregate on genres back
over tracks.

!beam-query
```haskell
!example chinook
-- Only return results for genres whose total track length is over 5 minutes
filter_ (\(genre, track, distinctPriceCount, totalTrackLength) -> totalTrackLength >=. 300000) $
do (genre, priceCnt, trackLength) <-
            aggregate_ (\(genre, track) ->
                          ( group_ genre
                          , as_ @Int32 $ countOver_ distinctInGroup_ (trackUnitPrice track)
                          , fromMaybe_ 0 (sumOver_ allInGroupExplicitly_ (trackMilliseconds track)) `div_` 1000 )) $
            ((,) <$> all_ (genre chinookDb) <*> all_ (track chinookDb))
   track <- genreTracks genre
   pure (genre, track, priceCnt, trackLength)
```

The position of `filter_` changes the code generated. Above, the `filter_`
produced a `WHERE` clause on the outermost `SELECT`. If instead, we put the
`filter_` clause right outside the `aggregate_`, beam will produce a `HAVING` clause instead.

!beam-query
```haskell
!example chinook
-- Only return results for genres whose total track length is over 5 minutes
do (genre, priceCnt, trackLength) <-
            filter_ (\(genre, distinctPriceCount, totalTrackLength) -> totalTrackLength >=. 300000) $
            aggregate_ (\(genre, track) ->
                          ( group_ genre
                          , as_ @Int32 $ countOver_ distinctInGroup_ (trackUnitPrice track)
                          , fromMaybe_ 0 (sumOver_ allInGroupExplicitly_ (trackMilliseconds track)) `div_` 1000 )) $
            ((,) <$> all_ (genre chinookDb) <*> all_ (track chinookDb))
   track <- genreTracks genre
   pure (genre, track, priceCnt, trackLength)
```

Due to the monadic structure, putting the filtered aggregate as the second
clause in the JOIN causes the HAVING to be floated out, because the compiler
can't prove that the conditional expression only depends on the results of the
aggregate.

!beam-query
```haskell
!example chinook
-- Only return results for genres whose total track length is over 5 minutes
do track_ <- all_ (track chinookDb)
   (genre, priceCnt, trackLength) <-
            filter_ (\(genre, distinctPriceCount, totalTrackLength) -> totalTrackLength >=. 300000) $
            aggregate_ (\(genre, track) ->
                          ( group_ genre
                          , as_ @Int32 $ countOver_ distinctInGroup_ (trackUnitPrice track)
                          , fromMaybe_ 0 (sumOver_ allInGroupExplicitly_ (trackMilliseconds track)) `div_` 1000 )) $
            ((,) <$> all_ (genre chinookDb) <*> all_ (track chinookDb))
   guard_ (trackGenreId track_ ==. just_ (pk genre))
   pure (genre, track_, priceCnt, trackLength)
```

You can prove to the compiler that the `filter_` should generate a having by
using the `subselect_` combinator.

!beam-query
```haskell
!example chinook
-- Only return results for genres whose total track length is over 5 minutes
do track_ <- all_ (track chinookDb)
   (genre, priceCnt, trackLength) <-
            subselect_ $
            filter_ (\(genre, distinctPriceCount, totalTrackLength) -> totalTrackLength >=. 300000) $
            aggregate_ (\(genre, track) ->
                          ( group_ genre
                          , as_ @Int32 $ countOver_ distinctInGroup_ (trackUnitPrice track)
                          , fromMaybe_ 0 (sumOver_ allInGroupExplicitly_ (trackMilliseconds track)) `div_` 1000 )) $
            ((,) <$> all_ (genre chinookDb) <*> all_ (track chinookDb))
   guard_ (trackGenreId track_ ==. just_ (pk genre))
   pure (genre, track_, priceCnt, trackLength)
```
