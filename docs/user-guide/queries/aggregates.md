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

```haskell
aggregate_ (\_ -> countAll_) (all_ (genre chinookDb))
-- Translates to (sqlite syntax):
--
-- SELECT COUNT(*) AS [res0] FROM [Genre] AS [t0]
```

### Adding a GROUP BY clause

Above, SQL used the default grouping, which puts all rows in one group. We can
also specify columns and expressions to group by. For example, if we wanted to
count the number of tracks for each genre, we can use the `group_` function to
group by the genre.

```haskell
aggregate_ (\(genre, track) -> (group_ genre, count_ (trackId track)))
           ((,) <$> all_ (genre chinookDb) <*> all_ (track chinookDb))
```

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

