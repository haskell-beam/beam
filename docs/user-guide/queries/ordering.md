Usually, queries are ordered before `LIMIT` and `OFFSET` are applied. Beam
supports the standard SQL `ORDER BY` construct through the `orderBy_` function.

`orderBy_` works like the Haskell function `sortBy`, with some restructions. Its
first argument is a function which takes as input the output of the given query.
The function should return a sorting key, which is either a single sort ordering
or a tuple of them. A sort ordering specifies an expression and a direction by
which to sort. The result is then sorted lexicographically based on these sort
expressions. The second argument to `orderBy_` is the query whose results to
sort.

Use the `asc_` and `desc_` functions to specify the sort ordering over an
arbitrary expression.

For example, to get the first ten albums when sorted lexicographically, use

!beam-query
```haskell
!chinook sqlite3
!chinookpg postgres
limit_ 10  $
orderBy_ (asc_ . albumTitle) $
all_ (album chinookDb)
```

Again, note that the ordering in which you apply the `limit_` and `orderBy_`
matters. In general, you want to sort before you limit or offset, to keep your
result set stable. However, if you really want to sort a limited number of
arbitrarily chosen rows, you can use a different ordering.

!beam-query
```haskell
!chinook sqlite3
!chinookpg postgres
orderBy_ (asc_ . albumTitle) $
limit_ 10 $
all_ (album chinookDb)
```

## Multiple ordering keys

You can specify multiple keys to order by as well. Keys are sorted
lexicographically in the given direction, as specified in the SQL standard.

For example, we can sort all employees by their state of residence in ascending
order and by their city name in descending order.

!beam-query
```haskell
!chinook sqlite3
!chinookpg postgres
limit_ 10 $
orderBy_ (\e -> (asc_ (addressState (employeeAddress e)), desc_ (addressCity (employeeAddress e)))) $
all_ (employee chinookDb)
```
