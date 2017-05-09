The `beam-core` library and respective backends strive to expose the full power
of each underlying database. If a particular feature is missing, please feel
free to file a bug report on the GitHub issue tracker.

However, in the meantime, beam offers a few options to inject raw SQL into your
queries. Of course, beam cannot predict types of expressions and queries that
were not created with its combinators, so *caveat emptor*.

## Custom expressions

If you'd like to write an expression that beam currently does not support, you
can use the `customExpr_` function. Your backend's syntax must implement the
`IsSqlCustomExpressionSyntax` type class. `customExpr_` takes a function of
arity *n* and *n* arguments, which must all be `QGenExpr`s with the same thread
parameter. The expressions may be from different contexts (i.e., you can pass an
aggregate and scalar into the same `customExpr_`).

The function supplied must return a string-like expression that it can build
using provided `IsString` and `Monoid` instances. The type of the expression is
opaque to the user. The function's arguments will have the same type as the
return type. Thus, they can be embedded into the returned expression using
`mappend`. The arguments will be properly parenthesized and can be inserted
whole into the final expression. You will likely need to explicitly supply a
result type using the `as_` function.

For example, below, we use `customExpr_` to access the `regr_intercept` and
`regr_slope` functions in postgres.

!beam-query
```haskell
!chinookpg postgres
aggregate_ (\t -> ( as_ @Double @QAggregateContext $ customExpr_ (\bytes ms -> "regr_intercept(" <> bytes <> ", " <> ms <> ")") (trackBytes t) (trackMilliseconds t)
                  , as_ @Double @QAggregateContext $ customExpr_ (\bytes ms -> "regr_slope(" <> bytes <> ", " <> ms <> ")") (trackBytes t) (trackMilliseconds t) )) $
all_ (track chinookDb)
```

!!! note "Note"
    Custom queries (i.e., embedding arbitrary expressions into `Q`) is currently
    being planned, but not implemented.

<!-- ## Custom queries -->

<!-- Sometimes you would like to drop down to raw SQL to write a query that will -->
<!-- return an entire result. Beam supports this through the `customQuery_` function. -->
<!-- Like `customExpr_`, this takes a function of *n* arity and *n* arguments, which -->
<!-- may be either `QGenExpr`s or `Q`s from the same thread, select syntax, etc. The -->
<!-- function supplied to `customQuery_` must return a `ByteString` and its arguments -->
<!-- are `ByteString`s corresponding to the given `Q` or `QGenExpr` parameter. -->

