## Typing

The type of all SQL-level expressions is `QGenExpr`. See the [query tutorial](../user-guide/queries/basic.md) for more information.

In many cases, you'd like to type the SQL-level result of an expression without
having to give explicit types for the other `QGenExpr` parameters. You can do
this with the `as_` combinator and `-XTypeApplications`.

The following code types the literal 1 as a `Double`.

```haskell
as_ @Double 1
```

This is rarely needed, but there are a few cases where the beam types are too
general for the compiler to meaningfully infer types.

## Literals

* **Integer literals** can be constructed using `fromIntegral` in the `Num`
  typeclass. This means you can also just use a Haskell integer literal as a
  `QGenExpr` in any context.
* **Rational literals** can be constructed via `fromRational` in `Rational`.
  Regular Haskell rational literals will be automatically converted to
  `QGenExprs`.
* **Text literals** can be constructed via `fromString` in `IsString`. Again,
  Haskell string constants will automaticall be converted to `QGenExprs`,
  although you may have to provide an explicit type, as different backends
  support different text types natively.
* **All other literals** can be constructed using the `val_` function in
  `SqlValable`. This requires that there is an implementation of
  `HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) x` for the type `x` in
  the appropriate `syntax` for the `QGenExpr`. For example, to construct a value
  of type `Vector Int` in the `beam-postgres` backend.
  
```haskell
val_ (V.fromList [1, 2, 3 :: Int])
```
