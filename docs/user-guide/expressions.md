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

* **Explicit tables** can be brought to the SQL value level by using `val_` as
  well. For example, if you have an `AddressT Identity` named `a`, `val_ a ::
  AddressT (QGenExpr context expr s)`.
  
## Arithmetic

Arithmetic operations that are part of the `Fractional` and `Num` classes can be
used directly. For example, if `a` and `b` are `QGenExpr`s of the same type,
then `a + b` is a `QGenExpr` of the same type.

Because of the `toInteger` class method in `Integral`, `QGenExpr`s cannot
implement `Integral`. Nevertheless, versions of `div` and `mod` are available as
`div_` and `mod_`, respectively, having the corresponding type.

## Comparison

The standard Haskell/SQL equality and comparison operators are available under
the Haskell operator names, suffixed with `.`. For example, `==.` can be used to
compare two `QGenExpr`s of the same context, thread, syntax, and type with one
another for equality. The return type is another `QGenExpr` with type `Bool`.

The other operators are named as you'd expect: `/=.` for inequality, `<.` for
less than, `>.` for greater than, `<=.` for less than or equal to, `>=.` for
greater than or equal to.

### Quantified comparison

SQL also allows comparisons to be *quantified*. For example, the SQL expression
`a == ANY(b)` evaluates to true only if one row of `b` is equal to `a`.
Similarly, `a > ALL(b)` returns true if `a > x` for every `x` in `b`.

These are also supported using the `==*.`, `/=*.`, `<*.`, `>*.`, `<=*.`, and
`>=*.` operators. Like their unquantified counterparts, these operators yield a
`QGenExpr` of type `Bool`. Unlike the unquantified operators, the second
argument of these operators is of type `QQuantified`. You can create a
`QQuantified` from a `QGenExpr` by using the `anyOf_` or `allOf_` functions,
which correspond to the `ANY` and `ALL` syntax respectively.

## `CASE .. WHEN .. ELSE ..` statements

The SQL `CASE .. WHEN .. ELSE` construct can be used to implement a multi-way
if. The corresponding beam syntax is

```haskell
if_ [ cond1 `then_` result1, cond2 `then_` result2, ... ] (else_ elseResult)
```

where `cond<n>` are `QGenExpr` of type `Bool`, and `result1`, `result2`, and
`elseResult` are `QGenExprs` of the same type.

## SQL Functions and operators

| SQL construct                                            | SQL standard   | Beam equivalent                    | Notes                                                                         |
| :--------------                                          | :------------: | :----------------                  | :--------------                                                               |
| `EXISTS (x)`                                             | SQL92          | `exists_ x`                        | Here, `x` is any query (of type `Q`)                                          |
| `UNIQUE (x)`                                             | SQL92          | `unique_ x`                        | See note for `EXISTS (x)`                                                     |
| `DISTINCT (x)`                                           | SQL99          | `distinct_ x`                      | See note for `EXISTS (x)`                                                     |
| `SELECT .. FROM ...` <br/> as an expression (subqueries) | SQL92          | `subquery_ x`                      | `x` is an query (of type `Q`)                                                 |
| `COALESCE(a, b, c, ...)`                                 | SQL92          | `coalesce_ [a, b, c, ...]`         | `a`, `b`, and `c` must be of <br/>type `Maybe a`.<br/>The result has type `a` |
| `a BETWEEN b AND c`                                      | SQL92          | `between_ a b c`                   |                                                                               |
| `a LIKE b`                                               | SQL92          | ``a `like_` b``                    | `a` and `b` should be string types                                            |
| `a SIMILAR TO b`                                         | SQL99          | ``a `similarTo_` b``               | See note for `LIKE`                                                           |
| `POSITION(x IN y)`                                       | SQL92          | `position_ x y`                    | `x` and `y` should be string types                                            |
| `CHAR_LENGTH(x)`                                         | SQL92          | `charLength_ x`                    |                                                                               |
| `OCTET_LENGTH(x)`                                        | SQL92          | `octetLength_ x`                   |                                                                               |
| `BIT_LENGTH(x)`                                          | SQL92          | `bitLength_ x`                     | `x` must be of the beam-specific `SqlBitString` type                          |
| `x IS TRUE` / `x IS NOT TRUE`                            | SQL92          | `isTrue_ x` / `isNotTrue_ x`       |                                                                               |
| `x IS FALSE` / `x IS NOT FALSE`                          | SQL92          | `isFalse_ x` / `isNotFalse_ x`     |                                                                               |
| `x IS UNKNOWN` / `x IS NOT UNKNOWN`                      | SQL92          | `isUnknown_ x` / `isNotUnknown_ x` |                                                                               |
| `NOT x`                                                  | SQL92          | `not_ x`                           |                                                                               |

### My favorite operator / function isn't listed here!

If your favorite operator or function is not provided here, first ask yourself
if it is part of any SQL standard. If it is not, then check the backend you are
using to see if it provides a corresponding construct. If the backend does not
or if the function / operator you need is part of a SQL standard, please open an
issue on GitHub. Alternatively, implement the construct yourself and send us a
pull request! See the section on [adding your own functions](extensibility.md)
