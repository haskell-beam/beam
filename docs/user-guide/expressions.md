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
  Haskell string constants will automatically be converted to `QGenExprs`,
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

### UTF support

All included beam backends play nicely with UTF. New backends should also
support UTF, if they support syntaxes and deserializers for `String` or `Text`.

!beam-query
```haskell
!example chinook utf8
filter_ (\s -> customerFirstName s ==. "あきら") $
  all_ (customer chinookDb)
```

## Arithmetic

Arithmetic operations that are part of the `Fractional` and `Num` classes can be
used directly. For example, if `a` and `b` are `QGenExpr`s of the same type,
then `a + b` is a `QGenExpr` of the same type.

Because of the `toInteger` class method in `Integral`, `QGenExpr`s cannot
implement `Integral`. Nevertheless, versions of `div` and `mod` are available as
`div_` and `mod_`, respectively, having the corresponding type.

## Comparison

SQL comparison is not as simple as you may think. `NULL` handling in particular
actually makes things rather complicated.  SQL comparison operators actually
return a *tri-state boolean*, representing true, false, and *unknown*, which is
the result when two nulls are compared. Boolean combinators (`AND` and `OR`)
handle these values in different ways. Beam abstracts some of this difference
away, if you ask it to.

### Haskell-like comparisons

Haskell provides much more reasonable equality between potentially optional
values. For example, `Nothing == Nothing` always! SQL does not provide a similar
guarantee. However, beam can emulate Haskell-like equality in SQL using the
`==.` operator. This uses a `CASE .. WHEN ..` statement or a special operator
that properly handles `NULL`s in your given backend. Depending on your backend,
this can severely impact performance, but it's 'correct'.

For example, to find all customers living in Berlin:

!beam-query
```haskell
!example chinook utf8
filter_ (\s -> addressCity (customerAddress s) ==. val_ (Just "Berlin")) $
  all_ (customer chinookDb)
```

Notice that SQLite uses a `CASE .. WHEN ..` statement, while Postgres uses the
`IS NOT DISTINCT FROM` operator.

The inequality operator is named `/=.`, as expected. Note that both `==.` and
`/=.` return a SQL expression whose type is `Bool`.

### SQL-like comparisons

Beam also provides equality operators that act like their underlying SQL
counterparts. These operators map most directly to the SQL `=` and `<>`
operators, but they require you to explicitly handle the possibility of
`NULL`s. These operators are named `==?.` and `/=?.` respectively.

Unlike `==.` and `/=.`, these operators return an expression of type
`SqlBool`. `SqlBool` is a type that can only be manipulated as part of a SQL
expression, and cannot be serialized or deserialized to/from Haskell. You need
to convert it to a `Bool` value explicitly in order to get the result or use it
with more advanced operators, such as `CASE .. WHEN ..`.

In SQL, you can handle potentially unknown comparisons using the `IS TRUE`, `IS
NOT TRUE`, `IS FALSE`, `IS NOT FALSE`, `IS UNKNOWN`, and `IS NOT UNKNOWN`
operators. These are provided as the beam functions `isTrue_`, `isNotTrue_`,
etc. These each take a SQL expression of type `SqlBool` and return one of type
`Bool`.

For example, to join every employee and customer who live in the same city, but
using SQL-like equality and making sure the comparison really is true (i.e.,
customers and employees who both have `NULL` cities will not be included).

!beam-query
```haskell
!example chinook utf8
do c <- all_ (customer chinookDb)
   e <- join_ (employee chinookDb) $ \e ->
        isTrue_ (addressCity (customerAddress c) ==?. addressCity (employeeAddress e))
   pure (c, e)
```

Thinking of which `IS ..` operator to use can be confusing. If you have a
default value you'd like to return in the case of an unknown comparison, use the
`unknownAs_` function. For example, if we want to treat unknown values as `True`
instead (i.e, we want customers and employees who both have `NULL` cities to be
included)

!beam-query
```haskell
!example chinook utf8
do c <- all_ (customer chinookDb)
   e <- join_ (employee chinookDb) $ \e ->
        unknownAs_ True (addressCity (customerAddress c) ==?. addressCity (employeeAddress e))
   pure (c, e)
```

### Quantified comparison

SQL also allows comparisons to be *quantified*. For example, the SQL expression
`a == ANY(b)` evaluates to true only if one row of `b` is equal to `a`.
Similarly, `a > ALL(b)` returns true if `a > x` for every `x` in `b`.

These are also supported using the `==*.`, `/=*.`, `<*.`, `>*.`, `<=*.`, and
`>=*.` operators. Like their unquantified counterparts, these operators yield a
`QGenExpr` of type `Bool`. Unlike the unquantified operators, the second
argument of these operators is of type `QQuantified`. You can create a
`QQuantified` from a `QGenExpr` by using the `anyOf_/anyIn_` or `allOf_/allIn_`
functions, which correspond to the `ANY` and `ALL` syntax respectively. `anyOf_`
and `allOf_` take `Q` expressions (representing a query) and `anyIn_` and
`allIn_` take lists of expressions.


Quantified comparisons are always performed according to SQL semantics, meaning
that they return values of type `SqlBOol`. This is because proper NULL handling
with quantified comparisons cannot be expressed in a reasonable way. Use the
functions described in [the section above](#sql-like-comparisons).

For example, to get all invoice lines containing tracks longer than 3 minutes:

!beam-query
```haskell
!example chinook !on:Sqlite
let tracksLongerThanThreeMinutes =
      fmap trackId $
      filter_ (\t -> trackMilliseconds t >=. 180000) $
        all_ (track chinookDb)
in filter_ (\ln -> let TrackId lnTrackId = invoiceLineTrack ln
                   in unknownAs_ False (lnTrackId ==*. anyOf_ tracksLongerThanThreeMinutes)) $
     all_ (invoiceLine chinookDb)
```

We can also supply a concrete list of values. For example to get everyone living
in either Los Angeles or Manila:

!beam-query
```haskell
!example chinook !on:Sqlite !on:MySQL
filter_ (\c ->  unknownAs_ False (addressCity (customerAddress c) ==*. anyIn_ [ just_ "Los Angeles", just_ "Manila" ])) $
     all_ (customer chinookDb)
```

### The `IN` predicate

You can also use `in_` to use the common `IN` predicate.

!beam-query
```haskell
!example chinook
limit_ 10 $
  filter_ (\customer -> customerFirstName customer `in_` [val_ "Johannes", val_ "Aaron", val_ "Ellie"]) $
  all_ (customer chinookDb)
```

!!! note "Note"
    If you want to filter for primary keys you have to manually unwrap them,
    for more information see [here](https://github.com/tathougies/beam/issues/333#issuecomment-444728791).

## `CASE .. WHEN .. ELSE ..` statements

The SQL `CASE .. WHEN .. ELSE` construct can be used to implement a multi-way
if. The corresponding beam syntax is

```haskell
if_ [ cond1 `then_` result1, cond2 `then_` result2, ... ] (else_ elseResult)
```

where `cond<n>` are `QGenExpr` of type `Bool`, and `result1`, `result2`, and
`elseResult` are `QGenExprs` of the same type.

## Manipulating types with `CAST`

Oftentimes, you want to cast data between two different types. SQL
provides the `CAST` function for this purpose. Beam exposes this
functionality through the `cast_` function which takes an expression
and a datatype. For example, to select all line items where the first
digit of the quantity is 2:

!beam-query
```haskell
!example chinook
filter_ (\ln -> cast_ (invoiceLineQuantity ln) (varchar Nothing) `like_` "2%") $
  all_ (invoiceLine chinookDb)
```

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
| `LOWER (x)`                                              | SQL92          | `lower_ x`                         |                                                                               |
| `UPPER (x)`                                              | SQL92          | `upper_ x`                         |                                                                               |
| `TRIM (x)`                                               | SQL92          | `trim_ x`                          |                                                                               |

### My favorite operator / function isn't listed here!

If your favorite operator or function is not provided here, first ask yourself
if it is part of any SQL standard. If it is not, then check the backend you are
using to see if it provides a corresponding construct. If the backend does not
or if the function / operator you need is part of a SQL standard, please open an
issue on GitHub. Alternatively, implement the construct yourself and send us a
pull request! See the section on [adding your own functions](extensibility.md)
