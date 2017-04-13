## How does `beam` compare with `<x>`?

## Help! The type checker keeps complaining about `Syntax` types

Suppose you had the following code to run a query over an arbitrary backend that
supported the SQL92 syntax.

```haskell
listEmployees :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m) => m [Employee]
listEmployees = runSelectReturningList $ select (all_ (employees employeeDb))
```

You may get an error message like the following

```
MyQueries.hs:1:1: error:
    * Could not deduce: Sql92ProjectionExpressionSyntax
                          (Sql92SelectTableProjectionSyntax
                             (Sql92SelectSelectTableSyntax (Sql92SelectSyntax cmd)))
                        ~
                        Sql92SelectTableExpressionSyntax
                          (Sql92SelectSelectTableSyntax (Sql92SelectSyntax cmd))
        arising from a use of 'select'
```

Beam uses a [finally-tagless](http://okmij.org/ftp/tagless-final/JFP.pdf)
encoding for syntaxes. This means we never deal with concrete syntax types
internally, just types that fulfill certain constraints (in this case, being a
valid description of a SQL92 syntax). This works really nicely for
extensibility, but makes the types slightly confusing. Here, the type checker is
complaining that it cannot prove that the type of expressions used in
projections is the same as the type of expressions used in `WHERE` and `HAVING`
clauses. Of course, any sane SQL92 syntax would indeed meet this criteria, but
this criteria is difficult to enforce at the type class level (it leads to
cycles in superclasses, which requires the scary-looking
`UndecidableSuperclasses` extension in GHC).

Nevertheless, we can avoid all this hullabaloo by using the `Sql92SanityCheck`
constraint synonym. This takes a command syntax and asserts all the type
equalities that a sane SQL92 syntax would support. Thus the code above becomes.

```haskell
listEmployees :: ( IsSql92Syntax cmd, Sql92SanityCheck cmd
                 , MonadBeam cmd be hdl m)
              => m [Employee]
listEmployees = runSelectReturningList $ select (all_ (employees employeeDb))
```
