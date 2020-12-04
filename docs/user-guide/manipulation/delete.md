SQL `DELETE` expressions allow you to remove rows from a database.

The `delete` function from `Database.Beam.Query` can be used to delete
rows from a particular table. The function takes a table and a
condition for the `WHERE` clause. The function returns a `SqlDelete`
object that can be run in `MonadBeam` with `runDelete`.

For example, to delete any customer, whose first name is Emilio.

!beam-query
```haskell
!example chinookdml
runDelete $ delete (customer chinookDb)
  (\c -> customerFirstName c ==. "Emilio")
```

`DELETE` is fairly simple compared to the other manipulation commands,
so that's really all there is to it. Expressions for the `WHERE`
clause can be arbitrarily complex, assuming your backend supports it.

For example, to delete any invoice with more than five invoice lines

!beam-query
```haskell
!example chinookdml !on:Postgres !on:MySQL
runDelete $ delete (invoice chinookDb)
  (\i -> 5 <. subquery_ (aggregate_ (\_ -> as_ @Int32 countAll_) $
                         invoiceLines i))
```

!!! note "Note"
    The example above was only given for SQLite because it violates a
    foreign key constraint in the underlying database, and other
    backends are more pedantic
