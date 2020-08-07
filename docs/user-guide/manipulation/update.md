SQL `UPDATE` expressions allow you to update rows in the database.

Beam supplies two functions to update a row in a beam database.

## Saving entire rows

The `save` function allows you to save entire rows to a database. It generates a
`SET` clause that sets the value of every non-primary-key column and a `WHERE`
clause that matches on the primary key.

For example, suppose we have a customer object representing Mark Philips

```haskell
let c :: Customer
    c = Customer 14 "Mark" "Philips" (Just "Telus")
                 (Address (Just "8210 111 ST NW") (Just "Edmonton") (Just "AB")
                          (Just "Canada") (Just "T6G 2C7"))
                 (Just "+1 (780) 434-4554")
                 (Just "+1 (780) 434-5565")
                 "mphilips12@shaw.ca" (EmployeeKey (Just 5))
```

Mark's phone number recently changed and we'd like to update the database based
on our new customer object. We can use Haskell record update syntax to easily
save the entire row.

!beam-query
```haskell
!example chinookdml
Just c <- runSelectReturningOne $ lookup_ (customer chinookDb) (CustomerId 14)
putStrLn ("Old phone number is " ++ show (customerPhone c))

runUpdate $
  save (customer chinookDb)
       (c { customerPhone = Just "+1 (123) 456-7890" })

Just c' <- runSelectReturningOne $ lookup_ (customer chinookDb) (CustomerId 14)
putStrLn ("New phone number is " ++ show (customerPhone c'))
```

The `save` function generates a value of type `SqlUpdate syntax CustomerT`,
where `syntax` is the type of the appropriate backend syntax
(`SqliteCommandSyntax` for `beam-sqlite` and `PgCommandSyntax` for
`beam-postgres`). Like `select` and the `runSelect*` functions, we use the
`runUpdate` function to run the command against the database

## Fine-grained updates

While `save` is useful when many fields may have changed, often times you only
want to update a few columns. Moreover, if you have several large columns, using
`save` may end up sending huge pieces of data to the database. The SQL `UPDATE`
syntax allows you to set or modify each column individually and to even
calculate a new value based off the result of an expression.

The beam `update` function exposes this functionality. `update` takes
a table, a set of assignments (which can be combined monoidally), and
a boolean expression, and returns a `SqlUpdate`.

For example, suppose Canada and the USA became one country and we needed to
update all customer addresses to reflect that.

!beam-query
```haskell
!example chinookdml

Just canadianCount <-
  runSelectReturningOne $ select $
  aggregate_ (\_ -> as_ @Int32 countAll_) $
  filter_ (\c -> addressCountry (customerAddress c) ==. val_ (Just "Canada")) $
  all_ (customer chinookDb)
Just usaCount <-
  runSelectReturningOne $ select $
  aggregate_ (\_ -> as_ @Int32 countAll_) $
  filter_ (\c -> addressCountry (customerAddress c) ==. val_ (Just "USA")) $
  all_ (customer chinookDb)
putStrLn ("Before, there were " ++ show canadianCount ++ " addresses in Canada and " ++ show usaCount ++ " in the USA.")

-- This is the important part!
runUpdate $ update (customer chinookDb)
                   (\c -> addressCountry (customerAddress c) <-. val_ (Just "USA"))
                   (\c -> addressCountry (customerAddress c) ==. val_ (Just "Canada"))

Just canadianCount' <-
  runSelectReturningOne $ select $
  aggregate_ (\_ -> as_ @Int32 countAll_) $
  filter_ (\c -> addressCountry (customerAddress c) ==. val_ (Just "Canada")) $
  all_ (customer chinookDb)
Just usaCount' <-
  runSelectReturningOne $ select $
  aggregate_ (\_ -> as_ @Int32 countAll_) $
  filter_ (\c -> addressCountry (customerAddress c) ==. val_ (Just "USA")) $
  all_ (customer chinookDb)
putStrLn ("Now, there are " ++ show canadianCount' ++ " addresses in Canada and " ++ show usaCount' ++ " in the USA.")
```

We can update columns based on their old value as well, using the `current_`
function. For example, suppose we wanted to fudge our sales data a bit and double
quantity of every line item.


!beam-query
```haskell
!example chinookdml

Just totalLineItems <-
  runSelectReturningOne $ select $
  aggregate_ (\ln -> sum_ (invoiceLineQuantity ln)) $
  all_ (invoiceLine chinookDb)
putStrLn ("Before, we had " ++ show totalLineItems ++ " total products sold\n")

runUpdate $ update (invoiceLine chinookDb)
                   (\ln -> invoiceLineQuantity ln <-. current_ (invoiceLineQuantity ln) * 2)
                   (\_ -> val_ True)

Just totalLineItems' <-
  runSelectReturningOne $ select $
  aggregate_ (\ln -> sum_ (invoiceLineQuantity ln)) $
  all_ (invoiceLine chinookDb)
putStrLn ("With a few simple lines, we've double our sales figure to " ++ show totalLineItems' ++ " products sold!")
```

Amazing! A few simple lines, and we've doubled our sales -- beam is awesome!
