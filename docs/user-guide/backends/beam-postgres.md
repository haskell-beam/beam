The `beam-postgres` backend is the most feature complete SQL backend for beam.
The Postgres RDBMS supports most of the standards beam follows, so you can
usually expect most queries to simply work. Additionally, `beam-postgres` is
part of the standard Beam distribution, and so upgrades are applied
periodically, and new functions are added to achieve feature-parity with the
latest Postgres stable

## Postgres-specific data types

Postgres has several data types not available from `beam-core`. The
`beam-postgres` library provides several types and functions to make working
with these easier.

### The `tsvector` and `tsquery` types

The `tsvector` and `tsquery` types form the basis of full-text search
in Postgres. They correspond to the haskell types `TsVector` and
`TsQuery`, which are just newtype-wrappers over `ByteString`.

!beam-query
```haskell
!example chinook only:Postgres
pure (Pg.toTsVector (Just Pg.english) (as_ @String (val_ "The quick brown fox jumps over the lazy dog")))
```

## Postgres extensions

### `SELECT` locking clause

Postgres allows you to explicitly lock rows retrieved during a select
using the [locking
clause](https://www.postgresql.org/docs/current/static/explicit-locking.html).

Beam supports most of the Postgres locking clause. However, there are some
invariants that are currently not checked at compile time. For example, Postgres
does not allow locking clauses with queries that use `UNION`, `EXCEPT`, or
`INTERSECT` or those with aggregates. Since all these queries have the same type
in Beam, we cannot catch these errors at compile-time. Current guidance is to
only use the locking clause in top-level queries that you know to be
safe.

The following example finds all customers living in Dublin, and requests a `ROW
SHARE` lock for each row. This prevents concurrent updates from updating these
rows until the current transaction is complete.

!beam-query
```haskell
!example chinook only:Postgres
Pg.lockingAllTablesFor_ Pg.PgSelectLockingStrengthShare Nothing $
  filter_ (\c -> fromMaybe_ "" (addressCity (customerAddress c)) ==. "Dublin") $
  all_ (customer chinookDb)
```

Now, suppose we want to update these rows, so we'll want to lock them for an update.

!beam-query
```haskell
!example chinook only:Postgres
Pg.lockingAllTablesFor_ Pg.PgSelectLockingStrengthUpdate Nothing $
  filter_ (\c -> fromMaybe_ "" (addressCity (customerAddress c)) ==. "Dublin") $
  all_ (customer chinookDb)
```

However, because there may be a lot of customers in Dublin that we'd like to
update, this may block for a long time. Perhaps, we'd only like to lock rows
that aren't already locked. This is inconsistent in general, but we do not
always care.  Postgres offers the `SKIP LOCKED` clause for this

!beam-query
```haskell
!example chinook only:Postgres
Pg.lockingAllTablesFor_ Pg.PgSelectLockingStrengthUpdate (Just Pg.PgSelectLockingOptionsSkipLocked) $
  filter_ (\c -> fromMaybe_ "" (addressCity (customerAddress c)) ==. "Dublin") $
  all_ (customer chinookDb)
```

Or, if we do care, and don't want to wait anyway, we can ask Postgres to fail
early instead of blocking, using `NO WAIT`

!beam-query
```haskell
!example chinook only:Postgres
Pg.lockingAllTablesFor_ Pg.PgSelectLockingStrengthUpdate (Just Pg.PgSelectLockingOptionsNoWait) $
  filter_ (\c -> fromMaybe_ "" (addressCity (customerAddress c)) ==. "Dublin") $
  all_ (customer chinookDb)
```

We can also specify the locking clauses when `JOIN`ing. Suppose we want to get
all customers who live in London *and* have a support rep who lives in Paris,
and skipping rows that we can't lock.

!beam-query
```haskell
!example chinook only:Postgres
Pg.lockingAllTablesFor_ Pg.PgSelectLockingStrengthShare (Just Pg.PgSelectLockingOptionsSkipLocked) $
  do customer <- filter_ (\c -> fromMaybe_ "" (addressCity (customerAddress c)) ==. "London") $
                 all_ (customer chinookDb)
     employee <- join_ (employee chinookDb)
                       (\e -> fromMaybe_ "" (addressCity (employeeAddress e)) ==. "Paris" &&.
                              just_ (pk e) ==. customerSupportRep customer)
     pure (customerFirstName customer, customerLastName customer, pk employee)
```

You may notice that this query will lock rows in both the customers and
employees table. This may not be what you want. You can also specify which
tables to lock by using the `lockingFor_` function. This requires you to specify
which locks you want to hold by returning them from your query. For example, to
lock only the customers table

!beam-query
```haskell
!example chinook only:Postgres
Pg.lockingFor_ Pg.PgSelectLockingStrengthShare (Just Pg.PgSelectLockingOptionsSkipLocked) $
  do (customerLock, customer) <- Pg.locked_ (customer chinookDb)
     guard_ (fromMaybe_ "" (addressCity (customerAddress customer)) ==. "London")
     employee <- filter_ (\e -> fromMaybe_ "" (addressCity (employeeAddress e)) ==. "Paris" &&.
                                just_ (pk e) ==. customerSupportRep customer) $
                 all_ (employee chinookDb)
     pure ((customerFirstName customer, customerLastName customer, pk employee) `Pg.withLocks_` customerLock)
```

In order to use the explicit locking clause, you need to use the `locked_`
function to get a reference to a lock for a particular table. This forces the
locked table to be part of the join, which is a requirement for the Postgres
locking clause. You can think of `locked_` as exactly like `all_`, except it
returns a table lock as the first return value.

!!! tip "Tip"
    Locks can be combined monoidally, using `mappend` or `(<>)`. You can use this
    to lock multiple tables, by passing the result of `mappend` to `withLocks_`.

    If you return `mempty` as the first argument, then this recovers the standard
    behavior of locking all tables.

`lockingFor_` is the most general locking combinator. You can recover the same
behavior as `lockingAllTablesFor_` by using the `lockAll_` function.

!beam-query
```haskell
!example chinook only:Postgres
Pg.lockingFor_ Pg.PgSelectLockingStrengthShare (Just Pg.PgSelectLockingOptionsSkipLocked) $
  do (customerLock, customer) <- Pg.locked_ (customer chinookDb)
     guard_ (fromMaybe_ "" (addressCity (customerAddress customer)) ==. "London")
     employee <- filter_ (\e -> fromMaybe_ "" (addressCity (employeeAddress e)) ==. "Paris" &&.
                                just_ (pk e) ==. customerSupportRep customer) $
                 all_ (employee chinookDb)
     pure (Pg.lockAll_ (customerFirstName customer, customerLastName customer, pk employee))
```

!!! tip "Tip"
    Table locks have the type `PgLockedTables s`, where `s` is the thread
    parameter, as described
    [here](../queries/basic.md#the-q-data-type)

### `DISTINCT ON` support

Postgres supports the `DISTINCT ON` clause with selects to return distinct
results based on a particular key. The `beam-postgres` package provides the
`pgNubBy_` function to use this feature.

For example, to get an arbitrary customer from each distinct area code

!beam-query
```haskell
!example chinook only:Postgres
Pg.pgNubBy_ (addressPostalCode . customerAddress) $
  all_ (customer chinookDb)
```

### Aggregates

#### `string_agg`

The Postgres `string_agg` aggregate combines all column values in a group
separated by a given separator. `beam-postgres` provides `pgStringAgg` and
`pgStringAggOver` to use the unquantified and quantified versions of the
`string_agg` aggregate appropriately.

For example, to put together a list of all cities in all the postal codes we have for customers,

!beam-query
```haskell
!example chinook only:Postgres
aggregate_ (\c -> ( group_ (addressPostalCode (customerAddress c))
                  , Pg.pgStringAgg (coalesce_ [addressCity (customerAddress c)] "") ",") ) $
  all_ (customer chinookDb)
```

The above will include one city multiple times if its shared by multiple customers.

!beam-query
```haskell
!example chinook only:Postgres
aggregate_ (\c -> ( group_ (addressPostalCode (customerAddress c))
                  , Pg.pgStringAggOver distinctInGroup_ (coalesce_ [addressCity (customerAddress c)] "") ",") ) $
  all_ (customer chinookDb)
```

### ON CONFLICT

Postgres supports targeting a particular constraint as the target of an `ON CONFLICT` clause. You
can use `conflictingConstraint` with the name of the constraint with the regular `insertOnConflict`
function to use this functionality.

For example, to update the row, only on conflicts relating to the `"PK_CUSTOMER"` constraint.

!beam-query
```haskell
!example chinookdml only:Postgres
--! import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict(..))
--! import qualified Database.Beam.Postgres as Pg
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  insertOnConflict (customer chinookDb) (insertValues [newCustomer])
    (Pg.conflictingConstraint "PK_Customer")
    (onConflictUpdateSet (\fields _ -> fields <-. val_ newCustomer))
```

#### Specifying actions

Often times, you do not want to update every field on a conflict. For
example, for upserts, you rarely want to update the primary key. The
function `onConflictUpdateInstead` allows you to restrict which fields
are updated in the case of a conflict. The required function argument
is a projection of which fields ought to be updated.

In the example below, we insert a new row, but if a row with the given
primary key already exists, we update *only* the first and last name.

!beam-query
```haskell
!example chinookdml only:Postgres
-- import qualified Database.Beam.Postgres as Pg
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  Pg.insert (customer chinookDb) (insertValues [newCustomer]) $
    Pg.onConflict
      (Pg.conflictingFields primaryKey)
      (Pg.onConflictUpdateInstead
         (\c -> ( customerFirstName c
                , customerLastName c )))
```

You can also specify a more specific update, using the
`onConflictUpdateSet` function. This is the most general form of the
postgres `ON CONFLICT` action. The `excluded` table is provided as the
second argument. The syntax of the updates is similar to that of
`update`.

In the following example, we append the old first name to the new
first name and replace the old last name.

!beam-query
```haskell
!example chinookdml only:Postgres
-- import qualified Database.Beam.Postgres as Pg
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  Pg.insert (customer chinookDb) (insertValues [newCustomer]) $
    Pg.onConflict
      (Pg.conflictingFields primaryKey)
      (Pg.onConflictUpdateSet
        -- tbl is the old row, tblExcluded is the row proposed for insertion
        (\tbl tblExcluded -> mconcat
          [ customerFirstName tbl <-. concat_ [ current_ (customerFirstName tbl),  customerFirstName tblExcluded ]
          , customerLastName tbl <-. customerLastName tblExcluded ]
        )
      )
```

