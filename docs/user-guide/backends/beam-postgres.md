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

The `tsvector` and `tsquery` types form the basis of full-text search in
Postgres.

## Postgres extensions

### `DISTINCT ON` support

Postgres supports the `DISTINCT ON` clause with selects to return distinct
results based on a particular key. The `beam-postgres` package provides the
`pgNubBy_` function to use this feature.

For example, to get an arbitrary customer from each distinct area code

!beam-query
```haskell
!chinookpg postgres
pgNubBy_ (addressPostalCode . customerAddress) $
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
!chinookpg postgres
aggregate_ (\c -> ( group_ (addressPostalCode (customerAddress c))
                  , pgStringAgg (coalesce_ [addressCity (customerAddress c)] "") ",") ) $
  all_ (customer chinookDb)
```

The above will include one city multiple times if its shared by multiple customers.

!beam-query
```haskell
!chinookpg postgres
aggregate_ (\c -> ( group_ (addressPostalCode (customerAddress c))
                  , pgStringAggOver distinctInGroup_ (coalesce_ [addressCity (customerAddress c)] "") ",") ) $
  all_ (customer chinookDb)
```

### ON CONFLICT

Beam supports `ON CONFLICT` statements in a Postgres-specific version of
`insert`. The code below uses the following imports to ensure the correct `insert` is used:

```haskell
import Database.Beam hiding (insert)
import qualified Database.Beam.Postgres as Pg
```

The 3rd argument of the `insert` from `Database.Beam.Postgres` allows
you to specify an `ON CONFLICT` clause. You can use
`onConflictDefault` in order to recover the standard behavior.

```haskell
insert :: DatabaseEntity Postgres db (TableEntity table)
       -> SqlInsertValues PgInsertValuesSyntax table
       -> PgInsertOnConflict table
       -> SqlInsert PgInsertSyntax
```

An explicit `ON CONFLICT` statement requires to specify the indexes
which are conflicting and an action to take when a conflict is
discovered. The `onConflict` function allows you to specify these
parts of the `ON CONFLICT` clause.

```haskell
onConflict :: Beamable tbl
           => PgInsertOnConflictTarget tbl
           -> PgConflictAction tbl
           -> PgInsertOnConflict tbl
```

#### Acting on any conflict

The `anyConflict` value causes the action to be executed when any
index or constraint is violated by the specified `INSERT`. The
following example causes any conflicting update to be ignored. This
could be useful if you want to upsert rows into a database.

!beam-query
```haskell
!chinookdmlpg postgres
-- import qualified Database.Beam.Postgres as Pg
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  Pg.insert (customer chinookDb) (insertValues [newCustomer]) $
    onConflict
      anyConflict
      onConflictDoNothing
```

#### Acting only on certain conflicts

Sometimes you only want to perform an action if a certain constraint
is violated.  If the conflicting index or constraint is on a field,
you can specify the fields with the function `conflictingFields`.

!beam-query
```haskell
!chinookdmlpg postgres
-- import qualified Database.Beam.Postgres as Pg
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  Pg.insert (customer chinookDb) (insertValues [newCustomer]) $
    onConflict
      (conflictingFields primaryKey)
      onConflictSetAll
```

!!!tip "Tip"
   To specify a conflict on the primary keys, use `conflictingField primaryKey`.

If the conflict target is an index, use `conflictingConstraint`, and supply the name of the constraint

!beam-query
```haskell
!chinookdmlpg postgres
-- import qualified Database.Beam.Postgres as Pg
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  Pg.insert (customer chinookDb) (insertValues [newCustomer]) $
    onConflict
      (conflictingConstraint "PK_Customer")
      onConflictSetAll
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
!chinookdmlpg postgres
-- import qualified Database.Beam.Postgres as Pg
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  Pg.insert (customer chinookDb) (insertValues [newCustomer]) $
    onConflict
      (conflictingFields primaryKey)
      (onConflictUpdateInstead
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
!chinookdmlpg postgres
-- import qualified Database.Beam.Postgres as Pg
let
  newCustomer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing "john.doe@johndoe.com" nothing_

runInsert $
  Pg.insert (customer chinookDb) (insertValues [newCustomer]) $
    onConflict
      (conflictingFields primaryKey)
      (onConflictUpdateSet
	    -- tbl is the old row, tblExcluded is the row proposed for insertion
        (\tbl tblExcluded ->
          [ (customerFirstName tbl) <-. concat_ [ current_ (customerFirstName tbl),  customerFirstName tblExcluded ]
          , (customerLastName tbl) <-. (customerLastName tblExcluded) ]
        )
      )
```

