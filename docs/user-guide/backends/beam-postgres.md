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

Beam supports `ON CONFLICT` statements on the postgres-specific version of
`insert`. Use the following imports to ensure the correct `insert` is used:

```haskell
import Database.Beam hiding (insert)
import Database.Beam.Postgres
```

To use the `insert` from `Postgres` like the basic `insert` statement, use
`onConflictDefault` as the third argument to `insert`.

The 3rd argument of the `insert` from `Database.Beam.Postgres` is the `ON
CONFLICT` statement.

```haskell
insert :: DatabaseEntity Postgres db (TableEntity table)
       -> SqlInsertValues PgInsertValuesSyntax table
       -> PgInsertOnConflict table
       -> SqlInsert PgInsertSyntax
```

Every `ON CONFLICT` statement requires to specify the indexes which are
conflicting, and an update statement which specifies which fields are getting
updated, and on which table. Construct the `ON CONFLICT` clause with the
`onConflict` function.

```haskell
onConflict :: Beamable tbl
           => PgInsertOnConflictTarget tbl
           -> PgConflictAction tbl
           -> PgInsertOnConflict tbl
```

#### Specifying targets

The first argument to the `onConflict` function is specfifying on which conflict
an action should be taken.

If the conflicting index is on a field, you can also specify the fields with the
function `conflictingFields`. To specify a conflict on the primary keys, use
`conflictingField primaryKey`.

!beam-query
```haskell
!chinookpg postgres
let
  customer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing Nothing "john.doe@johndoe.com" Nothing
in
  insert (employee chinookDb) (insertValues [customer]) $
    onConflict $
      (conflictingFields primaryKey)
      (onConflictUpdateInstead id)
```

If the conflict target is an index, use `conflictingConstraint`.

#### Specifying updating

The function `onConflictUpdateInstead` takes a
function which takes a table and returns a table to be updated with the
conflicting data. To update the current table, use `id`.

!beam-query
```haskell
!chinookpg postgres
let
  customer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing Nothing "john.doe@johndoe.com" Nothing
in
  insert (employee chinookDb) (insertValues [customer]) $
    onConflict $
      (conflictingFields primaryKey)
      (onConflictUpdateInstead id)
```

To update only some fields, specify them one at a time.

!beam-query
```haskell
!chinookpg postgres
let
  customer = Customer 42 "John" "Doe" Nothing (Address (Just "Street") (Just "City") (Just "State") Nothing Nothing) Nothing Nothing Nothing "john.doe@johndoe.com" Nothing
in
  insert (employee chinookDb) (insertValues [customer]) $
    onConflict $
      (conflictingFields primaryKey)
      (onConflictUpdateSet
        (\tbl tblExcluded ->
          [ (tbl ^. customerFirstName) <-. (tblExcluded ^. customerFirstName)
          , (tbl ^. customerLastName) <-. (tblExcluded ^. customerLastName)]
        )
      )
```

