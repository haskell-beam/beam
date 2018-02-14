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

