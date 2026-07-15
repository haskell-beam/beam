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

### Inner CTEs

`beam-core`'s `selectWith` produces a top-level `SqlSelect`. PostgreSQL also accepts a SELECT-only
`WITH` query in a derived table, which is useful when the result must participate in a larger Beam
query. For example:

```sql
SELECT a.column1, b.column2
FROM (WITH RECURSIVE ... SELECT ...) a
INNER JOIN b
```

`beam-postgres` provides `pgSelectWith` for this placement. It returns a `Q` value, so its result can
be reused in joins. Calling `select (pgSelectWith x)` projects the same rows as `selectWith x`, but
the generated SQL contains the derived-table wrapper shown above. `pgSelectWith` is useful precisely
when that nested `Q` is required.

### PostgreSQL-specific CTEs

PostgreSQL requires data-modifying CTEs to appear in a `WITH` clause attached to the top-level
statement. `Database.Beam.Postgres.Full` provides `PgWith`, whose `PgCteNestedAllowed`
and `PgCteTopLevelOnly` indices record that placement rule. `pgSelectWithNested` accepts only the
nested-safe form; `pgSelectWithTopLevel`, `pgInsertWith`, `pgUpdateWith`, and `pgDeleteWith` accept
either form because they all produce top-level statements. The existing portable `selecting`,
`selectWith`, and `pgSelectWith` APIs keep their existing types; the PostgreSQL-specific builders
are additive.

The examples in this section use `Database.Beam.Postgres.Full`, qualified as `Pg`, which exports
these PostgreSQL-specific statement builders.

Use `pgSelecting` to define an ordinary SELECT CTE in `PgWith`. Existing helpers returning
`With Postgres` can be composed without rewriting them by applying `pgLiftWith`; lifted and native
CTEs share one name supply. For example, if one native CTE precedes a portable helper containing two
CTEs, the generated names continue through the lifted action:

!beam-query
```haskell
!example chinookdml only:Postgres
rows <- runSelectReturningList $ Pg.pgSelectWithTopLevel $ do
  nativeRows <- Pg.pgSelecting $
    pure (as_ @Int32 (val_ 1))
  portableRows <- Pg.pgLiftWith $ do
    first <- selecting $
      pure (as_ @Int32 (val_ 2))
    selecting $ do
      value <- reuse first
      pure (value + 1)
  pure $ (,) <$> reuse nativeRows <*> reuse portableRows
putStrLn (show rows)
```

PostgreSQL 12 and later also support explicit materialization:

!beam-query
```haskell
!example chinookdml only:Postgres
rows <- runSelectReturningList $ Pg.pgSelectWithTopLevel $ do
  materializedRows <- Pg.pgSelectingWith Pg.PgCteMaterialized $
    pure (as_ @Int32 (val_ 1), as_ @Int32 (val_ 2))
  pure (reuse materializedRows)
putStrLn (show rows)
```

`PgCteDefault` emits no modifier and leaves the choice to PostgreSQL. `PgCteMaterialized` requests
separate calculation of the CTE, which can act as an optimization fence or prevent duplicated
computation. `PgCteNotMaterialized` allows the CTE and parent query to be optimized together, but
may duplicate work. PostgreSQL ignores `NOT MATERIALIZED` for recursive or non-side-effect-free
queries. These rules, and the default behavior for single and multiple references, are described in
the [PostgreSQL CTE materialization documentation](https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-CTE-MATERIALIZATION).

`cteInsertReturning`, `cteUpdateReturning`, and `cteDeleteReturning` put the corresponding
`... RETURNING` statement in a CTE and return a `ReusableQ` handle to its output. Later CTEs or the
final statement can read that output with `reuse`. Their side-effect-only counterparts,
`cteInsert`, `cteUpdate`, and `cteDelete`, omit `RETURNING` and therefore return `()`:

!beam-query
```haskell
!example chinookdml only:Postgres
rows <- runSelectReturningList $ Pg.pgSelectWithTopLevel $ do
  Pg.cteDelete
    (playlist chinookDb)
    (\row -> playlistId row ==. val_ 1000001)
  inserted <- Pg.cteInsertReturning
    (playlist chinookDb)
    (insertValues [Playlist 1000000 (Just "PostgreSQL CTE example")])
    Pg.onConflictDefault
    id
  case inserted of
    Nothing -> pure $
      filter_ (const (val_ False)) $ all_ (playlist chinookDb)
    Just rows -> pure (reuse rows)
putStrLn (show rows)
```

The generated statement contains both modifications in one `WITH` block. The first definition has
no output column list because it has no `RETURNING` output. The second exposes its `RETURNING`
output through generated column names, so it can be reused.

#### Zero-column CTE projections

A Beam projection may have no fields—for example, a custom `Beamable` product with a single
constructor and no record fields. Such a result is still a relation: it has no columns, but it has
one row for every input row. `selecting`, `pgSelecting`, and `pgSelectingWith` preserve that
cardinality. A projection type and query can be written as follows (the helper signature lets the
query scope be inferred at each use):

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

import Data.Kind (Type)
import GHC.Generics (Generic)

data NoColumns (f :: Type -> Type) = NoColumns
  deriving (Generic, Beamable)

noColumns :: NoColumns (QExpr Postgres scope)
noColumns = NoColumns

degreeZeroPlaylists = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.pgSelecting $ do
    _ <- all_ (playlist chinookDb)
    pure noColumns
  pure (reuse rows)
```

PostgreSQL represents the result by omitting the optional CTE column-alias list and by using a
SELECT with an empty target list:

```sql
WITH "cte0" AS
       (SELECT FROM "source" AS "t0")
SELECT FROM "cte0" AS "t0"
```

When this query is run with `runSelectReturningList`, its result contains one zero-field Haskell
value for every source row. The CTE can also be used with `EXISTS`, aggregation, or joins. Reusing
it twice in a Cartesian product multiplies its row count in the same way as any other relation; the
absence of columns does not make it a side-effect-only operation. The `MATERIALIZED` and
`NOT MATERIALIZED` policies have their normal meaning for a zero-column SELECT CTE.

PostgreSQL requires a data-modifying `RETURNING` clause to contain an expression. Therefore, when
the projection supplied to `cteInsertReturning`, `cteUpdateReturning`, or `cteDeleteReturning` has
no fields, `beam-postgres` adds one private boolean result inside the CTE:

```sql
WITH "cte0"("res0") AS
       (DELETE FROM "source" AS "delete_target"
        WHERE ...
        RETURNING NULL::boolean)
SELECT FROM "cte0" AS "t0"
```

The private `res0` value is not selected or passed to the Haskell result decoder. It exists only to
satisfy PostgreSQL's grammar, and one sentinel row is returned for every affected row. This makes
the degree-zero relation useful for counting, existence checks, and repeated reuse. A statement
which affects no rows produces no result rows.

When neither the final statement nor another CTE needs the rows affected by the operation, use
`cteInsert`, `cteUpdate`, or `cteDelete`. These functions omit `RETURNING` and therefore do not
produce a result that can be passed to `reuse`.

The private-sentinel handling is specific to zero-field projections in these reusable CTE
builders. A standalone `returning` call still requires a projection containing at least one value.

PostgreSQL executes every data-modifying CTE exactly once and to completion, even when its output
is not referenced. Sibling modifying statements use the same snapshot and cannot observe one
another's table changes; `RETURNING` rows are the supported way to communicate between them. Avoid
having sibling statements modify the same row, since PostgreSQL does not define which modification
wins.

See PostgreSQL's [data-modifying `WITH` documentation](https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-MODIFYING)
for these execution and visibility rules.

PostgreSQL also disallows a data-modifying CTE from recursively referring to itself. Recursive
construction is therefore limited to `PgCteNestedAllowed`. To let a recursive SELECT feed a later
modifying CTE, finish the recursive block and promote it with `pgToTopLevel` before adding the
modification.

A PostgreSQL `WITH` statement may finish with `INSERT`, `UPDATE`, or `DELETE` instead of a final
SELECT. For example:

!beam-query
```haskell
!example chinookdml only:Postgres
runInsert $ Pg.pgInsertWith $ do
  playlistToInsert <- Pg.pgSelecting $
    filter_ (\source -> playlistId source ==. val_ 1) $
      all_ (playlist chinookDb)
  pure $ Pg.insert
    (playlist chinookDb)
    (insertFrom (reuse playlistToInsert))
    (Pg.onConflict Pg.anyConflict Pg.onConflictDoNothing)
```

This produces one terminal `INSERT`, not a separate SELECT followed by an INSERT.

An empty CTE insert or identity CTE update registers no definition. An empty terminal insert or
identity terminal update remains a no-op: PostgreSQL cannot execute a bare `WITH` block, so CTE
bodies accumulated before that missing terminal statement are not executed.

As an example using our Chinook schema, suppose we had an error with all orders in the month of
September 2024, and needed to send out employees to customer homes to correct the issue. We want to
find, for each order, an employee who lives in the same city as the customer, but we only want the
highest ranking employee for each customer.

First, we order the employees by org structure so that managers appear first, followed by direct reports. We use a recursive query for this, and then join it against the orders.

!beam-query
```haskell
!example chinook only:Postgres
aggregate_ (\(cust, emp) -> (group_ cust, Pg.pgArrayAgg (employeeId emp))) $ do
  inv <- filter_ (\i -> invoiceDate i >=. val_ (read "2024-09-01 00:00:00.000000")  &&. invoiceDate i <=. val_ (read "2024-10-01 00:00:00.000000")) $ all_ (invoice chinookDb)
  cust <- filter_ (\c -> pk c ==. invoiceCustomer inv) $ all_ (customer chinookDb)
  -- Lookup all employees and their levels
  (employee, _, _) <-
    Pg.pgSelectWith $ do
      let topLevelEmployees =
            fmap (\e -> (e, as_ @Int32 (val_ 0))) $
            filter_ (\e -> isNothing_ (employeeReportsTo e)) $ all_ (employee chinookDb)
      rec employeeOrgChart <-
            selecting (topLevelEmployees `unionAll_`
                        do { (manager, managerLevel) <- reuse employeeOrgChart
                          ; report <- filter_ (\e -> employeeReportsTo e ==. just_ (pk manager)) $ all_ (employee chinookDb)
                          ; pure (report, managerLevel + val_ 1) })
      pure $ filter_ (\(_, level, minLevel) -> level ==. minLevel)
            $ withWindow_ (\(employee, _) -> frame_ (partitionBy_ (addressCity (employeeAddress employee))) noOrder_ noBounds_)
                          (\(employee, level) cityFrame ->
                            (employee, level, coalesce_ [min_ level `over_` cityFrame] (val_ 0)))
                          (reuse employeeOrgChart)
  -- Limit the search only to employees that live in the same city
  guard_ (addressCity (employeeAddress employee) ==. addressCity (customerAddress cust))
  pure (cust, employee)
```

### COPY support

`beam-postgres` provides instances of `MonadBeamCopyTo` and
`MonadBeamCopyFrom` (from `Database.Beam.Backend.SQL.BeamExtensions`) for
PostgreSQL's file-mode `COPY` statement. See
[the cross-backend COPY page](../manipulation/copy.md) for the shared
`copyTableTo` / `copySelectTo` / `copyTableFrom` API.

#### Server-side files only

PostgreSQL's `COPY ... TO 'path'` and `COPY ... FROM 'path'` operate on
files **on the database server**, not on the client. As a result, the
calling role needs the `pg_write_server_files` (for COPY TO) or
`pg_read_server_files` (for COPY FROM) role attribute, or be a superuser.
This is not a `beam-postgres` choice — it is a PostgreSQL security policy.

PostgreSQL supports CSV and a text format by default. Each format has its
own options record so the type system prevents mixing options across formats.
Both records have a default value to override selected fields against.


Here's an example of exporting to the text format:

!beam-query
```haskell
!example chinookdml only:Postgres
--! import Database.Beam.Backend.SQL.BeamExtensions
runCopyTo $
  copyTableTo
    (playlist chinookDb)
    id -- no projection: entire table
    (Pg.copyToText "/tmp/beam-docs-playlists.txt")
```

For CSV, the same export can be done this way:

!beam-query
```haskell
!example chinookdml only:Postgres
--! import Database.Beam.Backend.SQL.BeamExtensions
--! import Database.Beam.Postgres
runCopyTo $
  copyTableTo
    (playlist chinookDb)
    id
    ( Pg.copyToCSVWith "/tmp/beam-docs-csv-options.csv"
        Pg.defaultPgCSVCopyToOptions
          { pgCsvCopyToDelimiter = Just '|'
          , pgCsvCopyToHeader    = Just True
          }
    )
```

#### Streaming COPY

`beam-postgres` also provides instances of `MonadBeamCopyToStream` and
`MonadBeamCopyFromStream` for PostgreSQL's
`COPY ... TO STDOUT` / `COPY ... FROM STDIN` statements. The data flows
through the client connection rather than to/from a server-side file, so
**no special role attribute is required** — this is the usual choice for
application code.

The shared statement-builder API(`copyTableToStream` / `copySelectToStream` / `copyTableFromStream`) is
documented on [the cross-backend COPY page](../manipulation/copy.md#streaming-copy). The
PostgreSQL-specific pieces are the smart constructors that build the
options record:

| Smart constructor                                | Wire format               |
| ------------------------------------------------ | ------------------------- |
| `copyToTextStream` / `copyToTextStreamWith`      | PostgreSQL `text` format  |
| `copyToCSVStream`  / `copyToCSVStreamWith`       | `csv` format              |
| `copyFromTextStream` / `copyFromTextStreamWith`    | `text` format             |
| `copyFromCSVStream`  / `copyFromCSVStreamWith`     | `csv` format              |

The format-specific option records (`PgTextCopyToOptions`,
`PgCSVCopyToOptions`, …) are the same ones used by the file-mode API, so
overriding e.g. the delimiter or the header flag works identically.

The `*Stream` runners take an `IO`-typed callback that participates in the
streaming protocol. For `runCopyToStream`, the callback is a
`ByteString -> IO ()` *sink* invoked once per chunk emitted by the server.
The example below prints every chunk without materializing the whole dataset:

!beam-query
```haskell
!example chinookdml only:Postgres
--! import Database.Beam.Backend.SQL.BeamExtensions
--! import Database.Beam.Postgres
--! import qualified Data.ByteString.Char8 as BS

runCopyToStream
  (copyTableToStream
     (playlist chinookDb)
      id
      Pg.copyToCSVStream
  )
  BS.putStrLn -- print every chunk
```

For `runCopyFromStream`, the callback is an `IO (Maybe ByteString)`
*source* that is pulled until it returns `Nothing`. The example below
replays the bytes captured above back into the table — first deleting the
existing rows so the re-import does not conflict on the primary key. As
with the file-mode example, the doc runner rolls back the surrounding
transaction so the chinook database stays unchanged:

!beam-query
```haskell
!example chinookdml only:Postgres
--! import Database.Beam.Backend.SQL.BeamExtensions
--! import Database.Beam.Postgres
--! import qualified Data.ByteString as BS
--! import Data.IORef

-- Capture rows via streaming COPY ... TO STDOUT.
chunksRef <- liftIO $ newIORef []
runCopyToStream
  (copyTableToStream (playlist chinookDb) id Pg.copyToCSVStream)
  (\chunk -> modifyIORef' chunksRef (chunk :))
payload <- liftIO (BS.concat . Prelude.reverse <$> readIORef chunksRef)

-- Clear the playlist table (and its dependents) so the re-import doesn't
-- conflict on primary keys.
runDelete $ delete (playlistTrack chinookDb) (\_ -> val_ True)
runDelete $ delete (playlist chinookDb) (\_ -> val_ True)

-- Replay the captured payload via streaming COPY ... FROM STDIN.
sourceRef <- liftIO $ newIORef (Just payload)
runCopyFromStream
  (copyTableFromStream (playlist chinookDb) id Pg.copyFromCSVStream)
  (do mchunk <- readIORef sourceRef
      writeIORef sourceRef Nothing
      pure mchunk)
```

The format options behave the same as in the file-mode case. Switching the
above example to `Pg.copyToCSVStreamWith` / `Pg.copyFromCSVStreamWith` lets
you override the CSV delimiter, the header flag, the quote character, and
so on.
