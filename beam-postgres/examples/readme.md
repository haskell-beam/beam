# Pagila example

There are two executables:

### 1. `cabal run`
`cabal run` to see rendering of Postgres migration. This converts the Haskell to SQL statements and prints these to the console.

### 2. Destructive: apply migration to Postgres instance

Hard-code your Postgres connection parameters into `app/RunMigration.hs` `connInfo`.
Then `cabal run pagila-migration`. *This will overwrite your Postgres data*.

The result will be the Pagila database schema in your Postgres instance:
```
postgres=# \d
                      List of relations
 Schema |             Name              |   Type   |  Owner
--------+-------------------------------+----------+----------
 public | actor                         | table    | postgres
 public | actor_actor_id_seq            | sequence | postgres
 public | address                       | table    | postgres
 public | address_address_id_seq        | sequence | postgres
 public | beam_migration                | table    | postgres
 public | beam_version                  | table    | postgres
 public | category                      | table    | postgres
.
.
.
```

