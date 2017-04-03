In the User Guide we saw how to declare a schema for an already created database
and use it to perform queries. Beam can also manage a database schema based on
Haskell datatypes you feed it.

The Beam Migrations Framework is meant to be a robust, modular, and opinionated
way of managing schema changes. It is an optional part of beam provided in the
`beam-migrate` package.

Install the migrations framework and tool by running

```
$ cabal install beam-migrate
# or
$ stack install beam-migrate
```

If you use `stack` make sure you always use `stack exec -- beam-migrate` instead
of the typical `beam-migrate` command in order to have the package path
automatically and correctly set for you.

## The Opinions

As stated above, `beam-migrate` is an *opinionated* migrations framework. It
specifies a precise way to lay out your schema definitions.

You can use the `beam-migrate new <module-name> <db-type>` command to create a new
migration schema module. This will create the following directory structure

```
<module-name>.hs
<module-name>/V0001.hs
```

If you open up `<module-name>.hs`, it will contain the following

```haskell
module <module-name> ( module <module-name>.V0001, db, migration ) where

import Database.Beam

import qualified <module-name>.V0001 as V0001

db :: DatabaseSettings be <db-type>
db = V0001.db

migration :: MigrationSteps be <db-type>
migration = migrationStep "Initial schema" V0001.migration
```

If you specify the `--backend` option while running `beam-migrate new`, your
schema will be specialized to the specific backend.

When you create a new version of your schema, you will *not* delete the old one.
Instead, you will copy it, increase the version number, make your changes to the
database schema, write an appropriate migration, and then update the top-level
module to use the newest schema and invoke the latest migration.
