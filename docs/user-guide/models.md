Beam allows you to define models for your data that can be used across multiple
backends. Beam models are standard Haskell data structures, but with some extra
features that allow Beam to introspect them at run-time.

## A Simple Model

Let's define a simple model to represent a person. Open up a file named
`Schema.hs` and add the following.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
module Schema where

import Database.Beam
import Database.Beam.SQLite
import Database.SQLite.Simple

import Data.Text (Text)

data PersonT f
    = Person
    { personFirstName :: Columnar f Text
    , personLastName  :: Columnar f Text
    , personAge       :: Columnar f Int
    } deriving Generic
instance Beamable PersonT
```

Beam also requires that your tables have a primary key that can be used to
uniquely identify each tuple in your relation. We tell beam about the primary
key by implementing the `Table` type class for your table.

```haskell
instance Table PersonT where
  data PrimaryKey PersonT f
      = PersonKey
      { personKeyFirstName :: Columnar f Text
      , personKeyLastName  :: Columnar f Text
      } deriving Generic
  primaryKey person = PersonKey (personFirstName person) (personLastName person)
```

*Note*: Using the first and last name as a primary key is a bad idea, we use it
here to illustrate using multiple fields as the primary key.

For ease-of-use purposes we define some type synonyms for `PersonT` and
`PrimaryKey PersonT` and some convenient instances. These are not strictly
required but make working with these tables much easier.

```haskell
type Person = PersonT Identity
type PersonKey = PrimaryKey PersonT Identity
deriving instance Show Person; deriving instance Eq Person
deriving instance Show PersonKey; deriving instance Eq PersonKey
```

## Putting a Database Together

Beam also requires you to give a type for your database. The database type
contains all the entities (tables or otherwise) that would be present in your
database. Our database only has one table right now, so it only contains one
field.

```haskell
data ExampleDb f
    = ExampleDb
    { persons :: f (TableEntity PersonT)
    } deriving Generic
instance Database ExampleDb

exampleDb :: DatabaseSettings be ExampleDb
exampleDb = autoDbSettings
```

## Using your database

Let's open up a SQLite database. Open up `ghci` and import your module.

```
Prelude> :load Schema.hs
Prelude Schema> conn <- open "beam-manual.db"
```

### Inserting Data

Most backends ship with a `runInsert` function that let you insert values into
your table. Let's put some values into our persons table.

```
Prelude Schema> :{
Prelude Schema| runInsert (insert (persons exampleDb) $
Prelude Schema| insertValues [ Person "Bob" "Smith" 50
Prelude Schema|              , Person "Alice" "Wong" 55
Prelude Schema|              , Person "John" "Quincy" 30 ]
Prelude Schema| :}
```

Now, let's try retrieving our data. Just like we use `runInsert` to run insert
statements, we can use the `runSelectList` function to run a SELECT statement
and return the results as a list.

```
Prelude Schema> runSelect (select (all_ (persons exampleDb)))
[ Person { personFirstName = "Bob", personLastName="Smith", personAge=50 }, ... ]
```
