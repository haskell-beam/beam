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

!!! note "Note"
    Using the first and last name as a primary key is a bad idea, we use it here
    to illustrate using multiple fields as the primary key.
    
!!! tip "Tip"
    Many people find it useful to use the `Applicative` instance for `(->) a` to
    write `primaryKey`. For example, we could have written the above `primaryKey
    person = PersonKey (personFirstName person) (personLastName person)` as
    `primaryKey = PersonKey <$> personFirstName <*> personLastName`.

For ease-of-use purposes we define some type synonyms for `PersonT` and
`PrimaryKey PersonT` and some convenient instances. These are not strictly
required but make working with these tables much easier.

```haskell
type Person = PersonT Identity
type PersonKey = PrimaryKey PersonT Identity
deriving instance Show Person; deriving instance Eq Person
deriving instance Show PersonKey; deriving instance Eq PersonKey
```

Due to the magic of the `Columnar` type family, the `Person` type can
be thought of as having the following definition.

```haskell
data Person
    = Person
    { personFirstName :: Text
    , personLastName  :: Text
    , personAge       :: Int
    } deriving (Show, Eq)
```

This allows us to use your type definitions for Beam as regular
Haskell data structures without wrapping/unwrapping.

!!! tip "Tip"
    Typing `Columnar` may become tiresome. `Database.Beam` also exports `C` as a
    type alias for `Columnar`, which may make writing models easier. Since `C`
    may cause name clashes, all examples are given using `Columnar`.

## Foreign references

Foreign references are also easily supported in models by simply
embedding the `PrimaryKey` of the referred to table directly in the
parent. For example, suppose we want to create a new model
representing a post by a user.

```haskell
data PostT f
    = Post
    { postId       :: Columnar f (Auto Int)
    , postPostedAt :: Columnar f LocalTime
    , postContent  :: Columnar f Text
    , postPoster   :: PrimaryKey PersonT f
    } deriving Generic
instance Beamable PostT

instance Table PostT where
  data PrimaryKey PostT f
      = PostId (Columnar f (Auto Int)) deriving Generic
  primaryKey = PostId . postId

type Post = PostT Identity
type PostId = PrimaryKey PostT Identity
deriving instance Show Post; deriving instance Eq Post
deriving instance Show PostId; deriving instance Eq PostId
```

The `Auto` type constructor is provided by `beam-core` for fields that
are automatically assigned by the database. Internally, `Auto x` is
simply a newtype over `Maybe x`. The guarantee is that all values of
type `Auto x` returned by beam in the result set will have a value,
although this guarantee is not enforced at the type level (yet).

## Embedding

Sometimes, we want to declare multiple models with fields in
common. Beam allows you to simple embed such fields in common types
and embed those directly into models. For example,

<!-- TODO(travis) -->

## Defaults

Based on your data type declarations, beam can already guess a lot
about your tables. For example, it already assumes that the
`personFirstName` field is accessible in SQL as `first_name`. This
defaulting behavior makes it very easy to interact with typical
databases.

For the easiest user experience, it's best to follow beam's
conventions for declaring models. In particular, the defaulting
mechanisms rely on each table type declaring only one constructor
which has fields named in the camelCase style.

When defaulting the name of a table field or column, beam
un-camelCases the field name (after dropping leading underscores) and
drops the first word. The remaining words are joined with
underscores. If there is only one component, it is not
dropped. Trailing and internal underscores are preserved in the name
and if the name consists solely of underscores, beam makes no
changes. A summary of these rules is given in the table below.

| Haskell field name | Beam defaulted column name |
|:-------------------|:---------------------------|
| `personFirstName`  | `first_name`               |
| `_personLastName`  | `last_name`                |
| `name`             | `name`                     |
| `first_name`       | `first_name`               |
| `_first_name`      | `first_name`               |
| `___` (three underscores) | `___` (no changes)  |

Note that beam only uses lower case in field names. While typically
case does not matter for SQL queries, beam always quotes
identifiers. Many DBMS's are case-sensitive for quoted
identifiers. Thus, queries can sometimes fail if your tables use
mixtures of lower- and upper-case to distinguish between fields.

All of these defaults can be overriden using the modifications system,
described in the next section.

## What about tables without primary keys?

Tables without primary keys are considered bad style. However,
sometimes you need to use beam with a schema that you have no control
over. To declare a table without a primary key, simply instantiate the
`Table` class without overriding the defaults.

## More complicated relationships

This is the extent of beam's support for defining models. Although
similar packages in other languages provide support for declaring
one-to-many, many-to-one, and many-to-many relationships, beam's
focused is providing a direct mapping of relational database concepts
to Haskell, not on abstracting away the complexities of database
querying. Thus, beam does not use 'lazy-loading' or other tricks that
obfuscate performance. Because of this, the bulk of the functionality
dealing with different types of relations is found in the querying
support, rather than in the model declarations.

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

### A quick note on backends

Beam is backend-agnostic and doesn't provide any means to connect to a
database. Beam backend libraries usually use well-used Haskell
libraries to provide database connectivity. For example, the
`beam-sqlite` backend uses the `sqlite-simple` backend.

Beam distinguishes each backend via type indexes. Each backend defines
a type that is used to enable backend-specific behavior. For example,
the `beam-sqlite` backend ships with the `Sqlite` type that is used to
distinguish sqlite specific constructs with generic or other
backend-specific ones.

Each backend can have one or more 'syntaxes', which are particular
ways to query the database. While the `beam-core` library ships with a
standard ANSI SQL builder, few real-world database implementations
fully follow the standard. Most backends use their own custom syntax
type. Internally, beam uses a finally-tagless representation for
syntax trees that allow straightforward construction against any
backend.

Beam offers backend-generic functions for the most common operations
against databases. These functions are meant to fit the lowest common
denominator. For example, no control is offered over streaming results
from SELECT statements. While these backend-generic functions are
useful for ad-hoc querying and development, it is wisest to use
backend-specific functions in production for maximum control. Refer to
backend-specific documentation for more information.

For our examples, we will use the `beam-sqlite` backend and demonstrate
usage of the beam standard query functions.

### Inserting data

First, let's connect to a sqlite database, and create our schema. The
`beam-core` does not offer any support for the SQL DDL language. There
is a separate core library `beam-migrate` that offers complete support
for ANSI-standard SQL DDL operations, as well as tools to manipulate
database schemas. See the section on migrations for more information.

For our example, we will simply issue a `CREATE TABLE` command
directly against the database using `sqlite-simple` functionality:

```text
Prelude Schema> execute_ conn "CREATE TABLE persons ( first_name TEXT NOT NULL, last_name TEXT NOT NULL, age INT NOT NULL, PRIMARY KEY(first_name, last_name) )"
```

Now we can insert some data into our database. Beam ships with a
function `withDatabase`, with the following signature:

```haskell
withDatabase :: MonadBeam syntax be m => DbHandle be -> m a -> IO a
```

`DbHandle be` is a type family that refers to a backend-specific type
for referring to a particular database connection. For the
`beam-sqlite` backend `DbHandle Sqlite ~
Database.Sqlite.Simple.Connection`. 

`MonadBeam` is a type class relating a particular syntax and backend
to a monad we can use to execute data query and manipulation commands.

Let's insert some data into our database. We are going to use the
`runInsert` function from `MonadBeam`.

```text
Prelude Schema> :{
Prelude Schema| withDatabase conn $ do
Prelude Schema|   runInsert $ insert (persons exampleDb) $
Prelude Schema|               insertValues [ Person "Bob" "Smith" 50
Prelude Schema|                            , Person "Alice" "Wong" 55
Prelude Schema|                            , Person "John" "Quincy" 30 ]
Prelude Schema| :}
```

The `runInsert` function has the type signature

```haskell
runInsert :: MonadBeam syntax be m => SqlInsert syntax -> m ()
```

`SqlInsert syntax` represents a SQL `INSERT` command in the given
`syntax`. We construct this value using the `insert` function from
`Database.Beam.Query`.

```haskell
insert :: IsSql92InsertSyntax syntax =>
          DatabaseEntity be db (TableEntity table)
       -> Sql92InsertValuesSyntax syntax
       -> SqlInsert syntax
```

Intuitively, `insert` takes a database table descriptor and some
values (particular to the given syntax) and returns a statement to
insert these values. `Sql92InsertValuesSyntax syntax` always
implements the `IsSql92InsertValuesSyntax` typeclass, which is where
we get the `insertValues` function from. `IsSql92InsertValuesSyntax`
also defines the `insertSelect` function for inserting values from the
result of a `SELECT` statement. Other backends may provide other ways
of specifying the source of values. This brings us to another point


```text
Prelude Schema> runSelect (select (all_ (persons exampleDb)))
[ Person { personFirstName = "Bob", personLastName="Smith", personAge=50 }, ... ]
```
