In addition to defining types for each of your tables, beam also
requires you to declare your database as a type with fields for
holding all entities in your database. This includes more than just
tables. For example, user-defined types that you would like to work
with must also be included in your database type.

## A simple database type

Like tables, a database type takes a functor and applies it to each
entity in the database. For example, a database type for the two
tables defined above has the form.

```haskell
data ExampleDb f
    = ExampleDb
    { persons :: f (TableEntity PersonT)
    , posts   :: f (TableEntity PersonT)
    } deriving Generic
instance Database ExampleDb

exampleDb :: DatabaseSettings be ExampleDb
exampleDb = autoDbSettings
```

## Other database entities

### Views

Some databases also offer the concept of 'views' -- pseudo-tables that
are built from a pre-defined query. Suppose we wanted to create a view
that returned the latest comments and their respective posters.

```haskell
data PostAndPosterView f
    = PostAndPosterView
    { post   :: PostT f
    , poster :: PersonT f
    } deriving Generic
instance Beamable PostAndPosterView
```

We can include this in our database:

```haskell
data ExampleDb f
    = ExampleDb
    { persons        :: f (TableEntity PersonT)
    , posts          :: f (TableEntity PersonT)
    , postAndPosters :: f (ViewEntity PostAndPosterView)
    } deriving Generic
```

### Unique constraints

The `TableEntityWithUnique` database entity allows you to declare
tables with additional uniqueness constraints (the primary key is
considered to be unique by default).

For example, suppose you wanted to re-define the `PersonT` table with
an additional unique e-mail and another unique phone column.

```haskell
data PersonT f
    = Person
    { personFirstName :: Columnar f Text
    , personLastName  :: Columnar f Text
    , personAge       :: Columnar f Int
    , personEmail     :: Columnar f Text
    , personPhone     :: Columnar f Text
    } deriving Generic

data PersonByEmail f = PersonByEmail (Columnar f Text)
data PersonByPhone f = PersonByPhone (Columnar f Text)
```

Now, use `TableEntityWithUnique` to declare the table.

```haskell
data ExampleDb f
    = ExampleDb
    { persons        :: f (TableEntityWithUnique PersonT '[PersonByEmail, PersonByPhone])
    , posts          :: f (TableEntity PersonT)
    , postAndPosters :: f (ViewEntity PostAndPosterView)
    } deriving Generic
```

Beam will not complain about this definition, but you will need to
declare additional instances in order to actually use the unique
constraints.

```haskell
instance Unique PersonT PersonByEmail where
  mkUnique = PersonByEmail . personEmail

instance Unique PersonT PersonByPhone where
  mkUnique = PersonByPhone . personPhone
```

*TODO*: Should the unique constraints be declared at the database or table level?

### Domain types

Domain types are a way of creating new database types with additional
constraints. Beam supports declaring these types as part of your
database, so they can be used anywhere a data type can. In order to
use your domain type, you need to supply beam a Haskell newtype that
is used to represent values of this type in Haskell.

### Triggers

*TODO*

### Character sets

*TODO*

### Collations

*TODO*

### Translations

*TODO*

## Database descriptors

In order to interact with the database, beam needs to know more about
the data structure, it also needs to know how to refer to each entity
in your database. For the most part, beam can figure out the names for
you using its Generics-based defaulting mechanims. Once you have a
database type defined, you can create a database descriptor using the
`defaultDbSettings` function.

For example, to create a backend-agnostic database descriptor for the
`ExampleDb` type:

```haskell
exampleDb :: DatabaseSettings be ExampleDb
exampleDb = defaultDbSettings
```

Now, we can use the entities in `exampleDb` to write queries. The
rules for name defaulting for database entities are the same as those
for [table fields](models.md#defaults)
