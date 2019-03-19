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
    , posts   :: f (TableEntity PostT)
    } deriving (Generic, Database be)

exampleDb :: DatabaseSettings be ExampleDb
exampleDb = defaultDbSettings
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
    } deriving (Generic, Beamable)
```

We can include this in our database:

```haskell
data ExampleDb f
    = ExampleDb
    { persons        :: f (TableEntity PersonT)
    , posts          :: f (TableEntity PostT)
    , postAndPosters :: f (ViewEntity PostAndPosterView)
    } deriving (Generic, Database be)
```

Now we can use `postAndPosters` wherever we'd use a table. Note that you do not
need to specify the definition of the view. The definition is not important to
access the view, so beam does not need to know about it at the type-level. If
you want to manipulate view definitions, use the migrations package.

Note that the `all_` query primitive requires a `TableEntity`. Thus, `all_
(postAndPosters exampleDb)` will fail to type-check. Use the `allFromView_`
combinator instead.

!!! note "Note"
    You could also declare a view as a `TableEntity`. The main advantage of
    declaring an entity as `ViewEntity` is that you will be prevented by the
    Haskell type system from constructing `INSERT`s, `UPDATE`s, and `DELETE`s
    using your view. Also, `beam-migrate` will not recognize database schema
    equivalence if a view is declared as a table or vice versa.

<!-- ### Unique constraints -->

<!-- !!! note "Note" -->
<!--     This is the current implementation plan. Uniques are not currently implemented. -->

<!-- The `TableEntityWithUnique` database entity allows you to declare -->
<!-- tables with additional uniqueness constraints (the primary key is -->
<!-- considered to be unique by default). -->

<!-- For example, suppose you wanted to re-define the `PersonT` table with -->
<!-- an additional unique e-mail and another unique phone column. -->

<!-- ```haskell -->
<!-- data PersonT f -->
<!--     = Person -->
<!--     { personFirstName :: Columnar f Text -->
<!--     , personLastName  :: Columnar f Text -->
<!--     , personAge       :: Columnar f Int -->
<!--     , personEmail     :: Columnar f Text -->
<!--     , personPhone     :: Columnar f Text -->
<!--     } deriving Generic -->

<!-- data PersonByEmail f = PersonByEmail (Columnar f Text) -->
<!-- data PersonByPhone f = PersonByPhone (Columnar f Text) -->
<!-- ``` -->

<!-- Now, use `TableEntityWithUnique` to declare the table. -->

<!-- ```haskell -->
<!-- data ExampleDb f -->
<!--     = ExampleDb -->
<!--     { persons        :: f (TableEntityWithUnique PersonT '[PersonByEmail, PersonByPhone]) -->
<!--     , posts          :: f (TableEntity PostT) -->
<!--     , postAndPosters :: f (ViewEntity PostAndPosterView) -->
<!--     } deriving Generic -->
<!-- ``` -->

<!-- Beam will not complain about this definition, but you will need to -->
<!-- declare additional instances in order to actually use the unique -->
<!-- constraints. -->

<!-- ```haskell -->
<!-- instance Unique PersonT PersonByEmail where -->
<!--   mkUnique = PersonByEmail . personEmail -->

<!-- instance Unique PersonT PersonByPhone where -->
<!--   mkUnique = PersonByPhone . personPhone -->
<!-- ``` -->

<!-- *TODO*: Should the unique constraints be declared at the database or table level? -->

### Domain types

Domain types are a way of creating new database types with additional
constraints. Beam supports declaring these types as part of your
database, so they can be used anywhere a data type can. In order to
use your domain type, you need to supply beam a Haskell newtype that
is used to represent values of this type in Haskell.

### Character sets

Beam does not yet support character sets. Support is planned in future releases.

### Collations

Beam does not yet support collations. Support is planned in future releases.

### Translations

Beam does not yet support translations. Support is planned in future releases.

### Other database entities

Other standard SQL database entities (like triggers) are defined by
`beam-migrate` as they have no effect on query semantics.

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

The `defaultDbSettings` function produces a settings value where each entity is
given a default name as explained in the [previous section](models.md).

Now, we can use the entities in `exampleDb` to write queries. The
rules for name defaulting for database entities are the same as those
for [table fields](models.md#defaults)

### Modifying the defaults

The `withDbModification` function can be used to modify the output of the
`defaultDbSettings`. It combines a database settings value with a *database
modifications value*. The easiest way to construct a database modification value
is with the `dbModification` function, which produces a modification that makes
no changes.

You can then use Haskell record syntax to specify table or other entity
modifications. Modifications can be combined with the `(<>)` semigroup operator.

One common operation is renaming an entity. The `setEntityName`
function can be used to set the name of any entity (table, view,
etc). The `modifyEntityName` function can be used to derive a new name
based on the default-assigned beam one.

Another common operation is renaming table fields. You can use the
`modifyTableFields` modification for this. Simply pass a
`tableModification` where each record contains the new name of the
field.

For example, to rename the `persons` table as `people` in the database above,

```haskell
exampleDb :: DatabaseSettings be ExampleDb
exampleDb = defaultDbSettings `withDbModification`
            dbModification {
              persons = setEntityName "people"
            }
```

Or, to keep the `persons` table named as it is, but change the name of
the `personEmail` field from `"email"` to `"email_address"`

```haskell
exampleDb :: DatabaseSettings be ExampleDb
exampleDb = defaultDbSettings `withDbModification`
            dbModification {
              persons = modifyTableFields
                          tableModification {
                            personEmail = fieldNamed "email_address"
                          }
            }
```

To do both,

```haskell
exampleDb :: DatabaseSettings be ExampleDb
exampleDb = defaultDbSettings `withDbModification`
            dbModification {
              persons = setEntityName "people" <>
                        modifyTableFields
                          tableModification {
                            personEmail = fieldNamed "email_address"
                          }
            }
```

An appropriate `IsString` instance is also given so you can avoid the use of
`fieldNamed`. For example, the above is equivalent to

```haskell
exampleDb :: DatabaseSettings be ExampleDb
exampleDb = defaultDbSettings `withDbModification`
            dbModification {
              persons = setEntityName "people" <>
                        modifyTableFields
                          tableModification {
                            personEmail = "email_address"
                          }
            }
```
