In this tutorial sequence, we'll walk through creating a schema for a simple
shopping cart database. We'll start by defining a user table. Then, we'll show
how beam makes it easy to manipulate data in our database. Finally, we'll
demonstrate how beam lets us declare type-safe and composable queries.

## Beam Module Structure

Beam makes extensive use of GHC's Generics mechanism. This extension means beam does not need to
rely on template haskell.

To start defining beam schemas and queries, you only need to import the
`Database.Beam` module. To interface with an actual database, you'll need to
import one of the database backends. We'll see how to use the Sqlite backend
here (found in the `beam-sqlite` package). Now, open up a GHCi prompt for us to
use. Make sure to get the `beam-core`, `beam-sqlite` and `text` packages.

```console
$ stack repl --package beam-core --package beam-sqlite --package sqlite-simple --package beam-migrate --package text
```

This will put you into a GHCi prompt with the `beam-core` and `beam-sqlite`
packages available. We also include the `sqlite-simple` package. Beam mainly
manages querying and data marshalling. Connections to the backends are done via
backend specific packages. In this case, `beam-sqlite` uses the `sqlite-simple`
backend.

Before starting, we'll need to enable some extensions.

```
> :set -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XDeriveAnyClass
```

And import some modules...

```haskell
import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)
```

## Defining our first table

Beam tables are regular Haskell data types with a bit of scaffolding. Thankfully, the magic of the
modern Haskell type system allows us to remove the overhead and the syntactic fuzz of the
scaffolding in most situations.

We start by declaring a data structure named `UserT`. As a matter of convention, Beam table types
are suffixed with 'T'. Table types have only one constructor. Again, as a matter of convention, the
constructor has the same name as the table, but without the 'T' suffix. We'll soon see the reason
for this convention.

In this tutorial, I'll prefix all record selectors with an underscore. This is a matter of personal
preference. One reason for the prefix is that it plays nicely with the `lens` library. Beam does not
necessitate the use of `lens` (in fact Beam includes its own mechanism to generically derive van
Laarhoven lenses), but I recognize that some programmers use `lens` quite a lot.

```haskell
data UserT f
    = User
    { _userEmail     :: Columnar f Text
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving Generic
```

This data type might look very complicated, so I'd like to show you that it's not that scary. Let's
see if we can use GHCi to help us.

```
Prelude Database.Beam.Sqlite Database.Beam Data.Text> :t User
User
  :: Columnar f Text
     -> Columnar f Text -> Columnar f Text -> Columnar f Text -> UserT f
```

Hmm... That did not help much. Let's see what happens if we bind `f` to something concrete, like `Identity`. Using the `TypeApplications` extension:

```
Prelude Database.Beam Database.Beam.Sqlite Data.Text> :t (User @Identity)
(User @Identity) :: Text -> Text -> Text -> Text -> UserT Identity
```

Woah! That looks a lot like what we'd expect if we had declared the type in the "regular" Haskell way:

```haskell
data User = User
          { _userEmail     :: Text
          , _userFirstName :: Text
          , _userLastName  :: Text
          , _userPassword  :: Text }
```

This functionality is due to the fact that `Columnar` is a type family defined
such that for any `x`, `Columnar Identity x = x`. This strategy is known as
*defunctionalization* [^1] or *higher-kinded data types* [^4].

Knowing this, let's define a type synonym to make our life easier.

```haskell
type User = UserT Identity
type UserId = PrimaryKey UserT Identity
```

Now you can see why we named the type of the table `UserT` and its constructor
`User`. This allows us to use the "regular" `User` constructor to construct
values of type `User`. We can use the `StandaloneDeriving` and
`TypeSynonymInstances` extensions to derive instances of `Show` and `Eq` for the
'regular' datatype.

```
> :set -XStandaloneDeriving -XTypeSynonymInstances -XMultiParamTypeClasses
```

Now we can derive `Show` and `Eq` instances.

```haskell
deriving instance Show User
deriving instance Eq User
```

Note that this does require us to use an explicit type signature where we
otherwise wouldn't. For example,

```haskell
Prelude Database.Beam.Sqlite Database.Beam Data.Text> User "john@example.com" "John" "Smith" "password!"

<interactive>:46:2: error:
    * No instance for (Show (UserT f0)) arising from a use of ‘print’
    * In a stmt of an interactive GHCi command: print it
```

Here, GHC is complaining that it cannot infer the type of the `f` parameter
based on the values we've supplied. This is because the `Columnar` type family
is non-injective. However, an explicit type annotation fixes it all up.

```
Prelude Database.Beam.Sqlite Database.Beam Data.Text> User "john@example.com" "John" "Smith" "password!" :: User
User {_userEmail = "john@example.com", _userFirstName = "John", _userLastName = "Smith", _userPassword = "password!"}
```

You can also use type applications, if you like that style better:

```
Prelude Database.Beam Database.Beam.Sqlite Data.Text> User @Identity "john@example.com" "John" "Smith" "password!"
User {_userEmail = "john@example.com", _userFirstName = "John", _userLastName = "Smith", _userPassword = "password!"}
```

Usually, you won't need to deal with this, as you'll explicitly annotate your
top-level functions to use the `User` type.

## Teaching Beam about our table

We've defined a type that can represent the data in our table. Now, let's
inform beam that we'd like to use `UserT` as a table.

All beam tables need to implement the `Beamable` type class. Due to GHC's
DeriveGeneric and DefaultSignatures extensions, all these methods can be written
for us by the compiler at compile-time!

```haskell
instance Beamable UserT
```

!!! tip "Tip"
    If you turn on the `DeriveAnyClass` feature, you can simply `derive` the `Beamable` type class. For example, the type

    ```
    data UserT f
        = User
        { _userEmail     :: Columnar f Text
        , _userFirstName :: Columnar f Text
        , _userLastName  :: Columnar f Text
        , _userPassword  :: Columnar f Text }
        deriving Generic
    ```

    could be written

    ```
    data UserT f
        = User
        { _userEmail     :: Columnar f Text
        , _userFirstName :: Columnar f Text
        , _userLastName  :: Columnar f Text
        , _userPassword  :: Columnar f Text }
        deriving (Generic, Beamable)
    ```

Additionally, all beam tables must implement the `Table` type class, which we
can use to declare a primary key.

The only thing we need to provide is the type of the primary keys for users, and
a function that can extract the primary key from any `UserT f` object. To do
this, add the following lines to the instance declaration.

```
instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UserId . _userEmail
```

The `data` declaration is similar to a toplevel data definition, construct a key
for `UserT` with the `UserId` constructor like a regular table.

```haskell
userKey = UserId "john@doe.org"
```

## Defining our database

Now that we have our table, we're going to define a type to hold information about our
database. Defining our database is going to follow the same pattern as defining a table. We'll
define a higher-kinded datatype and then declare an instance of `Database`, and let the compiler
figure most of it out.

Tables are a collection of `Columnar` values. Databases are a collection of
entities, such as tables. Many database systems can also hold other entities
(such as views, domain types, etc). Beam allows you to declare these as well [^2].

Our database consists of only one table.

```haskell
data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving (Generic, Database be)
```

!!! note "Note"
    By deriving `Database be` we actually allowed our database to be used with any Beam
    backend that supports it. We could also have explicitly listed the database backends we
    liked. For example, specifying `deriving (Generic, Database Sqlite, Database Postgres)` would
    derive instances only for SQLite or Postgres.

The next step is to create a description of the particular database we'd like to create. This
involves giving each of the tables in our database a name. If you've named all your database
selectors using camel case, beam can automatically figure out what all the table names should be. If
you haven't, or you have multiple tables holding the same type in your database, you might have to
manually name your tables. For now, we'll let beam do the hard work [^3].

```haskell
shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings
```

## Adding users to our database

Let's add some users to our database. As we said above, beam is
backend-agnostic. However, backend integration libraries are maintained in the
official beam repository. The `beam-sqlite` package offers straightforwards
integration with the `sqlite-simple` library.

First, let's create a sqlite3 database with the right schema. Open up terminal, and do

```console
$ sqlite3 shoppingcart1.db
SQLite version 3.14.0 2016-07-26 15:17:14
Enter ".help" for usage hints.
sqlite> CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));
sqlite>
```

Now, let's open the database in Haskell.

```haskell
import Database.SQLite.Simple

conn <- open "shoppingcart1.db"
```

Now let's add a few users. We'll give each user an MD5 encoded password too.
We'll use the `runBeamSqliteDebug` function (supplied by `beam-sqlite`) to
output the statements that beam would normally run. In production, you'd use the
`runBeamSqlite` function, or use the backend integration packages to directly
use the underlying backend library.

```haskell
runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
insert (_shoppingCartUsers shoppingCartDb) $
insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
             , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
             , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
```

The `runInsert` function runs an insert statement, which we construct using the
`insert` function. Since we're inserting concrete values, we use the
`insertValues` function to supply the values. We can also use the
`insertExpressions` function to insert arbitrary SQL expressions, or the
`insertFrom` to insert the results of an arbitrary select (the `INSERT INTO ..
SELECT ..` syntax).

Because we're in debug mode, we'll see the SQL that beam is running:

```
INSERT INTO "cart_users"("email", "first_name", "last_name", "password") VALUES (?, ?, ?, ?), (?, ?, ?, ?), (?, ?, ?, ?)
-- With values: [SQLText "james@example.com",SQLText "James",SQLText "Smith",SQLText "b4cc344d25a2efe540adbf2678e2304c",SQLText "betty@example.com",SQLText "Betty",SQLText "Jones",SQLText "82b054bd83ffad9b6cf8bdb98ce3cc2f",SQLText "sam@example.com",SQLText "Sam",SQLText "Taylor",SQLText "332532dcfaa1cbf61e2a266bd723612c"]
```

The `?` represent the values passed to the database (beam uses the backend's
value interpolation to avoid SQL injection attacks).

## Querying the database

Now let's write some queries for the database. Let's get all the users we just
added. Click between the tabs to see the SQL and console output generated

!beam-query
```haskell
!employee1sql sql
!employee1out output
let allUsers = all_ (_shoppingCartUsers shoppingCartDb)

runBeamSqliteDebug putStrLn conn $ do
  users <- runSelectReturningList $ select allUsers
  mapM_ (liftIO . putStrLn . show) users
```

!!! note "Note"
    The `--` at the ends of the console output lines are an artifact of the
    documentation build process. They won't appear in your console.

Next let's suppose you wanted to sort the users into order by their first name,
and then descending by their last name. We can use the `orderBy_` function to
order the query results. This is similar to the `sortBy` function for lists.


!beam-query
```haskell
!employee1sql sql
!employee1out output
let sortUsersByFirstName = orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) (all_ (_shoppingCartUsers shoppingCartDb))

runBeamSqliteDebug putStrLn conn $ do
  users <- runSelectReturningList $ select sortUsersByFirstName
  mapM_ (liftIO . putStrLn . show) users
```


We can use `limit_` and `offset_` in a similar manner to `take` and `drop` respectively.

!beam-query
```haskell
!employee1sql sql
!employee1out output
let boundedQuery = limit_ 1 $ offset_ 1 $
                   orderBy_ (asc_ . _userFirstName) $
                   all_ (_shoppingCartUsers shoppingCartDb)

runBeamSqliteDebug putStrLn conn $ do
  users <- runSelectReturningList (select boundedQuery)
  mapM_ (liftIO . putStrLn . show) users
```

## Aggregations

Sometimes we also want to group our data together and perform calculations over
the groups of data. SQL calls these aggregations.

The simplest aggregation is counting. We use the `aggregate_` function to create
aggregations. For example, to count all users, we can use the `countAll_`
aggregation. We also use the `runSelectReturningOne` function to get at most one
record from the database.

!beam-query
```haskell
!employee1sql sql
!employee1out output
let userCount = aggregate_ (\u -> as_ @Int countAll_) (all_ (_shoppingCartUsers shoppingCartDb))

runBeamSqliteDebug putStrLn conn $ do
  Just c <- runSelectReturningOne $ select userCount
  liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")
```

!!! note "Note"
    `countAll_` is happy to unmarshal into any `Integral` type, so we use `as_`
    to constrain the type to `Int`.

Maybe we'd like something a little more interesting, such as the number of users
for each unique first name. We can also express these aggregations using the
`aggregate_` function. In order to get interesting results, we'll need to add
more users to our database.

!beam-query
```haskell
!employee1sql sql
runBeamSqliteDebug putStrLn conn $
  runInsert $
  insert (_shoppingCartUsers shoppingCartDb) $
  insertValues [ User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
               , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
               , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
               , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
               , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
```

Now we can use `aggregate_` to both group by a user's first name, and then count
the number of users.


!beam-query
```haskell
!employee1sql-agg sql
!employee1out-agg output
let numberOfUsersByName = aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int countAll_)) $
                          all_ (_shoppingCartUsers shoppingCartDb)

runBeamSqliteDebug putStrLn conn $ do
  countedByName <- runSelectReturningList $ select numberOfUsersByName
  mapM_ (liftIO . putStrLn . show) countedByName
```

## Conclusion

In this tutorial, we've covered creating a database schema, opening up a beam
database, inserting values into the database, and querying values from them. We
used the knowledge we learned to create a partial shopping cart database that
contains information about users. In the next tutorial, we'll delve deeper into
the some of the query types and show how we can create relations between tables.
We'll also use the monadic query interface to create SQL joins.

Until next time!

If you have any questions about beam, feel free to send them to
travis@athougies.net . Pull requests and bug reports are welcome
on [GitHub](https://github.com/tathougies/beam).

[^1]:
   Thanks to various bloggers for pointing this out. You can read more about this technique
   [here](https://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/).

[^4]:
   https://reasonablypolymorphic.com/blog/higher-kinded-data/

[^2]:
   Adding entities other than tables is covered in more depth in
   the [user guide](../user-guide/databases.md).

[^3]:
   More on the default naming conventions can be found in
   the [models section](../user-guide/models.md) of the user guide. We'll talk
   about how to override defaults in the next sections.
