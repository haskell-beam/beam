Introduction
============

In this tutorial sequence, I'll walk through creating a schema for a simple shopping cart database.

In this installment, we'll start by defining a user table. Then, I'll show how beam makes it easy to
manipulate data in our database.  Finally, I'll demonstrate how beam lets us declare type-safe and
composable queries.

One bit of administrivia: Although this file is literate Haskell and can be compiled directly by
GHC, I'll sometimes include output from a GHCi session with the appropriate declarations. If you'd
like, feel free to follow along in GHCi.

Beam Module Structure
=======

Beam makes extensive use of GHC's Generics mechanism. This extension means beam does not need to
rely on template haskell.

>{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, TypeFamilies, DeriveGeneric, OverloadedStrings #-}
> module Main where

To start defining beam schemas and queries, you only need to import the `Database.Beam`.
To interface with an actual database, you'll need to import one of the database backends.
Beam ships with a default sqlite3 backend in the standard package.

> import Database.Beam
> import Database.Beam.Backend.Sqlite3
>
> import Data.Text (Text)

Defining our first table
========

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

> data UserT f = User
>              { _userEmail     :: Columnar f Text
>              , _userFirstName :: Columnar f Text
>              , _userLastName  :: Columnar f Text
>              , _userPassword  :: Columnar f Text }
>               deriving Generic

This data type might look very complicated, so I'd like to show you that it's not that scary. Let's
see if we can use GHCi to help us.

< *BasicTutorial> :t User
< User
< :: Columnar f Text
<    -> Columnar f Text -> Columnar f Text -> Columnar f Text -> UserT f

Hmm... That did not help much. However, consider the type of the following:

< *BasicTutorial> :t (\email firstName lastName password -> User email firstName lastName password :: UserT Identity)
< (\email firstName lastName password -> User email firstName lastName password :: UserT Identity)
<  :: Text -> Text -> Text -> Text -> UserT Identity

Woah! That looks a lot like what we'd expect if we had declared the type in the "regular" Haskell way:

< data User = User
<           { _userEmail     :: Text
<           , _userFirstName :: Text
<           , _userLastName  :: Text
<           , _userPassword  :: Text }

This functionality is due to the fact that `Columnar` is a type family defined such that for any
`x`, `Columnar Identity x = x`. Knowing this, let's define a type synonym to make our life easier.

> type User = UserT Identity

Now you can see why we named the type of the table `UserT` and its constructor `User`. This allows
us to use the "regular" `User` constructor to construct values of type `User`. We can use the
`StandaloneDeriving` extension to derive an instance of `Show` for the 'regular' datatype.

> deriving instance Show User

Teaching Beam about our table
=========

We've defined a type that can represent our the data in our table. Now, let's inform beam that we'd like to
use `UserT' as a table.

> instance Beamable UserT
> instance Beamable (PrimaryKey UserT)
> instance Table UserT where

All beam tables need to implement the `Table` type class. The `Table` type class contains functions
for marshalling values between SQL and Haskell and for allowing the table to be queried. Due to
GHC's DeriveGeneric and DefaultSignatures extensions, all these methods can be written for us by the
compiler at compile-time!

The only thing we need to provide is the type of the primary keys for users, and a function that can
extract the primary key from any `UserT f` object. To do this, we just add the following lines to
the instance declaration.

>     data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
>     primaryKey = UserId . _userEmail

It would be nice to have a type synonym for `PrimaryKey UserT f`, so we make one now. We can also
derive an instance of `Show`.

> type UserId = PrimaryKey UserT Identity
> deriving instance Show UserId


Defining our database
========

Now that we have our table, we're going to define a type to hold information about our
database. Defining our database is going to follow the same pattern as defining a table. We'll
define a higher-kinded datatype and then declare an instance of `Database`, and let the compiler
figure most of it out.

> data ShoppingCartDb f = ShoppingCartDb
>                       { _shoppingCartUsers :: f UserT }
>                         deriving Generic
>
> instance Database ShoppingCartDb
>

The next step is to create a description of the particular database we'd like to create. This
involves giving each of the tables in our database a name. If you've named all your database
selectors using camel case, beam can automatically figure out what all the table names should be. If
you haven't, you would have to manually name your tables. Since we followed the convention, we can
let beam do all the hard work.

> shoppingCartDb :: DatabaseSettings Sqlite3Settings ShoppingCartDb
> shoppingCartDb = autoDbSettings
>

Adding users to our database
========

Let's add some users to our database. First, we'll open a connection to a SQLite3 database using
beam.  The `openDatabase` function will open the database and optionally attempt to make the
schema of the opened database match the schema implied by the data types. Below we use the
`openDatabaseDebug` function to put beam into debug mode. This will cause beam to log every sql
statement executed. `openDatabaseDebug` and `openDatabase` have the same type signature, so they can
be used interchangeably. In the call below, we pass in `AutoMigrate` to have beam automatically
attempt to match up the real database schema with what it would expect based on the Haskell types.

> main :: IO ()
> main = do beam <- openDatabaseDebug shoppingCartDb AutoMigrate (Sqlite3Settings "shoppingcart1.db")
>

To make sure that beam correctly inferred the database schema, let's dump it.

>
>           dumpSchema shoppingCartDb
>

This will dump the SQL CREATE TABLE statements to the console.

< Dumping database schema ...
< CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ))

Beam automatically converted our `UserT` table in the `_shoppingCartUsers` selector to a table named
`cart_users`. As we stated above, this comes from un-CamelCase-ing the selector name and dropping
the first component. Beam followed the same rule to derive the column names.

Now let's add a few users. We'll give each user an MD5 encoded password too.

>
>           beamTxn beam $ \(ShoppingCartDb usersT) ->
>            do insertInto usersT (User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -})
>               insertInto usersT (User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -})
>               insertInto usersT (User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -})

The `insertInto` function inserts a value into a table that can hold that type.

Because we're in debug mode, we'll see the SQL that beam is running:

< Will execute INSERT INTO cart_users VALUES (?, ?, ?, ?) with [SqlString "james@example.com",SqlString "James",SqlString "Smith",SqlString "b4cc344d25a2efe540adbf2678e2304c"]
< Will execute INSERT INTO cart_users VALUES (?, ?, ?, ?) with [SqlString "betty@example.com",SqlString "Betty",SqlString "Jones",SqlString "82b054bd83ffad9b6cf8bdb98ce3cc2f"]
< Will execute INSERT INTO cart_users VALUES (?, ?, ?, ?) with [SqlString "sam@example.com",SqlString "Sam",SqlString "Taylor",SqlString "332532dcfaa1cbf61e2a266bd723612c"]

Querying the database
=======

Now let's write some queries for the database. You can think of queries in Beam as similar to lists.
Like lists you can filter them (SQL WHERE clauses), take only a certain number of items from them
(SQL LIMIT clauses), and drop a certain number of items from them (SQL OFFSET clauses).

Let's look at a simple example. Let's get all users.

>           putStrLn "all users: "
>           Success allUsers <- beamTxn beam $ \(ShoppingCartDb usersT) ->
>                               queryList (all_ usersT)
>           mapM_ (putStrLn . show) allUsers
>           putStrLn "----\n\n"

When run, you should get output like the following:

< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password` FROM  cart_users AS t0 with []
< User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"}
< User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"}
< User {_userEmail = "sam@example.com", _userFirstName = "Sam", _userLastName = "Taylor", _userPassword = "332532dcfaa1cbf61e2a266bd723612c"}

Next let's suppose you wanted to sort the users into order by their first name. We can use the
`orderBy` function to order the query results. This is similar to the `sortBy` function for lists.

>           putStrLn "users sorted by first name:"
>           Success sortedUsersByFirstName <- beamTxn beam $ \(ShoppingCartDb usersT) ->
>                                             queryList $ orderBy (asc_ . _userFirstName) (all_ usersT)
>           mapM_ (putStrLn . show) sortedUsersByFirstName
>           putStrLn "----\n\n"

The corresponding output:

< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password` FROM  cart_users AS t0 ORDER BY `t0`.`first_name` ASC with []
< User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"}
< User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"}
< User {_userEmail = "sam@example.com", _userFirstName = "Sam", _userLastName = "Taylor", _userPassword = "332532dcfaa1cbf61e2a266bd723612c"}

We can also sort by more than one column.

>           putStrLn "users sorted by first and last name:"
>           Success sortedUsersByFirstAndLastName <- beamTxn beam $ \(ShoppingCartDb usersT) ->
>                                                    queryList $ orderBy (\user -> (asc_ (_userFirstName user), asc_ (_userLastName user))) (all_ usersT)
>           mapM_ (putStrLn . show) sortedUsersByFirstAndLastName
>           putStrLn "----\n\n"

Again, the output for convenience:

< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password` FROM  cart_users AS t0 ORDER BY `t0`.`first_name` ASC, `t0`.`last_name` ASC with []
< User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"}
< User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"}
< User {_userEmail = "sam@example.com", _userFirstName = "Sam", _userLastName = "Taylor", _userPassword = "332532dcfaa1cbf61e2a266bd723612c"}

We can use `limit_` and `offset_` in a similar manner to `take` and `drop` respectively.

>           putStrLn "the second user based on their first names"
>           Success [secondUser] <- beamTxn beam $ \(ShoppingCartDb usersT) ->
>                                   queryList $ limit_ 1 (offset_ 1 (orderBy (asc_ . _userFirstName) (all_ usersT)))
>           putStrLn (show secondUser)
>           putStrLn "----\n\n"

And the output:

< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password` FROM  cart_users AS t0 ORDER BY `t0`.`first_name` ASC LIMIT 1 OFFSET 1 with []
< User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"}

Aggregations
==========

Sometimes we also want to group our data together and perform calculations over the groups of data. SQL calls these aggregations.

The simplest aggregation is counting. We use the `aggregate` function to create aggregations.

>           putStrLn "The total number of users"
>           Success [userCount] <- beamTxn beam $ \(ShoppingCartDb usersT) ->
>                                  queryList $ aggregate (\user -> count_ (_userEmail user)) (all_ usersT)
>           putStrLn (show userCount)
>           putStrLn "----\n\n"

Maybe we'd like something a little more interesting, such as the number of users for each unique
first name. We can also express these aggregations using the `aggregate` function. In order to get
interesting results, we'll need to add more users to our database.

>           beamTxn beam $ \(ShoppingCartDb usersT) ->
>            do insertInto usersT (User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -})
>               insertInto usersT (User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -})
>               insertInto usersT (User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -})
>               insertInto usersT (User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -})
>               insertInto usersT (User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -})

Now we can use `aggregate` to both group by a user's first name, and then count the number of users.

>           putStrLn "The number of users for each name"
>           Success countsByFirstName <- beamTxn beam $ \(ShoppingCartDb usersT) ->
>                                        queryList $
>                                        aggregate (\user -> (group_ (_userFirstName user), count_ (_userEmail user))) $
>                                        all_ usersT
>           mapM (putStrLn . show) countsByFirstName
>           putStrLn "----\n\n"

Beam produces a very reasonable looking SQL statement, and returns the correct results.

< The number of users for each name
< Will execute SELECT `t0`.`first_name`, COUNT(`t0`.`email`) FROM  cart_users AS t0 GROUP BY `t0`.`first_name` with []
< ("Betty",2)
< ("James",3)
< ("Sam",3)
< ----

Conclusion
=======

In this tutorial, we've covered creating a database schema, opening up a beam database, inserting
values into the database, and querying values from them. We used the knowledge we learned to create
a partial shopping cart database that contains information about users. In the next tutorial, we'll
delve deeper into the some of the query types and show how we can create relations between
tables. We'll also use the monadic query interface to create SQL joins.

Until next time! In the mean-time, feel free to send questions to travis@athougies.net.

(Update) Checkout 02-NextSteps.lhs for the next tutorial in the sequence.
