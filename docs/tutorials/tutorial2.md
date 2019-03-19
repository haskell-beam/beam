Introduction
=======

In the last part, we created a simple database with one table. We then used the
beam interface to add entities into that table and query them. In this tutorial,
we'll see how to update and delete rows and how to establish and query relations
between tables.

We'll then delve deeper into queries to see how to create queries that return
multiple tables.

Adding a related table
=======

The users in our simple e-commerce application would like to ship orders to
their homes. Let's build an addresses model to allow users to add home addresses
to their profile. Our table will store United States addresses for now. An
address in the United States consists of

  * an auto-incrementing primary key
  * one required house number and street line
  * an optional apartment/suite number line
  * a required city
  * a required 2-letter state/territory code
  * one 5-digit ZIP code

Let's build the `AddressT` table. `AddressT` will follow a similar formula to
`UserT`, but it will contain a reference to a `UserT` table. ]

```haskell
data AddressT f = Address
                { _addressId    :: C f Int
                , _addressLine1 :: C f Text
                , _addressLine2 :: C f (Maybe Text)
                , _addressCity  :: C f Text
                , _addressState :: C f Text
                , _addressZip   :: C f Text

                , _addressForUser :: PrimaryKey UserT f }
                  deriving (Generic, Beamable)
type Address = AddressT Identity
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = AddressId . _addressId
type AddressId = PrimaryKey AddressT Identity -- For convenience
```

!!! tip "Tip"
    Above, we used the `C` constructor instead of `Columnar` for each column.
    `C` is a type synonym for `Columnar`, and some find it reduces the syntactic
    overhead of model declaration.

Notice that `_addressForUser` is declared as a `PrimaryKey UserT f`. This pulls
in all the columns necessary for referencing a `UserT` [^1]. Later, we'll also
see how beam can use the field to automatically create JOINs.

Notice also that `_addressId` corresponds to our auto-increminting primary key
field. In general, beam doesn't care if the underlying field is assigned
automatically, only about the type of final values of that field.

We have all the tables we need now, so let's go ahead and redefine our newest
database type.

```haskell
data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f (TableEntity UserT)
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT) }
                        deriving (Generic, Database be)
```

Modifying the default naming choices
---------

In the last part of the tutorial, we let beam decide our field names for us.
This is great for simple cases. However, sometimes you want more control over
the naming options.

!!! note "Note"
    Previous versions of this tutorial had instructions on changing the schema
    type of particular tables. This functionality has been moved from
    `beam-core` into the `beam-migrate` package. See
    the [migrations guide](../schema-guide/migrations.md) for more information.


The `defaultDbSettings` function generates names using the Haskell record
selector names [^2]. This function returns the `DatabaseType` parameterized over
`DatabaseEntity`, which is a type that contains metadata about entity names. We
can *modify* this description after it is created by using the
`withDbModification` function. You can think of `withDbModification` as applying
a transformation function to each name in our database.

Most of the time `withDbModification` needs a full description of the database
names. However, most of the time we only want to rename certain columns or
tables. We can use the `dbModification` value to construct a modification that
doesn't change any names. We can then use the Haskell record update syntax to
update field and column names. This is best illustrated by an example.

Recall our Haskell data types above.

```haskell
data UserT f
    = User
    { _userEmail     :: Columnar f Text
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving Generic

data AddressT f = Address
                { _addressId    :: C f Int
                , _addressLine1 :: C f Text
                , _addressLine2 :: C f (Maybe Text)
                , _addressCity  :: C f Text
                , _addressState :: C f Text
                , _addressZip   :: C f Text

                , _addressForUser :: PrimaryKey UserT f }
                  deriving Generic

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f (TableEntity UserT)
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT) }
                        deriving Generic
```

Now, let's say we want beam to use the name `addresses` to access the
`_shoppingCartUserAddresses` table, and the names `address1` and `address2` to
access `_addressLine1` and `_addressLine2` respectively.

```haskell
shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification`
                 dbModification {
                   _shoppingCartUserAddresses =
                     setEntityName "addresses" <>
                     modifyTableFields
                       tableModification {
                         _addressLine1 = fieldNamed "address1",
                         _addressLine2 = fieldNamed "address2"
                       }
                 }
```

Above, we use `dbModification` to produce a default modification, then
we override the `_shoppingCartUserAddresses` modification to change
the addresses table. We modify the table in two ways. First, we use
the `setEntityName` function to change the name of the table. Then, we
use `modifyTableFields` to change the names of each field. The
modifications can be combined with the semigroup operator `(<>)`.

We only override the `_addressLine1` and `_addressLine2` modifications with
`fieldNamed "address1"` and `fieldNamed "address2"`. Because `tableModification`
produces a default modification, the other columns are kept at their default
value.

!!! tip "Tip"
    The `OverloadedStrings` extension lets us avoid typing `fieldNamed`. For example, instead of

    `_addressLine1 = fieldNamed "address1"`

    we could have written

    `_addressLine1 = "address1"`

If you didn't need to modify any of the field names, you can omit
`modifyTableFields`. For example, to simply produce a database with
the first table named `users` and the second named `user_addresses`,
you can do

```haskell
shoppingCartDb1 :: DatabaseSettings be ShoppingCartDb
shoppingCartDb1 = defaultDbSettings `withDbModification`
                  dbModification {
                    _shoppingCartUsers = setEntityName "users",
                    _shoppingCartUserAddresses = setEntityName "user_addresses"
                  }
```

For the purposes of this tutorial, we'll stick with `shoppingCartDb`.

Easier queries with lenses
=====

In the previous part, we accessed table columns by using regular Haskell record
syntax. Sometimes, we would like to use the more convenient lens syntax to
access columns. Of course, all of beam's definitions are compatible with the
`lens` library -- that is to say, `makeLenses` will work just fine. However,
beam's motivation is, in part, the avoidance of Template Haskell, and it would
hardly be worth it if you had to include a Template Haskell splice just to have
lenses for the models you declared TH free.

In reality, the `lens` library isn't required to construct valid lenses. Lenses
are a plain old Haskell type.

We can use beam's `Columnar` mechanism to automatically derive lenses. The
`tableLenses` function produces a table value where each column is given a type
`LensFor`, which is a `newtype` wrapper over a correctly constructed,
polymorphic Van Laarhoven lens.

We can bring these lenses into scope globally via a global pattern match against
`tableLenses`. For example, to get lenses for each column of the `AddressT` and
`UserT` table.

```haskell
-- Add the following to the top of the file, for GHC >8.2
{-#  LANGUAGE ImpredicativeTypes #-}

Address (LensFor addressId)    (LensFor addressLine1)
        (LensFor addressLine2) (LensFor addressCity)
        (LensFor addressState) (LensFor addressZip)
        (UserId (LensFor addressForUserId)) =
        tableLenses

User (LensFor userEmail)    (LensFor userFirstName)
     (LensFor userLastName) (LensFor userPassword) =
     tableLenses
```

!!! note "Note"
    The `ImpredicativeTypes` language extension is necessary for newer
    GHC to allow the polymorphically typed lenses to be introduced at
    the top-level. Older GHCs were more lenient.

As in tables, we can generate lenses for databases via the `dbLenses` function.

```haskell
ShoppingCartDb (TableLens shoppingCartUsers)
               (TableLens shoppingCartUserAddresses) =
               dbLenses
```

We can ask GHCi for the type of a column lens.

```
Prelude Database.Beam Database.Beam.Sqlite Data.Text Database.SQLite.Simple> :t addressId
addressId
  :: Functor f2 =>
     (Columnar f1 Int -> f2 (Columnar f1 Int))
     -> AddressT f1 -> f2 (AddressT f1)
```

This lens is compatible with those of the `lens` library.

And a table lens, for good measure

```
Prelude Database.Beam Database.Beam.Sqlite Data.Text Database.SQLite.Simple> :t shoppingCartUsers
shoppingCartUsers
  :: Functor f1 =>
     (f2 (TableEntity UserT) -> f1 (f2 (TableEntity UserT)))
     -> ShoppingCartDb f2 -> f1 (ShoppingCartDb f2)
```

!!! warning "Warning"
    These lens generating functions are *awesome* but if you use them in a
    compiled Haskell module (rather than GHC), GHC may give you odd compile
    errors about ambiguous types. These occur due to what's known as the
    monomorphism restriction. You can turn it off using the
    `NoMonomorphismRestriction` extension.

    The monomorphism restriction is part of the Haskell standard, but there has
    been talk about removing it in future language versions. Basically, it
    requires GHC to not automatically infer polymorphic types for global
    definitions. In this case though, polymorphic global definitions is exactly
    what we want.

Working with relations
========

Now, let's see how we can add related addresses to our database. We begin by
opening up a connection for us to use in the rest of the tutorial.

First, let's open a new database and create the schema.

```console
$ sqlite3 shoppingcart2.db
SQLite version 3.14.0 2016-07-26 15:17:14
Enter ".help" for usage hints.
sqlite> CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));
sqlite> CREATE TABLE addresses ( id INTEGER PRIMARY KEY, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );
```

Now, in GHCi, we can use `sqlite-simple` to get a handle to this database.

```haskell
conn <- open "shoppingcart2.db"
```

Before we add addresses, we need to add some users that we can reference.

```haskell
let james = User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
    betty = User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
    sam = User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"
runBeamSqliteDebug putStrLn conn $ runInsert $
  insert (_shoppingCartUsers shoppingCartDb) $
  insertValues [ james, betty, sam ]
```

Now that we have some `User` objects, we can create associated addresses. Notice
that above, we used `insertValues` to insert concrete `User` rows. This worked
because we could determine every field of `User` before insertion. `Address`es
however have a pesky auto-incrementing primary key field. We can get around this
by inserting *expressions* instead of *values*. We can use `default_` to stand
for a value that the database needs to fill in. We can use `val_` to lift a
literal value into an expression.

With that in mind, let's give James one address, Betty two addresses, and Sam none.

```haskell
let addresses = [ Address default_ (val_ "123 Little Street") (val_ Nothing) (val_ "Boston") (val_ "MA") (val_ "12345") (pk james)
                , Address default_ (val_ "222 Main Street") (val_ (Just "Ste 1")) (val_ "Houston") (val_ "TX") (val_ "8888") (pk betty)
                , Address default_ (val_ "9999 Residence Ave") (val_ Nothing) (val_ "Sugarland") (val_ "TX") (val_ "8989") (pk betty) ]

runBeamSqliteDebug putStrLn conn $ runInsert $
  insert (_shoppingCartUserAddresses shoppingCartDb) $
  insertExpressions addresses
```

Notice that we used the `pk` function to assign the reference to the `UserT`
table. `pk` is a synonym of the `primaryKey` function from the `Table` type
class. It should be clear what's going on, but if it's not, let's ask GHCi.

```console
*NextSteps> pk (james :: User)p
UserId "james@example.com"
```

If we query for all the addresses, we'll see that SQLite has assigned them an
appropriate id.

First, let's use the new lenses we made. Make sure to import `Lens.Micro` or
`Control.Lens` or whichever (van Laarhoven) lens module you prefer.

!beam-query
```haskell
!employee2sql sql
!employee2out output
-- import Lens.Micro
-- import Control.Lens
addresses <- runBeamSqliteDebug putStrLn conn $
             runSelectReturningList $
             select (all_ (shoppingCartDb ^. shoppingCartUserAddresses))
mapM_ print addresses
```

A note about queries
=======

In the last tutorial, we saw how queries and list supported similar interfaces.
Namely we saw how `limit_` is like `take`, `offset_` like `drop`, `orderBy` like
an enhanced `sortBy`, and `aggregate` like an enhanced `groupBy`. These
corresponded to the `LIMIT`, `OFFSET`, `ORDER BY`, and `GROUP BY` SQL
constructs. The missing SQL operation in this list is the `JOIN`, which computes
the cartesian product of two tables. In other words, a join between table `A`
and table `B` results in a query of pairs `(x, y)` for every `x` in `A` and
every `y` in `B`. SQL joins can result in two-way, three-way, four-way, etc.
cartesian products.

Those familiar with lists in Haskell will note that there is an easy abstraction
for taking *n*-ary cartesian products over lists: monads.

The list monad
--------

We can use GHCi to see what we mean.

```haskell
*NextSteps> do { x <- [1,2,3]; y <- [4,5,6]; return (x, y); }
[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
```

We get the two-way cartesian product of `[1,2,3]` and `[4,5,6]`. We can make the
product arbitrarily long.

```haskell
*NextSteps> do { w <- [10, 20, 30]; x <- [1,2,3]; y <- [4,5,6]; z <- [100, 200, 1]; return (x, y, z, w); }
[(1,4,100,10),(1,4,200,10),(1,4,1,10),(1,5,100,10),(1,5,200,10),(1,5,1,10), ... ]
```

We can also use `guard` from `Control.Monad` to limit the combinations that the
list monad puts together. For example, if we had the lists

```haskell
let usersList = [(1, "james"), (2, "betty"), (3, "tom")]
    addressesList = [(1, "address1"), (1, "address2"), (3, "address3")]
```

We can use `guard` to return all pairs of elements from `usersList` and
`addressesList` that matched on their first element. For example,

```haskell
*NextSteps> do { user <- usersList; address <- addressesList; guard (fst user == fst address); return (user, address) }
[((1,"james"),(1,"address1")),((1,"james"),(1,"address2")),((3,"tom"),(3,"address3"))]
```

The query monad
-----

As I claimed in the first tutorial, queries support many of the same interfaces and operations lists
do. It follows that queries also expose a monadic interface.

For example, to retrieve every pair of user and address, we can write the following query:

!beam-query
```haskell
!employee2sql sql
!employee2out output
allPairs <- runBeamSqliteDebug putStrLn conn $
            runSelectReturningList $ select $ do
              user <- all_ (shoppingCartDb ^. shoppingCartUsers)
              address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
              return (user, address)

mapM_ print allPairs
```

Just like with lists we can also use a construct similar to guard to ensure that
we only retrieve users and addresses that are related. The `guard_` function
takes in expression of type `QExpr s Bool` which represents a SQL expression
that returns a boolean. `QExpr s Bool`s support all the common operators we have
on regular `Bool`, except they're suffixed with a `.`. For example, where you'd
use `(&&)` on two Haskell-level `Bool`s, we'd use `(&&.)` on `QExpr`-level
bools.

!beam-query
```haskell
!employee2sql sql
!employee2out output
usersAndRelatedAddresses <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $ select $
    do user <- all_ (shoppingCartDb ^. shoppingCartUsers)
       address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
       guard_ (address ^. addressForUserId ==. user ^. userEmail)
       pure (user, address)

mapM_ print usersAndRelatedAddresses
```

Of course this is kind of messy because it involves manually matching the
primary key of `User` with the reference in `Address`. Alternatively, we can use
the `references_` predicate to have beam automatically generate a `QExpr`
expression that can match primary keys together.

!beam-query
```haskell
!employee2sql sql
!employee2out output
usersAndRelatedAddressesUsingReferences <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $ select $
    do user <- all_ (shoppingCartDb ^. shoppingCartUsers)
       address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)

       guard_ (_addressForUser address `references_` user)
       pure (user, address)

mapM_ print usersAndRelatedAddressesUsingReferences
```

You may have noticed that the joins up until now did not include a SQL `ON`
clause. Instead we joined the tables together, and then used the `WHERE` clause
to filter out results we don't want. If you'd like to use the `ON` clause to
make the SQL clearer or save a line in your code, beam offers the `related_`
combinator to pull related tables directly into the query monad.

!beam-query
```haskell
!employee2sql sql
!employee2out output
usersAndRelatedAddressesUsingRelated <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $ select $
    do address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
       user <- related_ (shoppingCartDb ^. shoppingCartUsers) (_addressForUser address)
       pure (user, address)

mapM_ print usersAndRelatedAddressesUsingRelated
```

We can also query the addresses for a particular user given a `UserId`.

!beam-query
```haskell
!employee2sql sql
!employee2out output
-- This is a contrived example to show how we can use an arbitrary UserId to fetch a particular user.
-- We don't always have access to the full 'User' lying around. For example we may be in a function that
-- only accepts 'UserId's.

let bettyId = UserId "betty@example.com" :: UserId

bettysAddresses <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $ select $
    do address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
       guard_ (_addressForUser address ==. val_ bettyId)
       pure address

mapM_ print bettysAddresses
```

!!! tip "Tip"
    More complicated joins are also supported. See the section
    on [relationships](../user-guide/queries/relationships.md)

Updates and deletions
=======

So far we've only seen how to insert data and query it. There are two other SQL
operations that we have not covered: updates and deletions. Beam has full
support for these manipulations as well.

Updates
-------

Like `INSERT` and `SELECT`, to run an `UPDATE` command, we use the `runUpdate`
function, with a value of `SqlUpdate`.

The `save` function constructs a value of `SqlUpdate` given a full record. It
will generate an `UPDATE` that will set every field (except for the primary key
fields) for the row that completely matches the primary key.

Let's first look at updating passwords given a `User`. For this we can use the
`saveTo` function. Suppose James wants to change his password to the md5 hash of
"supersecure", which is `52a516ca6df436828d9c0d26e31ef704`. We have a `User`
object representing James so we can simply call `saveTo` on the update value to
update the corresponding record in the database.

!beam-query
```haskell
!employee2sql sql
!employee2out output

[james] <-
  runBeamSqliteDebug putStrLn conn $
    do runUpdate $
         save (shoppingCartDb ^. shoppingCartUsers) (james { _userPassword = "52a516ca6df436828d9c0d26e31ef704" })

       runSelectReturningList $
         lookup_ (shoppingCartDb ^. shoppingCartUsers) (UserId "james@example.com")

putStrLn ("James's new password is " ++ show (james ^. userPassword))
```

!!! tip "Tip"
    `lookup_` (defined in `Database.Beam.Query`) can be used to easily lookup a
    single entity given a table entity in a database and a primary key.

This works great, but `save` requires that we have the whole `User` object at
our disposal. Additionally, you'll notice that it causes every field to be set
in the `UPDATE` query. Typically, this doesn't matter, but sometimes we'd like
to update fewer fields, multiple rows, or use criteria other than a primary key
match. The `update` function offers finer-grained control over the command
submitted to the database.

To illustrate use of this function, let's suppose the city of "Sugarland, TX"
was renamed "Sugarville, TX" and had its ZIP code changed to be "12345"
citywide. The following beam command will update all addresses in the old city
to use the new name and ZIP code.

!beam-query
```haskell
!employee2sql sql
!employee2out output

addresses <-
  runBeamSqliteDebug putStrLn conn $
    do runUpdate $
         update (shoppingCartDb ^. shoppingCartUserAddresses)
                (\address -> mconcat
                             [ address ^. addressCity <-. val_ "Sugarville"
                             , address ^. addressZip <-. val_ "12345" ])
                (\address -> address ^. addressCity ==. val_ "Sugarland" &&.
                             address ^. addressState ==. val_ "TX")

       runSelectReturningList $ select $ all_ (shoppingCartDb ^. shoppingCartUserAddresses)

mapM_ print addresses
```

Deletions
---------

Now suppose that Betty has decided to give up her place in Houston. We can use
`runDelete` to run a `DELETE` command.

!beam-query
```haskell
!employee2sql sql

runBeamSqliteDebug putStrLn conn $
  runDelete $
  delete (shoppingCartDb ^. shoppingCartUserAddresses)
         (\address -> address ^. addressCity ==. "Houston" &&.
                      _addressForUser address `references_` betty)
```

Conclusion
=======

In this tutorial we created our first beam relationship. We saw how to use the
modifications system to override the default names given to database entities.
We saw how to use `tableLenses` to generate lenses that can be used with any
lens library. We used the monadic query interface to write queries that used SQL
joins, and we saw how beam makes it easy to automatically pull related tables
into our queries. Finally we introduced the `runUpdate` and `runDelete`
functions and demonstrated several ways to construct UPDATEs and DELETEs.

At this point, we've covered enough of the beam interface to start writing
interesting programs. Take some time to explore beam and create your own
databases. Afterwards, read on for the last part of the tutorial.

[^1]:
   Actually, any `Beamable` type can be wholly embedded in another. See the
   section on models in the [user guide](../user-guide/models.md) for more
   information.
[^2]:
   The [models guide](../user-guide/models.md) explains the exact mechanisms
   used
