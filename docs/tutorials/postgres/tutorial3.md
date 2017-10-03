Introduction
=======

!!! warning "Warning"
    This still needs to be done!!

In teh last part, we extended our shopping cart database to let users add
multiple addresses. We saw how to establish one-to-many relations between two
tables, and how to use the monadic query interface to write SQL JOINs. In this
installment, we'll be adding support for products and orders to our database
schema. We'll see how to use an intermediary table to create many-to-many
relations and how to write LEFT JOINs. Finally, we'll see how to use `Nullable`
to create optional foreign key references.

Creating tables is easy now
=======

Let's create our products table. By now, the pattern for adding a new table to
the schema should be pretty familiar, so I'm going to skip the explanation.

```haskell
data ProductT f = Product
                { _productId          :: C f (Auto Int)
                , _productTitle       :: C f Text
                , _productDescription :: C f Text
                , _productPrice       :: C f Int {- Price in cents -} }
                  deriving Generic
type Product = ProductT Identity
deriving instance Show Product

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f (Auto Int))
                               deriving Generic
  primaryKey = ProductId . _productId

instance Beamable ProductT
instance Beamable (PrimaryKey ProductT)
```

For orders, we want to store an id, date created, and the user who made the
order. We'd also like to create an optional link to a shipping information
table. When the shipping information is created, we'll fill in the shipping
information in the order. In order to create the optional reference, we're going
to use the `Nullable` tag modifier to modify the column tag. `Nullable` will
turn all fields of type `x` into `Maybe x`. Note that we could also create this
relation by installing a primary key on the shipping info table, and this is
arguably the better option. However, we'll go with a nullable foreign key here
to show the full breadth of beam's features, and because this sort of relation
exists in many existing databases.

```haskell
import Data.Time

deriving instance Show (PrimaryKey AddressT Identity)

data OrderT f = Order
              { _orderId      :: Columnar f (Auto Int)
              , _orderDate    :: Columnar f LocalTime
              , _orderForUser :: PrimaryKey UserT f
              , _orderShipToAddress :: PrimaryKey AddressT f
              , _orderShippingInfo :: PrimaryKey ShippingInfoT (Nullable f) }
                deriving Generic
type Order = OrderT Identity
deriving instance Show Order

instance Table OrderT where
    data PrimaryKey OrderT f = OrderId (Columnar f (Auto Int))
                               deriving Generic
    primaryKey = OrderId . _orderId

instance Beamable OrderT
instance Beamable (PrimaryKey OrderT)

data ShippingCarrier = USPS | FedEx | UPS | DHL
                       deriving (Show, Read, Eq, Ord, Enum)

data ShippingInfoT f = ShippingInfo
                     { _shippingInfoId             :: Columnar f (Auto Int)
                     , _shippingInfoCarrier        :: Columnar f ShippingCarrier
                     , _shippingInfoTrackingNumber :: Columnar f Text }
                       deriving Generic
type ShippingInfo = ShippingInfoT Identity
deriving instance Show ShippingInfo

instance Table ShippingInfoT where
    data PrimaryKey ShippingInfoT f = ShippingInfoId (Columnar f (Auto Int))
                                      deriving Generic
    primaryKey = ShippingInfoId . _shippingInfoId

instance Beamable ShippingInfoT
instance Beamable (PrimaryKey ShippingInfoT)
deriving instance Show (PrimaryKey ShippingInfoT (Nullable Identity))
```

In the above example, we show how to use a custom data type as a beam column.
Recall that beam lets you store any Haskell type in a `Columnar`. However, at
some point, we will need to demonstrate to SQLite how to store values of type
`ShippingCarrier`. We will come back to this later.

We would also like to be able to associate a list of products with each order as
line items. To do this we will create a table with two foreign keys. This table
will establish a many-to-many relationship between orders and products.

```haskell
deriving instance Show (PrimaryKey OrderT Identity)
deriving instance Show (PrimaryKey ProductT Identity)

data LineItemT f = LineItem
                 { _lineItemInOrder    :: PrimaryKey OrderT f
                 , _lineItemForProduct :: PrimaryKey ProductT f
                 , _lineItemQuantity   :: Columnar f Int }
                   deriving Generic
type LineItem = LineItemT Identity
deriving instance Show LineItem

instance Table LineItemT where
    data PrimaryKey LineItemT f = LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
                                  deriving Generic
    primaryKey = LineItemId <$> _lineItemInOrder <*> _lineItemForProduct

instance Beamable LineItemT
instance Beamable (PrimaryKey LineItemT)
```

!!! tip "Tip"
    We used the `Applicative` instance for `(->) a` above to write the
    `primaryKey` function. The `Applicative ((->) a)` instance operates like an
    unwrapper `Reader` of `a`. The applicative actions are then functions from
    `a -> x` that inject values from the `a` into the applicative bind.

Now we'll add all these tables to our database.

```haskell
-- Some convenience lenses

LineItem _ _ (LensFor lineItemQuantity) = tableLenses
Product (LensFor productId) (LensFor productTitle) (LensFor productDescription) (LensFor productPrice) = tableLenses

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f (TableEntity UserT)
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT)
                      , _shoppingCartProducts      :: f (TableEntity ProductT)
                      , _shoppingCartOrders        :: f (TableEntity OrderT)
                      , _shoppingCartShippingInfos :: f (TableEntity ShippingInfoT)
                      , _shoppingCartLineItems     :: f (TableEntity LineItemT) }
                        deriving Generic

instance Database ShoppingCartDb

ShoppingCartDb (TableLens shoppingCartUsers) (TableLens shoppingCartUserAddresses)
               (TableLens shoppingCartProducts) (TableLens shoppingCartOrders)
               (TableLens shoppingCartShippingInfos) (TableLens shoppingCartLineItems) = dbLenses

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification`
                 dbModification {
                   _shoppingCartUserAddresses =
                     modifyTable (\_ -> "addresses") $
                     tableModification {
                       _addressLine1 = fieldNamed "address1",
                       _addressLine2 = fieldNamed "address2"
                     },
                   _shoppingCartProducts = modifyTable (\_ -> "products") tableModification,
                   _shoppingCartOrders = modifyTable (\_ -> "orders") $
                                         tableModification {
                                           _orderShippingInfo = ShippingInfoId "shipping_info__id"
                                         },
                   _shoppingCartShippingInfos = modifyTable (\_ -> "shipping_info") $
                                                tableModification {
                                                  _shippingInfoId = "id",
                                                  _shippingInfoCarrier = "carrier",
                                                  _shippingInfoTrackingNumber = "tracking_number"
                                                },
                   _shoppingCartLineItems = modifyTable (\_ -> "line_items") tableModification
                 }
```

Fixtures
=======

Let's put some sample data into a new database.

Create a new file called `tutorial-3-schema.sql`

```sql
create table cart_users (
  email      varchar not null primary key,
  first_name varchar not null,
  last_name  varchar not null,
  password   varchar not null
);

create table addresses (
  id              serial primary key,
  address1        varchar not null,
  address2        varchar,
  city            varchar not null,
  state           varchar not null,
  zip             varchar not null,
  for_user__email varchar not null references cart_users (email)
);

create table products (
  id          serial primary key,
  title       varchar not null,
  description varchar not null,
  price       integer not null
);

create table orders (
  id                  serial primary key,
  date                timestamp without time zone not null,
  for_user__email     varchar not null references cart_users (email),
  ship_to_address__id integer not null references addresses (id),
  shipping_info__id   integer /* this is a nullable foreign key as per the tutorial */
);

create table shipping_info (
  id              serial primary key,
  carrier         varchar not null,
  tracking_number varchar not nulL
);

create table line_items (
  item_in_order__id    integer not null references orders (id),
  item_for_product__id integer not null references products (id),
  item_quantity        integer not nulL
);
```

```console
$ createdb shoppingcart3
$ psql -d shoppingcart3 -f `tutorial-3-schema.sql`
```

Let's put some sample data into our database. Below, we will use the
`beam-postgres` functions `insertReturning` and `runInsertReturningList` to insert
rows *and* retrieve the inserted rows from the database. This will let us see
what values the auto-incremented `id` columns took on, which will allow us to
create references to these inserted rows.

Let's first insert some users -- you have seen this before in the previous
tutorials, so the explanation is skipped.

```haskell
users :: [User]
users@[james, betty, sam] = [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
                            , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
                            , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"]

insertUsers :: Connection -> IO ()
insertUsers conn =
  withDatabaseDebug putStrLn conn $ B.runInsert $
    B.insert (_shoppingCartUsers shoppingCartDb) $
    insertValues users
```

Inserting to the rest of the tables is a little bit different -- we want to see
the incremented id key after it is written to the database.  The other functions
inserting into other tables will need these returned to get the proper primary
key.

Whereas with an insert with no returning values has the syntax `B.runInsert $
B.insert <table> <values>`, inserting and returning a list of what you
inserted has the syntax `runInsertReturningList <table> <values>`.

```haskell
addresses :: [Address]
addresses = [ Address (Auto Nothing) "123 Little Street" Nothing "Boston" "MA" "12345" (pk james)
            , Address (Auto Nothing) "222 Main Street" (Just "Ste 1") "Houston" "TX" "8888" (pk betty)
            , Address (Auto Nothing) "9999 Residence Ave" Nothing "Sugarland" "TX" "8989" (pk betty)
            ]

products :: [Product]
products = [ Product (Auto Nothing) "Red Ball" "A bright red, very spherical ball" 1000
           , Product (Auto Nothing) "Math Textbook" "Contains a lot of important math theorems and formulae" 2500
           , Product (Auto Nothing) "Intro to Haskell" "Learn the best programming language in the world" 3000
           , Product (Auto Nothing) "Suitcase" "A hard durable suitcase" 15000
           ]

insertAddresses :: Connection -> IO [Address]
insertAddresses conn =
  withDatabaseDebug putStrLn conn $
    runInsertReturningList (shoppingCartDb ^. shoppingCartUserAddresses) $
    insertValues addresses

insertProducts :: Connection -> IO [Product]
insertProducts conn =
  withDatabaseDebug putStrLn conn $
    runInsertReturningList (shoppingCartDb ^. shoppingCartProducts) $
    insertValues products
```

If we want to use what the insertions return we can desructure them on the left
hand side of a do expression in our main function

```haskell
main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=shoppingcart3"
  insertUsers conn
  addresses@[jamesAddress1, bettyAddress1, bettyAddress2] <- insertAddresses conn
  products@[redBall, mathTextbook, introToHaskell, suitcase] <- insertProducts conn
  [bettyShippingInfo] <- insertShippingInfos conn
```

Now, if we take a look at one of the returned addresses, like `jamesAddress1`,
we see it has had it's `Auto` field assigned correctly.  You put a `putStrLn
jamesAddress1` you will see:

```haskell
Address {_addressId = Auto {unAuto = Just 1}, _addressLine1 = "123 Little Street", _addressLine2 = Nothing, _addressCity = "Boston", _addressState = "MA", _addressZip = "12345", _addressForUser = UserId "james@example.com"}
```

!!! note "Note"
    `insertReturning` and `runInsertReturningList` are from the `beam-postgres`
    package. They emulate the `INSERT .. RETURNING ..` functionatily you may
    expect in other databases. Because this emulation is backend-specific it is
    part of the backend package, rather than `beam-core`.
    
    Other backends may have similar functionality. Please refer to the backend
    package you're interested in for more information, as well as notes on the
    implementation.

## Marshalling a custom type

Now we can insert shipping information. Of course, the shipping information
contains the `ShippingCarrier` enumeration.

```haskell
shippingInfos :: [ShippingInfo]
shippingInfos = [ ShippingInfo (Auto Nothing) USPS "12345790ABCDEFGHI" ]

insertShippingInfos :: Connection -> IO [ShippingInfo]
insertShippingInfos conn =
 withDatabaseDebug putStrLn conn $
  runInsertReturningList (shoppingCartDb ^. shoppingCartShippingInfos) $
  insertValues shippingInfos
```

The compiler is going to yell at you when you type this function up.

```
error:
• No instance for (FromBackendRow Postgres ShippingCarrier)
    arising from a use of ‘runInsertReturningList’
• In the expression:
    runInsertReturningList
      (shoppingCartDb ^. shoppingCartShippingInfos)
      In the second argument of ‘($)’, namely
        ‘runInsertReturningList
          (shoppingCartDb ^. shoppingCartShippingInfos)
        $ insertValues shippingInfos’
      In the expression:
        withDatabaseDebug putStrLn conn
        $ runInsertReturningList
            (shoppingCartDb ^. shoppingCartShippingInfos)
          $ insertValues shippingInfos (intero)
error:
• No instance for (HasSqlValueSyntax PgValueSyntax ShippingCarrier)
    arising from a use of ‘insertValues’
• In the second argument of ‘($)’, namely
    ‘insertValues shippingInfos’
    In the second argument of ‘($)’, namely
      ‘runInsertReturningList
        (shoppingCartDb ^. shoppingCartShippingInfos)
      $ insertValues shippingInfos’
    In the expression:
      withDatabaseDebug putStrLn conn
      $ runInsertReturningList
          (shoppingCartDb ^. shoppingCartShippingInfos)
        $ insertValues shippingInfos (intero)
```

These errors are because there's no way to express a `ShippingCarrier` in the
backend syntax. We can fix this by writing instances for beam. We can re-use the
functionality we already have for `String`.  Let's start with
`HasSqlValueSyntax` first.

The `HasSqlValueSyntax` class tells us how to convert a Haskell value into a
corresponding backend value.

We'll need a new language pragma, and another import.

```haskell
{-# LANGUAGE UndecidableInstances #-}
import Database.Beam.Backend.SQL

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax
```

That will take care of the value syntax error, but we still need the
`FromBackendRow` error addressed.

The `FromBackendRow` class tells us how to convert a value from the database
into a corresponding Haskell value. Most often, it is enough to declare an empty
instance, so long as there is a backend-specific instance for unmarshaling your
data type.

We will need another language pragma and another import

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
import           Database.Beam.Backend
instance FromBackendRow Postgres ShippingCarrier
```

This gets rid of another error, but oh no! Another error has reared its ugly
head!

```haskell
error:
  • No instance for (Database.PostgreSQL.Simple.FromField.FromField ShippingCarrier)
      arising from a use of ‘Database.Beam.Backend.Types.$dmfromBackendRow’
  • In the expression:
      Database.Beam.Backend.Types.$dmfromBackendRow
        @Postgres @ShippingCarrier
    In an equation for ‘fromBackendRow’:
        fromBackendRow
          = Database.Beam.Backend.Types.$dmfromBackendRow
              @Postgres @ShippingCarrier
    In the instance declaration for
      ‘FromBackendRow Postgres ShippingCarrier’
```

Let's see if we can write `Database.PostgreSQL.Simple.FromField.FromField` instance
for `ShippingCarrier` and then let's try put our instance declaration for
`FromBackendRow` after that.

```haskell
import           Database.PostgreSQL.Simple.FromField
import           Text.Read

instance FromField ShippingCarrier where
  fromField f = do x <- readMaybe <$> fromField f
                   case x of
                     Nothing -> returnError ConversionFailed f "Could not 'read' value for 'ShippingCarrier'"
                     Just x -> pure x

instance FromBackendRow Postgres ShippingCarrier
```

Now, we can insert shipping information in our main function!

```haskell
main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=shoppingcart3"
  insertUsers conn
  addresses@[jamesAddress1, bettyAddress1, bettyAddress2] <- insertAddresses conn
  products@[redBall, mathTextbook, introToHaskell, suitcase] <- insertProducts conn
  [bettyShippingInfo] <- insertShippingInfos conn
```

And if we look at the value of `bettyShippingInfo`, `ShippingCarrier` has been
stored correctly.

```haskell
ShippingInfo {_shippingInfoId = Auto {unAuto = Just 1}, _shippingInfoCarrier = USPS, _shippingInfoTrackingNumber = "12345790ABCDEFGHI"}
```

Now, let's insert some orders that just came in. In the previous `INSERT`
examples, we used `insertValues` to insert arbitrary values into the database.
Now, we want to insert transactions with the current database timestamp (i.e.,
`CURRENT_TIMESTAMP` in SQL). We can insert rows containing arbitrary expressions
using the `insertExpressions` function. As you can see, the resulting rows have
a timestamp set by the database.

!beam-query
```haskell
!employee3sql sql
!employee3out console
insertOrders :: Connection -> [Address] -> ShippingInfo -> IO [Order]
insertOrders conn [jamesAddress1, bettyAddress1, bettyAddress2] bettyShippingInfo =
  withDatabaseDebug putStrLn conn $
    runInsertReturningList (shoppingCartDb ^. shoppingCartOrders) $
    insertExpressions [ Order (val_ (Auto Nothing)) currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_
                      , Order (val_ (Auto Nothing)) currentTimestamp_ (val_ (pk betty)) (val_ (pk bettyAddress1)) (just_ (val_ (pk bettyShippingInfo)))
                      , Order (val_ (Auto Nothing)) currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_
                      ]
```

You can also do this with `insertValues` -- we won't need all the calls to
`val_` and we will need to get the current utc time and make that into a `LocalTime`

```haskell
insertOrders :: Connection -> [Address] -> ShippingInfo -> IO [Order]
insertOrders conn [jamesAddress1, bettyAddress1, bettyAddress2] bettyShippingInfo =
  do
    time <- getCurrentTime
    let localtime = utcToLocalTime utc time
    withDatabaseDebug putStrLn conn $
      runInsertReturningList (shoppingCartDb ^. shoppingCartOrders) $
      insertValues [ Order (Auto Nothing) localtime (pk james) (pk jamesAddress1) nothing_
                   , Order (Auto Nothing) localtime (pk betty) (pk bettyAddress1) (just_ (pk bettyShippingInfo))
                   , Order (Auto Nothing) localtime (pk james) (pk jamesAddress1) nothing_
                   ]
```

Finally, let's add some line items

!beam-query
```haskell
!employee3sql-1 sql
insertLineItems :: Connection -> [Order] -> [Product] -> IO ()
insertLineItems conn orders@[jamesOrder1, bettyOrder1, jamesOrder2] products@[redBall, mathTextbook, introToHaskell, suitcase] =
  withDatabaseDebug putStrLn conn $
  B.runInsert $ B.insert (shoppingCartDb ^. shoppingCartLineItems) $
  insertValues [ LineItem (pk jamesOrder1) (pk redBall) 10
               , LineItem (pk jamesOrder1) (pk mathTextbook) 1
               , LineItem (pk jamesOrder1) (pk introToHaskell) 4
               , LineItem (pk bettyOrder1) (pk mathTextbook) 3
               , LineItem (pk bettyOrder1) (pk introToHaskell) 3
               , LineItem (pk jamesOrder2) (pk mathTextbook) 1 ]
```

Phew! Let's write some queries on this data!

Our main function now looks like this:

```haskell
main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=shoppingcart3"
  insertUsers conn
  addresses@[jamesAddress1, bettyAddress1, bettyAddress2] <- insertAddresses conn
  products@[redBall, mathTextbook, introToHaskell, suitcase] <- insertProducts conn
  [bettyShippingInfo] <- insertShippingInfos conn
  orders@[jamesOrder1, bettyOrder1, jamesOrder2] <- insertOrders conn addresses bettyShippingInfo
  insertLineItems conn orders products
```

And inserts all the data to all the tables we have defined.

Would you like some left joins with that?
========================

Suppose we want to do some analytics on our users, and so we want to know how many orders each user
has made in our system. We can write a query to list every user along with the orders they've
made. We can use `leftJoin_` to include all users in our result set, even those who have no
orders.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out
selectAllUsersAndOrdersLeftJoin :: Connection -> IO [(User, Maybe Order)]
selectAllUsersAndOrdersLeftJoin conn =
  withDatabaseDebug putStrLn conn $
      runSelectReturningList $ select $ do
        user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
        order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders)) (\order -> _orderForUser order `references_` user)
        pure (user, order)
```

Notice that sam is included in the result set, even though he doesn't have any
associated orders. Instead of a `Just (Order ..)`, `Nothing` is returned
instead.

Next, perhaps our marketing team wanted to send e-mails out to all users with no
orders. We can use `isNothing_` or `isJust_` to determine the status if a
nullable table or `QExpr s (Maybe x)`. The following query uses `isNothing_` to
find users who have no associated orders.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out
selectUsersWithNoOrdersLeftJoin :: Connection -> IO [User]
selectUsersWithNoOrdersLeftJoin conn =
   withDatabaseDebug putStrLn conn $
    runSelectReturningList $ select $ do
      user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
      order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders)) (\order -> _orderForUser order `references_` user)
      guard_ (isNothing_ order)
      pure user
```

We see that beam generates a sensible SQL `SELECT` and `WHERE` clause.

We can also use the `exists_` combinator to utilize the SQL `EXISTS` clause.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out
selectUsersWithNoOrdersExistsCombinator :: Connection -> IO [User]
selectUsersWithNoOrdersExistsCombinator conn =
   withDatabaseDebug putStrLn conn $
    runSelectReturningList $ select $ do
      user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
      guard_ (not_ (exists_ (filter_ (\order -> _orderForUser order `references_` user) (all_ (shoppingCartDb ^. shoppingCartOrders)))))
      pure user
```

!!! warning "TODO BUG IN SQL GENERATION"
    This is not generating the correct sql for postgres.  It complains at us
    with the hint: `There is a column named "email" in table "t0", but it cannot
    be referenced from this part of the query.`

If you change the sql *just* a bit, to

```sql
SELECT
  "t0"."email" AS "res0",
  "t0"."first_name" AS "res1",
  "t0"."last_name" AS "res2",
  "t0"."password" AS "res3"
FROM "cart_users" AS "t0"
WHERE NOT(EXISTS (SELECT
                    "t1"."id" AS "res0",
                    "t1"."date" AS "res1",
                    "t1"."for_user__email" AS "res2",
                    "t1"."ship_to_address__id" AS "res3",
                    "t1"."shipping_info__id" AS "res4"
                  FROM "orders" AS "t1"
                  WHERE ("t1"."for_user__email") = ("t0"."email")))
```

Postgres is happy again :)

Now suppose we wanted to do some analysis on the orders themselves. To start, we
want to get the orders sorted by their portion of revenue. We can use
`aggregate_` to list every order and the total amount of all products in that
order.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out

ordersWithCostOrdered :: Connection -> IO [(Order, Int)]
ordersWithCostOrdered conn =
  withDatabaseDebug putStrLn conn $
      runSelectReturningList $ select $
      orderBy_ (\(order, total) -> desc_ total) $
      aggregate_ (\(order, lineItem, product) ->
                    (group_ order, sum_ (lineItem ^. lineItemQuantity * product ^. productPrice))) $
      do
        lineItem <- all_ (shoppingCartDb ^. shoppingCartLineItems)
        order    <- related_ (shoppingCartDb ^. shoppingCartOrders) (_lineItemInOrder lineItem)
        product  <- related_ (shoppingCartDb ^. shoppingCartProducts) (_lineItemForProduct lineItem)
        pure (order, lineItem, product)
```

We can also get the total amount spent by each user, even including users with no orders. Notice
that we have to use `maybe_` below in order to handle the fact that some tables have been introduced
into our query with a left join. `maybe_` is to `QExpr` what `maybe` is to normal Haskell
values. `maybe_` is polymorphic to either `QExpr`s or full on tables of `QExpr`s. For our purposes,
the type of `maybe_` is

```haskell
maybe_ :: QExpr s a -> (QExpr s b -> QExpr s a) -> QExpr s (Maybe b) -> QExpr s a
```

With that in mind, we can write the query to get the total spent by user

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out

allUsersAndTotals :: Connection -> IO [(User, Int)]
allUsersAndTotals conn =
  withDatabaseDebug putStrLn conn $
      runSelectReturningList $
      select $
      orderBy_ (\(user, total) -> desc_ total) $
      aggregate_ (\(user, lineItem, product) ->
                    (group_ user, sum_ (maybe_ 0 id (_lineItemQuantity lineItem) * maybe_ 0 id (product ^. productPrice)))) $
      do user     <- all_ (shoppingCartDb ^. shoppingCartUsers)
         order    <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                              (\order -> _orderForUser order `references_` user)
         lineItem <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartLineItems))
                              (\lineItem -> maybe_ (val_ False) (\order -> _lineItemInOrder lineItem `references_` order) order)
         product  <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartProducts))
                              (\product -> maybe_ (val_ False) (\lineItem -> _lineItemForProduct lineItem `references_` product) lineItem)
         pure (user, lineItem, product)
```

Queries with nullable foreign keys
=======

Recall that our schema contains a nullable foreign key from `OrderT` to `ShippingInfoT`. Above,
we've seen how `leftJoin_` introduces nullable tables into our queries. Below, we'll see how to use
nullable primary keys to optionally include information.

Suppose we want to find all orders who have not been shipped. We can do this by simply writing a query over the orders.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out

allUnshippedOrders :: Connection -> IO [Order]
allUnshippedOrders conn =
  withDatabaseDebug putStrLn conn $
    runSelectReturningList $
    select $
    filter_ (isNothing_ . _orderShippingInfo) $
    all_ (shoppingCartDb ^. shoppingCartOrders)
```

Let's count up all shipped and unshipped orders by user, including users who have no orders.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out

shippingInformationByUser :: Connection -> IO [(User, Int, Int)]
shippingInformationByUser conn =
  withDatabaseDebug putStrLn conn $
    runSelectReturningList $
    select $
    aggregate_ (\(user, order) ->
                   let ShippingInfoId shippingInfoId = _orderShippingInfo order
                   in ( group_ user
                      , as_ @Int $ count_ (as_ @(Maybe Int) (maybe_ (just_ 1) (\_ -> nothing_) shippingInfoId))
                      , as_ @Int $ count_ shippingInfoId ) ) $
    do user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
       order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders)) (\order -> _orderForUser order `references_` user)
       pure (user, order)
```

Uh-oh! There's an error in the result set! Sam is reported as having one
unshipped order, instead of zero.

Here we hit one of the limitations of beam's mapping to SQL, and really one of
the limitations of SQL itself. Namely, the NULL in the result rows for Sam is
not distinguished from the NULL in the shipping info key itself. Beam however
does make the distinction.

When beam deserializes a `NULL` in a `Maybe` field, the outermost `Maybe` is the
one populated with `Nothing`. Thus it is impossible to retrieve a value like
`Just Nothing` from the database using the default serializers and
deserializers. In general, it's best to avoid highly nested `Maybe`s in your
queries because it makes them more difficult to understand.

One way to work around this issue in the above query is to use subselects.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out

shippingInformationByUserSubselect :: Connection -> IO [(User, Int, Int)]
shippingInformationByUserSubselect conn =
  withDatabaseDebug putStrLn conn $
    runSelectReturningList $
    select $
    do user <- all_ (shoppingCartDb ^. shoppingCartUsers)

       (userEmail, unshippedCount) <-
         aggregate_ (\(userEmail, order) -> (group_ userEmail, countAll_)) $
         do user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
            order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                               (\order -> _orderForUser order `references_` user &&. isNothing_ (_orderShippingInfo order))
            pure (pk user, order)

       guard_ (userEmail `references_` user)

       (userEmail, shippedCount) <-
         aggregate_ (\(userEmail, order) -> (group_ userEmail, countAll_)) $
         do user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
            order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                               (\order -> _orderForUser order `references_` user &&. isJust_ (_orderShippingInfo order))
            pure (pk user, order)
       guard_ (userEmail `references_` user)

       pure (user, unshippedCount, shippedCount)
```

Notice that the `aggregate_`s embedded in the `Q` monad were automatically
converted into sub `SELECT`s. This is because beam queries are composable -- you
can use them wherever they type check and sensible SQL will result. Of course,
if you want more control, you can also use the `subselect_` combinator to force
generation of a sub `SELECT`.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out

shippingInformationByUserSubselectCombinator :: Connection -> IO [(User, Int, Int)]
shippingInformationByUserSubselectCombinator conn =
  withDatabaseDebug putStrLn conn $
    runSelectReturningList $
    select $
    do user <- all_ (shoppingCartDb ^. shoppingCartUsers)

       (userEmail, unshippedCount) <-
         subselect_ $
         aggregate_ (\(userEmail, order) -> (group_ userEmail, countAll_)) $
         do user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
            order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                               (\order -> _orderForUser order `references_` user &&. isNothing_ (_orderShippingInfo order))
            pure (pk user, order)

       guard_ (userEmail `references_` user)

       (userEmail, shippedCount) <-
         subselect_ $
         aggregate_ (\(userEmail, order) -> (group_ userEmail, countAll_)) $
         do user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
            order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                               (\order -> _orderForUser order `references_` user &&. isJust_ (_orderShippingInfo order))
            pure (pk user, order)
       guard_ (userEmail `references_` user)

       pure (user, unshippedCount, shippedCount)
```

Conclusion
=======

This tutorial completes our sequence on creating a shopping cart. Throughout the
tutorials, we saw how to create tables using regular Haskell data types, how to
link those tables up using relations, how to query tables using both the monadic
interface and the list-like functions on queries. We saw ha few examples of
using beam to generate advanced queries. More information on the Beam API is
havailable on [hackage](http://hackage.haskell.org/package/beam). Happy beaming!

Beam is a work in progress. Please submit bugs and patches
on [GitHub](https://github.com/tathougies/beam).

