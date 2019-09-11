Introduction
=======

In the last part, we extended our shopping cart database to let users add
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
                { _productId          :: C f Int
                , _productTitle       :: C f Text
                , _productDescription :: C f Text
                , _productPrice       :: C f Int {- Price in cents -} }
                  deriving (Generic, Beamable)
type Product = ProductT Identity
deriving instance Show Product

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int)
                               deriving (Generic, Beamable)
  primaryKey = ProductId . _productId
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
              { _orderId      :: Columnar f Int
              , _orderDate    :: Columnar f LocalTime
              , _orderForUser :: PrimaryKey UserT f
              , _orderShipToAddress :: PrimaryKey AddressT f
              , _orderShippingInfo :: PrimaryKey ShippingInfoT (Nullable f) }
                deriving (Generic, Beamable)
type Order = OrderT Identity
deriving instance Show Order

instance Table OrderT where
    data PrimaryKey OrderT f = OrderId (Columnar f Int)
                               deriving (Generic, Beamable)
    primaryKey = OrderId . _orderId

data ShippingCarrier = USPS | FedEx | UPS | DHL
                       deriving (Show, Read, Eq, Ord, Enum)

data ShippingInfoT f = ShippingInfo
                     { _shippingInfoId             :: Columnar f Int
                     , _shippingInfoCarrier        :: Columnar f ShippingCarrier
                     , _shippingInfoTrackingNumber :: Columnar f Text }
                       deriving (Generic, Beamable)
type ShippingInfo = ShippingInfoT Identity
deriving instance Show ShippingInfo

instance Table ShippingInfoT where
    data PrimaryKey ShippingInfoT f = ShippingInfoId (Columnar f Int)
                                      deriving (Generic, Beamable)
    primaryKey = ShippingInfoId . _shippingInfoId

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
                   deriving (Generic, Beamable)
type LineItem = LineItemT Identity
deriving instance Show LineItem

instance Table LineItemT where
    data PrimaryKey LineItemT f = LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
                                  deriving (Generic, Beamable)
    primaryKey = LineItemId <$> _lineItemInOrder <*> _lineItemForProduct
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
                        deriving (Generic, Database be)

ShoppingCartDb (TableLens shoppingCartUsers) (TableLens shoppingCartUserAddresses)
               (TableLens shoppingCartProducts) (TableLens shoppingCartOrders)
               (TableLens shoppingCartShippingInfos) (TableLens shoppingCartLineItems) = dbLenses

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification`
                 dbModification {
                   _shoppingCartUserAddresses =
                     setEntityName "addresses" <>
                     modifyTableFields tableModification {
                       _addressLine1 = "address1",
                       _addressLine2 = "address2"
                     },
                   _shoppingCartProducts = setEntityName "products",
                   _shoppingCartOrders = setEntityName "orders" <>
                                         modifyTableFields tableModification {
                                           _orderShippingInfo = ShippingInfoId "shipping_info__id"
                                         },
                   _shoppingCartShippingInfos = setEntityName "shipping_info" <>
                                                modifyTableFields tableModification {
                                                  _shippingInfoId = "id",
                                                  _shippingInfoCarrier = "carrier",
                                                  _shippingInfoTrackingNumber = "tracking_number"
                                                },
                   _shoppingCartLineItems = setEntityName "line_items"
                 }
```

Fixtures
=======

Let's put some sample data into a new database.

```haskell
conn <- open "shoppingcart3.db"

execute_ conn "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
execute_ conn "CREATE TABLE addresses ( id INTEGER PRIMARY KEY AUTOINCREMENT, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );"
execute_ conn "CREATE TABLE products ( id INTEGER PRIMARY KEY AUTOINCREMENT, title VARCHAR NOT NULL, description VARCHAR NOT NULL, price INT NOT NULL );"
execute_ conn "CREATE TABLE orders ( id INTEGER PRIMARY KEY AUTOINCREMENT, date TIMESTAMP NOT NULL, for_user__email VARCHAR NOT NULL, ship_to_address__id INT NOT NULL, shipping_info__id INT);"
execute_ conn "CREATE TABLE shipping_info ( id INTEGER PRIMARY KEY AUTOINCREMENT, carrier VARCHAR NOT NULL, tracking_number VARCHAR NOT NULL);"
execute_ conn "CREATE TABLE line_items (item_in_order__id INTEGER NOT NULL, item_for_product__id INTEGER NOT NULL, item_quantity INTEGER NOT NULL)"
```

Let's put some sample data into our database. Below, we will use the
`beam-sqlite` functions `insertReturning` and `runInsertReturningList` to insert
rows *and* retrieve the inserted rows from the database. This will let us see
what values the auto-incremented `id` columns took on, which will allow us to
create references to these inserted rows.

```haskell
let users@[james, betty, sam] =
          [ User "james@example.com" "James" "Smith"  "b4cc344d25a2efe540adbf2678e2304c" {- james -}
          , User "betty@example.com" "Betty" "Jones"  "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
          , User "sam@example.com"   "Sam"   "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
    addresses = [ Address default_ (val_ "123 Little Street") (val_ Nothing) (val_ "Boston") (val_ "MA") (val_ "12345") (pk james)
                , Address default_ (val_ "222 Main Street") (val_ (Just "Ste 1")) (val_ "Houston") (val_ "TX") (val_ "8888") (pk betty)
                , Address default_ (val_ "9999 Residence Ave") (val_ Nothing) (val_ "Sugarland") (val_ "TX") (val_ "8989") (pk betty) ]

    products = [ Product default_ (val_ "Red Ball") (val_ "A bright red, very spherical ball") (val_ 1000)
               , Product default_ (val_ "Math Textbook") (val_ "Contains a lot of important math theorems and formulae") (val_ 2500)
               , Product default_ (val_ "Intro to Haskell") (val_ "Learn the best programming language in the world") (val_ 3000)
               , Product default_ (val_ "Suitcase") "A hard durable suitcase" 15000 ]

(jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase) <-
  runBeamSqliteDebug putStrLn conn $ do
    runInsert $ insert (shoppingCartDb ^. shoppingCartUsers) $
                insertValues users

    [jamesAddress1, bettyAddress1, bettyAddress2] <-
      runInsertReturningList $
      insertReturning (shoppingCartDb ^. shoppingCartUserAddresses) $ insertExpressions addresses

    [redBall, mathTextbook, introToHaskell, suitcase] <-
      runInsertReturningList $
      insertReturning (shoppingCartDb ^. shoppingCartProducts) $ insertExpressions products

    pure ( jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase )
```

Now, if we take a look at one of the returned addresses, like
`jamesAddress1`, we see it has had the `default_` values assigned
correctly.

```haskell
Prelude Database.Beam Database.Beam.Sqlite Data.Time Database.SQLite.Simple Data.Text Lens.Micro> jamesAddress1
Address {_addressId = 1, _addressLine1 = "123 Little Street", _addressLine2 = Nothing, _addressCity = "Boston", _addressState = "MA", _addressZip = "12345", _addressForUser = UserId "james@example.com"}
```

## Marshalling a custom type

Now we can insert shipping information. Of course, the shipping information
contains the `ShippingCarrier` enumeration.

```haskell
bettyShippingInfo <-
  runBeamSqliteDebug putStrLn conn $ do
    [bettyShippingInfo] <-
      runInsertReturningList $
      insertReturning (shoppingCartDb ^. shoppingCartShippingInfos) $
      insertExpressions [ ShippingInfo default_ (val_ USPS) (val_ "12345790ABCDEFGHI") ]
    pure bettyShippingInfo
```

If you run this, you'll get an error from GHCi.

```
<interactive>:845:7: error:
    • No instance for (FromBackendRow Sqlite ShippingCarrier)
        arising from a use of ‘runInsertReturningList’
    • In a stmt of a 'do' block:
        [bettyShippingInfo] <- runInsertReturningList
                                 $ insertReturning (shoppingCartDb ^. shoppingCartShippingInfos)
                                     $ insertExpressions
                                         [ShippingInfo
                                            default_ (val_ USPS) (val_ "12345790ABCDEFGHI")]
...

<interactive>:847:50: error:
    • No instance for (Database.Beam.Backend.SQL.SQL92.HasSqlValueSyntax
                         Database.Beam.Sqlite.Syntax.SqliteValueSyntax ShippingCarrier)
```

These errors are because there's no way to express a `ShippingCarrier` in the
backend syntax. We can fix this by writing instances for beam. We can re-use the
functionality we already have for `String`.

The `HasSqlValueSyntax` class tells us how to convert a Haskell value into a
corresponding backend value.

```haskell
import Database.Beam.Backend.SQL

:set -XUndecidableInstances

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax
```

`autoSqlValueSyntax` uses the underlying `Show` instance to serialize
a type to a string representation.

The `FromBackendRow` class tells us how to convert a value from the database
into a corresponding Haskell value.

```haskell
import qualified Data.Text as T -- for unpack

instance FromBackendRow Sqlite ShippingCarrier where
  fromBackendRow = read . T.unpack <$> fromBackendRow
```

Since, `autoSqlValueSyntax` uses the `Show` instance, we can simply use the `Read` instance.

Now, if we try to insert the shipping info again, it works.

```haskell
bettyShippingInfo <-
  runBeamSqliteDebug putStrLn conn $ do
    [bettyShippingInfo] <-
      runInsertReturningList $
      insertReturning (shoppingCartDb ^. shoppingCartShippingInfos) $
      insertExpressions [ ShippingInfo default_ (val_ USPS) (val_ "12345790ABCDEFGHI") ]
    pure bettyShippingInfo
```

And if we look at the value of `bettyShippingInfo`, `ShippingCarrier` has been
stored correctly.

```haskell
> bettyShippingInfo
ShippingInfo {_shippingInfoId = 1, _shippingInfoCarrier = USPS, _shippingInfoTrackingNumber = "12345790ABCDEFGHI"}
```

Now, let's insert some orders that just came in. We want to insert
transactions with the current database timestamp (i.e.,
`CURRENT_TIMESTAMP` in SQL). We can do this using
`insertExpressions`. If you run the example below, you'll see the
resulting rows have a timestamp set by the database.

!beam-query
```haskell
!employee3sql sql
!employee3out console
[ jamesOrder1, bettyOrder1, jamesOrder2 ] <-
  runBeamSqliteDebug putStrLn conn $ do
    runInsertReturningList $
      insertReturning (shoppingCartDb ^. shoppingCartOrders) $
      insertExpressions $
      [ Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_
      , Order default_ currentTimestamp_ (val_ (pk betty)) (val_ (pk bettyAddress1)) (just_ (val_ (pk bettyShippingInfo)))
      , Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_ ]

print jamesOrder1
print bettyOrder1
print jamesOrder2
```

Finally, let's add some line items

!beam-query
```haskell
!employee3sql-1 sql
let lineItems = [ LineItem (pk jamesOrder1) (pk redBall) 10
                , LineItem (pk jamesOrder1) (pk mathTextbook) 1
                , LineItem (pk jamesOrder1) (pk introToHaskell) 4

                , LineItem (pk bettyOrder1) (pk mathTextbook) 3
                , LineItem (pk bettyOrder1) (pk introToHaskell) 3

                , LineItem (pk jamesOrder2) (pk mathTextbook) 1 ]

runBeamSqliteDebug putStrLn conn $ do
  runInsert $ insert (shoppingCartDb ^. shoppingCartLineItems) $
    insertValues lineItems
```

Phew! Let's write some queries on this data!

Would you like some left joins with that?
========================

Suppose we want to do some analytics on our users, and so we want to know how many orders each user
has made in our system. We can write a query to list every user along with the orders they've
made. We can use `leftJoin_` to include all users in our result set, even those who have no
orders.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 output
usersAndOrders <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
    select $ do
      user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
      order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders)) (\order -> _orderForUser order `references_` user)
      pure (user, order)

mapM_ print usersAndOrders
```

Notice that Sam is included in the result set, even though he doesn't have any
associated orders. Instead of a `Just (Order ..)`, `Nothing` is returned
instead.

Next, perhaps our marketing team wanted to send e-mails out to all users with no
orders. We can use `isNothing_` or `isJust_` to determine the status if a
nullable table or `QExpr s (Maybe x)`. The following query uses `isNothing_` to
find users who have no associated orders.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 output
usersWithNoOrders <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
    select $ do
      user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
      order <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders)) (\order -> _orderForUser order `references_` user)
      guard_ (isNothing_ order)
      pure user

mapM_ print usersWithNoOrders
```

We see that beam generates a sensible SQL `SELECT` and `WHERE` clause.

We can also use the `exists_` combinator to utilize the SQL `EXISTS` clause.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 output
usersWithNoOrders <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
    select $ do
      user  <- all_ (shoppingCartDb ^. shoppingCartUsers)
      guard_ (not_ (exists_ (filter_ (\order -> _orderForUser order `references_` user) (all_ (shoppingCartDb ^. shoppingCartOrders)))))
      pure user

mapM_ print usersWithNoOrders
```

Now suppose we wanted to do some analysis on the orders themselves. To start, we
want to get the orders sorted by their portion of revenue. We can use
`aggregate_` to list every order and the total amount of all products in that
order.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 output

ordersWithCostOrdered <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
    select $
    orderBy_ (\(order, total) -> desc_ total) $
    aggregate_ (\(order, lineItem, product) ->
                   (group_ order, sum_ (lineItem ^. lineItemQuantity * product ^. productPrice))) $
    do lineItem <- all_ (shoppingCartDb ^. shoppingCartLineItems)
       order    <- related_ (shoppingCartDb ^. shoppingCartOrders) (_lineItemInOrder lineItem)
       product  <- related_ (shoppingCartDb ^. shoppingCartProducts) (_lineItemForProduct lineItem)
       pure (order, lineItem, product)

mapM_ print ordersWithCostOrdered
```

We can also get the total amount spent by each user, even including users with no orders. Notice
that we have to use `maybe_` below in order to handle the fact that some tables have been introduced
into our query with a left join. `maybe_` is to `QExpr` what `maybe` is to normal Haskell
values. `maybe_` is polymorphic to either `QExpr`s or full on tables of `QExpr`s. For our purposes,
the type of `maybe_` is

```haskell
maybe_ :: QExpr be s a -> (QExpr be s b -> QExpr be s a) -> QExpr be s (Maybe b) -> QExpr be s a
```

With that in mind, we can write the query to get the total spent by user

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 output

allUsersAndTotals <-
  runBeamSqliteDebug putStrLn conn $
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

mapM_ print allUsersAndTotals
```

Take a couple seconds to examine the SQL generated by this query. Notice how every time we used
`maybe_` a `CASE` statement was emitted. While this provides a good match for Haskell semantics
we are used to, it is also not always desireable in practice due to severe performance implications.

Some RDBMSs, like Postgres, given such a query will be unable to utilize available indexes
to perform join operations - this translates to *extremely* poor perfomance for even moderately
sized data.

Luckily, Beam also provides an alternate way to phrase things that directly maps to SQL semantics

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 output

allUsersAndTotals2 <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
    select $
    orderBy_ (\(user, total) -> desc_ total) $
    aggregate_ (\(user, lineItem, product) ->
                   (group_ user, sum_ (maybe_ 0 id (_lineItemQuantity lineItem) * maybe_ 0 id (product ^. productPrice)))) $
    do user     <- all_ (shoppingCartDb ^. shoppingCartUsers)
       order    <- leftJoin_ (all_ (shoppingCartDb ^. shoppingCartOrders))
                             (\order -> _orderForUser order `references_` user)
       lineItem <- leftJoin_' (all_ (shoppingCartDb ^. shoppingCartLineItems))
                              (\lineItem -> just_ (_lineItemInOrder lineItem) ==?. pk order)
       product  <- leftJoin_' (all_ (shoppingCartDb ^. shoppingCartProducts))
                              (\product -> _lineItemForProduct lineItem ==?. just_ (pk product))
       pure (user, lineItem, product)

mapM_ print allUsersAndTotals2
```

Notice how we managed to eliminate `maybe_` from the join conditions by using the `SqlBool` version of
`leftJoin_`, `leftJoin_'` together with `just_` and the `SqlBool` version of the equality operator `==?.`.
Compare the generated SQL with the previous query. You can read more about how Beam handles NULL values
in the Queries, Relationships section in the User Guide.

Queries with nullable foreign keys
=======

Recall that our schema contains a nullable foreign key from `OrderT` to `ShippingInfoT`. Above,
we've seen how `leftJoin_` introduces nullable tables into our queries. Below, we'll see how to use
nullable primary keys to optionally include information.

Suppose we want to find all orders who have not been shipped. We can do this by simply writing a query over the orders.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 output

allUnshippedOrders <-
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
    select $
    filter_ (isNothing_ . _orderShippingInfo) $
    all_ (shoppingCartDb ^. shoppingCartOrders)

mapM_ print allUnshippedOrders
```

Let's count up all shipped and unshipped orders by user, including users who have no orders.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 out

shippingInformationByUser <-
  runBeamSqliteDebug putStrLn conn $
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

mapM_ print shippingInformationByUser
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
!employee3out-2 output

shippingInformationByUser <-
  runBeamSqliteDebug putStrLn conn $
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

mapM_ print shippingInformationByUser
```

Notice that the `aggregate_`s embedded in the `Q` monad were automatically
converted into sub `SELECT`s. This is because beam queries are composable -- you
can use them wherever they type check and sensible SQL will result. Of course,
if you want more control, you can also use the `subselect_` combinator to force
generation of a sub `SELECT`.

!beam-query
```haskell
!employee3sql-2 sql
!employee3out-2 output

shippingInformationByUser <-
  runBeamSqliteDebug putStrLn conn $
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

mapM_ print shippingInformationByUser
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
