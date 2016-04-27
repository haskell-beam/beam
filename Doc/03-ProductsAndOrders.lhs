Introduction
=======

In the previous tutorial (02-NextSteps.lhs), we extended our shopping cart database to let users add
multiple addresses. We saw how to establish one-to-many relations between two tables, and how to use
the monadic query interface to write SQL JOINs. In this installment, we'll be adding support for
products and orders to our database schema. We'll see how to use an intermediary table to create
many-to-many relations and how to use `perhaps_` to write LEFT JOINs. Finally, we'll see how to use
`Nullable` to create optional foreign key references.

Where we left off
=======

Because this file is literate haskell and because readers may want to refresh their memories a bit,
I've duplicated all the schema work we've done up until this point below.

>{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, TypeFamilies, DeriveGeneric, OverloadedStrings, PartialTypeSignatures #-}
> module Main where
>
> import Database.Beam
> import Database.Beam.Backend.Sqlite3
>
> import Control.Monad
>
> import Data.Time
> import Data.Text (Text)
> import Lens.Micro
>
> data UserT f = User
>              { _userEmail     :: Columnar f Text
>              , _userFirstName :: Columnar f Text
>              , _userLastName  :: Columnar f Text
>              , _userPassword  :: Columnar f Text }
>               deriving Generic
>
> type User = UserT Identity
> deriving instance Show User
>
> instance Table UserT where
>     data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
>     primaryKey = UserId . _userEmail
>
> instance Beamable UserT
> instance Beamable (PrimaryKey UserT)
>
> type UserId = PrimaryKey UserT Identity
> deriving instance Show UserId
>
> data AddressT f = Address
>                 { _addressId    :: Columnar f AutoId
>                 , _addressLine1 :: Columnar f Text
>                 , _addressLine2 :: Columnar f (Maybe Text)
>                 , _addressCity  :: Columnar f Text
>                 , _addressState :: Columnar f Text
>                 , _addressZip   :: Columnar f Text
>
>                 , _addressForUser :: PrimaryKey UserT f }
>                   deriving Generic
> type Address = AddressT Identity
> deriving instance Show Address
>
> instance Table AddressT where
>     data PrimaryKey AddressT f = AddressId (Columnar f AutoId) deriving Generic
>     primaryKey = AddressId . _addressId
>
> instance Beamable AddressT
> instance Beamable (PrimaryKey AddressT)
>
> Address (LensFor addressIdC)
>         (LensFor addressLine1C)
>         (LensFor addressLine2C)
>         (LensFor addressCityC)
>         (LensFor addressStateC)
>         (LensFor addressZipC)
>         (UserId (LensFor addressForUserIdC)) = tableConfigLenses

Creating tables is easy now
=======

Let's create our products table. By now, the pattern for adding a new table to the schema should be
pretty familiar, so I'm going to skip the explanation.

> data ProductT f = Product
>                 { _productId          :: Columnar f AutoId
>                 , _productTitle       :: Columnar f Text
>                 , _productDescription :: Columnar f Text
>                 , _productPrice       :: Columnar f Int {- Price in cents -} }
>                   deriving Generic
> type Product = ProductT Identity
> deriving instance Show Product
>
> instance Table ProductT where
>   data PrimaryKey ProductT f = ProductId (Columnar f AutoId)
>                                deriving Generic
>   primaryKey = ProductId . _productId
>
> instance Beamable ProductT
> instance Beamable (PrimaryKey ProductT)

For orders, we want to store an id, date created, and the user who made the order. We'd also like to
create an optional link to a shipping information table. When the shipping information is created,
we'll fill in the shipping information in the order. In order to create the optional reference,
we're going to use the `Nullable` tag modifier to modify the column tag. `Nullable` will turn all
fields of type `x` into `Maybe x`. Note that we could also create this relation by installing a
primary key on the shipping info table, and this is arguably the better option. However, we'll go
with a nullable foreign key here to show the full breadth of beam's features, and because this sort
of relation exists in many existing databases.

> data OrderT f = Order
>               { _orderId      :: Columnar f AutoId
>               , _orderDate    :: Columnar f UTCTime
>               , _orderForUser :: PrimaryKey UserT f
>               , _orderShipToAddress :: PrimaryKey AddressT f
>               , _orderShippingInfo :: PrimaryKey ShippingInfoT (Nullable f) }
>                 deriving Generic
> type Order = OrderT Identity
> deriving instance Show Order
>
> instance Table OrderT where
>     data PrimaryKey OrderT f = OrderId (Columnar f AutoId)
>                                deriving Generic
>     primaryKey = OrderId . _orderId
>
> instance Beamable OrderT
> instance Beamable (PrimaryKey OrderT)
>
> data ShippingCarrier = USPS | FedEx | UPS | DHL
>                        deriving (Show, Read, Eq, Ord, Enum)
>
> instance BeamBackend be => HasDefaultFieldSchema be ShippingCarrier where
>     defFieldSchema _ = enumSchema
> instance BeamBackend be => FromSqlValues be ShippingCarrier where
>     fromSqlValues' = fromEnumValue
>     makeSqlValues' = makeEnumValue
>
> data ShippingInfoT f = ShippingInfo
>                      { _shippingInfoId             :: Columnar f AutoId
>                      , _shippingInfoCarrier        :: Columnar f ShippingCarrier
>                      , _shippingInfoTrackingNumber :: Columnar f Text }
>                        deriving Generic
> type ShippingInfo = ShippingInfoT Identity
> deriving instance Show ShippingInfo
>
> instance Table ShippingInfoT where
>     data PrimaryKey ShippingInfoT f = ShippingInfoId (Columnar f AutoId)
>                                       deriving Generic
>     primaryKey = ShippingInfoId . _shippingInfoId
>
> instance Beamable ShippingInfoT
> instance Beamable (PrimaryKey ShippingInfoT)
>

The above example also shows how to use a custom data type as a beam column. In this case, we wanted
to use the `ShippingCarrier` enumerable type as a column type. To do this, we instantiated the
`HasDefaultFieldSchema` class for the type. We set the default schema for the field to be
`enumSchema` which can store types inhabiting the `Enum` type class as integers in the database. We
also instantiate the `FromSqlValues` type class for `ShippingCarrier` with an empty
implementation. For inhabitants of `HasDefaultFieldSchema`, `FromSqlValues` uses the default
deserializer. This is almost always the best option.

We would also like to be able to associate a list of products with each order as line items. To do
this we will create a table with two foreign keys. This table will establish a many-to-many
relationship between orders and products.

> data LineItemT f = LineItem
>                  { _lineItemInOrder    :: PrimaryKey OrderT f
>                  , _lineItemForProduct :: PrimaryKey ProductT f
>                  , _lineItemQuantity   :: Columnar f Int }
>                    deriving Generic
> type LineItem = LineItemT Identity
> deriving instance Show LineItem
>
> instance Table LineItemT where
>     data PrimaryKey LineItemT f = LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
>                                   deriving Generic
>     primaryKey lineItem = LineItemId (_lineItemInOrder lineItem) (_lineItemForProduct lineItem)
>
> instance Beamable LineItemT
> instance Beamable (PrimaryKey LineItemT)

Finally, we'll need some more `Show` instances for GHC to be happy.

> deriving instance Show (PrimaryKey OrderT Identity)
> deriving instance Show (PrimaryKey ProductT Identity)
> deriving instance Show (PrimaryKey AddressT Identity)
> deriving instance Show (PrimaryKey ShippingInfoT (Nullable Identity))

Now we'll add all these tables to our database.

> data ShoppingCartDb f = ShoppingCartDb
>                       { _shoppingCartUsers         :: f UserT
>                       , _shoppingCartUserAddresses :: f AddressT
>                       , _shoppingCartProducts      :: f ProductT
>                       , _shoppingCartOrders        :: f OrderT
>                       , _shoppingCartShippingInfos :: f ShippingInfoT
>                       , _shoppingCartLineItems     :: f LineItemT }
>                         deriving Generic
>
> instance Database ShoppingCartDb
>
> ShoppingCartDb { _shoppingCartUserAddresses = TableLens addressT } = dbLenses
>
> shoppingCartDb :: DatabaseSettings Sqlite3Settings ShoppingCartDb
> shoppingCartDb = autoDbSettings
>                  & addressT . tableSettings . addressStateC . fieldSchema .~ textSchema (Char (Just 2))
>                  & addressT . tableSettings . addressZipC . fieldSchema .~ textSchema (Varchar (Just 5))

Fixtures
=======

Let's put some sample data into our database.

> main :: IO ()
> main = do beam <- openDatabaseDebug shoppingCartDb AutoMigrate (Sqlite3Settings "shoppingcart3.db")
>           dumpSchema shoppingCartDb
>
>           let users = [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
>                       , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
>                       , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]
>
>           Success [james, betty, sam] <- beamTxn beam $ \db ->
>                                          mapM (insertInto (_shoppingCartUsers db)) users
>
>           let addresses = [ Address UnassignedId "123 Little Street" Nothing "Boston" "MA" "12345" (pk james)
>
>                           , Address UnassignedId "222 Main Street" (Just "Ste 1") "Houston" "TX" "8888" (pk betty)
>                           , Address UnassignedId "9999 Residence Ave" Nothing "Sugarland" "TX" "8989" (pk betty) ]
>
>               products = [ Product UnassignedId "Red Ball" "A bright red, very spherical ball" 1000
>                          , Product UnassignedId "Math Textbook" "Contains a lot of important math theorems and formulae" 2500
>                          , Product UnassignedId "Intro to Haskell" "Learn the best programming language in the world" 3000
>                          , Product UnassignedId "Suitcase" "A hard durable suitcase" 15000 ]
>
>           Success [jamesAddress1, bettyAddress1, bettyAddress2] <-
>               beamTxn beam $ \db ->
>               mapM (insertInto (_shoppingCartUserAddresses db)) addresses
>           Success [redBall, mathTextbook, introToHaskell, suitcase] <-
>               beamTxn beam $ \db ->
>               mapM (insertInto (_shoppingCartProducts db)) products
>
>           Success bettysShippingInfo <- beamTxn beam $ \db ->
>                                         insertInto (_shoppingCartShippingInfos db)
>                                                    (ShippingInfo UnassignedId USPS "123456790ABCDEFGHI")
>
>           now <- getCurrentTime
>           let orders = [ Order UnassignedId now (pk james) (pk jamesAddress1) nothing_
>                        , Order UnassignedId now (pk betty) (pk bettyAddress1) (just_ (pk bettysShippingInfo))
>                        , Order UnassignedId now (pk james) (pk jamesAddress1) nothing_ ]
>           (res :: BeamResult () [Order]) <-
>               beamTxn beam $ \db ->
>                   mapM (insertInto (_shoppingCartOrders db)) orders
>           putStrLn ("Got " ++ show res)
>           let Success [jamesOrder1, bettysOrder, jamesOrder2] = res
>
>
>           let lineItems = [ LineItem (pk jamesOrder1) (pk redBall) 10
>                           , LineItem (pk jamesOrder1) (pk mathTextbook) 1
>                           , LineItem (pk jamesOrder1) (pk introToHaskell) 4
>
>                           , LineItem (pk bettysOrder) (pk mathTextbook) 3
>                           , LineItem (pk bettysOrder) (pk introToHaskell) 3
>
>                           , LineItem (pk jamesOrder2) (pk mathTextbook) 1 ]
>
>           beamTxn beam $ \db ->
>               mapM_ (insertInto (_shoppingCartLineItems db)) lineItems

Phew! Let's write some queries on this data!

Would you like some left joins with that?
========================

Suppose we want to do some analytics on our users, and so we want to know how many orders each user
has made in our system. We can write a query to list every user along with the orders they've
made. We can use `perhapsAll_` to include all users in our result set, even those who have no
orders.

>           putStrLn "All pairs of users and orders, but listing all users"
>           Success allPairs <-
>               beamTxn beam $ \db ->
>                 queryList $
>                 do user <- all_ (_shoppingCartUsers db)
>                    order <- perhapsAll_ (_shoppingCartOrders db) (\order -> _orderForUser order ==. just_ (pk user))
>                    return (user, order)
>           mapM_ (putStrLn . show) allPairs
>           putStrLn "----\n\n"

Notice that sam is included in the result set, even though he doesn't have any associated
orders. Instead of a `Just (Order ..)`, `Nothing` is returned instead.

< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password`, `t1`.`id`, `t1`.`date`, `t1`.`for_user__email`, `t1`.`ship_to_address__id`, `t1`.`shipping_info__info_id` FROM  cart_users AS t0 LEFT JOIN cart_orders AS t1 ON (`t1`.`for_user__email` == `t0`.`email`) with []
< (User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"},Just (Order {_orderId = AssignedId 1, _orderDate = 2016-01-25 20:02:14.007038 UTC, _orderForUser = UserId "james@example.com", _orderShipToAddress = AddressId (AssignedId 1), _orderShippingInfo = ShippingInfoId Nothing}))
< (User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"},Just (Order {_orderId = AssignedId 3, _orderDate = 2016-01-25 20:02:14.007038 UTC, _orderForUser = UserId "james@example.com", _orderShipToAddress = AddressId (AssignedId 1), _orderShippingInfo = ShippingInfoId Nothing}))
< (User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},Just (Order {_orderId = AssignedId 2, _orderDate = 2016-01-25 20:02:14.007038 UTC, _orderForUser = UserId "betty@example.com", _orderShipToAddress = AddressId (AssignedId 2), _orderShippingInfo = ShippingInfoId (Just (AssignedId 1))}))
< (User {_userEmail = "sam@example.com", _userFirstName = "Sam", _userLastName = "Taylor", _userPassword = "332532dcfaa1cbf61e2a266bd723612c"},Nothing)

Next, perhaps our marketing team wanted to send e-mails out to all users with no orders. We can use
`isNothing_` or `isJust_` to determine the status if a nullable table or `QExpr s (Maybe x)`. The
following query uses `isNothing_` to find users who have no associated orders.

>           putStrLn "All users without any orders, using WHERE"
>           Success allPairsWithoutOrdersUsingWhere <-
>               beamTxn beam $ \db ->
>                 queryList $
>                 do user <- all_ (_shoppingCartUsers db)
>                    order <- perhapsAll_ (_shoppingCartOrders db) (\order -> _orderForUser order ==. just_ (pk user))
>                    guard_ (isNothing_ order)
>                    return user
>           mapM_ (putStrLn . show) allPairsWithoutOrdersUsingWhere
>           putStrLn "----\n\n"

We see that beam generates a sensible SQL SELECT using the WHERE clause.

< All users without any orders, using WHERE
< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password` FROM  cart_users AS t0 LEFT JOIN cart_orders AS t1 ON (`t1`.`for_user__email` == `t0`.`email`) WHERE (`t1`.`id` IS NULL AND (`t1`.`date` IS NULL AND (`t1`.`for_user__email` IS NULL AND (`t1`.`ship_to_address__id` IS NULL AND `t1`.`shipping_info__info_id` IS NULL)))) with []
< User {_userEmail = "sam@example.com", _userFirstName = "Sam", _userLastName = "Taylor", _userPassword = "332532dcfaa1cbf61e2a266bd723612c"}
< ----

We can also use the `exists_` combinator to utilize the SQL `EXISTS` clause.

>           putStrLn "All users without any orders, using EXISTS"
>           Success allPairsWithoutOrdersUsingExists <-
>               beamTxn beam $ \db ->
>                 queryList $
>                 do user <- all_ (_shoppingCartUsers db)
>                    guard_ (not_ (exists_ (do order <- all_ (_shoppingCartOrders db)
>                                              guard_ (_orderForUser order ==. pk user))))
>                    return user
>           mapM_ (putStrLn . show) allPairsWithoutOrdersUsingExists
>           putStrLn "----\n\n"

Now suppose we wanted to do some analysis on the orders themselves. To start, we want to get the
orders sorted by their portion of revenue. We can use `aggregate` to list every order and the total
amount of all products in that order.

>           putStrLn "All orders with the total cost of all products in that order, ordered by total cost descending"
>           Success allOrdersAndTotals <-
>               beamTxn beam $ \db ->
>                 queryList $
>                 orderBy (\(order, total) -> desc_ total) $
>                 aggregate (\(order, lineItem, product) ->
>                                (group_ order, sum_ (_lineItemQuantity lineItem * _productPrice product))) $
>                 do lineItem <- all_ (_shoppingCartLineItems db)
>                    order <- related_ (_shoppingCartOrders db) (_lineItemInOrder lineItem)
>                    product <- related_ (_shoppingCartProducts db) (_lineItemForProduct lineItem)
>                    pure (order, lineItem, product)
>           mapM_ (putStrLn . show) allOrdersAndTotals
>           putStrLn "----\n\n"

The output is just as we'd expect, with the correct `GROUP BY` and `ORDER BY` clauses.

< All orders with the total cost of all products in that order, ordered by total cost descending
< Will execute SELECT `t1`.`id`, `t1`.`date`, `t1`.`for_user__email`, `t1`.`ship_to_address__id`, `t1`.`shipping_info__info_id`, SUM((`t0`.`item_quantity` * `t2`.`price`)) FROM  cart_line_items AS t0 INNER JOIN cart_orders AS t1 ON (`t0`.`item_in_order__id` == `t1`.`id`) INNER JOIN cart_products AS t2 ON (`t0`.`item_for_product__id` == `t2`.`id`) GROUP BY `t1`.`id`, `t1`.`date`, `t1`.`for_user__email`, `t1`.`ship_to_address__id`, `t1`.`shipping_info__info_id` ORDER BY SUM((`t0`.`item_quantity` * `t2`.`price`)) DESC with []
< (Order {_orderId = AssignedId 1, _orderDate = 2016-01-25 21:50:03.704396 UTC, _orderForUser = UserId "james@example.com", _orderShipToAddress = AddressId (AssignedId 1), _orderShippingInfo = ShippingInfoId Nothing},24500)
< (Order {_orderId = AssignedId 2, _orderDate = 2016-01-25 21:50:03.704396 UTC, _orderForUser = UserId "betty@example.com", _orderShipToAddress = AddressId (AssignedId 2), _orderShippingInfo = ShippingInfoId (Just (AssignedId 1))},16500)
< (Order {_orderId = AssignedId 3, _orderDate = 2016-01-25 21:50:03.704396 UTC, _orderForUser = UserId "james@example.com", _orderShipToAddress = AddressId (AssignedId 1), _orderShippingInfo = ShippingInfoId Nothing},2500)
< ----

We can also get the total amount spent by each user, even including users with no orders. Notice
that we have to use `maybe_` below in order to handle the fact that some tables have been introduced
into our query with a left join. `maybe_` is to `QExpr` what `maybe` is to normal Haskell
values. `maybe_` is polymorphic to either `QExpr`s or full on tables of `QExpr`s. For our purposes,
the type of `maybe_` is

< maybe_ :: QExpr s a -> (QExpr s b -> QExpr s a) -> QExpr s (Maybe b) -> QExpr s a

With that in mind, we can write the query to get the total spent by user

>           putStrLn "All users with total spent, including users who have spent nothing"
>           Success allUsersAndTotals <-
>               beamTxn beam $ \db ->
>                 queryList $
>                 orderBy (\(user, total) -> desc_ total) $
>                 aggregate (\(user, order, lineItem, product) ->
>                                (group_ user, sum_ (maybe_ 0 id (_lineItemQuantity lineItem) * maybe_ 0 id (_productPrice product)))) $
>                 do user <- all_ (_shoppingCartUsers db)
>
>                    order <- perhapsAll_ (_shoppingCartOrders db) (\order -> _orderForUser order `references_` just_ user)
>                    lineItem <- perhapsAll_ (_shoppingCartLineItems db) (\lineItem -> _lineItemInOrder lineItem `references_` order)
>                    product <- perhapsAll_ (_shoppingCartProducts db) (\product -> _lineItemForProduct lineItem `references_` product)
>
>                    pure (user, order, lineItem, product)
>           mapM_ (putStrLn . show) allUsersAndTotals
>           putStrLn "----\n\n"

Again beam produces the correct SQL and results.

< All users with total spent, including users who have spent nothing
< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password`, SUM(((CASE WHEN (`t2`.`item_quantity` IS NOT NULL) THEN `t2`.`item_quantity` ELSE ? END) * (CASE WHEN (`t3`.`price` IS NOT NULL) THEN `t3`.`price` ELSE ? END))) FROM  cart_users AS t0 LEFT JOIN cart_orders AS t1 ON (`t1`.`for_user__email` == `t0`.`email`) LEFT JOIN cart_line_items AS t2 ON (`t2`.`item_in_order__id` == `t1`.`id`) LEFT JOIN cart_products AS t3 ON (`t2`.`item_for_product__id` == `t3`.`id`) GROUP BY `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password` ORDER BY SUM(((CASE WHEN (`t2`.`item_quantity` IS NOT NULL) THEN `t2`.`item_quantity` ELSE ? END) * (CASE WHEN (`t3`.`price` IS NOT NULL) THEN `t3`.`price` ELSE ? END))) DESC with [SqlInt64 0,SqlInt64 0,SqlInt64 0,SqlInt64 0]
< (User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"},27000)
< (User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},16500)
< (User {_userEmail = "sam@example.com", _userFirstName = "Sam", _userLastName = "Taylor", _userPassword = "332532dcfaa1cbf61e2a266bd723612c"},0)
< ----

Queries with nullable foreign keys
=======

Recall that our schema contains a nullable foreign key from `OrderT` to `ShippingInfoT`. Above,
we've seen how `leftJoin_` introduces nullable tables into our queries. Below, we'll see how to use
nullable primary keys to optionally include information.

Suppose we want to find all orders who have not been shipped. We can do this by simply writing a query over the orders.

>           putStrLn "All orders with no shipping information"
>           Success allUnshippedOrders <-
>               beamTxn beam $ \db ->
>                 queryList $
>                 do order <- all_ (_shoppingCartOrders db)
>                    guard_ (isNothing_ (_orderShippingInfo order))
>                    pure order
>           mapM_ (putStrLn . show) allUnshippedOrders
>           putStrLn "----\n\n"

Let's count up all shipped and unshipped orders by user, including users who have no orders.

>           putStrLn "Shipped and unshipped orders by user. Tuples returned like (user, unshipped, shipped)"
>           Success shippingInformationByUser <-
>               beamTxn beam $ \db ->
>                 queryList $
>                 aggregate (\(user, order) ->
>                                let ShippingInfoId shippingInfoId = _orderShippingInfo order
>                                in ( group_ user
>                                   , count_ (maybe_ (just_ 1) (\_ -> nothing_) shippingInfoId :: QExpr _ _ (Maybe Int))
>                                   , count_ shippingInfoId )) $
>                 do user <- all_ (_shoppingCartUsers db)
>
>                    order <- perhapsAll_ (_shoppingCartOrders db) (\order -> _orderForUser order `references_` just_ user)
>                    pure (user, order)
>           mapM_ (putStrLn . show) shippingInformationByUser
>           putStrLn "----\n\n"

Uh-oh! There's an error in the result set!

< Shipped and unshipped orders by user. Tuples returned like (user, unshipped, shipped)
< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password`, COUNT((CASE WHEN (`t1`.`shipping_info__info_id` IS NOT NULL) THEN ? ELSE ? END)), COUNT(`t1`.`shipping_info__info_id`) FROM  cart_users AS t0 LEFT JOIN cart_orders AS t1 ON (`t1`.`for_user__email` == `t0`.`email`) GROUP BY `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password` with [SqlNull,SqlInt64 1]
< (User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},0,1)
< (User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"},2,0)
< (User {_userEmail = "sam@example.com", _userFirstName = "Sam", _userLastName = "Taylor", _userPassword = "332532dcfaa1cbf61e2a266bd723612c"},1,0)
< ----

Here we hit one of the limitations of beam's mapping to SQL, and really one of the limitations of
SQL itself. Notice that the type of `shippingInfoId` in the aggregate expression is `QExpr s (Maybe
(Maybe Int))` because based on the Haskell query we constructed, `shippingInfoId` could be nullable
at two levels: either it's null in the order, or it's null because the entire order is nothing. SQL
makes no distinction between these two values, since they're both returned as `NULL`. When beam
deserializes a `NULL` in a `Maybe` field, the outermost `Maybe` is the one populated with
`Nothing`. Thus it is impossible to retrieve a value like `Just Nothing` from the database using the
default serializers and deserializers. In general, it's best to avoid highly nested `Maybe`s in
your queries because it makes them more difficult to understand.

One way to work around this issue in the above query is to use subqueries.

>           putStrLn "Shipped and unshipped orders by user, with correct double maybe handling. Tuples returned like (user, unshipped, shipped)."
>           Success shippingInformationByUserCorrect <-
>               beamTxn beam $ \db ->
>                 queryList $
>                 do user <- all_ (_shoppingCartUsers db)
>                    (userEmail, unshippedCount) <- subquery_ $
>                                                   aggregate (\(userEmail, order) -> (group_ userEmail, count_ (_orderId order))) $
>                                                   do user <- all_ (_shoppingCartUsers db)
>                                                      order <- perhapsAll_ (_shoppingCartOrders db) (\order -> _orderForUser order `references_` just_ user &&.
>                                                                                                               isNothing_ (_orderShippingInfo order))
>                                                      pure (_userEmail user, order)
>
>                    guard_ (_userEmail user ==. userEmail)
>
>                    (userEmail, shippedCount) <- subquery_ $
>                                                 aggregate (\(userEmail, order) -> (group_ userEmail, count_ (_orderId order))) $
>                                                 do user <- all_ (_shoppingCartUsers db)
>                                                    order <- perhapsAll_ (_shoppingCartOrders db) (\order -> _orderForUser order `references_` just_ user &&.
>                                                                                                             isJust_ (_orderShippingInfo order))
>                                                    pure (_userEmail user, order)
>                    guard_ (_userEmail user ==. userEmail)
>
>                    pure (user, unshippedCount, shippedCount)
>           mapM_ (putStrLn . show) shippingInformationByUserCorrect
>           putStrLn "----\n\n"

And now we get the expected result

< Shipped and unshipped orders by user, with correct double maybe handling. Tuples returned like (user, unshipped, shipped).
< Will execute SELECT `t0`.`email`, `t0`.`first_name`, `t0`.`last_name`, `t0`.`password`, `t3`.`e1`, `t6`.`e1` FROM  cart_users AS t0 INNER JOIN (SELECT `t1`.`email` AS e0, COUNT(`t2`.`id`) AS e1 FROM  cart_users AS t1 LEFT JOIN cart_orders AS t2 ON ((`t2`.`for_user__email` == `t1`.`email`) AND (`t2`.`shipping_info__info_id` IS NULL)) GROUP BY `t1`.`email`) AS t3 INNER JOIN (SELECT `t4`.`email` AS e0, COUNT(`t5`.`id`) AS e1 FROM  cart_users AS t4 LEFT JOIN cart_orders AS t5 ON ((`t5`.`for_user__email` == `t4`.`email`) AND (`t5`.`shipping_info__info_id` IS NOT NULL)) GROUP BY `t4`.`email`) AS t6 WHERE ((`t0`.`email` == `t3`.`e0`) AND (`t0`.`email` == `t6`.`e0`)) with []
< (User {_userEmail = "betty@example.com", _userFirstName = "Betty", _userLastName = "Jones", _userPassword = "82b054bd83ffad9b6cf8bdb98ce3cc2f"},0,1)
< (User {_userEmail = "james@example.com", _userFirstName = "James", _userLastName = "Smith", _userPassword = "b4cc344d25a2efe540adbf2678e2304c"},2,0)
< (User {_userEmail = "sam@example.com", _userFirstName = "Sam", _userLastName = "Taylor", _userPassword = "332532dcfaa1cbf61e2a266bd723612c"},0,0)
< ----

Conclusion
=======

This tutorial completes our sequence on creating a shopping cart. Throughout the tutorials, we saw
how to create tables using regular Haskell data types, how to link those tables up using relations,
how to query tables using both the monadic interface and the list-like functions on queries. We saw
ha few examples of using beam to generate advanced queries. More information on the Beam API is
havailable on [hackage](http://hackage.haskell.org/package/beam). Happy beaming!

Beam is a work in progress. Please submit bugs and patches on [GitHub](https://github.com/tathougies/beam).
