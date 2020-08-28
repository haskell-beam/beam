{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding (lookup)

import Database.Beam hiding (withDatabaseDebug)
import qualified Database.Beam as Beam
import Database.Beam.Backend.SQL
import Database.Beam.Backend.Types
import Database.Beam.Sqlite hiding (runBeamSqliteDebug)
import qualified Database.Beam.Sqlite as Sqlite
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Text.Read
import Data.Time

import Lens.Micro

import Data.Text (Text)
import Data.Int

import Control.Monad

import Data.IORef

data UserT f
    = User
    { _userEmail     :: Columnar f Text
    , _userFirstName :: Columnar f Text
    , _userLastName  :: Columnar f Text
    , _userPassword  :: Columnar f Text }
    deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

data AddressT f = Address
                { _addressId    :: C f Int32
                , _addressLine1 :: C f Text
                , _addressLine2 :: C f (Maybe Text)
                , _addressCity  :: C f Text
                , _addressState :: C f Text
                , _addressZip   :: C f Text

                , _addressForUser :: PrimaryKey UserT f }
                  deriving Generic
type Address = AddressT Identity
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show Address

instance Table AddressT where
    data PrimaryKey AddressT f = AddressId (Columnar f Int32) deriving Generic
    primaryKey = AddressId . _addressId
type AddressId = PrimaryKey AddressT Identity -- For convenience

instance Beamable AddressT
instance Beamable (PrimaryKey AddressT)

data ProductT f = Product
                { _productId          :: C f Int32
                , _productTitle       :: C f Text
                , _productDescription :: C f Text
                , _productPrice       :: C f Int32 {- Price in cents -} }
                  deriving Generic
type Product = ProductT Identity
deriving instance Show Product

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int32)
                               deriving Generic
  primaryKey = ProductId . _productId

instance Beamable ProductT
instance Beamable (PrimaryKey ProductT)
deriving instance Show (PrimaryKey AddressT Identity)

data OrderT f = Order
              { _orderId      :: Columnar f Int32
              , _orderDate    :: Columnar f LocalTime
              , _orderForUser :: PrimaryKey UserT f
              , _orderShipToAddress :: PrimaryKey AddressT f
              , _orderShippingInfo :: PrimaryKey ShippingInfoT (Nullable f) }
                deriving Generic
type Order = OrderT Identity
deriving instance Show Order

instance Table OrderT where
    data PrimaryKey OrderT f = OrderId (Columnar f Int32)
                               deriving Generic
    primaryKey = OrderId . _orderId

instance Beamable OrderT
instance Beamable (PrimaryKey OrderT)

data ShippingCarrier = USPS | FedEx | UPS | DHL
                       deriving (Show, Read, Eq, Ord, Enum)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax
instance FromField ShippingCarrier where
  fromField f = do x <- readMaybe <$> fromField f
                   case x of
                     Nothing -> returnError ConversionFailed f "Could not 'read' value for 'ShippingCarrier'"
                     Just x -> pure x
instance (BeamBackend be, BackendFromField be ShippingCarrier) => FromBackendRow be ShippingCarrier

data ShippingInfoT f = ShippingInfo
                     { _shippingInfoId             :: Columnar f Int32
                     , _shippingInfoCarrier        :: Columnar f ShippingCarrier
                     , _shippingInfoTrackingNumber :: Columnar f Text }
                       deriving Generic
type ShippingInfo = ShippingInfoT Identity
deriving instance Show ShippingInfo

instance Table ShippingInfoT where
    data PrimaryKey ShippingInfoT f = ShippingInfoId (Columnar f Int32)
                                      deriving Generic
    primaryKey = ShippingInfoId . _shippingInfoId

instance Beamable ShippingInfoT
instance Beamable (PrimaryKey ShippingInfoT)
deriving instance Show (PrimaryKey ShippingInfoT (Nullable Identity))

deriving instance Show (PrimaryKey OrderT Identity)
deriving instance Show (PrimaryKey ProductT Identity)

data LineItemT f = LineItem
                 { _lineItemInOrder    :: PrimaryKey OrderT f
                 , _lineItemForProduct :: PrimaryKey ProductT f
                 , _lineItemQuantity   :: Columnar f Int32 }
                   deriving Generic
type LineItem = LineItemT Identity
deriving instance Show LineItem

instance Table LineItemT where
    data PrimaryKey LineItemT f = LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
                                  deriving Generic
    primaryKey = LineItemId <$> _lineItemInOrder <*> _lineItemForProduct

instance Beamable LineItemT
instance Beamable (PrimaryKey LineItemT)


data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f (TableEntity UserT)
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT)
                      , _shoppingCartProducts      :: f (TableEntity ProductT)
                      , _shoppingCartOrders        :: f (TableEntity OrderT)
                      , _shoppingCartShippingInfos :: f (TableEntity ShippingInfoT)
                      , _shoppingCartLineItems     :: f (TableEntity LineItemT) }
                        deriving Generic

instance Database be ShoppingCartDb

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

Address (LensFor addressId)    (LensFor addressLine1)
        (LensFor addressLine2) (LensFor addressCity)
        (LensFor addressState) (LensFor addressZip)
        (UserId (LensFor addressForUserId)) =
        tableLenses

User (LensFor userEmail)    (LensFor userFirstName)
     (LensFor userLastName) (LensFor userPassword) =
     tableLenses

LineItem _ _ (LensFor lineItemQuantity) = tableLenses
Product (LensFor productId) (LensFor productTitle) (LensFor productDescription) (LensFor productPrice) = tableLenses

main :: IO ()
main =
  do conn <- open ":memory:"

     execute_ conn "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
     execute_ conn "CREATE TABLE addresses ( id INTEGER PRIMARY KEY AUTOINCREMENT, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );"
     execute_ conn "CREATE TABLE products ( id INTEGER PRIMARY KEY AUTOINCREMENT, title VARCHAR NOT NULL, description VARCHAR NOT NULL, price INT NOT NULL );"
     execute_ conn "CREATE TABLE orders ( id INTEGER PRIMARY KEY AUTOINCREMENT, date TIMESTAMP NOT NULL, for_user__email VARCHAR NOT NULL, ship_to_address__id INT NOT NULL, shipping_info__id INT);"
     execute_ conn "CREATE TABLE shipping_info ( id INTEGER PRIMARY KEY AUTOINCREMENT, carrier VARCHAR NOT NULL, tracking_number VARCHAR NOT NULL);"
     execute_ conn "CREATE TABLE line_items (item_in_order__id INTEGER NOT NULL, item_for_product__id INTEGER NOT NULL, item_quantity INTEGER NOT NULL)"

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
                    , Product default_ (val_ "Suitcase") (val_ "A hard durable suitcase") (val_ 15000) ]

     (jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase) <-
       runBeamSqlite conn $ do
         runInsert $ insert (shoppingCartDb ^. shoppingCartUsers) $
           insertValues users

         [jamesAddress1, bettyAddress1, bettyAddress2] <-
           runInsertReturningList $
           insertReturning (shoppingCartDb ^. shoppingCartUserAddresses) $ insertExpressions addresses

         [redBall, mathTextbook, introToHaskell, suitcase] <-
           runInsertReturningList $
           insertReturning (shoppingCartDb ^. shoppingCartProducts) $ insertExpressions products

         pure ( jamesAddress1, bettyAddress1, bettyAddress2, redBall, mathTextbook, introToHaskell, suitcase )

     bettyShippingInfo <-
       runBeamSqlite conn $ do
         [bettyShippingInfo] <-
           runInsertReturningList $
           insertReturning (shoppingCartDb ^. shoppingCartShippingInfos) $
           insertExpressions [ ShippingInfo default_ (val_ USPS) (val_ "12345790ABCDEFGHI") ]
         pure bettyShippingInfo

