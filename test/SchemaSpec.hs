module SchemaSpec where

import Test.Hspec

import Database.Beam
import Database.Beam.Backend.Sqlite3

import Data.Text (Text)

data UserT f = User
  { _userEmail :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName :: Columnar f Text
  , _userPassword :: Columnar f Text
  } deriving Generic

type User = UserT Identity
deriving instance Show User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail

type UserId = PrimaryKey UserT Identity
deriving instance Show UserId

data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers :: f UserT } deriving Generic

shoppingCartDb :: DatabaseSettings ShoppingCartDb
shoppingCartDb = autoDbSettings

instance Database ShoppingCartDb
                      
userSchema = unwords
  [ "CREATE TABLE cart_users (email VARCHAR NOT NULL,"
  , "first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL,"
  , "password VARCHAR NOT NULL, PRIMARY KEY( email ))\n"
  ]

spec :: Spec
spec = 
  describe "Database.Beam" $ do
    describe "AutoMigrate" $ do
      it "creates a sane schema from data types" $ do
        beam <- openDatabaseDebug shoppingCartDb AutoMigrate $ 
          Sqlite3Settings "shoppingcart1.db"
        showSchema shoppingCartDb `shouldReturn` userSchema
