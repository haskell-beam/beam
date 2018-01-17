{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- ! BUILD_COMMAND: stack runhaskell --package sqlite-simple --package beam-sqlite --package beam-core --package microlens -- -fglasgow-exts -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XPartialTypeSignatures
-- ! BUILD_DIR: beam-sqlite/examples/
module Main where

import Prelude hiding (lookup)

import Database.Beam hiding (withDatabaseDebug)
import qualified Database.Beam as Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Lens.Micro

import Data.Text (Text)

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
deriving instance Show User
deriving instance Eq User

instance Beamable UserT
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

type UserId = PrimaryKey UserT Identity

data AddressT f = Address
                { _addressId    :: C f (Auto Int)
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
    data PrimaryKey AddressT f = AddressId (Columnar f (Auto Int)) deriving Generic
    primaryKey = AddressId . _addressId

instance Beamable AddressT
instance Beamable (PrimaryKey AddressT)

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f (TableEntity UserT)
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT) }
                        deriving Generic

instance Database ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification`
                 dbModification {
                   _shoppingCartUserAddresses =
                     modifyTable (\_ -> "addresses") $
                     tableModification {
                       _addressLine1 = fieldNamed "address1",
                       _addressLine2 = fieldNamed "address2"
                     }
                 }

shoppingCartDb1 :: DatabaseSettings be ShoppingCartDb
shoppingCartDb1 = defaultDbSettings `withDbModification`
                  dbModification {
                    _shoppingCartUsers = modifyTable (\_ -> "users") tableModification,
                    _shoppingCartUserAddresses = modifyTable (\_ -> "user_addresses") tableModification
                  }

Address (LensFor addressId)    (LensFor addressLine1)
        (LensFor addressLine2) (LensFor addressCity)
        (LensFor addressState) (LensFor addressZip)
        (UserId (LensFor addressForUserId)) = 
        tableLenses

User (LensFor userEmail)    (LensFor userFirstName)
     (LensFor userLastName) (LensFor userPassword) =
     tableLenses

ShoppingCartDb (TableLens shoppingCartUsers)
               (TableLens shoppingCartUserAddresses) =
               dbLenses

main :: IO ()
main =
  do conn <- open ":memory:"
     execute_ conn "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
     execute_ conn "CREATE TABLE addresses ( id INTEGER PRIMARY KEY AUTOINCREMENT, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );"

     let james = User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
         betty = User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
         sam = User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"
     withDatabase conn $ runInsert $
       insert (_shoppingCartUsers shoppingCartDb) $
       insertValues [ james, betty, sam ]

     stmts <- newIORef id
     let onStmt s = modifyIORef stmts (. (s:))

         withDatabaseDebug _ q = Beam.withDatabaseDebug onStmt q
         putStrLn :: String -> IO ()
         putStrLn _ = pure ()
         print :: a -> IO ()
         print _ = pure ()

     let addresses = [ Address (Auto Nothing) "123 Little Street" Nothing "Boston" "MA" "12345" (pk james)
                     , Address (Auto Nothing) "222 Main Street" (Just "Ste 1") "Houston" "TX" "8888" (pk betty)
                     , Address (Auto Nothing) "9999 Residence Ave" Nothing "Sugarland" "TX" "8989" (pk betty) ]

     withDatabase conn $ runInsert $
       insert (_shoppingCartUserAddresses shoppingCartDb) $
       insertValues addresses

     BEAM_PLACEHOLDER

     stmts_ <- readIORef stmts
     forM_ (stmts_ []) $ \stmt ->
       putStr (stmt ++ "\n")
