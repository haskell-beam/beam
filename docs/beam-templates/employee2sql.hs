{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- ! BUILD_COMMAND: stack runhaskell --package sqlite-simple --package beam-sqlite --package beam-core --package microlens -- -fglasgow-exts -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XPartialTypeSignatures -fno-warn-partial-type-signatures
-- ! BUILD_DIR: beam-sqlite/examples/
-- ! FORMAT: sql
module Main where

import Prelude hiding (lookup)

import Database.Beam hiding (withDatabaseDebug)
import qualified Database.Beam as Beam
import Database.Beam.Sqlite hiding (runBeamSqliteDebug)
import qualified Database.Beam.Sqlite as Sqlite
import Database.SQLite.Simple

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
deriving instance Show User
deriving instance Eq User

instance Beamable UserT
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

type UserId = PrimaryKey UserT Identity

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

instance Beamable AddressT
instance Beamable (PrimaryKey AddressT)

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers         :: f (TableEntity UserT)
                      , _shoppingCartUserAddresses :: f (TableEntity AddressT) }
                        deriving Generic

instance Database be ShoppingCartDb

shoppingCartDb :: DatabaseSettings Sqlite ShoppingCartDb
shoppingCartDb = defaultDbSettings `withDbModification`
                 dbModification {
                   _shoppingCartUserAddresses =
                     modifyTable (\_ -> "addresses") $
                     tableModification {
                       _addressLine1 = fieldNamed "address1",
                       _addressLine2 = fieldNamed "address2"
                     }
                 }

shoppingCartDb1 :: DatabaseSettings Sqlite ShoppingCartDb
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
     runBeamSqlite conn $ runInsert $
       insert (_shoppingCartUsers shoppingCartDb) $
       insertValues [ james, betty, sam ]

     let addresses = [ Address default_ (val_ "123 Little Street") (val_ Nothing) (val_ "Boston") (val_ "MA") (val_ "12345") (pk james)
                     , Address default_ (val_ "222 Main Street") (val_ (Just "Ste 1")) (val_ "Houston") (val_ "TX") (val_ "8888") (pk betty)
                     , Address default_ (val_ "9999 Residence Ave") (val_ Nothing) (val_ "Sugarland") (val_ "TX") (val_ "8989") (pk betty) ]

     runBeamSqlite conn $ runInsert $
       insert (_shoppingCartUserAddresses shoppingCartDb) $
       insertExpressions addresses

     let runBeamSqliteDebug _ = Sqlite.runBeamSqliteDebug putStrLn


     (do let putStrLn :: String -> IO ()
             putStrLn _ = pure ()

             print _ = pure ()

         BEAM_PLACEHOLDER
       )
