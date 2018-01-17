{-# LANGUAGE MultiParamTypeClasses #-}

-- ! BUILD_COMMAND: stack runhaskell --package sqlite-simple --package beam-sqlite --package beam-core -- -fglasgow-exts -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XPartialTypeSignatures
-- ! BUILD_DIR: beam-sqlite/examples/
module Main where

import Database.Beam hiding (withDatabaseDebug)
import qualified Database.Beam as Beam
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Sqlite
import Database.SQLite.Simple

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

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving Generic

instance Database ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

main :: IO ()
main =
  do conn <- open ":memory:"
     execute_ conn "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"

     withDatabase conn $ runInsert $
       insert (_shoppingCartUsers shoppingCartDb) $
       insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                    , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                    ,  User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                    , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                    , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

     let onStmt s = pure ()

         withDatabaseDebug _ q = Beam.withDatabaseDebug onStmt q
         putStrLn :: String -> IO ()
         putStrLn x = putStr (concatMap (\x -> if x == '\n' then "\n\n" else [x]) x ++ "\n --\n")

     BEAM_PLACEHOLDER

