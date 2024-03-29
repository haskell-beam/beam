{-# LANGUAGE MultiParamTypeClasses #-}

-- ! BUILD_COMMAND: runhaskell --ghc-arg=-fglasgow-exts -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XPartialTypeSignatures -fno-warn-partial-type-signatures
-- ! BUILD_DIR: beam-sqlite/examples/
-- ! FORMAT: console
module Main where

import Database.Beam hiding (withDatabaseDebug)
import qualified Database.Beam as Beam
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Sqlite hiding (runBeamSqliteDebug)
import qualified Database.Beam.Sqlite as Sqlite
import Database.SQLite.Simple

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

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT) }
                        deriving Generic
instance Database be ShoppingCartDb

shoppingCartDb :: DatabaseSettings Sqlite ShoppingCartDb
shoppingCartDb = defaultDbSettings

main :: IO ()
main =
  do conn <- open ":memory:"
     execute_ conn "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"

     runBeamSqlite conn $ runInsert $
       insert (_shoppingCartUsers shoppingCartDb) $
       insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                    , User "sam@example.com"   "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

     let runBeamSqliteDebug _ = Sqlite.runBeamSqlite

     BEAM_PLACEHOLDER

