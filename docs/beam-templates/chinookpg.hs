{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- ! BUILD_COMMAND: stack runhaskell --package postgresql-simple --package beam-postgres --package beam-core -- -fglasgow-exts -XTypeFamilies -XOverloadedStrings -XPartialTypeSignatures -XTypeApplications -i../../beam-sqlite/examples
-- ! BUILD_DIR: beam-postgres/examples/

module Main where

import Database.Beam
import Database.Beam.Backend.Types
import Database.Beam.Postgres
import Database.PostgreSQL.Simple

import Control.Monad
import Control.Exception

import Data.IORef
import Data.Monoid

import Chinook.Schema

exampleQuery :: Q PgSelectSyntax ChinookDb s _
exampleQuery =
  BEAM_PLACEHOLDER

main :: IO ()
main =
  do chinook <- connectPostgreSQL "dbname=template1"

     stmts <- newIORef id

     let onStmt s = modifyIORef stmts (. (s:))
         record a = withDatabaseDebug onStmt chinook a `catch` (\(SomeException e) -> pure [])

     record $ runSelectReturningList $ select $ exampleQuery

     mkStmtList <- readIORef stmts
     let stmtList = mkStmtList []

     forM_ stmtList $ \stmt -> do
       putStrLn stmt
