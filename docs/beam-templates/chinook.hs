{-# LANGUAGE MultiParamTypeClasses #-}

-- ! BUILD_COMMAND: stack runhaskell --package sqlite-simple --package beam-sqlite --package beam-core -- -fglasgow-exts -XTypeFamilies -XOverloadedStrings -XPartialTypeSignatures -XTypeApplications
-- ! BUILD_DIR: beam-sqlite/examples/
module Main where

import Database.Beam
import Database.Beam.Backend.Types
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Control.Monad

import Data.IORef

import Chinook.Schema

exampleQuery :: Q SqliteSelectSyntax ChinookDb s _
exampleQuery =
  BEAM_PLACEHOLDER

main :: IO ()
main =
  do chinook <- open "chinook.db"

     stmts <- newIORef id

     let onStmt s = modifyIORef stmts (. (s:))
         record = withDatabaseDebug onStmt chinook

     record $ runSelectReturningList $ select $ exampleQuery

     mkStmtList <- readIORef stmts
     let stmtList = mkStmtList []

     forM_ stmtList $ \stmt -> do
       putStrLn stmt
