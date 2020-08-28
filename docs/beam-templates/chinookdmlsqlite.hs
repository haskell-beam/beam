{-# LANGUAGE MultiParamTypeClasses #-}

-- ! BUILD_COMMAND: stack runhaskell --package sqlite-simple --package beam-sqlite --package beam-core -- -fglasgow-exts -XTypeFamilies -XOverloadedStrings -XPartialTypeSignatures -XTypeApplications -fno-warn-partial-type-signatures
-- ! BUILD_DIR: beam-sqlite/examples/
module Main where

import Prelude hiding (lookup)

import Database.Beam
import Database.Beam.Backend.Types
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Control.Monad
import Control.Exception

import Data.IORef
import Data.Int

import Chinook.Schema

data BeamDone = BeamDone
  deriving (Show)
instance Exception BeamDone

exampleQuery :: (String -> SqliteM ()) -> SqliteM ()
exampleQuery putStrLn = do
  BEAM_PLACEHOLDER

main :: IO ()
main =
  do chinook <- open "chinook.db"

     stmts <- newIORef id

     let onStmt s = modifyIORef stmts (. (s:))
         record = withDatabaseDebug onStmt chinook

     handle (\BeamDone -> pure ()) $
       withTransaction chinook $ do
         record $ exampleQuery (liftIO . onStmt . ("-- Output: " ++))
         throwIO BeamDone

     mkStmtList <- readIORef stmts
     let stmtList = mkStmtList []

     forM_ stmtList $ \stmt -> do
       putStrLn stmt
