{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- ! BUILD_COMMAND: stack runhaskell --package postgresql-simple --package beam-postgres --package beam-core -- -fglasgow-exts -XTypeFamilies -XOverloadedStrings -XPartialTypeSignatures -XTypeApplications -i../../beam-sqlite/examples -fno-warn-partial-type-signatures
-- ! BUILD_DIR: beam-postgres/examples/

module Main where

import Prelude hiding (lookup)

import Database.Beam
import Database.Beam.Backend.Types
import Database.Beam.Postgres hiding (insert, runInsert)
import qualified Database.Beam.Postgres as Pg
import Database.PostgreSQL.Simple

import Control.Monad
import Control.Exception

import Data.IORef
import Data.Monoid
import Data.Int

import Chinook.Schema

data BeamDone = BeamDone
  deriving (Show)
instance Exception BeamDone

exampleQuery :: (String -> Pg ()) -> Pg ()
exampleQuery putStrLn = do
  BEAM_PLACEHOLDER

main :: IO ()
main =
  do chinook <- connectPostgreSQL "dbname=chinook"

     stmts <- newIORef id

     let onStmt s = modifyIORef stmts (. (s:))
         record a = withDatabaseDebug (onStmt . (++ ";")) chinook a

     handle (\BeamDone -> pure ()) $
       withTransaction chinook $ do
         record $ exampleQuery (liftIO . onStmt . ("-- Output: " ++))
         throwIO BeamDone

     mkStmtList <- readIORef stmts
     let stmtList = mkStmtList []

     forM_ stmtList $ \stmt -> do
       putStrLn stmt
