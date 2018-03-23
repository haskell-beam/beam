{-# LANGUAGE MultiParamTypeClasses #-}

-- ! BUILD_OPTIONS: -fglasgow-exts -XTypeFamilies -XOverloadedStrings -XPartialTypeSignatures -XTypeApplications -XStandaloneDeriving -XFlexibleInstances -XMultiParamTypeClasses -XDeriveGeneric -XFlexibleContexts -fno-warn-partial-type-signatures -i$$BEAM_SOURCE$$/beam-sqlite/examples/
-- ! BUILD_DIR: beam-sqlite/examples/
module Main where

import Database.Beam
import Database.Beam.Backend.Types
BEAM_MODULE_IMPORT

import Control.Monad
import Control.Exception

import Data.IORef
import Data.Monoid ((<>))

import Chinook.Schema

data BeamDone = BeamDone
  deriving (Show)
instance Exception BeamDone

BEAM_BACKEND_EXTRA

exampleQuery :: (String -> BEAM_BACKEND_MONAD ()) -> BEAM_BACKEND_MONAD ()
exampleQuery putStrLn = do
  BEAM_PLACEHOLDER

main :: IO ()
main =
  do
     BEAM_OPEN_DATABASE

     stmts <- newIORef id

     let onStmt s = modifyIORef stmts (. (s:))
         record a = withDatabaseDebug (onStmt . (++ ";")) chinook a

     handle (\BeamDone -> pure ()) $
       docsWithTransaction chinook $ do
         record $ exampleQuery (liftIO . onStmt . ("-- Output: " ++))
         throwIO BeamDone

     mkStmtList <- readIORef stmts
     let stmtList = mkStmtList []

     forM_ stmtList $ \stmt -> do
       putStrLn stmt
