{-# LANGUAGE MultiParamTypeClasses #-}

-- ! BUILD_OPTIONS: -fglasgow-exts -XTypeFamilies -XOverloadedStrings -XPartialTypeSignatures -XTypeApplications -XStandaloneDeriving -XFlexibleInstances -XMultiParamTypeClasses -XDeriveGeneric -XFlexibleContexts -fno-warn-partial-type-signatures -i$$BEAM_SOURCE$$/beam-sqlite/examples/
-- ! BUILD_DIR: beam-sqlite/examples/
module Main where

import Database.Beam
import Database.Beam.Backend.Types
import qualified Database.Beam.Query.Adhoc as Adhoc
BEAM_MODULE_IMPORT

import Control.Monad
import Control.Exception

import Data.IORef
import Data.Monoid ((<>))
import Data.Int
import Data.Text

import Chinook.Schema

import System.IO

data BeamDone = BeamDone
  deriving (Show)
instance Exception BeamDone

BEAM_BACKEND_EXTRA

exampleQuery :: Q BEAM_BACKEND ChinookDb s _
exampleQuery =
  BEAM_PLACEHOLDER

displayStmtList display stmtsV =
  do mkStmtList <- readIORef stmtsV
     let stmtList = mkStmtList []

     forM_ stmtList $ \stmt ->
       display stmt

main :: IO ()
main =
  do
     BEAM_OPEN_DATABASE

     stmts <- newIORef id

     let onStmt s = modifyIORef stmts (. (s:))

         record :: BEAM_BACKEND_MONAD a -> IO a
         record = BEAM_WITH_DATABASE_DEBUG onStmt chinook

     flip onException (displayStmtList (hPutStrLn stderr) stmts) $
       handle (\BeamDone -> pure ()) $
       docsWithTransaction chinook $ do
         record $ runSelectReturningList $ select $ exampleQuery
         throwIO BeamDone

     displayStmtList putStrLn stmts
