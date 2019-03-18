module Main where

import Database.SQLite.Simple

import Database.Beam.Sqlite
import Database.Beam

import Chinook.Schema

import Control.Monad

main :: IO ()
main = do
  c <- open "chinook.db"

  res <- runBeamSqliteDebug putStrLn conn $
         runSelectReturningList (select (all_ (album chinookDb)))

  forM_ res $ \row ->
      putStrLn ("Album: " ++ show row)
