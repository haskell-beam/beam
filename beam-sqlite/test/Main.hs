module Main where

import Test.Tasty

import qualified Database.Beam.Sqlite.Test.Migrate as Migrate

main :: IO ()
main = defaultMain $ testGroup "beam-sqlite tests"
  [ Migrate.tests
  ]
