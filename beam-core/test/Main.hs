module Main where

import           Test.Tasty

import qualified Database.Beam.Test.Schema as Schema
import qualified Database.Beam.Test.SQL as SQL

main :: IO ()
main = defaultMain (testGroup "beam-core tests"
                              [ Schema.tests
                              , SQL.tests ])
