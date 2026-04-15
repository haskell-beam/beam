module Main where

import Test.Tasty

import qualified Database.Beam.Sqlite.Test.Migrate as Migrate
import qualified Database.Beam.Sqlite.Test.Insert as Insert
import qualified Database.Beam.Sqlite.Test.InsertOnConflictReturning as InsertOnConflictReturning
import qualified Database.Beam.Sqlite.Test.Select as Select
import qualified Database.Beam.Sqlite.Test.Returning as Returning
import qualified Database.Beam.Sqlite.Test.TempTable as TempTable

main :: IO ()
main = defaultMain $ testGroup "beam-sqlite tests"
      [ Migrate.tests
      , Select.tests
      , Insert.tests
      , InsertOnConflictReturning.tests
      , Returning.tests
      , TempTable.tests
      ]
