module Database.Beam.DuckDB.Test.Extensions (tests) where

import qualified Database.Beam.DuckDB.Test.Extensions.Copy as Copy
import qualified Database.Beam.DuckDB.Test.Extensions.DataSource as DataSource
import qualified Database.Beam.DuckDB.Test.Extensions.InsertOnConflict as InsertOnConflict
import qualified Database.Beam.DuckDB.Test.Extensions.Returning as Returning
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Extensions"
    [ Copy.tests,
      DataSource.tests,
      InsertOnConflict.tests,
      Returning.tests
    ]
