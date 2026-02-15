module Main (main) where

import qualified Database.Beam.DuckDB.Test.Query (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "beam-duckdb tests"
            [Database.Beam.DuckDB.Test.Query.tests]
