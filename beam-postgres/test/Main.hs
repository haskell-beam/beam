module Main where

import Test.Tasty

import Database.Beam.Postgres.Test (startTempPostgres)
import qualified Database.Beam.Postgres.Test.Select as Select
import qualified Database.Beam.Postgres.Test.Marshal as Marshal

main :: IO ()
main = defaultMain (withResource startTempPostgres snd $ \getConnStr ->
                    testGroup "beam-postgres tests"
                              [ Marshal.tests (fst <$> getConnStr)
                              , Select.tests  (fst <$> getConnStr) ])
