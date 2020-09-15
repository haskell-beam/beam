module Main where

import           Test.Tasty

import           Database.Beam.Postgres.Test            (startTempPostgres)
import qualified Database.Beam.Postgres.Test.DataTypes  as DataType
import qualified Database.Beam.Postgres.Test.EscapeText as EscapeText
import qualified Database.Beam.Postgres.Test.Marshal    as Marshal
import qualified Database.Beam.Postgres.Test.Migrate    as Migrate
import qualified Database.Beam.Postgres.Test.Select     as Select

main :: IO ()
main = defaultMain (withResource startTempPostgres snd $ \getConnStr ->
                    testGroup "beam-postgres tests"
                              [ Marshal.tests (fst <$> getConnStr)
                              , Select.tests  (fst <$> getConnStr)
                              , DataType.tests (fst <$> getConnStr)
                              , Migrate.tests (fst <$> getConnStr) ])
