module Main where

import qualified Database.Postgres.Temp as TempDb
import Test.Tasty

import qualified Database.Beam.Postgres.Test.Select as Select
import qualified Database.Beam.Postgres.Test.Marshal as Marshal
import qualified Database.Beam.Postgres.Test.DataTypes as DataType
import qualified Database.Beam.Postgres.Test.Migrate as Migrate

main :: IO ()
main = defaultMain $ withDb $ \getDb ->
  let getConnStr = TempDb.toConnectionString <$> getDb
  in testGroup "beam-postgres tests"
      [ Marshal.tests getConnStr
      , Select.tests getConnStr
      , DataType.tests getConnStr
      , Migrate.tests getConnStr
      ]
  where
    withDb = withResource
      (either (\e -> error $ "Failed to start DB: " <> show e) pure =<< TempDb.startConfig mempty)
      TempDb.stop
