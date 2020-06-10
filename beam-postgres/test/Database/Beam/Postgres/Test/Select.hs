module Database.Beam.Postgres.Test.Select where

import           Data.ByteString (ByteString)

import           Test.Tasty

tests :: IO ByteString -> TestTree
tests _ =
    testGroup "Postgres Select Tests" []
