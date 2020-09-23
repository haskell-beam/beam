{-# LANGUAGE LambdaCase #-}

module Database.Beam.Postgres.Test.Select (tests) where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.Text (Text)
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.UUID (UUID, nil)

import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Extensions.UuidOssp

import           Database.Beam.Postgres.Test

tests :: IO ByteString -> TestTree
tests getConn = testGroup "Selection Tests"
  [ testGroup "JSON"
      [ testPgArrayToJSON getConn
      ]
  , testGroup "UUID"
      [ testUuidFunction getConn "uuid_nil" pgUuidNil
      , testUuidFunction getConn "uuid_ns_dns" pgUuidNsDns
      , testUuidFunction getConn "uuid_ns_url" pgUuidNsUrl
      , testUuidFunction getConn "uuid_ns_oid" pgUuidNsOid
      , testUuidFunction getConn "uuid_ns_x500" pgUuidNsX500
      , testUuidFunction getConn "uuid_generate_v1" pgUuidGenerateV1
      , testUuidFunction getConn "uuid_generate_v1mc" pgUuidGenerateV1Mc
      , testUuidFunction getConn "uuid_generate_v3" $ \ext ->
          pgUuidGenerateV3 ext (val_ nil) "nil"
      , testUuidFunction getConn "uuid_generate_v4" pgUuidGenerateV4
      , testUuidFunction getConn "uuid_generate_v5" $ \ext ->
          pgUuidGenerateV5 ext (val_ nil) "nil"
      ]
  , testInRowValues getConn
  , testReturningMany getConn
  ]

testPgArrayToJSON :: IO ByteString -> TestTree
testPgArrayToJSON getConn = testFunction getConn "array_to_json" $ \conn -> do
  let values :: [Int32] = [1, 2, 3]
  actual :: [PgJSON Value] <-
    runBeamPostgres conn $ runSelectReturningList $ select $
      return $ pgArrayToJson $ val_ $ V.fromList values
  assertEqual "JSON list" [PgJSON $ toJSON values] actual

data UuidSchema f = UuidSchema
  { _uuidOssp :: f (PgExtensionEntity UuidOssp)
  } deriving (Generic, Database Postgres)

testUuidFunction
  :: IO ByteString
  -> String
  -> (forall s. UuidOssp -> QExpr Postgres s UUID)
  -> TestTree
testUuidFunction getConn name mkUuid = testFunction getConn name $ \conn ->
  runBeamPostgres conn $ do
    db <- executeMigration runNoReturn $ UuidSchema <$>
      pgCreateExtension @UuidOssp
    [_] <- runSelectReturningList $ select $
      return $ mkUuid $ getPgExtension $ _uuidOssp $ unCheckDatabase db
    return ()

data Pair f = Pair
  { _left :: C f Bool
  , _right :: C f Bool
  } deriving (Generic, Beamable)

testInRowValues :: IO ByteString -> TestTree
testInRowValues getConn = testCase "IN with row values works" $
  withTestPostgres "db_in_row_values" getConn $ \conn -> do
    result <- runBeamPostgres conn $ runSelectReturningList $ select $ do
      let p :: forall ctx s. Pair (QGenExpr ctx Postgres s)
          p = val_ $ Pair False False
      return $ p `in_` [p, p]
    assertEqual "result" [True] result

testReturningMany :: IO ByteString -> TestTree
testReturningMany getConn = testCase "runReturningMany (batching via cursor) works" $
  withTestPostgres "run_returning_many_cursor" getConn $ \conn -> do
    result <- runBeamPostgres conn $ runSelectReturningMany
      (select $ pgUnnestArray $ array_ $ (as_ @Int32 . val_) <$> [0..rowCount - 1])
      (\fetch ->
        let count n = fetch >>= \case
              Nothing -> pure n
              Just _  -> count (n + 1)
        in count 0)
    assertEqual "result" rowCount result
 where
  rowCount = 500
  runSelectReturningMany ::
    (FromBackendRow Postgres x) =>
    SqlSelect Postgres x -> (Pg (Maybe x) -> Pg a) -> Pg a
  runSelectReturningMany (SqlSelect s) =
    runReturningMany (selectCmd s)

testFunction :: IO ByteString -> String -> (Connection -> Assertion) -> TestTree
testFunction getConn name mkAssertion = testCase name $
  withTestPostgres name getConn mkAssertion
