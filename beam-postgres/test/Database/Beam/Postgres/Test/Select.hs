{-# LANGUAGE LambdaCase #-}

module Database.Beam.Postgres.Test.Select (tests) where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.UUID (UUID, nil)
import qualified Data.UUID.V5 as V5

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
  , testGroup "ARRAY functions"
      [ testArrayReplace getConn
      , testArrayShuffle getConn
      , testArraySample getConn
      , testArrayToStringBasic getConn
      , testArrayToStringWithNull getConn
      , testArrayAppend getConn
      , testArrayPrepend getConn
      , testArrayRemove getConn
      ]
  , testGroup "UUID"
      [ testUuidFunction getConn "uuid_nil" $ \ext -> pgUuidNil ext
      , testUuidFunction getConn "uuid_ns_dns" $ \ext -> pgUuidNsDns ext
      , testUuidFunction getConn "uuid_ns_url" $ \ext -> pgUuidNsUrl ext
      , testUuidFunction getConn "uuid_ns_oid" $ \ext -> pgUuidNsOid ext
      , testUuidFunction getConn "uuid_ns_x500" $ \ext -> pgUuidNsX500 ext
      , testUuidFunction getConn "uuid_generate_v1" $ \ext ->
          pgUuidGenerateV1 ext
      , testUuidFunction getConn "uuid_generate_v1mc" $ \ext ->
          pgUuidGenerateV1Mc ext
      , testUuidFunction getConn "uuid_generate_v3" $ \ext ->
          pgUuidGenerateV3 ext (val_ nil) "nil"
      , testUuidFunction getConn "uuid_generate_v4" $ \ext ->
          pgUuidGenerateV4 ext
      , testUuidFunction getConn "uuid_generate_v5" $ \ext ->
          pgUuidGenerateV5 ext (val_ nil) "nil"
      , testUuuidInValues getConn
      ]
  , testInRowValues getConn
  , testInSelect getConn
  , testReturningMany getConn
  , testPgUnnest getConn
  ]

testPgArrayToJSON :: IO ByteString -> TestTree
testPgArrayToJSON getConn = testFunction getConn "array_to_json" $ \conn -> do
  let values :: [Int32] = [1, 2, 3]
  actual :: [PgJSON Value] <-
    runBeamPostgres conn $ runSelectReturningList $ select $
      return $ pgArrayToJson $ val_ $ V.fromList values
  assertEqual "JSON list" [PgJSON $ toJSON values] actual

testArrayReplace :: IO ByteString -> TestTree
testArrayReplace getConn = testFunction getConn "array_replace" $ \conn -> do
  let arr = V.fromList [1::Int32,2,5,4]
  res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    pure $ arrayReplace_ (val_ arr) (val_ (5::Int32)) (val_ (3::Int32))
  assertEqual "array_replace" [V.fromList [1,2,3,4 :: Int32]] res

testArrayShuffle :: IO ByteString -> TestTree
testArrayShuffle getConn = testFunction getConn "array_shuffle" $ \conn -> do
  let arr = V.fromList [1::Int32,2,3,4,5]
  res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    pure $ arrayShuffle_ (val_ arr)
  -- shuffled result has same length and elements, order may change
  case res of
    [shuf] -> do
      assertEqual "length" (V.length arr) (V.length shuf)
      assertBool "is permutation"
        (sort (V.toList arr) == sort (V.toList shuf))
    _ -> assertFailure "unexpected result"

testArraySample :: IO ByteString -> TestTree
testArraySample getConn = testFunction getConn "array_sample" $ \conn -> do
  let arr = V.fromList [1::Int32,2,3,4,5,6]
  res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    pure $ arraySample_ (val_ arr) (val_ (3::Int32))
  case res of
    [samp] -> do
      assertEqual "length 3" 3 (V.length samp)
      assertBool "subset" (V.all (`V.elem` arr) samp)
    _ -> assertFailure "unexpected result"

testArrayToStringBasic :: IO ByteString -> TestTree
testArrayToStringBasic getConn = testFunction getConn "array_to_string_basic" $ \conn -> do
  let arr = V.fromList [1::Int32,2,3]
  res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    pure $ arrayToString_ (val_ arr) (val_ ",")
  assertEqual "join" ["1,2,3" :: T.Text] res

testArrayToStringWithNull :: IO ByteString -> TestTree
testArrayToStringWithNull getConn = testFunction getConn "array_to_string_with_null" $ \conn -> do
  let arr :: V.Vector (Maybe T.Text)
      arr = V.fromList [Just "a", Nothing, Just "b"]
  res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    pure $ arrayToStringWithNull_ (val_ arr) (val_ "-") (val_ "*")
  assertEqual "join with null" ["a-*-b" :: T.Text] res

testArrayAppend :: IO ByteString -> TestTree
testArrayAppend getConn = testFunction getConn "array_append" $ \conn -> do
  let arr = V.fromList [1::Int32,2]
  res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    pure $ arrayAppend_ (val_ arr) (val_ (3::Int32))
  assertEqual "append" [V.fromList [1,2,3 :: Int32]] res

testArrayPrepend :: IO ByteString -> TestTree
testArrayPrepend getConn = testFunction getConn "array_prepend" $ \conn -> do
  let arr = V.fromList [2::Int32,3]
  res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    pure $ arrayPrepend_ (val_ (1::Int32)) (val_ arr)
  assertEqual "prepend" [V.fromList [1,2,3 :: Int32]] res

testArrayRemove :: IO ByteString -> TestTree
testArrayRemove getConn = testFunction getConn "array_remove" $ \conn -> do
  let arr = V.fromList [1::Int32,2,3,2]
  res <- runBeamPostgres conn $ runSelectReturningList $ select $ do
    pure $ arrayRemove_ (val_ arr) (val_ (2::Int32))
  assertEqual "remove" [V.fromList [1,3 :: Int32]] res

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

-- | Regression test for <https://github.com/haskell-beam/beam/issues/555 #555>
testUuuidInValues :: IO ByteString -> TestTree
testUuuidInValues getConn = testCase "UUID in values_ works" $
  withTestPostgres "uuid_values" getConn $ \conn -> do
    result <- runBeamPostgres conn $ do
      db <- executeMigration runNoReturn $ UuidSchema <$>
        pgCreateExtension @UuidOssp
      let ext = getPgExtension $ _uuidOssp $ unCheckDatabase db
      runSelectReturningList $ select $ do
        v <- values_ [val_ nil]
        return $ pgUuidGenerateV5 ext v ""
    assertEqual "result" [V5.generateNamed nil []] result

data Pair f = Pair
  { _left :: C f Bool
  , _right :: C f Bool
  } deriving (Generic, Beamable)

testInRowValues :: IO ByteString -> TestTree
testInRowValues getConn = testCase "IN with row values works" $
  withTestPostgres "db_in_row_values" getConn $ \conn -> do
    result <- runBeamPostgres conn $ runSelectReturningList $ select $ do
      let pair :: forall ctx s. Pair (QGenExpr ctx Postgres s)
          pair = val_ $ Pair False False
      return $ pair `in_` [pair, pair]
    assertEqual "result" [True] result

testInSelect :: IO ByteString -> TestTree
testInSelect getConn = testCase "IN (SELECT ...) works" $
  withTestPostgres "db_in_row_values" getConn $ \conn -> do
    result <- runBeamPostgres conn $ runSelectReturningList $ select $ do
      let x  = as_ @Int32 (val_ 1)
      return $ x `inQuery_` ( pgUnnestArray $ array_ $ (as_ @Int32 . val_) <$> [0..100])
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

-- | Regression test for <https://github.com/haskell-beam/beam/issues/541 #541>
testPgUnnest :: IO ByteString -> TestTree
testPgUnnest getConn = testCase "pgUnnest works" $
  withTestPostgres "pg_unnest" getConn $ \conn -> do
    let values = [Bool True, Number 1]
    result <- runBeamPostgres conn $ runSelectReturningList $ select $
      pgUnnest $ pgJsonArrayElements $ val_ $
        PgJSONB $ Array $ V.fromList values
    assertEqual "result" (PgJSONB <$> values) $ pgJsonElement <$> result