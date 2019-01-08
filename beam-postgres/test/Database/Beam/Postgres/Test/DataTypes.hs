{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Database.Beam.Postgres.Test.DataTypes where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Database.Beam.Postgres.Test
import Database.Beam.Migrate

import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.HUnit

tests :: IO ByteString -> TestTree
tests postgresConn =
    testGroup "Data-type unit tests"
    [ jsonNulTest postgresConn ]

data JsonT f
    = JsonT
    { _key :: C f Int
    , _field1 :: C f (PgJSON String) }
    deriving (Generic, Beamable)

instance Table JsonT where
    data PrimaryKey JsonT f = JsonKey (C f Int)
      deriving (Generic, Beamable)
    primaryKey = JsonKey <$> _key

data JsonDb entity
    = JsonDb
    { jsonTable :: entity (TableEntity JsonT) }
    deriving (Generic, Database Postgres)

-- | Regression test for <https://github.com/tathougies/beam/issues/297 #297>
jsonNulTest :: IO ByteString -> TestTree
jsonNulTest pgConn =
    testCase "JSON NUL handling (#297)" $
    withTestPostgres "db_jsonnul" pgConn $ \conn -> do
      readback <-
        runBeamPostgres conn $ do
          db <- fmap unCheckDatabase $
                executeMigration runNoReturn
                (JsonDb <$> createTable "json_test"
                              (JsonT (field "key" int notNull)
                                     (field "value" json notNull)))

          runInsert $ insert (jsonTable db) $
            insertValues [ JsonT 1 (PgJSON "hello\0world") ]
          runInsert $ insert (jsonTable db) $
            insertValues [ JsonT 2 (PgJSON "\0\0\0") ]
          runInsert $ insert (jsonTable db) $
            insertValues [ JsonT 3 (PgJSON "\0hello") ]
          runInsert $ insert (jsonTable db) $
            insertValues [ JsonT 4 (PgJSON "hello\0") ]
          runInsert $ insert (jsonTable db) $
            insertValues [ JsonT 5 (PgJSON "\0hello\0") ]
          runInsert $ insert (jsonTable db) $
            insertValues [ JsonT 6 (PgJSON "\0he\0\0llo\0") ]

          fmap (fmap (\(PgJSON v) -> v)) $
            runSelectReturningList $ select $
            fmap (\(JsonT _ v) -> v) $
            orderBy_ (\(JsonT pk _) -> asc_ pk) $
            all_ (jsonTable db)

      readback @?= [ "hello\0world"
                   , "\0\0\0"
                   , "\0hello"
                   , "hello\0"
                   , "\0hello\0"
                   , "\0he\0\0llo\0" ]

      return ()
