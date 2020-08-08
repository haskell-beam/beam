{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module Database.Beam.Postgres.Test.DataTypes where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Test
import Database.Beam.Migrate
import Database.Beam.Backend.SQL.BeamExtensions

import Control.Exception (SomeException(..), handle)

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

tests :: IO ByteString -> TestTree
tests postgresConn =
    testGroup "Data-type unit tests"
    [ jsonNulTest postgresConn
    , errorOnSchemaMismatch postgresConn ]

data JsonT f
    = JsonT
    { _key :: C f Int32
    , _field1 :: C f (PgJSON String) }
    deriving (Generic, Beamable)

instance Table JsonT where
    data PrimaryKey JsonT f = JsonKey (C f Int32)
      deriving (Generic, Beamable)
    primaryKey = JsonKey <$> _key

data JsonDb entity
    = JsonDb
    { jsonTable :: entity (TableEntity JsonT) }
    deriving (Generic, Database Postgres)

-- | Regression test for <https://github.com/haskell-beam/beam/issues/297 #297>
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
            orderBy_ (\(JsonT k _) -> asc_ k) $
            all_ (jsonTable db)

      readback @?= [ "hello\0world"
                   , "\0\0\0"
                   , "\0hello"
                   , "hello\0"
                   , "\0hello\0"
                   , "\0he\0\0llo\0" ]

      return ()

data TblT f
    = Tbl { _tblKey :: C f Int32, _tblValue :: C f Text }
      deriving (Generic, Beamable)

deriving instance Show (TblT Identity)
deriving instance Eq (TblT Identity)

instance Table TblT where
    data PrimaryKey TblT f = TblKey (C f Int32)
      deriving (Generic, Beamable)
    primaryKey = TblKey <$> _tblKey

data WrongTblT f
    = WrongTbl { _wrongTblKey :: C f Int32, _wrongTblValue :: C f Int32 }
      deriving (Generic, Beamable)

instance Table WrongTblT where
    data PrimaryKey WrongTblT f = WrongTblKey (C f Int32)
      deriving (Generic, Beamable)
    primaryKey = WrongTblKey <$> _wrongTblKey

data RealDb entity
    = RealDb { _realTbl :: entity (TableEntity TblT) }
      deriving (Generic, Database Postgres)

data WrongDb entity
    = WrongDb { _wrongTbl :: entity (TableEntity WrongTblT) }
      deriving (Generic, Database Postgres)

-- | Regression test for <https://github.com/haskell-beam/beam/issues/112>
errorOnSchemaMismatch :: IO ByteString -> TestTree
errorOnSchemaMismatch pgConn =
    testCase "runInsertReturningList should error on schema mismatch (#112)" $
    withTestPostgres "db_failures" pgConn $ \conn -> do
      vs <-
        runBeamPostgres conn $ do
          realDb <- fmap unCheckDatabase $ executeMigration runNoReturn
            (RealDb <$> createTable "tbl1" (Tbl (field "key" int notNull)
                                                (field "value" (varchar Nothing) notNull)))

          runInsertReturningList $ insert (_realTbl realDb) $ insertValues [ Tbl 1 "hello", Tbl 2 "world", Tbl 3 "foo" ]

      vs @?= [ Tbl 1 "hello", Tbl 2 "world", Tbl 3 "foo" ]

      let wrongDb = unCheckDatabase $
                    runMigrationSilenced (WrongDb <$> createTable "tbl1"
                                                        (WrongTbl (field "key" int notNull)
                                                                  (field "value" int notNull)))

      didFail <- handle (\(_ :: SomeException) -> pure True) $
        runBeamPostgres conn $ do
          _ <- runInsertReturningList $ insert (_wrongTbl wrongDb) $ insertValues [ WrongTbl 4 23, WrongTbl 5 24, WrongTbl 6 24 ]
          pure False

      assertBool "runInsertReturningList succeeded" didFail
      didFail @?= True
