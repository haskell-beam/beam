{-# LANGUAGE LambdaCase #-}
module Database.Beam.Postgres.Test.Migrate where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Database.Beam.Postgres.PgCrypto (PgCrypto)
import Database.Beam.Postgres.Test
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple

import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)

import qualified Database.PostgreSQL.Simple as Pg

import Test.Tasty
import Test.Tasty.HUnit

tests :: IO ByteString -> TestTree
tests postgresConn =
    testGroup "Migration tests"
      [ charWidthVerification postgresConn "VARCHAR" varchar
      , charNoWidthVerification postgresConn "VARCHAR" varchar
      , charWidthVerification postgresConn "CHAR" char
      , charNoWidthVerification postgresConn "CHAR" char
      , extensionVerification postgresConn
      , createTableWithSchemaWorks postgresConn
      , dropSchemaWorks postgresConn
      , indexVerification postgresConn
      , uniqueIndexVerification postgresConn
      ]

data CharT f
    = CharT { vcKey :: C f Text }
      deriving (Generic, Beamable)

instance Table CharT where
    data PrimaryKey CharT f = VcKey (C f Text)
      deriving (Generic, Beamable)

    primaryKey = VcKey . vcKey

data CharDb entity
    = CharDb
    { vcTbl :: entity (TableEntity CharT) }
    deriving (Generic, Database Postgres)

data CryptoDb entity
    = CryptoDb
    { cryptoExtension :: entity (PgExtensionEntity PgCrypto) }
    deriving (Generic, Database Postgres)

-- | Verifies that 'verifySchema' correctly checks the width of
-- @VARCHAR@ or @CHAR@ columns.
charWidthVerification :: IO ByteString -> String -> (Maybe Word -> DataType Postgres Text) -> TestTree
charWidthVerification pgConn tyName charTy =
    testCase ("verifySchema correctly checks width of " ++ tyName ++ "(n) columns (#274)") $ do
      withTestPostgres "db_char_width" pgConn $ \conn -> do
        runBeamPostgres conn $ do
          db <- executeMigration runNoReturn
                  (CharDb <$> createTable "char_test"
                                    (CharT (field "key" (charTy (Just 10)) notNull)))

          res <- verifySchema migrationBackend db

          case res of
            VerificationSucceeded -> return ()
            VerificationFailed failures -> fail ("Verification failed: " ++ show failures)

-- | Verifies that 'verifySchema' correctly checks the width of
-- @VARCHAR@ or @CHAR@ columns without any max width
charNoWidthVerification :: IO ByteString -> String -> (Maybe Word -> DataType Postgres Text) -> TestTree
charNoWidthVerification pgConn tyName charTy =
    testCase ("verifySchema correctly checks width of " ++ tyName ++ " columns (#274)") $ do
      withTestPostgres "db_char_no_width" pgConn $ \conn -> do
        runBeamPostgres conn $ do
          db <- executeMigration runNoReturn
                  (CharDb <$> createTable "char_test"
                                    (CharT (field "key" (charTy Nothing) notNull)))

          res <- verifySchema migrationBackend db

          case res of
            VerificationSucceeded -> return ()
            VerificationFailed failures -> fail ("Verification failed: " ++ show failures)

-- | Verifies that 'verifySchema' correctly checks enabled PgCrypto extension
extensionVerification :: IO ByteString -> TestTree
extensionVerification pgConn =
    testCase "verifySchema correctly checks enabled PgCrypto extension" $
      withTestPostgres "db_extension_pgcrypto" pgConn $ \conn ->
        runBeamPostgres conn $ do
          let migration = CryptoDb <$> pgCreateExtension
          dbBefore <- executeMigration (const $ return ()) migration
          resBefore <- verifySchema migrationBackend dbBefore
          case resBefore of
            VerificationSucceeded -> fail "Verification succeeded before migration when it should have failed"
            VerificationFailed _ -> return ()

          dbAfter <- executeMigration runNoReturn migration
          resAfter <- verifySchema migrationBackend dbAfter
          case resAfter of
            VerificationSucceeded -> return ()
            VerificationFailed failures -> fail ("Verification failed: " ++ show failures)


-- | Verifies that 'createTableWithSchema' correctly creates a table
-- with a schema.
createTableWithSchemaWorks :: IO ByteString -> TestTree
createTableWithSchemaWorks pgConn =
    testCase ("createTableWithSchema works correctly") $ do
      withTestPostgres "create_table_with_schema" pgConn $ \conn -> do
        res <- runBeamPostgres conn $ do
          db <- executeMigration runNoReturn $ do
                  internalSchema <- createDatabaseSchema "internal_schema"
                  (CharDb <$> createTableWithSchema (Just internalSchema) "char_test"
                                    (CharT (field "key" (varchar Nothing) notNull)))

          verifySchema migrationBackend db

        case res of
          VerificationSucceeded -> return ()
          VerificationFailed failures -> fail ("Verification failed: " ++ show failures)


-- | Verifies that creating a schema and dropping it works
dropSchemaWorks :: IO ByteString -> TestTree
dropSchemaWorks pgConn =
    testCase ("dropDatabaseSchema works correctly") $ do
      withTestPostgres "drop_schema" pgConn $ \conn -> do
        runBeamPostgres conn $ do
          db <- executeMigration runNoReturn $ do
                  internalSchema <- createDatabaseSchema "internal_schema"
                  willBeDroppedSchema <- createDatabaseSchema "will_be_dropped"
                  db <- (CharDb <$> createTableWithSchema (Just internalSchema) "char_test"
                                    (CharT (field "key" (varchar Nothing) notNull)))
                  dropDatabaseSchema willBeDroppedSchema
                  pure db

          verifySchema migrationBackend db >>= \case
            VerificationFailed failures -> fail ("Verification failed: " ++ show failures)
            VerificationSucceeded -> pure ()

-- Shared table type for index tests

newtype IdxT f = IdxT
  { _idx_value :: C f Int32
  } deriving (Generic, Beamable)

instance Table IdxT where
  newtype PrimaryKey IdxT f = IdxPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = IdxPk . _idx_value

data IdxDb entity = IdxDb
  { _idx_tbl :: entity (TableEntity IdxT)
  } deriving (Generic, Database Postgres)

-- | Verifies that 'verifySchema' correctly detects a secondary index
indexVerification :: IO ByteString -> TestTree
indexVerification pgConn =
    testCase "verifySchema correctly detects a secondary index" $
      withTestPostgres "db_index" pgConn $ \conn -> do
        Pg.execute_ conn "CREATE TABLE idx_tbl (idx_value integer NOT NULL PRIMARY KEY)"
        Pg.execute_ conn "CREATE INDEX idx_tbl_value ON idx_tbl (idx_value)"
        let db :: CheckedDatabaseSettings Postgres IdxDb
            db = defaultMigratableDbSettings `withDbModification`
                  (dbModification @_ @Postgres)
                    { _idx_tbl = addTableIndex "idx_tbl_value" defaultIndexOptions
                                   (\t -> selectorColumnName _idx_value t NE.:| []) }
        runBeamPostgres conn (verifySchema migrationBackend db) >>= \case
          VerificationSucceeded -> return ()
          VerificationFailed failures -> fail ("Verification failed: " ++ show failures)

-- | Verifies that 'verifySchema' correctly detects a UNIQUE secondary index
uniqueIndexVerification :: IO ByteString -> TestTree
uniqueIndexVerification pgConn =
    testCase "verifySchema correctly detects a UNIQUE secondary index" $
      withTestPostgres "db_unique_index" pgConn $ \conn -> do
        Pg.execute_ conn "CREATE TABLE idx_tbl (idx_value integer NOT NULL PRIMARY KEY)"
        Pg.execute_ conn "CREATE UNIQUE INDEX idx_tbl_value_uniq ON idx_tbl (idx_value)"
        let db :: CheckedDatabaseSettings Postgres IdxDb
            db = defaultMigratableDbSettings `withDbModification`
                  (dbModification @_ @Postgres)
                    { _idx_tbl = addTableIndex "idx_tbl_value_uniq" (setUniqueIndexOptions True defaultIndexOptions)
                                   (\t -> selectorColumnName _idx_value t NE.:| []) }
        runBeamPostgres conn (verifySchema migrationBackend db) >>= \case
          VerificationSucceeded -> return ()
          VerificationFailed failures -> fail ("Verification failed: " ++ show failures)