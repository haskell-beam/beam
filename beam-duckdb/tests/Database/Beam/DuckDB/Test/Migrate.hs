{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.DuckDB.Test.Migrate (tests) where

import Control.Arrow ((>>>))
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Int (Int32, Int8)
import qualified Data.Text as T
import Data.UUID.Types (UUID)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Beam
  ( Beamable,
    C,
    Database,
    Generic,
    MonadBeam (runNoReturn),
    Table (..),
    TableEntity,
  )
import Database.Beam.DuckDB (DuckDB, runBeamDuckDB)
import Database.Beam.DuckDB.Migrate
  ( blob,
    getDbConstraints,
    migrationBackend,
    text,
    tinyint,
    ubigint,
    uinteger,
    usmallint,
    utinyint,
    uuid,
  )
import Database.Beam.Migrate
  ( CheckedDatabaseSettings,
    DatabasePredicate (..),
    Migration,
    SomeDatabasePredicate (..),
    addColumn,
    alterTable,
    createTable,
    defaultMigratableDbSettings,
    dropColumn,
    executeMigration,
    field,
    migrationStep,
    notNull,
    runMigrationSteps,
  )
import Database.Beam.Migrate.Simple
  ( VerificationResult (..),
    autoMigrate,
    verifySchema,
  )
import Database.Beam.Query.DataTypes (double, int, maybeType, varchar)
import Database.DuckDB.Simple (Connection, execute_)
import qualified Database.DuckDB.Simple as DuckDB
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, testCaseSteps)

tests :: TestTree
tests =
  testGroup
    "Migration tests"
    [ verifiesPrimaryKey,
      verifiesNotNull,
      verifiesForeignKey,
      autoMigrateCreatesSchema,
      autoMigrateIsIdempotent,
      multiStepMigration,
      testGroup
        "Data types"
        [ duckDBSpecificTypesRoundTrip,
          duckDBUnsignedIntegersRoundTrip
        ]
    ]

withInMemory :: (Connection -> IO a) -> IO a
withInMemory = bracket (DuckDB.open ":memory:") DuckDB.close

newtype WithPkT f = WithPkT
  { _with_pk_value :: C f Int32
  }
  deriving (Generic, Beamable)

instance Table WithPkT where
  newtype PrimaryKey WithPkT f = WithPkPk (C f Int32)
    deriving (Generic, Beamable)

  primaryKey = WithPkPk . _with_pk_value

newtype WithPkDb entity = WithPkDb
  { _with_pk :: entity (TableEntity WithPkT)
  }
  deriving (Generic, Database DuckDB)

withPkDbChecked :: CheckedDatabaseSettings DuckDB WithPkDb
withPkDbChecked = defaultMigratableDbSettings

verifiesPrimaryKey :: TestTree
verifiesPrimaryKey =
  testCase "verifySchema correctly detects primary key" $
    withInMemory $ \conn -> do
      void $ execute_ conn "CREATE TABLE with_pk (with_pk_value INTEGER NOT NULL PRIMARY KEY)"
      runBeamDuckDB conn (verifySchema migrationBackend withPkDbChecked)
        >>= \case
          VerificationSucceeded -> pure ()
          VerificationFailed failures ->
            assertFailure $ "Verification failed: " ++ show failures

newtype NotNullT f = NotNullT
  { _nn_value :: C f T.Text
  }
  deriving (Generic, Beamable)

instance Table NotNullT where
  newtype PrimaryKey NotNullT f = NotNullPk (C f T.Text)
    deriving (Generic, Beamable)

  primaryKey = NotNullPk . _nn_value

newtype NotNullDb entity = NotNullDb
  { _nn_tbl :: entity (TableEntity NotNullT)
  }
  deriving (Generic, Database DuckDB)

notNullDbChecked :: CheckedDatabaseSettings DuckDB NotNullDb
notNullDbChecked = defaultMigratableDbSettings

verifiesNotNull :: TestTree
verifiesNotNull =
  testCase "verifySchema correctly detects NOT NULL columns" $
    withInMemory $ \conn -> do
      void $
        execute_
          conn
          "CREATE TABLE nn_tbl (nn_value VARCHAR NOT NULL PRIMARY KEY)"
      runBeamDuckDB conn (verifySchema migrationBackend notNullDbChecked)
        >>= \case
          VerificationSucceeded -> pure ()
          VerificationFailed failures ->
            assertFailure $ "Verification failed: " ++ show failures

newtype FkParentT f = FkParentT
  { _fk_parent_id :: C f Int32
  }
  deriving (Generic, Beamable)

instance Table FkParentT where
  newtype PrimaryKey FkParentT f = FkParentPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = FkParentPk . _fk_parent_id

data FkChildT f = FkChildT
  { _fk_child_id :: C f Int32,
    _fk_child_parent_id :: PrimaryKey FkParentT f
  }
  deriving (Generic, Beamable)

instance Table FkChildT where
  newtype PrimaryKey FkChildT f = FkChildPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = FkChildPk . _fk_child_id

data FkDb entity = FkDb
  { _fk_parent :: entity (TableEntity FkParentT),
    _fk_child :: entity (TableEntity FkChildT)
  }
  deriving (Generic, Database DuckDB)

verifiesForeignKey :: TestTree
verifiesForeignKey =
  testCase "getDbConstraints sees both parent and child tables" $
    withInMemory $ \conn -> do
      void $
        execute_
          conn
          "CREATE TABLE fk_parent (fk_parent_id INTEGER NOT NULL PRIMARY KEY)"
      void $
        execute_
          conn
          "CREATE TABLE fk_child (\
          \  fk_child_id INTEGER NOT NULL PRIMARY KEY,\
          \  fk_child_parent_id INTEGER NOT NULL,\
          \  FOREIGN KEY (fk_child_parent_id) REFERENCES fk_parent (fk_parent_id))"
      preds <- runBeamDuckDB conn getDbConstraints
      assertBool "should see fk_parent in predicates" $
        any (predicateNamesTable "fk_parent") preds
      assertBool "should see fk_child in predicates" $
        any (predicateNamesTable "fk_child") preds
  where
    predicateNamesTable :: T.Text -> SomeDatabasePredicate -> Bool
    predicateNamesTable name (SomeDatabasePredicate p) =
      name `T.isInfixOf` T.pack (englishDescription p)

autoMigrateCreatesSchema :: TestTree
autoMigrateCreatesSchema =
  testCase "autoMigrate creates a schema that round-trips through verifySchema" $
    withInMemory $ \conn -> do
      runBeamDuckDB conn $ autoMigrate migrationBackend withPkDbChecked
      runBeamDuckDB conn (verifySchema migrationBackend withPkDbChecked)
        >>= \case
          VerificationSucceeded -> pure ()
          VerificationFailed failures ->
            assertFailure $ "Verification failed: " ++ show failures
      preds <- runBeamDuckDB conn getDbConstraints
      assertBool "should see with_pk table after autoMigrate" $
        any (predicateNamesTable "with_pk") preds
  where
    predicateNamesTable :: T.Text -> SomeDatabasePredicate -> Bool
    predicateNamesTable name (SomeDatabasePredicate p) =
      name `T.isInfixOf` T.pack (englishDescription p)

autoMigrateIsIdempotent :: TestTree
autoMigrateIsIdempotent =
  testCase "autoMigrate is idempotent" $
    withInMemory $ \conn -> do
      runBeamDuckDB conn $ autoMigrate migrationBackend withPkDbChecked
      runBeamDuckDB conn $ autoMigrate migrationBackend withPkDbChecked
      runBeamDuckDB conn (verifySchema migrationBackend withPkDbChecked) >>= \case
        VerificationSucceeded -> pure ()
        VerificationFailed failures ->
          assertFailure $ "Verification failed: " ++ show failures

data TypesT f = TypesT
  { _types_pk :: C f Int32,
    _types_text :: C f T.Text,
    _types_blob :: C f ByteString,
    _types_tinyint :: C f Int8,
    _types_uuid :: C f UUID
  }
  deriving (Generic, Beamable)

instance Table TypesT where
  newtype PrimaryKey TypesT f = TypesPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = TypesPk . _types_pk

newtype TypesDb entity = TypesDb
  { _types_tbl :: entity (TableEntity TypesT)
  }
  deriving (Generic, Database DuckDB)

duckDBSpecificTypesRoundTrip :: TestTree
duckDBSpecificTypesRoundTrip =
  testCase "verifySchema round-trips schemas that use text/blob/tinyint/uuid" $
    withInMemory $ \conn -> do
      -- Create the table directly with the DuckDB-specific keywords. The
      -- schema we'll verify against, defined below via 'createTable' + 'field',
      -- should round-trip cleanly against information_schema.
      void $
        execute_
          conn
          "CREATE TABLE types_tbl (\
          \ types_pk INTEGER NOT NULL PRIMARY KEY,\
          \ types_text TEXT NOT NULL,\
          \ types_blob BLOB NOT NULL,\
          \ types_tinyint TINYINT NOT NULL,\
          \ types_uuid UUID NOT NULL)"
      let dsl = do
            typesTbl <-
              createTable
                "types_tbl"
                TypesT
                  { _types_pk = field "types_pk" int notNull,
                    _types_text = field "types_text" text notNull,
                    _types_blob = field "types_blob" blob notNull,
                    _types_tinyint = field "types_tinyint" tinyint notNull,
                    _types_uuid = field "types_uuid" uuid notNull
                  }
            pure (TypesDb typesTbl)
      -- Drop the table so executeMigration can re-create it from the DSL,
      -- then verify it matches. This is the standard 'beam-migrate' flow.
      void $ execute_ conn "DROP TABLE types_tbl"
      result <- runBeamDuckDB conn $ do
        db <- executeMigration runNoReturn dsl
        verifySchema migrationBackend db
      case result of
        VerificationSucceeded -> pure ()
        VerificationFailed failures ->
          assertFailure $ "Verification failed: " ++ show failures

data UIntsT f = UIntsT
  { _uints_pk :: C f Int32,
    _uints_utiny :: C f Word8,
    _uints_usmall :: C f Word16,
    _uints_uint :: C f Word32,
    _uints_ubig :: C f Word64
  }
  deriving (Generic, Beamable)

instance Table UIntsT where
  newtype PrimaryKey UIntsT f = UIntsPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = UIntsPk . _uints_pk

newtype UIntsDb entity = UIntsDb
  { _uints_tbl :: entity (TableEntity UIntsT)
  }
  deriving (Generic, Database DuckDB)

duckDBUnsignedIntegersRoundTrip :: TestTree
duckDBUnsignedIntegersRoundTrip =
  testCase "verifySchema round-trips schemas that use utinyint/usmallint/uinteger/ubigint" $
    withInMemory $ \conn -> do
      let dsl = do
            uintsTbl <-
              createTable
                "uints_tbl"
                UIntsT
                  { _uints_pk = field "uints_pk" int notNull,
                    _uints_utiny = field "uints_utiny" utinyint notNull,
                    _uints_usmall = field "uints_usmall" usmallint notNull,
                    _uints_uint = field "uints_uint" uinteger notNull,
                    _uints_ubig = field "uints_ubig" ubigint notNull
                  }
            pure (UIntsDb uintsTbl)
      result <- runBeamDuckDB conn $ do
        db <- executeMigration runNoReturn dsl
        verifySchema migrationBackend db
      case result of
        VerificationSucceeded -> pure ()
        VerificationFailed failures ->
          assertFailure $ "Verification failed: " ++ show failures

data WidgetsV1T f = WidgetsV1T
  { _v1_id :: C f Int32,
    _v1_name :: C f T.Text
  }
  deriving (Generic, Beamable)

instance Table WidgetsV1T where
  newtype PrimaryKey WidgetsV1T f = WidgetsV1Pk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = WidgetsV1Pk . _v1_id

data WidgetsV2T f = WidgetsV2T
  { _v2_id :: C f Int32,
    _v2_name :: C f T.Text,
    -- widgets gains a nullable 'price' column. DuckDB rejects
    -- @ALTER TABLE ... ADD COLUMN ... <constraint>@ with the parser error
    -- "Adding columns with constraints not yet supported", so the added
    -- column is nullable here.
    _v2_price :: C f (Maybe Double)
  }
  deriving (Generic, Beamable)

instance Table WidgetsV2T where
  newtype PrimaryKey WidgetsV2T f = WidgetsV2Pk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = WidgetsV2Pk . _v2_id

newtype WidgetsV1Db entity = WidgetsV1Db
  { _v1_widgets :: entity (TableEntity WidgetsV1T)
  }
  deriving (Generic, Database DuckDB)

newtype WidgetsV2Db entity = WidgetsV2Db
  { _v2_widgets :: entity (TableEntity WidgetsV2T)
  }
  deriving (Generic, Database DuckDB)

stepV1 :: () -> Migration DuckDB (CheckedDatabaseSettings DuckDB WidgetsV1Db)
stepV1 () =
  WidgetsV1Db
    <$> createTable
      "widgets"
      WidgetsV1T
        { _v1_id = field "id" int notNull,
          _v1_name = field "name" (varchar Nothing) notNull
        }

stepV2 ::
  CheckedDatabaseSettings DuckDB WidgetsV1Db ->
  Migration DuckDB (CheckedDatabaseSettings DuckDB WidgetsV2Db)
stepV2 oldDb =
  WidgetsV2Db
    <$> alterTable
      (_v1_widgets oldDb)
      ( \oldTbl -> do
          price <- addColumn (field "price" (maybeType double))
          pure
            WidgetsV2T
              { _v2_id = _v1_id oldTbl,
                _v2_name = _v1_name oldTbl,
                _v2_price = price
              }
      )

stepV3 ::
  CheckedDatabaseSettings DuckDB WidgetsV2Db ->
  Migration DuckDB (CheckedDatabaseSettings DuckDB WidgetsV1Db)
stepV3 (WidgetsV2Db v2Table) =
  WidgetsV1Db
    <$> alterTable
      v2Table
      ( \v2Table' -> do
          dropColumn (_v2_price v2Table')
          pure (WidgetsV1T (_v2_id v2Table') (_v2_name v2Table'))
      )

runVerifySchema ::
  (Database DuckDB db) =>
  Connection ->
  CheckedDatabaseSettings DuckDB db ->
  Assertion
runVerifySchema conn db =
  runBeamDuckDB conn (verifySchema migrationBackend db) >>= \case
    VerificationSucceeded -> pure ()
    VerificationFailed failures ->
      assertFailure $ "Verification failed: " ++ show failures

multiStepMigration :: TestTree
multiStepMigration =
  testCaseSteps "two-step migration: create + alter table verifies against final schema" $ \testStep ->
    withInMemory $ \conn -> do
      let getColumns =
            ( DuckDB.query_
                conn
                "SELECT column_name FROM information_schema.columns \
                \WHERE table_name = 'widgets' \
                \ORDER BY ordinal_position" ::
                IO [DuckDB.Only T.Text]
            )
              <&> map (\(DuckDB.Only c) -> c)

      testStep "Initial migration"
      v1Db <- runBeamDuckDB conn $
        runMigrationSteps 0 Nothing (migrationStep "V1" stepV1) $ \_ _ step ->
          executeMigration runNoReturn step
      runVerifySchema conn v1Db

      getColumns >>= \cols ->
        assertEqual
          ("expected 'price' in columns " ++ show cols)
          ["id", "name"]
          cols

      testStep "Migration across V1 and V2"
      v2Db <- runBeamDuckDB conn
        $ runMigrationSteps
          1
          Nothing
          ( migrationStep "V1" stepV1
              >>> migrationStep "V2" stepV2
          )
        $ \_ _ step ->
          executeMigration runNoReturn step
      runVerifySchema conn v2Db

      getColumns >>= \cols ->
        assertEqual
          ("expected 'price' in columns " ++ show cols)
          ["id", "name", "price"]
          cols

      testStep "Migration back to V1"
      v3Db <- runBeamDuckDB conn
        $ runMigrationSteps
          2
          Nothing
          ( migrationStep "V1" stepV1
              >>> migrationStep "V2" stepV2
              >>> migrationStep "V3" stepV3
          )
        $ \_ _ step ->
          executeMigration runNoReturn step
      runVerifySchema conn v3Db

      getColumns >>= \cols ->
        assertEqual
          ("expected price column dropped, in columns " ++ show cols)
          ["id", "name"]
          cols
