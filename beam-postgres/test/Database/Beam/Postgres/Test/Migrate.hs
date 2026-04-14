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
      , foreignKeyVerification postgresConn
      , foreignKeyOnDeleteCascadeVerification postgresConn
      , foreignKeyActionsWork postgresConn
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
                    { _idx_tbl = addTableIndex "idx_tbl_value" (defaultIndexOptions @PgCommandSyntax)
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
        let idxOpts = setUniqueIndexOptions @PgCommandSyntax True
                    $ defaultIndexOptions @PgCommandSyntax
            db :: CheckedDatabaseSettings Postgres IdxDb
            db = defaultMigratableDbSettings `withDbModification`
                  (dbModification @_ @Postgres)
                    { _idx_tbl = addTableIndex "idx_tbl_value_uniq" idxOpts
                                   (\t -> selectorColumnName _idx_value t NE.:| []) }
        runBeamPostgres conn (verifySchema migrationBackend db) >>= \case
          VerificationSucceeded -> return ()
          VerificationFailed failures -> fail ("Verification failed: " ++ show failures)

-- Foreign key test tables

data FkParentT f = FkParentT
  { _fk_parent_id :: C f Int32
  } deriving (Generic, Beamable)

instance Table FkParentT where
  newtype PrimaryKey FkParentT f = FkParentPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = FkParentPk . _fk_parent_id

data FkChildT f = FkChildT
  { _fk_child_id        :: C f Int32
  , _fk_child_parent_id :: PrimaryKey FkParentT f
  } deriving (Generic, Beamable)

instance Table FkChildT where
  newtype PrimaryKey FkChildT f = FkChildPk (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = FkChildPk . _fk_child_id

data FkDb entity = FkDb
  { _fk_parent :: entity (TableEntity FkParentT)
  , _fk_child  :: entity (TableEntity FkChildT)
  } deriving (Generic, Database Postgres)

-- | Verifies that 'verifySchema' correctly detects a plain foreign key
foreignKeyVerification :: IO ByteString -> TestTree
foreignKeyVerification pgConn =
    testCase "verifySchema correctly detects a plain foreign key" $
      withTestPostgres "db_fk" pgConn $ \conn -> do
        Pg.execute_ conn "CREATE TABLE fk_parent (fk_parent_id integer NOT NULL PRIMARY KEY)"
        Pg.execute_ conn "CREATE TABLE fk_child  (fk_child_id integer NOT NULL PRIMARY KEY, \
                         \fk_child_parent_id integer NOT NULL, \
                         \FOREIGN KEY (fk_child_parent_id) REFERENCES fk_parent (fk_parent_id))"
        let db :: CheckedDatabaseSettings Postgres FkDb
            db = defaultMigratableDbSettings `withDbModification`
                  (dbModification @_ @Postgres)
                    { _fk_child =
                        addTableForeignKey (_fk_parent db)
                          (foreignKeyColumns _fk_child_parent_id)
                          primaryKeyColumns
                          ForeignKeyNoAction
                          ForeignKeyNoAction
                        <> modifyCheckedTable id
                             (FkChildT { _fk_child_id        = "fk_child_id"
                                       , _fk_child_parent_id = FkParentPk "fk_child_parent_id" }) }
        runBeamPostgres conn (verifySchema migrationBackend db) >>= \case
          VerificationSucceeded -> return ()
          VerificationFailed failures -> fail ("Verification failed: " ++ show failures)

-- | Verifies that foreign key actions are enforced at runtime.
foreignKeyActionsWork :: IO ByteString -> TestTree
foreignKeyActionsWork pgConn =
    testCase "cascading foreign key actions" $
      withTestPostgres "db_fk_actions" pgConn $ \conn -> do
        let db :: CheckedDatabaseSettings Postgres FkDb
            db = defaultMigratableDbSettings `withDbModification`
                  (dbModification @_ @Postgres)
                    { _fk_child =
                        addTableForeignKey (_fk_parent db)
                          (foreignKeyColumns _fk_child_parent_id)
                          primaryKeyColumns
                          ForeignKeyActionCascade
                          ForeignKeyActionCascade
                    }
            unc = unCheckDatabase db
        runBeamPostgres conn $ autoMigrate migrationBackend db

        -- Insert two parents and three children (two for parent 1, one for parent 2).
        runBeamPostgres conn $ do
          runInsert $ insert (_fk_parent unc) $ insertValues
            [ FkParentT 1, FkParentT 2 ]
          runInsert $ insert (_fk_child unc) $ insertValues
            [ FkChildT 1 (FkParentPk 1), FkChildT 2 (FkParentPk 1), FkChildT 3 (FkParentPk 2) ]

        -- ON UPDATE CASCADE: changing fk_parent_id 1 → 10 should cascade to child rows.
        runBeamPostgres conn $
          runUpdate $ update (_fk_parent unc)
            (\p -> _fk_parent_id p <-. val_ 10)
            (\p -> _fk_parent_id p ==. val_ 1)
        childrenOf10 <- runBeamPostgres conn $ runSelectReturningList $ select $
          filter_ (\c -> let FkParentPk pid = _fk_child_parent_id c in pid ==. val_ 10) $ all_ (_fk_child unc)
        assertEqual "two children should now reference updated parent id 10"
          2 (length childrenOf10)
        childrenOf1 <- runBeamPostgres conn $ runSelectReturningList $ select $
          filter_ (\c -> let FkParentPk pid = _fk_child_parent_id c in pid ==. val_ 1) $ all_ (_fk_child unc)
        assertEqual "no children should still reference old parent id 1"
          0 (length childrenOf1)

        -- ON DELETE CASCADE: deleting parent 2 should remove its child row.
        runBeamPostgres conn $
          runDelete $ delete (_fk_parent unc) (\p -> _fk_parent_id p ==. val_ 2)
        childrenOf2 <- runBeamPostgres conn $ runSelectReturningList $ select $
          filter_ (\c -> let FkParentPk pid = _fk_child_parent_id c in pid ==. val_ 2) $ all_ (_fk_child unc)
        assertEqual "child of deleted parent 2 should be removed"
          0 (length childrenOf2)
        allChildren <- runBeamPostgres conn $ runSelectReturningList $ select $
          all_ (_fk_child unc)
        assertEqual "only the two children of parent 10 should remain"
          2 (length allChildren)

-- | Verifies that 'verifySchema' correctly detects a foreign key with ON DELETE CASCADE
foreignKeyOnDeleteCascadeVerification :: IO ByteString -> TestTree
foreignKeyOnDeleteCascadeVerification pgConn =
    testCase "verifySchema correctly detects a foreign key with ON DELETE CASCADE" $
      withTestPostgres "db_fk_cascade" pgConn $ \conn -> do
        Pg.execute_ conn "CREATE TABLE fk_parent (fk_parent_id integer NOT NULL PRIMARY KEY)"
        Pg.execute_ conn "CREATE TABLE fk_child  (fk_child_id integer NOT NULL PRIMARY KEY, \
                         \fk_child_parent_id integer NOT NULL, \
                         \FOREIGN KEY (fk_child_parent_id) REFERENCES fk_parent (fk_parent_id) \
                         \ON DELETE CASCADE)"
        let db :: CheckedDatabaseSettings Postgres FkDb
            db = defaultMigratableDbSettings `withDbModification`
                  (dbModification @_ @Postgres)
                    { _fk_child =
                        addTableForeignKey (_fk_parent db)
                          (foreignKeyColumns _fk_child_parent_id)
                          primaryKeyColumns
                          ForeignKeyNoAction
                          ForeignKeyActionCascade
                        <> modifyCheckedTable id
                             (FkChildT { _fk_child_id        = "fk_child_id"
                                       , _fk_child_parent_id = FkParentPk "fk_child_parent_id" }) }
        runBeamPostgres conn (verifySchema migrationBackend db) >>= \case
          VerificationSucceeded -> return ()
          VerificationFailed failures -> fail ("Verification failed: " ++ show failures)
