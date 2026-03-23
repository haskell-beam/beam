module Database.Beam.Sqlite.Test.Migrate (tests) where

import Database.SQLite.Simple
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List.NonEmpty as NE
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple

import Database.Beam.Sqlite.Test
import Database.Beam.Sqlite.Syntax (SqliteIndexOptions)

tests :: TestTree
tests = testGroup "Migration tests"
  [ verifiesPrimaryKey
  , verifiesNoPrimaryKey
  , verifiesIndex
  , verifiesUniqueIndex
  ]

newtype WithPkT f = WithPkT
  { _with_pk_value :: C f Bool
  } deriving (Generic, Beamable)

instance Table WithPkT where
  newtype PrimaryKey WithPkT f = Pk (C f Bool)
    deriving (Generic, Beamable)

  primaryKey = Pk . _with_pk_value

data WithPkDb entity = WithPkDb
  { _with_pk :: entity (TableEntity WithPkT)
  } deriving (Generic, Database Sqlite)

withPkDbChecked :: CheckedDatabaseSettings Sqlite WithPkDb
withPkDbChecked = defaultMigratableDbSettings

newtype WithoutPkT f = WithoutPkT
  { _without_pk_value :: C f Bool
  } deriving (Generic, Beamable)

instance Table WithoutPkT where
  data PrimaryKey WithoutPkT f = NoPk
    deriving (Generic, Beamable)

  primaryKey _ = NoPk

data WithoutPkDb entity = WithoutPkDb
  { _without_pk :: entity (TableEntity WithoutPkT)
  } deriving (Generic, Database Sqlite)

withoutPkDbChecked :: CheckedDatabaseSettings Sqlite WithoutPkDb
withoutPkDbChecked = defaultMigratableDbSettings

verifiesPrimaryKey :: TestTree
verifiesPrimaryKey = testCase "verifySchema correctly detects primary key" $
  withTestDb $ \conn -> do
    execute_ conn "create table with_pk (with_pk_value bool not null primary key)"
    testVerifySchema conn withPkDbChecked

verifiesNoPrimaryKey :: TestTree
verifiesNoPrimaryKey = testCase "verifySchema correctly handles table with no primary key" $
  withTestDb $ \conn -> do
    execute_ conn "create table without_pk (without_pk_value bool not null)"
    testVerifySchema conn withoutPkDbChecked

testVerifySchema
  :: Database Sqlite db
  => Connection -> CheckedDatabaseSettings Sqlite db -> Assertion
testVerifySchema conn db =
  runBeamSqlite conn (verifySchema migrationBackend db) >>= \case
    VerificationSucceeded -> return ()
    VerificationFailed failures ->
      fail $ "Verification failed: " ++ show failures

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
  } deriving (Generic, Database Sqlite)

verifiesIndex :: TestTree
verifiesIndex = testCase "verifySchema correctly detects a secondary index" $
  withTestDb $ \conn -> do
    execute_ conn "create table idx_tbl (idx_value int not null primary key)"
    execute_ conn "create index idx_tbl_value on idx_tbl (idx_value)"
    let db :: CheckedDatabaseSettings Sqlite IdxDb
        db = defaultMigratableDbSettings `withDbModification`
              (dbModification @_ @Sqlite)
                { _idx_tbl =
                    addTableIndex "idx_tbl_value" (defaultIndexOptions @SqliteCommandSyntax)
                      (\t -> selectorColumnName _idx_value t NE.:| []) }
    testVerifySchema conn db

verifiesUniqueIndex :: TestTree
verifiesUniqueIndex = testCase "verifySchema correctly detects a UNIQUE secondary index" $
  withTestDb $ \conn -> do
    execute_ conn "create table idx_tbl (idx_value int not null primary key)"
    execute_ conn "create unique index idx_tbl_value_uniq on idx_tbl (idx_value)"
    let db :: CheckedDatabaseSettings Sqlite IdxDb
        db = defaultMigratableDbSettings `withDbModification`
              (dbModification @_ @Sqlite)
                { _idx_tbl =
                    let idxOpts = setUniqueIndexOptions @SqliteCommandSyntax True
                                $ defaultIndexOptions @SqliteCommandSyntax
                    in
                    addTableIndex "idx_tbl_value_uniq" idxOpts
                      (\t -> selectorColumnName _idx_value t NE.:| []) }
    testVerifySchema conn db
