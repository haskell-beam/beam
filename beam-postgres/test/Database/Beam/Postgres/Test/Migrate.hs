module Database.Beam.Postgres.Test.Migrate where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Database.Beam.Postgres.Test
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple

import Data.ByteString (ByteString)
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

tests :: IO ByteString -> TestTree
tests postgresConn =
    testGroup "Migration tests"
      [ charWidthVerification postgresConn "VARCHAR" varchar
      , charNoWidthVerification postgresConn "VARCHAR" varchar
      , charWidthVerification postgresConn "CHAR" char
      , charNoWidthVerification postgresConn "CHAR" char
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
