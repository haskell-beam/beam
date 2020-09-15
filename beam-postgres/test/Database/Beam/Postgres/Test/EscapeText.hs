{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database.Beam.Postgres.Test.EscapeText where

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Test

import qualified Database.PostgreSQL.Simple               as Pg
import           Database.PostgreSQL.Simple.SqlQQ         (sql)

import           Control.Exception                        (SomeException (..),
                                                           handle)
import           Control.Monad                            (forM_)
import           Data.ByteString                          (ByteString)
import           Data.Text                                (Text)
import           Data.Traversable                         (for)

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: IO ByteString -> TestTree
tests postgresConn =
  testGroup "SQL injection tests"
    [ escapeSequenceTest postgresConn
    , escapeDslTest postgresConn
    ]

data EscapeSequenceTableT f
    = EscapeSequenceTableT
    { _key    :: C f Int
    , _field1 :: C f Text }
    deriving (Generic, Beamable)

instance Table EscapeSequenceTableT where
    data PrimaryKey EscapeSequenceTableT f = EscapeSequenceTableKey (C f Int)
      deriving (Generic, Beamable)
    primaryKey = EscapeSequenceTableKey <$> _key

data EscapeSequenceTableDb entity
    = EscapeSequenceTableDb
    { escapingTestTable :: entity (TableEntity EscapeSequenceTableT) }
    deriving (Generic, Database Postgres)

data EscapeDslT f
    = EscapeDslT
    { _key2   :: C f Int
    , _field2 :: C f Text }
    deriving (Generic, Beamable)

instance Table EscapeDslT where
    data PrimaryKey EscapeDslT f = EscapeDslKey (C f Int)
      deriving (Generic, Beamable)
    primaryKey = EscapeDslKey <$> _key2

data EscapeDslDb entity
    = EscapeDslDb
    { escapeDslTable :: entity (TableEntity EscapeDslT) }
    deriving (Generic, Database Postgres)

-- TODO: change how we handle "\0" if `text` decides to use UTF8 internally
escapeSequences :: [(Text, Text)]
escapeSequences =
  [ -- ("0", "\0")  -- An ASCII NUL (X'00') character -- TODO: fix this
    ("\'", "\'") -- A single quote (“'”) character
  , ("\"", "\"") -- A double quote (“"”) character
  , ("b", "\b")  -- A backspace character
  , ("n", "\n")  -- A newline (linefeed) character
  , ("r", "\r")  -- A carriage return character
  , ("t", "\t")  -- A tab character
  -- , "\Z"           --	ASCII 26 (Control+Z); see note following the table
  , ("\\", "\\") --	A backslash (“\”) character
  -- , "\%"           --	A “%” character; see note following the table
  -- , "\_"           --	A “_” character; see note following the table
  ]

bobbyTables :: Text
bobbyTables = "\'; DROP TABLE students; --"

escapeDslDb :: DatabaseSettings Postgres EscapeDslDb
escapeDslDb = defaultDbSettings

mkTestCase :: String -> (Pg.Connection -> IO ()) -> IO ByteString -> TestTree
mkTestCase description action pgConn =
  testCase description $ do
    withTestPostgres "db_test_escaping" pgConn $ \conn -> do
      Pg.execute_ conn [sql|
        CREATE TABLE dsl_table (
          key1 int PRIMARY KEY,
          field2 text NOT NULL
        );

        INSERT INTO dsl_table (key1, field2)
        VALUES
        (1, '''; DROP TABLE students; --'),
        (2, 'foo');
      |]
      (action conn)

escapeDslTest :: IO ByteString -> TestTree
escapeDslTest pgConn = testGroup "Bobby tables testing text escaping" $
  [ mkTestCase' "with select and filter_" testSelect
  , mkTestCase' "with insert" testInsert
  , mkTestCase' "with update" testUpdate
  , mkTestCase' "with save" testSave
  , mkTestCase' "with insertReturning" testInsertReturning
  ]
  where
    mkTestCase' desc act = mkTestCase desc act pgConn

    runSelect conn =
      runBeamPostgres conn
        $ runSelectReturningList
        $ select
        $ fmap (\(EscapeDslT _ v) -> v)
        $ filter_' (\(EscapeDslT _ v) -> v ==?. val_ bobbyTables)
        $ all_ (escapeDslTable escapeDslDb)

    testSelect conn = do
      readback <- runSelect conn
      readback @?= [bobbyTables]

    testInsert conn = do
      readback <- do
        runBeamPostgres conn
          $ runInsert
          $ insert (escapeDslTable escapeDslDb)
          $ insertExpressions [ val_ (EscapeDslT 3 bobbyTables) ]
        runSelect conn
      readback @?= [bobbyTables, bobbyTables]

    testUpdate conn = do
      readback <- do
        runBeamPostgres conn
          $ runUpdate
          $ update' (escapeDslTable escapeDslDb)
                    (\(EscapeDslT _ v) -> v <-. (val_ bobbyTables))
                    (\(EscapeDslT _ v) -> v /=?. (val_ bobbyTables))
        runSelect conn
      readback @?= [bobbyTables, bobbyTables]

    testSave conn = do
      readback <- do
        runBeamPostgres conn
          $ runUpdate
          $ save' (escapeDslTable escapeDslDb)
                  (EscapeDslT 2 bobbyTables)
        runSelect conn
      readback @?= [bobbyTables, bobbyTables]

    testInsertReturning conn = do
      readback1 <- do
        runBeamPostgres conn
          $ fmap (fmap (\(EscapeDslT _ v) -> v))
          $ runInsertReturningList
          $ insert (escapeDslTable escapeDslDb)
          $ insertExpressions [ val_ (EscapeDslT 3 bobbyTables) ]
      readback2 <- runSelect conn
      readback1 @?= [bobbyTables]
      readback2 @?= [bobbyTables, bobbyTables]

escapeSequenceTest :: IO ByteString -> TestTree
escapeSequenceTest pgConn =
    testCase "Bobby tables test" $
    withTestPostgres "db_escape_dsl" pgConn $ \conn -> do
      readback <-
        runBeamPostgres conn $ do
          db <- fmap unCheckDatabase $
            executeMigration runNoReturn
              (EscapeSequenceTableDb <$> createTable "escaping_test"
                (EscapeSequenceTableT (field "key" int notNull)
                  (field "value" text notNull)))

          forM_ (zip escapeSequences [1..]) $ \((repr, escapeSeq), ix) -> do
            runInsert $ insert (escapingTestTable db) $
              insertValues [ EscapeSequenceTableT ix ("hello" <> escapeSeq <> "world" :: Text) ]

          fmap (fmap (\(v) -> v)) $
            runSelectReturningList $ select $
            fmap (\(EscapeSequenceTableT _ v) -> v) $
            orderBy_ (\(EscapeSequenceTableT k _) -> asc_ k) $
            all_ (escapingTestTable db)

      readback @?= ((flip map) escapeSequences (\(repr, escapeSeq) -> "hello" <> escapeSeq <> "world"))
