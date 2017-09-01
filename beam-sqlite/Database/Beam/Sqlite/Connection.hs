{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Beam.Sqlite.Connection where

import           Database.Beam.Backend
import qualified Database.Beam.Backend.SQL.BeamExtensions as Beam
import           Database.Beam.Backend.URI
import           Database.Beam.Query (SqlInsert(..), SqlInsertValues(..), insert)
import           Database.Beam.Schema.Tables ( DatabaseEntity(..)
                                             , DatabaseEntityDescriptor(..)
                                             , TableEntity)
import           Database.Beam.Sqlite.Syntax
import           Database.Beam.Sqlite.Types

import           Database.SQLite.Simple ( Connection, ToRow(..), FromRow(..)
                                        , Query(..)
                                        , SQLData, field
                                        , execute, execute_
                                        , withStatement, bind, nextRow
                                        , withTransaction, query_
                                        , withConnection )
import           Database.SQLite.Simple.Internal (RowParser(RP), unRP)
import           Database.SQLite.Simple.Ok (Ok(..))
import           Database.SQLite.Simple.Types (Null)

import           Control.Exception (Exception, bracket, throwIO)
import           Control.Monad.Free.Church
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as D
import           Data.Monoid
import           Data.String
import           Data.Text (Text)

import           Network.URI

newtype SqliteM a = SqliteM { runSqliteM :: ReaderT (String -> IO (), Connection) IO a }
  deriving (Monad, Functor, Applicative, MonadIO)

data BeamSqliteError = BeamSqliteError String
  deriving Show
instance Exception BeamSqliteError

newtype BeamSqliteParams = BeamSqliteParams [SQLData]
instance ToRow BeamSqliteParams where
  toRow (BeamSqliteParams x) = x

newtype BeamSqliteRow a = BeamSqliteRow a
instance FromBackendRow Sqlite a => FromRow (BeamSqliteRow a) where
  fromRow = BeamSqliteRow <$> runF (fromBackendRow :: FromBackendRowM Sqlite a) finish step
      where
        finish = pure

        step :: FromBackendRowF Sqlite (RowParser a) -> RowParser a
        step (ParseOneField next) =
            field >>= next
        step (PeekField next) =
            RP $ do
              ro <- ask
              st <- get
              case runStateT (runReaderT (unRP field) ro) st of
                Ok (a, _) -> unRP (next (Just a))
                _ -> unRP (next Nothing)
        step (CheckNextNNull n next) =
            RP $ do
              ro <- ask
              st <- get
              case runStateT (runReaderT (unRP (replicateM_ n (field :: RowParser Null))) ro) st of
                Ok ((), st') ->
                  do put st'
                     unRP (next True)
                _ -> unRP (next False)

sqliteUriSyntax :: c SqliteCommandSyntax Sqlite Connection SqliteM
                -> BeamURIOpeners c
sqliteUriSyntax =
  mkUriOpener "sqlite:"
    (\uri action -> do
       let sqliteName = if null (uriPath uri) then ":memory:" else uriPath uri
       withConnection sqliteName action)

runSqlite :: Connection -> SqliteM a -> IO a
runSqlite = runSqliteDebug (\_ -> pure ())

runSqliteDebug :: (String -> IO ()) -> Connection
               -> SqliteM a -> IO a
runSqliteDebug printStmt conn x =
  runReaderT (runSqliteM x) (printStmt, conn)

runSqliteInsert :: (String -> IO ()) -> Connection -> SqliteInsertSyntax -> IO ()
runSqliteInsert logger conn (SqliteInsertSyntax tbl fields vs)
    -- If all expressions are simple expressions (no default), then just
    -- run the INSERT normally
  | SqliteInsertExpressions es <- vs, any (any (== SqliteExpressionDefault)) es =
      forM_ es $ \row -> do
        let (fields', row') = unzip $ filter ((/= SqliteExpressionDefault) . snd) $ zip fields row
            SqliteSyntax cmd vals = formatSqliteInsert tbl fields' (SqliteInsertExpressions [ row' ])
            cmdString = BL.unpack (toLazyByteString cmd)
        logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals))
        execute conn (fromString cmdString) (D.toList vals)
  | otherwise = do
      let SqliteSyntax cmd vals = formatSqliteInsert tbl fields vs
          cmdString = BL.unpack (toLazyByteString cmd)
      logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals))
      execute conn (fromString cmdString) (D.toList vals)

instance MonadBeam SqliteCommandSyntax Sqlite Connection SqliteM where
  withDatabase = runSqlite
  withDatabaseDebug = runSqliteDebug

  runNoReturn (SqliteCommandSyntax (SqliteSyntax cmd vals)) =
    SqliteM $ do
      (logger, conn) <- ask
      let cmdString = BL.unpack (toLazyByteString cmd)
      liftIO (logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals)))
      liftIO (execute conn (fromString cmdString) (D.toList vals))
  runNoReturn (SqliteCommandInsert insertStmt) =
    SqliteM $ do
      (logger, conn) <- ask
      liftIO (runSqliteInsert logger conn insertStmt)

  runReturningMany (SqliteCommandSyntax (SqliteSyntax cmd vals)) action =
      SqliteM $ do
        (logger, conn) <- ask
        let cmdString = BL.unpack (toLazyByteString cmd)
        liftIO $ do
          logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals))
          withStatement conn (fromString cmdString) $ \stmt ->
            do bind stmt (BeamSqliteParams (D.toList vals))
               let nextRow' = liftIO (nextRow stmt) >>= \x ->
                              case x of
                                Nothing -> pure Nothing
                                Just (BeamSqliteRow row) -> pure row
               runReaderT (runSqliteM (action nextRow')) (logger, conn)
  runReturningMany SqliteCommandInsert {} _ =
      SqliteM . liftIO . throwIO . BeamSqliteError . mconcat $
      [ "runReturningMany{Sqlite}: sqlite does not support returning "
      , "rows from an insert, use Database.Beam.Sqlite.insertReturning "
      , "for emulation" ]

instance Beam.MonadBeamInsertReturning SqliteCommandSyntax Sqlite Connection SqliteM where
  runInsertReturningList tbl values =
    runInsertReturningList (insertReturning tbl values)

-- * emulated INSERT returning support

data SqliteInsertReturning (table :: (* -> *) -> *) =
  SqliteInsertReturning Text SqliteInsertSyntax

insertReturning :: DatabaseEntity be db (TableEntity table)
                -> SqlInsertValues SqliteInsertValuesSyntax table
                -> SqliteInsertReturning table
insertReturning tbl@(DatabaseEntity (DatabaseTable tblNm _)) vs =
  let SqlInsert s = insert tbl vs
  in SqliteInsertReturning tblNm s

runInsertReturningList :: FromBackendRow Sqlite (table Identity)
                       => SqliteInsertReturning table
                       -> SqliteM [ table Identity ]
runInsertReturningList (SqliteInsertReturning nm insertStmt) =
  do (logger, conn) <- SqliteM ask
     SqliteM . liftIO $
       withTransaction conn $
         bracket (createInsertedValuesTable conn) (dropInsertedValuesTable conn) $ \() ->
         bracket (createInsertTrigger conn) (dropInsertTrigger conn) $ \() -> do
           runSqliteInsert logger conn insertStmt
           fmap (\(BeamSqliteRow r) -> r) <$> query_ conn "SELECT * FROM inserted_values"

  where
    createInsertedValuesTable conn =
      execute_ conn (Query ("CREATE TEMPORARY TABLE inserted_values AS SELECT * FROM \"" <> sqliteEscape nm <> "\" LIMIT 0"))
    dropInsertedValuesTable conn () =
      execute_ conn (Query "DROP TABLE inserted_values")

    createInsertTrigger conn =
      execute_ conn (Query ("CREATE TEMPORARY TRIGGER insert_trigger AFTER INSERT ON \"" <> sqliteEscape nm <> "\" BEGIN " <>
                            "INSERT INTO inserted_values SELECT * FROM \"" <> sqliteEscape nm <> "\" WHERE ROWID=last_insert_rowid(); END" ))
    dropInsertTrigger conn () =
      execute_ conn "DROP TRIGGER insert_trigger"
