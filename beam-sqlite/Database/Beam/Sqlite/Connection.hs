{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Beam.Sqlite.Connection
  ( Sqlite(..), SqliteM(..)
  , sqliteUriSyntax

    -- * Emulated @INSERT RETURNING@ support
  , SqliteInsertReturning
  , insertReturning, runInsertReturningList
  , ) where

import           Database.Beam.Backend
import           Database.Beam.Backend.SQL
import qualified Database.Beam.Backend.SQL.BeamExtensions as Beam
import           Database.Beam.Backend.URI
import           Database.Beam.Query (SqlInsert(..), SqlInsertValues(..), insert)
import           Database.Beam.Schema.Tables ( DatabaseEntity(..)
                                             , DatabaseEntityDescriptor(..)
                                             , TableEntity)
import           Database.Beam.Sqlite.Syntax

import           Database.SQLite.Simple ( Connection, ToRow(..), FromRow(..)
                                        , Query(..), SQLData(..), field
                                        , execute, execute_
                                        , withStatement, bind, nextRow
                                        , withTransaction, query_
                                        , withConnection )
import           Database.SQLite.Simple.FromField ( FromField(..), ResultError(..)
                                                  , returnError, fieldData)
import           Database.SQLite.Simple.Internal (RowParser(RP), unRP)
import           Database.SQLite.Simple.Ok (Ok(..))
import           Database.SQLite.Simple.Types (Null)

import           Control.Exception (bracket)
import           Control.Monad (forM_, replicateM_)
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import           Control.Monad.State.Strict (MonadState(..), runStateT)

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as D
import           Data.Int
import           Data.Monoid
import           Data.Scientific (Scientific)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time ( LocalTime, UTCTime, Day
                           , utc, utcToLocalTime )
import           Data.Word

import           Network.URI

import           Text.Read (readMaybe)

-- | The SQLite backend. Used to parameterize 'MonadBeam' and 'FromBackendRow'
-- to provide support for SQLite databases. See the documentation for
-- 'MonadBeam' and the <https://tathougies.github.io/beam/ user guide> for more
-- information on how to use this backend.
data Sqlite = Sqlite

instance BeamBackend Sqlite where
  type BackendFromField Sqlite = FromField

instance FromBackendRow Sqlite Bool
instance FromBackendRow Sqlite Double
instance FromBackendRow Sqlite Float
instance FromBackendRow Sqlite Int
instance FromBackendRow Sqlite Int8
instance FromBackendRow Sqlite Int16
instance FromBackendRow Sqlite Int32
instance FromBackendRow Sqlite Int64
instance FromBackendRow Sqlite Integer
instance FromBackendRow Sqlite Word
instance FromBackendRow Sqlite Word8
instance FromBackendRow Sqlite Word16
instance FromBackendRow Sqlite Word32
instance FromBackendRow Sqlite Word64
instance FromBackendRow Sqlite BS.ByteString
instance FromBackendRow Sqlite BL.ByteString
instance FromBackendRow Sqlite T.Text
instance FromBackendRow Sqlite TL.Text
instance FromBackendRow Sqlite UTCTime
instance FromBackendRow Sqlite Day
instance FromBackendRow Sqlite Null
instance FromBackendRow Sqlite Char where
  fromBackendRow = do
    t <- fromBackendRow
    case T.uncons t of
      Just (c, _) -> pure c
      _ -> fail "Need string of size one to parse Char"
instance FromBackendRow Sqlite SqlNull where
  fromBackendRow =
    SqlNull <$ (fromBackendRow :: FromBackendRowM Sqlite Null)
instance FromBackendRow Sqlite LocalTime where
  fromBackendRow = utcToLocalTime utc <$> fromBackendRow
instance FromBackendRow Sqlite Scientific where
  fromBackendRow = unSqliteScientific <$> fromBackendRow
instance FromBackendRow Sqlite SqliteScientific

newtype SqliteScientific = SqliteScientific { unSqliteScientific :: Scientific }
instance FromField SqliteScientific where
  fromField f =
    SqliteScientific <$>
    case fieldData f of
      SQLInteger i -> pure (fromIntegral i)
      SQLFloat d -> pure . fromRational . toRational $ d
      SQLText t -> tryRead (T.unpack t)
      SQLBlob b -> tryRead (BS.unpack b)
      SQLNull -> returnError UnexpectedNull f "null"
    where
      tryRead s =
        case readMaybe s of
          Nothing -> returnError ConversionFailed f $
                     "No conversion to Scientific for '" <> s <> "'"
          Just s'  -> pure s'

instance BeamSqlBackend Sqlite
instance BeamSql92Backend Sqlite

-- | 'MonadBeam' instance inside whiche SQLite queries are run. See the
-- <https://tathougies.github.io/beam/ user guide> for more information
newtype SqliteM a
  = SqliteM
  { runSqliteM :: ReaderT (String -> IO (), Connection) IO a
    -- ^ Run an IO action with access to a SQLite connection and a debug logging
    -- function, called or each query submitted on the connection.
  } deriving (Monad, Functor, Applicative, MonadIO)

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

-- | URI syntax for use with 'withDbConnection'. See documentation for
-- 'BeamURIOpeners' for more information.
sqliteUriSyntax :: c SqliteCommandSyntax Sqlite Connection SqliteM
                -> BeamURIOpeners c
sqliteUriSyntax =
  mkUriOpener "sqlite:"
    (\uri action -> do
       let sqliteName = if null (uriPath uri) then ":memory:" else uriPath uri
       withConnection sqliteName action)

instance MonadBeam SqliteCommandSyntax Sqlite Connection SqliteM where
  withDatabase = withDatabaseDebug (\_ -> pure ())
  withDatabaseDebug printStmt conn x = runReaderT (runSqliteM x) (printStmt, conn)

  runNoReturn (SqliteCommandSyntax (SqliteSyntax cmd vals)) =
    SqliteM $ do
      (logger, conn) <- ask
      let cmdString = BL.unpack (toLazyByteString (withPlaceholders cmd))
      liftIO (logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals)))
      liftIO (execute conn (fromString cmdString) (D.toList vals))
  runNoReturn (SqliteCommandInsert insertStmt_) =
    SqliteM $ do
      (logger, conn) <- ask
      liftIO (runSqliteInsert logger conn insertStmt_)

  runReturningMany (SqliteCommandSyntax (SqliteSyntax cmd vals)) action =
      SqliteM $ do
        (logger, conn) <- ask
        let cmdString = BL.unpack (toLazyByteString (withPlaceholders cmd))
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
      fail . mconcat $
      [ "runReturningMany{Sqlite}: sqlite does not support returning "
      , "rows from an insert, use Database.Beam.Sqlite.insertReturning "
      , "for emulation" ]

instance Beam.MonadBeamInsertReturning SqliteCommandSyntax Sqlite Connection SqliteM where
  runInsertReturningList tbl values =
    runInsertReturningList (insertReturning tbl values)

runSqliteInsert :: (String -> IO ()) -> Connection -> SqliteInsertSyntax -> IO ()
runSqliteInsert logger conn (SqliteInsertSyntax tbl fields vs)
    -- If all expressions are simple expressions (no default), then just
    -- run the INSERT normally
  | SqliteInsertExpressions es <- vs, any (any (== SqliteExpressionDefault)) es =
      forM_ es $ \row -> do
        let (fields', row') = unzip $ filter ((/= SqliteExpressionDefault) . snd) $ zip fields row
            SqliteSyntax cmd vals = formatSqliteInsert tbl fields' (SqliteInsertExpressions [ row' ])
            cmdString = BL.unpack (toLazyByteString (withPlaceholders cmd))
        logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals))
        execute conn (fromString cmdString) (D.toList vals)
  | otherwise = do
      let SqliteSyntax cmd vals = formatSqliteInsert tbl fields vs
          cmdString = BL.unpack (toLazyByteString (withPlaceholders cmd))
      logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals))
      execute conn (fromString cmdString) (D.toList vals)

-- * emulated INSERT returning support

-- | Represents an @INSERT@ statement, from which we can retrieve inserted rows.
-- Beam also offers a backend-agnostic way of using this functionality in the
-- 'MonadBeamInsertReturning' extension. This functionality is emulated in
-- SQLite using a temporary table and a trigger.
data SqliteInsertReturning (table :: (* -> *) -> *)
  = SqliteInsertReturning T.Text SqliteInsertSyntax
  | SqliteInsertReturningNoRows

-- | Build a 'SqliteInsertReturning' representing inserting the given values
-- into the given table. Use 'runInsertReturningList'
insertReturning :: DatabaseEntity be db (TableEntity table)
                -> SqlInsertValues SqliteInsertValuesSyntax table
                -> SqliteInsertReturning table
insertReturning tbl@(DatabaseEntity (DatabaseTable tblNm _)) vs =
  case insert tbl vs of
    SqlInsert s ->
      SqliteInsertReturning tblNm s
    SqlInsertNoRows ->
      SqliteInsertReturningNoRows

-- | Runs a 'SqliteInsertReturning' statement and returns a result for each
-- inserted row.
runInsertReturningList :: FromBackendRow Sqlite (table Identity)
                       => SqliteInsertReturning table
                       -> SqliteM [ table Identity ]
runInsertReturningList SqliteInsertReturningNoRows = pure []
runInsertReturningList (SqliteInsertReturning nm insertStmt_) =
  do (logger, conn) <- SqliteM ask
     SqliteM . liftIO $
       withTransaction conn $
         bracket (createInsertedValuesTable conn) (dropInsertedValuesTable conn) $ \() ->
         bracket (createInsertTrigger conn) (dropInsertTrigger conn) $ \() -> do
           runSqliteInsert logger conn insertStmt_
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
