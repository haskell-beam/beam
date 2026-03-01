{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Database.Beam.Sqlite.Connection
  ( Sqlite(..), SqliteM(..)
  , sqliteUriSyntax

  , runBeamSqlite, runBeamSqliteDebug

    -- * Support for @RETURNING@ with additional projection

    -- ** @INSERT ... RETURNING@
  , SqliteInsertReturning
  , insertReturning, insertOnConflictReturning, runInsertReturningList

    -- ** @DELETE ... RETURNING@
  , SqliteDeleteReturning
  , deleteReturning, runDeleteReturningList

    -- ** @UPDATE ... RETURNING@
  , SqliteUpdateReturning
  , updateReturning, runUpdateReturningList
  ) where

import           Prelude hiding (fail)

import           Database.Beam.Backend
import           Database.Beam.Backend.Internal.Compat
import qualified Database.Beam.Backend.SQL.BeamExtensions as Beam
import           Database.Beam.Backend.URI
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL ( BeamMigrateOnlySqlBackend, FieldReturnType(..) )
import qualified Database.Beam.Migrate.SQL as Beam
import           Database.Beam.Migrate.SQL.BeamExtensions
import           Database.Beam.Query ( SqlInsert(..), SqlInsertValues(..)
                                     , SqlDelete(..)
                                     , HasQBuilder(..), HasSqlEqualityCheck
                                     , HasSqlQuantifiedEqualityCheck
                                     , DataType(..)
                                     , HasSqlInTable(..)
                                     , QExprToIdentity
                                     , insert, current_, delete, SqlUpdate (..), update )
import           Database.Beam.Query.Internal
import           Database.Beam.Query.SQL92
import           Database.Beam.Schema.Tables ( Beamable
                                             , Columnar'(..)
                                             , DatabaseEntity(..)
                                             , DatabaseEntityDescriptor(..)
                                             , TableEntity
                                             , TableField(..)
                                             , changeBeamRep )
import           Database.Beam.Sqlite.Syntax

import           Database.SQLite.Simple ( Connection, ToRow(..), FromRow(..)
                                        , SQLData(..), field
                                        , execute
                                        , withStatement, bind, nextRow
                                        , open, close )
import           Database.SQLite.Simple.FromField ( FromField(..), ResultError(..)
                                                  , returnError, fieldData)
import           Database.SQLite.Simple.Internal (RowParser(RP), unRP)
import           Database.SQLite.Simple.Ok (Ok(..))
import           Database.SQLite.Simple.Types (Null)

import           Control.Exception (SomeException(..))
import           Control.Monad (forM_)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Fail (MonadFail(..))
import           Control.Monad.Free.Church
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader (ReaderT(..), MonadReader(..), runReaderT)
import           Control.Monad.State.Strict (MonadState(..), StateT(..), runStateT)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Writer (tell, execWriter)

import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as D
import           Data.Int
import           Data.IORef (newIORef, atomicModifyIORef')
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Strict.Map
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy(..))
import           Data.Scientific (Scientific)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time ( LocalTime, UTCTime, Day
                           , ZonedTime, utc, utcToLocalTime )
import           Data.Typeable (cast)
import           Data.Void (Void)
import           Data.Word
import           GHC.IORef (atomicModifyIORef'_)
import           GHC.TypeLits

import           Network.URI

import           Text.Read (readMaybe)

-- | The SQLite backend. Used to parameterize 'MonadBeam' and 'FromBackendRow'
-- to provide support for SQLite databases. See the documentation for
-- 'MonadBeam' and the <https://haskell-beam.github.io/beam/ user guide> for more
-- information on how to use this backend.
data Sqlite = Sqlite

instance BeamBackend Sqlite where
  type BackendFromField Sqlite = FromField

instance HasQBuilder Sqlite where
  buildSqlQuery = buildSql92Query' False -- SQLite does not support arbitrarily nesting UNION, INTERSECT, and EXCEPT

instance HasSqlInTable Sqlite where
  inRowValuesE Proxy e es = SqliteExpressionSyntax $ mconcat
    [ parens $ fromSqliteExpression e
    , emit " IN "
    , parens $ emit "VALUES " <> commas (map fromSqliteExpression es)
    ]

instance BeamSqlBackendIsString Sqlite T.Text
instance BeamSqlBackendIsString Sqlite String

instance FromBackendRow Sqlite Bool
instance FromBackendRow Sqlite Double
instance FromBackendRow Sqlite Float
instance FromBackendRow Sqlite Int8
instance FromBackendRow Sqlite Int16
instance FromBackendRow Sqlite Int32
instance FromBackendRow Sqlite Int64
instance FromBackendRow Sqlite Integer
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

instance TypeError (PreferExplicitSize Int Int32) => FromBackendRow Sqlite Int
instance TypeError (PreferExplicitSize Word Word32) => FromBackendRow Sqlite Word

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
instance BeamMigrateOnlySqlBackend Sqlite
type instance BeamSqlBackendSyntax Sqlite = SqliteCommandSyntax

data SqliteHasDefault = SqliteHasDefault
instance FieldReturnType 'True 'False Sqlite resTy a =>
         FieldReturnType 'False 'False Sqlite resTy (SqliteHasDefault -> a) where
  field' _ _ nm ty _ collation constraints SqliteHasDefault =
    field' (Proxy @'True) (Proxy @'False) nm ty Nothing collation constraints

instance BeamSqlBackendHasSerial Sqlite where
  genericSerial nm = Beam.field nm (DataType sqliteSerialType) SqliteHasDefault

-- | 'MonadBeam' instance inside which SQLite queries are run. See the
-- <https://haskell-beam.github.io/beam/ user guide> for more information
newtype SqliteM a
  = SqliteM
  { runSqliteM :: ReaderT (String -> IO (), Connection) IO a
    -- ^ Run an IO action with access to a SQLite connection and a debug logging
    -- function, called or each query submitted on the connection.
  } deriving (Monad, Functor, Applicative, MonadIO, MonadFail)
    deriving newtype (MonadBase IO, MonadBaseControl IO)

newtype BeamSqliteParams = BeamSqliteParams [SQLData]
instance ToRow BeamSqliteParams where
  toRow (BeamSqliteParams x) = x

newtype BeamSqliteRow a = BeamSqliteRow a
instance FromBackendRow Sqlite a => FromRow (BeamSqliteRow a) where
  fromRow = BeamSqliteRow <$> runF fromBackendRow' finish step
      where
        FromBackendRowM fromBackendRow' = fromBackendRow :: FromBackendRowM Sqlite a

        translateErrors :: Maybe Int -> SomeException -> Maybe SomeException
        translateErrors col (SomeException e) =
          case cast e of
            Just (ConversionFailed { errSQLType     = typeString
                                   , errHaskellType = hsString
                                   , errMessage     = msg }) ->
              Just (SomeException (BeamRowReadError col (ColumnTypeMismatch hsString typeString ("conversion failed: " ++ msg))))
            Just (UnexpectedNull {}) ->
              Just (SomeException (BeamRowReadError col ColumnUnexpectedNull))
            Just (Incompatible { errSQLType     = typeString
                               , errHaskellType = hsString
                               , errMessage     = msg }) ->
              Just (SomeException (BeamRowReadError col (ColumnTypeMismatch hsString typeString ("incompatible: " ++ msg))))
            Nothing -> Nothing

        finish = pure

        step :: forall a'. FromBackendRowF Sqlite (RowParser a') -> RowParser a'
        step (ParseOneField next) =
            RP $ ReaderT $ \ro -> StateT $ \st@(col, _) ->
            case runStateT (runReaderT (unRP field) ro) st of
              Ok (x, st') -> runStateT (runReaderT (unRP (next x)) ro) st'
              Errors errs -> Errors (mapMaybe (translateErrors (Just col)) errs)
        step (Alt (FromBackendRowM a) (FromBackendRowM b) next) = do
          RP $ do
            let RP a' = runF a finish step
                RP b' = runF b finish step

            st <- get
            ro <- ask
            case runStateT (runReaderT a' ro) st of
              Ok (ra, st') -> do
                put st'
                unRP (next ra)
              Errors aErrs ->
                case runStateT (runReaderT b' ro) st of
                  Ok (rb, st') -> do
                    put st'
                    unRP (next rb)
                  Errors bErrs ->
                    lift (lift (Errors (aErrs ++ bErrs)))
        step (FailParseWith err) = RP (lift (lift (Errors [SomeException err])))

-- * Equality checks
#define HAS_SQLITE_EQUALITY_CHECK(ty)                       \
  instance HasSqlEqualityCheck Sqlite (ty); \
  instance HasSqlQuantifiedEqualityCheck Sqlite (ty);

HAS_SQLITE_EQUALITY_CHECK(Int8)
HAS_SQLITE_EQUALITY_CHECK(Int16)
HAS_SQLITE_EQUALITY_CHECK(Int32)
HAS_SQLITE_EQUALITY_CHECK(Int64)
HAS_SQLITE_EQUALITY_CHECK(Word8)
HAS_SQLITE_EQUALITY_CHECK(Word16)
HAS_SQLITE_EQUALITY_CHECK(Word32)
HAS_SQLITE_EQUALITY_CHECK(Word64)
HAS_SQLITE_EQUALITY_CHECK(Double)
HAS_SQLITE_EQUALITY_CHECK(Float)
HAS_SQLITE_EQUALITY_CHECK(Bool)
HAS_SQLITE_EQUALITY_CHECK(String)
HAS_SQLITE_EQUALITY_CHECK(T.Text)
HAS_SQLITE_EQUALITY_CHECK(TL.Text)
HAS_SQLITE_EQUALITY_CHECK(BS.ByteString)
HAS_SQLITE_EQUALITY_CHECK(BL.ByteString)
HAS_SQLITE_EQUALITY_CHECK(UTCTime)
HAS_SQLITE_EQUALITY_CHECK(Day)
HAS_SQLITE_EQUALITY_CHECK(LocalTime)
HAS_SQLITE_EQUALITY_CHECK(ZonedTime)
HAS_SQLITE_EQUALITY_CHECK(Char)
HAS_SQLITE_EQUALITY_CHECK(Integer)
HAS_SQLITE_EQUALITY_CHECK(Scientific)

instance TypeError (PreferExplicitSize Int Int32) => HasSqlEqualityCheck Sqlite Int
instance TypeError (PreferExplicitSize Int Int32) => HasSqlQuantifiedEqualityCheck Sqlite Int
instance TypeError (PreferExplicitSize Word Word32) => HasSqlEqualityCheck Sqlite Word
instance TypeError (PreferExplicitSize Word Word32) => HasSqlQuantifiedEqualityCheck Sqlite Word

class HasDefaultSqlDataType Sqlite a => IsSqliteSerialIntegerType a
instance IsSqliteSerialIntegerType Int32
instance IsSqliteSerialIntegerType Int64
instance TypeError (PreferExplicitSize Int Int32) => IsSqliteSerialIntegerType Int

instance IsSqliteSerialIntegerType a => HasDefaultSqlDataType Sqlite (SqlSerial a) where
  defaultSqlDataType _ _ False = sqliteSerialType
  defaultSqlDataType _ _ True = intType

instance HasDefaultSqlDataType Sqlite BS.ByteString where
  -- TODO we should somehow allow contsraints based on backend
  defaultSqlDataType _ _ _ = sqliteBlobType

instance HasDefaultSqlDataType Sqlite LocalTime where
  defaultSqlDataType _ _ _ = timestampType Nothing False

-- | URI syntax for use with 'withDbConnection'. See documentation for
-- 'BeamURIOpeners' for more information.
sqliteUriSyntax :: c Sqlite Connection SqliteM
                -> BeamURIOpeners c
sqliteUriSyntax =
  mkUriOpener runBeamSqlite "sqlite:"
    (\uri -> do
        let sqliteName = if null (uriPath uri) then ":memory:" else uriPath uri
        hdl <- open sqliteName
        pure (hdl, close hdl))

runBeamSqliteDebug :: (String -> IO ()) -> Connection -> SqliteM a -> IO a
runBeamSqliteDebug debugStmt conn x = runReaderT (runSqliteM x) (debugStmt, conn)

runBeamSqlite :: Connection -> SqliteM a -> IO a
runBeamSqlite = runBeamSqliteDebug (\_ -> pure ())

instance MonadBeam Sqlite SqliteM where
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
  runReturningMany (SqliteCommandInsert (SqliteInsertSyntax tbl fields vs onConflict)) action =
      -- The DEFAULT grouping happens at query construction time, so we just
      -- handle the already-grouped statements here.
      SqliteM $ do
        (logger, conn) <- ask
        resultsRef <- liftIO (newIORef [])
        forM_ (sqliteGroupByDefaults fields vs) $ \(fields', vs') -> do
          let returningClause = emit " RETURNING " <> commas (map quotedIdentifier fields)
              SqliteSyntax cmd vals = formatSqliteInsertOnConflict tbl fields' vs' onConflict <> returningClause
              cmdString = BL.unpack (toLazyByteString (withPlaceholders cmd))

          liftIO $ do
            logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals))
            withStatement conn (fromString cmdString) $ \stmt ->
              do bind stmt (BeamSqliteParams (D.toList vals))
                 unfoldM (nextRow stmt) >>= \new -> atomicModifyIORef'_ resultsRef (new ++)

        -- We must reverse the list in the IORef because it has been constructed in reverse
        -- order. We construct the list in reverse because it's faster to prepend to
        -- a linked list
        _ <- liftIO (atomicModifyIORef'_ resultsRef reverse)
        let nextRow' = liftIO $ do
              atomicModifyIORef' resultsRef $ \results -> case results of
                (BeamSqliteRow h:rest) -> (rest, Just h)
                [] -> ([], Nothing)
        runSqliteM (action nextRow')


unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = go []
  where
    go acc = f >>= maybe (pure acc) (\x -> go (x : acc))

instance Beam.MonadBeamInsertReturning Sqlite SqliteM where
  runInsertReturningList SqlInsertNoRows = pure []
  runInsertReturningList (SqlInsert _ insertCommand) = runReturningList $ SqliteCommandInsert insertCommand

newtype SqliteInsertReturning a = SqliteInsertReturning [SqliteSyntax]
newtype SqliteDeleteReturning a = SqliteDeleteReturning SqliteSyntax
newtype SqliteUpdateReturning a = SqliteUpdateReturning (Maybe SqliteSyntax)

-- | SQLite cannot deal with DEFAULT in insertion. Instead, it expects these
-- fields to be omitted from the INSERT.
--
-- This means that, for any given INSERT query, the row value being inserted
-- must have a fixed set of fields with DEFAULT value.
--
-- This function groups the inserted values by the set of DEFAULT fields, so
-- that we can perform a separate INSERT for each set of DEFAULT fields.
sqliteGroupByDefaults :: [T.Text] -> SqliteInsertValuesSyntax -> [([T.Text], SqliteInsertValuesSyntax)]
sqliteGroupByDefaults flds (SqliteInsertExpressions es) =
  let
    partitionDefaultsFrom :: (IntSet, [SqliteExpressionSyntax]) -> Int -> [SqliteExpressionSyntax] -> (IntSet, [SqliteExpressionSyntax])
    partitionDefaultsFrom (dflts, acc) _ [] = (dflts, reverse acc)
    partitionDefaultsFrom (dflts, acc) i (x:xs)
      | x == SqliteExpressionDefault
      = partitionDefaultsFrom (IntSet.insert i dflts, acc) (i + 1) xs
      | otherwise
      = partitionDefaultsFrom (dflts, x : acc) (i + 1) xs

    grouped :: [(IntSet, [[SqliteExpressionSyntax]])]
    grouped =
      Strict.Map.toDescList $
      Strict.Map.fromListWith (++)
        [ (dflts, [e'])
        | e <- reverse es
        , let (dflts, e') = partitionDefaultsFrom (IntSet.empty, []) 0 e
        ]

    mkSyntax :: IntSet -> [[SqliteExpressionSyntax]] -> ([T.Text], SqliteInsertValuesSyntax)
    mkSyntax dflts xs =
      ( [fld | (i, fld) <- zip [0..] flds
              , not $ i `IntSet.member` dflts
              ]
      , SqliteInsertExpressions xs)
  in
    map (uncurry mkSyntax) grouped
sqliteGroupByDefaults flds orig@(SqliteInsertFromSql {}) =
  -- Preserve manually-written queries
  [(flds, orig)]

runDeleteReturningList
  :: ( MonadBeam be m
     , BeamSqlBackendSyntax be ~ SqliteCommandSyntax
     , FromBackendRow be a
     )
  => SqliteDeleteReturning a
  -> m [a]
runDeleteReturningList (SqliteDeleteReturning syntax) =
  runReturningList $ SqliteCommandSyntax syntax

-- | SQLite @DELETE ... RETURNING@ statement support. The last
-- argument takes the deleted row and returns the values to be returned.
--
-- Use 'runDeleteReturningList' to get the results.
deleteReturning :: forall a table be. Projectible Sqlite a
                => DatabaseEntity Sqlite be (TableEntity table)
                      -- ^ table to delete from
                -> (forall s. table (QExpr Sqlite s) -> QExpr Sqlite s Bool)
                      -- ^ predicate selecting rows to delete
                -> (table (QExpr Sqlite Void) -> a)
                      -- ^ projection describing what to return
                -> SqliteDeleteReturning (QExprToIdentity a)
deleteReturning table@(DatabaseEntity (DatabaseTable { dbTableSettings = tblSettings }))
                mkWhere
                mkProjection =
  SqliteDeleteReturning $
    fromSqliteDelete sqliteDelete
      <>
    emit " RETURNING " <> commas (map fromSqliteExpression (project (Proxy @Sqlite) (mkProjection tblQ) "t"))
  where

    SqlDelete _ sqliteDelete = delete table mkWhere
    tblQ = changeBeamRep getFieldName tblSettings
    getFieldName (Columnar' fd) =
      Columnar' $ QExpr $ pure $ fieldE (unqualifiedField (_fieldName fd))

runSqliteInsert :: (String -> IO ()) -> Connection -> SqliteInsertSyntax -> IO ()
runSqliteInsert logger conn (SqliteInsertSyntax tbl fields vs onConflict)
    -- If all expressions are simple expressions (no default), then just

  | SqliteInsertExpressions es <- vs, any (any (== SqliteExpressionDefault)) es =
      forM_ es $ \row -> do
        let (fields', row') = unzip $ filter ((/= SqliteExpressionDefault) . snd) $ zip fields row
            SqliteSyntax cmd vals = formatSqliteInsertOnConflict tbl fields' (SqliteInsertExpressions [ row' ]) onConflict
            cmdString = BL.unpack (toLazyByteString (withPlaceholders cmd))
        logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals))
        execute conn (fromString cmdString) (D.toList vals)
  | otherwise = do
      let SqliteSyntax cmd vals = formatSqliteInsertOnConflict tbl fields vs onConflict
          cmdString = BL.unpack (toLazyByteString (withPlaceholders cmd))
      logger (cmdString ++ ";\n-- With values: " ++ show (D.toList vals))
      execute conn (fromString cmdString) (D.toList vals)

-- | Build a 'SqliteInsertReturning' representing inserting the given values
-- into the given table and returning all inserted rows.
--
-- Use 'runInsertReturningList' to get the results.
insertReturning :: Beamable table
                => DatabaseEntity Sqlite db (TableEntity table)
                      -- ^ table to insert into
                -> SqlInsertValues Sqlite (table (QExpr Sqlite s))
                      -- ^ values/expressions to insert
                -> SqliteInsertReturning (table Identity)
insertReturning table vals =
  SqliteInsertReturning $
    case insert table vals of
      SqlInsert _ (SqliteInsertSyntax tbl fields values onConflict) ->
        [ formatSqliteInsertOnConflict tbl fields' values' onConflict <>
          emit " RETURNING " <> commas (map quotedIdentifier fields)
        | (fields', values') <- sqliteGroupByDefaults fields values
        ]
      SqlInsertNoRows -> []

-- | SQLite @INSERT ... RETURNING@ statement support with conflict handling.
-- The last argument takes the newly inserted row and returns the values to be
-- returned.
--
-- Use 'runInsertReturningList' to get the results.
insertOnConflictReturning
  :: forall s a table db
  .  (Beamable table, Projectible Sqlite a)
  => DatabaseEntity Sqlite db (TableEntity table)
      -- ^ table to insert into
  -> SqlInsertValues Sqlite (table (QExpr Sqlite s))
      -- ^ values/expressions to insert
  -> Beam.SqlConflictTarget Sqlite table
      -- ^ the target of the conflict (e.g. the row primary key)
  -> Beam.SqlConflictAction Sqlite table
      -- ^ what to do on conflict (e.g. nothing, override, etc)
  -> (table (QExpr Sqlite Void) -> a)
        -- ^ projection describing what to return
  -> SqliteInsertReturning (QExprToIdentity a)
insertOnConflictReturning
  table@(DatabaseEntity (DatabaseTable { dbTableSettings = tblSettings }))
  vals tgt action mkProjection =
  SqliteInsertReturning $
    case Beam.insertOnConflict table vals tgt action of
      SqlInsert _ (SqliteInsertSyntax tbl fields values onConflict) ->
        [ formatSqliteInsertOnConflict tbl fields' values' onConflict <>
          emit " RETURNING " <> commas (map fromSqliteExpression (project (Proxy @Sqlite) (mkProjection tblQ) "t"))
        | (fields', values') <- sqliteGroupByDefaults fields values
        ]
      SqlInsertNoRows -> []
  where
    tblQ = changeBeamRep getFieldName tblSettings
    getFieldName (Columnar' fd) =
      Columnar' $ QExpr $ pure $ fieldE (unqualifiedField (_fieldName fd))

-- | Runs a 'SqliteInsertReturning' statement and returns a result for each
-- inserted row.
runInsertReturningList
  :: ( MonadBeam be m
     , BeamSqlBackendSyntax be ~ SqliteCommandSyntax
     , FromBackendRow be a
     )
  => SqliteInsertReturning a
  -> m [a]
runInsertReturningList (SqliteInsertReturning syntaxes) =
  concat <$>
    traverse (\syntax -> runReturningList $ SqliteCommandSyntax syntax) syntaxes

instance Beam.BeamHasInsertOnConflict Sqlite where
  newtype SqlConflictTarget Sqlite table = SqliteConflictTarget
    { unSqliteConflictTarget :: table (QExpr Sqlite QInternal) -> SqliteSyntax }
  newtype SqlConflictAction Sqlite table = SqliteConflictAction
    { unSqliteConflictAction :: forall s. table (QField s) -> SqliteSyntax }

  insertOnConflict
    :: forall db table s. Beamable table
    => DatabaseEntity Sqlite db (TableEntity table)
    -> SqlInsertValues Sqlite (table (QExpr Sqlite s))
    -> Beam.SqlConflictTarget Sqlite table
    -> Beam.SqlConflictAction Sqlite table
    -> SqlInsert Sqlite table
  insertOnConflict (DatabaseEntity dt) values target action = case values of
    SqlInsertValuesEmpty -> SqlInsertNoRows
    SqlInsertValues vs -> SqlInsert (dbTableSettings dt) $
      let getFieldName
            :: forall a
            .  Columnar' (TableField table) a
            -> Columnar' (QField QInternal) a
          getFieldName (Columnar' fd) =
            Columnar' $ QField False (dbTableCurrentName dt) $ _fieldName fd
          tableFields = changeBeamRep getFieldName $ dbTableSettings dt
          tellFieldName _ _ f = tell [f] >> pure f
          fieldNames = execWriter $
            project' (Proxy @AnyType) (Proxy @((), T.Text)) tellFieldName tableFields
          currentField
            :: forall a
            .  Columnar' (QField QInternal) a
            -> Columnar' (QExpr Sqlite QInternal) a
          currentField (Columnar' f) = Columnar' $ current_ f
          tableCurrent = changeBeamRep currentField tableFields
      in SqliteInsertSyntax (tableNameFromEntity dt) fieldNames vs $ Just $
           SqliteOnConflictSyntax $ mconcat
             [ emit "ON CONFLICT "
             , unSqliteConflictTarget target tableCurrent
             , emit " DO "
             , unSqliteConflictAction action tableFields
             ]

  anyConflict = SqliteConflictTarget $ const mempty
  conflictingFields makeProjection = SqliteConflictTarget $ \table ->
    parens $ commas $ map fromSqliteExpression $
      project (Proxy @Sqlite) (makeProjection table) "t"
  conflictingFieldsWhere makeProjection makeWhere =
    SqliteConflictTarget $ \table -> mconcat
      [ unSqliteConflictTarget (Beam.conflictingFields makeProjection) table
      , emit " WHERE "
      , let QExpr mkE = makeWhere table
        in fromSqliteExpression $ mkE "t"
      ]

  onConflictDoNothing = SqliteConflictAction $ const $ emit "NOTHING"
  onConflictUpdateSet makeAssignments = SqliteConflictAction $ \table -> mconcat
    [ emit "UPDATE SET "
    , let QAssignment assignments = makeAssignments table $ excluded table
          emitAssignment (fieldName, expr) = mconcat
            [ fromSqliteFieldNameSyntax fieldName
            , emit " = "
            , fromSqliteExpression expr
            ]
      in commas $ map emitAssignment assignments
    ]
  onConflictUpdateSetWhere makeAssignments makeWhere =
    SqliteConflictAction $ \table -> mconcat
      [ unSqliteConflictAction (Beam.onConflictUpdateSet makeAssignments) table
      , emit " WHERE "
      , let QExpr mkE = makeWhere table $ excluded table
        in fromSqliteExpression $ mkE "t"
      ]

excluded
  :: forall table s
  .  Beamable table
  => table (QField s)
  -> table (QExpr Sqlite s)
excluded table = changeBeamRep excludedField table
  where excludedField (Columnar' (QField _ _ name)) =
          Columnar' $ QExpr $ const $ fieldE $ qualifiedField "excluded" name

-- | Use in conjunction with 'updateReturning'.
runUpdateReturningList
  :: ( MonadBeam be m
     , BeamSqlBackendSyntax be ~ SqliteCommandSyntax
     , FromBackendRow be a
     )
  => SqliteUpdateReturning a
  -> m [a]
runUpdateReturningList (SqliteUpdateReturning Nothing) = pure []
runUpdateReturningList (SqliteUpdateReturning (Just syntax)) =
  runReturningList $ SqliteCommandSyntax syntax

-- | SQLite @UPDATE ... RETURNING@ statement support. The last
-- argument takes the updated row and returns the values to be returned.
--
-- Use 'runUpdateReturningList' to get the results.
updateReturning :: forall a table db. Projectible Sqlite a
                => DatabaseEntity Sqlite db (TableEntity table)
                      -- ^ table to update
                -> (forall s. table (QField s) -> QAssignment Sqlite s)
                      -- ^ assignment for the update
                -> (forall s. table (QExpr Sqlite s) -> QExpr Sqlite s Bool)
                      -- ^ predicate selecting rows to update
                -> (table (QExpr Sqlite Void) -> a)
                      -- ^ projection describing what to return
                -> SqliteUpdateReturning (QExprToIdentity a)
updateReturning table@(DatabaseEntity (DatabaseTable { dbTableSettings = tblSettings }))
                mkAssignments
                mkWhere
                mkProjection =
  SqliteUpdateReturning $
    case update table mkAssignments mkWhere of
      SqlIdentityUpdate -> Nothing
      SqlUpdate _ sqliteUpdate ->
        Just $ fromSqliteUpdate sqliteUpdate
          <> emit " RETURNING "
          <> commas (map fromSqliteExpression (project (Proxy @Sqlite) (mkProjection tblQ) "t"))
  where
    tblQ = changeBeamRep getFieldName tblSettings
    getFieldName (Columnar' fd) =
      Columnar' $ QExpr $ pure $ fieldE (unqualifiedField (_fieldName fd))
