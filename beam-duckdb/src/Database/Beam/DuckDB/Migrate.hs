{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Migrations support for @beam-duckdb@. See "Database.Beam.Migrate" for
-- more information on beam migrations.
module Database.Beam.DuckDB.Migrate
  ( -- * Top-level @beam-migrate@ backend
    migrationBackend,
    DuckDBCommandSyntax,

    -- * DuckDB-specific data types

    --
    -- DuckDB has a number of data types that aren't part of standard SQL or
    -- that DuckDB names differently. These smart constructors produce
    -- 'Db.DataType's tagged with @DuckDB@ so they can be used in
    -- @beam-migrate@ schemas just like 'Database.Beam.Migrate.int' or
    -- 'Database.Beam.Migrate.smallint'.
    text,
    blob,
    tinyint,
    uuid,
    utinyint,
    usmallint,
    uinteger,
    ubigint,

    -- * Conversions and utilities
    getDbConstraints,
    getDbConstraintsForSchemas,
    migrateScript,
  )
where

import Control.Exception (SomeException (..), catch)
import Control.Monad (forM, void)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (object, withText, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.Int (Int8)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID.Types (UUID)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Beam.Backend.SQL
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Connection
  ( DuckDBM (..),
    liftIOWithHandle,
  )
import Database.Beam.DuckDB.Syntax
  ( DuckDBCommandSyntax (..),
    DuckDBDataTypeSyntax (..),
  )
import Database.Beam.DuckDB.Syntax.Builder
  ( duckDBRenderSyntaxScript,
    emit,
  )
import Database.Beam.Haskell.Syntax
import Database.Beam.Migrate.Actions
  ( createIndexActionProvider,
    defaultActionProvider,
    defaultSchemaActionProvider,
    dropIndexActionProvider,
  )
import qualified Database.Beam.Migrate.Backend as Tool
import qualified Database.Beam.Migrate.Checks as Db
import qualified Database.Beam.Migrate.SQL as Db
import qualified Database.Beam.Migrate.Serialization as Db
import qualified Database.Beam.Migrate.Types as Db
import qualified Database.Beam.Query.DataTypes as Db
import qualified Database.DuckDB.Simple as DuckDB
import Database.DuckDB.Simple.Types (Only (..))

-- | Top-level migration backend for use by @beam-migrate@ tools.
--
-- @since 0.3.1.0
migrationBackend :: Tool.BeamMigrationBackend DuckDB DuckDBM
migrationBackend =
  Tool.BeamMigrationBackend
    { Tool.backendName = "duckdb",
      Tool.backendConnStringExplanation =
        unlines
          [ "For beam-duckdb, this is the path to a DuckDB database file, or :memory: for an in-memory database.",
            "",
            "See <https://duckdb.org/docs/connect.html> for more information."
          ],
      Tool.backendGetDbConstraints = getDbConstraints,
      Tool.backendPredicateParsers =
        Db.sql92Deserializers
          <> Db.sql99DataTypeDeserializers
          <> Db.sql2008BigIntDataTypeDeserializers
          <> duckDBDataTypeDeserializers
          <> Db.beamCheckDeserializers,
      Tool.backendRenderSyntax = renderDuckDBCommand,
      Tool.backendFileExtension = "duckdb.sql",
      Tool.backendConvertToHaskell = duckDBPredConverter,
      Tool.backendActionProvider =
        mconcat
          [ defaultActionProvider,
            defaultSchemaActionProvider,
            createIndexActionProvider,
            dropIndexActionProvider
          ],
      Tool.backendRunSqlScript = \t -> liftIOWithHandle $ \conn ->
        void $ DuckDB.execute_ conn (DuckDB.Query t),
      Tool.backendWithTransaction = duckDBMWithTransaction,
      Tool.backendConnect = \options -> do
        conn <- DuckDB.open options
        pure
          Tool.BeamMigrateConnection
            { Tool.backendRun = \(DuckDBM action) ->
                catch
                  (Right <$> runReaderT action (\_ -> pure (), conn))
                  (\e -> pure (Left (show (e :: SomeException)))),
              Tool.backendClose = DuckDB.close conn
            }
    }
  where
    duckDBMWithTransaction :: forall a. DuckDBM a -> DuckDBM a
    duckDBMWithTransaction (DuckDBM (ReaderT k)) =
      DuckDBM $ ReaderT $ \ctx@(_, conn) ->
        DuckDB.withTransaction conn (k ctx)

    renderDuckDBCommand :: DuckDBCommandSyntax -> String
    renderDuckDBCommand (DuckDBCommandSyntax inner) =
      T.unpack (duckDBRenderSyntaxScript inner) ++ ";"

    duckDBPredConverter :: Tool.HaskellPredicateConverter
    duckDBPredConverter =
      Tool.sql92HsPredicateConverters @DuckDB duckDBTypeToHs
        <> Tool.hsPredicateConverter duckDBHasColumnConstraint
      where
        duckDBHasColumnConstraint
          ( Db.TableColumnHasConstraint tblNm colNm c ::
              Db.TableColumnHasConstraint DuckDB
            )
            | c == Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing =
                Just
                  ( Db.SomeDatabasePredicate
                      ( Db.TableColumnHasConstraint
                          tblNm
                          colNm
                          (Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing) ::
                          Db.TableColumnHasConstraint HsMigrateBackend
                      )
                  )
            | otherwise = Nothing

        duckDBTypeToHs :: DuckDBDataTypeSyntax -> Maybe HsDataType
        duckDBTypeToHs = Just . hsErrorType . T.unpack . duckDBRenderSyntaxScript . duckDBDataType

-- | Build a backend-specific 'Db.BeamSerializedDataType' for DuckDB, tagged
-- with the given keyword. Used to give DuckDB-only types a stable serialized
-- representation that round-trips through @beam-migrate@'s predicate cache.
duckDBDataTypeJSON :: Text -> Db.BeamSerializedDataType
duckDBDataTypeJSON tag =
  Db.BeamSerializedDataType (Db.beamSerializeJSON "duckdb" (object ["data-type" .= tag]))

duckDBTinyIntType :: DuckDBDataTypeSyntax
duckDBTinyIntType = DuckDBDataTypeSyntax (emit "TINYINT") (duckDBDataTypeJSON "tinyint")

duckDBHugeIntType :: DuckDBDataTypeSyntax
duckDBHugeIntType = DuckDBDataTypeSyntax (emit "HUGEINT") (duckDBDataTypeJSON "hugeint")

duckDBUuidType :: DuckDBDataTypeSyntax
duckDBUuidType = DuckDBDataTypeSyntax (emit "UUID") (duckDBDataTypeJSON "uuid")

duckDBUTinyIntType :: DuckDBDataTypeSyntax
duckDBUTinyIntType = DuckDBDataTypeSyntax (emit "UTINYINT") (duckDBDataTypeJSON "utinyint")

duckDBUSmallIntType :: DuckDBDataTypeSyntax
duckDBUSmallIntType = DuckDBDataTypeSyntax (emit "USMALLINT") (duckDBDataTypeJSON "usmallint")

duckDBUIntegerType :: DuckDBDataTypeSyntax
duckDBUIntegerType = DuckDBDataTypeSyntax (emit "UINTEGER") (duckDBDataTypeJSON "uinteger")

duckDBUBigIntType :: DuckDBDataTypeSyntax
duckDBUBigIntType = DuckDBDataTypeSyntax (emit "UBIGINT") (duckDBDataTypeJSON "ubigint")

duckDBUHugeIntType :: DuckDBDataTypeSyntax
duckDBUHugeIntType = DuckDBDataTypeSyntax (emit "UHUGEINT") (duckDBDataTypeJSON "uhugeint")

-- | DuckDB @TEXT@. Equivalent to 'Database.Beam.Migrate.characterLargeObject'
-- and to @VARCHAR@ without a length limit; DuckDB reports all three as
-- @VARCHAR@ in @information_schema@.
--
-- @since 0.3.1.0
text :: Db.DataType DuckDB Text
text = Db.DataType (varCharType Nothing Nothing)

-- | DuckDB @BLOB@ for arbitrary binary data. Equivalent to
-- 'Database.Beam.Migrate.binaryLargeObject' (DuckDB accepts both @BLOB@ and
-- @BYTEA@ in DDL but reports @BLOB@ in @information_schema@).
--
-- @since 0.3.1.0
blob :: Db.DataType DuckDB ByteString
blob = Db.DataType binaryLargeObjectType

-- | DuckDB @TINYINT@ — a signed 8-bit integer. Not part of SQL standard
-- nor of beam-core (which only exposes 'smallint' upwards).
--
-- @since 0.3.1.0
tinyint :: Db.DataType DuckDB Int8
tinyint = Db.DataType duckDBTinyIntType

-- | DuckDB @UUID@. Maps to 'Data.UUID.Types.UUID' from @uuid-types@.
--
-- @since 0.3.1.0
uuid :: Db.DataType DuckDB UUID
uuid = Db.DataType duckDBUuidType

-- | DuckDB @UTINYINT@ — an unsigned 8-bit integer.
--
-- @since 0.3.1.0
utinyint :: Db.DataType DuckDB Word8
utinyint = Db.DataType duckDBUTinyIntType

-- | DuckDB @USMALLINT@ — an unsigned 16-bit integer.
--
-- @since 0.3.1.0
usmallint :: Db.DataType DuckDB Word16
usmallint = Db.DataType duckDBUSmallIntType

-- | DuckDB @UINTEGER@ — an unsigned 32-bit integer.
--
-- @since 0.3.1.0
uinteger :: Db.DataType DuckDB Word32
uinteger = Db.DataType duckDBUIntegerType

-- | DuckDB @UBIGINT@ — an unsigned 64-bit integer.
--
-- @since 0.3.1.0
ubigint :: Db.DataType DuckDB Word64
ubigint = Db.DataType duckDBUBigIntType

-- | 'Db.BeamDeserializers' for DuckDB-specific data types not covered by
-- the standard SQL92/SQL99/SQL2008 deserializers. Wired into
-- 'migrationBackend' by default.
--
-- @since 0.3.1.0
duckDBDataTypeDeserializers :: Db.BeamDeserializers DuckDB
duckDBDataTypeDeserializers =
  Db.beamDeserializer $ \_ ->
    withText "DuckDB data type" $ \tag ->
      case tag of
        "tinyint" -> pure duckDBTinyIntType
        "hugeint" -> pure duckDBHugeIntType
        "uuid" -> pure duckDBUuidType
        "utinyint" -> pure duckDBUTinyIntType
        "usmallint" -> pure duckDBUSmallIntType
        "uinteger" -> pure duckDBUIntegerType
        "ubigint" -> pure duckDBUBigIntType
        "uhugeint" -> pure duckDBUHugeIntType
        _ -> fail $ "Unknown DuckDB data type with tag " <> T.unpack tag

-- | Render a series of 'Db.MigrationSteps' for DuckDB as a list of lazy
-- 'BL.ByteString's suitable for assembling a migration script.
--
-- @since 0.3.1.0
migrateScript :: Db.MigrationSteps DuckDB () a -> [BL.ByteString]
migrateScript steps =
  "-- Generated by beam-duckdb beam-migrate backend\n"
    : "\n"
    : appEndo (Db.migrateScript renderHeader renderCommand steps) []
  where
    renderHeader nm =
      Endo (("-- " <> BL.fromStrict (TE.encodeUtf8 nm) <> "\n") :)
    renderCommand (DuckDBCommandSyntax inner) =
      Endo ((BL.fromStrict (TE.encodeUtf8 (duckDBRenderSyntaxScript inner)) <> ";\n") :)

-- | Convert a DuckDB type name (as reported by @information_schema@) into a
-- 'DuckDBDataTypeSyntax', or 'Nothing' if the name is not recognised.
--
-- Handles common DuckDB-printed forms such as @INTEGER@, @VARCHAR(10)@,
-- @DECIMAL(18,3)@, @TIMESTAMP WITH TIME ZONE@, etc.
duckDBDataTypeFromText :: Text -> Maybe DuckDBDataTypeSyntax
duckDBDataTypeFromText t0 =
  let t = T.toUpper (T.strip t0)
      (base, parens) = splitParens t
   in case base of
        "BOOLEAN" -> Just booleanType
        "BOOL" -> Just booleanType
        "TINYINT" -> Just duckDBTinyIntType
        "INT1" -> Just duckDBTinyIntType
        "SMALLINT" -> Just smallIntType
        "INT2" -> Just smallIntType
        "INTEGER" -> Just intType
        "INT" -> Just intType
        "INT4" -> Just intType
        "BIGINT" -> Just bigIntType
        "INT8" -> Just bigIntType
        "HUGEINT" -> Just duckDBHugeIntType
        "UTINYINT" -> Just duckDBUTinyIntType
        "USMALLINT" -> Just duckDBUSmallIntType
        "UINTEGER" -> Just duckDBUIntegerType
        "UBIGINT" -> Just duckDBUBigIntType
        "UHUGEINT" -> Just duckDBUHugeIntType
        "REAL" -> Just realType
        "FLOAT" -> Just (floatType (parsePrec parens))
        "FLOAT4" -> Just realType
        "DOUBLE" -> Just doubleType
        "FLOAT8" -> Just doubleType
        "DOUBLE PRECISION" -> Just doubleType
        "NUMERIC" -> Just (numericType (parseNumericPrec parens))
        "DECIMAL" -> Just (decimalType (parseNumericPrec parens))
        "VARCHAR" -> Just (varCharType (parsePrec parens) Nothing)
        "CHAR" -> Just (charType (parsePrec parens) Nothing)
        "CHARACTER" -> Just (charType (parsePrec parens) Nothing)
        "CHARACTER VARYING" -> Just (varCharType (parsePrec parens) Nothing)
        "TEXT" -> Just (varCharType Nothing Nothing)
        "STRING" -> Just (varCharType Nothing Nothing)
        "BLOB" -> Just binaryLargeObjectType
        "BYTEA" -> Just binaryLargeObjectType
        "UUID" -> Just duckDBUuidType
        "DATE" -> Just dateType
        "TIME" -> Just (timeType (parsePrec parens) False)
        "TIME WITH TIME ZONE" -> Just (timeType (parsePrec parens) True)
        "TIMETZ" -> Just (timeType (parsePrec parens) True)
        "TIMESTAMP" -> Just (timestampType (parsePrec parens) False)
        "TIMESTAMP WITH TIME ZONE" -> Just (timestampType (parsePrec parens) True)
        "TIMESTAMPTZ" -> Just (timestampType (parsePrec parens) True)
        _ -> Nothing
  where
    splitParens :: Text -> (Text, Maybe Text)
    splitParens t =
      case T.breakOn "(" t of
        (base, "") -> (T.strip base, Nothing)
        (base, rest) ->
          let inside = T.dropWhile (== '(') (T.dropWhileEnd (== ')') rest)
           in (T.strip base, Just (T.strip inside))

    parsePrec :: Maybe Text -> Maybe Word
    parsePrec Nothing = Nothing
    parsePrec (Just t)
      | T.null digits = Nothing
      | otherwise = Just (read (T.unpack digits))
      where
        digits = T.takeWhile isDigit (T.strip t)

    parseNumericPrec :: Maybe Text -> Maybe (Word, Maybe Word)
    parseNumericPrec Nothing = Nothing
    parseNumericPrec (Just t) =
      case T.splitOn "," t of
        [p] -> fmap (,Nothing) (readMaybeWord (T.strip p))
        [p, d] -> case (readMaybeWord (T.strip p), readMaybeWord (T.strip d)) of
          (Just pw, Just dw) -> Just (pw, Just dw)
          (Just pw, Nothing) -> Just (pw, Nothing)
          _ -> Nothing
        _ -> Nothing

    readMaybeWord :: Text -> Maybe Word
    readMaybeWord t
      | T.null t || T.any (not . isDigit) t = Nothing
      | otherwise = Just (read (T.unpack t))

-- | Discover the set of 'Db.SomeDatabasePredicate's describing the current
-- DuckDB database. Includes schema, table, column, primary-key, secondary
-- index, and foreign-key constraint predicates.
--
-- @since 0.3.1.0
getDbConstraints :: DuckDBM [Db.SomeDatabasePredicate]
getDbConstraints = do
  schemas <-
    liftIOWithHandle $ \conn ->
      DuckDB.query_
        conn
        "SELECT schema_name FROM information_schema.schemata \
        \WHERE schema_name NOT IN ('information_schema', 'pg_catalog', 'main') \
        \AND schema_name NOT LIKE 'pg\\_%' ESCAPE '\\'"
  let userSchemas = map (\(Only s) -> s) schemas :: [Text]
  case userSchemas of
    [] -> getDbConstraintsForSchemas Nothing
    ss ->
      (++)
        <$> getDbConstraintsForSchemas (Just ss)
        <*> getDbConstraintsForSchemas Nothing

-- | Like 'getDbConstraints', but optionally restricted to a list of schemas.
-- If 'Nothing', the @main@ schema (the DuckDB default) is queried.
--
-- @since 0.3.1.0
getDbConstraintsForSchemas :: Maybe [Text] -> DuckDBM [Db.SomeDatabasePredicate]
getDbConstraintsForSchemas mSchemas = do
  let schemaFilter = case mSchemas of
        Nothing -> "table_schema = '" <> implicitSchema <> "'"
        Just ss ->
          "table_schema IN ("
            <> T.intercalate
              ","
              (map (\s -> "'" <> s <> "'") ss)
            <> ")"
      schemaPreds =
        case mSchemas of
          Nothing -> []
          Just ss -> map (Db.SomeDatabasePredicate . Db.SchemaExistsPredicate) ss

  tbls <-
    liftIOWithHandle $ \conn ->
      DuckDB.query_ conn $
        DuckDB.Query $
          T.unlines
            [ "SELECT table_schema, table_name FROM information_schema.tables",
              "WHERE table_type='BASE TABLE' AND " <> schemaFilter
            ]
  let tblExistsPreds =
        map
          ( \(s, t) ->
              Db.SomeDatabasePredicate
                ( Db.TableExistsPredicate
                    (Db.QualifiedName (qualifySchema s mSchemas) t)
                )
          )
          tbls

  columnsAndConstraints <-
    fmap mconcat . forM tbls $ \(s, t) -> collectColumnInfo s t mSchemas

  primaryKeyPreds <- collectPrimaryKeys schemaFilter mSchemas
  fkPreds <- collectForeignKeys schemaFilter mSchemas

  pure $
    concat
      [ schemaPreds,
        tblExistsPreds,
        columnsAndConstraints,
        primaryKeyPreds,
        fkPreds
      ]

-- | The implicit schema in case it's not specified
--
-- https://duckdb.org/docs/lts/sql/statements/use
implicitSchema :: Text
implicitSchema = "main"

-- | If we are looking at the 'main' schema, qualify columns/tables with
-- 'Nothing' (the implicit default). Otherwise qualify with the actual schema.
qualifySchema :: Text -> Maybe [Text] -> Maybe Text
qualifySchema s Nothing
  | s == implicitSchema = Nothing
  | otherwise = Just s
qualifySchema s _ = Just s

collectColumnInfo ::
  Text ->
  Text ->
  Maybe [Text] ->
  DuckDBM [Db.SomeDatabasePredicate]
collectColumnInfo schemaNm tblNm mSchemas = do
  columns <-
    liftIOWithHandle $ \conn ->
      DuckDB.query
        conn
        "SELECT column_name, data_type, is_nullable \
        \FROM information_schema.columns \
        \WHERE table_schema = ? AND table_name = ? \
        \ORDER BY ordinal_position"
        (schemaNm, tblNm)
  let qual = Db.QualifiedName (qualifySchema schemaNm mSchemas) tblNm
      mkPreds (colNm, typ, isNullable) =
        let dt = fromMaybe (unknownDuckDBType typ) (duckDBDataTypeFromText typ)
            colPred =
              Db.SomeDatabasePredicate
                ( Db.TableHasColumn qual colNm dt ::
                    Db.TableHasColumn DuckDB
                )
            notNullPred =
              ( [ Db.SomeDatabasePredicate
                    ( Db.TableColumnHasConstraint
                        qual
                        colNm
                        ( Db.constraintDefinitionSyntax
                            Nothing
                            Db.notNullConstraintSyntax
                            Nothing
                        ) ::
                        Db.TableColumnHasConstraint DuckDB
                    )
                | isNullable == ("NO" :: Text)
                ]
              )
         in colPred : notNullPred
  pure $ concatMap mkPreds (columns :: [(Text, Text, Text)])
  where
    unknownDuckDBType :: Text -> DuckDBDataTypeSyntax
    unknownDuckDBType = domainType

collectPrimaryKeys ::
  Text ->
  Maybe [Text] ->
  DuckDBM [Db.SomeDatabasePredicate]
collectPrimaryKeys schemaFilter mSchemas = do
  rows <-
    liftIOWithHandle $ \conn ->
      DuckDB.query_ conn $
        DuckDB.Query $
          T.unlines
            [ "SELECT tc.table_schema, tc.table_name, kcu.column_name, kcu.ordinal_position",
              "FROM information_schema.table_constraints tc",
              "JOIN information_schema.key_column_usage kcu",
              "  ON tc.constraint_name = kcu.constraint_name",
              " AND tc.constraint_schema = kcu.constraint_schema",
              " AND tc.table_name = kcu.table_name",
              "WHERE tc.constraint_type = 'PRIMARY KEY' AND tc." <> schemaFilter,
              "ORDER BY tc.table_schema, tc.table_name, kcu.ordinal_position"
            ]
  let grouped = groupByPk (rows :: [(Text, Text, Text, Int)])
  pure $
    map
      ( \((sch, tbl), cols) ->
          Db.SomeDatabasePredicate
            ( Db.TableHasPrimaryKey
                (Db.QualifiedName (qualifySchema sch mSchemas) tbl)
                cols
            )
      )
      grouped

groupByPk ::
  [(Text, Text, Text, Int)] ->
  [((Text, Text), [Text])]
groupByPk = go []
  where
    go acc [] = reverse acc
    go acc rs@((s, t, _, _) : _) =
      let (here, rest) = span (\(s', t', _, _) -> s == s' && t == t') rs
          cols = map (\(_, _, c, _) -> c) here
       in go (((s, t), cols) : acc) rest

collectForeignKeys ::
  Text ->
  Maybe [Text] ->
  DuckDBM [Db.SomeDatabasePredicate]
collectForeignKeys schemaFilter mSchemas = do
  rows <-
    liftIOWithHandle $ \conn ->
      DuckDB.query_ conn $
        DuckDB.Query $
          T.unlines
            [ "SELECT tc.table_schema, tc.table_name, kcu.column_name,",
              "       ccu.table_schema, ccu.table_name, ccu.column_name,",
              "       rc.update_rule, rc.delete_rule,",
              "       kcu.ordinal_position",
              "FROM information_schema.table_constraints tc",
              "JOIN information_schema.key_column_usage kcu",
              "  ON tc.constraint_name = kcu.constraint_name",
              " AND tc.constraint_schema = kcu.constraint_schema",
              "JOIN information_schema.referential_constraints rc",
              "  ON rc.constraint_name = tc.constraint_name",
              " AND rc.constraint_schema = tc.constraint_schema",
              "JOIN information_schema.constraint_column_usage ccu",
              "  ON ccu.constraint_name = rc.unique_constraint_name",
              " AND ccu.constraint_schema = rc.unique_constraint_schema",
              "WHERE tc.constraint_type = 'FOREIGN KEY' AND tc." <> schemaFilter,
              "ORDER BY tc.constraint_schema, tc.constraint_name, kcu.ordinal_position"
            ]
  pure $
    mapMaybe mkFk $
      groupByFk
        ( rows ::
            [ ( Text,
                Text,
                Text,
                Text,
                Text,
                Text,
                Text,
                Text,
                Int
              )
            ]
        )
  where
    groupByFk = go []
      where
        go acc [] = reverse acc
        go acc rs@((s, t, _, fs, ft, _, _, _, _) : _) =
          let (here, rest) =
                span
                  ( \(s', t', _, fs', ft', _, _, _, _) ->
                      s == s' && t == t' && fs == fs' && ft == ft'
                  )
                  rs
           in go (here : acc) rest

    mkFk [] = Nothing
    mkFk rows@((srcSchema, srcTbl, _, refSchema, refTbl, _, upd, del, _) : _) =
      let localCols = map (\(_, _, c, _, _, _, _, _, _) -> c) rows
          refCols = map (\(_, _, _, _, _, c, _, _, _) -> c) rows
       in case (NE.nonEmpty localCols, NE.nonEmpty refCols) of
            (Just lc, Just rc) ->
              Just $
                Db.SomeDatabasePredicate
                  ( Db.TableHasForeignKey
                      (Db.QualifiedName (qualifySchema srcSchema mSchemas) srcTbl)
                      lc
                      (Db.QualifiedName (qualifySchema refSchema mSchemas) refTbl)
                      rc
                      (parseDuckDBForeignKeyAction upd)
                      (parseDuckDBForeignKeyAction del)
                  )
            _ -> Nothing

    parseDuckDBForeignKeyAction :: Text -> Db.ForeignKeyAction
    parseDuckDBForeignKeyAction t =
      case T.toUpper t of
        "CASCADE" -> Db.ForeignKeyActionCascade
        "SET NULL" -> Db.ForeignKeyActionSetNull
        "SET DEFAULT" -> Db.ForeignKeyActionSetDefault
        "RESTRICT" -> Db.ForeignKeyActionRestrict
        "NO ACTION" -> Db.ForeignKeyNoAction
        _ -> Db.ForeignKeyNoAction
