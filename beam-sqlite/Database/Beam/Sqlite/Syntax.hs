{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing#-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | SQLite implementations of the Beam SQL syntax classes
--
-- The SQLite command syntax is implemented by 'SQLiteCommandSyntax'.
module Database.Beam.Sqlite.Syntax
  (  -- * SQLite syntaxes
    SqliteSyntax(..)

  , SqliteCommandSyntax(..)

  , SqliteSelectSyntax(..), SqliteInsertSyntax(..)
  , SqliteUpdateSyntax(..), SqliteDeleteSyntax(..)

  , SqliteInsertValuesSyntax(..)
  , SqliteColumnSchemaSyntax(..)
  , SqliteExpressionSyntax(..), SqliteValueSyntax(..)

    -- * SQLite data type syntax
  , SqliteDataTypeSyntax(..)
  , sqliteTextType, sqliteBlobType
  , sqliteBigIntType

    -- * Building and consuming 'SqliteSyntax'
  , fromSqliteCommand, formatSqliteInsert

  , emit, emitValue

  , sqliteEscape, withPlaceholders
  , sqliteRenderSyntaxScript
  ) where

import           Database.Beam.Backend.SQL
import           Database.Beam.Haskell.Syntax
import           Database.Beam.Migrate.Checks
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL.Builder hiding (fromSqlConstraintAttributes)
import           Database.Beam.Migrate.SQL.SQL92
import           Database.Beam.Migrate.Serialization
import           Database.Beam.Migrate.Types
import           Database.Beam.Query
import           Database.Beam.Query.SQL92

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import           Data.Coerce
import qualified Data.DList as DL
import           Data.Hashable
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
import           Data.Word

import           Database.SQLite.Simple (SQLData(..))

import           GHC.Float
import           GHC.Generics

-- | The syntax for SQLite is stored as a 'Builder' along with a list of data
-- that hasn't been serialized yet.
--
-- The first argument is a function that receives a builder for 'SQLData' and
-- returns the concrete syntax to embed into the query. For queries sent to the
-- backend, this is simply a function that returns @"?"@. Thus, the syntax sent
-- to the backend includes proper placeholders. The list of data is sent to the
-- SQLite library for proper escaping.
--
-- When the syntax is being serialized for display (for use in beam migrate for
-- example), the data builder attempts to properly format and escape the data.
-- This returns syntax suitable for inclusion in scripts. In this case, the
-- value list is ignored.
data SqliteSyntax = SqliteSyntax ((SQLData -> Builder) -> Builder) (DL.DList SQLData)
newtype SqliteData = SqliteData SQLData -- newtype for Hashable

instance Show SqliteSyntax where
  show (SqliteSyntax s d) =
    "SqliteSyntax (" <> show (toLazyByteString (withPlaceholders s)) <> ") " <> show d

instance Sql92DisplaySyntax SqliteSyntax where
  displaySyntax = BL.unpack . sqliteRenderSyntaxScript

instance Monoid SqliteSyntax where
  mempty = SqliteSyntax (\_ -> mempty) mempty
  mappend (SqliteSyntax ab av) (SqliteSyntax bb bv) =
    SqliteSyntax (\v -> ab v <> bb v) (av <> bv)

instance Eq SqliteSyntax where
  SqliteSyntax ab av == SqliteSyntax bb bv =
    toLazyByteString (withPlaceholders ab) ==
      toLazyByteString (withPlaceholders bb) &&
    av == bv

instance Hashable SqliteSyntax where
  hashWithSalt salt (SqliteSyntax s d) =
      hashWithSalt salt ( toLazyByteString (withPlaceholders s)
                        , map SqliteData (DL.toList d) )
instance Hashable SqliteData where
  hashWithSalt salt (SqliteData (SQLInteger i)) = hashWithSalt salt (0 :: Int, i)
  hashWithSalt salt (SqliteData (SQLFloat d))   = hashWithSalt salt (1 :: Int, d)
  hashWithSalt salt (SqliteData (SQLText t))    = hashWithSalt salt (2 :: Int, t)
  hashWithSalt salt (SqliteData (SQLBlob b))    = hashWithSalt salt (3 :: Int, b)
  hashWithSalt salt (SqliteData SQLNull)        = hashWithSalt salt (4 :: Int)

-- | Convert the first argument of 'SQLiteSyntax' to a 'ByteString' 'Builder',
-- where all the data has been replaced by @"?"@ placeholders.
withPlaceholders :: ((SQLData -> Builder) -> Builder) -> Builder
withPlaceholders build = build (\_ -> "?")

-- | Embed a 'ByteString' directly in the syntax
emit :: ByteString -> SqliteSyntax
emit b = SqliteSyntax (\_ -> byteString b) mempty

emit' :: Show a => a -> SqliteSyntax
emit' x = SqliteSyntax (\_ -> byteString (fromString (show x))) mempty

quotedIdentifier :: T.Text -> SqliteSyntax
quotedIdentifier txt = emit "\"" <> SqliteSyntax (\_ -> stringUtf8 (T.unpack (sqliteEscape txt))) mempty <> emit "\""

-- | A best effort attempt to implement the escaping rules of SQLite. This is
-- never used to escape data sent to the database; only for emitting scripts or
-- displaying syntax to the user.
sqliteEscape :: T.Text -> T.Text
sqliteEscape = T.concatMap (\c -> if c == '"' then "\"\"" else T.singleton c)

-- | Emit a properly escaped value into the syntax
--
-- This causes a literal @?@ 3
emitValue ::  SQLData -> SqliteSyntax
emitValue v = SqliteSyntax ($ v) (DL.singleton v)

-- | Render a 'SqliteSyntax' as a lazy 'BL.ByteString', for purposes of
-- displaying to a user. Embedded 'SQLData' is directly embedded into the
-- concrete syntax, with a best effort made to escape strings.
sqliteRenderSyntaxScript :: SqliteSyntax -> BL.ByteString
sqliteRenderSyntaxScript (SqliteSyntax s _) =
    toLazyByteString . s $ \case
      SQLInteger i -> int64Dec i
      SQLFloat   d -> doubleDec d
      SQLText    t -> TE.encodeUtf8Builder (sqliteEscape t)
      SQLBlob    b -> char8 'X' <> char8 '\'' <>
                      foldMap word8Hex (B.unpack b) <>
                      char8 '\''
      SQLNull      -> "NULL"

-- * Syntax types

-- | A SQLite command. @INSERT@ is special cased to handle @AUTO INCREMENT@
-- columns. The 'fromSqliteCommand' function will take an 'SqliteCommandSyntax'
-- and convert it into the correct 'SqliteSyntax'.
data SqliteCommandSyntax
  = SqliteCommandSyntax SqliteSyntax
  | SqliteCommandInsert SqliteInsertSyntax

-- | Convert a 'SqliteCommandSyntax' into a renderable 'SqliteSyntax'
fromSqliteCommand :: SqliteCommandSyntax -> SqliteSyntax
fromSqliteCommand (SqliteCommandSyntax s) = s
fromSqliteCommand (SqliteCommandInsert (SqliteInsertSyntax tbl fields values)) =
    formatSqliteInsert tbl fields values

-- | SQLite @SELECT@ syntax
newtype SqliteSelectSyntax = SqliteSelectSyntax { fromSqliteSelect :: SqliteSyntax }
instance HasQBuilder SqliteSelectSyntax where
  buildSqlQuery = buildSql92Query' False -- SQLite does not support arbitrarily nesting UNION, INTERSECT, and EXCEPT


-- | SQLite @INSERT@ syntax. This doesn't directly wrap 'SqliteSyntax' because
-- we need to do some processing on @INSERT@ statements to deal with @AUTO
-- INCREMENT@ columns. Use 'formatSqliteInsert' to turn 'SqliteInsertSyntax'
-- into 'SqliteSyntax'.
data SqliteInsertSyntax
  = SqliteInsertSyntax
  { sqliteInsertTable :: T.Text
  , sqliteInsertFields :: [ T.Text ]
  , sqliteInsertValues :: SqliteInsertValuesSyntax
  }

-- | SQLite @UPDATE@ syntax
newtype SqliteUpdateSyntax = SqliteUpdateSyntax { fromSqliteUpdate :: SqliteSyntax }
-- | SQLite @DELETE@ syntax
newtype SqliteDeleteSyntax = SqliteDeleteSyntax { fromSqliteDelete :: SqliteSyntax }

newtype SqliteSelectTableSyntax = SqliteSelectTableSyntax { fromSqliteSelectTable :: SqliteSyntax }

-- | Implements beam SQL expression syntaxes
data SqliteExpressionSyntax
  = SqliteExpressionSyntax SqliteSyntax
  | SqliteExpressionDefault
  deriving (Show, Eq, Generic)
instance Hashable SqliteExpressionSyntax
newtype SqliteFromSyntax = SqliteFromSyntax { fromSqliteFromSyntax :: SqliteSyntax }
newtype SqliteComparisonQuantifierSyntax = SqliteComparisonQuantifierSyntax { fromSqliteComparisonQuantifier :: SqliteSyntax }
newtype SqliteExtractFieldSyntax = SqliteExtractFieldSyntax { fromSqliteExtractField :: SqliteSyntax }
newtype SqliteAggregationSetQuantifierSyntax = SqliteAggregationSetQuantifierSyntax { fromSqliteAggregationSetQuantifier :: SqliteSyntax }
newtype SqliteProjectionSyntax = SqliteProjectionSyntax { fromSqliteProjection :: SqliteSyntax }
newtype SqliteGroupingSyntax = SqliteGroupingSyntax { fromSqliteGrouping :: SqliteSyntax }
newtype SqliteOrderingSyntax = SqliteOrderingSyntax { fromSqliteOrdering :: SqliteSyntax }
-- | SQLite syntax for values that can be embedded in 'SqliteSyntax'
newtype SqliteValueSyntax = SqliteValueSyntax { fromSqliteValue :: SqliteSyntax }
newtype SqliteTableSourceSyntax = SqliteTableSourceSyntax { fromSqliteTableSource :: SqliteSyntax }
newtype SqliteFieldNameSyntax = SqliteFieldNameSyntax { fromSqliteFieldNameSyntax :: SqliteSyntax }

-- | SQLite @VALUES@ clause in @INSERT@. Expressions need to be handled
-- explicitly in order to deal with @DEFAULT@ values and @AUTO INCREMENT@
-- columns.
data SqliteInsertValuesSyntax
  = SqliteInsertExpressions [ [ SqliteExpressionSyntax ] ]
  | SqliteInsertFromSql SqliteSelectSyntax
newtype SqliteCreateTableSyntax = SqliteCreateTableSyntax { fromSqliteCreateTable :: SqliteSyntax }
data SqliteTableOptionsSyntax = SqliteTableOptionsSyntax SqliteSyntax SqliteSyntax

-- | SQLite syntax for column schemas in @CREATE TABLE@ or @ALTER COLUMN ... ADD
-- COLUMN@ statements
newtype SqliteColumnSchemaSyntax
  = SqliteColumnSchemaSyntax { fromSqliteColumnSchema :: SqliteSyntax }
  deriving (Show, Eq, Hashable)

instance Sql92DisplaySyntax SqliteColumnSchemaSyntax where
  displaySyntax = displaySyntax . fromSqliteColumnSchema

-- | SQLite syntax that implements 'IsSql92DataTypeSyntax' and a good portion of
-- 'IsSql99DataTypeSyntax', except for array and row types.
data SqliteDataTypeSyntax
  = SqliteDataTypeSyntax
  { fromSqliteDataType :: SqliteSyntax
  , sqliteDataTypeToHs :: HsDataType
  , sqliteDataTypeSerialized :: BeamSerializedDataType
  } deriving (Show, Eq, Generic)
instance Hashable SqliteDataTypeSyntax where
  hashWithSalt salt (SqliteDataTypeSyntax s _ _) = hashWithSalt salt s
instance Sql92DisplaySyntax SqliteDataTypeSyntax where
  displaySyntax = displaySyntax . fromSqliteDataType

data SqliteColumnConstraintDefinitionSyntax
  = SqliteColumnConstraintDefinitionSyntax
  { fromSqliteColumnConstraintDefinition :: SqliteSyntax
  , sqliteColumnConstraintDefinitionSerialized :: BeamSerializedConstraintDefinition
  } deriving (Show, Eq)
instance Hashable SqliteColumnConstraintDefinitionSyntax where
  hashWithSalt salt (SqliteColumnConstraintDefinitionSyntax s _) = hashWithSalt salt s
instance Sql92DisplaySyntax SqliteColumnConstraintDefinitionSyntax where
  displaySyntax = displaySyntax . fromSqliteColumnConstraintDefinition

data SqliteColumnConstraintSyntax
    = SqliteColumnConstraintSyntax
    { fromSqliteColumnConstraint :: SqlConstraintAttributesBuilder -> SqliteSyntax
    , sqliteColumnConstraintSerialized :: BeamSerializedConstraint }
newtype SqliteTableConstraintSyntax = SqliteTableConstraintSyntax { fromSqliteTableConstraint :: SqliteSyntax }
data SqliteMatchTypeSyntax
    = SqliteMatchTypeSyntax
    { fromSqliteMatchType :: SqliteSyntax
    , sqliteMatchTypeSerialized :: BeamSerializedMatchType }
data SqliteReferentialActionSyntax
    = SqliteReferentialActionSyntax
    { fromSqliteReferentialAction :: SqliteSyntax
    , sqliteReferentialActionSerialized :: BeamSerializedReferentialAction }
newtype SqliteAlterTableSyntax = SqliteAlterTableSyntax { fromSqliteAlterTable :: SqliteSyntax }
newtype SqliteAlterTableActionSyntax = SqliteAlterTableActionSyntax { fromSqliteAlterTableAction :: Maybe SqliteSyntax }
newtype SqliteAlterColumnActionSyntax = SqliteAlterColumnActionSyntax { fromSqliteAlterColumnAction :: Maybe SqliteSyntax }
newtype SqliteDropTableSyntax = SqliteDropTableSyntax { fromSqliteDropTable :: SqliteSyntax }

fromSqliteExpression :: SqliteExpressionSyntax -> SqliteSyntax
fromSqliteExpression (SqliteExpressionSyntax s) = s
fromSqliteExpression SqliteExpressionDefault = emit "NULL /* DEFAULT */"

sqliteExpressionSerialized :: SqliteExpressionSyntax -> BeamSerializedExpression
sqliteExpressionSerialized = BeamSerializedExpression . TE.decodeUtf8 . BL.toStrict .
                             sqliteRenderSyntaxScript . fromSqliteExpression

-- | Format a SQLite @INSERT@ expression for the given table name, fields, and values.
formatSqliteInsert :: T.Text -> [ T.Text ] -> SqliteInsertValuesSyntax -> SqliteSyntax
formatSqliteInsert tblNm fields values =
  emit "INSERT INTO " <> quotedIdentifier tblNm <> parens (commas (map quotedIdentifier fields)) <> emit " " <>
  case values of
    SqliteInsertFromSql (SqliteSelectSyntax select) -> select
    SqliteInsertExpressions es ->
      emit "VALUES " <> commas (map (\row -> parens (commas (map fromSqliteExpression row)) ) es)

instance IsSql92Syntax SqliteCommandSyntax where
  type Sql92SelectSyntax SqliteCommandSyntax = SqliteSelectSyntax
  type Sql92InsertSyntax SqliteCommandSyntax = SqliteInsertSyntax
  type Sql92UpdateSyntax SqliteCommandSyntax = SqliteUpdateSyntax
  type Sql92DeleteSyntax SqliteCommandSyntax = SqliteDeleteSyntax

  selectCmd = SqliteCommandSyntax . fromSqliteSelect
  insertCmd = SqliteCommandInsert
  updateCmd = SqliteCommandSyntax . fromSqliteUpdate
  deleteCmd = SqliteCommandSyntax . fromSqliteDelete

instance IsSql92DdlCommandSyntax SqliteCommandSyntax where
  type Sql92DdlCommandCreateTableSyntax SqliteCommandSyntax = SqliteCreateTableSyntax
  type Sql92DdlCommandAlterTableSyntax SqliteCommandSyntax  = SqliteAlterTableSyntax
  type Sql92DdlCommandDropTableSyntax SqliteCommandSyntax = SqliteDropTableSyntax

  createTableCmd = SqliteCommandSyntax . fromSqliteCreateTable
  alterTableCmd = SqliteCommandSyntax . fromSqliteAlterTable
  dropTableCmd = SqliteCommandSyntax . fromSqliteDropTable

instance IsSql92DropTableSyntax SqliteDropTableSyntax where
  dropTableSyntax nm = SqliteDropTableSyntax (emit "DROP TABLE " <> quotedIdentifier nm)

instance IsSql92AlterTableSyntax SqliteAlterTableSyntax where
  type Sql92AlterTableAlterTableActionSyntax SqliteAlterTableSyntax = SqliteAlterTableActionSyntax

  alterTableSyntax nm action =
    SqliteAlterTableSyntax $
    case fromSqliteAlterTableAction action of
      Just alterTable ->
        emit "ALTER TABLE " <> quotedIdentifier nm <> alterTable
      Nothing ->
        emit "SELECT 1"

instance IsSql92AlterTableActionSyntax SqliteAlterTableActionSyntax where
  type Sql92AlterTableAlterColumnActionSyntax SqliteAlterTableActionSyntax = SqliteAlterColumnActionSyntax
  type Sql92AlterTableColumnSchemaSyntax SqliteAlterTableActionSyntax = SqliteColumnSchemaSyntax

  alterColumnSyntax columnNm columnAction =
    SqliteAlterTableActionSyntax $
    case fromSqliteAlterColumnAction columnAction of
      Nothing -> Nothing
      Just columnAction ->
        Just (emit "ALTER COLUMN " <> quotedIdentifier columnNm <> columnAction)
  addColumnSyntax columnNm schema =
    SqliteAlterTableActionSyntax . Just $
    emit "ADD COLUMN " <> quotedIdentifier columnNm <> emit " " <> fromSqliteColumnSchema schema
  dropColumnSyntax _ = SqliteAlterTableActionSyntax Nothing

  renameTableToSyntax newNm =
    SqliteAlterTableActionSyntax . Just $
    emit "RENAME TO " <> quotedIdentifier newNm

  renameColumnToSyntax oldNm newNm =
    SqliteAlterTableActionSyntax . Just $
    emit "RENAME COLUMN " <> quotedIdentifier oldNm <>
    emit " TO "           <> quotedIdentifier newNm

instance IsSql92AlterColumnActionSyntax SqliteAlterColumnActionSyntax where
  setNotNullSyntax = SqliteAlterColumnActionSyntax Nothing
  setNullSyntax = SqliteAlterColumnActionSyntax Nothing

instance IsSql92ColumnSchemaSyntax SqliteColumnSchemaSyntax where
  type Sql92ColumnSchemaColumnTypeSyntax SqliteColumnSchemaSyntax = SqliteDataTypeSyntax
  type Sql92ColumnSchemaExpressionSyntax SqliteColumnSchemaSyntax = SqliteExpressionSyntax
  type Sql92ColumnSchemaColumnConstraintDefinitionSyntax SqliteColumnSchemaSyntax = SqliteColumnConstraintDefinitionSyntax

  columnSchemaSyntax  ty defVal constraints collation =
    SqliteColumnSchemaSyntax $
    fromSqliteDataType ty <>
    maybe mempty (\defVal -> emit " DEFAULT " <> parens (fromSqliteExpression defVal)) defVal <>
    foldMap (\constraint -> emit " " <> fromSqliteColumnConstraintDefinition constraint <> emit " ") constraints <>
    maybe mempty (\c -> emit " COLLATE " <> quotedIdentifier c) collation

instance IsSql92ColumnConstraintDefinitionSyntax SqliteColumnConstraintDefinitionSyntax where
  type Sql92ColumnConstraintDefinitionConstraintSyntax SqliteColumnConstraintDefinitionSyntax = SqliteColumnConstraintSyntax
  type Sql92ColumnConstraintDefinitionAttributesSyntax SqliteColumnConstraintDefinitionSyntax = SqlConstraintAttributesBuilder

  constraintDefinitionSyntax nm def attrs =
    SqliteColumnConstraintDefinitionSyntax
      (maybe mempty (\nm' -> emit "CONSTRAINT " <> quotedIdentifier nm') nm <>
       fromSqliteColumnConstraint def (fromMaybe mempty attrs))
      (constraintDefinitionSyntax nm (sqliteColumnConstraintSerialized def)
                                     (fmap sqlConstraintAttributesSerialized attrs))

instance Sql92SerializableConstraintDefinitionSyntax SqliteColumnConstraintDefinitionSyntax where
  serializeConstraint = fromBeamSerializedConstraintDefinition . sqliteColumnConstraintDefinitionSerialized

instance IsSql92ColumnConstraintSyntax SqliteColumnConstraintSyntax where
  type Sql92ColumnConstraintMatchTypeSyntax SqliteColumnConstraintSyntax = SqliteMatchTypeSyntax
  type Sql92ColumnConstraintReferentialActionSyntax SqliteColumnConstraintSyntax = SqliteReferentialActionSyntax
  type Sql92ColumnConstraintExpressionSyntax SqliteColumnConstraintSyntax = SqliteExpressionSyntax

  notNullConstraintSyntax = SqliteColumnConstraintSyntax (\_ -> emit "NOT NULL") notNullConstraintSyntax
  uniqueColumnConstraintSyntax = SqliteColumnConstraintSyntax (\_ -> emit "UNIQUE") uniqueColumnConstraintSyntax
  primaryKeyColumnConstraintSyntax = SqliteColumnConstraintSyntax (\_ -> emit "PRIMARY KEY") primaryKeyColumnConstraintSyntax
  checkColumnConstraintSyntax expr =
    SqliteColumnConstraintSyntax (\_ -> emit "CHECK " <> parens (fromSqliteExpression expr))
                                 (checkColumnConstraintSyntax (sqliteExpressionSerialized expr))
  referencesConstraintSyntax tbl fields matchType onUpdate onDelete =
    SqliteColumnConstraintSyntax sqliteConstraint
                                 (referencesConstraintSyntax tbl fields (fmap sqliteMatchTypeSerialized matchType)
                                                             (fmap sqliteReferentialActionSerialized onUpdate)
                                                             (fmap sqliteReferentialActionSerialized onDelete))
    where
      sqliteConstraint (SqlConstraintAttributesBuilder atTime deferrable) =
        emit "REFERENCES " <> quotedIdentifier tbl <> parens (commas (map quotedIdentifier fields)) <>
        maybe mempty (\matchType' -> emit " MATCH " <> fromSqliteMatchType matchType') matchType <>
        maybe mempty (\onUpdate' -> emit " ON UPDATE " <> fromSqliteReferentialAction onUpdate') onUpdate <>
        maybe mempty (\onDelete' -> emit " ON DELETE " <> fromSqliteReferentialAction onDelete') onDelete <>
        case (deferrable, atTime) of
          (_, Just atTime) ->
            let deferrable' = fromMaybe False deferrable
            in (if deferrable' then emit " DEFERRABLE " else emit " NOT DEFERRABLE ") <>
               case atTime of
                 InitiallyDeferred -> emit "INITIALLY DEFERRED"
                 InitiallyImmediate -> emit "INITIALLY IMMEDIATE"
          (Just deferrable', _) ->
            if deferrable' then emit " DEFERRABLE" else emit " NOT DEFERRABLE"
          _ -> mempty

instance IsSql92MatchTypeSyntax SqliteMatchTypeSyntax where
  fullMatchSyntax = SqliteMatchTypeSyntax (emit "FULL") fullMatchSyntax
  partialMatchSyntax = SqliteMatchTypeSyntax (emit "PARTIAL") partialMatchSyntax

instance IsSql92ReferentialActionSyntax SqliteReferentialActionSyntax where
  referentialActionCascadeSyntax = SqliteReferentialActionSyntax (emit "CASCADE") referentialActionCascadeSyntax
  referentialActionSetNullSyntax = SqliteReferentialActionSyntax (emit "SET NULL") referentialActionSetNullSyntax
  referentialActionSetDefaultSyntax = SqliteReferentialActionSyntax (emit "SET DEFAULT") referentialActionSetDefaultSyntax
  referentialActionNoActionSyntax = SqliteReferentialActionSyntax (emit "NO ACTION") referentialActionNoActionSyntax

instance IsSql92TableConstraintSyntax SqliteTableConstraintSyntax where
  primaryKeyConstraintSyntax fields =
    SqliteTableConstraintSyntax $
    emit "PRIMARY KEY" <> parens (commas (map quotedIdentifier fields))

instance IsSql92CreateTableSyntax SqliteCreateTableSyntax where
  type Sql92CreateTableColumnSchemaSyntax SqliteCreateTableSyntax = SqliteColumnSchemaSyntax
  type Sql92CreateTableTableConstraintSyntax SqliteCreateTableSyntax = SqliteTableConstraintSyntax
  type Sql92CreateTableOptionsSyntax SqliteCreateTableSyntax = SqliteTableOptionsSyntax

  createTableSyntax _ nm fields constraints =
    let fieldDefs = map mkFieldDef fields
        constraintDefs = map fromSqliteTableConstraint constraints

        mkFieldDef (fieldNm, fieldTy) =
          quotedIdentifier fieldNm <> emit " " <> fromSqliteColumnSchema fieldTy

    in SqliteCreateTableSyntax $
       emit "CREATE TABLE " <> quotedIdentifier nm <> parens (commas (fieldDefs <> constraintDefs))

instance IsSql92DataTypeSyntax SqliteDataTypeSyntax where
  domainType nm = SqliteDataTypeSyntax (quotedIdentifier nm) (domainType nm) (domainType nm)
  charType prec charSet = SqliteDataTypeSyntax (emit "CHAR" <> sqliteOptPrec prec <> sqliteOptCharSet charSet)
                                               (charType prec charSet)
                                               (charType prec charSet)
  varCharType prec charSet = SqliteDataTypeSyntax (emit "VARCHAR" <> sqliteOptPrec prec <> sqliteOptCharSet charSet)
                                                  (varCharType prec charSet)
                                                  (varCharType prec charSet)
  nationalCharType prec = SqliteDataTypeSyntax (emit "NATIONAL CHAR" <> sqliteOptPrec prec)
                                               (nationalCharType prec)
                                               (nationalCharType prec)
  nationalVarCharType prec = SqliteDataTypeSyntax (emit "NATIONAL CHARACTER VARYING" <> sqliteOptPrec prec)
                                                  (nationalVarCharType prec)
                                                  (nationalVarCharType prec)

  bitType prec = SqliteDataTypeSyntax (emit "BIT" <> sqliteOptPrec prec) (bitType prec) (bitType prec)
  varBitType prec = SqliteDataTypeSyntax (emit "BIT VARYING" <> sqliteOptPrec prec) (varBitType prec) (varBitType prec)

  numericType prec = SqliteDataTypeSyntax (emit "NUMERIC" <> sqliteOptNumericPrec prec) (numericType prec) (numericType prec)
  decimalType prec = SqliteDataTypeSyntax (emit "DOUBLE" <> sqliteOptNumericPrec prec) (decimalType prec) (decimalType prec)

  intType = SqliteDataTypeSyntax (emit "INTEGER") intType intType
  smallIntType = SqliteDataTypeSyntax (emit "SMALLINT") smallIntType smallIntType

  floatType prec = SqliteDataTypeSyntax (emit "FLOAT" <> sqliteOptPrec prec) (floatType prec) (floatType prec)
  doubleType = SqliteDataTypeSyntax (emit "DOUBLE PRECISION") doubleType doubleType
  realType = SqliteDataTypeSyntax (emit "REAL") realType realType
  dateType = SqliteDataTypeSyntax (emit "DATE") dateType dateType
  timeType prec withTz = SqliteDataTypeSyntax (emit "TIME" <> sqliteOptPrec prec <> if withTz then emit " WITH TIME ZONE" else mempty)
                                              (timeType prec withTz) (timeType prec withTz)
  timestampType prec withTz = SqliteDataTypeSyntax (emit "TIMESTAMP" <> sqliteOptPrec prec <> if withTz then emit " WITH TIME ZONE" else mempty)
                                                   (timestampType prec withTz) (timestampType prec withTz)

instance IsSql99DataTypeSyntax SqliteDataTypeSyntax where
  characterLargeObjectType = sqliteTextType
  binaryLargeObjectType = sqliteBlobType
  booleanType = SqliteDataTypeSyntax (emit "BOOLEAN") booleanType booleanType
  arrayType _ _ = error "SQLite does not support arrayType"
  rowType _ = error "SQLite does not support rowType"

instance IsSql2008BigIntDataTypeSyntax SqliteDataTypeSyntax where
  bigIntType = sqliteBigIntType

sqliteTextType, sqliteBlobType, sqliteBigIntType :: SqliteDataTypeSyntax
sqliteTextType = SqliteDataTypeSyntax (emit "TEXT")
                                      (HsDataType (hsVarFrom "sqliteText" "Database.Beam.Sqlite")
                                                  (HsType (tyConNamed "Text")
                                                          (importSome "Data.Text" [importTyNamed "Text"]))
                                                  characterLargeObjectType)
                                     characterLargeObjectType
sqliteBlobType = SqliteDataTypeSyntax (emit "BLOB")
                                      (HsDataType (hsVarFrom "sqliteBlob" "Database.Beam.Sqlite")
                                                  (HsType (tyConNamed "ByteString")
                                                          (importSome "Data.ByteString" [importTyNamed "ByteString"]))
                                                  binaryLargeObjectType)
                                     binaryLargeObjectType
sqliteBigIntType = SqliteDataTypeSyntax (emit "BIGINT")
                                      (HsDataType (hsVarFrom "sqliteBigInt" "Database.Beam.Sqlite")
                                                  (HsType (tyConNamed "Int64")
                                                          (importSome "Data.Int" [importTyNamed "Int64"]))
                                                  bigIntType)
                                     bigIntType

instance Sql92SerializableDataTypeSyntax SqliteDataTypeSyntax where
  serializeDataType = fromBeamSerializedDataType . sqliteDataTypeSerialized

sqliteOptPrec :: Maybe Word -> SqliteSyntax
sqliteOptPrec Nothing = mempty
sqliteOptPrec (Just x) = parens (emit (fromString (show x)))

sqliteOptNumericPrec :: Maybe (Word, Maybe Word)  -> SqliteSyntax
sqliteOptNumericPrec Nothing = mempty
sqliteOptNumericPrec (Just (prec, Nothing)) = sqliteOptPrec (Just prec)
sqliteOptNumericPrec (Just (prec, Just dec)) = parens $ emit (fromString (show prec)) <> emit ", " <> emit (fromString (show dec))

sqliteOptCharSet :: Maybe T.Text -> SqliteSyntax
sqliteOptCharSet Nothing = mempty
sqliteOptCharSet (Just cs) = emit " CHARACTER SET " <> emit (TE.encodeUtf8 cs)

instance IsSql92SelectSyntax SqliteSelectSyntax where
  type Sql92SelectSelectTableSyntax SqliteSelectSyntax = SqliteSelectTableSyntax
  type Sql92SelectOrderingSyntax SqliteSelectSyntax = SqliteOrderingSyntax

  selectStmt tbl ordering limit offset =
    SqliteSelectSyntax $
    fromSqliteSelectTable tbl <>
    (case ordering of
       [] -> mempty
       _ -> emit " ORDER BY " <> commas (coerce ordering)) <>
    case (limit, offset) of
      (Nothing, Nothing) -> mempty
      (Just limit, Nothing) -> emit " LIMIT " <> emit' limit
      (Nothing, Just offset) -> emit " LIMIT -1 OFFSET " <> emit' offset
      (Just limit, Just offset) -> emit " LIMIT " <> emit' limit <>
                                   emit " OFFSET " <> emit' offset

instance IsSql92SelectTableSyntax SqliteSelectTableSyntax where
  type Sql92SelectTableSelectSyntax SqliteSelectTableSyntax = SqliteSelectSyntax
  type Sql92SelectTableExpressionSyntax SqliteSelectTableSyntax = SqliteExpressionSyntax
  type Sql92SelectTableProjectionSyntax SqliteSelectTableSyntax = SqliteProjectionSyntax
  type Sql92SelectTableFromSyntax SqliteSelectTableSyntax = SqliteFromSyntax
  type Sql92SelectTableGroupingSyntax SqliteSelectTableSyntax = SqliteGroupingSyntax
  type Sql92SelectTableSetQuantifierSyntax SqliteSelectTableSyntax = SqliteAggregationSetQuantifierSyntax

  selectTableStmt setQuantifier proj from where_ grouping having =
    SqliteSelectTableSyntax $
    emit "SELECT " <>
    maybe mempty (<> emit " ") (fromSqliteAggregationSetQuantifier <$> setQuantifier) <>
    fromSqliteProjection proj <>
    maybe mempty (emit " FROM " <>) (fromSqliteFromSyntax <$> from) <>
    maybe mempty (emit " WHERE " <>) (fromSqliteExpression <$> where_) <>
    maybe mempty (emit " GROUP BY " <>) (fromSqliteGrouping <$> grouping) <>
    maybe mempty (emit " HAVING " <>) (fromSqliteExpression <$> having)

  unionTables all = tableOp (if all then "UNION ALL" else "UNION")
  intersectTables all = tableOp (if all then "INTERSECT ALL" else "INTERSECT")
  exceptTable all = tableOp (if all then "EXCEPT ALL" else "EXCEPT")

tableOp :: ByteString -> SqliteSelectTableSyntax -> SqliteSelectTableSyntax -> SqliteSelectTableSyntax
tableOp op a b =
  SqliteSelectTableSyntax $
  fromSqliteSelectTable a <> spaces (emit op) <> fromSqliteSelectTable b

instance IsSql92FromSyntax SqliteFromSyntax where
  type Sql92FromExpressionSyntax SqliteFromSyntax = SqliteExpressionSyntax
  type Sql92FromTableSourceSyntax SqliteFromSyntax = SqliteTableSourceSyntax

  fromTable tableSrc Nothing = SqliteFromSyntax (fromSqliteTableSource tableSrc)
  fromTable tableSrc (Just nm) =
    SqliteFromSyntax (fromSqliteTableSource tableSrc <> emit " AS " <> quotedIdentifier nm)

  innerJoin = _join "INNER JOIN"
  leftJoin = _join "LEFT JOIN"
  rightJoin = _join "RIGHT JOIN"

_join :: ByteString -> SqliteFromSyntax -> SqliteFromSyntax -> Maybe SqliteExpressionSyntax -> SqliteFromSyntax
_join joinType a b Nothing =
  SqliteFromSyntax (fromSqliteFromSyntax a <> spaces (emit joinType) <> fromSqliteFromSyntax b)
_join joinType a b (Just on) =
  SqliteFromSyntax (fromSqliteFromSyntax a <> spaces (emit joinType) <> fromSqliteFromSyntax b <> emit " ON " <> fromSqliteExpression on)

instance IsSql92ProjectionSyntax SqliteProjectionSyntax where
  type Sql92ProjectionExpressionSyntax SqliteProjectionSyntax = SqliteExpressionSyntax

  projExprs exprs =
    SqliteProjectionSyntax $
    commas (map (\(expr, nm) -> fromSqliteExpression expr <>
                                maybe mempty (\nm -> emit " AS " <> quotedIdentifier nm) nm) exprs)

instance IsSql92FieldNameSyntax SqliteFieldNameSyntax where
  qualifiedField a b =
    SqliteFieldNameSyntax $
    quotedIdentifier a <> emit "." <> quotedIdentifier b
  unqualifiedField a =
    SqliteFieldNameSyntax $
    quotedIdentifier a

instance IsSql92TableSourceSyntax SqliteTableSourceSyntax where
  type Sql92TableSourceSelectSyntax SqliteTableSourceSyntax = SqliteSelectSyntax

  tableNamed = SqliteTableSourceSyntax . quotedIdentifier
  tableFromSubSelect s =
    SqliteTableSourceSyntax (parens (fromSqliteSelect s))

instance IsSql92GroupingSyntax SqliteGroupingSyntax where
  type Sql92GroupingExpressionSyntax SqliteGroupingSyntax = SqliteExpressionSyntax

  groupByExpressions es =
    SqliteGroupingSyntax $
    commas (map fromSqliteExpression es)

instance IsSql92OrderingSyntax SqliteOrderingSyntax where
  type Sql92OrderingExpressionSyntax SqliteOrderingSyntax = SqliteExpressionSyntax

  ascOrdering e = SqliteOrderingSyntax (fromSqliteExpression e <> emit " ASC")
  descOrdering e = SqliteOrderingSyntax (fromSqliteExpression e <> emit " DESC")

instance IsSqlExpressionSyntaxStringType SqliteExpressionSyntax T.Text
instance IsSqlExpressionSyntaxStringType SqliteExpressionSyntax String
instance HasSqlValueSyntax SqliteValueSyntax Int where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Int8 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Int16 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Int32 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Int64 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Word where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Word16 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Word32 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Word64 where
  sqlValueSyntax i = SqliteValueSyntax (emitValue (SQLInteger (fromIntegral i)))
instance HasSqlValueSyntax SqliteValueSyntax Float where
  sqlValueSyntax f = SqliteValueSyntax (emitValue (SQLFloat (float2Double f)))
instance HasSqlValueSyntax SqliteValueSyntax Double where
  sqlValueSyntax f = SqliteValueSyntax (emitValue (SQLFloat f))
instance HasSqlValueSyntax SqliteValueSyntax Bool where
  sqlValueSyntax = sqlValueSyntax . (\b -> if b then 1 else 0 :: Int)
instance HasSqlValueSyntax SqliteValueSyntax SqlNull where
  sqlValueSyntax _ = SqliteValueSyntax (emit "NULL")
instance HasSqlValueSyntax SqliteValueSyntax String where
  sqlValueSyntax s = SqliteValueSyntax (emitValue (SQLText (fromString s)))
instance HasSqlValueSyntax SqliteValueSyntax T.Text where
  sqlValueSyntax s = SqliteValueSyntax (emitValue (SQLText s))
instance HasSqlValueSyntax SqliteValueSyntax x =>
  HasSqlValueSyntax SqliteValueSyntax (Maybe x) where
  sqlValueSyntax (Just x) = sqlValueSyntax x
  sqlValueSyntax Nothing = sqlValueSyntax SqlNull
instance HasSqlValueSyntax SqliteValueSyntax (Maybe x) => HasSqlValueSyntax SqliteValueSyntax (Auto x) where
  sqlValueSyntax (Auto x) = sqlValueSyntax x

instance IsCustomSqlSyntax SqliteExpressionSyntax where
  newtype CustomSqlSyntax SqliteExpressionSyntax =
    SqliteCustomExpressionSyntax { fromSqliteCustomExpression :: SqliteSyntax }
    deriving Monoid

  customExprSyntax = SqliteExpressionSyntax . fromSqliteCustomExpression
  renderSyntax = SqliteCustomExpressionSyntax . fromSqliteExpression
instance IsString (CustomSqlSyntax SqliteExpressionSyntax) where
  fromString = SqliteCustomExpressionSyntax . emit . fromString

instance IsSql92QuantifierSyntax SqliteComparisonQuantifierSyntax where
  quantifyOverAll = SqliteComparisonQuantifierSyntax (emit "ALL")
  quantifyOverAny = SqliteComparisonQuantifierSyntax (emit "ANY")

instance IsSql92ExpressionSyntax SqliteExpressionSyntax where
  type Sql92ExpressionValueSyntax SqliteExpressionSyntax = SqliteValueSyntax
  type Sql92ExpressionSelectSyntax SqliteExpressionSyntax = SqliteSelectSyntax
  type Sql92ExpressionFieldNameSyntax SqliteExpressionSyntax = SqliteFieldNameSyntax
  type Sql92ExpressionQuantifierSyntax SqliteExpressionSyntax = SqliteComparisonQuantifierSyntax
  type Sql92ExpressionCastTargetSyntax SqliteExpressionSyntax = SqliteDataTypeSyntax
  type Sql92ExpressionExtractFieldSyntax SqliteExpressionSyntax = SqliteExtractFieldSyntax

  addE = binOp "+"; subE = binOp "-"; mulE = binOp "*"; divE = binOp "/"
  modE = binOp "%"; orE = binOp "OR"; andE = binOp "AND"; likeE = binOp "LIKE"
  overlapsE = binOp "OVERLAPS"

  eqE = compOp "="; neqE = compOp "<>"; ltE = compOp "<"; gtE = compOp ">"
  leE = compOp "<="; geE = compOp ">="

  negateE = unOp "-"; notE = unOp "NOT"

  isNotNullE = postFix "IS NOT NULL"; isNullE = postFix "IS NULL"
  isTrueE = postFix "IS TRUE"; isNotTrueE = postFix "IS NOT TRUE"
  isFalseE = postFix "IS FALSE"; isNotFalseE = postFix "IS NOT FALSE"
  isUnknownE = postFix "IS UNKNOWN"; isNotUnknownE = postFix "IS NOT UNKNOWN"

  existsE select = SqliteExpressionSyntax (emit "EXISTS " <> parens (fromSqliteSelect select))
  uniqueE select = SqliteExpressionSyntax (emit "UNIQUE " <> parens (fromSqliteSelect select))

  betweenE a b c = SqliteExpressionSyntax (parens (fromSqliteExpression a) <>
                                           emit " BETWEEN " <>
                                           parens (fromSqliteExpression b) <>
                                           emit " AND " <>
                                           parens (fromSqliteExpression c))

  valueE = SqliteExpressionSyntax . fromSqliteValue

  rowE vs = SqliteExpressionSyntax (parens (commas (map fromSqliteExpression vs)))
  fieldE = SqliteExpressionSyntax . fromSqliteFieldNameSyntax

  subqueryE = SqliteExpressionSyntax . parens . fromSqliteSelect

  positionE needle haystack =
    SqliteExpressionSyntax $
    emit "POSITION" <> parens (parens (fromSqliteExpression needle) <> emit " IN " <> parens (fromSqliteExpression haystack))
  nullIfE a b =
    SqliteExpressionSyntax $
    emit "NULLIF" <> parens (fromSqliteExpression a <> emit ", " <> fromSqliteExpression b)
  absE x = SqliteExpressionSyntax (emit "ABS" <> parens (fromSqliteExpression x))
  bitLengthE x = SqliteExpressionSyntax (emit "BIT_LENGTH" <> parens (fromSqliteExpression x))
  charLengthE x = SqliteExpressionSyntax (emit "CHAR_LENGTH" <> parens (fromSqliteExpression x))
  octetLengthE x = SqliteExpressionSyntax (emit "OCTET_LENGTH" <> parens (fromSqliteExpression x))
  lowerE x = SqliteExpressionSyntax (emit "LOWER" <> parens (fromSqliteExpression x))
  upperE x = SqliteExpressionSyntax (emit "UPPER" <> parens (fromSqliteExpression x))
  coalesceE es = SqliteExpressionSyntax (emit "COALESCE" <> parens (commas (map fromSqliteExpression es)))
  extractE field from =
    SqliteExpressionSyntax $
    emit "EXTRACT" <> parens (fromSqliteExtractField field <> emit " FROM " <> parens (fromSqliteExpression from))
  castE e t = SqliteExpressionSyntax (emit "CAST" <> parens (parens (fromSqliteExpression e) <> emit " AS " <> fromSqliteDataType t))
  caseE cases else_ =
    SqliteExpressionSyntax $
    emit "CASE " <>
    foldMap (\(cond, res) -> emit "WHEN " <> fromSqliteExpression cond <> emit " THEN " <> fromSqliteExpression res <> emit " ") cases <>
    emit "ELSE " <> fromSqliteExpression else_ <> emit " END"

  currentTimestampE = SqliteExpressionSyntax (emit "CURRENT_TIMESTAMP")

  defaultE = SqliteExpressionDefault
  inE e es = SqliteExpressionSyntax (parens (fromSqliteExpression e) <> emit " IN " <> parens (commas (map fromSqliteExpression es)))

instance IsSql99ConcatExpressionSyntax SqliteExpressionSyntax where
  concatE [] = valueE (sqlValueSyntax ("" :: T.Text))
  concatE (x:xs) =
    SqliteExpressionSyntax $ parens $
    foldr (\a b -> parens (fromSqliteExpression a) <> emit " || " <> parens b) (fromSqliteExpression x) xs

binOp :: ByteString -> SqliteExpressionSyntax -> SqliteExpressionSyntax -> SqliteExpressionSyntax
binOp op a b =
  SqliteExpressionSyntax $
  parens (fromSqliteExpression a) <> emit " " <> emit op <> emit " " <> parens (fromSqliteExpression b)

compOp :: ByteString -> Maybe SqliteComparisonQuantifierSyntax
       -> SqliteExpressionSyntax -> SqliteExpressionSyntax
       -> SqliteExpressionSyntax
compOp op quantifier a b =
  SqliteExpressionSyntax $
  parens (fromSqliteExpression a) <>
  emit op <>
  parens (maybe mempty (\q -> emit " " <> fromSqliteComparisonQuantifier q <> emit " ") quantifier <>
          fromSqliteExpression b)

unOp, postFix :: ByteString -> SqliteExpressionSyntax -> SqliteExpressionSyntax
unOp op a =
  SqliteExpressionSyntax (emit op <> parens (fromSqliteExpression a))
postFix op a =
  SqliteExpressionSyntax (parens (fromSqliteExpression a) <> emit " " <> emit op)

instance IsSql92AggregationExpressionSyntax SqliteExpressionSyntax where
  type Sql92AggregationSetQuantifierSyntax SqliteExpressionSyntax = SqliteAggregationSetQuantifierSyntax

  countAllE = SqliteExpressionSyntax (emit "COUNT(*)")
  countE = unAgg "COUNT"
  sumE = unAgg "SUM"
  avgE = unAgg "AVG"
  minE = unAgg "MIN"
  maxE = unAgg "MAX"

unAgg :: ByteString -> Maybe SqliteAggregationSetQuantifierSyntax -> SqliteExpressionSyntax
      -> SqliteExpressionSyntax
unAgg fn q e =
  SqliteExpressionSyntax $
  emit fn <> parens (maybe mempty (\q -> fromSqliteAggregationSetQuantifier q <> emit " ") q <>
                     fromSqliteExpression e)

instance IsSql92AggregationSetQuantifierSyntax SqliteAggregationSetQuantifierSyntax where
  setQuantifierDistinct = SqliteAggregationSetQuantifierSyntax (emit "DISTINCT")
  setQuantifierAll = SqliteAggregationSetQuantifierSyntax (emit "ALL")

instance IsSql92InsertSyntax SqliteInsertSyntax where
  type Sql92InsertValuesSyntax SqliteInsertSyntax = SqliteInsertValuesSyntax

  insertStmt = SqliteInsertSyntax

instance IsSql92InsertValuesSyntax SqliteInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax SqliteInsertValuesSyntax = SqliteExpressionSyntax
  type Sql92InsertValuesSelectSyntax SqliteInsertValuesSyntax = SqliteSelectSyntax

  insertSqlExpressions = SqliteInsertExpressions
  insertFromSql = SqliteInsertFromSql

instance IsSql92UpdateSyntax SqliteUpdateSyntax where
  type Sql92UpdateFieldNameSyntax SqliteUpdateSyntax = SqliteFieldNameSyntax
  type Sql92UpdateExpressionSyntax SqliteUpdateSyntax = SqliteExpressionSyntax

  updateStmt tbl fields where_ =
    SqliteUpdateSyntax $
    emit "UPDATE " <> quotedIdentifier tbl <>
    (case fields of
       [] -> mempty
       _ -> emit " SET " <>
            commas (map (\(field, val) -> fromSqliteFieldNameSyntax field <> emit "=" <> fromSqliteExpression val) fields)) <>
    maybe mempty (\where_ -> emit " WHERE " <> fromSqliteExpression where_) where_

instance IsSql92DeleteSyntax SqliteDeleteSyntax where
  type Sql92DeleteExpressionSyntax SqliteDeleteSyntax = SqliteExpressionSyntax

  deleteStmt tbl where_ =
    SqliteDeleteSyntax $
    emit "DELETE FROM " <> quotedIdentifier tbl <>
    maybe mempty (\where_ -> emit " WHERE " <> fromSqliteExpression where_) where_

spaces, parens :: SqliteSyntax -> SqliteSyntax
spaces a = emit " " <> a <> emit " " 
parens a = emit "(" <> a <> emit ")"

commas :: [SqliteSyntax] -> SqliteSyntax
commas [] = mempty
commas [x] = x
commas (x:xs) = x <> foldMap (emit ", " <>) xs

instance HasDefaultSqlDataType SqliteDataTypeSyntax (SqlSerial Int) where
  defaultSqlDataType _ _ = intType
instance HasDefaultSqlDataTypeConstraints SqliteColumnSchemaSyntax (SqlSerial Int) where
  defaultSqlDataTypeConstraints _ _ False = [ FieldCheck $ \tblNm colNm -> p (TableColumnHasConstraint tblNm colNm c :: TableColumnHasConstraint SqliteColumnSchemaSyntax) ]
    where c = constraintDefinitionSyntax Nothing (SqliteColumnConstraintSyntax (\_ -> emit "DEFAULT ROWID")
                                                                               (BeamSerializedConstraint (beamSerializeJSON "sqlite" "serial"))) Nothing
  defaultSqlDataTypeConstraints _ _ _ = []

instance HasDefaultSqlDataType SqliteDataTypeSyntax ByteString where
  -- TODO we should somehow allow contsraints based on backend
  defaultSqlDataType _ _ = sqliteBlobType
instance HasDefaultSqlDataTypeConstraints SqliteColumnSchemaSyntax ByteString

instance HasDefaultSqlDataType SqliteDataTypeSyntax LocalTime where
  defaultSqlDataType _ _ = timestampType Nothing False
instance HasDefaultSqlDataTypeConstraints SqliteColumnSchemaSyntax LocalTime

instance HasSqlValueSyntax SqliteValueSyntax ByteString where
  sqlValueSyntax bs = SqliteValueSyntax (emitValue (SQLBlob bs))

instance HasSqlValueSyntax SqliteValueSyntax LocalTime where
  sqlValueSyntax tm = SqliteValueSyntax (emitValue (SQLText (fromString tmStr)))
    where tmStr = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q")) tm

instance HasSqlValueSyntax SqliteValueSyntax Day where
  sqlValueSyntax tm = SqliteValueSyntax (emitValue (SQLText (fromString tmStr)))
    where tmStr = formatTime defaultTimeLocale (iso8601DateFormat Nothing) tm
