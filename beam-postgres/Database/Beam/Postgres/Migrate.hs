{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

-- | Migrations support for beam-postgres. See "Database.Beam.Migrate" for more
-- information on beam migrations.
module Database.Beam.Postgres.Migrate
  ( PgCommandSyntax, migrationBackend
  , postgresDataTypeDeserializers
  , pgPredConverter
  , getDbConstraints
  , getDbConstraintsForSchemas
  , pgTypeToHs
  , migrateScript
  , writeMigrationScript
  , pgDataTypeFromAtt

    -- * Postgres data types
  , tsquery, tsvector, text, bytea
  , unboundedArray, uuid, money
  , json, jsonb
  , smallserial, serial, bigserial
  , point, line, lineSegment, box
  ) where

import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.Actions (defaultActionProvider)
import qualified Database.Beam.Migrate.Backend as Tool
import qualified Database.Beam.Migrate.Checks as Db
import qualified Database.Beam.Migrate.SQL as Db
import           Database.Beam.Migrate.SQL.BeamExtensions
import qualified Database.Beam.Migrate.Serialization as Db
import qualified Database.Beam.Migrate.Types as Db
import qualified Database.Beam.Query.DataTypes as Db

import           Database.Beam.Postgres.Connection
import           Database.Beam.Postgres.CustomTypes
import           Database.Beam.Postgres.Extensions
import           Database.Beam.Postgres.PgSpecific
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import           Database.Beam.Haskell.Syntax

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg

import           Control.Applicative ((<|>))
import           Control.Arrow
import           Control.Exception (bracket)
import           Control.Monad

import           Data.Aeson hiding (json)
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.Maybe
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable
import           Data.UUID.Types (UUID)
import qualified Data.Vector as V
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#else
import           Data.Monoid (Endo(..))
#endif
import           Data.Word (Word64)

-- | Top-level migration backend for use by @beam-migrate@ tools
migrationBackend :: Tool.BeamMigrationBackend Postgres Pg
migrationBackend = Tool.BeamMigrationBackend
                        "postgres"
                        (unlines [ "For beam-postgres, this is a libpq connection string which can either be a list of key value pairs or a URI"
                                 , ""
                                 , "For example, 'host=localhost port=5432 dbname=mydb connect_timeout=10' or 'dbname=mydb'"
                                 , ""
                                 , "Or use URIs, for which the general form is:"
                                 , "  postgresql://[user[:password]@][netloc][:port][/dbname][?param1=value1&...]"
                                 , ""
                                 , "See <https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING> for more information" ])
                        (liftIOWithHandle getDbConstraints)
                        (Db.sql92Deserializers <> Db.sql99DataTypeDeserializers <>
                         Db.sql2008BigIntDataTypeDeserializers <>
                         postgresDataTypeDeserializers <>
                         Db.beamCheckDeserializers)
                        (BCL.unpack . (<> ";") . pgRenderSyntaxScript . fromPgCommand) "postgres.sql"
                        pgPredConverter (defaultActionProvider <> pgExtensionActionProvider <>
                                         pgCustomEnumActionProvider)
                        (\options action ->
                            bracket (Pg.connectPostgreSQL (fromString options)) Pg.close $ \conn ->
                              left show <$> withPgDebug (\_ -> pure ()) conn action)

-- | 'BeamDeserializers' for postgres-specific types:
--
--    * 'bytea'
--    * 'smallserial'
--    * 'serial'
--    * 'bigserial'
--    * 'tsvector'
--    * 'tsquery'
--    * 'text'
--    * 'json'
--    * 'jsonb'
--    * 'uuid'
--    * 'money'
--
postgresDataTypeDeserializers
  :: Db.BeamDeserializers Postgres
postgresDataTypeDeserializers =
  Db.beamDeserializer $ \_ v ->
  case v of
    "bytea"       -> pure pgByteaType
    "smallserial" -> pure pgSmallSerialType
    "serial"      -> pure pgSerialType
    "bigserial"   -> pure pgBigSerialType
    "tsquery"     -> pure pgTsQueryType
    "tsvector"    -> pure pgTsVectorType
    "text"        -> pure pgTextType
    "json"        -> pure pgJsonType
    "jsonb"       -> pure pgJsonbType
    "uuid"        -> pure pgUuidType
    "money"       -> pure pgMoneyType
    "point"       -> pure pgPointType
    "line"        -> pure pgLineType
    "lseg"        -> pure pgLineSegmentType
    "box"         -> pure pgBoxType
    _             -> fail "Postgres data type"

-- | Converts postgres 'DatabasePredicate's to 'DatabasePredicate's in the
-- Haskell syntax. Allows automatic generation of Haskell schemas from postgres
-- constraints.
pgPredConverter :: Tool.HaskellPredicateConverter
pgPredConverter = Tool.sql92HsPredicateConverters @Postgres pgTypeToHs <>
                  Tool.hsPredicateConverter pgHasColumnConstraint
  where
    pgHasColumnConstraint (Db.TableColumnHasConstraint tblNm colNm c :: Db.TableColumnHasConstraint Postgres)
      | c == Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing =
          Just (Db.SomeDatabasePredicate (Db.TableColumnHasConstraint tblNm colNm (Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing) :: Db.TableColumnHasConstraint HsMigrateBackend))
      | otherwise = Nothing

-- | Turn a 'PgDataTypeSyntax' into the corresponding 'HsDataType'. This is a
-- best effort guess, and may fail on more exotic types. Feel free to send PRs
-- to make this function more robust!
pgTypeToHs :: PgDataTypeSyntax -> Maybe HsDataType
pgTypeToHs (PgDataTypeSyntax tyDescr _ _) =
  case tyDescr of
    PgDataTypeDescrOid oid width
      | Pg.typoid Pg.int2    == oid -> Just smallIntType
      | Pg.typoid Pg.int4    == oid -> Just intType
      | Pg.typoid Pg.int8    == oid -> Just bigIntType

      | Pg.typoid Pg.bpchar  == oid -> Just (charType (fromIntegral <$> width) Nothing)
      | Pg.typoid Pg.varchar == oid -> Just (varCharType (fromIntegral <$> width) Nothing)
      | Pg.typoid Pg.bit     == oid -> Just (bitType (fromIntegral <$> width))
      | Pg.typoid Pg.varbit  == oid -> Just (varBitType (fromIntegral <$> width))

      | Pg.typoid Pg.numeric == oid ->
          let decimals = fromMaybe 0 width .&. 0xFFFF
              prec     = (fromMaybe 0 width `shiftR` 16) .&. 0xFFFF
          in Just (numericType (Just (fromIntegral prec, Just (fromIntegral decimals))))

      | Pg.typoid Pg.float4  == oid -> Just (floatType (fromIntegral <$> width))
      | Pg.typoid Pg.float8  == oid -> Just doubleType

      | Pg.typoid Pg.date    == oid -> Just dateType

      -- We prefer using the standard beam names
      | Pg.typoid Pg.text    == oid -> Just characterLargeObjectType
      | Pg.typoid Pg.bytea   == oid -> Just binaryLargeObjectType
      | Pg.typoid Pg.bool    == oid -> Just booleanType

      -- TODO timestamp prec
      | Pg.typoid Pg.time        == oid -> Just (timeType Nothing False)
      | Pg.typoid Pg.timestamp   == oid -> Just (timestampType Nothing False)
      | Pg.typoid Pg.timestamptz == oid -> Just (timestampType Nothing True)

      -- Postgres specific datatypes, haskell versions
      | Pg.typoid Pg.uuid        == oid ->
          Just $ HsDataType (hsVarFrom "uuid" "Database.Beam.Postgres")
                            (HsType (tyConNamed "UUID")
                                    (importSome "Data.UUID.Types" [importTyNamed "UUID"]))
                            (pgDataTypeSerialized pgUuidType)
      | Pg.typoid Pg.money       == oid ->
          Just $ HsDataType (hsVarFrom "money" "Database.Beam.Postgres")
                            (HsType (tyConNamed "PgMoney")
                                    (importSome "Database.Beam.Postgres" [importTyNamed "PgMoney"]))
                            (pgDataTypeSerialized pgMoneyType)
      | Pg.typoid Pg.json        == oid ->
          Just $ HsDataType (hsVarFrom "json" "Database.Beam.Postgres")
                            (HsType (tyApp (tyConNamed "PgJSON") [ tyConNamed "Value" ])
                                    (importSome "Data.Aeson" [importTyNamed "Value"] <>
                                     importSome "Database.Beam.Postgres" [importTyNamed "PgJSON"]))
                            (pgDataTypeSerialized pgJsonType)
      | Pg.typoid Pg.jsonb       == oid ->
          Just $ HsDataType (hsVarFrom "jsonb" "Database.Beam.Postgres")
                            (HsType (tyApp (tyConNamed "PgJSONB") [ tyConNamed "Value" ])
                                    (importSome "Data.Aeson" [importTyNamed "Value"] <>
                                     importSome "Database.Beam.Postgres" [importTyNamed "PgJSONB"]))
                            (pgDataTypeSerialized pgJsonType)
      | Pg.typoid pgTsVectorTypeInfo == oid ->
          Just $ HsDataType (hsVarFrom "tsvector" "Database.Beam.Postgres")
                            (HsType (tyConNamed "TsVector")
                                    (importSome "Database.Beam.Postgres" [importTyNamed "TsVector"]))
                            (pgDataTypeSerialized pgTsVectorType)
      | Pg.typoid pgTsQueryTypeInfo == oid ->
          Just $ HsDataType (hsVarFrom "tsquery" "Database.Beam.Postgres")
                            (HsType (tyConNamed "TsQuery")
                                    (importSome "Database.Beam.Postgres" [importTyNamed "TsQuery"]))
                            (pgDataTypeSerialized pgTsQueryType)

      | Pg.typoid Pg.point   == oid ->
          Just $ HsDataType (hsVarFrom "point" "Database.Beam.Postgres")
                            (HsType (tyConNamed "PgPoint")
                                    (importSome "Database.Beam.Postgres" [ importTyNamed "PgPoint" ]))
                            (pgDataTypeSerialized pgPointType)
      | Pg.typoid Pg.line    == oid ->
          Just $ HsDataType (hsVarFrom "line" "Database.Beam.Postgres")
                            (HsType (tyConNamed "PgLine")
                                    (importSome "Database.Beam.Postgres" [ importTyNamed "PgLine" ]))
                            (pgDataTypeSerialized pgLineType)
      | Pg.typoid Pg.lseg    == oid ->
          Just $ HsDataType (hsVarFrom "lineSegment" "Database.Beam.Postgres")
                            (HsType (tyConNamed "PgLineSegment")
                                    (importSome "Database.Beam.Postgres" [ importTyNamed "PgLineSegment" ]))
                            (pgDataTypeSerialized pgLineSegmentType)
      | Pg.typoid Pg.box     == oid ->
          Just $ HsDataType (hsVarFrom "box" "Database.Beam.Postgres")
                            (HsType (tyConNamed "PgBox")
                                    (importSome "Database.Beam.Postgres" [ importTyNamed "PgBox" ]))
                            (pgDataTypeSerialized pgBoxType)

    _ -> Just (hsErrorType ("PG type " ++ show tyDescr))

-- | Turn a series of 'Db.MigrationSteps' into a line-by-line array of
-- 'BL.ByteString's suitable for writing to a script.
migrateScript :: Db.MigrationSteps Postgres () a' -> [BL.ByteString]
migrateScript steps =
  "-- CAUTION: beam-postgres currently escapes postgres string literals somewhat\n"                 :
  "--          haphazardly when generating scripts (but not when generating commands)\n"            :
  "--          This is due to technical limitations in libPq that require a Postgres\n"             :
  "--          Connection in order to correctly escape strings. Please verify that the\n"           :
  "--          generated migration script is correct before running it on your database.\n"         :
  "--          If you feel so called, please contribute better escaping support to beam-postgres\n" :
  "\n"                                                                                              :
  "-- Set connection encoding to UTF-8\n"                                                           :
  "SET client_encoding = 'UTF8';\n"                                                                 :
  "SET standard_conforming_strings = off;\n\n"                                                      :
  appEndo (Db.migrateScript renderHeader renderCommand steps) []
  where
    renderHeader nm =
      Endo (("-- " <> BL.fromStrict (TE.encodeUtf8 nm) <> "\n"):)
    renderCommand command =
      Endo ((pgRenderSyntaxScript (fromPgCommand command) <> ";\n"):)

-- | Write the migration given by the 'Db.MigrationSteps' to a file.
writeMigrationScript :: FilePath -> Db.MigrationSteps Postgres () a -> IO ()
writeMigrationScript fp steps =
  let stepBs = migrateScript steps
  in BL.writeFile fp (BL.concat stepBs)

pgExpandDataType :: Db.DataType Postgres a -> PgDataTypeSyntax
pgExpandDataType (Db.DataType pg) = pg

pgCharLength :: Maybe Int32 -> Maybe Word
pgCharLength Nothing = Nothing
pgCharLength (Just (-1)) = Nothing
pgCharLength (Just x) = Just (fromIntegral x)

pgDataTypeFromAtt :: ByteString -> Pg.Oid -> Maybe Int32 -> Maybe PgDataTypeSyntax
pgDataTypeFromAtt _ oid pgMod
  | Pg.typoid Pg.bool == oid        = Just $ pgExpandDataType Db.boolean
  | Pg.typoid Pg.bytea == oid       = Just $ pgExpandDataType Db.binaryLargeObject
  | Pg.typoid Pg.char == oid        = Just $ pgExpandDataType (Db.char (pgCharLength pgMod))
  -- TODO Pg.name
  | Pg.typoid Pg.int8 == oid        = Just $ pgExpandDataType (Db.bigint :: Db.DataType Postgres Int64)
  | Pg.typoid Pg.int4 == oid        = Just $ pgExpandDataType (Db.int :: Db.DataType Postgres Int32)
  | Pg.typoid Pg.int2 == oid        = Just $ pgExpandDataType (Db.smallint :: Db.DataType Postgres Int16)
  | Pg.typoid Pg.varchar == oid     = Just $ pgExpandDataType (Db.varchar (pgCharLength pgMod))
  | Pg.typoid Pg.timestamp == oid   = Just $ pgExpandDataType Db.timestamp
  | Pg.typoid Pg.timestamptz == oid = Just $ pgExpandDataType Db.timestamptz
  | Pg.typoid Pg.float8 == oid      = Just $ pgExpandDataType Db.double
  | Pg.typoid Pg.text  == oid       = Just $ pgTextType
  | Pg.typoid Pg.json  == oid       = Just $ pgJsonType
  | Pg.typoid Pg.jsonb == oid       = Just $ pgJsonbType
  | Pg.typoid Pg.uuid  == oid       = Just $ pgUuidType
  | Pg.typoid Pg.point == oid       = Just $ pgPointType
  | Pg.typoid Pg.line  == oid       = Just $ pgLineType
  | Pg.typoid Pg.lseg  == oid       = Just $ pgLineSegmentType
  | Pg.typoid Pg.box   == oid       = Just $ pgBoxType
  | Pg.typoid Pg.numeric == oid =
      let precAndDecimal =
            case pgMod of
              Nothing -> Nothing
              Just pgMod' ->
                let prec = fromIntegral (pgMod' `shiftR` 16)
                    dec = fromIntegral (pgMod' .&. 0xFFFF)
                in Just (prec, if dec == 0 then Nothing else Just dec)
      in Just $ pgExpandDataType (Db.numeric precAndDecimal)
  | otherwise = Nothing

pgEnumerationTypeFromAtt :: [ (T.Text, Pg.Oid, V.Vector T.Text) ] -> ByteString -> Pg.Oid -> Maybe Int32 -> Maybe PgDataTypeSyntax
pgEnumerationTypeFromAtt enumData =
  let enumDataMap = HM.fromList [ (fromIntegral oid' :: Word64, -- Get around lack of Hashable for CUInt
                                   PgDataTypeSyntax (PgDataTypeDescrDomain nm) (emit (TE.encodeUtf8 nm))
                                          (pgDataTypeJSON (object [ "customType" .= nm ]))) | (nm, (Pg.Oid oid'), _) <- enumData ]
  in \_ (Pg.Oid oid) _ -> HM.lookup (fromIntegral oid) enumDataMap

pgUnknownDataType :: Pg.Oid -> Maybe Int32 -> PgDataTypeSyntax
pgUnknownDataType oid@(Pg.Oid oid') pgMod =
  PgDataTypeSyntax (PgDataTypeDescrOid oid pgMod) (emit "{- UNKNOWN -}")
                   (pgDataTypeJSON (object [ "oid" .= (fromIntegral oid' :: Word), "mod" .= pgMod ]))

-- * Create constraints from a connection

getDbConstraints :: Pg.Connection -> IO [ Db.SomeDatabasePredicate ]
getDbConstraints = getDbConstraintsForSchemas Nothing

getDbConstraintsForSchemas :: Maybe [String] -> Pg.Connection -> IO [ Db.SomeDatabasePredicate ]
getDbConstraintsForSchemas subschemas conn =
  do tbls <- case subschemas of
        Nothing -> Pg.query_ conn "SELECT cl.oid, relname FROM pg_catalog.pg_class \"cl\" join pg_catalog.pg_namespace \"ns\" on (ns.oid = relnamespace) where nspname = any (current_schemas(false)) and relkind='r'"
        Just ss -> Pg.query  conn "SELECT cl.oid, relname FROM pg_catalog.pg_class \"cl\" join pg_catalog.pg_namespace \"ns\" on (ns.oid = relnamespace) where nspname IN ? and relkind='r'" (Pg.Only (Pg.In ss))
     let tblsExist = map (\(_, tbl) -> Db.SomeDatabasePredicate (Db.TableExistsPredicate (Db.QualifiedName Nothing tbl))) tbls

     enumerationData <-
       Pg.query_ conn
         (fromString (unlines
                      [ "SELECT t.typname, t.oid, array_agg(e.enumlabel ORDER BY e.enumsortorder)"
                      , "FROM pg_enum e JOIN pg_type t ON t.oid = e.enumtypid"
                      , "GROUP BY t.typname, t.oid" ]))

     columnChecks <-
       fmap mconcat . forM tbls $ \(oid, tbl) ->
       do columns <- Pg.query conn "SELECT attname, atttypid, atttypmod, attnotnull, pg_catalog.format_type(atttypid, atttypmod) FROM pg_catalog.pg_attribute att WHERE att.attrelid=? AND att.attnum>0 AND att.attisdropped='f'"
                       (Pg.Only (oid :: Pg.Oid))
          let columnChecks = map (\(nm, typId :: Pg.Oid, typmod, _, typ :: ByteString) ->
                                    let typmod' = if typmod == -1 then Nothing else Just (typmod - 4)

                                        pgDataType = fromMaybe (pgUnknownDataType typId typmod') $
                                                     pgDataTypeFromAtt typ typId typmod' <|>
                                                     pgEnumerationTypeFromAtt enumerationData typ typId typmod'

                                    in Db.SomeDatabasePredicate (Db.TableHasColumn (Db.QualifiedName Nothing tbl) nm pgDataType :: Db.TableHasColumn Postgres)) columns
              notNullChecks = concatMap (\(nm, _, _, isNotNull, _) ->
                                           if isNotNull then
                                            [Db.SomeDatabasePredicate (Db.TableColumnHasConstraint (Db.QualifiedName Nothing tbl) nm (Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing)
                                              :: Db.TableColumnHasConstraint Postgres)]
                                           else [] ) columns

          pure (columnChecks ++ notNullChecks)

     primaryKeys <-
       map (\(relnm, cols) -> Db.SomeDatabasePredicate (Db.TableHasPrimaryKey (Db.QualifiedName Nothing relnm) (V.toList cols))) <$>
       Pg.query_ conn (fromString (unlines [ "SELECT c.relname, array_agg(a.attname ORDER BY k.n ASC)"
                                           , "FROM pg_index i"
                                           , "CROSS JOIN unnest(i.indkey) WITH ORDINALITY k(attid, n)"
                                           , "JOIN pg_attribute a ON a.attnum=k.attid AND a.attrelid=i.indrelid"
                                           , "JOIN pg_class c ON c.oid=i.indrelid"
                                           , "JOIN pg_namespace ns ON ns.oid=c.relnamespace"
                                           , "WHERE ns.nspname = any (current_schemas(false)) AND c.relkind='r' AND i.indisprimary GROUP BY relname, i.indrelid" ]))

     let enumerations =
           map (\(enumNm, _, options) -> Db.SomeDatabasePredicate (PgHasEnum enumNm (V.toList options))) enumerationData

     extensions <-
       map (\(Pg.Only extname) -> Db.SomeDatabasePredicate (PgHasExtension extname)) <$>
       Pg.query_ conn "SELECT extname from pg_extension"

     pure (tblsExist ++ columnChecks ++ primaryKeys ++ enumerations ++ extensions)

-- * Postgres-specific data types

-- | 'Db.DataType' for @tsquery@. See 'TsQuery' for more information
tsquery :: Db.DataType Postgres TsQuery
tsquery = Db.DataType pgTsQueryType

-- | 'Db.DataType' for @tsvector@. See 'TsVector' for more information
tsvector :: Db.DataType Postgres TsVector
tsvector = Db.DataType pgTsVectorType

-- | 'Db.DataType' for Postgres @TEXT@. 'characterLargeObject' is also mapped to
-- this data type
text :: Db.DataType Postgres T.Text
text = Db.DataType pgTextType

-- | 'Db.DataType' for Postgres @BYTEA@. 'binaryLargeObject' is also mapped to
-- this data type
bytea :: Db.DataType Postgres ByteString
bytea = Db.DataType pgByteaType

-- | 'Db.DataType' for a Postgres array without any bounds.
--
-- Note that array support in @beam-migrate@ is still incomplete.
unboundedArray :: forall a. Typeable a
               => Db.DataType Postgres a
               -> Db.DataType Postgres (V.Vector a)
unboundedArray (Db.DataType elTy) =
  Db.DataType (pgUnboundedArrayType elTy)

-- | 'Db.DataType' for @JSON@. See 'PgJSON' for more information
json :: (ToJSON a, FromJSON a) => Db.DataType Postgres (PgJSON a)
json = Db.DataType pgJsonType

-- | 'Db.DataType' for @JSONB@. See 'PgJSON' for more information
jsonb :: (ToJSON a, FromJSON a) => Db.DataType Postgres (PgJSONB a)
jsonb = Db.DataType pgJsonbType

-- | 'Db.DataType' for @UUID@ columns. The 'pgCryptoGenRandomUUID' function in
-- the 'PgCrypto' extension can be used to generate UUIDs at random.
uuid :: Db.DataType Postgres UUID
uuid = Db.DataType pgUuidType

-- | 'Db.DataType' for @MONEY@ columns.
money :: Db.DataType Postgres PgMoney
money = Db.DataType pgMoneyType

point :: Db.DataType Postgres PgPoint
point = Db.DataType pgPointType

line :: Db.DataType Postgres PgLine
line = Db.DataType pgLineType

lineSegment :: Db.DataType Postgres PgLineSegment
lineSegment = Db.DataType pgLineSegmentType

box :: Db.DataType Postgres PgBox
box = Db.DataType pgBoxType

-- * Pseudo-data types

-- | Postgres @SERIAL@ data types. Automatically generates an appropriate
-- @DEFAULT@ clause and sequence
smallserial, serial, bigserial :: Integral a => Db.DataType Postgres (SqlSerial a)
smallserial = Db.DataType pgSmallSerialType
serial = Db.DataType pgSerialType
bigserial = Db.DataType pgBigSerialType

data PgHasDefault = PgHasDefault
instance Db.FieldReturnType 'True 'False Postgres resTy a =>
         Db.FieldReturnType 'False 'False Postgres resTy (PgHasDefault -> a) where
  field' _ _ nm ty _ collation constraints PgHasDefault =
    Db.field' (Proxy @'True) (Proxy @'False) nm ty Nothing collation constraints

instance BeamSqlBackendHasSerial Postgres where
  genericSerial nm = Db.field nm serial PgHasDefault
