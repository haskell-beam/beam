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
  ( migrationBackend
  , postgresDataTypeDeserializers
  , pgPredConverter
  , getDbConstraints
  , pgTypeToHs
  , migrateScript
  , writeMigrationScript

    -- * Postgres data types
  , tsquery, tsvector, text, bytea
  , unboundedArray, uuid, money
  , json, jsonb
  , smallserial, serial, bigserial
  ) where

import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.Actions (defaultActionProvider)
import qualified Database.Beam.Migrate.Backend as Tool
import qualified Database.Beam.Migrate.Checks as Db
import           Database.Beam.Migrate.SQL.BeamExtensions
import qualified Database.Beam.Migrate.SQL as Db
import qualified Database.Beam.Migrate.Serialization as Db
import qualified Database.Beam.Migrate.Types as Db

import           Database.Beam.Postgres.Connection
import           Database.Beam.Postgres.PgSpecific
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types
import           Database.Beam.Postgres.Extensions

import           Database.Beam.Haskell.Syntax

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg

import           Control.Arrow
import           Control.Exception (bracket)
import           Control.Monad
import           Control.Monad.Free.Church (liftF)

import           Data.Aeson hiding (json)
import           Data.Bits
import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.String
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.UUID.Types (UUID)
import           Data.Typeable
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

-- | Top-level migration backend for use by @beam-migrate@ tools
migrationBackend :: Tool.BeamMigrationBackend PgCommandSyntax Postgres Pg.Connection Pg
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
                        (BL.concat . migrateScript)
                        (liftF (PgLiftWithHandle getDbConstraints id))
                        (Db.sql92Deserializers <> Db.sql99DataTypeDeserializers <>
                         Db.sql2008BigIntDataTypeDeserializers <>
                         postgresDataTypeDeserializers <>
                         Db.beamCheckDeserializers)
                        (BCL.unpack . (<> ";") . pgRenderSyntaxScript . fromPgCommand) "postgres.sql"
                        pgPredConverter (defaultActionProvider <> pgExtensionActionProvider)
                        (\options action ->
                            bracket (Pg.connectPostgreSQL (fromString options)) Pg.close $ \conn ->
                              left pgToToolError <$> withPgDebug (\_ -> pure ()) conn action)
  where
    pgToToolError (PgRowParseError err) = show err
    pgToToolError (PgInternalError err) = err

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
  :: Db.BeamDeserializers PgCommandSyntax
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
    _             -> fail "Postgres data type"

-- | Converts postgres 'DatabasePredicate's to 'DatabasePredicate's in the
-- Haskell syntax. Allows automatic generation of Haskell schemas from postgres
-- constraints.
pgPredConverter :: Tool.HaskellPredicateConverter
pgPredConverter = Tool.sql92HsPredicateConverters @PgColumnSchemaSyntax pgTypeToHs <>
                  Tool.hsPredicateConverter pgHasColumnConstraint
  where
    pgHasColumnConstraint (Db.TableColumnHasConstraint tblNm colNm c :: Db.TableColumnHasConstraint PgColumnSchemaSyntax)
      | c == Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing =
          Just (Db.SomeDatabasePredicate (Db.TableColumnHasConstraint tblNm colNm (Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing) :: Db.TableColumnHasConstraint HsColumnSchema))
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

    _ -> Just (hsErrorType ("PG type " ++ show tyDescr))

-- | Turn a series of 'Db.MigrationSteps' into a line-by-line array of
-- 'BL.ByteString's suitable for writing to a script.
migrateScript :: Db.MigrationSteps PgCommandSyntax () a' -> [BL.ByteString]
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
writeMigrationScript :: FilePath -> Db.MigrationSteps PgCommandSyntax () a -> IO ()
writeMigrationScript fp steps =
  let stepBs = migrateScript steps
  in BL.writeFile fp (BL.concat stepBs)

pgExpandDataType :: Db.DataType PgDataTypeSyntax a -> PgDataTypeSyntax
pgExpandDataType (Db.DataType pg) = pg

pgDataTypeFromAtt :: ByteString -> Pg.Oid -> Maybe Int32 -> PgDataTypeSyntax
pgDataTypeFromAtt _ oid pgMod
  | Pg.typoid Pg.bool == oid = pgExpandDataType Db.boolean
  | Pg.typoid Pg.bytea == oid = pgExpandDataType Db.binaryLargeObject
  | Pg.typoid Pg.char == oid = pgExpandDataType (Db.char Nothing) -- TODO length
  -- TODO Pg.name
  | Pg.typoid Pg.int8 == oid = pgExpandDataType (Db.bigint :: Db.DataType PgDataTypeSyntax Int64)
  | Pg.typoid Pg.int4 == oid = pgExpandDataType (Db.int :: Db.DataType PgDataTypeSyntax Int32)
  | Pg.typoid Pg.int2 == oid = pgExpandDataType (Db.smallint :: Db.DataType PgDataTypeSyntax Int16)
  | Pg.typoid Pg.varchar == oid = pgExpandDataType (Db.varchar Nothing)
  | Pg.typoid Pg.timestamp == oid = pgExpandDataType Db.timestamp
  | Pg.typoid Pg.numeric == oid =
      let precAndDecimal =
            case pgMod of
              Nothing -> Nothing
              Just pgMod' ->
                let prec = fromIntegral (pgMod' `shiftR` 16)
                    dec = fromIntegral (pgMod' .&. 0xFFFF)
                in Just (prec, if dec == 0 then Nothing else Just dec)
      in pgExpandDataType (Db.numeric precAndDecimal)
  | otherwise = PgDataTypeSyntax (PgDataTypeDescrOid oid pgMod) (emit "{- UNKNOWN -}")
                                 (pgDataTypeJSON (object [ "oid" .= (fromIntegral oid' :: Word), "mod" .= pgMod ]))
  where
    Pg.Oid oid' = oid
  -- , \_ _ -> errorTypeHs (show fallback))

-- * Create constraints from a connection

getDbConstraints :: Pg.Connection -> IO [ Db.SomeDatabasePredicate ]
getDbConstraints conn =
  do tbls <- Pg.query_ conn "SELECT oid, relname FROM pg_catalog.pg_class where relnamespace=(select oid from pg_catalog.pg_namespace where nspname='public') and relkind='r'"
     let tblsExist = map (\(_, tbl) -> Db.SomeDatabasePredicate (Db.TableExistsPredicate tbl)) tbls

     columnChecks <-
       fmap mconcat . forM tbls $ \(oid, tbl) ->
       do columns <- Pg.query conn "SELECT attname, atttypid, atttypmod, attnotnull, pg_catalog.format_type(atttypid, atttypmod) FROM pg_catalog.pg_attribute att WHERE att.attrelid=? AND att.attnum>0 AND att.attisdropped='f'"
                       (Pg.Only (oid :: Pg.Oid))
          let columnChecks = map (\(nm, typId :: Pg.Oid, typmod, _, typ :: ByteString) ->
                                    let typmod' = if typmod == -1 then Nothing else Just (typmod - 4)
                                        pgDataType = pgDataTypeFromAtt typ typId typmod'

                                    in Db.SomeDatabasePredicate (Db.TableHasColumn tbl nm pgDataType :: Db.TableHasColumn PgColumnSchemaSyntax)) columns
              notNullChecks = concatMap (\(nm, _, _, isNotNull, _) ->
                                           if isNotNull then
                                            [Db.SomeDatabasePredicate (Db.TableColumnHasConstraint tbl nm (Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing)
                                              :: Db.TableColumnHasConstraint PgColumnSchemaSyntax)]
                                           else [] ) columns

          pure (columnChecks ++ notNullChecks)

     primaryKeys <-
       map (\(relnm, cols) -> Db.SomeDatabasePredicate (Db.TableHasPrimaryKey relnm (V.toList cols))) <$>
       Pg.query_ conn (fromString (unlines [ "SELECT c.relname, array_agg(a.attname ORDER BY k.n ASC)"
                                           , "FROM pg_index i"
                                           , "CROSS JOIN unnest(i.indkey) WITH ORDINALITY k(attid, n)"
                                           , "JOIN pg_attribute a ON a.attnum=k.attid AND a.attrelid=i.indrelid"
                                           , "JOIN pg_class c ON c.oid=i.indrelid"
                                           , "WHERE c.relkind='r' AND i.indisprimary GROUP BY relname, i.indrelid" ]))

     pure (tblsExist ++ columnChecks ++ primaryKeys)

-- * Postgres-specific data types

-- | 'Db.DataType' for @tsquery@. See 'TsQuery' for more information
tsquery :: Db.DataType PgDataTypeSyntax TsQuery
tsquery = Db.DataType pgTsQueryType

-- | 'Db.DataType' for @tsvector@. See 'TsVector' for more information
tsvector :: Db.DataType PgDataTypeSyntax TsVector
tsvector = Db.DataType pgTsVectorType

-- | 'Db.DataType' for Postgres @TEXT@. 'characterLargeObject' is also mapped to
-- this data type
text :: Db.DataType PgDataTypeSyntax T.Text
text = Db.DataType pgTextType

-- | 'Db.DataType' for Postgres @BYTEA@. 'binaryLargeObject' is also mapped to
-- this data type
bytea :: Db.DataType PgDataTypeSyntax ByteString
bytea = Db.DataType pgByteaType

-- | 'Db.DataType' for a Postgres array without any bounds.
--
-- Note that array support in @beam-migrate@ is still incomplete.
unboundedArray :: forall a. Typeable a
               => Db.DataType PgDataTypeSyntax a
               -> Db.DataType PgDataTypeSyntax (V.Vector a)
unboundedArray (Db.DataType elTy) =
  Db.DataType (pgUnboundedArrayType elTy)

-- | 'Db.DataType' for @JSON@. See 'PgJSON' for more information
json :: (ToJSON a, FromJSON a) => Db.DataType PgDataTypeSyntax (PgJSON a)
json = Db.DataType pgJsonType

-- | 'Db.DataType' for @JSONB@. See 'PgJSON' for more information
jsonb :: (ToJSON a, FromJSON a) => Db.DataType PgDataTypeSyntax (PgJSONB a)
jsonb = Db.DataType pgJsonbType

-- | 'Db.DataType' for @UUID@ columns. The 'pgCryptoGenRandomUUID' function in
-- the 'PgCrypto' extension can be used to generate UUIDs at random.
uuid :: Db.DataType PgDataTypeSyntax UUID
uuid = Db.DataType pgUuidType

-- | 'Db.DataType' for @MONEY@ columns.
money :: Db.DataType PgDataTypeSyntax PgMoney
money = Db.DataType pgMoneyType

-- * Pseudo-data types

-- | Postgres @SERIAL@ data types. Automatically generates an appropriate
-- @DEFAULT@ clause and sequence
smallserial, serial, bigserial :: Integral a => Db.DataType PgDataTypeSyntax (SqlSerial a)
smallserial = Db.DataType pgSmallSerialType
serial = Db.DataType pgSerialType
bigserial = Db.DataType pgBigSerialType

data PgHasDefault = PgHasDefault
instance Db.FieldReturnType 'True 'False PgColumnSchemaSyntax resTy a =>
         Db.FieldReturnType 'False 'False PgColumnSchemaSyntax resTy (PgHasDefault -> a) where
  field' _ _ nm ty _ collation constraints PgHasDefault =
    Db.field' (Proxy @'True) (Proxy @'False) nm ty Nothing collation constraints

instance IsBeamSerialColumnSchemaSyntax PgColumnSchemaSyntax where
  genericSerial nm = Db.field nm serial PgHasDefault
