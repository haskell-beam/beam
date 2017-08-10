{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Database.Beam.Postgres.Migrate where

import qualified Database.Beam.Migrate.Checks as Db
import qualified Database.Beam.Migrate.SQL.Types as Db
import qualified Database.Beam.Migrate.SQL.SQL92 as Db
import qualified Database.Beam.Migrate.Types as Db

import           Database.Beam.Backend.SQL

import           Database.Beam.Migrate.SQL.BeamExtensions
import qualified Database.Beam.Migrate.Backend as Tool
import           Database.Beam.Migrate.Actions (defaultActionProviders)

import           Database.Beam.Postgres.Connection
import           Database.Beam.Postgres.PgSpecific
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import           Database.Beam.Haskell.Syntax

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg

import           Control.Arrow
import           Control.Exception (bracket)
import           Control.Monad

import           Data.Bits
import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Monoid
import           Data.String
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Typeable

--import qualified Language.Haskell.Exts.Syntax as Hs

migrationBackend :: Tool.BeamMigrationBackend Postgres PgCommandSyntax
migrationBackend = Tool.BeamMigrationBackend
                        "postgres" "sql"
                        (unlines [ "For beam-postgres, this is a libpq connection string which can either be a list of key value pairs or a URI"
                                 , ""
                                 , "For example, 'host=localhost port=5432 dbname=mydb connect_timeout=10' or 'dbname=mydb'"
                                 , ""
                                 , "Or use URIs, for which the general form is:"
                                 , "  postgresql://[user[:password]@][netloc][:port][/dbname][?param1=value1&...]"
                                 , ""
                                 , "See <https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING> for more information" ])
                        (BL.concat . migrateScript)
                        (\options -> Pg.connectPostgreSQL (fromString options) >>= getDbConstraints)
                        (BCL.unpack . pgRenderSyntaxScript . fromPgCommand) "sql"
                        pgPredConverter defaultActionProviders
                        (\options action ->
                            bracket (Pg.connectPostgreSQL (fromString options)) Pg.close $ \conn ->
                              left pgToToolError <$> withPgDebug (\_ -> pure ()) conn action)
  where
    pgToToolError (PgRowParseError err) = show err
    pgToToolError (PgInternalError err) = err

pgPredConverter :: Tool.HaskellPredicateConverter
pgPredConverter = Tool.easyHsPredicateConverter <>
                  Tool.hsPredicateConverter pgHasColumn <>
                  Tool.hsPredicateConverter pgHasColumnConstraint
  where
    pgHasColumn (Db.TableHasColumn tblNm colNm pgType :: Db.TableHasColumn PgColumnSchemaSyntax) =
      Db.SomeDatabasePredicate <$> (Db.TableHasColumn tblNm colNm <$> pgTypeToHs pgType :: Maybe (Db.TableHasColumn HsColumnSchema))

    pgHasColumnConstraint (Db.TableColumnHasConstraint tblNm colNm c :: Db.TableColumnHasConstraint PgColumnSchemaSyntax)
      | c == Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing =
          Just (Db.SomeDatabasePredicate (Db.TableColumnHasConstraint tblNm colNm (Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing) :: Db.TableColumnHasConstraint HsColumnSchema))
      | otherwise = Nothing
      

pgTypeToHs :: PgDataTypeSyntax -> Maybe HsDataType
pgTypeToHs (PgDataTypeSyntax tyDescr _) =
  case tyDescr of
    PgDataTypeDescrOid oid width
      | Pg.typoid Pg.int2    == oid -> Just smallIntType
      | Pg.typoid Pg.int4    == oid -> Just intType

      | Pg.typoid Pg.bpchar  == oid -> Just (charType (fromIntegral <$> width) Nothing)
      | Pg.typoid Pg.varchar == oid -> Just (varCharType (fromIntegral <$> width) Nothing)
      -- TODO timestamp prec
      | Pg.typoid Pg.timestamp == oid -> Just (timestampType Nothing False)
      | Pg.typoid Pg.timestamptz == oid -> Just (timestampType Nothing True)

      | Pg.typoid Pg.numeric == oid ->
          let decimals = fromMaybe 0 width .&. 0xFFFF
              prec     = (fromMaybe 0 width `shiftR` 16) .&. 0xFFFF
          in Just (numericType (Just (fromIntegral prec, Just (fromIntegral decimals))))
    _ -> Just (hsErrorType ("PG type " ++ show tyDescr))

migrateScript :: Db.MigrationSteps PgCommandSyntax () a' -> [BL.ByteString]
migrateScript steps =
  "-- CAUTION: beam-postgres currently escapes postgres string literals somewhat\n"                 :
  "--          haphazardly when generating scripts (but not when generating commands)\n"            :
  "--          This is due to technical limitations in libPq that require a Postgres\n"             :
  "--          Connection in order to correctly escape strings. Please verify that the\n"           :
  "--          Generated migration script is correct before running it on your database.\n"         :
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

writeMigrationScript :: FilePath -> Db.MigrationSteps PgCommandSyntax () a -> IO ()
writeMigrationScript fp steps =
  let stepBs = migrateScript steps
  in BL.writeFile fp (BL.concat stepBs)

pgExpandDataType :: Db.DataType PgDataTypeSyntax a -> PgDataTypeSyntax
pgExpandDataType (Db.DataType pg) = pg

pgDataTypeFromAtt :: ByteString -> Pg.Oid -> Maybe Int32 -> PgDataTypeSyntax
pgDataTypeFromAtt _ oid pgMod
  | Pg.typoid Pg.bool == oid = pgExpandDataType (boolean :: Db.DataType PgDataTypeSyntax Bool)
  | Pg.typoid Pg.bytea == oid = pgExpandDataType bytea
  | Pg.typoid Pg.char == oid = pgExpandDataType (Db.char Nothing) -- TODO length
  -- TODO Pg.name
  | Pg.typoid Pg.int8 == oid = pgExpandDataType (bigint :: Db.DataType PgDataTypeSyntax Int64)
  | Pg.typoid Pg.int4 == oid = pgExpandDataType (Db.int :: Db.DataType PgDataTypeSyntax Int32)
  | Pg.typoid Pg.int2 == oid = pgExpandDataType (Db.smallint :: Db.DataType PgDataTypeSyntax Int16)
  | Pg.typoid Pg.varchar == oid = pgExpandDataType (Db.varchar Nothing)
  | Pg.typoid Pg.timestamp == oid = pgExpandDataType Db.timestamp
  | otherwise = PgDataTypeSyntax (PgDataTypeDescrOid oid pgMod) (emit "{- UNKNOWN -}")
  -- , \_ _ -> errorTypeHs (show fallback))

-- * Create constraints from a connection

getDbConstraints :: Pg.Connection -> IO [ Db.SomeDatabasePredicate ]
getDbConstraints conn =
  do tbls <- Pg.query_ conn "SELECT oid, relname FROM pg_catalog.pg_class where relnamespace=(select oid from pg_catalog.pg_namespace where nspname='public') and relkind='r'"
     let tblsExist = map (\(_, tbl) -> Db.SomeDatabasePredicate (Db.TableExistsPredicate tbl)) tbls

     columnChecks <-
       fmap mconcat . forM tbls $ \(oid, tbl) ->
       do columns <- Pg.query conn "SELECT attname, atttypid, atttypmod, attnotnull, pg_catalog.format_type(atttypid, atttypmod) FROM pg_catalog.pg_attribute att WHERE att.attrelid=? AND att.attnum>0"
                       (Pg.Only (oid :: Pg.Oid))
          let columnChecks = map (\(nm, typId :: Pg.Oid, typmod, _, typ :: ByteString) ->
                                    let typmod' = if typmod == -1 then Nothing else Just (typmod - 4)
                                        pgDataType = pgDataTypeFromAtt typ typId typmod'
                                        -- (PgDataTypeSyntax (PgDataTypeDescrOid typId typmod') (emit typ)) (errorTypeHs (show typ))
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

-- * Data types

-- pgContext :: TyCon -> HaskellSyntaxContext
-- pgContext con =
--   HaskellSyntaxContext False []
--                        (HM.fromList [ ("be", postgresTy), ("syntax", postgresSyntaxTy) ])
--                        (HM.fromList [ (("Database.Beam.Postgres", Nothing, False), [])
--                                     , (("Database.Beam.Postgres.Migrate", Nothing, False), [])
--                                     , ((tyConModule con, Just (tyConModule con), True), [tyConName con]) ])
--                        mempty []
--   where postgresTy = Hs.TyCon () (Hs.UnQual () (Hs.Ident () "Postgres"))
--         postgresSyntaxTy = Hs.TyCon () (Hs.UnQual () (Hs.Ident () "PgSyntax"))

-- pgArrayTypeHs :: forall a. Typeable a => Proxy a -> (T.Text -> T.Text -> HaskellDataType) -> T.Text -> T.Text -> HaskellDataType
-- pgArrayTypeHs a mkHsTy tbl col =
--   let HaskellDataType (HaskellSyntax expCtxt exp) (HaskellSyntax tyCtxt ty) = mkHsTy tbl col

--       con = typeRepTyCon (typeRep (Proxy @(V.Vector a)))
--       ctxt' = pgContext con
--       expCtxt' = expCtxt <> ctxt'
--       tyCtxt' = tyCtxt <> ctxt'

--       typeExp = HaskellSyntax expCtxt' (Hs.App () (Hs.Var () (Hs.UnQual () (Hs.Ident () "array"))) exp)
--       typeType = HaskellSyntax tyCtxt' (Hs.TyApp () (Hs.TyCon () (Hs.Qual () (Hs.ModuleName () (tyConModule con)) (Hs.Ident () (tyConName con))))
--                                                     (Hs.TyCon () (Hs.UnQual () (Hs.Ident () (show (typeRep a))))))
--   in HaskellDataType typeExp typeType

-- pgStdTypeHs :: Typeable a => String -> Proxy a -> T.Text -> T.Text -> HaskellDataType
-- pgStdTypeHs typeFunc a tblNm colNm =
--   let con = typeRepTyCon (typeRep a)
--       typeExp = HaskellSyntax (pgContext con) (Hs.Var () (Hs.UnQual () (Hs.Ident () typeFunc)))
--       typeType = HaskellSyntax (pgContext con) (Hs.TyCon () (Hs.Qual () (Hs.ModuleName () (tyConModule con)) (Hs.Ident () (tyConName con))))
--   in HaskellDataType typeExp typeType

tsquery :: Db.DataType PgDataTypeSyntax TsQuery
tsquery = Db.DataType (PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid tsqueryType) Nothing) (emit "tsquery"))
--                      (pgStdTypeHs "tsquery" (Proxy @TsQuery))

tsvector :: Db.DataType PgDataTypeSyntax TsVector
tsvector = Db.DataType (PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid tsvectorType) Nothing) (emit "tsvector"))
--                       (pgStdTypeHs "tsvector" (Proxy @TsVector))

text :: Db.DataType PgDataTypeSyntax T.Text
text = Db.DataType (PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.text) Nothing) (emit "TEXT"))
--                   (pgStdTypeHs "text" (Proxy @T.Text))

boolean :: forall a. Typeable a => Db.DataType PgDataTypeSyntax a
boolean = Db.DataType pgBooleanType -- (pgStdTypeHs "bool" (Proxy @a))

bytea :: Db.DataType PgDataTypeSyntax ByteString
bytea = Db.DataType pgByteaType -- (pgStdTypeHs "bytea" (Proxy @ByteString))

bigint :: Db.DataType PgDataTypeSyntax Int64
bigint = Db.DataType pgBigIntType -- (pgStdTypeHs "bigint" (Proxy @Int64))

array :: forall a. Typeable a => Maybe Int -> Db.DataType PgDataTypeSyntax a
      -> Db.DataType PgDataTypeSyntax (V.Vector a)
array Nothing (Db.DataType (PgDataTypeSyntax _ syntax)) =
  Db.DataType (PgDataTypeSyntax (error "Can't do array migrations yet") (syntax <> emit "[]"))
              -- (pgArrayTypeHs (Proxy @a) hsTy)
array (Just sz) (Db.DataType (PgDataTypeSyntax _ syntax)) =
  Db.DataType (PgDataTypeSyntax (error "can't do array migrations yet") (syntax <> emit "[" <> emit (fromString (show sz)) <> emit "]"))
              -- (pgArrayTypeHs (Proxy @a) hsTy)

-- * Pseudo-data types

smallserial, serial, bigserial :: Integral a => Db.DataType PgDataTypeSyntax (SqlSerial a)
smallserial = Db.DataType pgSmallSerialType
serial = Db.DataType pgSerialType
bigserial = Db.DataType pgBigSerialType

instance IsBeamSerialColumnSchemaSyntax PgColumnSchemaSyntax where
  genericSerial nm = Db.field nm serial
