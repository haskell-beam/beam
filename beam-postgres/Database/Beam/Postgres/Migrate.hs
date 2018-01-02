{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Database.Beam.Postgres.Migrate where

import           Database.Beam.Backend.Types
import qualified Database.Beam.Migrate.Checks as Db
import qualified Database.Beam.Migrate.SQL.Types as Db
import           Database.Beam.Migrate.SQL.BeamExtensions
import qualified Database.Beam.Migrate.SQL.SQL92 as Db
import qualified Database.Beam.Migrate.Tool as Tool
import qualified Database.Beam.Migrate.Types as Db

import           Database.Beam.Postgres.Connection
import           Database.Beam.Postgres.PgSpecific
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg

import           Control.Arrow
import           Control.Exception (bracket)
import           Control.Monad

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word16)

import           Options.Applicative hiding (action, command, columns)

data PgMigrateOpts
  = PgMigrateOpts
  { pgMigrateHost :: String
  , pgMigrateDatabase :: String
  , pgMigratePort :: Word16
  , pgMigrateUser :: String
  , pgMigratePromptPassword :: Bool
  } deriving Show

parsePgMigrateOpts :: Parser PgMigrateOpts
parsePgMigrateOpts =
  PgMigrateOpts <$> strOption (long "host" <> metavar "HOST" <> help "Postgres host to connect to" <> value "localhost")
                <*> strOption (long "database" <> metavar "DATABASE" <> help "Name of postgres database")
                <*> option auto (long "port" <> metavar "PORT" <> help "Port number to connect to " <> value 5432)
                <*> strOption (long "user" <> metavar "USER" <> help "Name of postgres user")
                <*> pure True

beConnectInfo :: PgMigrateOpts -> Pg.ConnectInfo
beConnectInfo opts = Pg.defaultConnectInfo { connectHost = pgMigrateHost opts
                                           , connectPort = pgMigratePort opts
                                           , connectUser = pgMigrateUser opts
                                           , connectDatabase = pgMigrateDatabase opts }

migrationBackend :: Tool.BeamMigrationBackend Postgres PgCommandSyntax PgMigrateOpts
migrationBackend = Tool.BeamMigrationBackend parsePgMigrateOpts
                        (BL.concat . migrateScript)
                        (\options -> Pg.connect (beConnectInfo options) >>= getDbConstraints)
                        (BCL.unpack . pgRenderSyntaxScript . fromPgCommand)
                        (\beOptions action ->
                            bracket (Pg.connect (beConnectInfo beOptions)) Pg.close $ \conn ->
                              left pgToToolError <$> withPgDebug (\_ -> pure ()) conn action)
  where
    pgToToolError (PgRowParseError err) = Tool.DdlCustomError (show err)
    pgToToolError (PgInternalError err) = Tool.DdlCustomError err

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
                                    in Db.SomeDatabasePredicate (Db.TableHasColumn tbl nm (PgDataTypeSyntax (PgDataTypeDescrOid typId typmod') (emit typ)) :: Db.TableHasColumn PgColumnSchemaSyntax)) columns
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

tsquery :: Db.DataType PgDataTypeSyntax TsQuery
tsquery = Db.DataType pgTsQueryType

tsvector :: Db.DataType PgDataTypeSyntax TsVector
tsvector = Db.DataType pgTsVectorType

text :: Db.DataType PgDataTypeSyntax T.Text
text = Db.DataType pgTextType

bytea :: Db.DataType PgDataTypeSyntax ByteString
bytea = Db.DataType pgByteaType

array :: Maybe Int -> Db.DataType PgDataTypeSyntax a
      -> Db.DataType PgDataTypeSyntax (V.Vector a)
array Nothing (Db.DataType (PgDataTypeSyntax _ syntax)) = Db.DataType (PgDataTypeSyntax (error "Can't do array migrations yet") (syntax <> emit "[]"))
array (Just sz) (Db.DataType (PgDataTypeSyntax _ syntax)) = Db.DataType (PgDataTypeSyntax (error "Can't do array migrations yet") (syntax <> emit "[" <> emit (fromString (show sz)) <> emit "]"))

json :: Db.DataType PgDataTypeSyntax (PgJSON a)
json = Db.DataType pgJsonType

jsonb :: Db.DataType PgDataTypeSyntax (PgJSONB a)
jsonb = Db.DataType pgJsonBType

-- * Pseudo-data types

smallserial, serial, bigserial :: Integral a => Db.DataType PgDataTypeSyntax (Auto a)
smallserial = Db.DataType pgSmallSerialType
serial = Db.DataType pgSerialType
bigserial = Db.DataType pgBigSerialType

instance IsBeamSerialColumnSchemaSyntax PgColumnSchemaSyntax where
  genericSerial nm = Db.field nm serial
