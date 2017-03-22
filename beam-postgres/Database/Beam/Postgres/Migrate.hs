module Database.Beam.Postgres.Migrate where

import qualified Database.Beam.Migrate.Types as Db
import qualified Database.Beam.Migrate.SQL.Types as Db
import qualified Database.Beam.Migrate.Tool as Tool
import           Database.Beam.Postgres.Syntax

import           Control.Monad.Free.Church

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.List (intersperse)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text.Encoding as TE

import           Options.Applicative

data PgMigrateOpts
  = PgMigrateOpts
  { pgMigrateHost :: String
  , pgMigrateDatabase :: String
  , pgMigratePort :: Int
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

migrationBackend :: Tool.BeamMigrationBackend PgCommandSyntax
migrationBackend = Tool.BeamMigrationBackend parsePgMigrateOpts (BL.concat . migrateScript)

pgRenderSyntaxScript :: PgSyntax -> BL.ByteString
pgRenderSyntaxScript (PgSyntax mkQuery) =
  toLazyByteString (runF mkQuery finish step)
  where
    finish _ = mempty
    step (EmitBuilder b next) = b <> next
    step (EmitByteString b next) = byteString b <> next
    step (EscapeString b next) = escapePgString b <> next
    step (EscapeBytea b next) = escapePgBytea b <> next
    step (EscapeIdentifier b next) = escapePgIdentifier b <> next

    escapePgString b = byteString (B.concatMap (\w -> if w == fromIntegral (ord '\'') then "''" else B.singleton w) b)
    escapePgBytea b = error "escapePgBytea: no connection"
    escapePgIdentifier b = error "escapePgIdentifier: no connection"

migrateScript :: Db.MigrationSteps PgCommandSyntax a -> [BL.ByteString]
migrateScript steps =
  "-- CAUTION: beam-postgres currently escapes postgres string literals somewhat\n"                 :
  "--          haphazardly when generating scripts (but not when generating commands)\n"            :
  "--          This is due to technical limitations in libPq that require a Postgres\n"             :
  "--          Connection in order to correctly escape strings. Please verify that the\n"           :
  "--          Generated migration script is correct before running it on your database.\n"         :
  "--          If you feel so called, please contribute better escaping support to beam-postgres\n" :
  "\n":
  "-- Set connection encoding to UTF-8\n":
  "SET client_encoding = 'UTF8';\n":
  "SET standard_conforming_strings = off;\n\n":
  appEndo (Db.migrateScript renderHeader renderCommand steps) []
  where
    renderHeader nm =
      Endo (("-- " <> BL.fromStrict (TE.encodeUtf8 nm) <> "\n"):)
    renderCommand command =
      Endo ((pgRenderSyntaxScript (fromPgCommand command) <> ";\n"):)

writeMigrationScript :: FilePath -> Db.MigrationSteps PgCommandSyntax a -> IO ()
writeMigrationScript fp steps =
  let stepBs = migrateScript steps
  in BL.writeFile fp (BL.concat stepBs)

smallserial, serial, bigserial :: Integral a => Db.DataType PgDataTypeSyntax a
smallserial = Db.DataType (PgDataTypeSyntax (emit "SMALLSERIAL"))
serial = Db.DataType (PgDataTypeSyntax (emit "SERIAL"))
bigserial = Db.DataType (PgDataTypeSyntax (emit "BIGSERIAL"))

boolean :: Db.DataType PgDataTypeSyntax a
boolean = Db.DataType pgBooleanType

bytea :: Db.DataType PgDataTypeSyntax ByteString
bytea = Db.DataType pgByteaType
