{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Postgres.Migrate where

import           Database.Beam.Backend.Types
import qualified Database.Beam.Migrate.SQL.Types as Db
import qualified Database.Beam.Migrate.Tool as Tool
import qualified Database.Beam.Migrate.Types as Db
import           Database.Beam.Postgres.Connection
import           Database.Beam.Postgres.PgSpecific
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import qualified Database.PostgreSQL.Simple as Pg

import           Control.Arrow
import           Control.Exception (bracket)
import           Control.Monad.Free.Church
import           Control.Monad.Reader (ask)
import           Control.Monad.State (put)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Char
import           Data.Coerce
import           Data.List (intersperse)
import           Data.Monoid
import           Data.Proxy
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word16)

import           Options.Applicative

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

migrationBackend :: Tool.BeamMigrationBackend PgCommandSyntax PgMigrateOpts
migrationBackend = Tool.BeamMigrationBackend parsePgMigrateOpts Proxy
                        (BL.concat . migrateScript)
                        (BCL.unpack . pgRenderSyntaxScript . fromPgCommand)
                        (\beOptions action -> do
                            bracket (Pg.connect (beConnectInfo beOptions)) Pg.close $ \conn ->
                              left pgToToolError <$> withPgDebug (\_ -> pure ()) conn action)
  where
    pgToToolError (PgRowParseError err) = Tool.DdlCustomError (show err)
    pgToToolError (PgInternalError err) = Tool.DdlCustomError err


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

migrateScript :: Db.MigrationSteps PgCommandSyntax () a' -> [BL.ByteString]
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

writeMigrationScript :: FilePath -> Db.MigrationSteps PgCommandSyntax () a -> IO ()
writeMigrationScript fp steps =
  let stepBs = migrateScript steps
  in BL.writeFile fp (BL.concat stepBs)

tsquery :: Db.DataType PgDataTypeSyntax TsQuery
tsquery = Db.DataType (PgDataTypeSyntax (emit "tsquery"))

tsvector :: Db.DataType PgDataTypeSyntax TsVector
tsvector = Db.DataType (PgDataTypeSyntax (emit "tsvector"))

smallserial, serial, bigserial :: Integral a => Db.DataType PgDataTypeSyntax a
smallserial = Db.DataType (PgDataTypeSyntax (emit "SMALLSERIAL"))
serial = Db.DataType (PgDataTypeSyntax (emit "SERIAL"))
bigserial = Db.DataType (PgDataTypeSyntax (emit "BIGSERIAL"))

text :: Db.DataType PgDataTypeSyntax T.Text
text = Db.DataType (PgDataTypeSyntax (emit "TEXT"))

boolean :: Db.DataType PgDataTypeSyntax a
boolean = Db.DataType pgBooleanType

bytea :: Db.DataType PgDataTypeSyntax ByteString
bytea = Db.DataType pgByteaType

array :: Maybe Int -> Db.DataType PgDataTypeSyntax a
      -> Db.DataType PgDataTypeSyntax (V.Vector a)
array Nothing (Db.DataType (PgDataTypeSyntax syntax)) = Db.DataType (PgDataTypeSyntax (syntax <> emit "[]"))
array (Just sz) (Db.DataType (PgDataTypeSyntax syntax)) = Db.DataType (PgDataTypeSyntax (syntax <> emit "[" <> emit (fromString (show sz)) <> emit "]"))
