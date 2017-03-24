module Database.Beam.Postgres.Migrate where

import           Database.Beam.Backend.Types
import qualified Database.Beam.Migrate.SQL.Types as Db
import qualified Database.Beam.Migrate.Tool as Tool
import qualified Database.Beam.Migrate.Types as Db
import           Database.Beam.Postgres.Connection
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Internal as PgI

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
import qualified Data.Text.Encoding as TE
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

data PgStream a = PgStreamDone     (Either Tool.DdlError a)
                | PgStreamContinue (Maybe PgI.Row -> IO (PgStream a))

beConnectInfo :: PgMigrateOpts -> Pg.ConnectInfo
beConnectInfo opts = Pg.defaultConnectInfo { connectHost = pgMigrateHost opts
                                           , connectPort = pgMigratePort opts
                                           , connectUser = pgMigrateUser opts
                                           , connectDatabase = pgMigrateDatabase opts }

migrationBackend :: Tool.BeamMigrationBackend PgCommandSyntax PgMigrateOpts
migrationBackend = Tool.BeamMigrationBackend parsePgMigrateOpts Proxy
                        (BL.concat . migrateScript)
                        (BCL.unpack . pgRenderSyntaxScript . fromPgCommand)
                        (\beOptions (Pg action) -> do
                            bracket (Pg.connect (beConnectInfo beOptions)) Pg.close (runPg action))
  where
    runPg :: forall a. F PgF a -> Pg.Connection -> IO (Either Tool.DdlError a)
    runPg action conn =
      let finish x = pure (Right x)
          step (PgLiftIO action next) = action >>= next
          step (PgFetchNext next) = next Nothing
          step (PgRunReturning (PgCommandSyntax syntax)
                               (mkProcess :: Pg (Maybe x) -> Pg a')
                               next) =
            do query <- pgRenderSyntax conn syntax
               let Pg process = mkProcess (Pg (liftF (PgFetchNext id)))
               action <- runF process finishProcess stepProcess Nothing
               case action of
                 PgStreamDone (Right x) -> Pg.execute_ conn (fromString (BS.unpack query)) >> next x
                 PgStreamDone (Left err) -> pure (Left err)
                 PgStreamContinue nextStream ->
                   let finishUp (PgStreamDone (Right x)) = next x
                       finishUp (PgStreamDone (Left err)) = pure (Left err)
                       finishUp (PgStreamContinue next) = next Nothing >>= finishUp

                       columnCount = fromIntegral $ valuesNeeded (Proxy @Postgres) (Proxy @x)
                   in Pg.foldWith_ (PgI.RP (put columnCount >> ask)) conn (fromString (BS.unpack query)) (PgStreamContinue nextStream) runConsumer >>= finishUp

          finishProcess :: forall row a. a -> Maybe PgI.Row -> IO (PgStream a)
          finishProcess x _ = pure (PgStreamDone (Right x))

          stepProcess :: forall a. PgF (Maybe PgI.Row -> IO (PgStream a)) -> Maybe PgI.Row -> IO (PgStream a)
          stepProcess (PgLiftIO action next) row = action >>= flip next row
          stepProcess (PgFetchNext next) Nothing =
            pure . PgStreamContinue $ \res ->
            case res of
              Nothing -> next Nothing Nothing
              Just (PgI.Row rowIdx res) ->
                getFields res >>= \fields ->
                runPgRowReader conn rowIdx res fields fromBackendRow >>= \res ->
                case res of
                  Left err -> pure (PgStreamDone (Left (Tool.DdlError ("Row read error: " ++ show err))))
                  Right r -> next r Nothing
          stepProcess (PgFetchNext next) (Just (PgI.Row rowIdx res)) =
            getFields res >>= \fields ->
            runPgRowReader conn rowIdx res fields fromBackendRow >>= \res ->
            case res of
              Left err -> pure (PgStreamDone (Left (Tool.DdlError ("Row read error: " ++ show err))))
              Right r -> pure (PgStreamContinue (next (Just r)))
          stepProcess (PgRunReturning _ _ _) _ = pure (PgStreamDone (Left (Tool.DdlError "Nested queries not allowed")))

          runConsumer :: forall a. PgStream a -> PgI.Row -> IO (PgStream a)
          runConsumer s@(PgStreamDone x) _ = pure s
          runConsumer (PgStreamContinue next) row = next (Just row)
      in runF action finish step

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
