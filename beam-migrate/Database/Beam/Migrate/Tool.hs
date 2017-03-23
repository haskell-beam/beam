{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Beam.Migrate.Tool where

import           Database.Beam
import           Database.Beam.Migrate.Tool.Schema
import           Database.Beam.Migrate.Types (MigrationSteps, stepNames)

import           Control.Monad

import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T

import           Options.Applicative

import           GHC.Generics

import           System.Directory
import           System.FilePath

--import Options.Applicative

-- * Options type

data BeamMigrationCommand beOptions
  = BeamMigrationCommand
  { migrationCommandBackend        :: String
  , migrationCommandMigrationModule :: String
  , migrationCommandBackendOptions :: beOptions }
  deriving Show

data BeamMigrationSubcommand
  = WriteScript
  | ListMigrations
  | Init
  | Up
  deriving Show

data DdlResult
  = DdlResultOk
  | DdlResultError String
  deriving Show

data BeamMigrationBackend commandSyntax where
  BeamMigrationBackend :: (Show beOptions, MonadBeamDdl commandSyntax m) =>
                       { backendOptsParser :: Parser beOptions
                       , backendRenderSteps :: forall a. MigrationSteps commandSyntax a -> BL.ByteString
                       , backendTransact :: forall a. beOptions -> m a -> IO (Either DdlResult a)
                       } -> BeamMigrationBackend commandSyntax
data SomeBeamMigrationBackend where
  SomeBeamMigrationBackend :: Typeable commandSyntax => BeamMigrationBackend commandSyntax
                           -> SomeBeamMigrationBackend

migrationCommandArgParser :: Parser beOptions -> Parser (BeamMigrationCommand beOptions)
migrationCommandArgParser beOptions =
  BeamMigrationCommand <$> strOption (long "backend" <> metavar "BACKEND" <> help "Module to load for migration backend. Must be given first")
                       <*> strOption (long "migration" <> short 'M' <> metavar "MIGRATIONMODULE" <> help "Module containing migration steps")
                       <*> beOptions

subcommandParser :: Parser BeamMigrationSubcommand
subcommandParser = subparser (command "write-script" writeScriptCommand <> help "Write the entire set of migrations as one script") <|>
                   subparser (command "list-migrations" listMigrationsCommand <> help "List all discovered migrations")
  where
    writeScriptCommand =
      info (writeScriptArgParser <**> helper) (fullDesc <> progDesc "Write a migration script for the given migrations")
    listMigrationsCommand =
      info (listMigrationsArgParser <**> helper) (fullDesc <> progDesc "List all discovered migrations")

    writeScriptArgParser    = pure WriteScript
    listMigrationsArgParser = pure ListMigrations

migrationCommandOptions :: Parser beOptions -> ParserInfo (BeamMigrationCommand beOptions)
migrationCommandOptions beOptions =
  info (migrationCommandArgParser beOptions <**> helper)
       (fullDesc <> progDesc "Beam schema migration tool" <> header "beam-migrate -- migrate database schemas for various beam backends")

migrationCommandAndSubcommandOptions :: Parser beOptions -> ParserInfo (BeamMigrationCommand beOptions, BeamMigrationSubcommand)
migrationCommandAndSubcommandOptions beOptions =
  info (((,) <$> migrationCommandArgParser beOptions <*> subcommandParser) <**> helper)
       (fullDesc <> progDesc "Beam schema migration tool" <> header "beam-migrate -- migrate database schemas for various beam backends")

-- * Migration table creation script

writeMigrationTables :: IO ()
writeMigrationTables = pure ()

-- * Tool entry point

invokeMigrationTool :: BeamMigrationBackend cmdSyntax -> BeamMigrationCommand beOptions
                    -> BeamMigrationSubcommand -> MigrationSteps cmdSyntax () -> IO ()
invokeMigrationTool be@(BeamMigrationBackend _ renderSteps) BeamMigrationCommand {..}  subcommand steps =
  case subcommand of
    ListMigrations ->
      do putStrLn "Registered migrations:"
         mapM_ (putStrLn . ("  - " ++) . T.unpack) (stepNames steps)
    WriteScript ->
      do BL.putStrLn (renderSteps steps)
    Init ->
      do writeMigrationsTable
--    Up ->
--      do ensureMigrationsTable
