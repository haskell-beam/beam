{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Beam.Migrate.Tool where

import           Database.Beam
import           Database.Beam.Backend.Types
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

data DdlError
  = DdlError String
  deriving Show

data BeamMigrationBackend commandSyntax beOptions where
  BeamMigrationBackend :: (Show beOptions, MonadBeam commandSyntax be m) =>
                       { backendOptsParser :: Parser beOptions
                       , backendProxy :: Proxy be
                       , backendRenderSteps :: forall a. MigrationSteps commandSyntax a -> BL.ByteString
                       , backendTransact :: forall a. beOptions -> m a -> IO (Either DdlError a)
                       } -> BeamMigrationBackend commandSyntax beOptions
data SomeBeamMigrationBackend where
  SomeBeamMigrationBackend :: (Typeable commandSyntax, Typeable beOptions) =>
                              BeamMigrationBackend commandSyntax beOptions
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

writeMigrationsTable :: BeamMigrationBackend cmdSyntax beOptions -> beOptions -> IO ()
writeMigrationsTable BeamMigrationBackend {..} opts =
    do err <- backendTransact opts $ pure ()
       putStrLn (show err)
       pure ()

-- * Tool entry point

invokeMigrationTool :: BeamMigrationBackend cmdSyntax beOptions -> BeamMigrationCommand beOptions
                    -> BeamMigrationSubcommand -> MigrationSteps cmdSyntax () -> IO ()
invokeMigrationTool be@(BeamMigrationBackend {..}) BeamMigrationCommand {..}  subcommand steps =
  case subcommand of
    ListMigrations ->
      do putStrLn "Registered migrations:"
         mapM_ (putStrLn . ("  - " ++) . T.unpack) (stepNames steps)
    WriteScript ->
      do BL.putStrLn (backendRenderSteps steps)
    Init ->
      do writeMigrationsTable be migrationCommandBackendOptions
--    Up ->
--      do ensureMigrationsTable
