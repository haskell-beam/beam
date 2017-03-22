{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Beam.Migrate.Tool where

import           Database.Beam
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
import           Data.Time (LocalTime)

import           Options.Applicative

import           GHC.Generics

import           System.Directory
import           System.FilePath

--import Options.Applicative

-- * Migration table

data MigrationT f
  = MigrationT
  { migrationNumber :: Columnar f Int
  , migrationName   :: Columnar f Text
  , migrationRanAt  :: Columnar f LocalTime
  } deriving Generic
instance Beamable MigrationT
type MigrationTable = MigrationT Identity
deriving instance Show MigrationTable; deriving instance Eq MigrationTable

instance Table MigrationT where
  data PrimaryKey MigrationT f = MigrationId (Columnar f Int) deriving Generic
  primaryKey = MigrationId . migrationNumber
instance Beamable (PrimaryKey MigrationT)

-- * Options type

data BeamMigrationCommand beOptions
  = BeamMigrationCommand
  { migrationCommandBackend        :: String
  , migrationCommandMigrationModule :: String
  , migrationCommandBackendOptions :: beOptions }
  deriving Show

data BeamMigrationSubcommand
  = BeamMigrationWriteScript
  | BeamMigrationListMigrations
  deriving Show

data DdlResult
  = DdlResultOk
  | DdlResultError String
  deriving Show

data BeamMigrationBackend commandSyntax where
  BeamMigrationBackend :: Show beOptions =>
                          Parser beOptions
                       -> (forall a. MigrationSteps commandSyntax a -> BL.ByteString)
--                       -> (forall a. beOptions -> (beConnection -> IO a) -> IO a)
--                       -> (beConnection -> commandSyntax -> IO DdlResult)
                       -> BeamMigrationBackend commandSyntax
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

    writeScriptArgParser = pure BeamMigrationWriteScript
    listMigrationsArgParser = pure BeamMigrationListMigrations

migrationCommandOptions :: Parser beOptions -> ParserInfo (BeamMigrationCommand beOptions)
migrationCommandOptions beOptions =
  info (migrationCommandArgParser beOptions <**> helper)
       (fullDesc <> progDesc "Beam schema migration tool" <> header "beam-migrate -- migrate database schemas for various beam backends")

migrationCommandAndSubcommandOptions :: Parser beOptions -> ParserInfo (BeamMigrationCommand beOptions, BeamMigrationSubcommand)
migrationCommandAndSubcommandOptions beOptions =
  info (((,) <$> migrationCommandArgParser beOptions <*> subcommandParser) <**> helper)
       (fullDesc <> progDesc "Beam schema migration tool" <> header "beam-migrate -- migrate database schemas for various beam backends")

-- * Tool entry point

invokeMigrationTool :: BeamMigrationBackend cmdSyntax -> BeamMigrationCommand beOptions
                    -> BeamMigrationSubcommand -> MigrationSteps cmdSyntax () -> IO ()
invokeMigrationTool be@(BeamMigrationBackend _ renderSteps) BeamMigrationCommand {..}  subcommand steps =
  case subcommand of
    BeamMigrationListMigrations ->
      do putStrLn "Registered migrations:"
         mapM_ (putStrLn . ("  - " ++) . T.unpack) (stepNames steps)
    BeamMigrationWriteScript ->
      do BL.putStrLn (renderSteps steps)
