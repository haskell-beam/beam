{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Beam.Migrate.Tool.CmdLine where

import Data.Monoid
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import Data.String (fromString)

import Options.Applicative

newtype ModuleName = ModuleName { unModuleName :: String }
  deriving (Show, Eq, Ord, ToJSON, FromJSON)
newtype DatabaseName = DatabaseName { unDatabaseName :: String }
  deriving (Show, Eq, Ord, ToJSONKey, FromJSONKey, Hashable)

data InitCommand
  = InitCommand
  { initBackend          :: Maybe ModuleName
  , initConnectionString :: Maybe String
  , initModule           :: ModuleName
  , initModulePath       :: FilePath

  , initInteractive      :: Bool
  , initCreateSchema     :: Bool
  } deriving Show

data CleanCommand
  = CleanCommand
  { cleanForce :: Bool
  } deriving Show

data DatabaseCommand
  = DatabaseCommandList
  | DatabaseCommandAdd DatabaseName ModuleName String
  | DatabaseCommandShow DatabaseName
  | DatabaseCommandRename DatabaseName DatabaseName
  deriving Show

data SimpleCommand
  = SimpleCommandHsSchema ModuleName String
  deriving Show

data BranchCommand
  = BranchCommandList
  | BranchCommandDelete Text
  | BranchCommandNew    Bool {-^ Don't switch -} Text
  deriving Show

data SchemaCommand
  = SchemaCommandImport !Bool DatabaseName (Maybe Text) !Bool {-^ Commit in database -} !Bool {-^ Do auto migrate -}
    -- ^ Create a haskell migration for the given database

  | SchemaCommandNew Text {-^ The schema to iterate on -}
                     FilePath     {-^ The temporary file to use -}
  deriving Show

data MigrateCommand
  = MigrateCommandInit InitCommand   -- ^ Initialize a new beam migrate registry
  | MigrateCommandClean CleanCommand -- ^ Remove beam-migrate tables from a database

  | MigrateCommandLog
  | MigrateCommandStatus

  | MigrateCommandDatabases DatabaseCommand
  | MigrateCommandBranch    BranchCommand

  | MigrateCommandSchema SchemaCommand

  | MigrateCommandAbort !Bool

  | MigrateCommandDiff Bool (Maybe (Text, Maybe Text))

  | MigrateCommandMigrate

  | MigrateCommandSimple SimpleCommand
  deriving Show

data MigrateCmdLine
  = MigrateCmdLine
  { migrateRegistryPath :: Maybe FilePath
  , migratePackagePath  :: [ FilePath ]
  , migrateDatabase     :: Maybe DatabaseName

  , migrateSubcommand   :: MigrateCommand
  } deriving Show

migrationArgParser :: Parser MigrateCmdLine
migrationArgParser =
  MigrateCmdLine <$> optional (strOption (long "registry" <> short 'r' <> metavar "REGISTRY" <> help "Path to beam-migrate registry"))
                 <*> many (strOption (long "package-path" <> metavar "PACKAGE-PATH" <> help "Additional GHC package paths to search for backends"))
                 <*> optional (DatabaseName <$> strOption (long "database" <> short 'd' <> metavar "DATABASE" <> help "Name of database to use"))
                 <*> (subparser $ mconcat [ command "init" initCommand, command "clean" cleanCommand

                                          , command "log" logCommand, command "status" statusCommand

                                          , command "diff" diffCommand

                                          , command "database" databaseCommand
                                          , command "branch" branchCommand
                                          , command "simple" simpleCommand

                                          , command "schema" schemaCommand
                                          , command "abort" abortCommand

                                          , command "migrate" migrateCommand ])
  where
    initCommand = info (initParser <**> helper) (fullDesc <> progDesc "Initialize a beam-migrate registry in this directory, or in the given registration file")
    cleanCommand = info (cleanParser <**> helper) (fullDesc <> progDesc "Remove all beam-migrate tables from the database")

    logCommand = info (pure MigrateCommandLog <**> helper) (fullDesc <> progDesc "Display migration history of the given database")
    statusCommand = info (pure MigrateCommandStatus <**> helper) (fullDesc <> progDesc "Show status of beam migrations")
    diffCommand = info (diffParser <**> helper) (fullDesc <> progDesc "Show diff between revisions")
    databaseCommand = info (databasesParser <**> helper) (fullDesc <> progDesc "Create, update, list databases in the registry")
    branchCommand = info (branchParser <**> helper) (fullDesc <> progDesc "Create, update, list branches in the registry")
    schemaCommand = info (schemaParser <**> helper) (fullDesc <> progDesc "Create, update, import, and list schemas")
    simpleCommand = info (simpleParser <**> helper) (fullDesc <> progDesc "Simple utilities that do not require a full beam-migrate setup")
    migrateCommand = info (migrateParser <**> helper) (fullDesc <> progDesc "Bring the given database up-to-date with the current branch")
    abortCommand = info (abortParser <**> helper) (fullDesc <> progDesc "Abort any edits taking place")

    initParser = MigrateCommandInit <$>
                 (InitCommand <$> optional (ModuleName <$> strOption (long "backend" <> metavar "BACKEND" <> help "Backend module to use"))
                              <*> optional (strOption (long "connection" <> metavar "CONNECTION" <> help "Connection string for backend"))
                              <*> (ModuleName <$> strOption (long "module" <> metavar "MODULE" <> help "Module to use"))
                              <*> strOption (long "src-dir" <> metavar "SOURCEDIR" <> help "Directory containing source files" <> value ".")
                              <*> flag True False (long "no-prompt" <> help "Do not prompt; fail instead")
                              <*> flag True False (long "no-create" <> help "Do not create the beam-migrate schema in the database"))

    cleanParser = MigrateCommandClean <$> (CleanCommand <$> switch (long "force" <> short 'f' <> help "Do not prompt"))

    diffParser = MigrateCommandDiff <$> flag False True (long "auto-script" <> help "Display migration as an auto-generated script, if possible")
                                    <*> optional ((,) <$> (fromString <$> strArgument (metavar "ACTUAL" <> help "Reference to use as actual predicate source (default: DB!)"))
                                                      <*> optional (fromString <$> strArgument (metavar "EXPECTED" <> help "Reference to use as correct predicate source (default: HEAD)")))

    databasesParser = MigrateCommandDatabases <$> subparser (mconcat [ command "add"    databasesAddCommand
                                                                     , command "list"   databasesListCommand
                                                                     , command "show"   databasesShowCommand
                                                                     , command "rename" databasesRenameCommand ])
    databasesAddCommand =
      info (databasesAddParser <**> helper) (fullDesc <> progDesc "Add a new database to the registry")
      where databasesAddParser = DatabaseCommandAdd <$> (DatabaseName <$> strArgument (metavar "NAME" <> help "Name of new database"))
                                                    <*> (ModuleName <$> strArgument (metavar "BACKEND" <> help "Backend Haskell module"))
                                                    <*> strArgument (metavar "CONN" <> help "Connection string")
    databasesListCommand =
      info (databasesListParser <**> helper) (fullDesc <> progDesc "List databases in the registry")
      where databasesListParser = pure DatabaseCommandList
    databasesShowCommand =
      info (databasesShowParser <**> helper) (fullDesc <> progDesc "Show database with given name")
      where databasesShowParser = DatabaseCommandShow <$> (DatabaseName <$> strArgument (metavar "DATABASE" <> help "Database to show"))
    databasesRenameCommand =
      info (databasesRenameParser <**> helper) (fullDesc <> progDesc "Rename a database from OLDNAME to NEWNAME")
      where databasesRenameParser = DatabaseCommandRename <$> (DatabaseName <$> strArgument (metavar "OLDNAME" <> help "Database to rename"))
                                                          <*> (DatabaseName <$> strArgument (metavar "NEWNAME" <> help "New name for database"))

    branchParser = MigrateCommandBranch <$> subparser (mconcat [ command "list" branchListCommand
                                                               , command "delete" branchDeleteCommand
                                                               , command "new" branchNewCommand ])

    branchListCommand =
      info (branchListParser <**> helper) (fullDesc <> progDesc "List branches in registry")
      where branchListParser = pure BranchCommandList
    branchDeleteCommand =
      info (branchDeleteParser <**> helper) (fullDesc <> progDesc "Delete branch from registry")
      where branchDeleteParser = BranchCommandDelete <$> (fromString <$> strArgument (metavar "BRANCH" <> help "Branch to delete"))
    branchNewCommand =
      info (branchNewParser <**> helper) (fullDesc <> progDesc "Create new branch starting from current HEAD")
      where branchNewParser = BranchCommandNew <$> flag False True (long "dont-switch" <> help "Do not switch to the new branch")
                                               <*> (fromString <$> strArgument (metavar "BRANCH" <> help "Name of new branch"))

    schemaParser = MigrateCommandSchema <$> subparser (mconcat [ command "import" schemaImportCommand
                                                               , command "new" schemaNewCommand ])

    schemaImportCommand = info (importParser <**> helper) (fullDesc <> progDesc "Import a database schema into haskell")
      where
        importParser = SchemaCommandImport <$> flag True False (long "interactive" <> short 'i' <> help "Run in interactive mode")
                                           <*> (DatabaseName <$> strArgument (metavar "DATABASE" <> help "Database to import from"))
                                           <*> optional (fromString <$> strArgument (metavar "BRANCHNAME" <> help "Branch to import into"))
                                           <*> flag True False (long "no-commit" <> help "Do not record commit in local change log")
                                           <*> flag True False (long "no-migrate" <> help "Do not generate an automatic migration")

    schemaNewCommand = info (newParser <**> helper) (fullDesc <> progDesc "Create a new schema")
      where
        newParser = SchemaCommandNew <$> (fromString <$> strArgument (metavar "FROM" <> help "Schema to iterate on" <> value "HEAD"))
                                     <*> strOption (long "tmp-file" <> metavar "TMPFILE" <> help "Temporary file to edit schema" <> value "BeamMigrateSchema.hs")

    simpleParser = MigrateCommandSimple <$> subparser (mconcat [ command "schema" simpleSchemaCommand ])

    simpleSchemaCommand =
      info (simpleSchemaParser <**> helper) (fullDesc <> progDesc "Extract a haskell schema from the given database")
      where simpleSchemaParser = SimpleCommandHsSchema <$> (ModuleName <$> strOption (long "backend" <> metavar "BACKEND" <> help "Backend module to use"))
                                                       <*> strOption (long "connection" <> metavar "CONNECTION" <> help "Connection string for backend")

    migrateParser = pure MigrateCommandMigrate

    abortParser = MigrateCommandAbort <$> flag False True (long "force" <> short 'f' <> help "Force this abort, even if the file has local changes")

migrationCliOptions :: ParserInfo MigrateCmdLine
migrationCliOptions =
  info (migrationArgParser <**> helper)
       (fullDesc <> progDesc "Beam migrate command-line interface" <>
        header "beam-migrate -- migrate database schemas for various beam backends")
