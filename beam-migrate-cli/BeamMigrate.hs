{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Database.Beam.Migrate.Tool.CmdLine
import Database.Beam.Migrate.Tool.Database
import Database.Beam.Migrate.Tool.Init

import Options.Applicative

main :: IO ()
main = do
  cmdLine@MigrateCmdLine { migrateSubcommand } <- execParser migrationCliOptions

  case migrateSubcommand of
    MigrateCommandDatabases DatabaseCommandList ->
      listDatabases cmdLine
    MigrateCommandDatabases (DatabaseCommandRename from to) ->
      renameDatabase cmdLine from to

    MigrateCommandInit initCommand ->
      initBeamMigrate cmdLine initCommand

--    MigrateCommandClean cleanCommand ->
--      cleanBeamMigrate cmdLine cleanCommand

--    MigrateCommandLog ->
--      displayLog cmdLine

--    MigrateCommandStatus ->
--      displayStatus cmdLine

    _ -> putStrLn "Bad"
