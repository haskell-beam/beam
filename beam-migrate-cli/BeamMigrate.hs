{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Database.Beam.Migrate.Tool.Branch
import Database.Beam.Migrate.Tool.CmdLine
import Database.Beam.Migrate.Tool.Database
import Database.Beam.Migrate.Tool.Diff
import Database.Beam.Migrate.Tool.Init
import Database.Beam.Migrate.Tool.Log
import Database.Beam.Migrate.Tool.Migrate
import Database.Beam.Migrate.Tool.SchemaCmd
import Database.Beam.Migrate.Tool.Status

import Data.Maybe

import Options.Applicative

main :: IO ()
main = do
  cmdLine@MigrateCmdLine { migrateSubcommand } <- execParser migrationCliOptions

  case migrateSubcommand of
    MigrateCommandDatabases DatabaseCommandList ->
      listDatabases cmdLine
    MigrateCommandDatabases (DatabaseCommandAdd dbName beName url) ->
      initDatabase cmdLine dbName beName url
    MigrateCommandDatabases (DatabaseCommandRename from to) ->
      renameDatabase cmdLine from to
    MigrateCommandDatabases (DatabaseCommandShow dbName) ->
      showDatabase cmdLine dbName

    MigrateCommandInit initCommand ->
      initBeamMigrate cmdLine initCommand

    MigrateCommandClean _ -> fail "Unimplemented"
--      cleanBeamMigrate cmdLine cleanCommand

    MigrateCommandLog ->
      displayLog cmdLine

    MigrateCommandStatus ->
      displayStatus cmdLine

    MigrateCommandDiff autogen expActual ->
      let (actual, expected) =
            maybe ("HEAD", "DB!")
                  (\(actualSrc, expSrc) -> (actualSrc,) . fromMaybe "HEAD" $ expSrc) expActual
      in displayDiff cmdLine expected actual autogen

    MigrateCommandBranch BranchCommandList ->
      listBranches cmdLine
    MigrateCommandBranch (BranchCommandDelete branchNm) ->
      deleteBranch cmdLine branchNm
    MigrateCommandBranch (BranchCommandNew dontSwitch branchNm) ->
      newBranch cmdLine dontSwitch branchNm

    MigrateCommandSchema (SchemaCommandImport _ dbName branchName doCommit doAutoMigrate) ->
      importDb cmdLine dbName branchName doCommit doAutoMigrate

    MigrateCommandSimple (SimpleCommandHsSchema backend connStr) ->
      showSimpleSchema cmdLine backend connStr

    MigrateCommandMigrate ->
      doMigrateDatabase cmdLine
