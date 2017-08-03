{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Database.Beam.Haskell.Syntax
import Database.Beam.Backend.SQL
import Database.Beam.Migrate.SQL.SQL92
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
    MigrateCommandDatabases (DatabaseCommandShow dbName) ->
      showDatabase cmdLine dbName

    MigrateCommandInit initCommand ->
      initBeamMigrate cmdLine initCommand

--    MigrateCommandClean cleanCommand ->
--      cleanBeamMigrate cmdLine cleanCommand

--    MigrateCommandLog ->
--      displayLog cmdLine

--    MigrateCommandStatus ->
--      displayStatus cmdLine

    _ -> let actions = [ createTableCmd (createTableSyntax mempty "testTbl" [("testField", fieldTy), ("field2", field2Ty)] []) ]
             fieldTy = columnSchemaSyntax intType Nothing [c] Nothing
             field2Ty = columnSchemaSyntax (varCharType (Just 128) Nothing) Nothing [c] Nothing
             c = constraintDefinitionSyntax Nothing notNullConstraintSyntax Nothing
         in case renderHsSchema (hsActionsToModule "Test" actions) of
              Left  err -> fail err
              Right sch -> putStrLn sch
