module Database.Beam.Migrate.Tool.Database where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry

import           Control.Monad

import qualified Data.HashMap.Strict as HM

listDatabases :: MigrateCmdLine -> IO ()
listDatabases cmdLine = do
  reg <- lookupRegistry cmdLine

  forM_ (HM.toList (migrationRegistryDatabases reg)) $ \(DatabaseName dbName, _) ->
    putStrLn dbName

renameDatabase :: MigrateCmdLine -> DatabaseName -> DatabaseName -> IO ()
renameDatabase cmdLine from to =
  updatingRegistry cmdLine $ \reg ->
  case HM.lookup from (migrationRegistryDatabases reg) of
    Nothing -> fail ("No such database " ++ unDatabaseName from)
    Just db ->
      pure reg { migrationRegistryDatabases = HM.insert to db $
                                              HM.delete from  $
                                              migrationRegistryDatabases reg }

showDatabase :: MigrateCmdLine -> DatabaseName -> IO ()
showDatabase cmdLine dbName@(DatabaseName dbNameStr) = do
  reg <- lookupRegistry cmdLine

  case HM.lookup dbName (migrationRegistryDatabases reg) of
    Nothing -> fail "No such database"
    Just MigrationDatabase {..} -> do
      putStrLn ("Database '" ++ dbNameStr ++ "'")
      putStrLn ("  Backend: " ++ unModuleName migrationDbBackend)
      putStrLn ("  Conn   : " ++ migrationDbConnString)
