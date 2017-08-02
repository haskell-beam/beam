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
