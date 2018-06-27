module Database.Beam.Migrate.Tool.Log where

import           Database.Beam
import           Database.Beam.Migrate.Log
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Tool.Backend
import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Diff
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Status

import           Control.Exception
import           Control.Monad

import qualified Data.Text as T
import           Text.Read (readMaybe)

displayLog :: MigrateCmdLine -> IO ()
displayLog MigrateCmdLine { migrateDatabase = Nothing } =
  fail "No database specified"
displayLog cmdLine@MigrateCmdLine { migrateDatabase = Just dbName } = do
  reg <- lookupRegistry cmdLine

  (db, _, SomeBeamMigrationBackend (be :: BeamMigrationBackend be m)) <-
    loadBackend cmdLine reg dbName

  case be of
    BeamMigrationBackend { backendTransact = transact } -> do
      res <- transact (migrationDbConnString db) $
             runSelectReturningList $ select $
             orderBy_ (desc_ . _logEntryId) $
             all_ (_beamMigrateLogEntries (beamMigrateDb @be @m))
      case res of
        Left err -> throwIO (CouldNotFetchLog err)
        Right entries ->
          forM_ entries $ \logEntry ->
          let sch = do
                commitId <- readMaybe (T.unpack (_logEntryCommitId logEntry))
                lookupSchema commitId [] reg
          in case sch of
               Nothing -> throwIO (InvalidCommitId (_logEntryCommitId logEntry))
               Just sch' -> do
                 showCommit (_logEntryDate logEntry) sch'
