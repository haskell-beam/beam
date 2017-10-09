module Database.Beam.Migrate.Tool.Status where

import           Database.Beam.Migrate hiding (timestamp)
import           Database.Beam.Migrate.Tool.Backend
import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Schema
import           Database.Beam.Migrate.Tool.Diff

import           Data.Monoid
import           Data.Text (unpack)
import qualified Data.Text as T
import           Data.Time (LocalTime)
import           Data.UUID (UUID)

import           System.Console.ANSI

import           Text.Read

data MigrateStatus
  = MigrateStatusNoCommits
  | MigrateStatusAtBranch UUID LocalTime PredicateDiff
  deriving Show

displayMigrateStatus :: MigrateCmdLine -> MigrationRegistry
                     -> DatabaseName -> MigrateStatus
                     -> IO ()
displayMigrateStatus _ reg dbName sts = do
  putStrLn ("Status for '" ++ unDatabaseName dbName ++ "':\n")
  case sts of
    MigrateStatusNoCommits ->
      putStr . unlines $
      [ "No commit history in this database."
      , ""
      , "You have some choices: "
      , ""
      , "  1. Run 'beam-migrate database match -d DBNAME' to attempt to determine"
      , "     which commit this database is at. If you have a good idea of which"
      , "     commit we're at, run "
      , "     'beam-migrate database match -d DBNAME --hint COMMIT'"
      , ""
      , "  2. Run 'beam-migrate import -d DBNAME --commit' to import the current"
      , "     database state as a new migration from the empty state."
      , ""
      , "  3. Run 'beam-migrate migrate --auto -d DBNAME COMMIT' to generate a"
      , "     commit for the current database, and then an automatic migration"
      , "     from that commit to the commit specified." ]
    MigrateStatusAtBranch branchId timestamp (PredicateDiff expected actual) ->
      case lookupSchema branchId [] reg of
        Nothing ->
          putStrLn . unlines $
          [ "At commit " ++ show branchId ++ ", which is not registered in "
          , "the registry."
          , ""
          , "This usually happens because this database is managed by another"
          , "beam-migrate registry. Are you running 'beam-migrate' from the"
          , "right directory?" ]
        Just sch -> do
          putStrLn ("At commit " ++ show branchId ++ "\n")
          showCommit timestamp sch

          let green x = setSGRCode [ SetColor Foreground Dull Green ] ++ x ++ setSGRCode [ Reset ]
          if expected == actual
            then putStrLn (green "Everything is up-to-date")
            else putStrLn "Database is ahead of schema\nRun 'beam-migrate diff' for a full diff"

showCommit :: LocalTime -> RegisteredSchemaInfo -> IO ()
showCommit atTime sch = do
  let green x = setSGRCode [ SetColor Foreground Dull Green ] ++ x ++ setSGRCode [ Reset ]
  putStrLn . unlines $
    [  setSGRCode [ SetColor Foreground Dull Yellow ] ++ "schema " ++ show (registeredSchemaInfoHash sch)
    , green "Date" ++ ":      " ++ show atTime
    , green "Commiter" ++ ":  " ++ unpack (userInfoCommitter (registeredSchemaInfoCommitter sch))
    , green "Formats" ++ ":   " ++
      showMigrationFormats (registeredSchemaInfoFormats sch) ]
  putStrLn (T.unpack . T.unlines . map ("    " <>) .
             T.lines . registeredSchemaInfoMessage $ sch)

displayStatus :: MigrateCmdLine -> IO ()
displayStatus MigrateCmdLine { migrateDatabase = Nothing } =
  fail "No database specified"
displayStatus cmdLine@(MigrateCmdLine { migrateDatabase = Just dbName }) = do
  reg <- lookupRegistry cmdLine
  (db, _, SomeBeamMigrationBackend be) <- loadBackend cmdLine reg dbName

  hasSchema <- hasBackendTables (migrationDbConnString db) be
  migrateStatus <-
    if not hasSchema
    then do
      putStrLn "WARNING: Beam migrate not installed in this database. Run"
      putStrLn "WARNING:"
      putStrLn ("WARNING:  beam-migrate database upgrade " ++ unDatabaseName dbName)
      putStrLn "WARNING:"
      putStrLn "WARNING: to build the beam tables in the database"
      pure MigrateStatusNoCommits
    else case be of
           BeamMigrationBackend { backendTransact = transact } -> do
             logEntry <- reportDdlErrors (transact (migrationDbConnString db) getLatestLogEntry)
             case logEntry of
               Nothing -> pure MigrateStatusNoCommits
               Just logEntry' ->
                 case readMaybe (unpack (_logEntryCommitId logEntry')) of
                   Nothing -> fail "Invalid commit id for last log entry"
                   Just commitId -> do
                     diff <- genDiffFromSources cmdLine reg
                                                (PredicateFetchSourceDbHead db Nothing)
                                                (PredicateFetchSourceCommit (Just (migrationDbBackend db)) commitId)
                     pure (MigrateStatusAtBranch commitId (_logEntryDate logEntry') diff)

  displayMigrateStatus cmdLine reg dbName migrateStatus
