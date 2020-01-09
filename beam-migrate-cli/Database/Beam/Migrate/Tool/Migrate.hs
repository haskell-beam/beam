module Database.Beam.Migrate.Tool.Migrate where

import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Tool.Backend
import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Diff
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Status

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Fail as Fail

import qualified Data.ByteString.Char8 as BS
import           Data.Char as Char
import           Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Query as Gr
import qualified Data.HashSet as HS
import           Data.List (find)
import           Data.List.Split (splitWhen)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.UUID (UUID)

import           System.IO
import           System.Console.ANSI

data MigrateDDLCommand cmd
  = MigrateDDLCommandBeam BS.ByteString cmd
  | MigrateDDLCommandRaw BS.ByteString
  deriving Show

data CLIMigration cmd = CLIMigration Text [MigrateDDLCommand cmd]

showCommands :: [CLIMigration cmd] -> IO ()
showCommands cmds = do
  putStrLn "The following commands will be run, in order:"

  forM_ (zip [1..] cmds) $ \(i, CLIMigration stepDescr cmds) ->
    do putStrLn (yellow ("Step " ++ show i ++ ": " ++ T.unpack stepDescr))
       forM_ (zip [1..] cmds) $ \(cmdIdx, cmd) ->
         case cmd of
           MigrateDDLCommandRaw rawCmd ->
             putStrLn (green ("  " ++ show cmdIdx) ++ ": " ++ BS.unpack rawCmd)
           MigrateDDLCommandBeam rawCmd _ ->
             putStrLn (green ("  " ++ show cmdIdx) ++ ": " ++ BS.unpack rawCmd)
       putStrLn ""

  where
    yellow x = setSGRCode [ SetColor Foreground Dull Yellow ] ++ x ++ setSGRCode [ Reset ]
    green x = setSGRCode [ SetColor Foreground Dull Green ] ++ x ++ setSGRCode [ Reset ]

getSchemaCommandsForBackend :: Fail.MonadFail m
                            => MigrationRegistry -> Maybe (BeamMigrationBackend be m)
                            -> UUID -> IO [ MigrateDDLCommand cmd ]
getSchemaCommandsForBackend reg Nothing id = fail "Asked to get haskell schema"
getSchemaCommandsForBackend reg (Just be@(BeamMigrationBackend {})) commitId =
  do let path = schemaFilePathForBackend (Just (SomeBeamMigrationBackend be)) reg commitId
     cmdData <- BS.lines <$> BS.readFile path

     let cmds = fmap (fst . BS.spanEnd isSpace . BS.dropWhile isSpace . BS.unlines) $
                filter (any (not . BS.null)) $ -- Ensure every command contains something
                splitWhen ("--" `BS.isPrefixOf`) cmdData

     pure (fmap MigrateDDLCommandRaw cmds)

doMigrateDatabase :: MigrateCmdLine -> Bool -> IO ()
doMigrateDatabase MigrateCmdLine { migrateDatabase = Nothing } _ =
  fail "No database specified"
doMigrateDatabase cmdLine@MigrateCmdLine { migrateDatabase = Just dbName } script = do
  registry <- lookupRegistry cmdLine
  db <- lookupDb registry cmdLine

  let destCommit = registryHeadCommit registry

  sts <- getStatus cmdLine registry dbName
  (_, _, SomeBeamMigrationBackend be@(BeamMigrationBackend {} :: BeamMigrationBackend be m)) <-
    loadBackend cmdLine registry dbName

  cmds <-
    case sts of
      MigrateStatusNoCommits False -> fail "The beam-migrate schema does not exist in this database"
      MigrateStatusNoCommits True -> do
        putStrLn "This database has no history."
        putStrLn ("Migrating database to " ++ show destCommit)

        let backendFmt = MigrationFormatBackend (unModuleName (migrationDbBackend db))

            schema = lookupSchema destCommit [ backendFmt ] registry <|>
                     lookupSchema destCommit [ MigrationFormatHaskell ] registry

        case schema of
          Nothing -> fail ("No schema for " ++ show destCommit ++ " in backend " ++ unModuleName (migrationDbBackend db))
          Just sch -> do
            -- TODO user should be able to override this choice
            backend <- if backendFmt `elem` registeredSchemaInfoFormats sch
                       then do
                         putStrLn "Using native backend"
                         pure (Just be)
                       else do
                         putStrLn "Using Haskell beam-migrate backend"
                         pure Nothing

            Just . pure . CLIMigration (fromString ("Initial import of schema " ++ show destCommit)) <$>
              getSchemaCommandsForBackend registry backend destCommit

      MigrateStatusAtBranch srcCommit _ (PredicateDiff expected actual)
        | expected /= actual -> fail "The database does not match its current schema (TODO add --fix option)"
        | otherwise -> do
            let migrations = registryMigrationGraph registry

                findMigration commit = fst <$> find (\(_, s) -> registeredSchemaInfoHash s == commit) (labNodes migrations)

            case (,) <$> findMigration srcCommit
                     <*> findMigration  destCommit of
              Nothing -> fail "Can't find schemas"
              Just (sourceNode, destNode)
                  | sourceNode == destNode -> do
                      putStrLn "The database is already up-to-date with HEAD"
                      pure Nothing
                  | otherwise ->
                      case unLPath (Gr.lesp sourceNode destNode migrations) of
                        [] -> do
                          hPutStr stderr . unlines $
                            [ "There is no migration between " ++ show srcCommit ++
                              " and " ++ show destCommit
                            , ""
                            , "You can ask beam to generate one for you automatically, by running 'beam-migrate migration new --auto"
                            , "Alternatively, you can run 'beam-migrate migrate --manual', to write a custom migration script"
                            ]
                          fail "No possible migration"
                        path -> fail ("Migrating " ++ show path)

  -- Now run all the commands, which are given as bytestrings
  case cmds of
    Nothing -> putStrLn "No migration being performed"
    Just cmds' -> do
      showCommands cmds'

  putStrLn ("Should I run these commands?")
  ack <- getLine

  if fmap Char.toLower ack /= "yes"
    then fail "Exiting due to user request..."
    else do
      putStrLn "Would run commands"
