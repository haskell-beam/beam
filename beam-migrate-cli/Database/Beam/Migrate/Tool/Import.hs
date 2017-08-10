module Database.Beam.Migrate.Tool.Import where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Backend

import           Database.Beam.Migrate (SomeDatabasePredicate)
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Backend

import           Database.Beam.Haskell.Syntax

import           Control.Monad

import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Time
import           Data.UUID (UUID)

finalSolutionIO :: Solver HsAction -> IO (FinalSolution HsAction)
finalSolutionIO (ProvideSolution Nothing sts)    = pure $ Candidates sts
finalSolutionIO (ProvideSolution (Just cmds) _)  = pure $ Solved cmds
finalSolutionIO (ChooseActions (DatabaseState st _ cmds) _ actions next) = do
  putStrLn ("Have " ++ show (Seq.length cmds) ++ " commands")
  putStrLn ("Have " ++ show (Map.size st) ++ " left")

  finalSolutionIO (next actions)

importDb :: MigrateCmdLine -> DatabaseName -> Text -> IO ()
importDb cmdLine dbName@(DatabaseName dbNameStr) branchName =
  updatingRegistry cmdLine $ \registry -> do
  (connStr, backendMigFmt, backend) <- loadBackend cmdLine registry dbName

  case backend of
    SomeBeamMigrationBackend be@(BeamMigrationBackend { backendGetDbConstraints = getConstraints
                                                      , backendConvertToHaskell = HaskellPredicateConverter convDbPred }) -> do
      cs <- getConstraints connStr
      let backendHash = hashToUUID cs

      case lookupMigration backendHash backendMigFmt registry of
        Just {} -> putStrLn ("Backend migration exists at " ++ show backendHash)
        Nothing -> do
          putStrLn "Exporting migration for backend..."
          fileName <- writeMigration cmdLine registry backendHash be cs
          putStrLn ("Exported migration as " ++ fileName)

      cs'' <- fmap catMaybes .
              forM cs $ \c -> do
                let c' = convDbPred c
                case c' of
                  Nothing -> putStrLn ("Tossing constraint " ++ show c)
                  Just {} -> pure ()
                pure c'
      let hsHash = hashToUUID cs''

      case lookupMigration hsHash MigrationFormatHaskell registry of
        Just {} -> putStrLn ("Haskell migration exists at " ++ show hsHash)
        Nothing -> do
          putStrLn "Exporting haskell schema..."
          fileName <- writeHsMigration cmdLine registry hsHash cs'' [ (backendMigFmt, backendHash) ]
          putStrLn ("Exported haskell migration as " ++ fileName)

      putStrLn "Import complete"

      let msg = "Initial import of " <> fromString dbNameStr

      newBaseBranch branchName hsHash =<<
        newMigration hsHash MigrationFormatHaskell msg [] =<<
        newMigration backendHash backendMigFmt msg [] registry

showSimpleSchema :: MigrateCmdLine
                 -> ModuleName
                 -> String
                 -> IO ()
showSimpleSchema cmdLine backend connStr = do
  backend' <- loadBackend' cmdLine backend
  case backend' of
    SomeBeamMigrationBackend (BeamMigrationBackend { backendGetDbConstraints = getConstraints
                                                   , backendConvertToHaskell = HaskellPredicateConverter convDbPred }) -> do
      cs <- getConstraints connStr
      cs' <- fmap catMaybes .
             forM cs $ \c -> do
               let c' = convDbPred c
               case c' of
                 Nothing -> putStrLn ("Tossing constraint " ++ show c)
                 Just {} -> pure ()
               pure c'
      case finalSolution $ heuristicSolver (defaultActionProviders @HsAction) [] cs' of
        Candidates {} -> fail "Could not form haskell schema"
        Solved actions ->
          case renderHsSchema (hsActionsToModule "Schema" actions) of
            Left err -> fail ("Could not render schema: " ++ err)
            Right sch -> putStrLn sch

writeMigration :: MigrateCmdLine -> MigrationRegistry
               -> UUID -> BeamMigrationBackend be cmd
               -> [SomeDatabasePredicate]
               -> IO FilePath
writeMigration cmdLine registry commitId be cs = do
  case finalSolution $ heuristicSolver (backendActionProviders be) [] cs of
    Candidates {} -> fail "Could not form backend schema"
    Solved actions ->
      writeMigrationFile cmdLine registry (backendFileExtension be) (commitScriptName commitId) $
       unlines (map (backendRenderSyntax be) actions)

writeHsMigration :: MigrateCmdLine -> MigrationRegistry -> UUID -> [SomeDatabasePredicate]
                 -> [(MigrationFormat, UUID)] -> IO FilePath
writeHsMigration cmdLine registry commitId cs fmts =
  case finalSolution $ heuristicSolver (defaultActionProviders @HsAction) [] cs of
    Candidates [] -> fail "Could not form haskell schema"
    Candidates (x:_) ->
      let allSolved = dbStateCurrentState x
          allDesired = Map.fromList (map (,()) cs)
          left = allDesired `Map.difference` allSolved
      in putStrLn "Left" >> mapM_ (putStrLn . show . fst) (Map.toList left) >> fail ("Some predicates left ")
    Solved actions -> do
      metadata <- MigrationMetaData <$> pure commitId <*> pure fmts <*> getCurrentTime
      writeMigrationFile cmdLine registry "hs" (commitModuleName commitId) $
        let modName = unModuleName (migrationRegistrySchemaModule registry) <> "." <> commitModuleName commitId
        in case renderHsSchema (hsActionsToModule modName actions) of
             Left err -> error ("Could not render schema: " ++ err)
             Right sch -> sch ++ "\n\n" ++
                          metadataComment "--" metadata
