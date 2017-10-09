module Database.Beam.Migrate.Tool.SchemaCmd where

import           Prelude hiding (pred)

import           Database.Beam.Migrate.Tool.Backend
import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Diff
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Schema

import           Database.Beam.Migrate (SomeDatabasePredicate(..), DatabasePredicate(..))
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Backend

import           Database.Beam.Haskell.Syntax

import           Control.Exception
import           Control.Monad
import           Control.Monad.State

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as HS
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
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

modifyM :: Functor m => (s -> m s) -> StateT s m ()
modifyM f = StateT $ fmap ((),) . f

withRegistry :: Monad m => (MigrationRegistry -> StateT MigrationRegistry m a) -> StateT MigrationRegistry m a
withRegistry f = get >>= f

importDb :: MigrateCmdLine -> DatabaseName -> Maybe Text -> Bool -> Bool -> IO ()
importDb cmdLine dbName@(DatabaseName dbNameStr) branchName doDbCommit autoCreateMigration =
  updatingRegistry cmdLine . runStateT $ do
  branchName' <- case branchName of
                   Nothing -> do
                     regHead <- gets migrationRegistryHead
                     case regHead of
                       MigrationHeadDetached {} -> fail "Cannot import into detached HEAD"
                       MigrationHeadBranch nm -> pure nm
                   Just nm -> pure nm

  (db, backendMigFmt, backend) <- withRegistry $ \registry -> lift $ loadBackend cmdLine registry dbName

  case backend of
    SomeBeamMigrationBackend be@(BeamMigrationBackend { backendGetDbConstraints = getConstraints
                                                      , backendConvertToHaskell = HaskellPredicateConverter convDbPred
                                                      , backendTransact = transact
                                                      , backendActionProviders = actionProviders }) -> do
      newCommit <- withRegistry $ \registry -> lift (registryNewCommitId registry)
      let backendHash = newCommit
          hsHash = newCommit

      cs <- lift (transact (migrationDbConnString db) getConstraints)

      case cs of
        Left err -> fail ("Error getting constraints from database: " ++ show err)
        Right cs' -> do

          when autoCreateMigration $ do
            registry <- get
            oldPreds <- liftIO ((Just <$> getPredicatesFromSpec cmdLine registry
                                        (PredicateFetchSourceDbHead db (Just 0)))
                                `catch` (\NoRecordedSchemas -> pure Nothing))

            -- Generate haskell and backend migration
            case oldPreds of
              Nothing -> liftIO $ putStrLn "Skipping migration generation, since this is an initial import"
              Just (Nothing, _) -> fail "No commit for DB^"
              Just (Just oldCommitId, oldPreds') ->
                let solver = heuristicSolver actionProviders oldPreds' cs'
                in case finalSolution solver of
                     Candidates {} -> fail "Could not find migration in backend"
                     Solved [] -> fail "Schemas are equivalent, not importing"
                     Solved cmds -> do
                       liftIO . void $ writeMigration cmdLine registry be oldCommitId newCommit cmds
                       modifyM (newMigration oldCommitId hsHash [MigrationFormatHaskell, backendMigFmt])

          -- TODO factor out
          let schema = Schema (map (\pred@(SomeDatabasePredicate (_ :: pred)) -> ( HS.singleton (predicateSource (Proxy @pred)), pred)) cs')

          liftIO $ putStrLn "Exporting migration for backend..."
          backendFileName <- withRegistry $ \registry -> liftIO (writeSchema cmdLine registry backendHash be cs')
          liftIO (putStrLn ("Exported migration as " ++ backendFileName))

          cs''' <- lift . fmap catMaybes .
                   forM cs' $ \c -> do
                     let c' = convDbPred c
                     case c' of
                       Nothing -> putStrLn ("Tossing constraint " ++ show c)
                       Just {} -> pure ()
                     pure c'

          liftIO $ putStrLn "Exporting haskell schema..."
          hsFileName <- withRegistry $ \registry -> liftIO $ writeHsSchema cmdLine registry hsHash cs''' schema [ backendMigFmt ]
          liftIO $ putStrLn ("Exported haskell migration as " ++ hsFileName)

          liftIO $ putStrLn "Import complete"

          let msg = "Initial import of " <> fromString dbNameStr

          when doDbCommit $
            liftIO $ reportDdlErrors $ transact (migrationDbConnString db) $ recordCommit newCommit

          modifyM (newSchema hsHash [MigrationFormatHaskell, backendMigFmt] msg)

          modifyM (\registry ->
                     case lookupBranch registry branchName' of
                       Nothing ->
                         newBaseBranch branchName' hsHash registry
                       Just branch ->
                         updateBranch branchName' (branch { migrationBranchCommit = hsHash }) registry)

showSimpleSchema :: MigrateCmdLine
                 -> ModuleName
                 -> String
                 -> IO ()
showSimpleSchema cmdLine backend connStr = do
  backend' <- loadBackend' cmdLine backend
  case backend' of
    SomeBeamMigrationBackend (BeamMigrationBackend { backendGetDbConstraints = getConstraints
                                                   , backendConvertToHaskell = HaskellPredicateConverter convDbPred
                                                   , backendTransact = transact }) -> do
      cs <- transact connStr getConstraints
      case cs of
        Left err -> fail ("Error getting constraints: " ++ show err)
        Right cs' -> do
          cs'' <- fmap catMaybes .
                 forM cs' $ \c -> do
                   let c' = convDbPred c
                   case c' of
                     Nothing -> putStrLn ("Tossing constraint " ++ show c)
                     Just {} -> pure ()
                   pure c'
          case finalSolution $ heuristicSolver (defaultActionProviders @HsAction) [] cs'' of
            Candidates {} -> fail "Could not form haskell schema"
            Solved actions ->
              case renderHsSchema (hsActionsToModule "Schema" actions) of
                Left err -> fail ("Could not render schema: " ++ err)
                Right sch -> putStrLn sch

writeSchema :: MigrateCmdLine -> MigrationRegistry
             -> UUID -> BeamMigrationBackend be cmd hdl
             -> [SomeDatabasePredicate]
             -> IO FilePath
writeSchema cmdLine registry commitId be cs = do
  case finalSolution $ heuristicSolver (backendActionProviders be) [] cs of
    Candidates {} -> fail "Could not form backend schema"
    Solved actions ->
      writeSchemaFile cmdLine registry (backendFileExtension be) (schemaScriptName commitId) $
       unlines (map (backendRenderSyntax be) actions)

writeHsSchema :: MigrateCmdLine -> MigrationRegistry
              -> UUID
              -> [SomeDatabasePredicate]
              -> Schema
              -> [MigrationFormat] -> IO FilePath
writeHsSchema cmdLine registry commitId cs dbSchema fmts =
  case finalSolution $ heuristicSolver (defaultActionProviders @HsAction) [] cs of
    Candidates [] -> fail "Could not form haskell schema"
    Candidates (x:_) ->
      let allSolved = dbStateCurrentState x
          allDesired = Map.fromList (map (,()) cs)
          left = allDesired `Map.difference` allSolved
      in putStrLn "Left" >> mapM_ (putStrLn . show . fst) (Map.toList left) >> fail ("Some predicates left ")
    Solved actions -> do
      metadata <- SchemaMetaData <$> pure commitId <*> pure fmts <*> getCurrentTime <*> pure dbSchema
      writeSchemaFile cmdLine registry "hs" (schemaModuleName commitId) $
        let modName = unModuleName (migrationRegistrySchemaModule registry) <> "." <> schemaModuleName commitId
        in case renderHsSchema (hsActionsToModule modName actions) of
             Left err -> error ("Could not render schema: " ++ err)
             Right sch -> sch ++ "\n\n" ++
                          metadataComment "--" metadata

writeMigration :: MigrateCmdLine -> MigrationRegistry
               -> BeamMigrationBackend be cmd hdl
               -> UUID -> UUID
               -> [ cmd ]
               -> IO FilePath
writeMigration cmdLine reg be fromId toId cmds =
  case be of
    BeamMigrationBackend { backendRenderSyntax = renderCmd
                         , backendFileExtension = fileExtension } ->
      writeSchemaFile cmdLine reg fileExtension (migrationScriptName fromId toId) $
      unlines (map renderCmd cmds)
