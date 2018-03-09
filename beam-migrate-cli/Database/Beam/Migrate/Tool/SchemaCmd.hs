module Database.Beam.Migrate.Tool.SchemaCmd where

import           Prelude hiding (pred)

import           Database.Beam.Migrate.Tool.Backend
import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Diff
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Schema

import           Database.Beam.Migrate ( SomeDatabasePredicate(..)
                                       , DatabasePredicate(..)
                                       , PredicateSpecificity(..) )
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Backend

import           Database.Beam.Haskell.Syntax

import           Control.Exception
import           Control.Monad
import           Control.Monad.State

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as HS
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Time
import           Data.UUID (UUID)
import qualified Data.UUID as UUID (nil)
import qualified Data.Yaml as Yaml

import qualified Language.Haskell.Exts as Hs

import           System.Directory
import           System.FilePath

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
                                                      , backendActionProvider = actionProvider }) -> do
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
                let solver = heuristicSolver actionProvider oldPreds' cs'
                in case finalSolution solver of
                     Candidates {} -> fail "Could not find migration in backend"
                     Solved [] -> fail "Schemas are equivalent, not importing"
                     Solved cmds -> do
                       liftIO . void $ writeMigration cmdLine registry be oldCommitId newCommit cmds
                       modifyM (newMigration oldCommitId hsHash [MigrationFormatHaskell, backendMigFmt])

          -- TODO factor out
          let schema = Schema (map (\pred@(SomeDatabasePredicate (_ :: pred)) -> ( HS.singleton (predicateSpecificity (Proxy @pred)), pred)) cs')

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

dumpSchema :: MigrateCmdLine
           -> ModuleName
           -> String
           -> IO ()
dumpSchema cmdLine backend connStr = do
  backend' <- loadBackend' cmdLine backend
  case backend' of
    SomeBeamMigrationBackend (BeamMigrationBackend { backendGetDbConstraints = getConstraints
                                                   , backendTransact = transact }) -> do
      cs <- transact connStr getConstraints
      case cs of
        Left err -> fail ("Error getting constraints: " ++ show err)
        Right cs' ->
          let sch = Schema (map (\c -> (HS.singleton predSrc, c)) cs')
              predSrc = PredicateSpecificityOnlyBackend (unModuleName backend)
          in BS.putStrLn (Yaml.encode sch)

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
          case finalSolution $ heuristicSolver (defaultActionProvider @HsAction) [] cs'' of
            Candidates {} -> fail "Could not form haskell schema"
            Solved actions ->
              case renderHsSchema (hsActionsToModule "Schema" actions) of
                Left err -> fail ("Could not render schema: " ++ err)
                Right sch -> putStrLn sch

writeSchema :: MigrateCmdLine -> MigrationRegistry
             -> UUID -> BeamMigrationBackend cmd be hdl m
             -> [SomeDatabasePredicate]
             -> IO FilePath
writeSchema cmdLine registry commitId be cs = do
  case finalSolution $ heuristicSolver (backendActionProvider be) [] cs of
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
  case finalSolution $ heuristicSolver (defaultActionProvider @HsAction) [] cs of
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
               -> BeamMigrationBackend cmd be hdl m
               -> UUID -> UUID
               -> [ cmd ]
               -> IO FilePath
writeMigration cmdLine reg be fromId toId cmds =
  case be of
    BeamMigrationBackend { backendRenderSyntax = renderCmd
                         , backendFileExtension = fileExtension } ->
      writeSchemaFile cmdLine reg fileExtension (migrationScriptName fromId toId) $
      unlines (map renderCmd cmds)

-- * Schema new command

renameModule :: String -> Hs.Module a -> Hs.Module a
renameModule modName (Hs.Module l (Just (Hs.ModuleHead l' (Hs.ModuleName l'' _) warning exports))
                                  pragmas imports decls) =
  Hs.Module l (Just (Hs.ModuleHead l' (Hs.ModuleName l'' modName) warning exports))
            pragmas imports decls
renameModule _ _ = error "Could not rename module"

beginNewSchema :: MigrateCmdLine -> Text -> FilePath -> IO ()
beginNewSchema cmdLine tmplSrc tmpFile = do
  updatingRegistry cmdLine $ \reg -> do
    assertRegistryReady reg

    tmpFileExists <- doesFileExist tmpFile
    when tmpFileExists (fail (tmpFile ++ ": already exists... aborting"))

    predSrc <- parsePredicateFetchSourceSpec cmdLine reg tmplSrc
    src <- case predSrc of
             PredicateFetchSourceDbHead {} -> fail "Cannot base schema off database. Import the database first"
             PredicateFetchSourceCommit _ schema -> pure (Just schema)
             PredicateFetchSourceEmpty -> pure Nothing

    let modName = unModuleName (migrationRegistrySchemaModule reg) <> "." <>
                  schemaModuleName UUID.nil

    hsModSrc <-
      case src of
        Nothing ->
          case renderHsSchema (hsActionsToModule modName []) of
            Left err -> fail ("Could not render empty schema: " ++ err)
            Right sch -> pure (BS.pack sch)
        Just srcSchema ->
          case lookupSchema srcSchema [MigrationFormatHaskell] reg of
            Nothing -> fail "Specified schema does not exist"
            Just {} -> do
              let srcSchemaFilePath = migrationRegistrySrcDir reg </> schemaModuleName srcSchema <.> "hs"
              schemaExists <- doesFileExist srcSchemaFilePath
              when (not schemaExists) (fail (show srcSchemaFilePath ++ ": could not find haskell source"))

              -- read schema module
              BS.readFile srcSchemaFilePath

    let noMetadata = BS.unpack (BS.unlines (withoutMetadata (BS.lines hsModSrc)))
        parseMode = Hs.defaultParseMode { Hs.parseFilename = tmpFile
                                        , Hs.extensions = map Hs.EnableExtension $
                                          [ Hs.ExplicitNamespaces, Hs.StandaloneDeriving
                                          , Hs.TypeFamilies, Hs.ExplicitForAll
                                          , Hs.MultiParamTypeClasses, Hs.TypeApplications ] }

    case Hs.parseModuleWithComments parseMode noMetadata of
      Hs.ParseFailed loc err -> fail ("Could not parse schema module: " ++ show loc ++ ": " ++ err)
      Hs.ParseOk (srcMod, comments) -> do
        let srcMod' = renameModule modName srcMod
            modStr = Hs.exactPrint srcMod' comments

            hash = sha256 modStr

        writeFile tmpFile modStr

        putStrLn ("You can now edit the schema at " ++ tmpFile)
        putStrLn "When you are done, run 'beam-migrate schema commit' to commit the schema to the registry."

        pure ((), reg { migrationRegistryMode = BeamMigrateCreatingSchema tmpFile src hash })
