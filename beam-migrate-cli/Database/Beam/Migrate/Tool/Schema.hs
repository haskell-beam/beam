module Database.Beam.Migrate.Tool.Schema where

import           Prelude hiding (pred)

import           Database.Beam (Database)
import           Database.Beam.Migrate.Tool.Backend
import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Diff
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Log

import           Database.Beam.Backend.SQL

import           Database.Beam.Migrate ( SomeDatabasePredicate(..)
                                       , DatabasePredicate(..)
                                       , PredicateSpecificity(..)
                                       , MigrationCommand(..)
                                       , CheckedDatabaseSettings
                                       , collectChecks )
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Backend

import           Database.Beam.Haskell.Syntax

import           Control.Exception
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as HS
import           Data.Maybe
#if !MIN_VERSION_base(4, 11, 0)
import           Data.List
import           Data.Monoid
#endif
import           Data.Proxy
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
import           Data.Typeable
import           Data.UUID (UUID)
import qualified Data.UUID as UUID (nil)
import qualified Data.Yaml as Yaml

import           Language.Haskell.Interpreter hiding (ModuleName, get, set)
import qualified Language.Haskell.Interpreter as GHCI
import qualified Language.Haskell.Exts as Hs

import           System.Directory
import           System.FilePath
import           System.IO

import           Text.Editor

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
    SomeBeamMigrationBackend be@(BeamMigrationBackend { backendConvertToHaskell = HaskellPredicateConverter convDbPred
                                                      , backendTransact = transact
                                                      , backendActionProvider = actionProvider }) -> do
      newCommit <- withRegistry $ \registry -> lift (registryNewCommitId registry)
      let backendHash = newCommit
          hsHash = newCommit

      (_, cs) <- withRegistry $ \registry -> liftIO (getPredicatesFromSpec cmdLine registry (PredicateFetchSourceDbHead db Nothing))

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
            let solver = heuristicSolver actionProvider oldPreds' cs
            in case finalSolution solver of
                 Candidates {} -> fail "Could not find migration in backend"
                 Solved [] -> fail "Schemas are equivalent, not importing"
                 Solved cmds' -> do
                   let cmds = fmap migrationCommand cmds'
                   liftIO . void $ writeMigration cmdLine registry be oldCommitId newCommit cmds
                   modifyM (newMigration oldCommitId hsHash [MigrationFormatHaskell, backendMigFmt])

      -- TODO factor out
      let schema = Schema (map (\pred@(SomeDatabasePredicate (_ :: pred)) -> ( HS.singleton (predicateSpecificity (Proxy @pred)), pred)) cs)

      liftIO $ putStrLn "Exporting migration for backend..."
      backendFileName <- withRegistry $ \registry -> liftIO (writeSchema cmdLine registry backendHash be cs)
      liftIO (putStrLn ("Exported migration as " ++ backendFileName))

      cs' <- lift . fmap catMaybes .
             forM cs $ \c -> do
               let c' = convDbPred c
               case c' of
                 Nothing -> putStrLn ("Tossing constraint " ++ show c)
                 Just {} -> pure ()
               pure c'

      liftIO $ putStrLn "Exporting haskell schema..."
      hsFileName <- withRegistry $ \registry -> liftIO $ writeHsSchema cmdLine registry hsHash cs' schema [ backendMigFmt ]
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
                 -> SchemaKind
                 -> IO ()
showSimpleSchema cmdLine backend connStr schemaKind = do
  backend' <- loadBackend' cmdLine backend
  case backend' of
    SomeBeamMigrationBackend (BeamMigrationBackend { backendGetDbConstraints = getConstraints
                                                   , backendConvertToHaskell = HaskellPredicateConverter convDbPred
                                                   , backendTransact = transact
                                                   , backendActionProvider = actionProvs
                                                   , backendRenderSyntax = renderSyntax }) -> do
      cs <- transact connStr getConstraints
      case cs of
        Left err -> fail ("Error getting constraints: " ++ show err)
        Right cs' ->
          case schemaKind of
            YamlSchema ->
              let sch = Schema (map (\c -> (HS.singleton predSrc, c)) cs')
                  predSrc = PredicateSpecificityOnlyBackend (unModuleName backend)
              in BS.putStrLn (Yaml.encode sch)
            BackendSchema -> do
              case finalSolution $ heuristicSolver actionProvs [] cs' of
                Candidates {} -> fail "Could not form native schema"
                Solved actions ->
                    putStrLn (unlines (map renderSyntax (fmap migrationCommand actions)))
            HsSchema -> do
              cs'' <- fmap catMaybes .
                     forM cs' $ \c -> do
                       let c' = convDbPred c
                       case c' of
                         Nothing -> putStrLn ("Tossing constraint " ++ show c)
                         Just {} -> pure ()
                       pure c'
              case finalSolution $ heuristicSolver (defaultActionProvider @HsMigrateBackend) [] cs'' of
                Candidates {} -> fail "Could not form haskell schema"
                Solved actions ->
                  case renderHsSchema (hsActionsToModule "Schema" (fmap migrationCommand actions)) of
                    Left err -> fail ("Could not render schema: " ++ err)
                    Right sch -> putStrLn sch

writeSchema :: MigrateCmdLine -> MigrationRegistry
             -> UUID -> BeamMigrationBackend be m
             -> [SomeDatabasePredicate]
             -> IO FilePath
writeSchema cmdLine registry commitId be cs = do
  case finalSolution $ heuristicSolver (backendActionProvider be) [] cs of
    Candidates {} -> fail "Could not form backend schema"
    Solved actions ->
      writeSchemaFile cmdLine registry (backendFileExtension be) (schemaScriptName commitId) $
       unlines (map (backendRenderSyntax be) (fmap migrationCommand actions))

writeHsSchema :: MigrateCmdLine -> MigrationRegistry
              -> UUID
              -> [SomeDatabasePredicate]
              -> Schema
              -> [MigrationFormat] -> IO FilePath
writeHsSchema cmdLine registry commitId cs dbSchema fmts =
  case finalSolution $ heuristicSolver (defaultActionProvider @HsMigrateBackend) [] cs of
    Candidates [] -> fail "Could not form haskell schema"
    Candidates (x:_) ->
      let allSolved = dbStateCurrentState x
          allDesired = Map.fromList (map (,()) cs)
          left = allDesired `Map.difference` allSolved
      in putStrLn "Left" >> mapM_ (putStrLn . show . fst) (Map.toList left) >> fail ("Some predicates left ")
    Solved actions -> do
      metadata <- SchemaMetaData <$> pure commitId <*> pure fmts <*> getCurrentTime <*> pure dbSchema
      writeHsSchemaFile cmdLine registry commitId metadata $
        let modName = unModuleName (migrationRegistrySchemaModule registry) <> "." <> schemaModuleName commitId
        in case renderHsSchema (hsActionsToModule modName (fmap migrationCommand actions)) of
             Left err -> error ("Could not render schema: " ++ err)
             Right sch -> sch ++ "\n\n" ++
                          metadataComment "--" metadata

writeHsSchemaFile :: MigrateCmdLine -> MigrationRegistry -> UUID
                  -> SchemaMetaData -> String -> IO FilePath
writeHsSchemaFile cmdLine registry commitId metadata modStr =
    writeSchemaFile cmdLine registry "hs" (schemaModuleName commitId)
                    (modStr ++ "\n\n" ++ metadataComment "--" metadata)

writeMigration :: MigrateCmdLine -> MigrationRegistry
               -> BeamMigrationBackend be m
               -> UUID -> UUID
               -> [ BeamSqlBackendSyntax be ]
               -> IO FilePath
writeMigration cmdLine reg be fromId toId cmds =
  case be of
    BeamMigrationBackend { backendRenderSyntax = renderCmd
                         , backendFileExtension = fileExtension } ->
      writeSchemaFile cmdLine reg fileExtension (migrationScriptName fromId toId) $
      unlines (map renderCmd cmds)

-- * Schema new command

renamedSchemaModule :: FilePath -> String -> String -> IO String
renamedSchemaModule srcFile modName modSrc = do
  let parseMode = Hs.defaultParseMode { Hs.parseFilename = srcFile
                                      , Hs.extensions = map Hs.EnableExtension $
                                        [ Hs.ExplicitNamespaces, Hs.StandaloneDeriving
                                        , Hs.TypeFamilies, Hs.ExplicitForAll
                                        , Hs.MultiParamTypeClasses, Hs.TypeApplications ] }
  case Hs.parseModuleWithComments parseMode modSrc of
    Hs.ParseFailed loc err -> do
      hPutStrLn stderr (show loc ++ ": " ++ err)
      fail "Could not parse schema module"
    Hs.ParseOk (srcMod, comments) ->
      let srcMod' = renameModule modName srcMod
          modStr = Hs.exactPrint srcMod' comments
      in pure modStr

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

    modStr <- renamedSchemaModule tmpFile modName noMetadata
    let hash = sha256 modStr

    putStrLn ("Writing temporary schema based off of commit " ++ show src ++ "...")
    writeFile tmpFile modStr

    putStrLn ("You can now edit the schema at " ++ tmpFile)
    putStrLn "When you are done, run 'beam-migrate schema commit' to commit the schema to the registry."

    pure ((), reg { migrationRegistryMode = BeamMigrateCreatingSchema tmpFile src hash })


loadSchema :: forall be m a
            . ModuleName -> BeamMigrationBackend be m
           -> MigrateCmdLine -> MigrationRegistry -> FilePath
           -> (forall be' db. Database be' db => CheckedDatabaseSettings be' db -> IO a)
           -> IO a
loadSchema backendModule (BeamMigrationBackend {}) cmdLine reg modPath withDb = do
  putStrLn ("Loading module " ++ unModuleName backendModule)
  res <- runBeamInterpreter cmdLine $ do
           loadModules [ modPath ]
           setImportsQ [ ("Database.Beam.Migrate", Just "BeamMigrate")
                       , ("Database.Beam.Migrate.Backend", Nothing)
                       , (unModuleName backendModule, Just "BeamBackend") ]

           let beName = tyConName (typeRepTyCon (typeRep (Proxy @be)))

           setTopLevelModules [ unModuleName (migrationRegistrySchemaModule reg) <> "." <> schemaModuleName UUID.nil ]
           GHCI.set [ languageExtensions := [ TypeApplications ] ]
           interpret ("SomeCheckedDatabaseSettings (BeamMigrate.runMigrationSilenced (migration @BeamBackend." <> beName <> "))") (undefined :: SomeCheckedDatabaseSettings)

  case res of
    Left e -> reportHintError e
    Right (SomeCheckedDatabaseSettings db) ->
      withDb db

ensureCommitMsg :: Maybe Text -> IO Text
ensureCommitMsg Nothing = do
  time <- getCurrentTime
  T.strip . TE.decodeUtf8 <$> runUserEditorDWIM txtTemplate (fromString ("New schema created at " <> show time))
ensureCommitMsg (Just msg) = pure msg

normalizePredicates :: (SomeBeamMigrationBackend, ModuleName, HS.HashSet SomeDatabasePredicate)
                    -> Map.HashMap SomeDatabasePredicate PredicateSpecificity
normalizePredicates (SomeBeamMigrationBackend be, _, preds) =
    let predList = HS.toList preds
        bestPreds =
            fmap (\pred@(SomeDatabasePredicate (p :: p)) ->
                      case predicateSpecificity (Proxy :: Proxy p) of
                        PredicateSpecificityAllBackends ->
                            case be of
                              BeamMigrationBackend { backendConvertToHaskell = HaskellPredicateConverter conv2Hs } ->
                                  fromMaybe (pred, PredicateSpecificityAllBackends) $
                                  do pred'@(SomeDatabasePredicate p') <- conv2Hs pred
                                     guard (serializePredicate p == serializePredicate p')
                                     pure (pred', PredicateSpecificityAllBackends)
                        spec@(PredicateSpecificityOnlyBackend _) -> (pred, spec)) predList
   in Map.fromList bestPreds

commitSchema :: MigrateCmdLine -> Bool -> Bool -> Maybe Text -> IO ()
commitSchema cmdLine force overwrite commitMsg =
  updatingRegistry cmdLine $ \reg -> do
    case migrationRegistryMode reg of
      BeamMigrateCreatingSchema tmpFile src initialHash -> do
        contents <- readFile tmpFile
        let curHash = sha256 contents

        if curHash == initialHash
           then do
             putStrLn "No changes were made to schema, so no commit is being made"
             putStrLn ("We are still at " ++ show (registryHeadCommit reg))

             abortEdits' True reg

           else do
             -- Validate the schema by attempting to load it in hint, and collecting the checks.
             -- Then, diff the checks with the current schema.
             -- If they are the same, output a warning message, but don't do anything
             -- If they are different, then commit the schema to the current branch and make it the HEAD of the current branch


             let allBackends = fmap ModuleName . HS.toList $
                               foldMap (\MigrationDatabase { migrationDbBackend = ModuleName nm } ->
                                            HS.singleton nm) $
                               migrationRegistryDatabases reg

                 predsInBackend backendModule = do
                     someBe@(SomeBeamMigrationBackend be) <- loadBackend' cmdLine backendModule
                     destinationPreds <- loadSchema backendModule be cmdLine reg tmpFile
                                           (\schema -> pure (collectChecks schema))
                     pure (someBe, backendModule, HS.fromList destinationPreds)

             predsInBackends <- mapM predsInBackend allBackends

             let predsDifferentForBackend src' (SomeBeamMigrationBackend be, backendModule, destinationPreds) = do
                     initialPreds <- predsForBackend be . schemaPredicates . schemaMetaDataSchema <$>
                                     readSchemaMetaData reg be src'

                     pure (HS.fromList initialPreds /= destinationPreds)

                 finishSchemaCommit =
                     case migrationRegistryHead reg of
                       MigrationHeadDetached _ ->
                           fail "Cannot commit atop detached branch. Please check out a branch and then commit"
                       MigrationHeadBranch branchNm ->
                           case lookupBranch reg branchNm of
                             Nothing -> fail ("Cannot commit atop non-existent branch: " ++ T.unpack branchNm)
                             Just branch -> do
                               let schemaPreds = foldr (Map.unionWith mappend) mempty $
                                                 fmap (fmap HS.singleton . normalizePredicates) $
                                                 predsInBackends

                                   schemaDesc = Schema $
                                                fmap (\(pred, spec) -> (spec, pred)) $
                                                Map.toList $ schemaPreds

                               commitMsg' <- ensureCommitMsg commitMsg

                               -- Add the schema
                               newSchemaId <- registryNewCommitId reg
                               curTime <- getCurrentTime
                               let newModFullName = unModuleName (migrationRegistrySchemaModule reg) <> "." <> newModName
                                   newModName = schemaModuleName newSchemaId

                                   schemaMetaData = SchemaMetaData newSchemaId [ MigrationFormatHaskell ] curTime schemaDesc

                               newSchemaModStr <- renamedSchemaModule tmpFile newModFullName
                                                    (unlines (withoutMetadata (lines contents)))

                               _ <- writeHsSchemaFile cmdLine reg newSchemaId schemaMetaData newSchemaModStr

                               reg' <- newSchema newSchemaId [MigrationFormatHaskell] commitMsg' reg
                               reg'' <- updateBranch branchNm (branch { migrationBranchCommit = newSchemaId }) reg'
                               abortEdits' True reg''

             schemaChangedSignificantly <-
                 maybe (pure True)
                       (\src' -> anyM (predsDifferentForBackend src') predsInBackends)
                       src
             if not schemaChangedSignificantly && not force
               then if overwrite
                    then finishSchemaCommit
                    else do
                      putStrLn (unlines [ "The schema was edited, but the new schema is identical to the last."
                                        , "beam-migrate has decided to do nothing. Here is what you can do:"
                                        , ""
                                        , "  1. If you intend to update the schema, edit the schema file so that it "
                                        , "     is different than the original."
                                        , ""
                                        , "  2. If you no longer want to add a new schema, use 'beam-migrate abort'."
                                        , ""
                                        , "  3. If you want to commit this schema as a new schema anyway, add the "
                                        , "     '--force' option to the commit command."
                                        , ""
                                        , "  4. If you want to *overwrite* the schema in the given database, add the "
                                        , "    '--overwrite' option to the commit command. This can be dangerous."
                                        , ""
                                        , "For now, the registry is left in-tact, you can still edit the file at"
                                        , "   " ++ tmpFile
                                        , "to make changes."
                                        ])

                      pure ((), reg)
               else finishSchemaCommit

      _ -> fail (mconcat [ "There is no schema to commit. "
                         , "Run 'beam-migrate schema new' to start working on a new schema" ])
