module Database.Beam.Migrate.Tool.Diff where

import           Prelude hiding (pred)

import           Database.Beam hiding (timestamp)
import           Database.Beam.Migrate hiding (p)

import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Log
import           Database.Beam.Migrate.Tool.Backend
import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry

import           Control.Exception
import           Control.Monad

import qualified Data.HashSet as HS

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID (UUID)

import           Text.Read

data NoRecordedSchemas = NoRecordedSchemas
  deriving Show
instance Exception NoRecordedSchemas

data CouldNotFetchLog = CouldNotFetchLog DdlError
  deriving Show
instance Exception CouldNotFetchLog

data CouldNotFetchConstraints = CouldNotFetchConstraints DdlError
  deriving Show
instance Exception CouldNotFetchConstraints

data PredicateDiff
  = PredicateDiff
  { predicateDiffExpected :: HS.HashSet SomeDatabasePredicate
  , predicateDiffActual :: HS.HashSet SomeDatabasePredicate
  } deriving Show

predicateDiffMissing, predicateDiffExtra :: PredicateDiff -> HS.HashSet SomeDatabasePredicate
predicateDiffMissing (PredicateDiff expected actual) =
  expected `HS.difference` actual
predicateDiffExtra (PredicateDiff expected actual) =
  actual `HS.difference` expected

genDiff :: MigrateCmdLine -> MigrationRegistry
        -> Text -> Text
        -> IO (ModuleName, PredicateDiff)
genDiff cmdLine reg actualSpec expSpec = do

  actualSource <- parsePredicateFetchSourceSpec cmdLine reg actualSpec
  expSource <- parsePredicateFetchSourceSpec cmdLine reg expSpec

  be <-
    case ( predicateFetchSourceBackend actualSource
         , predicateFetchSourceBackend expSource ) of
      (Nothing, Nothing) ->
        fail "Predicate sources do not specify a backend"
      (Just be, Nothing) -> pure be
      (Nothing, Just be) -> pure be
      (Just be, Just be')
        | be == be' -> pure be
        | otherwise -> fail ("Cannot compare schemas from two different backends: " ++ show (be, be'))

  (be,) <$> genDiffFromSources cmdLine reg (predicateSourceWithBackend be actualSource)
                                           (predicateSourceWithBackend be expSource)

genDiffFromSources :: MigrateCmdLine -> MigrationRegistry
                   -> PredicateFetchSource
                   -> PredicateFetchSource
                   -> IO PredicateDiff
genDiffFromSources cmdLine reg actualSource expSource =
  do (_, actual)   <- getPredicatesFromSpec cmdLine reg actualSource
     (_, expected) <- getPredicatesFromSpec cmdLine reg expSource

     pure (PredicateDiff (HS.fromList expected) (HS.fromList actual))

filterBeamMigratePreds :: SomeBeamMigrationBackend -> [SomeDatabasePredicate] -> [SomeDatabasePredicate]
filterBeamMigratePreds (SomeBeamMigrationBackend (BeamMigrationBackend {} :: BeamMigrationBackend be m)) preds =
  let beamMigrateDbSchema = collectChecks (beamMigratableDb @be @m)
  in foldr (\pred@(SomeDatabasePredicate pred') preds' ->
              if pred `elem` preds'
              then filter (\p@(SomeDatabasePredicate p') -> p /= pred && not (predicateCascadesDropOn p' pred')) preds'
              else preds')
           preds beamMigrateDbSchema

getPredicatesFromSpec :: MigrateCmdLine -> MigrationRegistry
                      -> PredicateFetchSource
                      -> IO (Maybe UUID, [ SomeDatabasePredicate ])
getPredicatesFromSpec _ _ (PredicateFetchSourceCommit Nothing _) = fail "No backend to read commit with"
getPredicatesFromSpec cmdLine reg (PredicateFetchSourceCommit (Just modName) commitId) = do
  SomeBeamMigrationBackend be <- loadBackend' cmdLine modName

  SchemaMetaData _ _ _ (Schema preds) <- readSchemaMetaData reg be commitId

  let applicablePreds = predsForBackend be preds
  pure (Just commitId, applicablePreds)
getPredicatesFromSpec cmdLine reg (PredicateFetchSourceDbHead (MigrationDatabase modName connStr) ref) = do
  be@(SomeBeamMigrationBackend
      (BeamMigrationBackend { backendGetDbConstraints = getCs
                            , backendTransact = transact } ::
          BeamMigrationBackend be m)) <-
    loadBackend' cmdLine modName

  case ref of
    Nothing -> do
      cs <- transact connStr getCs
      case cs of
        Left err -> throwIO (CouldNotFetchConstraints err)
        Right cs' -> pure (Nothing, filterBeamMigratePreds be cs')
    Just fromHead -> do
      logEntry <- transact connStr $
                  runSelectReturningOne $ select $
                  limit_ 1 $ offset_ (fromIntegral fromHead) $
                  orderBy_ (desc_ . _logEntryId) $
                  all_ (_beamMigrateLogEntries (beamMigrateDb @be @m))

      case logEntry of
        Left err -> throwIO (CouldNotFetchLog err)
        Right Nothing -> throwIO NoRecordedSchemas
        Right (Just logEntry') ->
          case readMaybe (T.unpack (_logEntryCommitId logEntry')) of
            Nothing -> throwIO (InvalidCommitId (_logEntryCommitId logEntry'))
            Just commitId ->
              getPredicatesFromSpec cmdLine reg (PredicateFetchSourceCommit (Just modName) commitId)
getPredicatesFromSpec _ _ PredicateFetchSourceEmpty = pure (Nothing, [])

displayDiff :: MigrateCmdLine
            -> Text -> Text
            -> Bool -> IO ()
displayDiff cmdLine expected actual autogen = do
  reg <- lookupRegistry cmdLine

  (backendMod, diff) <- genDiff cmdLine reg actual expected

  let missing = predicateDiffMissing diff
      extra = predicateDiffExtra diff

  if autogen
    then displayScript cmdLine backendMod diff
    else
      if HS.null extra && HS.null missing
      then putStrLn "Schemas match"
      else do
        when (not (HS.null extra)) $ do
          putStrLn "The following constraints are extraneous:"
          forM_ extra $ \(SomeDatabasePredicate p') ->
            putStrLn ("  - " ++ englishDescription p')
        when (not (HS.null missing)) $ do
          putStrLn "The following constraints are missing:"
          forM_ missing $ \(SomeDatabasePredicate p') ->
            putStrLn ("  - " ++ englishDescription p')

displayScript :: MigrateCmdLine -> ModuleName -> PredicateDiff -> IO ()
displayScript cmdLine modName (PredicateDiff dest from) = do
  SomeBeamMigrationBackend BeamMigrationBackend
    { backendActionProvider = actionProvider
    , backendRenderSyntax = renderCmd } <- loadBackend' cmdLine modName

  let solver = heuristicSolver actionProvider (HS.toList from) (HS.toList dest)

  case finalSolution solver of
    Candidates {} -> fail "Could not find appropriate migration between schemas."
    Solved cmds ->
      putStrLn (unlines (map (renderCmd . migrationCommand) cmds))
