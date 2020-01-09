{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Utility functions for common use cases
module Database.Beam.Migrate.Simple
  ( autoMigrate
  , planMigration
  , describeMigrationPlan
  , executeMigrationPlan
  , autoMigrateVerbose
  , simpleSchema
  , simpleMigration
  , runSimpleMigration
  , backendMigrationScript

  , VerificationResult(..)
  , verifySchema

  , createSchema

  , BringUpToDateHooks(..)
  , defaultUpToDateHooks
  , bringUpToDate, bringUpToDateWithHooks

  , haskellSchema

  , module Database.Beam.Migrate.Actions
  , module Database.Beam.Migrate.Types ) where

import           Prelude hiding (log)

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Haskell.Syntax
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Checks (HasDataTypeCreatedCheck)
import           Database.Beam.Migrate.Log
import           Database.Beam.Migrate.SQL (BeamMigrateSqlBackendDataTypeSyntax)
import           Database.Beam.Migrate.Types

import           Control.Monad.Cont
import           Control.Monad.Writer
import           Control.Monad.State

import qualified Data.HashSet as HS
import           Data.Semigroup (Max(..))
import qualified Data.Text as T

import qualified Control.Monad.Fail as Fail

data BringUpToDateHooks m
  = BringUpToDateHooks
  { runIrreversibleHook :: m Bool
    -- ^ Called before we're about to run an irreversible migration step. Return
    -- 'True' to run the step, or 'False' to abort immediately.
  , startStepHook       :: Int -> T.Text -> m ()
    -- ^ Called at the beginning of each step with the step index and description
  , endStepHook         :: Int -> T.Text -> m ()
    -- ^ Called at the end of each step with the step index and description
  , runCommandHook      :: Int -> String -> m ()
    -- ^ Called before a command is about to run. The first argument is the step
    -- index and the second is a string representing the command about to be run.

  , queryFailedHook     :: m ()
    -- ^ Called when a query fails
  , discontinuousMigrationsHook
                        :: Int -> m ()
    -- ^ Called when the migration log has a discontinuity at the supplied index
  , logMismatchHook     :: Int -> T.Text -> T.Text -> m ()
    -- ^ The migration log at the given index is not what was expected. The
    -- first text is the actual commit id, the second, the expected
  , databaseAheadHook   :: Int -> m ()
    -- ^ The database is ahead of the given migrations. The parameter supplies
    -- the number of entries passed the given migrations the database has.
  }

-- | Default set of 'BringUpToDateHooks'. Refuses to run irreversible
-- migrations, and fails in case of error, using 'fail'.
defaultUpToDateHooks :: Fail.MonadFail m => BringUpToDateHooks m
defaultUpToDateHooks =
  BringUpToDateHooks
  { runIrreversibleHook = pure False
  , startStepHook       = \_ _ -> pure ()
  , endStepHook         = \_ _ -> pure ()
  , runCommandHook      = \_ _ -> pure ()
  , queryFailedHook     = Fail.fail "Log entry query fails"
  , discontinuousMigrationsHook =
      \ix -> Fail.fail ("Discontinuous migration log: missing migration at " ++ show ix)
  , logMismatchHook =
      \ix actual expected ->
        Fail.fail ("Log mismatch at index " ++ show ix ++ ":\n" ++
              "  expected: " ++ T.unpack expected ++ "\n" ++
              "  actual  : " ++ T.unpack actual)
  , databaseAheadHook =
      \aheadBy ->
        Fail.fail ("The database is ahead of the known schema by " ++ show aheadBy ++ " migration(s)")
  }

-- | Equivalent to calling 'bringUpToDateWithHooks' with 'defaultUpToDateHooks'.
--
-- Tries to bring the database up to date, using the database log and the given
-- 'MigrationSteps'. Fails if the migration is irreversible, or an error occurs.
bringUpToDate :: ( Database be db, Fail.MonadFail m
                 , HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be) )
              => BeamMigrationBackend be m
              -> MigrationSteps be () (CheckedDatabaseSettings be db)
              -> m (Maybe (CheckedDatabaseSettings be db))
bringUpToDate be@BeamMigrationBackend {} =
  bringUpToDateWithHooks defaultUpToDateHooks be

-- | Check for the beam-migrate log. If it exists, use it and the supplied
-- migrations to bring the database up-to-date. Otherwise, create the log and
-- run all migrations.
--
-- Accepts a set of hooks that can be used to customize behavior. See the
-- documentation for 'BringUpToDateHooks' for more information. Calling this
-- with 'defaultUpToDateHooks' is the same as using 'bringUpToDate'.
bringUpToDateWithHooks :: forall db be m
                        . ( Database be db, Fail.MonadFail m
                          , HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be) )
                       => BringUpToDateHooks m
                       -> BeamMigrationBackend be m
                       -> MigrationSteps be () (CheckedDatabaseSettings be db)
                       -> m (Maybe (CheckedDatabaseSettings be db))
bringUpToDateWithHooks hooks be@(BeamMigrationBackend { backendRenderSyntax = renderSyntax' }) steps = do
  ensureBackendTables be

  entries <- runSelectReturningList $ select $
             all_ (_beamMigrateLogEntries (beamMigrateDb @be @m))
  let verifyMigration :: Int -> T.Text -> Migration be a -> StateT [LogEntry] (WriterT (Max Int) m) a
      verifyMigration stepIx stepNm step =
        do log <- get
           case log of
             [] -> pure ()
             LogEntry actId actStepNm _:log'
               | actId == stepIx && actStepNm == stepNm ->
                   tell (Max stepIx) >> put log'
               | actId /= stepIx ->
                   lift . lift $ discontinuousMigrationsHook hooks stepIx
               | otherwise ->
                   lift . lift $ logMismatchHook hooks stepIx actStepNm stepNm
           executeMigration (\_ -> pure ()) step

  (futureEntries, Max lastCommit) <-
    runWriterT (execStateT (runMigrationSteps 0 Nothing steps verifyMigration) entries <*
                tell (Max (-1)))

  case futureEntries of
    _:_ -> databaseAheadHook hooks (length futureEntries)
    [] -> pure ()

  -- Check data loss
  shouldRunMigration <-
    flip runContT (\_ -> pure True) $
    runMigrationSteps (lastCommit + 1) Nothing steps
      (\_ _ step -> do
          case migrationDataLoss step of
            MigrationLosesData ->
              ContT $ \_ -> runIrreversibleHook hooks
            MigrationKeepsData ->
              executeMigration (\_ -> pure ()) step)

  if shouldRunMigration
    then Just <$>
         runMigrationSteps (lastCommit + 1) Nothing steps
           (\stepIx stepName step ->
              do startStepHook hooks stepIx stepName
                 ret <-
                   executeMigration
                     (\cmd -> do
                         runCommandHook hooks stepIx (renderSyntax' cmd)
                         runNoReturn cmd)
                     step

                 runInsert $ insert (_beamMigrateLogEntries (beamMigrateDb @be @m)) $
                   insertExpressions [ LogEntry (val_ stepIx) (val_ stepName) currentTimestamp_ ]
                 endStepHook hooks stepIx stepName

                 return ret)
    else pure Nothing

-- | Attempt to find a SQL schema given an 'ActionProvider' and a checked
-- database. Returns 'Nothing' if no schema could be found, which usually means
-- you have chosen the wrong 'ActionProvider', or the backend you're using is
-- buggy.
simpleSchema :: Database be db
             => ActionProvider be
             -> CheckedDatabaseSettings be db
             -> Maybe [BeamSqlBackendSyntax be]
simpleSchema provider settings =
  let allChecks = collectChecks settings
      solver    = heuristicSolver provider [] allChecks
  in case finalSolution solver of
       Solved cmds -> Just (fmap migrationCommand cmds)
       Candidates {} -> Nothing

-- | Given a 'CheckedDatabaseSettings' and a 'BeamMigrationBackend',
-- attempt to create the schema from scratch in the current database.
--
-- May 'fail' if we cannot find a schema
createSchema :: (Database be db, Fail.MonadFail m)
             => BeamMigrationBackend be m
             -> CheckedDatabaseSettings be db
             -> m ()
createSchema BeamMigrationBackend { backendActionProvider = actions } db =
  case simpleSchema actions db of
    Nothing -> Fail.fail "createSchema: Could not determine schema"
    Just cmds ->
        mapM_ runNoReturn cmds

-- | Given a 'BeamMigrationBackend', attempt to automatically bring the current
-- database up-to-date with the given 'CheckedDatabaseSettings'. Fails (via
-- 'fail') if this involves an irreversible migration (one that may result in
-- data loss).
autoMigrate :: (Database be db, Fail.MonadFail m)
            => BeamMigrationBackend be m
            -> CheckedDatabaseSettings be db
            -> m ()
autoMigrate b@(BeamMigrationBackend {})
            db =
  do cmds <- planMigration b db
     executeMigrationPlan b cmds

planMigration :: (Database be db, Fail.MonadFail m)
              => BeamMigrationBackend be m
              -> CheckedDatabaseSettings be db
              -> m [MigrationCommand be]
planMigration BeamMigrationBackend { backendActionProvider = actions
                                   , backendGetDbConstraints = getCs }
              db =
  do actual <- getCs
     let expected = collectChecks db
     case finalSolution (heuristicSolver actions actual expected) of
       Candidates {} -> Fail.fail "planMigration: Could not determine migration"
       Solved cmds -> return cmds

describeMigrationPlan :: Sql92DisplaySyntax (BeamSqlBackendSyntax be)
                      => [MigrationCommand be]
                      -> [String]
describeMigrationPlan = map $ \cmd -> safety cmd <> displaySyntax (migrationCommand cmd)
  where
    safety cmd = case migrationCommandDataLossPossible cmd of
      MigrationLosesData -> "<UNSAFE> "
      MigrationKeepsData -> "         "

executeMigrationPlan :: Fail.MonadFail m
                     => BeamMigrationBackend be m
                     -> [MigrationCommand be]
                     -> m ()
executeMigrationPlan (BeamMigrationBackend {}) cmds =
     -- Check if any of the commands are irreversible
     case foldMap migrationCommandDataLossPossible cmds of
       MigrationKeepsData -> mapM_ (runNoReturn . migrationCommand) cmds
       _ -> Fail.fail "executeMigrationPlan: Not performing automatic migration due to data loss"

autoMigrateVerbose :: ( Database be db, Sql92DisplaySyntax (BeamSqlBackendSyntax be), MonadIO m
                      , Fail.MonadFail m
                      )
                   => BeamMigrationBackend be m
                   -> CheckedDatabaseSettings be db
                   -> m ()
autoMigrateVerbose b@(BeamMigrationBackend {})
                   db =
  do cmds <- planMigration b db
     liftIO $ putStr $ unlines $ describeMigrationPlan cmds
     executeMigrationPlan b cmds

-- | Given a migration backend, a handle to a database, and a checked database,
-- attempt to find a schema. This should always return 'Just', unless the
-- backend has incomplete migrations support.
--
-- 'BeamMigrationBackend's can usually be found in a module named
-- @Database.Beam.<Backend>.Migrate@ with the name@migrationBackend@
simpleMigration :: ( MonadBeam be m
                 ,   Database be db )
                => (forall a. handle -> m a -> IO a)
                -> BeamMigrationBackend be m
                -> handle
                -> CheckedDatabaseSettings be db
                -> IO (Maybe [BeamSqlBackendSyntax be])
simpleMigration runner BeamMigrationBackend { backendGetDbConstraints = getCs
                                            , backendActionProvider = action } hdl db = do
  pre <- runner hdl getCs

  let post = collectChecks db
      solver = heuristicSolver action pre post

  case finalSolution solver of
    Solved cmds -> pure (Just (fmap migrationCommand cmds))
    Candidates {} -> pure Nothing

-- | Result type for 'verifySchema'
data VerificationResult
  = VerificationSucceeded
  | VerificationFailed [SomeDatabasePredicate]
  deriving Show

-- | Verify that the given, beam database matches the actual
-- schema. On success, returns 'VerificationSucceeded', on failure,
-- returns 'VerificationFailed' and a list of missing predicates.
verifySchema :: ( Database be db, MonadBeam be m )
             => BeamMigrationBackend be m
             -> CheckedDatabaseSettings be db
             -> m VerificationResult
verifySchema BeamMigrationBackend { backendGetDbConstraints = getConstraints } db =
  do actualSchema <- HS.fromList <$> getConstraints
     let expectedSchema = HS.fromList (collectChecks db)
         missingPredicates = expectedSchema `HS.difference` actualSchema
     if HS.null missingPredicates
       then pure VerificationSucceeded
       else pure (VerificationFailed (HS.toList missingPredicates))

-- | Run a sequence of commands on a database

runSimpleMigration :: MonadBeam be m
                   => (forall a. hdl -> m a -> IO a)
                   -> hdl -> [BeamSqlBackendSyntax be] -> IO ()
runSimpleMigration runner hdl =
  runner hdl . mapM_ runNoReturn

-- | Given a function to convert a command to a 'String', produce a script that
-- will execute the given migration. Usually, the function you provide
-- eventually calls 'displaySyntax' to rendere the command.
backendMigrationScript :: BeamSqlBackend be
                       => (BeamSqlBackendSyntax be -> String)
                       -> Migration be a
                       -> String
backendMigrationScript render mig =
  migrateScript ((++"\n") . T.unpack) ((++"\n") . render) (migrationStep "Migration Script" (\() -> mig))

-- | Given a 'BeamMigrationBackend', get a string representing a Haskell module
-- that would be a good starting point for further development.
--
-- For example, for a postgres database named @chinook@
--
-- > import Database.Beam.Migrate.Simple
-- > import Database.Beam.Postgres (runBeamPostgres)
-- > import Database.Beam.Postgres.Migrate (migrationBackend)
-- > import Database.PostgreSQL.Simple
-- >
-- > getSchema :: IO String
-- > getSchema = do pg <- connectPostgreSQL
-- >                runBeamPostgres pg (haskellSchema migrationBackend)
--
-- Backends that have a migration backend typically export it under the module
-- name @Database.Beam./Backend/.Migrate@.
haskellSchema :: (MonadBeam be m, Fail.MonadFail m)
              => BeamMigrationBackend be m
              -> m String
haskellSchema BeamMigrationBackend { backendGetDbConstraints = getCs
                                   , backendConvertToHaskell = HaskellPredicateConverter conv2Hs } = do
  constraints <- getCs
  let hsConstraints = [ hsConstraint | c <- constraints, Just hsConstraint <- [ conv2Hs c ] ]

      solver = heuristicSolver (defaultActionProvider @HsMigrateBackend) [] hsConstraints

  case finalSolution solver of
    Solved cmds   ->
      let hsModule = hsActionsToModule "NewBeamSchema" (map migrationCommand cmds)
      in case renderHsSchema hsModule of
           Left err -> Fail.fail ("Error writing Haskell schema: " ++ err)
           Right modStr -> pure modStr
    Candidates {} -> Fail.fail "Could not form Haskell schema"
