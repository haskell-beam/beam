
-- | Utility functions for common use cases
module Database.Beam.Migrate.Simple where
  -- ( autoMigrate
  -- , simpleSchema
  -- , simpleMigration
  -- , runSimpleMigration
  -- , backendMigrationScript

  -- , VerificationResult(..)
  -- , verifySchema

  -- , createSchema

  -- , BringUpToDateHooks(..)
  -- , defaultUpToDateHooks
  -- , bringUpToDate, bringUpToDateWithHooks

  -- , module Database.Beam.Migrate.Actions
  -- , module Database.Beam.Migrate.Types ) where

import           Prelude hiding (log)

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Types
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Log

import           Control.Monad.Cont
import           Control.Monad.Writer
import           Control.Monad.State

import qualified Data.HashSet as HS
import           Data.Semigroup (Max(..))
import qualified Data.Text as T

-- data BringUpToDateHooks m
--   = BringUpToDateHooks
--   { runIrreversibleHook :: m Bool
--   , startStepHook       :: Int -> T.Text -> m ()
--   , endStepHook         :: Int -> T.Text -> m ()
--   , runCommandHook      :: Int -> String -> m ()

--   , queryFailedHook     :: m ()
--     -- ^ Called when a query fails
--   , discontinuousMigrationsHook
--                         :: Int -> m ()
--     -- ^ Called when the migration log has a discontinuity at the supplied index
--   , logMismatchHook     :: Int -> T.Text -> T.Text -> m ()
--     -- ^ The migration log at the given index is not what was expected. The
--     -- first text is the actual commit id, the second, the expected
--   , databaseAheadHook   :: Int -> m ()
--     -- ^ The database is ahead of the given migrations. The parameter supplies
--     -- the number of entries passed the given migrations the database has.
--   }

-- defaultUpToDateHooks :: Monad m => BringUpToDateHooks m
-- defaultUpToDateHooks =
--   BringUpToDateHooks
--   { runIrreversibleHook = pure False
--   , startStepHook       = \_ _ -> pure ()
--   , endStepHook         = \_ _ -> pure ()
--   , runCommandHook      = \_ _ -> pure ()
--   , queryFailedHook     = fail "Log entry query fails"
--   , discontinuousMigrationsHook =
--       \ix -> fail ("Discontinuous migration log: missing migration at " ++ show ix)
--   , logMismatchHook =
--       \ix actual expected ->
--         fail ("Log mismatch at index " ++ show ix ++ ":\n" ++
--               "  expected: " ++ T.unpack expected ++ "\n" ++
--               "  actual  : " ++ T.unpack actual)
--   , databaseAheadHook =
--       \aheadBy ->
--         fail ("The database is ahead of the known schema by " ++ show aheadBy ++ " migration(s)")
--   }

-- -- | Check for the beam-migrate log. If it exists, use it and the supplied
-- -- migrations to bring the database up-to-date. Otherwise, create the log and
-- -- run all migrations.
-- bringUpToDate :: Database be db
--               => BeamMigrationBackend cmd be hdl m
--               -> MigrationSteps cmd () (CheckedDatabaseSettings be db)
--               -> m (Maybe (CheckedDatabaseSettings be db))
-- bringUpToDate be@BeamMigrationBackend {} =
--   bringUpToDateWithHooks defaultUpToDateHooks be

-- bringUpToDateWithHooks :: forall db cmd be hdl m
--                         . Database be db
--                        => BringUpToDateHooks m
--                        -> BeamMigrationBackend cmd be hdl m
--                        -> MigrationSteps cmd () (CheckedDatabaseSettings be db)
--                        -> m (Maybe (CheckedDatabaseSettings be db))
-- bringUpToDateWithHooks hooks be@(BeamMigrationBackend { backendRenderSyntax = renderSyntax' }) steps = do
--   ensureBackendTables be

--   entries <- runSelectReturningList $ select $
--              all_ (_beamMigrateLogEntries (beamMigrateDb @be @cmd))
--   let verifyMigration :: Int -> T.Text -> Migration cmd a -> StateT [LogEntry] (WriterT (Max Int) m) a
--       verifyMigration stepIx stepNm step =
--         do log <- get
--            case log of
--              [] -> pure ()
--              LogEntry actId actStepNm _:log'
--                | actId == stepIx && actStepNm == stepNm ->
--                    tell (Max stepIx) >> put log'
--                | actId /= stepIx ->
--                    lift . lift $ discontinuousMigrationsHook hooks stepIx
--                | otherwise ->
--                    lift . lift $ logMismatchHook hooks stepIx actStepNm stepNm
--            executeMigration (\_ -> pure ()) step

--   (futureEntries, Max lastCommit) <-
--     runWriterT (execStateT (runMigrationSteps 0 Nothing steps verifyMigration) entries <*
--                 tell (Max (-1)))

--   case futureEntries of
--     _:_ -> databaseAheadHook hooks (length futureEntries)
--     [] -> pure ()

--   -- Check data loss
--   shouldRunMigration <-
--     flip runContT (\_ -> pure True) $
--     runMigrationSteps (lastCommit + 1) Nothing steps
--       (\_ _ step -> do
--           case migrationDataLoss step of
--             MigrationLosesData ->
--               ContT $ \_ -> runIrreversibleHook hooks
--             MigrationKeepsData ->
--               executeMigration (\_ -> pure ()) step)

--   if shouldRunMigration
--     then Just <$>
--          runMigrationSteps (lastCommit + 1) Nothing steps
--            (\stepIx stepName step ->
--               do startStepHook hooks stepIx stepName
--                  ret <-
--                    executeMigration
--                      (\cmd -> do
--                          runCommandHook hooks stepIx (renderSyntax' cmd)
--                          runNoReturn cmd)
--                      step

--                  runInsert $ insert (_beamMigrateLogEntries (beamMigrateDb @be @cmd)) $
--                    insertExpressions [ LogEntry (val_ stepIx) (val_ stepName) currentTimestamp_ ]
--                  endStepHook hooks stepIx stepName

--                  return ret)
--     else pure Nothing

-- -- | Attempt to find a SQL schema given an 'ActionProvider' and a checked
-- -- database. Returns 'Nothing' if no schema could be found, which usually means
-- -- you have chosen the wrong 'ActionProvider', or the backend you're using is
-- -- buggy.
-- simpleSchema :: Database be db
--              => ActionProvider cmd
--              -> CheckedDatabaseSettings be db
--              -> Maybe [cmd]
-- simpleSchema provider settings =
--   let allChecks = collectChecks settings
--       solver    = heuristicSolver provider [] allChecks
--   in case finalSolution solver of
--        Solved cmds -> Just (fmap migrationCommand cmds)
--        Candidates {} -> Nothing

-- -- | Given a 'CheckedDatabaseSettings' and a 'BeamMigrationBackend',
-- -- attempt to create the schema from scratch in the current database.
-- --
-- -- May 'fail' if we cannot find a schema
-- createSchema :: Database be db
--              => BeamMigrationBackend cmd be hdl m
--              -> CheckedDatabaseSettings be db
--              -> m ()
-- createSchema BeamMigrationBackend { backendActionProvider = actions } db =
--   case simpleSchema actions db of
--     Nothing -> fail "createSchema: Could not determine schema"
--     Just cmds ->
--         mapM_ runNoReturn cmds

-- autoMigrate :: Database be db
--             => BeamMigrationBackend cmd be hdl m
--             -> CheckedDatabaseSettings be db
--             -> m ()
-- autoMigrate BeamMigrationBackend { backendActionProvider = actions
--                                  , backendGetDbConstraints = getCs }
--             db =
--   do actual <- getCs
--      let expected = collectChecks db
--      case finalSolution (heuristicSolver actions actual expected) of
--        Candidates {} -> fail "autoMigrate: Could not determine migration"
--        Solved cmds ->
--          -- Check if any of the commands are irreversible
--          case foldMap migrationCommandDataLossPossible cmds of
--            MigrationKeepsData -> mapM_ (runNoReturn . migrationCommand) cmds
--            _ -> fail "autoMigrate: Not performing automatic migration due to data loss"

-- -- | Given a migration backend, a handle to a database, and a checked database,
-- -- attempt to find a schema. This should always return 'Just', unless the
-- -- backend has incomplete migrations support.
-- --
-- -- 'BeamMigrationBackend's can usually be found in a module named
-- -- @Database.Beam.<Backend>.Migrate@ with the name@migrationBackend@
-- simpleMigration :: ( MonadBeam cmd be handle m
--                  ,   Database be db )
--                 => BeamMigrationBackend cmd be handle m
--                 -> handle
--                 -> CheckedDatabaseSettings be db
--                 -> IO (Maybe [cmd])
-- simpleMigration BeamMigrationBackend { backendGetDbConstraints = getCs
--                                      , backendActionProvider = action } hdl db = do
--   pre <- withDatabase hdl getCs

--   let post = collectChecks db
--       solver = heuristicSolver action pre post

--   case finalSolution solver of
--     Solved cmds -> pure (Just (fmap migrationCommand cmds))
--     Candidates {} -> pure Nothing

-- -- | Result type for 'verifySchema'
-- data VerificationResult
--   = VerificationSucceeded
--   | VerificationFailed [SomeDatabasePredicate]
--   deriving Show

-- -- | Verify that the given, beam database matches the actual
-- -- schema. On success, returns 'VerificationSucceeded', on failure,
-- -- returns 'VerificationFailed' and a list of missing predicates.
-- verifySchema :: ( Database be db, MonadBeam cmd be handle m )
--              => BeamMigrationBackend cmd be handle m
--              -> CheckedDatabaseSettings be db
--              -> m VerificationResult
-- verifySchema BeamMigrationBackend { backendGetDbConstraints = getConstraints } db =
--   do actualSchema <- HS.fromList <$> getConstraints
--      let expectedSchema = HS.fromList (collectChecks db)
--          missingPredicates = expectedSchema `HS.difference` actualSchema
--      if HS.null missingPredicates
--        then pure VerificationSucceeded
--        else pure (VerificationFailed (HS.toList missingPredicates))

-- -- | Run a sequence of commands on a database
-- runSimpleMigration :: MonadBeam cmd be hdl m
--                    => hdl -> [cmd] -> IO ()
-- runSimpleMigration hdl =
--   withDatabase hdl . mapM_ runNoReturn

-- -- | Given a function to convert a command to a 'String', produce a script that
-- -- will execute the given migration. Usually, the function you provide
-- -- eventually calls 'displaySyntax' to rendere the command.
-- backendMigrationScript :: (cmd -> String)
--                        -> Migration cmd a
--                        -> String
-- backendMigrationScript render mig =
--   migrateScript ((++"\n") . T.unpack) ((++"\n") . render) (migrationStep "Migration Script" (\() -> mig))
