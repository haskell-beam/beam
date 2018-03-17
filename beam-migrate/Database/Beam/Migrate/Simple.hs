
-- | Utility functions for common use cases
module Database.Beam.Migrate.Simple
  ( simpleSchema, simpleMigration
  , runSimpleMigration, backendMigrationScript

  , VerificationResult(..)
  , verifySchema

  , createSchema

  , module Database.Beam.Migrate.Actions
  , module Database.Beam.Migrate.Types ) where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Actions

import qualified Data.Text as T
import qualified Data.HashSet as HS

-- | Attempt to find a SQL schema given an 'ActionProvider' and a checked
-- database. Returns 'Nothing' if no schema could be found, which usually means
-- you have chosen the wrong 'ActionProvider', or the backend you're using is
-- buggy.
simpleSchema :: Database be db
             => ActionProvider cmd
             -> CheckedDatabaseSettings be db
             -> Maybe [cmd]
simpleSchema provider settings =
  let allChecks = collectChecks settings
      solver    = heuristicSolver provider [] allChecks
  in case finalSolution solver of
       Solved cmds -> Just cmds
       Candidates {} -> Nothing

-- | Given a 'CheckedDatabaseSettings' and a 'BeamMigrationBackend',
-- attempt to create the schema from scratch in the current database.
--
-- May 'fail' if we cannot find a schema
createSchema :: Database be db
             => BeamMigrationBackend cmd be hdl m
             -> CheckedDatabaseSettings be db
             -> m ()
createSchema BeamMigrationBackend { backendActionProvider = actions } db =
  case simpleSchema actions db of
    Nothing -> fail "createSchema: Could not determine schema"
    Just cmds ->
        mapM_ runNoReturn cmds

autoMigrate :: Database be db
            => BeamMigrationBackend cmd be hdl m
            -> CheckedDatabaseSettings be db
            -> m ()
autoMigrate BeamMigrationBackend { backendActionProvider = actions
                                 , backendGetDbConstraints = getCs }
            db =
  do actual <- getCs
     let expected = collectChecks db
     case finalSolution (heuristicSolver actions actual expected) of
       Candidates {} -> fail "autoMigrate: Could not determine migration"
       Solved cmds -> do
         -- Check if any commands are irreversible
         fail "autoMigrate: TODO"

-- | Given a migration backend, a handle to a database, and a checked database,
-- attempt to find a schema. This should always return 'Just', unless the
-- backend has incomplete migrations support.
--
-- 'BeamMigrationBackend's can usually be found in a module named
-- @Database.Beam.<Backend>.Migrate@ with the name@migrationBackend@
simpleMigration :: ( MonadBeam cmd be handle m
                 ,   Database be db )
                => BeamMigrationBackend cmd be handle m
                -> handle
                -> CheckedDatabaseSettings be db
                -> IO (Maybe [cmd])
simpleMigration BeamMigrationBackend { backendGetDbConstraints = getCs
                                     , backendActionProvider = action } hdl db = do
  pre <- withDatabase hdl getCs

  let post = collectChecks db
      solver = heuristicSolver action pre post

  case finalSolution solver of
    Solved cmds -> pure (Just cmds)
    Candidates {} -> pure Nothing

-- | Result type for 'verifySchema'
data VerificationResult
  = VerificationSucceeded
  | VerificationFailed [SomeDatabasePredicate]
  deriving Show

-- | Verify that the given, beam database matches the actual
-- schema. On success, returns 'VerificationSucceeded', on failure,
-- returns 'VerificationFailed' and a list of missing predicates.
verifySchema :: ( Database be db, MonadBeam cmd be handle m )
             => BeamMigrationBackend be cmd handle m
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
runSimpleMigration :: MonadBeam cmd be hdl m
                   => hdl -> [cmd] -> IO ()
runSimpleMigration hdl =
  withDatabase hdl . mapM_ runNoReturn

-- | Given a function to convert a command to a 'String', produce a script that
-- will execute the given migration. Usually, the function you provide
-- eventually calls 'displaySyntax' to rendere the command.
backendMigrationScript :: (cmd -> String)
                       -> Migration cmd a
                       -> String
backendMigrationScript render mig =
  migrateScript ((++"\n") . T.unpack) ((++"\n") . render) (migrationStep "Migration Script" (\() -> mig))
