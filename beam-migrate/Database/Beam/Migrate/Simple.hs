
-- | Utility functions for common use cases
module Database.Beam.Migrate.Simple
  ( simpleSchema, simpleMigration
  , runSimpleMigration, backendMigrationScript

  , module Database.Beam.Migrate.Actions
  , module Database.Beam.Migrate.Types ) where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate.Backend
import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Actions

import qualified Data.Text as T

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

-- | Given a migration backend, a handle to a database, and a checked database,
-- attempt to find a schema. This should always return 'Just', unless the
-- backend has incomplete migrations support.
--
-- 'BeamMigrationBackend's can usually be found in a module named
-- @Database.Beam.<Backend>.Migrate@ with the name@migrationBackend@
simpleMigration :: ( MonadBeam cmd be handle m
                 ,   Database be db )
                => BeamMigrationBackend be cmd handle
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
