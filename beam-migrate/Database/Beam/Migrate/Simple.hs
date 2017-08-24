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

import Unsafe.Coerce

simpleSchema :: Database be db
             => [ ActionProvider cmd ]
             -> CheckedDatabaseSettings be db
             -> Maybe [cmd]
simpleSchema providers settings =
  let allChecks = collectChecks settings
      solver    = heuristicSolver providers [] allChecks
  in case finalSolution solver of
       Solved cmds -> Just cmds
       Candidates {} -> Nothing

simpleMigration :: ( MonadBeam cmd be handle m
                 ,   Database be db )
                => BeamMigrationBackend be cmd
                -> handle
                -> CheckedDatabaseSettings be db
                -> IO (Maybe [cmd])
simpleMigration (BeamMigrationBackend { backendGetDbConstraints = getCs, backendActionProviders = actions }) hdl db = do
  pre <- getCs (unsafeCoerce hdl)

  let post = collectChecks db
  let solver = heuristicSolver actions pre post
  case finalSolution solver of
    Solved cmds -> pure (Just cmds)
    Candidates {} -> pure Nothing

runSimpleMigration :: MonadBeam cmd be hdl m
                   => hdl -> [cmd] -> IO ()
runSimpleMigration hdl =
  withDatabase hdl . mapM_ runNoReturn

backendMigrationScript :: (cmd -> String)
                       -> Migration cmd a
                       -> String
backendMigrationScript render mig =
  migrateScript ((++"\n") . T.unpack) ((++"\n") . render) (migrationStep "Migration Script" (\() -> mig))
