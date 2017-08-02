module Database.Beam.Migrate.Simple
  ( simpleSchema
  , module Database.Beam.Migrate.Actions
  , module Database.Beam.Migrate.Types ) where

import Database.Beam
import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Actions

simpleSchema :: Database db
             => [ ActionProvider cmd ]
             -> CheckedDatabaseSettings be db
             -> Maybe [cmd]
simpleSchema providers settings =
  let allChecks = collectChecks settings
      solver    = heuristicSolver providers [] allChecks
  in case finalSolution solver of
       Solved cmds -> Just cmds
       Candidates {} -> Nothing
