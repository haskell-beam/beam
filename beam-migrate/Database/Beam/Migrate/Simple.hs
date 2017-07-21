module Database.Beam.Migrate.Simple
  ( simpleSchema
  , module Database.Beam.Migrate.Actions ) where

import Database.Beam
import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Actions

import Data.ByteString (ByteString)

simpleSchema :: Database db
             => [ ActionProvider cmd ]
             -> CheckedDatabaseSettings be db
             -> Maybe [cmd]
simpleSchema providers settings =
  let allChecks = collectChecks settings
      solver = heuristicSolver providers [] allChecks
  in case finalSolution solver of
       Solved cmds -> Just cmds
       Candidates {} -> Nothing
 
