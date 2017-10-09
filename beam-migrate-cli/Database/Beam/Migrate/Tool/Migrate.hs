module Database.Beam.Migrate.Tool.Migrate where

import Databsae.Beam.Migrate.Tool.CmdLine
import Databsae.Beam.Migrate.Tool.Registry

doMigrateDatabase :: MigrateCmdLine -> IO ()
doMigrateDatabase MigrateCmdLine { migrateDatabase = Nothing } =
  fail "No database specified"
doMigrateDatabase cmdLine@MigrateCmdLine { migrateDatabase = Just dbName } = do
  registry <- lookupRegistry cmdLine

  expPreds <- getPredicatesFromSpec cmdLine registry (PredicateFetchSourceDbHead dbName (Just 0))
  actualPreds <- getPredicatesFromSpec cmdLine registry (PredicateFetchSourceDbHead dbName Nothing)

  let expPredSet = HS.fromList expPreds
      actualPredSet = HS.fromList actualPreds

  when (expPredSet /= actualPredSet) $
    fail "Database does not match expectation. Use 'beam-migrate diff'"

  curPreds <- getPredicatesFromSpec cmdLine registry (PredicateFetchSourceCommit backend (registryHeadCommit registry))
  
