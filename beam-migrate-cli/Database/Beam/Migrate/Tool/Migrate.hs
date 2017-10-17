module Database.Beam.Migrate.Tool.Migrate where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Diff

import           Control.Monad

import qualified Data.HashSet as HS

doMigrateDatabase :: MigrateCmdLine -> IO ()
doMigrateDatabase MigrateCmdLine { migrateDatabase = Nothing } =
  fail "No database specified"
doMigrateDatabase cmdLine = do
  registry <- lookupRegistry cmdLine
  db <- lookupDb registry cmdLine

  (_, expPreds) <- getPredicatesFromSpec cmdLine registry (PredicateFetchSourceDbHead db (Just 0))
  (_, actualPreds) <- getPredicatesFromSpec cmdLine registry (PredicateFetchSourceDbHead db Nothing)

  let expPredSet = HS.fromList expPreds
      actualPredSet = HS.fromList actualPreds

      backend = migrationDbBackend db

  when (expPredSet /= actualPredSet) $
    fail "Database does not match expectation. Use 'beam-migrate diff'"

  _ <- getPredicatesFromSpec cmdLine registry (PredicateFetchSourceCommit (Just backend) (registryHeadCommit registry))
  fail "Migrating'"
