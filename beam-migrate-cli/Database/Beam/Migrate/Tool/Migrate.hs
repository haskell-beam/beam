module Database.Beam.Migrate.Tool.Migrate where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Diff

import           Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Query as Gr
import qualified Data.HashSet as HS
import           Data.List (find)

doMigrateDatabase :: MigrateCmdLine -> IO ()
doMigrateDatabase MigrateCmdLine { migrateDatabase = Nothing } =
  fail "No database specified"
doMigrateDatabase cmdLine = do
  registry <- lookupRegistry cmdLine
  db <- lookupDb registry cmdLine

  (expCommit, expPreds) <- getPredicatesFromSpec cmdLine registry (PredicateFetchSourceDbHead db (Just 0))
  (_, actualPreds) <- getPredicatesFromSpec cmdLine registry (PredicateFetchSourceDbHead db Nothing)

  let expPredSet = HS.fromList expPreds
      actualPredSet = HS.fromList actualPreds

--      backend = migrationDbBackend db

  case expCommit of
    Nothing -> fail "No commit recorded in database"
    Just expCommit'
      | expPredSet /= actualPredSet ->
          fail "Database does not match expectation. Use 'beam-migrate diff'"
      | expCommit' == registryHeadCommit registry ->
          putStrLn "Database is up-to-date"
      | otherwise -> do
          let migrations = registryMigrationGraph registry
              findMigration commit = fst <$> find (\(_, s) -> registeredSchemaInfoHash s == commit) (labNodes migrations)

          case (,) <$> findMigration expCommit'
                   <*> findMigration (registryHeadCommit registry) of
            Nothing -> fail "Can't find schemas"
            Just (sourceNode, destNode) ->
              let path = Gr.lesp sourceNode destNode migrations
              in fail ("Migrating " ++ show path)
