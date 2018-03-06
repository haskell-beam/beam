module Database.Beam.Migrate.Tool.Migrate where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Diff

import           Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Query as Gr
import qualified Data.HashSet as HS
import           Data.List (find)

import           System.IO

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
    Nothing -> fail "TODO: No commit in database, so we should run the entire migration as recorded in the schema"
    Just expCommit'
      | expPredSet /= actualPredSet ->
          fail "Database does not match expectation. Use 'beam-migrate diff'"
      | expCommit' == registryHeadCommit registry ->
          putStrLn "Database is up-to-date"
      | otherwise -> do
          let migrations = registryMigrationGraph registry
              findMigration commit = fst <$> find (\(_, s) -> registeredSchemaInfoHash s == commit) (labNodes migrations)

              sourceCommit = expCommit'
              destinationCommit = registryHeadCommit registry

          case (,) <$> findMigration sourceCommit
                   <*> findMigration  destinationCommit of
            Nothing -> fail "Can't find schemas"
            Just (sourceNode, destNode)
                | sourceNode == destNode ->
                    putStrLn "The database is already up-to-date with HEAD"
                | otherwise ->
                    case unLPath (Gr.lesp sourceNode destNode migrations) of
                      [] -> do
                        hPutStr stderr . unlines $
                          [ "There is no migration between " ++ show sourceCommit ++
                            " and " ++ show destinationCommit
                          , ""
                          , "You can ask beam to generate one for you automatically, by running 'beam-migrate migration new --auto"
                          , "Alternatively, you can run 'beam-migrate migrate --manual', to write a custom migration script"
                          ]
                        fail "No possible migration"
                      path -> fail ("Migrating " ++ show path)
