module Database.Beam.Migrate.Tool.Backend where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Backend

import qualified Data.HashMap.Strict as HM
import           Data.Monoid

import           Language.Haskell.Interpreter hiding (ModuleName)
import           Language.Haskell.Interpreter.Unsafe

loadBackend :: MigrateCmdLine -> MigrationRegistry
            -> DatabaseName
            -> IO (String, MigrationFormat, SomeBeamMigrationBackend)
loadBackend cmdLine reg dbName =
  case HM.lookup dbName (migrationRegistryDatabases reg) of
    Nothing -> fail "No such database"
    Just (MigrationDatabase backend connString ) -> do
      be <- loadBackend' cmdLine backend
      pure (connString, MigrationFormatBackend (unModuleName backend), be)

loadBackend' :: MigrateCmdLine -> ModuleName -> IO SomeBeamMigrationBackend
loadBackend' cmdLine (ModuleName backend) = do
  let ghciArgs = map (\p -> "--package-db " <> p) (migratePackagePath cmdLine)
      runInterpreter' = unsafeRunInterpreterWithArgs ghciArgs

  res <- runInterpreter' $ do
    unsafeSetGhcOption "-v"

    setImports [ "Database.Beam.Migrate.Backend", backend ]
    interpret "SomeBeamMigrationBackend migrationBackend" (undefined :: SomeBeamMigrationBackend)

  case res of
    Left err -> fail ("Plugin load error: " ++ show err)
    Right be -> pure be
