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
            -> IO (MigrationDatabase, MigrationFormat, SomeBeamMigrationBackend)
loadBackend cmdLine reg dbName =
  case HM.lookup dbName (migrationRegistryDatabases reg) of
    Nothing -> fail "No such database"
    Just db@(MigrationDatabase backend _) -> do
      be <- loadBackend' cmdLine backend
      pure (db, MigrationFormatBackend (unModuleName backend), be)

loadBackend' :: MigrateCmdLine -> ModuleName -> IO SomeBeamMigrationBackend
loadBackend' cmdLine (ModuleName backend) = do
  let ghciArgs = map (\p -> "-package-db " <> p) (migratePackagePath cmdLine) <> [ "-v", "-fno-code" ]
      runInterpreter' = unsafeRunInterpreterWithArgs ghciArgs

  res <- runInterpreter' $ do
    unsafeSetGhcOption "-v"

    setImports [ "Database.Beam.Migrate.Backend", backend ]
    interpret "SomeBeamMigrationBackend migrationBackend" (undefined :: SomeBeamMigrationBackend)

  case res of
    Right be -> pure be
    Left e -> fail $ case e of
      WontCompile errs ->
        "Plugin load error: " ++ unlines (map errMsg errs)
      UnknownError err ->
        "Unknown interpeter error: " ++ err
      NotAllowed err ->
        "Not allowed: " ++ err
      GhcException err ->
        "GHC exception: " ++ err
