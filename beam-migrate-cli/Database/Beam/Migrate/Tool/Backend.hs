module Database.Beam.Migrate.Tool.Backend where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Backend

import           Control.Monad.Catch

import qualified Data.HashMap.Strict as HM
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Monoid
#endif

import           Language.Haskell.Interpreter hiding (ModuleName)
import           Language.Haskell.Interpreter.Unsafe

import           System.Exit
import           System.IO

loadBackend :: MigrateCmdLine -> MigrationRegistry
            -> DatabaseName
            -> IO (MigrationDatabase, MigrationFormat, SomeBeamMigrationBackend)
loadBackend cmdLine reg dbName =
  case HM.lookup dbName (migrationRegistryDatabases reg) of
    Nothing -> fail "No such database"
    Just db@(MigrationDatabase backend _) -> do
      be <- loadBackend' cmdLine backend
      pure (db, MigrationFormatBackend (unModuleName backend), be)

runBeamInterpreter :: (MonadIO m, MonadMask m)
                   => MigrateCmdLine -> InterpreterT m a
                   -> m (Either InterpreterError a)
runBeamInterpreter cmdLine action =
    let ghciArgs = map (\p -> "-package-db " <> p) (migratePackagePath cmdLine) <> [ "-v" ]--, "-fno-code" ]
    in unsafeRunInterpreterWithArgs ghciArgs $ do
       unsafeSetGhcOption "-v3"
       action

loadBackend' :: MigrateCmdLine -> ModuleName -> IO SomeBeamMigrationBackend
loadBackend' cmdLine (ModuleName backend) = do
  res <- runBeamInterpreter cmdLine $ do
    setImports [ "Database.Beam.Migrate.Backend", backend ]
    interpret "SomeBeamMigrationBackend migrationBackend" (undefined :: SomeBeamMigrationBackend)

  case res of
    Right be -> pure be
    Left e -> reportHintError e

reportHintError :: InterpreterError -> IO a
reportHintError e =
  do hPutStrLn stderr $
       case e of
         WontCompile errs ->
           "Plugin load error: " ++ unlines (map errMsg errs)
         UnknownError err ->
           "Unknown interpeter error: " ++ err
         NotAllowed err ->
           "Not allowed: " ++ err
         GhcException err ->
           "GHC exception: " ++ err
     exitWith (ExitFailure 1)
