module Database.Beam.Migrate.Tool.Init where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry

import qualified Data.UUID.V4 as UUID
import qualified Data.Yaml as Yaml

import           System.Directory

initBeamMigrate :: MigrateCmdLine -> InitCommand -> IO ()
initBeamMigrate _ initCmd = do
  alreadyInitialized <- doesPathExist ".beam-migrate"

  if alreadyInitialized then fail ".beam-migrate already exists. Run 'beam-migrate clean'"
    else do
      curDir <- getCurrentDirectory
      headUuid <- UUID.nextRandom

      let registry' = MigrationRegistry
                    { migrationRegistryDatabases = mempty
                    , migrationRegistryHead      = headUuid
                    , migrationRegistryMigrations = []
                    , migrationRegistryBranches = []

                    , migrationRegistrySrcDir = initModulePath initCmd
                    , migrationRegistrySchemaModule = initModule initCmd

                    , migrationRegistryUserInfo = Nothing }

      Yaml.encodeFile ".beam-migrate" registry'
      putStrLn ("Beam migrations initialized in " ++ curDir)
  where
