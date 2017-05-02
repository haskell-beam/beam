module Database.Beam.Migrate.Tool.Registry where

import Data.UUID (UUID)
import Data.Text (Text)
import Data.Aeson

data RegisteredMigrationInfo
  = RegisteredMigrationInfo
  { registeredMigrationInfoHash    :: UUID
  , registeredMigrationInfoMessage :: Text
  , registeredMigrationInfoInputs  :: [ UUID ]
  } deriving Show

data MigrationBranch
  = MigrationBranch
  { migrationBranchName   :: Text
  , migrationBranchCommit :: UUID
  } deriving Show

data UserInfo
  = UserInfo
  { userInfoFullName      :: Text
  , userInfoEmail         :: Text
  } deriving Show

data MigrationRegistry options
  = MigrationRegistry
  { migrationRegistryBackendOptions :: options
  , migrationRegistryUserInfo       :: UserInfo
  , migrationRegistryHead           :: UUID
  , migrationRegistryMigrations     :: [ RegisteredMigrationInfo ]
  , migrationRegistryBranches       :: [ MigrationBranch ]
  } deriving Show

instance ToJSON options => ToJSON (MigrationRegistry options) where
  toJSON MigrationRegistry {..} =
    object [ "options"    .= migrationRegistryBackendOptions
           , "head"       .= migrationRegistryHead
           , "migrations" .= migrationRegistryMigrations
           , "branches"   .= migrationRegistryBranches ]
instance FromJSON options => FromJSON (MigrationRegistry options) where
  parseJSON = withObject "MIgrationRegistry" $ \o ->
              MigrationRegistry <$> o .: "options"    <*> o .: "head"
                                <*> o .: "migrations" <*> o .: "branches"

instance ToJSON MigrationBranch where
  toJSON MigrationBranch {..} =
    object [ "name"   .= migrationBranchName
           , "commit" .= migrationBranchCommit ]
instance FromJSON MigrationBranch where
  parseJSON = withObject "MigrationBranch" $ \o ->
              MigrationBranch <$> o .: "name" <*> o .: "commit"

instance ToJSON RegisteredMigrationInfo where
  toJSON RegisteredMigrationInfo {..} =
    object [ "hash"    .= registeredMigrationInfoHash
           , "message" .= registeredMigrationInfoMessage
           , "inputs"  .= registeredMigrationInfoInputs ]
instance FromJSON RegisteredMigrationInfo where
  parseJSON = withObject "RegisteredMigrationInfo" $ \o ->
              RegisteredMigrationInfo <$> o .: "hash" <*> o .: "message"
                                      <*> o .: "inputs"
