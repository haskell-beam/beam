module Database.Beam.Migrate.Tool.Registry where

import           Database.Beam.Migrate.Tool.CmdLine

import           Control.Applicative

import           Data.Aeson
import           Data.Bits
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS
import           Data.Hashable
import           Data.List (find)
import           Data.Monoid
import           Data.String
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Time
import           Data.UUID (UUID, fromWords)
import qualified Data.Yaml as Yaml

import           Network.BSD

import           System.Directory
#if defined(mingw32_HOST_OS)
import           System.Environment
#endif
import           System.FilePath
import           System.IO

#if !defined(mingw32_HOST_OS)
import           System.Posix.User
#endif

import           Text.Read hiding (String)

import           Debug.Trace

newtype MigrateUUID = MigrateUUID { unMigrateUUID :: UUID }

data MigrationFormat = MigrationFormatHaskell | MigrationFormatBackend String
  deriving (Show, Eq)

data RegisteredMigrationInfo
  = RegisteredMigrationInfo
  { registeredMigrationInfoHash    :: UUID
  , registeredMigrationInfoMessage :: Text
  , registeredMigrationInfoInputs  :: [ UUID ]
  , registeredMigrationInfoFormats :: [ MigrationFormat ]
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

data MigrationDatabase
  = MigrationDatabase
  { migrationDbBackend :: ModuleName
  , migrationDbConnString :: String
  } deriving Show

data MigrationHead
  = MigrationHeadDetached UUID
  | MigrationHeadBranch   Text
  deriving (Show, Eq)

data MigrationRegistry
  = MigrationRegistry
  { migrationRegistryDatabases      :: HM.HashMap DatabaseName MigrationDatabase
  , migrationRegistryHead           :: MigrationHead
  , migrationRegistryMigrations     :: [ RegisteredMigrationInfo ]
  , migrationRegistryBranches       :: [ MigrationBranch ]

  , migrationRegistrySrcDir         :: FilePath
  , migrationRegistrySchemaModule   :: ModuleName

  , migrationRegistryUserInfo       :: Maybe UserInfo
  } deriving Show

data MigrationMetaData
  = MigrationMetaData
  { migrationMetaDataCommit    :: UUID
  , migrationMetaDataFormats   :: [ (MigrationFormat, UUID) ]
  , migrationMetaDataCreatedOn :: UTCTime
  } deriving Show

instance ToJSON MigrationMetaData where
  toJSON (MigrationMetaData commit formats timestamp) =
    object [ "commit"    .= commit
           , "formats"   .= map (\(fmt, hash') -> object [ "format" .= fmt, "commit" .= hash' ]) formats
           , "createdOn" .= timestamp ]
instance FromJSON MigrationMetaData where
  parseJSON = withObject "MigrationMetaData" $ \o ->
              MigrationMetaData <$> o .: "commit" <*> (mapM parseFormats =<< o .: "formats") <*> o .: "createdOn"
    where
      parseFormats = withObject "MigrationMetaData.formats[]" $ \o ->
                     (,) <$> o .: "format" <*> o .: "commit"

instance ToJSON MigrationDatabase where
  toJSON (MigrationDatabase backend connString) =
    object [ "backend" .= backend, "uri" .= connString ]
instance FromJSON MigrationDatabase where
  parseJSON = withObject "MigrationDatabase" $ \o ->
              MigrationDatabase <$> o .: "backend" <*> o .: "uri"

instance ToJSON MigrateUUID where
  toJSON = toJSON . show . unMigrateUUID
instance FromJSON MigrateUUID where
  parseJSON v = fmap readMaybe (parseJSON v) >>=
                \case
                  Nothing -> fail "Could not read UUID"
                  Just uuid -> pure (MigrateUUID uuid)

instance ToJSON MigrationHead where
  toJSON (MigrationHeadDetached headId) = toJSON (MigrateUUID headId)
  toJSON (MigrationHeadBranch branch) = toJSON ("ref/branch/" <> branch)
instance FromJSON MigrationHead where
  parseJSON x = (MigrationHeadDetached . unMigrateUUID <$> parseJSON x) <|>
                readRef x
    where
      readRef = withText "MigrationHead" $ \ref ->
                if "ref/branch/" `T.isPrefixOf` ref
                  then pure (MigrationHeadBranch (T.drop (T.length "ref/branch/") ref))
                  else fail "Cannot read head"

instance ToJSON MigrationRegistry where
  toJSON MigrationRegistry {..} =
    object ( [ "databases"  .= migrationRegistryDatabases
             , "head"       .= migrationRegistryHead
             , "migrations" .= migrationRegistryMigrations
             , "branches"   .= migrationRegistryBranches
             , "module"     .= object [ "src" .= migrationRegistrySrcDir, "name" .= migrationRegistrySchemaModule ] ] ++
             case migrationRegistryUserInfo of
               Nothing -> []
               Just ui -> [ "user" .= ui ] )

instance FromJSON MigrationRegistry where
  parseJSON = withObject "MigrationRegistry" $ \o -> do
    (srcDir, name) <- (o .: "module") >>= withObject "MigrationRegistry.module" (\o' -> (,) <$> o' .: "src" <*> o' .: "name")
    MigrationRegistry <$> o .: "databases"
                      <*> o .: "head"
                      <*> o .: "migrations" <*> o .: "branches"
                      <*> pure srcDir <*> pure name
                      <*> o .:? "user"

instance ToJSON UserInfo where
  toJSON UserInfo {..} =
    object [ "full-name" .= userInfoFullName
           , "email"     .= userInfoEmail ]
instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \o ->
              UserInfo <$> o .: "full-name" <*> o .: "email"

instance ToJSON MigrationBranch where
  toJSON MigrationBranch {..} =
    object [ "name"   .= migrationBranchName
           , "commit" .= MigrateUUID migrationBranchCommit ]
instance FromJSON MigrationBranch where
  parseJSON = withObject "MigrationBranch" $ \o ->
              MigrationBranch <$> o .: "name" <*> (unMigrateUUID <$> o .: "commit")

instance ToJSON RegisteredMigrationInfo where
  toJSON RegisteredMigrationInfo {..} =
    object [ "hash"    .= MigrateUUID registeredMigrationInfoHash
           , "message" .= registeredMigrationInfoMessage
           , "inputs"  .= (MigrateUUID <$> registeredMigrationInfoInputs)
           , "formats" .= registeredMigrationInfoFormats ]
instance FromJSON RegisteredMigrationInfo where
  parseJSON = withObject "RegisteredMigrationInfo" $ \o ->
              RegisteredMigrationInfo <$> (unMigrateUUID <$> o .: "hash")
                                      <*> o .: "message"
                                      <*> (fmap unMigrateUUID <$> o .: "inputs")
                                      <*> o .: "formats"

instance ToJSON MigrationFormat where
  toJSON MigrationFormatHaskell = "haskell"
  toJSON (MigrationFormatBackend be) = fromString be
instance FromJSON MigrationFormat where
  parseJSON "haskell" = pure MigrationFormatHaskell
  parseJSON (String be) = pure (MigrationFormatBackend (unpack be))
  parseJSON _ = fail "Cannot parse MigrationFormat"

-- | Attempt to read a registry from the common lookup paths
--
--     1. If a registry is given on the command line, don't attempt lookup
--     2. Otherwise, look for a @.beam-migrate@ file in this directory and each parent directory
lookupRegistry' :: MigrateCmdLine -> IO (FilePath, MigrationRegistry)
lookupRegistry' MigrateCmdLine { migrateRegistryPath = Just path } =
  Yaml.decodeFile path >>= maybe (fail "Could not read migration registry") (pure . (path,))
lookupRegistry' cmdLine = getCurrentDirectory >>= lookupRegistry''
  where
    lookupRegistry'' dir =
      do let potentialRegistry = dir </> ".beam-migrate"
         registryExists <- doesPathExist potentialRegistry

         if registryExists
           then lookupRegistry' cmdLine { migrateRegistryPath = Just potentialRegistry }
           else if isDrive dir
                then fail "Could not find migration registry"
                else lookupRegistry'' (takeDirectory dir)

lookupRegistry :: MigrateCmdLine -> IO MigrationRegistry
lookupRegistry = fmap snd . lookupRegistry'

lookupUserInfo :: MigrationRegistry -> IO UserInfo
lookupUserInfo MigrationRegistry { migrationRegistryUserInfo = Just ui } = pure ui
lookupUserInfo _ = do
#if defined(mingw32_HOST_OS)
  username <- getEnv "USERNAME"
  let fullName = username
#else
  userId <- getEffectiveUserID
  UserEntry { userName = username, userGecos = fullName } <- getUserEntryForID userId
#endif
  hostname <- getHostName

  let email = username ++ "@" ++ hostname
      userInfo = UserInfo { userInfoFullName = fromString fullName, userInfoEmail = fromString email }

  hPutStrLn stderr ("WARNING: defaulting user info to " ++ show userInfo)
  pure userInfo

userInfoCommitter :: UserInfo -> Text
userInfoCommitter ui = "\"" <> userInfoEmail ui <> "\"<" <> userInfoEmail ui <> ">"

updatingRegistry :: MigrateCmdLine -> (MigrationRegistry -> IO MigrationRegistry) -> IO ()
updatingRegistry cmdLine action =
  do (registryPath, registry) <- lookupRegistry' cmdLine
     registry' <- action registry
     Yaml.encodeFile registryPath registry'

lookupDb :: MigrationRegistry -> MigrateCmdLine -> IO MigrationDatabase
lookupDb MigrationRegistry { migrationRegistryDatabases = dbs } MigrateCmdLine { migrateDatabase = Nothing }
  | HM.null dbs = fail "No databases in registry"
  | [(dbName, db)] <- HM.toList dbs =
    do hPutStrLn stderr ("WARNING: No database specified, defaulting to '" ++ unDatabaseName dbName ++ "'")
       pure db
  | otherwise = fail ("Please specify database with the --database option")
lookupDb reg MigrateCmdLine { migrateDatabase = Just db } =
  case HM.lookup db (migrationRegistryDatabases reg) of
    Nothing -> fail ("No such database: " ++ unDatabaseName db)
    Just db' -> pure db'

lookupBranch :: MigrationRegistry -> Text -> Maybe MigrationBranch
lookupBranch reg branchNm =
  find ((==branchNm) . migrationBranchName) (migrationRegistryBranches reg)

lookupMigration :: UUID -> MigrationFormat -> MigrationRegistry -> Maybe RegisteredMigrationInfo
lookupMigration commitId fmt reg =
  find (\mig -> registeredMigrationInfoHash mig == commitId &&
                any (==fmt) (registeredMigrationInfoFormats mig))
       (migrationRegistryMigrations reg)

newBaseBranch :: Text -> UUID -> MigrationRegistry -> IO MigrationRegistry
newBaseBranch branchName commit reg =
  case find ((==branchName) . migrationBranchName) (migrationRegistryBranches reg) of
    Just {} -> fail "Branch already exists"
    Nothing -> do
      putStrLn ("Created new branch " ++ unpack branchName)
      let branch = MigrationBranch branchName commit

      pure reg { migrationRegistryBranches = branch:migrationRegistryBranches reg }

newMigration :: UUID -> MigrationFormat -> Text -> [UUID] -> MigrationRegistry -> IO MigrationRegistry
newMigration commitId fmt msg inputs reg =
  case lookupMigration commitId fmt reg of
    Just {} -> fail "Migration already exists"
    Nothing ->
      let migration = RegisteredMigrationInfo commitId msg inputs [fmt]
      in pure reg { migrationRegistryMigrations = migration:migrationRegistryMigrations reg }

commitScriptName, commitModuleName :: UUID -> String
commitScriptName commitId =
  "mig_" <> map (\c -> if c == '-' then '_' else c) (show commitId)
commitModuleName commitId =
  "Schema_" <> map (\c -> if c == '-' then '_' else c) (show commitId)

writeMigrationFile :: MigrateCmdLine -> MigrationRegistry -> String -> String -> String -> IO FilePath
writeMigrationFile _ reg extension fileNm content = do
  let path = migrationRegistrySrcDir reg </> (fileNm <.> extension)

  putStrLn ("Writing migration to " ++ path ++ "...")
  createDirectoryIfMissing True (migrationRegistrySrcDir reg)
  writeFile path content

  pure path

registryHeadCommit :: MigrationRegistry -> UUID
registryHeadCommit reg =
  case migrationRegistryHead reg of
    MigrationHeadDetached commitId -> commitId
    MigrationHeadBranch nm ->
      case lookupBranch reg nm of
        Nothing -> error "Cannot find branch"
        Just branch -> migrationBranchCommit branch

hashToUUID :: Hashable a => a -> UUID
hashToUUID a =
  let intSize = finiteBitSize (undefined :: Int)
      wordsNeeded = (128 + intSize - 1) `div` intSize

      wordsData = take wordsNeeded (tail (iterate (\seed -> hashWithSalt seed a) 0))

      uuidData :: Integer
      uuidData = foldr (\w a' -> a' `shiftL` intSize .|. fromIntegral (fromIntegral w :: Word)) 0 wordsData

      uuidWord1 = fromIntegral $ (uuidData `shiftR` 96) .&. 0xFFFFFFFF
      uuidWord2 = fromIntegral $ (uuidData `shiftR` 64) .&. 0xFFFFFFFF
      uuidWord3 = fromIntegral $ (uuidData `shiftR` 32) .&. 0xFFFFFFFF
      uuidWord4 = fromIntegral $ uuidData .&. 0xFFFFFFFF

  in trace ("Hash" ++ show uuidData ++ " " ++ show wordsData) $
     fromWords uuidWord1 uuidWord2 uuidWord3 uuidWord4

metadataComment :: String -> MigrationMetaData -> String
metadataComment commentMarker metadata =
  let encoded = map BS.unpack (BS.lines (Yaml.encode metadata))
  in unlines ( [commentMarker <> " + BEAM-MIGRATE"] <>
               map ((commentMarker <> " + ") <>) encoded <>
               [commentMarker <> " + END-BEAM-MIGRATE"])
