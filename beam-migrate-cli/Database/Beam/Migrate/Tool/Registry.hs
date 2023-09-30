{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Beam.Migrate.Tool.Registry where

import           Database.Beam.Migrate
import           Database.Beam.Migrate.Backend
import           Database.Beam.Migrate.Serialization
import           Database.Beam.Migrate.Tool.CmdLine

import           Control.Applicative
import           Control.Exception

import qualified Crypto.Hash as Crypto

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.LargeWord (Word256)
import           Data.List (find, intercalate, sort)
import           Data.Maybe
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Monoid
#endif
import           Data.String
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Time
import           Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified Data.Yaml as Yaml

import           Network.HostName (getHostName)

import           Numeric (showHex, readHex)

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

data InvalidCommitId = InvalidCommitId T.Text
  deriving Show
instance Exception InvalidCommitId

newtype MigrateUUID = MigrateUUID { unMigrateUUID :: UUID }

data BeamMigrateMode
  = BeamMigrateReady
  | BeamMigrateCreatingSchema !FilePath !(Maybe UUID) !Word256 -- ^ currently editing a schema, with SHA256 hash of file
  | BeamMigrateEditingMigration !FilePath !UUID !UUID !Word256 -- ^ currently editing a migration, with SHA256 hash of file
  deriving Show

data PredicateFetchSource
  = PredicateFetchSourceCommit     !(Maybe ModuleName) !UUID
  | PredicateFetchSourceDbHead     !MigrationDatabase !(Maybe Int)
  | PredicateFetchSourceEmpty
  deriving Show

data RegisteredSchemaInfo
  = RegisteredSchemaInfo
  { registeredSchemaInfoHash    :: UUID
  , registeredSchemaInfoCommitter :: UserInfo
  , registeredSchemaInfoMessage :: Text
  , registeredSchemaInfoFormats :: [ MigrationFormat ]
  } deriving Show

data RegisteredMigrationInfo
  = RegisteredMigrationInfo
  { registeredMigrationInfoResult :: UUID
  , registeredMigrationInfoSource :: UUID
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
  , migrationRegistrySchemas        :: [ RegisteredSchemaInfo ]
  , migrationRegistryMigrations     :: [ RegisteredMigrationInfo ]
  , migrationRegistryBranches       :: [ MigrationBranch ]

  , migrationRegistrySrcDir         :: FilePath
  , migrationRegistrySchemaModule   :: ModuleName

  , migrationRegistryUserInfo       :: Maybe UserInfo

  , migrationRegistryMode           :: BeamMigrateMode
  } deriving Show

data Schema
  = Schema
  { schemaPredicates :: [ (HS.HashSet PredicateSpecificity, SomeDatabasePredicate) ]
  } deriving Show

data SchemaMetaData
  = SchemaMetaData
  { schemaMetaDataCommit    :: UUID
  , schemaMetaDataFormats   :: [ MigrationFormat ]
  , schemaMetaDataCreatedOn :: UTCTime
  , schemaMetaDataSchema    :: Schema
  } deriving Show

instance ToJSON SchemaMetaData where
  toJSON (SchemaMetaData commit formats timestamp' schema) =
    object [ "commit"    .= commit
           , "formats"   .= formats
           , "createdOn" .= timestamp'
           , "schema"    .= schema ]
-- instance FromJSON SchemaMetaData where
--   parseJSON = withObject "SchemaMetaData" $ \o ->
--               SchemaMetaData <$> o .: "commit" <*> o .: "formats" <*> o .: "createdOn"
--                              <*> o .: "schema"

instance ToJSON Schema where
  toJSON (Schema predicates) =
    let grouped = HM.fromListWith mappend (map (fmap pure) predicates)
    in toJSON (map (\(specificity, predicates') ->
                       object [ "specificity" .= specificity
                              , "predicates" .= map (\(SomeDatabasePredicate predicate) -> serializePredicate predicate) predicates' ])
                   (HM.toList grouped))
-- instance FromJSON Schema where
--   parseJSON = withArray "Schema" $ \a ->
--               foldlM (\done -> withObject "Schema[]" $ \o ->
--                                do specificity <- o .: "specificity"
--                                   predicates <- o .: "predicates"
--                                   pure (map (specifity,) predicates ++ done))
--                      [] a

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

instance ToJSON BeamMigrateMode where
  toJSON BeamMigrateReady = "ready"
  toJSON (BeamMigrateCreatingSchema schemaFl src hash) =
    object [ "tmpFile" .= schemaFl
           , "src" .= src
           , "hash" .= showHex hash "" ]
  toJSON (BeamMigrateEditingMigration migrationFl from to hash) =
    object [ "tmpFile" .= migrationFl
           , "from" .= from, "to" .= to
           , "hash" .= showHex hash "" ]
instance FromJSON BeamMigrateMode where
  parseJSON "ready" = pure BeamMigrateReady
  parseJSON o = withObject "BeamMigrateMode"
                (\v -> BeamMigrateCreatingSchema <$> v .: "tmpFile"
                                                 <*> v .: "src"
                                                 <*> (rdHash =<< v .: "hash") <|>
                       BeamMigrateEditingMigration <$> v .: "tmpFile"
                                                   <*> v .: "from" <*> v .: "to"
                                                   <*> (rdHash =<< v .: "hash")) o
    where
      rdHash h = case filter (null . snd) (readHex h) of
                   [(x, _)] -> pure x
                   _ -> fail "Invalid hash"

instance ToJSON MigrationRegistry where
  toJSON MigrationRegistry {..} =
    object ( (case migrationRegistryMode of
                BeamMigrateReady -> []
                mode -> [ "mode" .= mode ]) ++
             [ "databases"  .= migrationRegistryDatabases
             , "head"       .= migrationRegistryHead
             , "schemas"    .= migrationRegistrySchemas
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
                      <*> fmap (fromMaybe []) (o .:? "schemas")
                      <*> fmap (fromMaybe []) (o .:? "migrations")
                      <*> o .: "branches"
                      <*> pure srcDir <*> pure name
                      <*> o .:? "user"
                      <*> (fromMaybe BeamMigrateReady <$> o .:? "mode")

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

instance ToJSON RegisteredSchemaInfo where
  toJSON RegisteredSchemaInfo {..} =
    object [ "hash"    .= MigrateUUID registeredSchemaInfoHash
           , "message" .= registeredSchemaInfoMessage
           , "formats" .= registeredSchemaInfoFormats
           , "committer" .= registeredSchemaInfoCommitter ]
instance FromJSON RegisteredSchemaInfo where
  parseJSON = withObject "RegisteredSchemaInfo" $ \o ->
              RegisteredSchemaInfo <$> (unMigrateUUID <$> o .: "hash")
                                   <*> o .: "committer"
                                   <*> o .: "message"
                                   <*> o .: "formats"

instance ToJSON RegisteredMigrationInfo where
  toJSON RegisteredMigrationInfo {..} =
    object [ "result" .= registeredMigrationInfoResult
           , "source" .= registeredMigrationInfoSource
           , "formats" .= registeredMigrationInfoFormats ]
instance FromJSON RegisteredMigrationInfo where
  parseJSON = withObject "RegisteredMigrationInfo" $ \o ->
              RegisteredMigrationInfo <$> o .: "result"
                                      <*> o .: "source"
                                      <*> o .: "formats"

instance ToJSON MigrationFormat where
  toJSON MigrationFormatHaskell = "haskell"
  toJSON (MigrationFormatBackend be) = fromString be
instance FromJSON MigrationFormat where
  parseJSON "haskell" = pure MigrationFormatHaskell
  parseJSON (String be) = pure (MigrationFormatBackend (unpack be))
  parseJSON _ = fail "Cannot parse MigrationFormat"

reportDdlErrors :: IO (Either DdlError a) -> IO a
reportDdlErrors go = do
  res <- go
  case res of
    Left err -> fail ("DDL error: " ++ show err)
    Right  x -> pure x

registeredSchemaInfoShortMessage :: RegisteredSchemaInfo -> Text
registeredSchemaInfoShortMessage sch =
  let fullMsg = registeredSchemaInfoMessage sch
  in case T.lines fullMsg of
       [] -> "(No message)"
       x:_ -> x

-- | Attempt to read a registry from the common lookup paths
--
--     1. If a registry is given on the command line, don't attempt lookup
--     2. Otherwise, look for a @.beam-migrate@ file in this directory and each parent directory
lookupRegistry' :: MigrateCmdLine -> IO (FilePath, MigrationRegistry)
lookupRegistry' MigrateCmdLine { migrateRegistryPath = Just path } =
  Yaml.decodeFileEither path >>= either (\e -> fail ("Could not read migration registry: " ++ show e)) (pure . (path,))
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

showMigrationFormats :: [ MigrationFormat ] -> String
showMigrationFormats = intercalate ", ". map showFormat . sort
  where
    showFormat MigrationFormatHaskell = "Haskell"
    showFormat (MigrationFormatBackend be) = be

userInfoCommitter :: UserInfo -> Text
userInfoCommitter ui = "\"" <> userInfoEmail ui <> "\"<" <> userInfoEmail ui <> ">"

assertRegistryReady :: MigrationRegistry -> IO ()
assertRegistryReady MigrationRegistry { migrationRegistryMode = BeamMigrateReady } = pure ()
assertRegistryReady _ = fail "There is an edit in progress. Use 'beam-migrate abort' to cancel"

updatingRegistry :: MigrateCmdLine -> (MigrationRegistry -> IO (a, MigrationRegistry)) -> IO a
updatingRegistry cmdLine action =
  do (registryPath, registry) <- lookupRegistry' cmdLine
     (x, registry') <- action registry
     Yaml.encodeFile registryPath registry'
     pure x

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

lookupSchema :: UUID -> [MigrationFormat] -> MigrationRegistry -> Maybe RegisteredSchemaInfo
lookupSchema commitId fmts reg =
  find (\sch -> registeredSchemaInfoHash sch == commitId &&
                all (`elem` registeredSchemaInfoFormats sch) fmts)
       (migrationRegistrySchemas reg)

lookupMigration :: UUID -> UUID -> [MigrationFormat] -> MigrationRegistry -> Maybe RegisteredMigrationInfo
lookupMigration from dest fmts reg =
  find (\mig -> registeredMigrationInfoSource mig == from &&
                registeredMigrationInfoResult mig == dest &&
                all (`elem` registeredMigrationInfoFormats mig) fmts)
       (migrationRegistryMigrations reg)

newBaseBranch :: Text -> UUID -> MigrationRegistry -> IO MigrationRegistry
newBaseBranch branchName commit reg =
  case find ((==branchName) . migrationBranchName) (migrationRegistryBranches reg) of
    Just {} -> fail "Branch already exists"
    Nothing -> do
      putStrLn ("Created new branch " ++ unpack branchName)
      let branch = MigrationBranch branchName commit

      pure reg { migrationRegistryBranches = branch:migrationRegistryBranches reg }

updateBranch :: Text -> MigrationBranch -> MigrationRegistry -> IO MigrationRegistry
updateBranch branchNm newBranch reg =
  case lookupBranch reg branchNm of
    Nothing -> fail ("Cannot update branch " ++ T.unpack branchNm)
    Just {} ->
      pure reg { migrationRegistryBranches =
                   map (\br -> if migrationBranchName br == branchNm
                               then newBranch else br)
                       (migrationRegistryBranches reg) }

newSchema :: UUID -> [MigrationFormat] -> Text -> MigrationRegistry -> IO MigrationRegistry
newSchema commitId fmts msg reg =
  case lookupSchema commitId [] reg of
    Just {} -> fail "Schema already exists"
    Nothing -> do
      userInfo <- lookupUserInfo reg
      let schema = RegisteredSchemaInfo commitId userInfo msg fmts
      pure reg { migrationRegistrySchemas = schema:migrationRegistrySchemas reg }

newMigration :: UUID -> UUID -> [MigrationFormat] -> MigrationRegistry -> IO MigrationRegistry
newMigration from dest fmts reg =
  case lookupMigration from dest [] reg of
    Just {} -> fail "Migration alread exists"
    Nothing ->
      let mig = RegisteredMigrationInfo from dest fmts
      in pure reg { migrationRegistryMigrations = mig:migrationRegistryMigrations reg}

uuidToFileName :: UUID -> String
uuidToFileName = map (\c -> if c == '-' then '_' else c) . show

schemaScriptName, schemaModuleName :: UUID -> String
schemaScriptName commitId =
  "schema_" <> uuidToFileName commitId
schemaModuleName commitId =
  "Schema_" <> uuidToFileName commitId

migrationScriptName, migrationModuleName :: UUID -> UUID -> String
migrationScriptName fromId toId =
  "migration_" <> uuidToFileName fromId <> "_to_" <> uuidToFileName toId
migrationModuleName fromId toId =
  "Migration_" <> uuidToFileName fromId <> "_To_" <> uuidToFileName toId

writeSchemaFile :: MigrateCmdLine -> MigrationRegistry -> String -> String -> String -> IO FilePath
writeSchemaFile _ reg extension fileNm content = do
  let path = migrationRegistrySrcDir reg </> (fileNm <.> extension)

  putStrLn ("Writing schema to " ++ path ++ "...")
  createDirectoryIfMissing True (migrationRegistrySrcDir reg)
  writeFile path content

  pure path

schemaFilePath :: MigrationRegistry -> UUID -> FilePath
schemaFilePath reg commitId =
  migrationRegistrySrcDir reg </> schemaModuleName commitId <.> "hs"

schemaFilePathForBackend :: Maybe SomeBeamMigrationBackend -> MigrationRegistry -> UUID -> FilePath
schemaFilePathForBackend Nothing reg commit = schemaFilePath reg commit
schemaFilePathForBackend (Just (SomeBeamMigrationBackend be)) reg commit =
  migrationRegistrySrcDir reg </> schemaScriptName commit <.> backendFileExtension be

registryNewCommitId :: MigrationRegistry -> IO UUID
registryNewCommitId reg = do
  newCommitId <- UUID.nextRandom
  case lookupSchema newCommitId [] reg of
    Just {} -> registryNewCommitId reg
    Nothing -> pure newCommitId

registryHeadCommit :: MigrationRegistry -> UUID
registryHeadCommit reg =
  case migrationRegistryHead reg of
    MigrationHeadDetached commitId -> commitId
    MigrationHeadBranch nm ->
      case lookupBranch reg nm of
        Nothing -> error "Cannot find branch"
        Just branch -> migrationBranchCommit branch

-- hashToUUID :: Hashable a => a -> UUID
-- hashToUUID a =
--   let intSize = finiteBitSize (undefined :: Int)
--       wordsNeeded = (128 + intSize - 1) `div` intSize

--       wordsData = take wordsNeeded (tail (iterate (\seed -> hashWithSalt seed a) 0))

--       uuidData :: Integer
--       uuidData = foldr (\w a' -> a' `shiftL` intSize .|. fromIntegral (fromIntegral w :: Word)) 0 wordsData

--       uuidWord1 = fromIntegral $ (uuidData `shiftR` 96) .&. 0xFFFFFFFF
--       uuidWord2 = fromIntegral $ (uuidData `shiftR` 64) .&. 0xFFFFFFFF
--       uuidWord3 = fromIntegral $ (uuidData `shiftR` 32) .&. 0xFFFFFFFF
--       uuidWord4 = fromIntegral $ uuidData .&. 0xFFFFFFFF

--   in fromWords uuidWord1 uuidWord2 uuidWord3 uuidWord4

metadataComment :: String -> SchemaMetaData -> String
metadataComment commentMarker metadata =
  let encoded = map BS.unpack (BS.lines (Yaml.encode metadata))
  in unlines ( [commentMarker <> " + BEAM-MIGRATE"] <>
               map ((commentMarker <> " + ") <>) encoded <>
               [commentMarker <> " + END-BEAM-MIGRATE"])

parseMetaData :: BeamDeserializers cmd -> Value -> Parser SchemaMetaData
parseMetaData d =
  withObject "SchemaMetaData" $ \o ->
  SchemaMetaData <$> o .: "commit" <*> o .: "formats" <*> o .: "createdOn"
                 <*> (Schema <$> (parseSchema =<< o .: "schema"))
  where
    parseSchema =
      fmap mconcat .
      mapM (withObject "SchemaMetaData.schema" $ \o ->
            do specificity <- o .: "specificity"
               predicates <- o .: "predicates"
               -- TODO parse dummy if we can't parse
               fmap (fmap (specificity,)) (mapM (beamDeserialize d) predicates))

predsForBackend :: BeamMigrationBackend be m -> [ (HS.HashSet PredicateSpecificity, SomeDatabasePredicate) ]
                -> [ SomeDatabasePredicate ]
predsForBackend be = predsForBackendNamed (backendName be)

predsForBackendNamed :: String -> [ (HS.HashSet PredicateSpecificity, SomeDatabasePredicate) ]
                     -> [SomeDatabasePredicate]
predsForBackendNamed be preds =
  let ourSources = HS.fromList [ PredicateSpecificityAllBackends, PredicateSpecificityOnlyBackend be ]
      applicablePreds = map snd (filter (not . HS.null . HS.intersection ourSources . fst) preds)
  in applicablePreds

withMetadata, withoutMetadata :: (Eq a, IsString a) => [a] -> [a]
withMetadata =
    takeWhile (/="-- + END-BEAM-MIGRATE") .
    dropWhile (/="-- + BEAM-MIGRATE")
withoutMetadata =
    takeWhile (/="-- + BEAM-MIGRATE")

readSchemaMetaData :: MigrationRegistry
                   -> BeamMigrationBackend be m
                   -> UUID
                   -> IO SchemaMetaData
readSchemaMetaData reg BeamMigrationBackend { backendPredicateParsers = parsers } commitId = do
  d <- withMetadata .
       BS.lines <$>
       BS.readFile (schemaFilePath reg commitId)
  case d of
    [] -> fail "Invalid data in schema"
    _:d' | Just realData <- BS.unlines <$> traverse (BS.stripPrefix "-- + ") d'
         , Right metadataV <- Yaml.decodeEither' realData
         -> case Yaml.parseEither (parseMetaData parsers) metadataV of
             Left err -> fail ("Could not parse metadata: " ++ show err)
             Right metadata -> pure metadata
         | otherwise -> fail "Invalid data in schema"

predicateFetchSourceBackend :: PredicateFetchSource -> Maybe ModuleName
predicateFetchSourceBackend (PredicateFetchSourceCommit be _) = be
predicateFetchSourceBackend (PredicateFetchSourceDbHead (MigrationDatabase be _) _) = Just be
predicateFetchSourceBackend PredicateFetchSourceEmpty = Nothing

predicateSourceWithBackend :: ModuleName -> PredicateFetchSource -> PredicateFetchSource
predicateSourceWithBackend nm (PredicateFetchSourceCommit _ c) = PredicateFetchSourceCommit (Just nm) c
predicateSourceWithBackend nm (PredicateFetchSourceDbHead (MigrationDatabase _ conn) ref) =
  PredicateFetchSourceDbHead (MigrationDatabase nm conn) ref
predicateSourceWithBackend _ PredicateFetchSourceEmpty = PredicateFetchSourceEmpty

parsePredicateFetchSourceSpec :: MigrateCmdLine -> MigrationRegistry -> Text
                              -> IO PredicateFetchSource
parsePredicateFetchSourceSpec cmdLine reg t
  | t == "0" = pure PredicateFetchSourceEmpty
  | Just t' <- T.stripPrefix "HEAD" t =
      let mod' = parsePredicateFetchSourceModulePart t'
      in pure (PredicateFetchSourceCommit mod' (registryHeadCommit reg))
  | Just t' <- T.stripPrefix "DB" t = do
      let (t'', db) = parsePredicateFetchSourceDbName t'
          ref = parsePredicateFetchSourceDbRef t''
      db' <-
        case db of
          Nothing ->
            case migrateDatabase cmdLine of
              Nothing -> fail "Ambiguous database: DB given with no -d option or database specified"
              Just cmdLineDb -> pure cmdLineDb
          Just explicitDb -> pure explicitDb

      dbInfo <- lookupDb reg cmdLine { migrateDatabase = Just db' }

      pure (PredicateFetchSourceDbHead dbInfo ref)
  | Just branchNm <- T.stripPrefix "branch/" t = do
      let (branchNm', dbModNm) = T.breakOn "/" branchNm
      case lookupBranch reg branchNm' of
        Nothing -> fail ("No such branch: " ++ T.unpack branchNm')
        Just branch -> do
          let dbMod = if dbModNm == "" then Nothing else Just (ModuleName (T.unpack dbModNm))
          pure (PredicateFetchSourceCommit dbMod (migrationBranchCommit branch))
  | (commitIdTxt, dbModNm) <- T.breakOn "/" t
  , Just commitId <- readMaybe (T.unpack commitIdTxt) = do
      let dbMod = if T.null dbModNm then Nothing else Just (ModuleName (T.unpack (T.tail dbModNm)))
      pure (PredicateFetchSourceCommit dbMod commitId)
  | (t', rest) <- T.break (\c -> c == '/' || c == '!') t
  , Just dbInfo <- HM.lookup (DatabaseName (T.unpack t')) (migrationRegistryDatabases reg) =
      pure (PredicateFetchSourceDbHead dbInfo (parsePredicateFetchSourceDbRef rest))
  | otherwise = fail "Invalid predicate source spec"

  where
    parsePredicateFetchSourceModulePart t'
      | Just mod' <- T.stripPrefix "/" t' = Just (ModuleName (T.unpack mod'))
      | otherwise = Nothing
    parsePredicateFetchSourceDbName t'
      | Just dbName <- T.stripPrefix "/" t' =
          let (dbName', t'') = T.break (\c -> c == '/' || c == '!') dbName
          in (t'', Just (DatabaseName (T.unpack dbName')))
      | otherwise = (t', Nothing)
    parsePredicateFetchSourceDbRef "!" = Nothing
    parsePredicateFetchSourceDbRef "" = Just 0
    parsePredicateFetchSourceDbRef dbRef
      | (dbRef', "") <- T.span (=='^') dbRef =
          Just (T.length dbRef')
      | otherwise = error "Invalid dbref"

registryMigrationGraph :: MigrationRegistry -> Gr RegisteredSchemaInfo RegisteredMigrationInfo
registryMigrationGraph reg =
  let schemaIdxs = zip [0..] (migrationRegistrySchemas reg)
      schemaIdToIdxMap = HM.fromList (map (\(i, schema) -> (registeredSchemaInfoHash schema, i)) schemaIdxs)
      schemaIdToIdx = flip HM.lookup schemaIdToIdxMap

      migrations = mapMaybe (\mig -> (,,mig) <$> schemaIdToIdx (registeredMigrationInfoSource mig)
                                             <*> schemaIdToIdx (registeredMigrationInfoResult mig))
                            (migrationRegistryMigrations reg)

  in mkGraph schemaIdxs migrations

sha256' :: BS.ByteString -> Word256
sha256' d = let digest :: Crypto.Digest Crypto.SHA256
                digest = Crypto.hash d
            in case filter (null . snd) (readHex (show digest)) of
                 [(x, _)] -> x
                 _ -> error "Can't parse digest"

sha256 :: String -> Word256
sha256 = sha256' . BS.pack

abortEdits :: MigrateCmdLine -> Bool -> IO ()
abortEdits cmdLine force =
  updatingRegistry cmdLine (abortEdits' force)

abortEdits' :: Bool -> MigrationRegistry -> IO ((), MigrationRegistry)
abortEdits' force reg =
  let reg' = reg { migrationRegistryMode = BeamMigrateReady }

      tryAbort flNm flHash = do
        flExists <- doesFileExist flNm
        if flExists
          then do
            doDelete <- if force
                        then pure True
                        else do
                          flContents <- BS.readFile flNm
                          let actualHash = sha256' flContents
                          return (actualHash == flHash)

            if doDelete
              then do
                removeFile flNm
                pure ((), reg')
              else fail "WARNING: the editing files have been modified use '--force' to force abort"
          else pure ((), reg')
  in case migrationRegistryMode reg of
       BeamMigrateReady -> pure ((), reg)
       BeamMigrateEditingMigration tmpFile _ _ hash -> tryAbort tmpFile hash
       BeamMigrateCreatingSchema tmpFile _ hash -> tryAbort tmpFile hash
