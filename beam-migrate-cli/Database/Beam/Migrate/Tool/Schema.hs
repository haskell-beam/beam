module Database.Beam.Migrate.Tool.Schema where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate
import Database.Beam.Migrate.Backend

import Data.String (fromString)
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID (UUID)

data LogEntryT f
  = LogEntry
  { _logEntryId       :: C f Int
  , _logEntryCommitId :: C f Text
  , _logEntryDate     :: C f LocalTime
  } deriving Generic

instance Beamable LogEntryT
type LogEntry = LogEntryT Identity
deriving instance Show LogEntry

instance Table LogEntryT where
  data PrimaryKey LogEntryT f = LogEntryKey (C f Int)
    deriving Generic
  primaryKey = LogEntryKey <$> _logEntryId

instance Beamable (PrimaryKey LogEntryT)

type LogEntryKey = PrimaryKey LogEntryT Identity
deriving instance Show LogEntryKey

newtype BeamMigrateVersionT f
  = BeamMigrateVersion
  { _beamMigrateVersion :: C f Int
  } deriving Generic

instance Beamable BeamMigrateVersionT
type BeamMigrateVersion = BeamMigrateVersionT Identity
deriving instance Show BeamMigrateVersion

instance Table BeamMigrateVersionT where
  data PrimaryKey BeamMigrateVersionT f = BeamMigrateVersionKey (C f Int)
    deriving Generic
  primaryKey = BeamMigrateVersionKey <$> _beamMigrateVersion

instance Beamable (PrimaryKey BeamMigrateVersionT)

type BeamMigrateVersionKey = PrimaryKey BeamMigrateVersionT Identity
deriving instance Show BeamMigrateVersionKey

-- Database
data BeamMigrateDb entity
  = BeamMigrateDb
  { _beamMigrateVersionTbl :: entity (TableEntity BeamMigrateVersionT)
  , _beamMigrateLogEntries :: entity (TableEntity LogEntryT)
  } deriving Generic

instance Database be BeamMigrateDb

beamMigratableDb :: forall cmd be hdl m
                  . ( Sql92SaneDdlCommandSyntax cmd
                    , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
                    , MonadBeam cmd be hdl m )
                 => CheckedDatabaseSettings be BeamMigrateDb
beamMigratableDb = runMigrationSilenced beamMigrateDbMigration

beamMigrateDb :: forall be cmd hdl m
               . ( Sql92SaneDdlCommandSyntax cmd
                 , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
                 , MonadBeam cmd be hdl m )
               => DatabaseSettings be BeamMigrateDb
beamMigrateDb = unCheckDatabase beamMigratableDb

beamMigrateDbMigration ::  forall cmd be hdl m
                        . ( Sql92SaneDdlCommandSyntax cmd
                          , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
                          , MonadBeam cmd be hdl m )
                       => Migration cmd (CheckedDatabaseSettings be BeamMigrateDb)
beamMigrateDbMigration =
  BeamMigrateDb <$> createTable "beam_version"
                      (BeamMigrateVersion (field "version" int notNull))
                <*> createTable "beam_migration"
                      (LogEntry (field "id" int notNull) (field "commitId" (varchar Nothing) notNull)
                                (field "date" timestamp notNull))

beamMigrateSchemaVersion :: Int
beamMigrateSchemaVersion = 1

getLatestLogEntry :: forall be cmd hdl m
                   . ( IsSql92Syntax cmd
                     , HasQBuilder (Sql92SelectSyntax cmd)
                     , Sql92ReasonableMarshaller be
                     , Sql92SanityCheck cmd
                     , Sql92SaneDdlCommandSyntax cmd
                     , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
                     , MonadBeam cmd be hdl m )
                  => m (Maybe LogEntry)
getLatestLogEntry =
  runSelectReturningOne (select $
                         limit_ 1 $
                         orderBy_ (desc_ . _logEntryId) $
                         all_ (_beamMigrateLogEntries (beamMigrateDb @be @cmd)))

updateSchemaToCurrent :: forall be cmd hdl m
                       . ( IsSql92Syntax cmd
                         , Sql92SanityCheck cmd
                         , Sql92ReasonableMarshaller be
                         , Sql92SaneDdlCommandSyntax cmd
                         , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
                         , MonadBeam cmd be hdl m )
                      => m ()
updateSchemaToCurrent =
  runInsert (insert (_beamMigrateVersionTbl (beamMigrateDb @be @cmd)) (insertValues [BeamMigrateVersion beamMigrateSchemaVersion]))

recordCommit :: forall be cmd hdl m
             . ( IsSql92Syntax cmd
               , Sql92SanityCheck cmd
               , Sql92SaneDdlCommandSyntax cmd
               , HasQBuilder (Sql92SelectSyntax cmd)
               , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
               , HasSqlValueSyntax (Sql92ValueSyntax cmd) Text
               , Sql92ReasonableMarshaller be
               , MonadBeam cmd be hdl m )
             => UUID -> m ()
recordCommit commitId = do
  let commitIdTxt = fromString (show commitId)

  logEntry <- getLatestLogEntry
  let nextLogEntryId = maybe 0 (succ . _logEntryId) logEntry

  runInsert (insert (_beamMigrateLogEntries (beamMigrateDb @be @cmd))
                    (insertExpressions
                     [ LogEntry (val_ nextLogEntryId)
                                (val_ commitIdTxt)
                                currentTimestamp_]))

reportDdlErrors :: IO (Either DdlError a) -> IO a
reportDdlErrors go = do
  res <- go
  case res of
    Left err -> fail ("DDL error: " ++ show err)
    Right  x -> pure x

-- Ensure the backend tables exist
ensureBackendTables :: forall be cmd hdl
                     . String
                    -> BeamMigrationBackend be cmd hdl
                    -> IO ()
ensureBackendTables connStr
                    be@BeamMigrationBackend { backendTransact = transact
                                            , backendRenderSyntax = renderBeSyntax } =
  do backendSchemaBuilt <- hasBackendTables connStr be
     if backendSchemaBuilt
       then continueMigrate
       else createSchema

  where
    continueMigrate = fail "ContinueMigrate"
    createSchema = do
      let doStep cmd = do
            liftIO $ putStrLn ("MIGRATE: " ++ renderBeSyntax cmd)
            runNoReturn cmd
      _ <- transact connStr $ do
             _ <- executeMigration doStep (beamMigrateDbMigration @cmd @be)
             updateSchemaToCurrent
      pure ()

hasBackendTables :: String -> BeamMigrationBackend be cmd hdl -> IO Bool
hasBackendTables connStr BeamMigrationBackend { backendTransact = transact
                                              , backendGetDbConstraints = getCs } =
  do cs <- transact connStr getCs
     case cs of
       Left err -> fail ("Could not get db constraints: " ++ show err)
       Right cs' -> pure (any (== p (TableExistsPredicate "beam_version")) cs')
