{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Contains a schema for beam migration tools. Used by the CLI and
-- the managed migrations support here.
module Database.Beam.Migrate.Log where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate
import Database.Beam.Migrate.Backend

import Control.Monad (when)

import Data.String (fromString)
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID.Types (UUID)

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
beamMigratableDb = runMigrationSilenced $ beamMigrateDbMigration @cmd @be @hdl @m

beamMigrateDb :: forall be cmd hdl m
               . ( Sql92SaneDdlCommandSyntax cmd
                 , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
                 , MonadBeam cmd be hdl m )
               => DatabaseSettings be BeamMigrateDb
beamMigrateDb = unCheckDatabase $ beamMigratableDb @cmd @be @hdl @m

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
                         all_ (_beamMigrateLogEntries (beamMigrateDb @be @cmd @hdl @m)))

updateSchemaToCurrent :: forall be cmd hdl m
                       . ( IsSql92Syntax cmd
                         , Sql92SanityCheck cmd
                         , Sql92ReasonableMarshaller be
                         , Sql92SaneDdlCommandSyntax cmd
                         , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
                         , MonadBeam cmd be hdl m )
                      => m ()
updateSchemaToCurrent =
  runInsert (insert (_beamMigrateVersionTbl (beamMigrateDb @be @cmd @hdl @m)) (insertValues [BeamMigrateVersion beamMigrateSchemaVersion]))

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

  runInsert (insert (_beamMigrateLogEntries (beamMigrateDb @be @cmd @hdl @m))
                    (insertExpressions
                     [ LogEntry (val_ nextLogEntryId)
                                (val_ commitIdTxt)
                                currentTimestamp_]))

-- Ensure the backend tables exist
ensureBackendTables :: forall be cmd hdl m
                     . BeamMigrationBackend cmd be hdl m
                    -> m ()
ensureBackendTables be@BeamMigrationBackend { backendGetDbConstraints = getCs } =
  do backendSchemaBuilt <- checkForBackendTables be
     if backendSchemaBuilt
       then continueMigrate
       else createSchema

  where
    doStep cmd = runNoReturn cmd

    continueMigrate = do
      maxVersion <-
        runSelectReturningOne $ select $
        aggregate_ (\v -> max_ (_beamMigrateVersion v)) $
        all_ (_beamMigrateVersionTbl (beamMigrateDb @be @cmd @hdl @m))

      case maxVersion of
        Nothing -> cleanAndCreateSchema
        Just Nothing -> cleanAndCreateSchema
        Just (Just maxVersion')
          | maxVersion' > beamMigrateSchemaVersion ->
              fail "This database is being managed by a newer version of beam-migrate"
          | maxVersion' < beamMigrateSchemaVersion ->
              fail "This database is being managed by an older version of beam-migrate, but there are no older versions"
          | otherwise -> pure ()

    cleanAndCreateSchema = do
      cs <- getCs
      let migrationLogExists = any (== p (TableExistsPredicate "beam_migration")) cs

      when migrationLogExists $ do
        Just totalCnt <-
          runSelectReturningOne $ select $
          aggregate_ (\_ -> as_ @Int countAll_) $
          all_ (_beamMigrateLogEntries (beamMigrateDb @be @cmd @hdl @m))
        when (totalCnt > 0) (fail "beam-migrate: No versioning information, but log entries present")
        runNoReturn (dropTableCmd (dropTableSyntax "beam_migration"))

      runNoReturn (dropTableCmd (dropTableSyntax "beam_version"))

      createSchema

    createSchema = do
      _ <- executeMigration doStep (beamMigrateDbMigration @cmd @be @hdl @m)
      updateSchemaToCurrent

checkForBackendTables :: BeamMigrationBackend cmd be hdl m -> m Bool
checkForBackendTables BeamMigrationBackend { backendGetDbConstraints = getCs } =
  do cs <- getCs
     pure (any (== p (TableExistsPredicate "beam_version")) cs)
