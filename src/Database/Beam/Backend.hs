module Database.Beam.Backend where

import Database.Beam.Internal
import Database.Beam.Schema
import Database.Beam.SQL.Types
import Database.Beam.SQL

import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad

import Data.String
import Data.Maybe
import Data.List
import Data.Proxy
import Data.Text (Text, unpack)
import qualified Data.Set as S
import qualified Data.Map as M

import Database.HDBC

instance Show (MigrationAction be) where
    show (MACreateTable name t) = concat ["MACreateTable ", unpack name , " ", show (allBeamValues (\(Columnar' tf) -> show tf) t)]
deriving instance Show (DBSchemaComparison be)

-- | Passed to 'openDatabase' or 'openDatabaseDebug' to specify
--   whether you want beam to automatically attempt to make the database
--   schema match the haskell schema.
data MigrationStrategy = DontMigrate
                       | AutoMigrate
                         deriving Show

instance Monoid (DBSchemaComparison be) where
    mappend (Migration a) (Migration b) = Migration (a <> b)
    mappend _ Unknown = Unknown
    mappend Unknown _ = Unknown

    mempty = Migration []

reifyDBSchema :: Database db => DatabaseSettings be db -> ReifiedDatabaseSchema
reifyDBSchema dbSettings =
    let tables = allTableSettings dbSettings
    in map (\(GenDatabaseTable (DatabaseTable _ name table)) -> (name, reifyTableSchema table)) tables

defaultBeamCompareSchemas :: Database db => ReifiedDatabaseSchema -> DatabaseSettings be db -> DBSchemaComparison be
defaultBeamCompareSchemas actual db = execWriter compare
    where dbTables = allTableSettings db

          expected = S.fromList (map (\(GenDatabaseTable (DatabaseTable _ name _)) -> name) dbTables)
          actualTables = S.fromList (map fst actual)

          tablesToBeMadeNames = expected S.\\ actualTables
          tablesToBeMade = mapMaybe (\(GenDatabaseTable (DatabaseTable _ name tbl)) ->
                                         if name `S.member` tablesToBeMadeNames
                                         then Just (MACreateTable name tbl)
                                         else Nothing) dbTables

          compare = tell (Migration tablesToBeMade)

hdbcSchema :: (IConnection conn, MonadIO m) => conn -> m ReifiedDatabaseSchema
hdbcSchema conn =
    liftIO $
    do tables <- getTables conn
       forM tables $ \tbl ->
           do descs <- describeTable conn tbl
              return (fromString tbl, map (fromString *** noConstraints) descs)

createStmtFor :: Table t => Beam be db m -> Text -> TableSettings be t -> SQLCreateTable
createStmtFor beam name tblFieldSettings =
    let tblSchema = reifyTableSchema tblFieldSettings
        tblSchema' = map addPrimaryKeyConstraint tblSchema
        tblSchemaInDb' = map (second (adjustColDescForBackend beam)) tblSchema'

        addPrimaryKeyConstraint (name, sch)
            | elem name primaryKeyFields = (name, sch { csConstraints = SQLPrimaryKey:csConstraints sch })
            | otherwise = (name, sch)

        _fieldName' :: Columnar' (TableField be t) x -> Text
        _fieldName' (Columnar' x) = _fieldName x

        primaryKeyFields = allBeamValues _fieldName' (primaryKey tblFieldSettings)
    in SQLCreateTable name tblSchemaInDb'

migrateDB :: (MonadIO m, BeamBackend be) => DatabaseSettings be db -> Beam be db m -> [MigrationAction be] -> m ()
migrateDB db (beam :: Beam be db m) actions =
  forM_ actions $ \action ->
      do when (beamDebug beam) (liftIO (putStrLn ("Performing " ++ show action)))

         case action of
           MACreateTable name t -> do let stmt = createStmtFor beam name t
                                          (sql, vals) = ppSQL (CreateTable stmt :: SQLCommand be)
                                      when (beamDebug beam) (liftIO (putStrLn ("Will run SQL:\n" ++sql)))
                                      beamExecute beam sql []
                                      when (beamDebug beam) (liftIO (putStrLn "Done..."))

autoMigrateDB db beam =
    do actDBSchema <- beamDescribeDatabase beam
       let comparison = compareSchemas beam actDBSchema db

       case comparison of
         Migration actions -> do when (beamDebug beam) (liftIO $ putStrLn ("Comparison result: " ++ show actions))
                                 migrateDB db beam actions
         Unknown -> when (beamDebug beam) (liftIO $ putStrLn "Unknown comparison")

openDatabaseDebug, openDatabase :: (BeamBackend be, MonadIO m, Database db) => DatabaseSettings be db -> MigrationStrategy -> be -> m (Beam be db m)
openDatabase = openDatabase' False
openDatabaseDebug = openDatabase' True

openDatabase' :: (BeamBackend be, MonadIO m, Database db) => Bool -> DatabaseSettings be db -> MigrationStrategy -> be -> m (Beam be db m)
openDatabase' isDebug db strat dbSettings =
  do beam <- openBeam db dbSettings
     let beam' = beam { beamDebug = isDebug }
     case strat of
       DontMigrate -> pure ()
       AutoMigrate -> autoMigrateDB db beam'

     return beam'

dumpSchema :: (Database db, BeamBackend be)  => DatabaseSettings be db -> IO ()
dumpSchema (db :: DatabaseSettings be db) =
    do let createTableStmts = allTables (\(DatabaseTable _ name tbl) -> createStmtFor debugBeam name tbl) db
       putStrLn "Dumping database schema ..."
       mapM_ (putStrLn . fst . (ppSQL :: SQLCommand be -> (String, [BeamBackendValue be])) . CreateTable) createTableStmts
    where debugBeam ::Beam be db Identity
          debugBeam = Beam { beamDbSettings = db
                           , beamDebug = False
                           , closeBeam = return ()
                           , compareSchemas = \_ _ -> Unknown
                           , adjustColDescForBackend = id
                           , getLastInsertedRow = \_ -> return []

                           , beamDescribeDatabase = return []
                           , beamCommit = return ()
                           , beamRollback = return ()
                           , beamExecute = \_ _ -> error "trying to run in debug mode" }
