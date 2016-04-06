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
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Map as M

import Database.HDBC

-- | Passed to 'openDatabase' or 'openDatabaseDebug' to specify
--   whether you want beam to automatically attempt to make the database
--   schema match the haskell schema.
data MigrationStrategy = DontMigrate
                       | AutoMigrate
                         deriving Show

instance Monoid DBSchemaComparison where
    mappend (Migration a) (Migration b) = Migration (a <> b)
    mappend _ Unknown = Unknown
    mappend Unknown _ = Unknown

    mempty = Migration []

reifyDBSchema :: Database db => DatabaseSettings db -> ReifiedDatabaseSchema
reifyDBSchema dbSettings =
    let tables = allTableSettings dbSettings
    in map (\(GenDatabaseTable (DatabaseTable table name)) -> (name, reifyTableSchema table)) tables

defaultBeamCompareSchemas :: Database db => ReifiedDatabaseSchema -> DatabaseSettings db -> DBSchemaComparison
defaultBeamCompareSchemas actual db = execWriter compare
    where dbTables = allTableSettings db

          expected = S.fromList (map (\(GenDatabaseTable (DatabaseTable _ name)) -> name) dbTables)
          actualTables = S.fromList (map fst actual)

          tablesToBeMadeNames = expected S.\\ actualTables
          tablesToBeMade = mapMaybe (\(GenDatabaseTable (DatabaseTable table name)) ->
                                         if name `S.member` tablesToBeMadeNames
                                         then Just (MACreateTable name table)
                                         else Nothing) dbTables

          compare = tell (Migration tablesToBeMade)

hdbcSchema :: (IConnection conn, MonadIO m) => conn -> m ReifiedDatabaseSchema
hdbcSchema conn =
    liftIO $
    do tables <- getTables conn
       forM tables $ \tbl ->
           do descs <- describeTable conn tbl
              return (fromString tbl, map (fromString *** noConstraints) descs)

createStmtFor :: Table t => Beam db m -> Text -> Proxy t -> SQLCreateTable
createStmtFor beam name (table :: Proxy t) =
    let tblSchema = reifyTableSchema table
        tblSchema' = map addPrimaryKeyConstraint tblSchema
        tblSchemaInDb' = map (second (adjustColDescForBackend beam)) tblSchema'

        addPrimaryKeyConstraint (name, sch)
            | elem name primaryKeyFields = (name, sch { csConstraints = SQLPrimaryKey:csConstraints sch })
            | otherwise = (name, sch)

        _fieldName' :: Columnar' (TableField t) x -> Text
        _fieldName' (Columnar' x) = _fieldName x

        primaryKeyFields = pkAllValues _fieldName' (primaryKey (tblFieldSettings :: TableSettings t))
    in SQLCreateTable name tblSchemaInDb'

migrateDB :: MonadIO m => DatabaseSettings db -> Beam db m -> [MigrationAction] -> m ()
migrateDB db beam actions =
  forM_ actions $ \action ->
      do when (beamDebug beam) (liftIO (putStrLn ("Performing " ++ show action)))

         case action of
           MACreateTable name t -> do let stmt = createStmtFor beam name t
                                          (sql, vals) = ppSQL (CreateTable stmt)
                                      when (beamDebug beam) (liftIO (putStrLn ("Will run SQL:\n" ++sql)))
                                      withHDBCConnection beam (\conn -> liftIO $ do runRaw conn sql
                                                                                    commit conn)
                                      when (beamDebug beam) (liftIO (putStrLn "Done..."))

autoMigrateDB db beam =
    do actDBSchema <- withHDBCConnection beam hdbcSchema
       let comparison = compareSchemas beam actDBSchema db

       case comparison of
         Migration actions -> do when (beamDebug beam) (liftIO $ putStrLn ("Comparison result: " ++ show actions))
                                 migrateDB db beam actions
         Unknown -> when (beamDebug beam) (liftIO $ putStrLn "Unknown comparison")

openDatabaseDebug, openDatabase :: (BeamBackend dbSettings, MonadIO m, Database db) => DatabaseSettings db -> MigrationStrategy -> dbSettings -> m (Beam db m)
openDatabase = openDatabase' False
openDatabaseDebug = openDatabase' True

openDatabase' :: (BeamBackend dbSettings, MonadIO m, Database db) => Bool -> DatabaseSettings db -> MigrationStrategy -> dbSettings -> m (Beam db m)
openDatabase' isDebug db strat dbSettings =
  do beam <- openBeam db dbSettings
     let beam' = beam { beamDebug = isDebug }
     case strat of
       DontMigrate -> pure ()
       AutoMigrate -> autoMigrateDB db beam'

     return beam'


dumpSchema :: Database db => DatabaseSettings db -> IO ()
dumpSchema (db :: DatabaseSettings db) = do
  putStrLn "Dumping database schema ..."
  schema <- showSchema db
  mapM_ putStrLn $ lines schema

showSchema :: Database db => DatabaseSettings db -> IO String
showSchema (db :: DatabaseSettings db) =
    do let createTableStmts = allTables (\(DatabaseTable tbl name) -> createStmtFor debugBeam name tbl) db
       return $ unlines (map (fst . ppSQL . CreateTable) createTableStmts)
    where debugBeam ::Beam db Identity
          debugBeam = Beam { beamDbSettings = db
                           , beamDebug = False
                           , closeBeam = return ()
                           , compareSchemas = \_ _ -> Unknown
                           , adjustColDescForBackend = id
                           , getLastInsertedRow = \_ -> return []
                           , withHDBCConnection = \_ -> error "trying to run in debug mode" }

