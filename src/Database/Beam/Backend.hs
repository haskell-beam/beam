{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Database.Beam.Backend where

import Database.Beam.Types
import Database.Beam.Schema
import Database.Beam.SQL.Types
import Database.Beam.SQL

import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad

import Data.String
import Data.Maybe
import Data.List
import Data.Proxy
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Map as M

import Database.HDBC

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

createStmtFor :: (Table t) => Beam db m -> Text -> Proxy t -> SQLCreateTable
createStmtFor beam name (table :: Proxy t) =
    let tblSchema = reifyTableSchema table
        tblSchema' = map addPrimaryKeyConstraint tblSchema
        tblSchemaInDb' = map (second (adjustColDescForBackend beam)) tblSchema'

        addPrimaryKeyConstraint (name, sch)
            | any (==name) primaryKeyFields = (name, sch { csConstraints = SQLPrimaryKey:csConstraints sch })
            | otherwise = (name, sch)

        _fieldName' :: Columnar' (TableField t) x -> Text
        _fieldName' (Columnar' x) = _fieldName x

        primaryKeyFields = pkAllValues (Proxy :: Proxy t) _fieldName' (primaryKey (tblFieldSettings :: TableSettings t))
    in SQLCreateTable name (tblSchemaInDb')

migrateDB :: MonadIO m => DatabaseSettings db -> Beam db m -> [MigrationAction] -> m ()
migrateDB db beam actions =
  forM_ actions $ \action ->
      do liftIO (putStrLn (concat ["Performing ", show action]))

         case action of
           MACreateTable name t -> do let stmt = createStmtFor beam name t
                                          (sql, vals) = ppSQL (CreateTable stmt)
                                      liftIO (putStrLn (concat ["Will run SQL:\n", sql]))
                                      withHDBCConnection beam (\conn -> liftIO $ do runRaw conn sql
                                                                                    commit conn)
                                      liftIO (putStrLn "Done...")

autoMigrateDB db beam =
    do actDBSchema <- withHDBCConnection beam hdbcSchema
       let comparison = compareSchemas beam actDBSchema db

       case comparison of
         Migration actions -> do liftIO $ putStrLn (concat ["Comparison result: ", show actions])
                                 migrateDB db beam actions
         Unknown -> liftIO $ putStrLn "Unknown comparison"

openDatabase :: (BeamBackend dbSettings, MonadIO m, Database db) => DatabaseSettings db -> dbSettings -> m (Beam db m)
openDatabase db dbSettings =
  do beam <- openBeam db dbSettings
     autoMigrateDB db beam

     return beam
