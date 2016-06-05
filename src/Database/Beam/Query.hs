{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( -- * Query type
      module Database.Beam.Query.Types

    -- * General query combinators
    , module Database.Beam.Query.Combinators

    -- * Run queries in MonadIO
    , beamTxn, insertInto, query, queryList, getOne
    , updateWhere, saveTo
    , deleteWhere, deleteFrom )
    where

import Database.Beam.Query.Types
import Database.Beam.Query.Combinators
import Database.Beam.Query.Internal

import Database.Beam.Schema.Tables
import Database.Beam.Internal
import Database.Beam.SQL

import Control.Arrow
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Data.Proxy
import Data.List (find)
import Data.Maybe
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Text as T

import Database.HDBC

-- * Query

runSQL' :: IConnection conn => Bool -> conn -> SQLCommand -> IO (Either String (IO (Maybe [SqlValue])))
runSQL' debug conn cmd = do
  let (sql, vals) = ppSQL cmd
  when debug (putStrLn ("Will execute " ++ sql ++ " with " ++ show vals))
  stmt <- prepare conn sql
  _ <- execute stmt vals
  return (Right (fetchRow stmt))

-- * Data insertion, updating, and deletion

insertToSQL :: Table table => T.Text -> table Identity -> SQLInsert
insertToSQL name (table :: table Identity) = SQLInsert name
                                                       (fieldAllValues (\(Columnar' (SqlValue' x)) -> x) (makeSqlValues table))

runInsert :: (MonadIO m, Table table, FromSqlValues (table Identity)) => T.Text -> table Identity -> Beam d m -> m (Either String (table Identity))
runInsert tableName (table :: table Identity) beam =
    do let insertCmd = Insert sqlInsert
           sqlInsert@(SQLInsert tblName sqlValues) = insertToSQL tableName table
       _ <- withHDBCConnection beam (\conn -> liftIO (runSQL' (beamDebug beam) conn insertCmd))

       -- There are three possibilities here:
       --
       --   * we have no autoincrement keys, and so we simply have to return the
       --     newly created QueryTable, or
       --   * we have autoincrement keys, but all the fields marked autoincrement
       --     were non-null. In this case, we have all the information needed to
       --     construct the QueryTable, or
       --   * we have autoincrement keys, and some of the fields were marked null.
       --     In this case, we need to ask the backend for the last inserted row.
       let tableSchema = reifyTableSchema (Proxy :: Proxy table)

           autoIncrementsAreNull = zipWith (\(_, columnSchema) value -> hasAutoIncrementConstraint columnSchema && value == SqlNull) tableSchema sqlValues
           hasNullAutoIncrements = or autoIncrementsAreNull

           hasAutoIncrementConstraint SQLColumnSchema { csConstraints = cs } = isJust (find (== SQLAutoIncrement) cs)

       insertedValues <- if hasNullAutoIncrements
                         then getLastInsertedRow beam tblName
                         else return sqlValues
       return (fromSqlValues insertedValues)

-- | Insert the given row value into the table specified by the first argument.
insertInto :: (MonadIO m, Table table, FromSqlValues (table Identity)) =>
              DatabaseTable db table -> table Identity -> BeamT e db m (table Identity)
insertInto (DatabaseTable _ name) data_ =
    BeamT (\beam -> toBeamResult <$> runInsert name data_ beam)

updateToSQL :: Table table => T.Text -> table (QExpr s) -> QExpr s Bool -> Maybe SQLUpdate
updateToSQL tblName (setTo :: table (QExpr s)) where_ =
    let setExprs = fieldAllValues (\(Columnar' x) -> optimizeExpr x) setTo
        setColumns = fieldAllValues (\(Columnar' fieldS) -> _fieldName fieldS) (tblFieldSettings :: TableSettings table)

        isInteresting columnName (SQLFieldE (SQLFieldName fName))
            | fName == columnName = Nothing
        isInteresting columnName newE = Just (SQLFieldName columnName, newE)

        assignments = catMaybes (zipWith isInteresting setColumns setExprs)

        where_' = case optimizeExpr where_ of
                    SQLValE (SqlBool True) -> Nothing
                    where_' -> Just where_'

    in case assignments of
         [] -> Nothing
         _  -> Just SQLUpdate
               { uTableNames = [tblName]
               , uAssignments = assignments
               , uWhere = where_' }

-- | Update every entry in the given table where the third argument yields true, using the second
-- argument to give the new values.
updateWhere :: (MonadIO m, Table tbl) => DatabaseTable db tbl -> (tbl (QExpr s) -> tbl (QExpr s)) -> (tbl (QExpr s) -> QExpr s Bool) -> BeamT e db m ()
updateWhere tbl@(DatabaseTable _ name :: DatabaseTable db tbl) mkAssignments mkWhere =
    do let assignments = mkAssignments tblExprs
           where_ = mkWhere tblExprs

           tblExprs = changeRep (\(Columnar' fieldS) -> Columnar' (QExpr (SQLFieldE (QField name Nothing (_fieldName fieldS))))) (tblFieldSettings :: TableSettings tbl)

       case updateToSQL name assignments where_ of
         Nothing -> pure () -- Assignments were empty, so do nothing
         Just upd ->
             let updateCmd = Update upd
             in BeamT $ \beam ->
                 withHDBCConnection beam $ \conn ->
                     do _ <- liftIO (runSQL' (beamDebug beam) conn updateCmd)
                        pure (Success ())

-- | Use the 'PrimaryKey' of the given table entry to update the corresponding table row in the
-- database.
saveTo :: (MonadIO m, Table tbl) => DatabaseTable db tbl -> tbl Identity -> BeamT e db m ()
saveTo tbl (newValues :: tbl Identity) =
    updateWhere tbl (\_ -> tableVal newValues) (val_ (primaryKey newValues) `references_`)

-- | Delete all entries in the given table matched by the expression
deleteWhere :: (MonadIO m, Table tbl) => DatabaseTable db tbl -> (tbl (QExpr s) -> QExpr s Bool) -> BeamT e db m ()
deleteWhere (DatabaseTable _ name :: DatabaseTable db tbl) mkWhere =
    let tblExprs = changeRep (\(Columnar' fieldS) -> Columnar' (QExpr (SQLFieldE (QField name Nothing (_fieldName fieldS))))) (tblFieldSettings :: TableSettings tbl)

        cmd = Delete SQLDelete
              { dTableName = name
              , dWhere = case optimizeExpr (mkWhere tblExprs) of
                           SQLValE (SqlBool True) -> Nothing
                           where_ -> Just where_ }
    in BeamT $ \beam ->
        withHDBCConnection beam $ \conn ->
            do _ <- liftIO (runSQL' (beamDebug beam) conn cmd)
               pure (Success ())

-- | Delete the entry referenced by the given 'PrimaryKey' in the given table.
deleteFrom :: (MonadIO m, Table tbl) => DatabaseTable db tbl -> PrimaryKey tbl Identity -> BeamT e db m ()
deleteFrom tbl pkToDelete = deleteWhere tbl (\tbl -> primaryKey tbl ==. val_ pkToDelete)

-- * BeamT actions

-- | Run the 'BeamT' action in a database transaction. On successful
-- completion, the transaction will be committed. Use 'throwE' to
-- stop the transaction and report an error.
beamTxn :: MonadIO m => Beam db m -> (DatabaseSettings db -> BeamT e db m a) -> m (BeamResult e a)
beamTxn beam action = do res <- runBeamT (action (beamDbSettings beam)) beam
                         withHDBCConnection beam $
                             case res of
                               Success  _ -> liftIO . commit
                               Rollback _ -> liftIO . rollback
                         return res

toBeamResult :: Either String a -> BeamResult e a
toBeamResult = (Rollback . InternalError) ||| Success

runQuery :: ( MonadIO m
            , FromSqlValues (QExprToIdentity a)
            , Projectible a
            , IsQuery q ) =>
            q db s a -> Beam db m -> m (Either String (Source m (QExprToIdentity a)))
runQuery q beam =
    do let selectCmd = Select select
           (_, _, select) = queryToSQL' (toQ q) 0

       res <- withHDBCConnection beam $ \conn ->
                liftIO $ runSQL' (beamDebug beam) conn selectCmd

       case res of
         Left err -> return (Left err)
         Right fetchRow -> do let source = do row <- liftIO fetchRow
                                              case row of
                                                Just row ->
                                                    case fromSqlValues row of
                                                      Left err -> fail err
                                                      Right  q -> do yield q
                                                                     source
                                                Nothing -> return ()
                              return (Right source)

-- | Run the given query in the transaction and yield a 'Source' that can be used to read results
-- incrementally. If your result set is small and you want to just get a list, use 'queryList'.
query :: (IsQuery q, MonadIO m, FromSqlValues (QExprToIdentity a), Projectible a) => q db () a -> BeamT e db m (Source (BeamT e db m) (QExprToIdentity a))
query q = BeamT $ \beam ->
          do res <- runQuery q beam
             case res of
               Right x -> return (Success (transPipe (BeamT . const . fmap Success) x))
               Left err -> return (Rollback (InternalError err))

-- | Execute 'query' and use the 'Data.Conduit.List.consume' function to return a list of
-- results. Best used for small result sets.
queryList :: (IsQuery q, MonadIO m, FromSqlValues (QExprToIdentity a), Projectible a) => q db () a -> BeamT e db m [QExprToIdentity a]
queryList q = do src <- query q
                 src $$ C.consume

-- | Execute the query using 'query' and return exactly one result. The return value will be
-- 'Nothing' if either zero or more than one values were returned.
getOne :: (IsQuery q, MonadIO m, FromSqlValues (QExprToIdentity a), Projectible a) => q db () a -> BeamT e db m (Maybe (QExprToIdentity a))
getOne q =
    do let justOneSink = await >>= \x ->
                         case x of
                           Nothing -> return Nothing
                           Just  x -> noMoreSink x
           noMoreSink x = await >>= \nothing ->
                          case nothing of
                            Nothing -> return (Just x)
                            Just  _ -> return Nothing
       src <- query q
       src $$ justOneSink

fromSqlValues :: FromSqlValues a => [SqlValue] -> Either String a
fromSqlValues vals =
    case runState (runExceptT fromSqlValues') vals of
      (Right a, []) -> Right a
      (Right _,  _) -> Left "fromSqlValues: Not all values were consumed"
      (Left err, _) -> Left err
