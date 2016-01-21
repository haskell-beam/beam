{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( module Database.Beam.Query.Types
    , module Database.Beam.Query.Combinators

    , beamTxn, insertInto, query, queryList
    , ref, justRef )where

import Database.Beam.Query.Types
import Database.Beam.Query.Combinators
import Database.Beam.Query.Internal

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Fields
import Database.Beam.Types
import Database.Beam.SQL
import Database.Beam.SQL.Types

import Control.Applicative
import Control.Arrow
import Control.Monad.Trans
import Control.Monad.Writer (tell, execWriter, Writer)
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import Data.Monoid hiding (All)
import Data.Proxy
import Data.Data
import Data.List (find)
import Data.Maybe
import Data.Convertible
import Data.String (fromString)
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Set as S
import qualified Data.Text as T

import Database.HDBC

import Unsafe.Coerce

-- * Query

runSQL' :: IConnection conn => conn -> SQLCommand -> IO (Either String (IO (Maybe [SqlValue])))
runSQL' conn cmd = do
  let (sql, vals) = ppSQL cmd
  putStrLn ("Will execute " ++ sql ++ " with " ++ show vals)
  stmt <- prepare conn sql
  execute stmt vals
  return (Right (fetchRow stmt))

-- * Data updating

insertToSQL :: Table table => T.Text -> table Identity -> SQLInsert
insertToSQL name (table :: table Identity) = SQLInsert name
                                                       (makeSqlValues table)

runInsert :: (MonadIO m, Table table, FromSqlValues (table Identity)) => T.Text -> table Identity -> Beam d m -> m (Either String (table Identity))
runInsert tableName (table :: table Identity) beam =
    do let insertCmd = Insert sqlInsert
           sqlInsert@(SQLInsert tblName sqlValues) = (insertToSQL tableName table)
       withHDBCConnection beam (\conn -> liftIO (runSQL' conn insertCmd))

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

           hasAutoIncrementConstraint (SQLColumnSchema { csConstraints = cs }) = isJust (find (== SQLAutoIncrement) cs)

       insertedValues <- if hasNullAutoIncrements
                         then liftIO (putStrLn "Got null auto increments") >> getLastInsertedRow beam tblName
                         else return sqlValues
       return (fromSqlValues insertedValues)

insertInto :: (MonadIO m, Functor m, Table table, FromSqlValues (table Identity)) =>
              DatabaseTable db table -> table Identity -> BeamT e db m (table Identity)
insertInto (DatabaseTable _ name) data_ =
    BeamT (\beam -> toBeamResult <$> runInsert name data_ beam)

queryToSQL :: Projectible a => (forall s. Q db s a) -> SQLSelect
queryToSQL q = let (res, qb) = runState (runQ q) emptyQb
                   emptyQb = QueryBuilder 0 Nothing (ValE (SqlBool True)) Nothing Nothing
                   projection = map (\(GenQExpr q) -> SQLAliased (optimizeExpr q) Nothing) (project res)

                   optimizeExpr :: QExpr a -> SQLExpr
                   optimizeExpr = mkSqlExpr . runIdentity . rewriteExprM (return . allExprOpts)

                   sel = SQLSelect
                         { selProjection = SQLProj projection
                         , selFrom = qbFrom qb
                         , selWhere = optimizeExpr (qbWhere qb)
                         , selGrouping = Nothing
                         , selOrderBy = []
                         , selLimit = qbLimit qb
                         , selOffset = qbOffset qb }
               in sel

-- * BeamT actions

beamTxn :: MonadIO m => Beam db m -> (DatabaseSettings db -> BeamT e db m a) -> m (BeamResult e a)
beamTxn beam action = do res <- runBeamT (action (beamDbSettings beam)) beam
                         withHDBCConnection beam $
                             case res of
                               Success  x -> liftIO . commit
                               Rollback e -> liftIO . rollback
                         return res

toBeamResult :: Either String a -> BeamResult e a
toBeamResult = (Rollback . InternalError) ||| Success

runQuery :: ( MonadIO m
            , FromSqlValues (QExprToIdentity a)
            , Projectible a ) =>
            (forall s. Q db s a) -> Beam db m -> m (Either String (Source m (QExprToIdentity a)))
runQuery q beam =
    do let selectCmd = Select (queryToSQL q)

       res <- withHDBCConnection beam $ \conn ->
              do liftIO $ runSQL' conn selectCmd

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

query :: (MonadIO m, Functor m, FromSqlValues (QExprToIdentity a), Projectible a) => (forall s. Q db s a) -> BeamT e db m (Source (BeamT e db m) (QExprToIdentity a))
query q = BeamT $ \beam ->
          do res <- runQuery q beam
             case res of
               Right x -> return (Success (transPipe (BeamT . const . fmap Success) x))
               Left err -> return (Rollback (InternalError err))

queryList :: (MonadIO m, Functor m, FromSqlValues (QExprToIdentity a), Projectible a) => (forall s. Q db s a) -> BeamT e db m [QExprToIdentity a]
queryList q = do src <- query q
                 src $$ C.consume

fromSqlValues :: FromSqlValues a => [SqlValue] -> Either String a
fromSqlValues vals =
    case runState (runErrorT fromSqlValues') vals of
      (Right a, []) -> Right a
      (Right _,  _) -> Left "fromSqlValues: Not all values were consumed"
      (Left err, _) -> Left err

-- * Generic combinators

ref :: Table t => t c -> ForeignKey t c
ref = ForeignKey . primaryKey
justRef :: Table related =>
           related Identity -> ForeignKey related (Nullable Identity)
justRef (e :: related Identity) = ForeignKey (pkChangeRep (Proxy :: Proxy related) just (primaryKey e))
    where just :: Columnar' Identity a -> Columnar' (Nullable Identity) a
          just (Columnar' x) = Columnar' (Just (unsafeCoerce x)) -- TODO : Why is unsafecoerce necessary here?

-- nothingRef :: Table related =>
--               Query db (related Identity) -> ForeignKey related (Nullable Identity)
-- nothingRef (_ :: Query db (related Identity)) = ForeignKey (pkChangeRep (Proxy :: Proxy related) nothing (primaryKey fieldSettings))
--     where nothing :: Columnar' (TableField related) a -> Columnar' (Nullable Identity) a
--           nothing x = Columnar' Nothing

--           fieldSettings :: TableSettings related
--           fieldSettings = tblFieldSettings
