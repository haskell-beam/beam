{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( module Database.Beam.Query.Types
    , module Database.Beam.Query.Rewrite
    , module Database.Beam.Query.SimpleCombinators
    , module Database.Beam.Query.Combinators

    , runQuery, runInsert, runUpdate, updateEntity
    , EqExprFor(..)
    , findByPrimaryKeyExpr, primaryKeyForTable

    , inBeamTxn, query, insert, save, update, getOne ) where

import Database.Beam.Query.Types
import Database.Beam.Query.Rewrite
import Database.Beam.Query.SimpleCombinators
import Database.Beam.Query.Combinators

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Locate
import Database.Beam.Schema.Fields
import Database.Beam.Types
import Database.Beam.SQL
import Database.Beam.SQL.Types

import Control.Applicative
import Control.Arrow
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Error

import Data.Monoid hiding (All)
import Data.Proxy
import Data.Data
import Data.List (find)
import Data.Maybe
import Data.String (fromString)
import Data.Conduit
import qualified Data.Text as T

import Database.HDBC

-- * Query Compilation

mkSqlTblId :: Int -> T.Text
mkSqlTblId i = fromString (concat ["t", show i])

mkQualifiedFieldName :: (Table table, Field table field) => ScopedField table field -> SQLFieldName
mkQualifiedFieldName (ScopedField i :: ScopedField table field) = SQLQualifiedFieldName (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field)) (mkSqlTblId i)

mkUnqualifiedFieldName :: (Table table, Field table field) => ScopedField table field -> SQLFieldName
mkUnqualifiedFieldName (ScopedField i :: ScopedField table field) = SQLFieldName (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field))

mkSqlExpr :: (forall table field q. (Table table, Field table field) => ScopedField table field -> SQLFieldName) -> QExpr a -> SQLExpr
mkSqlExpr f (EqE a b) = SQLEqE (mkSqlExpr f a) (mkSqlExpr f b)
mkSqlExpr f (FieldE fd) = SQLFieldE (f fd)
mkSqlExpr f (ValE v) = SQLValE v
mkSqlExpr f (AndE a b) = SQLAndE (mkSqlExpr f a) (mkSqlExpr f b)
mkSqlExpr f (OrE a b) = SQLOrE (mkSqlExpr f a) (mkSqlExpr f b)
mkSqlExpr f (JustE a) = SQLJustE (mkSqlExpr f a)
mkSqlExpr f NothingE = SQLValE SqlNull

queryToSQL :: Query a -> SQLSelect
queryToSQL q = selectStatement
    where q' = rewriteQuery allQueryOpts allExprOpts q
          selectStatement = generateQuery q'

          sqlTrue = SQLValE (SqlBool True)

          generateQuery :: Query a -> SQLSelect
          generateQuery (All (tbl :: Proxy table) i) = simpleSelect { selFrom = SQLAliased (SQLSourceTable (dbTableName tbl)) (Just (mkSqlTblId i)) }
          generateQuery (Filter q e) = conjugateWhere (generateQuery q) (mkSqlExpr mkQualifiedFieldName e)
          generateQuery (Join (All tbl1 i1) y) = continueJoin y (simpleSelect { selFrom = SQLAliased (SQLSourceTable (dbTableName tbl1)) (Just (mkSqlTblId i1)) })
--          generateQuery (Join q y) = continueJoin y (simpleSelect { selFrom = SQLAliased (SQLSourceSelect (generateQuery q)) Nothing})

          continueJoin :: Query a -> SQLSelect -> SQLSelect
          continueJoin (Join x y) base = continueJoin y (continueJoin x base)
          continueJoin (All tbl2 i2) base = base { selJoins = (SQLJoin SQLInnerJoin (SQLAliased (SQLSourceTable (dbTableName tbl2)) (Just (mkSqlTblId i2))) sqlTrue):selJoins base }
--          continueJoin q base = base { selJoins = (SQLJoin SQLInnerJoin (SQLAliased (SQLSourceSelect (generateQuery q)) Nothing) sqlTrue):selJoins base }

insertToSQL :: Table table => table -> SQLInsert
insertToSQL (table :: table) = SQLInsert (dbTableName (Proxy :: Proxy table))
                                         (makeSqlValues table)

runQuery :: (MonadIO m, FromSqlValues a) => Query a -> Beam m -> m (Either String (Source m a))
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

runInsert :: (MonadIO m, Table table, FromSqlValues (QueryTable table)) => table -> Beam m -> m (Either String (QueryTable table))
runInsert (table :: table) beam =
    do let insertCmd = Insert sqlInsert
           sqlInsert@(SQLInsert tblName sqlValues) = (insertToSQL table)
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

           hasAutoIncrementConstraint (SQLColumnSchema { csConstraints = cs }) = isJust (find (\x -> x == SQLPrimaryKeyAutoIncrement || x == SQLAutoIncrement) cs)

       insertedValues <- if hasNullAutoIncrements
                         then getLastInsertedRow beam tblName
                         else return sqlValues
       return (fromSqlValues insertedValues)

runUpdate :: MonadIO m => SQLUpdate -> Beam m -> m (Either String ())
runUpdate update beam =
    do withHDBCConnection beam (\conn -> liftIO (runSQL' conn (Update update)))
       return (Right ())

updateWhere :: ( ScopeFields (WrapFields (QueryField table) (Schema table))
               , ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
               , Table table) => table -> (Scope (QueryTable table) -> ([QAssignment], QExpr Bool)) -> SQLUpdate
updateWhere (_ :: table) mkAssignmentsAndWhere = go
    where tblProxy :: Proxy (QueryTable table)
          tblProxy = Proxy
          go =
            let (assignments, whereClause) = mkAssignmentsAndWhere (scopeFields tblProxy 0)
                sqlAssignments = map ( \(QAssignment nm ex) -> (mkUnqualifiedFieldName nm, mkSqlExpr mkUnqualifiedFieldName ex) ) assignments
                sqlWhereClause = Just (mkSqlExpr mkUnqualifiedFieldName whereClause)
                sqlTables = [dbTableName (Proxy :: Proxy table)]
            in (SQLUpdate sqlTables sqlAssignments sqlWhereClause)

updateEntity :: ( ScopeFields (WrapFields (QueryField table) (Schema table))
                , ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
                , LocateAll (FullSchema table) (PrimaryKey table) ~ locator
                , LocateResult (FullSchema table) locator ~ locateResult
                , EqExprFor (Scope (QueryTable table)) (WrapFields Gen locateResult)
                , MakeAssignmentsFor table (Scope (QueryTable table)) (FullSchema table)
                , SchemaPart locateResult
                , Table table) => QueryTable table -> SQLUpdate
updateEntity qt@(QueryTable phantom table :: QueryTable table) = updateWhere table (\sch -> (assignments, findByPrimaryKeyExpr sch qt))
    where assignments = makeAssignments table (scopeFields (Proxy :: Proxy (QueryTable table)) 0) (phantom :|: getSchema table)

class Table t => MakeAssignmentsFor t s a where
    makeAssignments :: t -> s -> a -> [QAssignment]
instance (MakeAssignmentsFor t sa a, MakeAssignmentsFor t sb b) => MakeAssignmentsFor t (sa :|: sb) (a :|: b) where
    makeAssignments t (sa :|: sb) (a :|: b) = makeAssignments t sa a ++ makeAssignments t sb b
instance ( Field t fn
         , FieldSchema f
         , NameFor f ~ fn ) => MakeAssignmentsFor t (ScopedField t fn) f where
    makeAssignments t field f = [QAssignment field (ValE (makeSqlValue f))]

class EqExprFor schema a where
    eqExprFor :: schema -> a -> QExpr Bool
instance (EqExprFor schema a, EqExprFor schema b) => EqExprFor schema (a :|: b) where
    eqExprFor schema (a :|: b) = eqExprFor schema a &&# eqExprFor schema b
instance ( Locate schema (NameFor n) ~ locator
         , Locator schema locator
         , LocateResult schema locator ~ ScopedField table (NameFor n)
         , FieldSchema n
         , Table table, Field table (NameFor n)) => EqExprFor schema (Gen n) where

    eqExprFor schema (Gen a :: Gen a) = FieldE field' ==# ValE (makeSqlValue a)
        where ScopedField i = getField' schema (undefined :: NameFor a)

              field' :: forall q. ScopedField table (NameFor a)
              field' = ScopedField i

findByPrimaryKeyExpr :: ( Table table
                        , LocateAll (FullSchema table) (PrimaryKey table) ~ locator
                        , LocateResult (FullSchema table) locator ~ locateResult
                        , SchemaPart locateResult
                        , EqExprFor schema (WrapFields Gen locateResult) ) => schema -> QueryTable table -> QExpr Bool
findByPrimaryKeyExpr schema (QueryTable phantomFields tbl) =
    let pk = primaryKeyForTable' phantomFields tbl
    in  eqExprFor schema (mapSchema Gen pk)

primaryKeyForTable :: Table table => QueryTable table -> PrimaryKeySchema table
primaryKeyForTable (QueryTable phantom fields) = primaryKeyForTable' phantom fields

simpleSelect = SQLSelect
               { selProjection = SQLProjStar
               , selFrom = undefined
               , selJoins = []
               , selWhere = SQLValE (SqlBool True)
               , selGrouping = Nothing
               , selOrderBy = []
               , selLimit = Nothing
               , selOffset = Nothing }

fromSqlValues :: FromSqlValues a => [SqlValue] -> Either String a
fromSqlValues vals =
    case runState (runErrorT fromSqlValues') vals of
      (Right a, []) -> Right a
      (Right _,  _) -> Left "fromSqlValues: Not all values were consumed"
      (Left err, _) -> Left err

runSQL' :: IConnection conn => conn -> SQLCommand -> IO (Either String (IO (Maybe [SqlValue])))
runSQL' conn cmd = do
  let (sql, vals) = ppSQL cmd
  putStrLn ("Will execute " ++ sql ++ " with " ++ show vals)
  stmt <- prepare conn sql
  execute stmt vals
  return (Right (fetchRow stmt))

allResults :: IO (Maybe [SqlValue]) -> IO [[SqlValue]]
allResults getMore = keepGetting id
    where keepGetting a = do
            row <- getMore
            case row of
              Nothing -> return (a [])
              Just row -> keepGetting ((row :) . a)

-- * BeamT actions

inBeamTxn :: MonadIO m => Beam m -> BeamT e m a -> m (BeamResult e a)
inBeamTxn beam action = do res <- runBeamT action beam
                           withHDBCConnection beam $
                             case res of
                               Success  x -> liftIO . commit
                               Rollback e -> liftIO . rollback
                           return res

toBeamResult :: Either String a -> BeamResult e a
toBeamResult = (Rollback . InternalError) ||| Success

query :: (MonadIO m, Functor m, FromSqlValues a) => Query a -> BeamT e m (Source (BeamT e m) a)
query q = BeamT $ \beam ->
          do res <- runQuery q beam
             case res of
               Right x -> return (Success (transPipe (BeamT . const . fmap Success) x))
               Left err -> return (Rollback (InternalError err))

insert :: (MonadIO m, Functor m, Table t, FromSqlValues (QueryTable t)) => t -> BeamT e m (QueryTable t)
insert table = BeamT (\beam -> toBeamResult <$> runInsert table beam)

save :: ( ScopeFields (WrapFields (QueryField table) (Schema table))
        , ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
        , LocateAll (FullSchema table) (PrimaryKey table) ~ locator
        , LocateResult (FullSchema table) locator ~ locateResult
        , EqExprFor (Scope (QueryTable table)) (WrapFields Gen locateResult)
        , MakeAssignmentsFor table (Scope (QueryTable table)) (FullSchema table)
        , SchemaPart locateResult
        , Table table
        , MonadIO m, Functor m) => QueryTable table -> BeamT e m ()
save qt = BeamT (\beam -> toBeamResult <$> runUpdate (updateEntity qt) beam)

update :: ( ScopeFields (WrapFields (QueryField table) (Schema table))
          , ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
          , LocateAll (FullSchema table) (PrimaryKey table) ~ locator
          , LocateResult (FullSchema table) locator ~ locateResult
          , EqExprFor (Scope (QueryTable table)) (WrapFields Gen locateResult)
          , SchemaPart locateResult
          , Table table
          , MonadIO m, Functor m) =>
          QueryTable table
       -> (Scope (QueryTable table) -> [QAssignment])
       -> BeamT e m ()
update qt@(QueryTable _ table) mkAssignments = set table mkAssignments (flip findByPrimaryKeyExpr qt)

set :: ( ScopeFields (WrapFields (QueryField table) (Schema table))
       , ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
       , Table table
       , MonadIO m, Functor m) =>
       table
    -> (Scope (QueryTable table) -> [QAssignment])
    -> (Scope (QueryTable table) -> QExpr Bool)
    -> BeamT e m ()
set tbl mkAssignments mkWhere = BeamT (\beam -> toBeamResult <$> runUpdate (updateWhere tbl (mkAssignments &&& mkWhere)) beam)

getOne :: (MonadIO m, Functor m, FromSqlValues a) => Query a -> BeamT e m (Maybe a)
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