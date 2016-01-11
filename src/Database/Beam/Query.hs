{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( module Database.Beam.Query.Types
    , module Database.Beam.Query.Rewrite
    , module Database.Beam.Query.Combinators

    , runQuery, runInsert, runUpdate,  updateEntity
    , findByPrimaryKeyExpr

    , inBeamTxn, query, queryList, getOne
    , insert, save, update
    , findByPk_ --, matching_
    , ref, justRef, nothingRef, pk

    , debugQuerySQL ) where

import Database.Beam.Query.Types
import Database.Beam.Query.Rewrite
import Database.Beam.Query.Combinators

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

import Data.Monoid hiding (All)
import Data.Proxy
import Data.Data
import Data.List (find)
import Data.Maybe
import Data.String (fromString)
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Set as S
import qualified Data.Text as T

import Database.HDBC
import Debug.Trace

import GHC.Generics

-- * Query Compilation

mkSqlExpr :: ScopingRule -> (forall table c ty. ScopedField table c ty -> SQLFieldName) -> QExpr a -> SQLExpr
mkSqlExpr s f (EqE a b) = SQLEqE (mkSqlExpr s f a) (mkSqlExpr s f b)
mkSqlExpr s f (NeqE a b) = SQLNeqE (mkSqlExpr s f a) (mkSqlExpr s f b)
mkSqlExpr s f (LtE a b) = SQLLtE (mkSqlExpr s f a) (mkSqlExpr s f b)
mkSqlExpr s f (GtE a b) = SQLGtE (mkSqlExpr s f a) (mkSqlExpr s f b)
mkSqlExpr s f (LeE a b) = SQLLeE (mkSqlExpr s f a) (mkSqlExpr s f b)
mkSqlExpr s f (GeE a b) = SQLGeE (mkSqlExpr s f a) (mkSqlExpr s f b)
mkSqlExpr s f (FieldE fd) = SQLFieldE (s (f fd))
mkSqlExpr s f (MaybeFieldE (Just fd)) = SQLFieldE (s (f fd))
mkSqlExpr s f (MaybeFieldE Nothing) = SQLValE SqlNull
mkSqlExpr s f (ValE v) = SQLValE v
mkSqlExpr s f (AndE a b) = SQLAndE (mkSqlExpr s f a) (mkSqlExpr s f b)
mkSqlExpr s f (OrE a b) = SQLOrE (mkSqlExpr s f a) (mkSqlExpr s f b)
mkSqlExpr s f (JustE a) = mkSqlExpr s f a
mkSqlExpr s f NothingE = SQLValE SqlNull
mkSqlExpr s f (IsNothingE q) = SQLIsNothingE (mkSqlExpr s f q)
mkSqlExpr s f (IsJustE q) = SQLIsJustE (mkSqlExpr s f q)
mkSqlExpr s f (InE x xs) = SQLInE (mkSqlExpr s f x) (mkSqlExpr s f xs)
mkSqlExpr s f (ListE xs) = SQLListE (map (mkSqlExpr s f) xs)
mkSqlExpr s f (CountE x) = SQLCountE (mkSqlExpr s f x)
mkSqlExpr s f (MinE x) = SQLMinE (mkSqlExpr s f x)
mkSqlExpr s f (MaxE x) = SQLMaxE (mkSqlExpr s f x)
mkSqlExpr s f (SumE x) = SQLSumE (mkSqlExpr s f x)
mkSqlExpr s f (AverageE x) = SQLAverageE (mkSqlExpr s f x)
mkSqlExpr s f (RefE i x) = SQLFieldE (s (SQLFieldName (mkSqlExprId i)))

relabelSubqueryFields :: T.Text -- ^ Join name
                      -> S.Set T.Text -- ^ table names
                      -> S.Set T.Text -- ^ expression names
                      -> SQLFieldName
                      -> SQLFieldName
relabelSubqueryFields joinName tableNames _ (SQLQualifiedFieldName fieldName tableName)
    | tableName `S.member` tableNames = SQLQualifiedFieldName (tableName <> "_" <> fieldName) joinName
relabelSubqueryFields joinName _ exprNames (SQLFieldName x)
    | x `S.member` exprNames = SQLQualifiedFieldName x joinName
relabelSubqueryFields joinName _ _ x = x

mkSqlTblId, mkSqlExprId :: Int -> T.Text
mkSqlTblId i = fromString (concat ["t", show i])
mkSqlExprId i = fromString (concat ["f", show i])

mkQualifiedFieldName :: ScopedField table c ty -> SQLFieldName
mkQualifiedFieldName (ScopedField i fieldName) = SQLQualifiedFieldName fieldName (mkSqlTblId i)

mkUnqualifiedFieldName :: ScopedField table c field -> SQLFieldName
mkUnqualifiedFieldName (ScopedField i fieldName) = SQLFieldName fieldName

instance (Project a, Project b) => Project (a :|: b) where
    project r (a :|: b) = project r a ++ project r b
instance Project (ScopedField table c ty) where
    project r x = [ SQLAliased (SQLFieldE . r . mkQualifiedFieldName $ x) Nothing ]
instance Table table => Project (Entity table (ScopedField table c)) where
    project r (Entity phantom tbl :: Entity table (ScopedField table c)) =
        allValues getScopedField phantom tbl
        where getScopedField :: ScopedField table c field -> SQLAliased SQLExpr
              getScopedField x = SQLAliased (SQLFieldE . r . mkQualifiedFieldName $ x) Nothing
instance Project (QExpr a) where
    project r (RefE i x) = [SQLAliased (mkSqlExpr r mkQualifiedFieldName x) (Just (mkSqlExprId i))]
    project r e = [SQLAliased (mkSqlExpr r mkQualifiedFieldName e) Nothing]

capture :: State s a -> State s (a, s)
capture action = do orig <- get
                    res <- action
                    after <- get
                    put orig
                    return (res, after)

queryToSQL :: Project (Scope a) => Query a -> SQLSelect
queryToSQL q = selectStatement
    where q' = rewriteQuery allQueryOpts allExprOpts q
          selectStatement = let (s, r) = runState (generateQuery q') id
                            in projectSelect r q' s

          projectSelect :: Project (Scope a) => ScopingRule -> Query a -> SQLSelect -> SQLSelect
          projectSelect r q s = s { selProjection = SQLProj . project r . getScope $ q }

          aliasedSubquery :: SQLSelect -> SQLSelect
          aliasedSubquery s = s { selProjection = SQLProj proj' }
              where SQLProj proj = selProjection s
                    proj' = map aliasField proj

                    -- TODO what if there's a QueryExpr here?
                    aliasField (SQLAliased e@(SQLFieldE (SQLQualifiedFieldName f tbl)) Nothing) = SQLAliased e (Just (tbl <> "_" <> f))
                    aliasField (SQLAliased e (Just f)) = SQLAliased e (Just f)
                    aliasField _ = error "aliasField: malformed projection"

          getAllTables :: Query a -> S.Set T.Text
          getAllTables q = execWriter (rewriteQueryM recordAll (\_ -> return Nothing) q)
              where recordAll :: Query a -> Writer (S.Set T.Text) (Maybe (Query a))
                    recordAll (All _ i) = tell (S.singleton (mkSqlTblId i)) >> return Nothing
                    recordAll _ = return Nothing

          sqlTrue = SQLValE (SqlBool True)

          generateQuery :: Query a -> State ScopingRule SQLSelect
          generateQuery (All (tbl :: Proxy table) i) = pure (simpleSelect { selFrom = SQLFromSource (SQLAliased (SQLSourceTable (dbTableName tbl)) (Just (mkSqlTblId i))) })
          generateQuery (Filter q e) = do q' <- generateQuery q
                                          scopingRule <- get
                                          pure (conjugateWhere q' (mkSqlExpr scopingRule mkQualifiedFieldName e))
          generateQuery (Join x y) = genJoin SQLInnerJoin x y Nothing

          generateQuery (LeftJoin x y e) = genJoin SQLLeftJoin x y (Just e)
          generateQuery (RightJoin x y e) = genJoin SQLRightJoin x y (Just e)
          generateQuery (OuterJoin x y e) = genJoin SQLOuterJoin x y (Just e)
          generateQuery (Project q _) = generateQuery q
          generateQuery (Limit q i) = do q' <- generateQuery q
                                         pure (q' { selLimit =
                                                    case selLimit q' of
                                                      Nothing -> Just i
                                                      Just i' -> Just (min i i') })
          generateQuery (Offset q i) = do q' <- generateQuery q
                                          pure (q' { selOffset =
                                                     case selOffset q' of
                                                       Nothing -> Just i
                                                       Just i' -> Just (i + i') })
          generateQuery (GroupBy q (GenQExpr e)) = do q' <- generateQuery q
                                                      scopingRule <- get
                                                      pure (q' { selGrouping = Just (mempty { sqlGroupBy = [mkSqlExpr scopingRule mkQualifiedFieldName e] }) <>
                                                                               selGrouping q' })
          generateQuery (OrderBy q (GenQExpr e) ord) = do q' <- generateQuery q
                                                          scopingRule <- get
                                                          let e' = mkSqlExpr scopingRule mkQualifiedFieldName e
                                                          pure (q' { selOrderBy = case ord of
                                                                                    Ascending  -> [Asc e'] <> selOrderBy q'
                                                                                    Descending -> [Desc e'] <> selOrderBy q' } )

          genJoin :: Project (Scope b) => SQLJoinType -> Query a -> Query b -> Maybe (QExpr Bool) -> State ScopingRule SQLSelect
          genJoin joinType x y e = do (qx, xScoping) <- capture (generateQuery x)

                                      case isSimple qx of
                                        False -> do (qy, yScoping) <- capture (generateQuery y)
                                                    modify ((yScoping . xScoping) .)
                                                    scopingRule <- get
                                                    pure (simpleSelect {
                                                            selFrom = SQLJoin joinType (SQLFromSource (SQLAliased (SQLSourceSelect qx) Nothing))
                                                                                       (SQLFromSource (SQLAliased (SQLSourceSelect qy) Nothing))
                                                                                       (maybe sqlTrue (mkSqlExpr scopingRule mkQualifiedFieldName) e) })
                                        True  -> do (qy, yScoping) <- capture (generateQuery y)
                                                    case isSimple qy of
                                                      True -> do modify ((yScoping . xScoping) .)
                                                                 scopingRule <- get
                                                                 pure (qx { selFrom = SQLJoin joinType
                                                                                              (selFrom qx)
                                                                                              (selFrom qy)
                                                                                              (maybe sqlTrue (mkSqlExpr scopingRule mkQualifiedFieldName) e) })
                                                      False -> do let qyProj'@(SQLSelect { selProjection = SQLProj qyProjList }) = projectSelect yScoping y qy
                                                                      qyProj     = aliasedSubquery qyProj'
                                                                      joinName   = "j_" <> T.intercalate "_" (S.toList tableNames)
                                                                      tableNames = getAllTables y
                                                                      exprNames  = S.fromList (catMaybes (map (\(SQLAliased _ f) -> f) qyProjList ))
                                                                  modify (relabelSubqueryFields joinName tableNames exprNames .)
                                                                  qx <- generateQuery x -- Rebuild x with y's new scoping rules. TODO figure out how to remove this step
                                                                  scopingRule <- get
                                                                  pure (qx  { selFrom = SQLJoin joinType
                                                                                                (selFrom qx)
                                                                                                (SQLFromSource (SQLAliased (SQLSourceSelect qyProj) (Just joinName)))
                                                                                                (maybe sqlTrue (mkSqlExpr scopingRule mkQualifiedFieldName) e) } )
          combineWheres x y = case (selWhere x, selWhere y) of
                                (SQLValE (SqlBool True), y) -> y
                                (x, SQLValE (SqlBool True)) -> x
                                (x, y) -> SQLAndE x y

          isSimple s = isNothing (selGrouping s) && null (selOrderBy s) && selLimit s == Nothing && selOffset s == Nothing

insertToSQL :: Table table => Simple table -> SQLInsert
insertToSQL (table :: Simple table) = SQLInsert (dbTableName (Proxy :: Proxy table))
                                                (makeSqlValues table)

runQuery :: (MonadIO m, FromSqlValues a, Project (Scope a)) => Query a -> Beam m -> m (Either String (Source m a))
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

runInsert :: (MonadIO m, Table table, FromSqlValues (Entity table Column)) => Simple table -> Beam m -> m (Either String (Entity table Column))
runInsert (table :: Simple table) beam =
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
                         -- TODO, this needs to ensure that the values are returned in the correct column order
                         then getLastInsertedRow beam tblName
                         else return sqlValues
       return (fromSqlValues insertedValues)

runUpdate :: MonadIO m => SQLUpdate -> Beam m -> m (Either String ())
runUpdate update beam =
    do withHDBCConnection beam (\conn -> liftIO (runSQL' conn (Update update)))
       return (Right ())

updateWhere :: ( ScopeFields (Entity table Column)
               , Table table) => Proxy table -> (Scope (Entity table Column) -> ([QAssignment], QExpr Bool)) -> SQLUpdate
updateWhere (_ :: Proxy table) mkAssignmentsAndWhere = go
    where tblProxy :: Proxy (Entity table Column)
          tblProxy = Proxy
          go =
            let (assignments, whereClause) = mkAssignmentsAndWhere (scopeFields tblProxy (Proxy :: Proxy Column) 0)
                sqlAssignments = map ( \(QAssignment nm ex) -> (mkUnqualifiedFieldName nm, mkSqlExpr id mkUnqualifiedFieldName ex) ) assignments
                sqlWhereClause = Just (mkSqlExpr id mkUnqualifiedFieldName whereClause)
                sqlTables = [dbTableName (Proxy :: Proxy table)]
            in (SQLUpdate sqlTables sqlAssignments sqlWhereClause)

updateEntity :: Table table => Entity table Column -> SQLUpdate
updateEntity qt@(Entity phantom table :: Entity table Column) = updateWhere tProxy
                                                         (\s -> (assignments, findByPrimaryKeyExpr s qt))
    where tProxy = Proxy :: Proxy table
          fieldNames = allValues fieldName (phantomFieldSettings tProxy) (tblFieldSettings :: TableSettings table)
          exprs = allValues mkValE phantom table

          mkValE :: FieldSchema a => Column a -> GenQExpr
          mkValE (x :: Column a) = GenQExpr (ValE (makeSqlValue (columnValue x)) :: QExpr a)

          assignments = zipWith (\name (GenQExpr (q :: QExpr ty)) -> QAssignment (ScopedField 0 name :: ScopedField table Column ty) q) fieldNames exprs

findByPrimaryKeyExpr :: Table table =>
                        Entity table (ScopedField table Column) -> Entity table Column -> QExpr Bool
findByPrimaryKeyExpr (Entity phantom fields) (table :: Entity table Column) = primaryKeyExpr (Proxy :: Proxy table) (Proxy :: Proxy table) (primaryKey phantom fields) (primaryKey tablePhantom tableFields)
    where Entity tablePhantom tableFields = table

findByPk_ :: Table table =>
             PrimaryKey table Column -> Query (Entity table Column)
findByPk_ pk = let query = all_ (queriedTable query) `where_`
                           (\(Entity phantom fields) -> primaryKeyExpr (tblProxy query) (tblProxy query) (primaryKey phantom fields) pk)

                   queriedTable :: Query (Entity table Column) -> table Column
                   queriedTable _ = undefined
                   tblProxy :: Query (Entity table Column) -> Proxy table
                   tblProxy _ = Proxy
               in query

simpleSelect = SQLSelect
               { selProjection = SQLProjStar
               , selFrom = undefined
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

query :: (MonadIO m, Functor m, FromSqlValues a, Project (Scope a)) => Query a -> BeamT e m (Source (BeamT e m) a)
query q = BeamT $ \beam ->
          do res <- runQuery q beam
             case res of
               Right x -> return (Success (transPipe (BeamT . const . fmap Success) x))
               Left err -> return (Rollback (InternalError err))

queryList :: (MonadIO m, Functor m, FromSqlValues a, Project (Scope a)) => Query a -> BeamT e m [a]
queryList q = do src <- query q
                 src $$ C.consume

insert :: (MonadIO m, Functor m, Table t, FromSqlValues (Entity t Column)) => Simple t -> BeamT e m (Entity t Column)
insert table = BeamT (\beam -> toBeamResult <$> runInsert table beam)

save :: ( MonadIO m, Functor m, Table table ) => Entity table Column -> BeamT e m ()
save qt = BeamT (\beam -> toBeamResult <$> runUpdate (updateEntity qt) beam)

update :: ( MonadIO m, Functor m, Table table ) =>
          Entity table Column
       -> (Scope (Entity table Column) -> [QAssignment])
       -> BeamT e m ()
update (qt :: Entity table Column) mkAssignments = set (Proxy :: Proxy table) mkAssignments (flip findByPrimaryKeyExpr qt)

set :: ( MonadIO m, Functor m, Table table ) =>
       Proxy table
    -> (Scope (Entity table Column) -> [QAssignment])
    -> (Scope (Entity table Column) -> QExpr Bool)
    -> BeamT e m ()
set tbl mkAssignments mkWhere = BeamT (\beam -> toBeamResult <$> runUpdate (updateWhere tbl (mkAssignments &&& mkWhere)) beam)

getOne :: (MonadIO m, Functor m, FromSqlValues a, Project (Scope a)) => Query a -> BeamT e m (Maybe a)
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

from' :: Generic a => a -> Rep a ()
from' = from
to' :: Generic a => Rep a () -> a
to' = to

ref :: Table t => Entity t c -> ForeignKey t c
ref = ForeignKey . pk
justRef :: Table related =>
           Entity related Column -> ForeignKey related (Nullable Column)
justRef (e :: Entity related Column) = ForeignKey (pkChangeRep (Proxy :: Proxy related) just (pk e))
    where just :: Column a -> Nullable Column a
          just (Column x) = column (Just x)
nothingRef :: Table related =>
              Query (Entity related Column) -> ForeignKey related (Nullable Column)
nothingRef (_ :: Query (Entity related Column)) = ForeignKey (pkChangeRep (Proxy :: Proxy related) nothing (pk entitySettings))
    where nothing :: TableField related a -> Nullable Column a
          nothing x = column Nothing

          entitySettings = Entity (phantomFieldSettings (Proxy :: Proxy related)) (tblFieldSettings :: TableSettings related)

pk :: Table t => Entity t c -> PrimaryKey t c
pk (Entity phantom fields) = primaryKey phantom fields

debugQuerySQL :: Project (Scope a) => Query a -> (String, [SqlValue])
debugQuerySQL q = ppSQL (Select (queryToSQL q))
