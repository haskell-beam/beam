{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( module Database.Beam.Query.Types
    , module Database.Beam.Query.Rewrite
    , module Database.Beam.Query.Combinators

    , runQuery, runInsert, runUpdate,  updateEntity
    , findByPrimaryKeyExpr

    , inBeamTxn, query, insert, save, getOne, update
    --, findByPk_, matching_
    , ref, pk

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
import Control.Monad.Identity
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
import qualified Data.Set as S
import qualified Data.Text as T

import Database.HDBC

import GHC.Generics

-- * Query Compilation

mkSqlExpr :: ScopingRule -> (forall table ty. ScopedField table ty -> SQLFieldName) -> QExpr a -> SQLExpr
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
mkSqlExpr s f (InE x xs) = SQLInE (mkSqlExpr s f x) (mkSqlExpr s f xs)
mkSqlExpr s f (ListE xs) = SQLListE (map (mkSqlExpr s f) xs)
mkSqlExpr s f (CountE x) = SQLCountE (mkSqlExpr s f x)

relabelSubqueryFields :: T.Text -- ^ Join name
                      -> S.Set T.Text -- ^ Tables that are part of this join
                      -> SQLFieldName
                      -> SQLFieldName
relabelSubqueryFields joinName renamedTables (SQLQualifiedFieldName fieldName tableName)
    | tableName `S.member` renamedTables = SQLQualifiedFieldName (tableName <> "_" <> fieldName) joinName
relabelSubqueryFields _ _ x = x

mkSqlTblId :: Int -> T.Text
mkSqlTblId i = fromString (concat ["t", show i])

mkQualifiedFieldName :: ScopedField table ty -> SQLFieldName
mkQualifiedFieldName (ScopedField i fieldName) = SQLQualifiedFieldName fieldName (mkSqlTblId i)

mkUnqualifiedFieldName :: ScopedField table field -> SQLFieldName
mkUnqualifiedFieldName (ScopedField i fieldName) = SQLFieldName fieldName

instance (Project a, Project b) => Project (a :|: b) where
    project r (a :|: b) = project r a ++ project r b
instance Project (ScopedField table ty) where
    project r x = [ SQLFieldE . r . mkQualifiedFieldName $ x ]
instance Table table => Project (ScopedTable table) where
    project r (QueryTable phantom tbl :: ScopedTable table) =
        allValues getScopedField phantom tbl
        where getScopedField :: ScopedField table field -> SQLExpr
              getScopedField = SQLFieldE . r . mkQualifiedFieldName

instance Project a => Project (Maybe a) where
    project r (Just x) = project r x
    project _ Nothing = error "project: Nothing"
instance Project (QExpr a) where
    project r e = [mkSqlExpr r mkQualifiedFieldName e]

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

          projection :: Project (Scope a) => ScopingRule -> Query a -> [SQLAliased SQLExpr]
          projection r = map (flip SQLAliased Nothing) . project r . getScope

          projectSelect :: Project (Scope a) => ScopingRule -> Query a -> SQLSelect -> SQLSelect
          projectSelect r q s = s { selProjection = SQLProj . projection r $ q }

          aliasedSubquery :: SQLSelect -> SQLSelect
          aliasedSubquery s = s { selProjection = SQLProj proj' }
              where SQLProj proj = selProjection s
                    proj' = map aliasField proj

                    aliasField (SQLAliased e@(SQLFieldE (SQLQualifiedFieldName f tbl)) _) = SQLAliased e (Just (tbl <> "_" <> f))
                    aliasField x = error "aliasField: Malformed projection"

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
                                                      False -> do let qyProj = aliasedSubquery (projectSelect yScoping y qy)
                                                                      joinName = "j_" <> T.intercalate "_" (S.toList tableNames)
                                                                      tableNames = getAllTables y
                                                                  modify (relabelSubqueryFields joinName tableNames .)
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

runInsert :: (MonadIO m, Table table, FromSqlValues (Entity table)) => Simple table -> Beam m -> m (Either String (Entity table))
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
                         then getLastInsertedRow beam tblName
                         else return sqlValues
       return (fromSqlValues insertedValues)

runUpdate :: MonadIO m => SQLUpdate -> Beam m -> m (Either String ())
runUpdate update beam =
    do withHDBCConnection beam (\conn -> liftIO (runSQL' conn (Update update)))
       return (Right ())

updateWhere :: ( ScopeFields (Entity table)
               , Table table) => Proxy table -> (Scope (Entity table) -> ([QAssignment], QExpr Bool)) -> SQLUpdate
updateWhere (_ :: Proxy table) mkAssignmentsAndWhere = go
    where tblProxy :: Proxy (Entity table)
          tblProxy = Proxy
          go =
            let (assignments, whereClause) = mkAssignmentsAndWhere (scopeFields tblProxy 0)
                sqlAssignments = map ( \(QAssignment nm ex) -> (mkUnqualifiedFieldName nm, mkSqlExpr id mkUnqualifiedFieldName ex) ) assignments
                sqlWhereClause = Just (mkSqlExpr id mkUnqualifiedFieldName whereClause)
                sqlTables = [dbTableName (Proxy :: Proxy table)]
            in (SQLUpdate sqlTables sqlAssignments sqlWhereClause)

updateEntity :: ( Generic (PrimaryKey table Identity)
                , Generic (PrimaryKey table (ScopedField table))
                , Table table
                , GAllValues Identity (Rep (PrimaryKey table Identity) ())
                , GAllValues (ScopedField table) (Rep (PrimaryKey table (ScopedField table)) ())) => Entity table -> SQLUpdate
updateEntity qt@(QueryTable phantom table :: Entity table) = updateWhere tProxy
                                                                         (\s -> (assignments, findByPrimaryKeyExpr s qt))
    where tProxy = Proxy :: Proxy table
          fieldNames = allValues fieldName (phantomFieldSettings tProxy) (tblFieldSettings :: TableSettings table)
          exprs = allValues mkValE phantom table

          mkValE :: FieldSchema a => Identity a -> GenQExpr
          mkValE (x :: Identity a) = GenQExpr (ValE (makeSqlValue (runIdentity x)) :: QExpr a)

          assignments = zipWith (\name (GenQExpr (q :: QExpr ty)) -> QAssignment (ScopedField 0 name :: ScopedField table ty) q) fieldNames exprs

findByPrimaryKeyExpr :: ( Generic (PrimaryKey table Identity)
                        , Generic (PrimaryKey table (ScopedField table))
                        , Table table
                        , GAllValues Identity (Rep (PrimaryKey table Identity) ())
                        , GAllValues (ScopedField table) (Rep (PrimaryKey table (ScopedField table)) ())) =>
                        ScopedTable table -> Entity table -> QExpr Bool
findByPrimaryKeyExpr scope (table :: Entity table) =
    foldl1 (&&#) $
    zipWith (\(GenScopedField (x :: ScopedField table ty)) (GenQExpr (q :: QExpr ty2)) ->
                 case cast q of
                   Just q -> FieldE (x :: ScopedField table ty) ==# (q :: QExpr ty) :: QExpr Bool
                   Nothing -> val_ False) pkFieldNames pkValueExprs
    where QueryTable scopePhantom scopeFields = scope
          QueryTable tablePhantom tableFields = table

          pkFields = primaryKey scopePhantom scopeFields
          pkValues = primaryKey tablePhantom tableFields

          pkFieldNames = gAllValues mkScopedField (from' pkFields :: Rep (PrimaryKey table (ScopedField table)) ())
          pkValueExprs = gAllValues mkValE (from' pkValues)

          mkScopedField :: (Table table, FieldSchema a) => ScopedField table a -> GenScopedField table
          mkScopedField = GenScopedField
          mkValE :: FieldSchema a => Identity a -> GenQExpr
          mkValE (x :: Identity a) = GenQExpr (ValE (makeSqlValue (runIdentity x)) :: QExpr a)

-- findByPk_ :: ( Table table
--              , LocateAll (FullSchema table) (PrimaryKey table) ~ locator
--              , LocateResult (FullSchema table) locator ~ locateResult
--              , SchemaPart locateResult
--              , ScopeFields (QueryTable table)
--              , EqExprFor (Scope (QueryTable table)) locateResult ) =>
--              PrimaryKeySchema table -> Query (QueryTable table)
-- findByPk_ primaryKey = let query = all_ (queriedTable query) `where_`
--                                    (\table -> eqExprFor table primaryKey)

--                            queriedTable :: Query (QueryTable table) -> table
--                            queriedTable _ = undefined
--                        in query

-- -- | Get all records whose relationships match
-- matching_ :: ( hasManyPKSchema ~ PrimaryKeySchema hasMany
--              , embeddedPKSchemaNames ~ NameFor (EmbedIn name (PrimaryKeySchema hasMany))

--              , Reference hasOne name
--              , LookupInTable hasOne name ~ ForeignKey hasMany name
--              , RenameFields embeddedPKSchemaNames hasManyPKSchema
--              , EqExprFor (Scope (QueryTable hasOne)) (Rename embeddedPKSchemaNames hasManyPKSchema)
--              , ScopeFields (QueryTable hasOne)
--              , Table hasMany, Table hasOne ) =>
--              name -> QueryTable hasMany -> Query (QueryTable hasOne)
-- matching_ (name :: name) (hasMany :: QueryTable hasMany) = all_ of_ `where_` (\hasOneS -> eqExprFor hasOneS (renameFields fkNamesProxy (pk hasMany)))
--     where fkNamesProxy :: Proxy (NameFor (EmbedIn name (PrimaryKeySchema hasMany)))
--           fkNamesProxy = Proxy

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

insert :: (MonadIO m, Functor m, Table t, FromSqlValues (Entity t)) => Simple t -> BeamT e m (Entity t)
insert table = BeamT (\beam -> toBeamResult <$> runInsert table beam)

save :: ( MonadIO m, Functor m, Table table
        , GAllValues (ScopedField table) (Rep (PrimaryKey table (ScopedField table)) ())
        , Generic (PrimaryKey table (ScopedField table))
        , GAllValues Identity (Rep (PrimaryKey table Identity) ())
        , Generic (PrimaryKey table Identity)) => Entity table -> BeamT e m ()
save qt = BeamT (\beam -> toBeamResult <$> runUpdate (updateEntity qt) beam)

update :: ( MonadIO m, Functor m, Table table
          , GAllValues (ScopedField table) (Rep (PrimaryKey table (ScopedField table)) ())
          , Generic (PrimaryKey table (ScopedField table))
          , GAllValues Identity (Rep (PrimaryKey table Identity) ())
          , Generic (PrimaryKey table Identity)) =>
          Entity table
       -> (Scope (Entity table) -> [QAssignment])
       -> BeamT e m ()
update (qt :: Entity table) mkAssignments = set (Proxy :: Proxy table) mkAssignments (flip findByPrimaryKeyExpr qt)

set :: ( MonadIO m, Functor m, Table table
       , GAllValues (ScopedField table) (Rep (PrimaryKey table (ScopedField table)) ())
       , Generic (PrimaryKey table (ScopedField table))
       , GAllValues Identity (Rep (PrimaryKey table Identity) ())
       , Generic (PrimaryKey table Identity)) =>
       Proxy table
    -> (Scope (Entity table) -> [QAssignment])
    -> (Scope (Entity table) -> QExpr Bool)
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

ref :: Table t => QueryTable t c -> ForeignKey t c
ref = ForeignKey . pk

pk :: Table t => QueryTable t c -> PrimaryKey t c
pk (QueryTable phantom fields) = primaryKey phantom fields

debugQuerySQL :: Project (Scope a) => Query a -> (String, [SqlValue])
debugQuerySQL q = ppSQL (Select (queryToSQL q))