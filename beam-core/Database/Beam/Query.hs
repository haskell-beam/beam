{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( -- * Query type
      module Database.Beam.Query.Types

    -- * General query combinators
    , module Database.Beam.Query.Combinators

    , SqlSelect(..)
    , select
    , runSelectReturningList
    , dumpSqlSelect

    , SqlInsert(..)
    , insert

    , SqlInsertValues(..)
    , insertValues
    , insertFrom

    , SqlUpdate(..)
    , update

    , SqlDelete(..)
    , delete ) where

import Database.Beam.Query.Types
import Database.Beam.Query.Combinators
import Database.Beam.Query.Internal

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.Builder
import Database.Beam.Schema.Tables

import Control.Monad.Identity

-- * Query

data QueryInaccessible

-- * SELECT

newtype SqlSelect select a
    = SqlSelect select

select :: forall q syntax db s res.
          ( ProjectibleInSelectSyntax syntax res
          , IsSql92SelectSyntax syntax
          , HasQBuilder syntax ) =>
          Q syntax db QueryInaccessible res -> SqlSelect syntax (QExprToIdentity res)
select q =
  SqlSelect (buildSqlQuery q)

runSelectReturningList ::
  (IsSql92Syntax cmd, MonadBeam cmd be m, FromBackendRow be a) =>
  SqlSelect (Sql92SelectSyntax cmd) a -> m [ a ]
runSelectReturningList (SqlSelect select) =
  runReturningList (selectCmd select)

dumpSqlSelect :: ProjectibleInSelectSyntax SqlSyntaxBuilder res =>
                 Q SqlSyntaxBuilder db QueryInaccessible res -> IO ()
dumpSqlSelect q =
    let SqlSelect s = select q
    in putStrLn (renderSql s)

-- * INSERT

newtype SqlInsert syntax = SqlInsert syntax

insert :: IsSql92InsertSyntax syntax =>
          DatabaseEntity be db (TableEntity table)
       -> Sql92InsertValuesSyntax syntax
       -> SqlInsert syntax
insert (DatabaseEntity (DatabaseTable tblNm tblSettings)) insertValues =
    SqlInsert (insertStmt tblNm tblFields insertValues)
  where
    tblFields = allBeamValues (\(Columnar' f) -> _fieldName f) tblSettings

newtype SqlInsertValues insertValues (tbl :: (* -> *) -> *)
    = SqlInsertValues insertValues

insertValues ::
    ( Beamable table
    , IsSql92InsertValuesSyntax syntax
    , exprSyntax ~ Sql92InsertValuesExpressionSyntax syntax
    , IsSql92ExpressionSyntax exprSyntax
    , valueSyntax ~ Sql92ExpressionValueSyntax exprSyntax
    , MakeSqlLiterals valueSyntax table ) =>
    [ table Identity ] -> SqlInsertValues syntax table
insertValues = SqlInsertValues . insertValuesGeneric

insertFrom ::
    IsSql92InsertValuesSyntax syntax =>
    SqlSelect (Sql92InsertValuesSelectSyntax syntax) (table Identity) -> SqlInsertValues syntax table
insertFrom (SqlSelect select) = SqlInsertValues . insertFromSql $ select

-- * UPDATE

newtype SqlUpdate syntax (table :: (* -> *) -> *) = SqlUpdate syntax

update :: ( Beamable table
          , IsSql92UpdateSyntax syntax) =>
          DatabaseEntity be db (TableEntity table)
       -> (forall s. table (QField s) -> [ QAssignment (Sql92UpdateFieldNameSyntax syntax) (Sql92UpdateExpressionSyntax syntax) s ])
       -> (forall s. table (QExpr (Sql92UpdateExpressionSyntax syntax) s) -> QExpr (Sql92UpdateExpressionSyntax syntax) s Bool)
       -> SqlUpdate syntax table
update (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkAssignments mkWhere =
  SqlUpdate (updateStmt tblNm assignments (Just where_))
  where
    assignments = map (\(QAssignment fieldName expr) -> (fieldName, expr)) (mkAssignments tblFields)
    QExpr where_ = mkWhere tblFieldExprs

    tblFields = changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QField tblNm name)) tblSettings
    tblFieldExprs = changeBeamRep (\(Columnar' (QField _ nm)) -> Columnar' (QExpr (fieldE (unqualifiedField nm)))) tblFields

-- * DELETE

newtype SqlDelete syntax (table :: (* -> *) -> *) = SqlDelete syntax

delete :: IsSql92DeleteSyntax delete
       => DatabaseEntity be db (TableEntity table)
       -> (forall s. table (QExpr (Sql92DeleteExpressionSyntax delete) s) -> QExpr (Sql92DeleteExpressionSyntax delete) s Bool)
       -> SqlDelete delete table
delete (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkWhere =
  SqlDelete (deleteStmt tblNm (Just where_))
  where
    QExpr where_ = mkWhere (changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QExpr (fieldE (unqualifiedField name)))) tblSettings)

--delete :: blah

-- runSQL' :: (BeamBackend be, MonadIO m) => Bool -> Beam be d m -> SQLCommand be -> m (Either String (IO (Maybe [BeamBackendValue be])))
-- runSQL' debug beam cmd =
--   do let (sql, vals) = ppSQL cmd
--      when debug (liftIO (putStrLn ("Will execute " ++ sql ++ " with " ++ show vals)))
--      beamExecute beam sql vals

-- * Data insertion, updating, and deletion

-- insertToSQL :: (Table table, BeamBackend be, MakeBackendLiterals be table) => T.Text -> table Identity -> SQLInsert be
-- insertToSQL name (table :: table Identity) = fix $ \(_ :: SQLInsert be ) ->
--                                              SQLInsert name
--                                                        (allBeamValues (\(Columnar' (BackendLiteral' x :: BackendLiteral' be a)) -> SQLValue x)
--                                                                       (makeBackendLiterals table))

-- runInsert :: (MonadIO m, Table table, FromSqlValues be (table Identity), BeamBackend be, MakeSqlValues be table)
--              => DatabaseTable be d table -> table Identity -> Beam be d m -> m (Either String (table Identity))
-- runInsert tbl@(DatabaseTable _ tableName tblFieldSettings :: DatabaseTable be d table) (table :: table Identity) beam =
--     do let insertCmd = Insert sqlInsert :: SQLCommand be
--            sqlInsert@(SQLInsert tblName sqlValues) = insertToSQL tableName table
--        runSQL' (beamDebug beam) beam insertCmd

--        -- There are three possibilities here:
--        --
--        --   * we have no autoincrement keys, and so we simply have to return the
--        --     newly created QueryTable, or
--        --   * we have autoincrement keys, but all the fields marked autoincrement
--        --     were non-null. In this case, we have all the information needed to
--        --     construct the QueryTable, or
--        --   * we have autoincrement keys, and some of the fields were marked null.
--        --     In this case, we need to ask the backend for the last inserted row.
--        let tableSchema = reifyTableSchema tblFieldSettings

--            autosAreNull = zipWith (\(_, columnSchema) value -> csIsAuto columnSchema && value == sqlNull) tableSchema sqlValues
--            hasNullAutos = or autosAreNull

--        insertedValues <- if hasNullAutos
--                          then getLastInsertedRow beam tblName
--                          else return sqlValues
--        return (fromSqlValues insertedValues)

-- -- | Insert the given row value into the table specified by the first argument.
-- insertInto :: (MonadIO m, Functor m, Table table, FromSqlValues be (table Identity), BeamBackend be, MakeSqlValues be table) =>
--               DatabaseTable be db table -> table Identity -> BeamT be e db m (table Identity)
-- insertInto tbl data_ =
--     BeamT (\beam -> toBeamResult <$> runInsert tbl data_ beam)

-- updateToSQL :: (Table table, BeamSqlBackend be) => DatabaseTable be db tbl -> table (QExpr be s) -> QExpr be s Bool -> Maybe (SQLUpdate be)
-- updateToSQL (DatabaseTable _ name tblFieldSettings) (setTo :: table (QExpr be s)) where_ =
--     let setExprs = allBeamValues (\(Columnar' x) -> optimizeExpr x) setTo
--         setColumns = allBeamValues (\(Columnar' fieldS) -> _fieldName fieldS) tblFieldSettings

--         isInteresting columnName (SQLFieldE (SQLFieldName fName))
--             | fName == columnName = Nothing
--         isInteresting columnName newE = Just (SQLFieldName columnName, newE)

--         assignments = catMaybes (zipWith isInteresting setColumns setExprs)

--         where_' = case optimizeExpr where_ of
--                     SQLValE v | sqlValueIsTrue v -> Nothing
--                     where_' -> Just where_'

--     in case assignments of
--          [] -> Nothing
--          _  -> Just SQLUpdate
--                { uTableNames = [name]
--                , uAssignments = assignments
--                , uWhere = where_' }

-- -- | Update every entry in the given table where the third argument yields true, using the second
-- -- argument to give the new values.
-- updateWhere :: (MonadIO m, Table tbl, BeamBackend be) => DatabaseTable be db tbl -> (tbl (QExpr be s) -> tbl (QExpr be s)) -> (tbl (QExpr be s) -> QExpr be s Bool) -> BeamT be e db m ()
-- updateWhere tbl@(DatabaseTable _ name tblFieldSettings) mkAssignments mkWhere =
--     do let assignments = mkAssignments tblExprs
--            where_ = mkWhere tblExprs

--            tblExprs = changeBeamRep (\(Columnar' fieldS) -> Columnar' (QExpr (SQLFieldE (QField name Nothing (_fieldName fieldS))))) tblFieldSettings

--        case updateToSQL tbl assignments where_ of
--          Nothing -> pure () -- Assignments were empty, so do nothing
--          Just upd ->
--              let updateCmd = Update upd
--              in BeamT $ \beam ->
--                  do runSQL' (beamDebug beam) beam updateCmd
--                     pure (Success ())

-- -- | Use the 'PrimaryKey' of the given table entry to update the corresponding table row in the
-- -- database.
-- saveTo :: ( MonadIO m, Table tbl, BeamBackend be
--           , MakeSqlValues be (PrimaryKey tbl)
--           , MakeSqlValues be tbl )
--          => DatabaseTable be db tbl -> tbl Identity -> BeamT be e db m ()
-- saveTo tbl (newValues :: tbl Identity) =
--     updateWhere tbl (\_ -> val_ newValues) (val_ (primaryKey newValues) `references_`)

-- -- | Delete all entries in the given table matched by the expression
-- deleteWhere :: (MonadIO m, Table tbl, BeamBackend be) => DatabaseTable be db tbl -> (tbl (QExpr be s) -> QExpr be s Bool) -> BeamT be e db m ()
-- deleteWhere (DatabaseTable _ name tblFieldSettings) mkWhere =
--     let tblExprs = changeBeamRep (\(Columnar' fieldS) -> Columnar' (QExpr (SQLFieldE (QField name Nothing (_fieldName fieldS))))) tblFieldSettings

--         cmd = Delete SQLDelete
--               { dTableName = name
--               , dWhere = case optimizeExpr (mkWhere tblExprs) of
--                            SQLValE v | backendAsBool v -> Nothing
--                            where_ -> Just where_ }
--     in BeamT $ \beam ->
--         do runSQL' (beamDebug beam) beam cmd
--            pure (Success ())

-- -- | Delete the entry referenced by the given 'PrimaryKey' in the given table.
-- deleteFrom :: ( MonadIO m, Table tbl, BeamBackend be
--               , MakeSqlValues be (PrimaryKey tbl) )
--              => DatabaseTable be db tbl -> PrimaryKey tbl Identity -> BeamT be e db m ()
-- deleteFrom tbl pkToDelete = deleteWhere tbl (\tbl -> primaryKey tbl ==. val_ pkToDelete)

-- -- * BeamT actions

-- -- | Run the 'BeamT' action in a database transaction. On successful
-- -- completion, the transaction will be committed. Use 'throwError' to
-- -- stop the transaction and report an error.
-- beamTxn :: MonadIO m => Beam be db m -> (DatabaseSettings be db -> BeamT be e db m a) -> m (BeamResult e a)
-- beamTxn beam action = do res <- runBeamT (action (beamDbSettings beam)) beam
--                          case res of
--                            Success  x -> beamCommit beam
--                            Rollback e -> beamRollback beam
--                          return res

-- toBeamResult :: Either String a -> BeamResult e a
-- toBeamResult = (Rollback . InternalError) ||| Success

-- runQuery :: ( MonadIO m
--             , FromSqlValues be (QExprToIdentity a)
--             , Projectible be a
--             , IsQuery q ) =>
--             q be db s a -> Beam be db m -> m (Either String (Source m (QExprToIdentity a)))
-- runQuery q beam =
--     do let selectCmd = Select select
--            (_, _, select) = queryToSQL' (toQ q) 0

--        res <- runSQL' (beamDebug beam) beam selectCmd

--        case res of
--          Left err -> return (Left err)
--          Right fetchRow -> do let source = do row <- liftIO fetchRow
--                                               case row of
--                                                 Just row ->
--                                                     case fromSqlValues row of
--                                                       Left err -> fail err
--                                                       Right  q -> do yield q
--                                                                      source
--                                                 Nothing -> return ()
--                               return (Right source)

-- -- | Run the given query in the transaction and yield a 'Source' that can be used to read results
-- -- incrementally. If your result set is small and you want to just get a list, use 'queryList'.
-- query :: (IsQuery q, MonadIO m, Functor m, FromSqlValues be (QExprToIdentity a), Projectible be a) => q be db () a -> BeamT be e db m (Source (BeamT be e db m) (QExprToIdentity a))
-- query q = BeamT $ \beam ->
--           do res <- runQuery q beam
--              case res of
--                Right x -> return (Success (transPipe (BeamT . const . fmap Success) x))
--                Left err -> return (Rollback (InternalError err))

-- -- | Execute 'query' and use the 'Data.Conduit.List.consume' function to return a list of
-- -- results. Best used for small result sets.
-- queryList :: (IsQuery q, MonadIO m, Functor m, FromSqlValues be (QExprToIdentity a), Projectible be a) => q be db () a -> BeamT be e db m [QExprToIdentity a]
-- queryList q = do src <- query q
--                  src $$ C.consume

-- -- | Execute the query using 'query' and return exactly one result. The return value will be
-- -- 'Nothing' if either zero or more than one values were returned.
-- getOne :: (IsQuery q, MonadIO m, Functor m, FromSqlValues be (QExprToIdentity a), Projectible be a) => q be db () a -> BeamT be e db m (Maybe (QExprToIdentity a))
-- getOne q =
--     do let justOneSink = await >>= \x ->
--                          case x of
--                            Nothing -> return Nothing
--                            Just  x -> noMoreSink x
--            noMoreSink x = await >>= \nothing ->
--                           case nothing of
--                             Nothing -> return (Just x)
--                             Just  _ -> return Nothing
--        src <- query q
--        src $$ justOneSink

-- fromSqlValues :: FromSqlValues be a => [BeamBackendValue be] -> Either String a
-- fromSqlValues (vals :: [BeamBackendValue be]) =
--     case runState (runErrorT fromSqlValues') vals of
--       (Right a, []) -> Right a
--       (Right _,  _) -> Left "fromSqlValues: Not all values were consumed"
--       (Left err, _) -> Left err
