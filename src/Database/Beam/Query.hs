{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( module Database.Beam.Query.Types
    , module Database.Beam.Query.Rewrite
    , module Database.Beam.Query.SimpleCombinators
    , module Database.Beam.Query.Combinators

    , runQuery, runInsert, runUpdate
    , EqExprFor(..)
    , findByPrimaryKeyExpr ) where

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
import Data.String (fromString)
import Data.Conduit
import qualified Data.Text as T

import Database.HDBC

-- * Query Compilation

mkSqlTblId :: Int -> T.Text
mkSqlTblId i = fromString (concat ["t", show i])

mkQualifiedFieldName :: (Table table, Field table field) => ScopedField q table field -> SQLFieldName
mkQualifiedFieldName (ScopedField i :: ScopedField q table field) = SQLQualifiedFieldName (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field)) (mkSqlTblId i)

mkUnqualifiedFieldName :: (Table table, Field table field) => ScopedField q table field -> SQLFieldName
mkUnqualifiedFieldName (ScopedField i :: ScopedField q table field) = SQLFieldName (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field))

mkSqlExpr :: (forall table field q. (Table table, Field table field) => ScopedField q table field -> SQLFieldName) -> QExpr () a -> SQLExpr
mkSqlExpr f (EqE a b) = SQLEqE (mkSqlExpr f a) (mkSqlExpr f b)
mkSqlExpr f (FieldE fd) = SQLFieldE (f fd)
mkSqlExpr f (ValE v) = SQLValE v
mkSqlExpr f (AndE a b) = SQLAndE (mkSqlExpr f a) (mkSqlExpr f b)
mkSqlExpr f (OrE a b) = SQLOrE (mkSqlExpr f a) (mkSqlExpr f b)
mkSqlExpr f (JustE a) = SQLJustE (mkSqlExpr f a)
mkSqlExpr f NothingE = SQLValE SqlNull

queryToSQL :: Query q a -> SQLSelect
queryToSQL q = selectStatement
    where q' = coerceQueryThread (rewriteQuery allQueryOpts allExprOpts q)
          selectStatement = generateQuery q'

          sqlTrue = SQLValE (SqlBool True)

          generateQuery :: Query () a -> SQLSelect
          generateQuery (All (tbl :: Proxy table) i) = simpleSelect { selFrom = SQLAliased (SQLSourceTable (dbTableName tbl)) (Just (mkSqlTblId i)) }
          generateQuery (Filter q e) = conjugateWhere (generateQuery q) (mkSqlExpr mkQualifiedFieldName e)
          generateQuery (Join (All tbl1 i1) y) = continueJoin y (simpleSelect { selFrom = SQLAliased (SQLSourceTable (dbTableName tbl1)) (Just (mkSqlTblId i1)) })
--          generateQuery (Join q y) = continueJoin y (simpleSelect { selFrom = SQLAliased (SQLSourceSelect (generateQuery q)) Nothing})

          continueJoin :: Query () a -> SQLSelect -> SQLSelect
          continueJoin (Join x y) base = continueJoin y (continueJoin x base)
          continueJoin (All tbl2 i2) base = base { selJoins = (SQLJoin SQLInnerJoin (SQLAliased (SQLSourceTable (dbTableName tbl2)) (Just (mkSqlTblId i2))) sqlTrue):selJoins base }
--          continueJoin q base = base { selJoins = (SQLJoin SQLInnerJoin (SQLAliased (SQLSourceSelect (generateQuery q)) Nothing) sqlTrue):selJoins base }

insertToSQL :: Table table => table -> SQLInsert
insertToSQL (table :: table) = SQLInsert (dbTableName (Proxy :: Proxy table))
                                         (makeSqlValues table)

runQuery :: (MonadIO m, FromSqlValues a) => Query q a -> Beam m -> m (Either String (Source m a))
runQuery q beam =
    do let selectCmd = Select (queryToSQL q)

       res <- withHDBCConnection beam $ \conn ->
              do liftIO $ runSQL conn selectCmd

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

runInsert :: (MonadIO m, Table table) => table -> Beam m -> m (Either String ())
runInsert table beam =
    do let insertCmd = Insert (insertToSQL table)
       withHDBCConnection beam (\conn -> liftIO (runSQL conn insertCmd))
       return (Right ())

runUpdate :: forall q table.
             ( ScopeFields (WrapFields (QueryField table) (Schema table))
             , ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
             , Table table) => table -> (forall q. Scope q (QueryTable table) -> ([QAssignment q], QExpr q Bool)) -> SQLUpdate
runUpdate (tbl :: table) mkAssignmentsAndWhere = go
    where tblProxy :: Proxy (QueryTable table)
          tblProxy = Proxy

          qProxy :: Proxy ()
          qProxy = Proxy

          go =
            let (assignments, whereClause) = mkAssignmentsAndWhere (scopeFields qProxy tblProxy 0)
                sqlAssignments = map ( \(QAssignment nm ex) -> (mkUnqualifiedFieldName nm, mkSqlExpr mkUnqualifiedFieldName ex) ) assignments
                sqlWhereClause = Just (mkSqlExpr mkUnqualifiedFieldName whereClause)
                sqlTables = [dbTableName (Proxy :: Proxy table)]
            in (SQLUpdate sqlTables sqlAssignments sqlWhereClause)

-- updateEntity :: ( ScopeFields (WrapFields (QueryField table) (Schema table))
--                 , ScopeFields (WrapFields (QueryField table) (PhantomFieldSchema table))
--                 , LocateAll (FullSchema table) (PrimaryKey table) ~ locator
--                 , LocateResult (FullSchema table) locator ~ locateResult
--                 , EqExprFor (WrapFields (QueryField table) (Scope () (QueryTable table))) (WrapFields Gen locateResult)
--                 , Table table) => QueryTable table -> SQLUpdate
-- updateEntity qt@(QueryTable _ table) = runUpdate table (\sch -> ([] , findByPrimaryKeyExpr sch qt))

class EqExprFor schema a where
    eqExprFor :: schema -> a -> QExpr q Bool
instance (EqExprFor schema a, EqExprFor schema b) => EqExprFor schema (a :|: b) where
    eqExprFor schema (a :|: b) = eqExprFor schema a &&# eqExprFor schema b
instance ( Locate schema (NameFor n) ~ locator
         , Locator schema locator
         , LocateResult schema locator ~ ScopedField q table (NameFor n)
         , FieldSchema n
         , Table table, Field table (NameFor n)) => EqExprFor schema (Gen n) where

    eqExprFor schema (Gen a :: Gen a) = FieldE field' ==# ValE (makeSqlValue a)
        where ScopedField i = getField' schema (undefined :: NameFor a)

              field' :: forall q. ScopedField q table (NameFor a)
              field' = ScopedField i

findByPrimaryKeyExpr :: ( Table table
                        , LocateAll (FullSchema table) (PrimaryKey table) ~ locator
                        , LocateResult (FullSchema table) locator ~ locateResult
                        , SchemaPart locateResult
                        , EqExprFor schema (WrapFields Gen locateResult) ) => schema -> QueryTable table -> QExpr q Bool
findByPrimaryKeyExpr schema (QueryTable phantomFields tbl) =
    let pk = primaryKeyForTable phantomFields tbl
    in  eqExprFor schema (mapSchema Gen pk)

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

runSQL :: IConnection conn => conn -> SQLCommand -> IO (Either String (IO (Maybe [SqlValue])))
runSQL conn cmd = do
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
