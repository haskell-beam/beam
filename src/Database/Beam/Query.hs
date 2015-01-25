{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( module Database.Beam.Query.Types
    , module Database.Beam.Query.Rewrite
    , module Database.Beam.Query.SimpleCombinators
    , module Database.Beam.Query.Combinators

    , runQuery, runInsert
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

queryToSQL :: Query q a -> SQLSelect
queryToSQL q = selectStatement
    where q' = coerceQueryThread (rewriteQuery allQueryOpts allExprOpts q)
          selectStatement = generateQuery q'

          sqlTrue = SQLValE (SqlBool True)

          generateQuery :: Query () a -> SQLSelect
          generateQuery (All (tbl :: Proxy table) i) = simpleSelect { selFrom = SQLAliased (SQLSourceTable (dbTableName tbl)) (Just (tblId i)) }
          generateQuery (Filter q e) = conjugateWhere (generateQuery q) (generateExpr e)
          generateQuery (Join (All tbl1 i1) y) = continueJoin y (simpleSelect { selFrom = SQLAliased (SQLSourceTable (dbTableName tbl1)) (Just (tblId i1)) })
--          generateQuery (Join q y) = continueJoin y (simpleSelect { selFrom = SQLAliased (SQLSourceSelect (generateQuery q)) Nothing})

          continueJoin :: Query () a -> SQLSelect -> SQLSelect
          continueJoin (Join x y) base = continueJoin y (continueJoin x base)
          continueJoin (All tbl2 i2) base = base { selJoins = (SQLJoin SQLInnerJoin (SQLAliased (SQLSourceTable (dbTableName tbl2)) (Just (tblId i2))) sqlTrue):selJoins base }
--          continueJoin q base = base { selJoins = (SQLJoin SQLInnerJoin (SQLAliased (SQLSourceSelect (generateQuery q)) Nothing) sqlTrue):selJoins base }

          generateExpr :: QExpr () a -> SQLExpr a
          generateExpr (EqE a b) = SQLEqE (generateExpr a) (generateExpr b)
          generateExpr (FieldE (ScopedField i:: ScopedField () table field)) = SQLFieldE (SQLQualifiedFieldName (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field)) (tblId i) )
          -- generateExpr (StrE s) = SQLValE (SqlString (T.unpack s))
          -- generateExpr (IntE i) = SQLValE (SqlInteger (fromIntegral i))
          -- generateExpr (BoolE b) = SQLValE (SqlBool b)
          generateExpr (ValE v) = SQLValE v
          generateExpr (AndE a b) = SQLAndE (generateExpr a) (generateExpr b)
          generateExpr (OrE a b) = SQLOrE (generateExpr a) (generateExpr b)
          generateExpr (JustE a) = SQLJustE (generateExpr a)
          generateExpr NothingE = SQLValE SqlNull

          tblId :: Int -> T.Text
          tblId i = fromString (concat ["t", show i])

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
