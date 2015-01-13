{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances #-}
module Database.Beam.Query
    ( module Database.Beam.Query.Types
    , module Database.Beam.Query.Rewrite
    , module Database.Beam.Query.Combinators

    , runQuery, runInsert ) where

import Database.Beam.Query.Types
import Database.Beam.Query.Rewrite
import Database.Beam.Query.Combinators

import Database.Beam.Schema
import Database.Beam.Types
import Database.Beam.SQL
import Database.Beam.SQL.Types

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans

import Data.Monoid hiding (All)
import Data.Proxy
import Data.Data
import Data.String (fromString)
import qualified Data.Text as T

import Database.HDBC

-- * Query Compilation

queryToSQL :: Query q a -> SQLSelect
queryToSQL q = selectStatement
    where q' = coerceQueryThread (rewriteQuery allQueryOpts allExprOpts q)
          selectStatement = generateQuery q'

          generateQuery :: Query () a -> SQLSelect
          generateQuery (All (tbl :: Proxy table) i) = simpleSelect { selFrom = SQLAliased (SQLSourceTable (dbTableName tbl)) (Just (tblId i)) }
          generateQuery (Filter q e) = conjugateWhere (generateQuery q) (generateExpr e)

          generateExpr :: QExpr () a -> SQLExpr a
          generateExpr (EqE a b) = SQLEqE (generateExpr a) (generateExpr b)
          generateExpr (FieldE (ScopedField i:: ScopedField () table field)) = SQLFieldE (SQLQualifiedFieldName (fieldName (Proxy :: Proxy table) (Proxy :: Proxy field)) (tblId i) )
          generateExpr (StrE s) = SQLValE s
          generateExpr (IntE i) = SQLValE i
          generateExpr (AndE a b) = SQLAndE (generateExpr a) (generateExpr b)

          tblId :: Int -> T.Text
          tblId i = fromString (concat ["t", show i])

insertToSQL :: Table table => table -> SQLInsert
insertToSQL (table :: table) = SQLInsert (dbTableName (Proxy :: Proxy table))
                                         (makeSqlValues table)

runQuery :: MonadIO m => Query q a -> Beam m -> m a
runQuery q beam =
    do let selectCmd = Select (queryToSQL q)

       res <- withHDBCConnection beam $ \conn ->
              do liftIO $ runSQL conn selectCmd []

       case res of
         Left err -> liftIO (putStrLn (concat ["An error occured: ", err]))
         Right fetchRow -> do res <- liftIO $ allResults fetchRow
                              liftIO (putStrLn (concat ["Results :", show res]))

       return undefined

runInsert :: (MonadIO m, Table table) => table -> Beam m -> m ()
runInsert table beam =
    do let insertCmd = Insert (insertToSQL table)
       liftIO (putStrLn (concat ["Will run SQL: ", ppSQL insertCmd]))
       withHDBCConnection beam (\conn -> liftIO (runRaw conn (ppSQL insertCmd)))

simpleSelect = SQLSelect
               { selProjection = SQLProjStar
               , selFrom = undefined
               , selJoins = []
               , selWhere = SQLValE True
               , selGrouping = Nothing
               , selOrderBy = []
               , selLimit = Nothing
               , selOffset = Nothing }

runSQL :: IConnection conn => conn -> SQLCommand -> [SqlValue] -> IO (Either String (IO (Maybe [SqlValue])))
runSQL conn cmd values = do
  let query = ppSQL cmd
  putStrLn ("Will execute " ++ query)
  stmt <- prepare conn query
  execute stmt []
  return (Right (fetchRow stmt))

allResults :: IO (Maybe [SqlValue]) -> IO [[SqlValue]]
allResults getMore = keepGetting id
    where keepGetting a = do
            row <- getMore
            case row of
              Nothing -> return (a [])
              Just row -> keepGetting ((row :) . a)
