{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving, OverloadedStrings, FlexibleInstances, TypeOperators, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Main where

import Database.Beam
import Database.Beam.Backend.Sqlite3

import Control.Monad
import Control.Monad.Identity

import Data.Typeable
import Data.Text
import Data.Time.Clock

import GHC.Generics

import System.Environment

-- * Beam schema
data TodoListT column = TodoList
                     { todoListId          :: column AutoId
                     , todoListName        :: column Text
                     , todoListDescription :: column Text }
                 deriving (Generic, Typeable)
type TodoList = TodoListT Column
data TodoItemT column = TodoItem
                      { todoItemId          :: column AutoId
                      , todoItemList        :: ForeignKey TodoListT column
                      , todoItemPriority    :: column Int
                      , todoItemName        :: column Text
                      , todoItemDescription :: column Text }
                        deriving (Generic, Typeable)
type TodoItem = TodoItemT Column

deriving instance Show (TodoListT Column)
deriving instance Show (TodoListT (TableField TodoListT))
deriving instance Show (TodoItemT Column)
deriving instance Show (TodoItemT (TableField TodoItemT))

instance Table TodoItemT where
    type PrimaryKey TodoItemT column = PK column AutoId
    primaryKey = PK .todoItemId
instance Table TodoListT where
    type PrimaryKey TodoListT column = PK column AutoId
    primaryKey = PK . todoListId

todoListDb :: Database
todoListDb = database_
             [ table_ (schema_ :: TodoList)
             , table_ (schema_ :: TodoItem) ]

instance FromSqlValues (TodoItemT Column) where
    fromSqlValues' = tableFromSqlValues
    valuesNeeded _ = tableValuesNeeded (Proxy :: Proxy TodoItemT)
instance FromSqlValues (TodoListT Column) where
    fromSqlValues' = tableFromSqlValues
    valuesNeeded _ = tableValuesNeeded (Proxy :: Proxy TodoListT)

main = do
  -- Usage: ./todoList <sqlite-db-path>
  [sqliteDbPath] <- getArgs
  beam <- openDatabase todoListDb (Sqlite3Settings sqliteDbPath)

  inBeamTxn beam $ beamNoErrors $
    do let todoLists = [ TodoList (column UnassignedId) (column "List 1") (column "Description for list 1")
                       , TodoList (column UnassignedId) (column "List 2") (column "Description for list 2")
                       , TodoList (column UnassignedId) (column "List 3") (column "Description for list 3") ]
       [list1, list2, list3] <- mapM insert todoLists
       liftIO $ putStrLn ("inserted lists " ++ show list1 ++ " " ++ show list2 ++ " " ++ show list3)

       let todoItems = [ TodoItem (column UnassignedId) (ref list1) (column 4) (column "Item 1") (column "This is item 1 in list 1")
                       , TodoItem (column UnassignedId) (ref list1) (column 2) (column "Item 2") (column "This is item 2 in list 1")
                       , TodoItem (column UnassignedId) (ref list2) (column 10) (column "Item 1") (column "This is item 1 in list 2")
                       , TodoItem (column UnassignedId) (ref list2) (column 1) (column "Item 2") (column "This is item 2 in list 2") ]
       mapM_ insert todoItems

       liftIO (putStrLn "---- Query 1: All Todo Lists")
       q1 <- queryList (all_ (of_ :: TodoList))
       liftIO (mapM_ (putStrLn . show) q1)

       liftIO (putStrLn "\n---- Query 2: All TodoItems associated with list 1")
       q2 <- queryList (todoItemList <-@ list1)
       liftIO (mapM_ (putStrLn . show) q2)

       liftIO (putStrLn "\n---- Query 3: All TodoItems JOINed with all TodoLists")
       q3 <- queryList (all_ (of_ :: TodoItem) ==> todoItemList)
       liftIO (mapM_ (putStrLn . show) q3)

       liftIO (putStrLn "\n---- Query 4: All TodoLists left joined with the TodoItems")
       q4 <- queryList (all_ (of_ :: TodoList) <=? todoItemList)
       liftIO (mapM_ (putStrLn . show) q4)

       liftIO (putStrLn "\n---- Query 5: All TodoLists along with the highest priority TodoItem(s) (or Nothing, if there are no TodoItems)")
       q5 <- queryList (all_ (of_ :: TodoList)
                        `leftJoin_` ( all_ (of_ :: TodoItem)
                                      `groupBy_` (\todoItem -> columnValue . tableId . reference . todoItemList $ todoItem)
                                      `project_` (\todoItem -> (columnValue . tableId . reference . todoItemList $ todoItem) :|:
                                                               max_ (columnValue . todoItemPriority $ todoItem))
                                      :: Query (QueryExpr AutoId :|: QueryExpr Int)
                                    , \(todoList :|: (expTodoListId :|: _)) ->
                                        field_ (todoListId todoList) ==# expTodoListId)
                        `leftJoin_` ( all_ (of_ :: TodoItem)
                                    , (\(todoList :|: (_ :|: maxTodoItemPriority) :|: todoItem) ->
                                       field_ (todoListId todoList) ==# (columnValue . tableId . reference . todoItemList $ todoItem) &&#
                                       maxTodoItemPriority ==# just_ (columnValue . todoItemPriority $ todoItem)) )
                        `project_` (\(todoList :|: _ :|: todoItem) -> todoList :|: todoItem)
                            :: Query (TodoListT Column :|: Maybe (TodoItemT Column)) )
       liftIO (mapM_ (putStrLn . show) q5)

