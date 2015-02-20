{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving, OverloadedStrings, FlexibleInstances #-}
module Main where

import Database.Beam
import Database.Beam.Backend.Sqlite3

import Control.Monad

import Data.Typeable
import Data.Text

import GHC.Generics

import System.Environment

-- * Beam schema
data TodoListT column = TodoList
                     { todoListName        :: column Text
                     , todoListDescription :: column Text }
                 deriving (Generic, Typeable)
type TodoList = TodoListT Column
data TodoItemT column = TodoItem
                      { todoItemList        :: ForeignKey TodoListT column
                      , todoItemName        :: column Text
                      , todoItemDescription :: column Text }
                        deriving (Generic, Typeable)
type TodoItem = TodoItemT Column

deriving instance Show (TodoListT Column)
deriving instance Show (TodoItemT Column)

instance Table TodoItemT
instance Table TodoListT

todoListDb :: Database
todoListDb = database_
             [ table_ (schema_ :: TodoList)
             , table_ (schema_ :: TodoItem) ]

main = do
  -- Usage: ./todoList <sqlite-db-path>
  [sqliteDbPath] <- getArgs
  beam <- openDatabase todoListDb (Sqlite3Settings sqliteDbPath)

  inBeamTxn beam $ beamNoErrors $
    do let todoLists = [ TodoList (column "List 1") (column "Description for list 1")
                       , TodoList (column "List 2") (column "Description for list 2")
                       , TodoList (column "List 3") (column "Description for list 3") ]
       [list1, list2, list3] <- mapM insert todoLists

       let todoItems = [ TodoItem (ref list1) (column "Item 1") (column "This is item 1 in list 1")
                       , TodoItem (ref list1) (column "Item 2") (column "This is item 2 in list 1")
                       , TodoItem (ref list2) (column "Item 1") (column "This is item 1 in list 2")
                       , TodoItem (ref list2) (column "Item 2") (column "This is item 2 in list 2") ]
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