{-# LANGUAGE TypeOperators, OverloadedStrings, DeriveDataTypeable, DeriveGeneric, TypeFamilies, MultiParamTypeClasses, DefaultSignatures, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, DataKinds, KindSignatures, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Example where
import Data.Text (Text)

import GHC.Generics
import Data.Typeable

import Database.Beam
import Database.Beam.Backend.Sqlite3

import Data.Conduit
import qualified Data.Conduit.List as C
import Data.Time.Clock

import Control.Monad.Trans

data TodoList column = TodoList
                     { todoListName :: column Text
                     , todoListDescription :: column (Maybe Text) }
                       deriving (Generic, Typeable)
deriving instance (Show (column Text), Show (column (Maybe Text))) => Show (TodoList column)
instance Table TodoList where
    tblFieldSettings = defTblFieldSettings
                       & todoListName .~ 

data TodoItem column = TodoItem
                     { todoItemName :: column Text
                     , todoItemDescription :: column Text
                     , todoItemDueDate :: column UTCTime
                     , todoItemList :: ForeignKey TodoList column }
                     deriving (Generic, Typeable)
deriving instance ( Show (column Text), Show (column UTCTime)
                  , Show (ForeignKey TodoList column) ) => Show (TodoItem column)
instance Table TodoItem

data TodoLog column = TodoLog
                    { todoLogList :: ForeignKey TodoList (Nullable column)
                    , todoLogItem :: ForeignKey TodoItem (Nullable column)
                    , todoLogEnty :: column Text }
                      deriving (Generic, Typeable)
deriving instance ( Show (column Text)
                  , Show (column (Maybe Int)) ) => Show (TodoLog column)
instance Table TodoLog

data MyDatabase table = MyDatabase
    { todoListTable :: table TodoList
    , todoItemTable :: table TodoItem
    , todoLogTable  :: table TodoLog }
      deriving (Generic, Typeable)

myDatabase :: Database
myDatabase = database_
             [ table_ (schema_ :: Simple TodoList)
             , table_ (schema_ :: Simple TodoItem)
             , table_ (schema_ :: Simple TodoLog) ]

test fp = do beam <- openDatabase myDatabase (Sqlite3Settings fp)
             inBeamTxn beam $ beamNoErrors $
              do l1 <- insert (TodoList (column "List 1") (column (Just "Description of List 1")))
                 l2 <- insert (TodoList (column "List 2") (column Nothing))
                 mapM_ (liftIO . putStrLn . show) [l1, l1]

                 now <- liftIO getCurrentTime
                 i1 <- insert (TodoItem (column "Item 1") (column "Desc for Item 1") (column now) (ref l1))
                 i2 <- insert (TodoItem (column "Item 2") (column "Desc for Item 2") (column now) (ref l1))
                 mapM_ (liftIO . putStrLn . show) [i1, i2]

                 let l1' = l1 { tableFields = (tableFields l1) { todoListDescription = column (Just "Modified description") } }
                 save l1'

                 let todoLog = TodoLog (justRef l2) (nothingRef (all_ (of_ :: Simple TodoItem))) (column "Test log entry")
                 insert todoLog

                 src <- query ( all_ (of_ :: Simple TodoList)
                               `leftJoin_` ( all_ (of_ :: Simple TodoItem)
                                           , (\(Entity (PK listId) todoList :|: Entity _ todoItem) ->
                                              field_ listId ==# tableId . reference . todoItemList # todoItem))
                               `leftJoin_` ( all_ (of_ :: Simple TodoLog)
                                           , \(Entity (PK listId) _ :|: _ :|: Entity _ todoLog) ->
                                               just_ (field_ listId) ==# tableId . reference . todoLogList #? todoLog ))
                 es <- src $$ C.mapM_ (liftIO . putStrLn . show)

                 return ()
