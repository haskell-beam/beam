{-# LANGUAGE TypeOperators, OverloadedStrings, DeriveDataTypeable, DeriveGeneric, TypeFamilies, MultiParamTypeClasses, DefaultSignatures, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, DataKinds, KindSignatures, GeneralizedNewtypeDeriving #-}
module Example where
import Data.Text (Text)

import Database.Beam.Backend
import Database.Beam.Schema
import Database.Beam.Query
import Database.Beam.Backend.Sqlite3
import Database.Beam.SQL
import Database.Beam.Types

import Control.Monad.Trans

import Data.Time.Clock
import Data.Conduit

import Database.HDBC.Sqlite3
import Database.HDBC

import GHC.Generics
import Data.Typeable

data Name = Name deriving (Generic, Typeable)
data Description = Description deriving (Generic, Typeable)
data DueDate = DueDate deriving (Generic, Typeable)
data TodoList = TodoList deriving (Generic, Typeable)
data Date = Date deriving (Generic, Typeable)
data CompanyId = CompanyId deriving (Generic, Typeable)
data Revenue = Revenue deriving (Generic, Typeable)

data TodoListItems = TodoListItems deriving (Generic, Typeable)

data TodoListTable = TodoListTable (Column Name Text)
                                   (Column Description Text)
                                   deriving (Generic, Typeable, Show)
instance Table TodoListTable
instance Field TodoListTable Name
instance Field TodoListTable Description where
    fieldSettings _ _ = TextFieldSettings (Varchar (Just 100))

data TodoListItemTable = TodoListItemTable (Column Name Text)
                                           (Column Description Text)
                                           (Column DueDate UTCTime)
                                           (ForeignKey TodoListTable TodoList)
                          deriving (Generic, Typeable, Show)
instance Table TodoListItemTable
instance Field TodoListItemTable Name
instance Field TodoListItemTable Description
instance Field TodoListItemTable DueDate
instance Reference TodoListItemTable TodoList
-- instance Field TodoListItemTable (TodoList :-> TableId) where
--     fieldName _ _ = "TodoList_id"
--     fieldNameD _ _ = TodoList :-> TableId

instance Relationship TodoListTable TodoListItemTable TodoListItems where
    type SubjectFields TodoListTable TodoListItemTable TodoListItems = PrimaryKey TodoListTable
    type ObjectFields TodoListTable TodoListItemTable TodoListItems = TodoList :-> TableId

data HistDataTable = HistDataTable (Column CompanyId Int)
                                   (Column Revenue Int)
                                   (Column Date UTCTime)
                     deriving (Generic, Typeable, Show)
instance Table HistDataTable where
    type PrimaryKey HistDataTable = CompanyId :|: Date
instance Field HistDataTable CompanyId
instance Field HistDataTable Revenue
instance Field HistDataTable Date

myDatabase :: Database
myDatabase = database_
             [ table_ (schema_ :: TodoListTable)
             , table_ (schema_ :: TodoListItemTable)
             , table_ (schema_ :: HistDataTable) ]

test fp = do beam <- openDatabase myDatabase (Sqlite3Settings fp)

             Right l1 <- runInsert (TodoListTable (column "List 1") (column "Description of List 1")) beam
             Right l2 <- runInsert (TodoListTable (column "List 2") (column "Description of List 2")) beam

             now <- getCurrentTime

             runInsert (TodoListItemTable (column "Item 1") (column "Description of Item 1") (column now) (ref l1)) beam
             runInsert (TodoListItemTable (column "Item 2") (column "Description of Item 2") (column now) (ref l1)) beam

             runInsert (TodoListItemTable (column "Item 3") (column "Description of Item 3") (column now) (ref l2)) beam
             runInsert (TodoListItemTable (column "Item 4") (column "Description of Item 4") (column now) (ref l2)) beam

             let x = ((all_ (of_ :: TodoListTable)) `where_` (\tbl -> (tbl # Name) ==# text_ "List 1")) #@* TodoListItems
                 y = (all_ (of_ :: TodoListTable)) `where_` (\tbl -> (tbl # Name) ==# text_ "List 1")
                 xOpt = rewriteQuery allQueryOpts allExprOpts x

             putStrLn (concat ["Query fully optimized is: ", show xOpt ])
             src <- runQuery x beam
             let printAllLines = await >>=
                                 \x -> case x of
                                         Nothing -> return ()
                                         Just  x -> liftIO (putStrLn (show x)) >> printAllLines
             case src of
               Left err -> putStrLn (concat ["There was an error with the request: ", err])
               Right src -> src $$ printAllLines
             withHDBCConnection beam commit