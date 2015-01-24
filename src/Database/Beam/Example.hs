{-# LANGUAGE TypeOperators, OverloadedStrings, DeriveDataTypeable, DeriveGeneric, TypeFamilies, MultiParamTypeClasses, DefaultSignatures, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, DataKinds, KindSignatures, GeneralizedNewtypeDeriving #-}
module Example where
import Data.Text (Text)

import Database.Beam.Backend
import Database.Beam.Schema
import Database.Beam.Query
import Database.Beam.Schema.Database
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

data TodoListTable = TodoListTable (TextField Name)
                                   (TextField Description)
                                   deriving (Generic, Typeable, Show)
instance Table TodoListTable
instance Field TodoListTable Name
instance Field TodoListTable Description where
    fieldSettings _ _ = TextFieldSettings (Varchar (Just 100))

data TodoListItemTable = TodoListItemTable (TextField Name)
                                           (TextField Description)
                                           (DateTimeField DueDate)
                                           (DefaultPKField TodoListTable TodoList)
                          deriving (Generic, Typeable, Show)
instance Table TodoListItemTable
instance Field TodoListItemTable Name
instance Field TodoListItemTable Description
instance Field TodoListItemTable DueDate
instance Field TodoListItemTable TodoList

instance OneToMany TodoListItems where
    type OneToManyDomain TodoListItems = TodoListTable
    type OneToManyRange  TodoListItems = TodoListItemTable

    type OneToManyDomainKey TodoListItems = PrimaryKey TodoListTable
    type OneToManyRangeKey TodoListItems = TodoList

data HistDataTable = HistDataTable (IntField CompanyId)
                                   (IntField Revenue)
                                   (DateTimeField Date)
                     deriving (Generic, Typeable, Show)
instance Table HistDataTable where
    type PrimaryKey HistDataTable = CompanyId :|: Date
instance Field HistDataTable CompanyId
instance Field HistDataTable Revenue
instance Field HistDataTable Date

data MyDatabase = MyDatabase (TableSchema TodoListTable)
                             (TableSchema TodoListItemTable)
                             (TableSchema HistDataTable)
                             deriving (Generic, Typeable)
instance Database MyDatabase

test fp = do beam <- openDatabase (Proxy :: Proxy MyDatabase) (Sqlite3Settings fp)

             runInsert (TodoListTable (TextField "List 1") (TextField "Description of List 1")) beam
             runInsert (TodoListTable (TextField "List 2") (TextField "Description of List 2")) beam

             now <- getCurrentTime

             runInsert (TodoListItemTable (TextField "Item 1") (TextField "Description of Item 1") (DateTimeField now) (DefaultPKField (IntField 1))) beam
             runInsert (TodoListItemTable (TextField "Item 2") (TextField "Description of Item 2") (DateTimeField now) (DefaultPKField (IntField 1))) beam

             runInsert (TodoListItemTable (TextField "Item 3") (TextField "Description of Item 3") (DateTimeField now) (DefaultPKField (IntField 2))) beam
             runInsert (TodoListItemTable (TextField "Item 4") (TextField "Description of Item 4") (DateTimeField now) (DefaultPKField (IntField 2))) beam

             let x = ((all_ (of_ :: TodoListTable)) `where_` (\tbl -> (tbl # Name) ==# text_ "List 1")) #* TodoListItems
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