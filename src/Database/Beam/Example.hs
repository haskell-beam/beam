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

import Database.HDBC.Sqlite3

import GHC.Generics
import Data.Typeable

data Name = Name deriving (Generic, Typeable)
data Description = Description deriving (Generic, Typeable)
data DueDate = DueDate deriving (Generic, Typeable)

instance Field TodoListTable Name
instance Field TodoListTable Description where
    fieldSettings _ _ = TextFieldSettings (Varchar (Just 100))

data TodoListTable = TodoListTable (TextField Name)
                                   (TextField Description)
                                   deriving (Generic, Typeable)
instance Table TodoListTable

data TodoListItemTable = TodoListItemTable (TextField Name)
                                           (TextField Description)
                                           (DateTimeField DueDate)
                          deriving (Generic, Typeable)
instance Table TodoListItemTable
instance Field TodoListItemTable Name
instance Field TodoListItemTable Description
instance Field TodoListItemTable DueDate

data MyDatabase = MyDatabase (TableSchema TodoListTable)
                             (TableSchema TodoListItemTable)
                             deriving (Generic, Typeable)
instance Database MyDatabase