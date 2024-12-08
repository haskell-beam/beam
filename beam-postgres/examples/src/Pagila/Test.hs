{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Pagila.Test where

import Control.Arrow ()

import Data.Int ( Int32 )
import Data.Text(Text)

import Database.Beam
    ( Generic, Beamable, Columnar, Database, Table(..), TableEntity )
import Database.Beam.Postgres
import Database.Beam.Backend.Types ()
import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.Types hiding (migrateScript)

data SimpleTbl f
  = SimpleTbl
  { simpletblField1 :: Columnar f Int32
  , simpletblField2 :: Columnar f (Maybe Text) }
  deriving Generic
instance Beamable SimpleTbl

instance Table SimpleTbl where
  data PrimaryKey SimpleTbl f = SimpleTblId (Columnar f Int32) deriving Generic
  primaryKey = SimpleTblId <$> simpletblField1
instance Beamable (PrimaryKey SimpleTbl)

data MyDb f =
  MyDb { mydbSimpleTbl :: f (TableEntity SimpleTbl) } deriving Generic
instance Database be MyDb

myDbMigratable :: forall syntax 
                . IsSql92DdlCommandSyntax syntax
               => CheckedDatabaseSettings Postgres MyDb
myDbMigratable = defaultMigratableDbSettings @Postgres @MyDb
