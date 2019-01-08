{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.Generics.Types where

import           Database.Beam.Migrate.Types

import           Data.Proxy
import qualified Data.Text as T

import           GHC.Generics

class GAutoMigratableDb syntax x where
  defaultMigratableDbSettings' :: Proxy syntax -> x ()

instance GAutoMigratableDb syntax x => GAutoMigratableDb syntax (D1 f x) where
  defaultMigratableDbSettings' syntax = M1 $ defaultMigratableDbSettings' syntax

instance GAutoMigratableDb syntax x => GAutoMigratableDb syntax (C1 f x) where
  defaultMigratableDbSettings' syntax = M1 $ defaultMigratableDbSettings' syntax

instance (GAutoMigratableDb syntax x, GAutoMigratableDb syntax y) =>
  GAutoMigratableDb syntax (x :*: y) where
  defaultMigratableDbSettings' syntax = defaultMigratableDbSettings' syntax :*: defaultMigratableDbSettings' syntax

instance ( Selector f, IsCheckedDatabaseEntity be x
         , CheckedDatabaseEntityDefaultRequirements be x syntax ) =>
  GAutoMigratableDb syntax (S1 f (Rec0 (CheckedDatabaseEntity be db x))) where

  defaultMigratableDbSettings' syntax = M1 (K1 (CheckedDatabaseEntity (checkedDbEntityAuto syntax name) []))
    where name = T.pack (selName (undefined :: S1 f (Rec0 (CheckedDatabaseEntity be db x)) ()))
