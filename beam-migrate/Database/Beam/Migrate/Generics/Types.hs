{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Migrate.Generics.Types where

import           Database.Beam.Migrate.Types

import           Data.Proxy
import qualified Data.Text as T

import           GHC.Generics

class GAutoMigratableDb be x where
  defaultMigratableDbSettings' :: Proxy be -> x ()

instance GAutoMigratableDb be x => GAutoMigratableDb be (D1 f x) where
  defaultMigratableDbSettings' be = M1 $ defaultMigratableDbSettings' be

instance GAutoMigratableDb be x => GAutoMigratableDb be (C1 f x) where
  defaultMigratableDbSettings' be = M1 $ defaultMigratableDbSettings' be

instance (GAutoMigratableDb be x, GAutoMigratableDb be y) =>
  GAutoMigratableDb be (x :*: y) where
  defaultMigratableDbSettings' be = defaultMigratableDbSettings' be :*: defaultMigratableDbSettings' be

instance ( Selector f, IsCheckedDatabaseEntity be x
         , CheckedDatabaseEntityDefaultRequirements be x ) =>
  GAutoMigratableDb be (S1 f (Rec0 (CheckedDatabaseEntity be db x))) where

  defaultMigratableDbSettings' _ = M1 (K1 (CheckedDatabaseEntity (checkedDbEntityAuto name) []))
    where name = T.pack (selName (undefined :: S1 f (Rec0 (CheckedDatabaseEntity be db x)) ()))
