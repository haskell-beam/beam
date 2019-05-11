{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Migrate.Generics.Types where

import           Database.Beam.Migrate.Types
import           Database.Beam.Schema.Tables

import           Data.Functor.Identity
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

instance ( Selector f
         , Generic (CheckedDatabaseSettings be innerDb)
         , GAutoMigratableDb be (Rep (CheckedDatabaseSettings be innerDb))
         , Database be innerDb
         ) =>
  GAutoMigratableDb be (S1 f (Rec0 (innerDb (CheckedDatabaseEntity be outerDb)))) where

  defaultMigratableDbSettings' _ = M1 (K1 (runIdentity $ zipTables (Proxy :: Proxy be) (\_ -> pure . changeCheckedDatabaseEntityDb @innerDb @outerDb @be) settings settings :: innerDb (CheckedDatabaseEntity be outerDb))) --TODO: Rename inner tables based on field name
    where -- name = T.pack (selName (undefined :: S1 f (Rec0 (innerDb (CheckedDatabaseEntity be outerDb))) ()))
          settings :: CheckedDatabaseSettings be innerDb
          settings = defaultMigratableDbSettings

changeCheckedDatabaseEntityDb
  :: forall db1 db2 be entityType
  .  CheckedDatabaseEntity be db1 entityType
  -> CheckedDatabaseEntity be db2 entityType
changeCheckedDatabaseEntityDb (CheckedDatabaseEntity a b) = CheckedDatabaseEntity a b

-- | Produce a checked database for the given Haskell database type
--
-- See <http://tathougies.github.io/beam/schema-guide/migrations/ the manual>
-- for more information on the defaults.
defaultMigratableDbSettings
  :: forall be db.
   ( Generic (CheckedDatabaseSettings be db)
   , GAutoMigratableDb be (Rep (CheckedDatabaseSettings be db)) )
  => CheckedDatabaseSettings be db
defaultMigratableDbSettings =
  to (defaultMigratableDbSettings' (Proxy @be) :: Rep (CheckedDatabaseSettings be db) ())
