{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Beam.Migrate.Types where

import Database.Beam

import Control.Monad.Free.Church
import Control.Monad.Identity
import Control.Arrow
import Control.Category (Category)

import Data.Functor
import Data.Monoid
import Data.Text (Text)
import Data.Proxy

-- * Migration types

data MigrationStep syntax next where
    MigrationStep :: Text -> Migration syntax a -> (a -> next) -> MigrationStep syntax next
deriving instance Functor (MigrationStep syntax)
newtype MigrationSteps syntax from to = MigrationSteps (Kleisli (F (MigrationStep syntax)) from to)
  deriving (Category, Arrow)

data MigrationF syntax next where
  MigrationRunCommand
    :: { _migrationUpCommand   :: syntax {-^ What to execute when applying the migration -}
       , _migrationDownCommand :: Maybe syntax {-^ What to execute when unapplying the migration -}
       , _migrationNext :: next }
    -> MigrationF syntax next
deriving instance Functor (MigrationF syntax)
type Migration syntax = F (MigrationF syntax)


migrationStepsToMigration :: Int -> Maybe Int
                          -> MigrationSteps syntax () a
                          -> (forall a'. Text -> Migration syntax a' -> IO a')
                          -> IO a
migrationStepsToMigration firstIdx lastIdx (MigrationSteps steps) runMigration =
  runF (runKleisli steps ()) finish step 0
  where finish x _ = pure x
        step (MigrationStep nm doStep next) i =
          if i >= firstIdx && maybe True (i <) lastIdx
          then runMigration nm doStep >>= \x -> next x (i + 1)
          else next (runMigrationSilenced doStep) (i + 1)

runMigrationSilenced :: Migration syntax a -> a
runMigrationSilenced m = runF m id step
  where
    step (MigrationRunCommand _ _ next) = next

eraseMigrationType :: a -> MigrationSteps syntax a a' -> MigrationSteps syntax () ()
eraseMigrationType a (MigrationSteps steps) = MigrationSteps (arr (const a) >>> steps >>> arr (const ()))

migrationStep :: Text -> (a -> Migration syntax a') -> MigrationSteps syntax a a'
migrationStep stepName migration =
    MigrationSteps (Kleisli (\a -> liftF (MigrationStep stepName (migration a) id)))

upDown :: syntax -> Maybe syntax -> Migration syntax ()
upDown up down = liftF (MigrationRunCommand up down ())

migrateScript :: forall syntax m a.
                 Monoid m => (Text -> m) -> (syntax -> m) -> MigrationSteps syntax () a -> m
migrateScript renderMigrationHeader renderSyntax (MigrationSteps steps) =
  runF (runKleisli steps ()) (\_ x -> x)
    (\(MigrationStep header migration next) x ->
       let (res, script) = renderMigration migration mempty
       in next res (x <> renderMigrationHeader header <> script)) mempty
  where
    renderMigration :: forall a. Migration syntax a -> m -> (a, m)
    renderMigration steps =
      runF steps (,)
           (\(MigrationRunCommand a _ next) x -> next (x <> renderSyntax a))

evaluateDatabase :: forall syntax a. MigrationSteps syntax () a -> a
evaluateDatabase (MigrationSteps f) = runF (runKleisli f ()) id (\(MigrationStep _ migration next) -> next (runMigration migration))
  where
    runMigration :: forall a. Migration syntax a -> a
    runMigration f = runF f id (\(MigrationRunCommand _ _ next) -> next)

stepNames :: forall syntax a. MigrationSteps syntax () a -> [Text]
stepNames (MigrationSteps f) = runF (runKleisli f ()) (\_ x -> x) (\(MigrationStep nm migration next) x -> next (runMigration migration) (x ++ [nm])) []
  where
    runMigration :: forall a. Migration syntax a -> a
    runMigration f = runF f id (\(MigrationRunCommand _ _ next) -> next)

-- * Checked database entities

data DatabaseCheck
data TableCheck = TableFieldCheck Text FieldCheck deriving (Show, Eq)
data FieldCheck = FieldCheck deriving (Show, Eq)

class IsDatabaseEntity be entity => IsCheckedDatabaseEntity be entity where
  data CheckedDatabaseEntityDescriptor be entity :: *

  unCheck :: CheckedDatabaseEntityDescriptor be entity -> DatabaseEntityDescriptor be entity

instance IsCheckedDatabaseEntity be (TableEntity tbl) where
  data CheckedDatabaseEntityDescriptor be (TableEntity tbl) where
    CheckedDatabaseTable :: Table tbl => DatabaseEntityDescriptor be (TableEntity tbl)
                         -> [ TableCheck ] -> CheckedDatabaseEntityDescriptor be (TableEntity tbl)

  unCheck (CheckedDatabaseTable x _) = x

data CheckedDatabaseEntity be (db :: (* -> *) -> *) entityType where
  CheckedDatabaseEntity :: IsCheckedDatabaseEntity be entityType
                        => CheckedDatabaseEntityDescriptor be entityType
                        -> [ DatabaseCheck ]
                        -> CheckedDatabaseEntity be db entityType

type CheckedDatabaseSettings be db = db (CheckedDatabaseEntity be db)

unCheckDatabase :: forall be db. Database db => CheckedDatabaseSettings be db -> DatabaseSettings be db
unCheckDatabase db = runIdentity $ zipTables (Proxy @be) (\(CheckedDatabaseEntity x _) _ -> pure $ DatabaseEntity (unCheck x)) db db
