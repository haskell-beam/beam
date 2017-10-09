{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.Types
  ( module Database.Beam.Migrate.Types.CheckedEntities
  , module Database.Beam.Migrate.Types.Predicates

  , MigrationStep(..), MigrationSteps(..)
  , Migration, MigrationF(..)

  , migrationStepsToMigration, runMigrationSilenced
  , runMigrationVerbose, executeMigration
  , eraseMigrationType, migrationStep, upDown
  , executeMigration

  , migrateScript, evaluateDatabase, stepNames ) where

import Database.Beam
import Database.Beam.Backend

import Database.Beam.Migrate.Types.CheckedEntities
import Database.Beam.Migrate.Types.Predicates

import Control.Monad.Free.Church
import Control.Arrow
import Control.Category (Category)

import Data.Monoid
import Data.Text (Text)

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

runMigrationVerbose :: MonadBeam syntax be hdl m => (syntax -> String)
                    -> Migration syntax a -> m a
runMigrationVerbose renderMigrationSyntax steps =
  runF steps finish step
  where finish = pure
        step (MigrationRunCommand up _ next) =
          do liftIO (putStrLn (renderMigrationSyntax up))
             runNoReturn up
             next

eraseMigrationType :: a -> MigrationSteps syntax a a' -> MigrationSteps syntax () ()
eraseMigrationType a (MigrationSteps steps) = MigrationSteps (arr (const a) >>> steps >>> arr (const ()))

migrationStep :: Text -> (a -> Migration syntax a') -> MigrationSteps syntax a a'
migrationStep stepName migration =
    MigrationSteps (Kleisli (\a -> liftF (MigrationStep stepName (migration a) id)))

upDown :: syntax -> Maybe syntax -> Migration syntax ()
upDown up down = liftF (MigrationRunCommand up down ())

migrateScript :: forall syntax m a.
                 Monoid m => (Text -> m) -> (syntax -> m) -> MigrationSteps syntax () a -> m
migrateScript renderMigrationHeader renderMigrationSyntax (MigrationSteps steps) =
  runF (runKleisli steps ()) (\_ x -> x)
    (\(MigrationStep header migration next) x ->
       let (res, script) = renderMigration migration mempty
       in next res (x <> renderMigrationHeader header <> script)) mempty
  where
    renderMigration :: forall a'. Migration syntax a' -> m -> (a', m)
    renderMigration migrationSteps =
      runF migrationSteps (,)
           (\(MigrationRunCommand a _ next) x -> next (x <> renderMigrationSyntax a))

-- | Execute a given migration, provided a command to execute arbitrary syntax.
--   You usually use this with 'runNoReturn'.
executeMigration :: Applicative m => (syntax -> m ()) -> Migration syntax a -> m a
executeMigration runSyntax go = runF go pure doStep
  where
    doStep (MigrationRunCommand cmd _ next) =
      runSyntax cmd *> next

evaluateDatabase :: forall syntax a. MigrationSteps syntax () a -> a
evaluateDatabase (MigrationSteps f) = runF (runKleisli f ()) id (\(MigrationStep _ migration next) -> next (runMigration migration))
  where
    runMigration :: forall a'. Migration syntax a' -> a'
    runMigration migration = runF migration id (\(MigrationRunCommand _ _ next) -> next)

stepNames :: forall syntax a. MigrationSteps syntax () a -> [Text]
stepNames (MigrationSteps f) = runF (runKleisli f ()) (\_ x -> x) (\(MigrationStep nm migration next) x -> next (runMigration migration) (x ++ [nm])) []
  where
    runMigration :: forall a'. Migration syntax a' -> a'
    runMigration migration = runF migration id (\(MigrationRunCommand _ _ next) -> next)

-- * Checked database entities
