{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.Types
  ( -- * Checked database entities
    CheckedDatabaseSettings

  , IsCheckedDatabaseEntity(..)
  , CheckedDatabaseEntityDescriptor(..)
  , CheckedDatabaseEntity(..)

  -- , unCheckDatabase, collectChecks

    -- ** Modifyinging checked entities
    --
    --    The functions in this section can be used to modify 'CheckedDatabaseSettings' objects.
  , CheckedFieldModification
  , checkedFieldNamed

  , modifyCheckedTable
  , checkedTableModification

    -- * Predicates
  , DatabasePredicate(..)
  , SomeDatabasePredicate(..)
  , PredicateSpecificity(..)

  , p

    -- * Entity checks
  , TableCheck(..), DomainCheck(..)
  , FieldCheck(..)

    -- * Migrations
  , MigrationStep(..), MigrationSteps(..)
  , Migration, MigrationF(..)

  , MigrationCommand(..), MigrationDataLoss(..)

  , runMigrationSteps, runMigrationSilenced
  , runMigrationVerbose, executeMigration
  , eraseMigrationType, migrationStep, upDown
  , migrationDataLoss

  , migrateScript, evaluateDatabase, stepNames ) where

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Syntax

import Database.Beam.Migrate.Types.CheckedEntities
import Database.Beam.Migrate.Types.Predicates

import Control.Monad.Free.Church
import Control.Arrow
import Control.Category (Category)

import Data.Monoid
import Data.Text (Text)

-- * Migration types

data MigrationStep next where
    MigrationStep :: Text -> Migration a -> (a -> next) -> MigrationStep next
deriving instance Functor MigrationStep
newtype MigrationSteps from to = MigrationSteps (Kleisli (F MigrationStep) from to)
  deriving (Category, Arrow)

data MigrationF next where
  MigrationRunCommand
    :: { _migrationUpCommand   :: Command {-^ What to execute when applying the migration -}
       , _migrationDownCommand :: Maybe Command {-^ What to execute when unapplying the migration -}
       , _migrationNext :: next }
    -> MigrationF next
deriving instance Functor MigrationF 
type Migration = F MigrationF 

-- | Information on whether a 'MigrationCommand' loses data. You can
-- monoidally combine these to get the potential data loss for a
-- sequence of commands.
data MigrationDataLoss
  = MigrationLosesData
    -- ^ The command loses data
  | MigrationKeepsData
    -- ^ The command keeps all data
  deriving Show

instance Monoid MigrationDataLoss where
    mempty = MigrationKeepsData
instance Semigroup MigrationDataLoss where
    (<>) MigrationLosesData _ = MigrationLosesData
    (<>) _ MigrationLosesData = MigrationLosesData
    (<>) MigrationKeepsData MigrationKeepsData = MigrationKeepsData

-- | A migration command along with metadata on wheth
data MigrationCommand cmd
  = MigrationCommand
  { migrationCommand :: cmd
    -- ^ The command to run
  , migrationCommandDataLossPossible :: MigrationDataLoss
    -- ^ Information on whether the migration loses data
  } deriving Show

runMigrationSteps :: Monad m
                  => Int -> Maybe Int
                  -> MigrationSteps () a
                  -> (forall a'. Int -> Text -> Migration a' -> m a')
                  -> m a
runMigrationSteps firstIdx lastIdx (MigrationSteps steps) runMigration =
  runF (runKleisli steps ()) finish step 0
  where finish x _ = pure x
        step (MigrationStep nm doStep next) i =
          if i >= firstIdx && maybe True (i <) lastIdx
          then runMigration i nm doStep >>= \x -> next x (i + 1)
          else next (runMigrationSilenced doStep) (i + 1)

runMigrationSilenced :: Migration a -> a
runMigrationSilenced m = runF m id step
  where
    step (MigrationRunCommand _ _ next) = next

runMigrationVerbose :: MonadBeam hdl m => (Command -> String)
                    -> Migration a -> m a
runMigrationVerbose renderMigrationSyntax steps =
  runF steps finish step
  where finish = pure
        step (MigrationRunCommand up _ next) =
          do liftIO (putStrLn (renderMigrationSyntax up))
             runNoReturn up
             next

eraseMigrationType :: a -> MigrationSteps a a' -> MigrationSteps () ()
eraseMigrationType a (MigrationSteps steps) = MigrationSteps (arr (const a) >>> steps >>> arr (const ()))

migrationStep :: Text -> (a -> Migration a') -> MigrationSteps a a'
migrationStep stepName migration =
    MigrationSteps (Kleisli (\a -> liftF (MigrationStep stepName (migration a) id)))

upDown :: Command -> Maybe Command -> Migration ()
upDown up down = liftF (MigrationRunCommand up down ())

migrateScript :: forall m a.
                 Monoid m => (Text -> m) -> (Command -> m) -> MigrationSteps () a -> m
migrateScript renderMigrationHeader renderMigrationSyntax (MigrationSteps steps) =
  runF (runKleisli steps ()) (\_ x -> x)
    (\(MigrationStep header migration next) x ->
       let (res, script) = renderMigration migration mempty
       in next res (x <> renderMigrationHeader header <> script)) mempty
  where
    renderMigration :: forall a'. Migration a' -> m -> (a', m)
    renderMigration migrationSteps =
      runF migrationSteps (,)
           (\(MigrationRunCommand a _ next) x -> next (x <> renderMigrationSyntax a))

-- | Execute a given migration, provided a command to execute arbitrary syntax.
--   You usually use this with 'runNoReturn'.
executeMigration :: Applicative m => (Command -> m ()) -> Migration a -> m a
executeMigration runSyntax go = runF go pure doStep
  where
    doStep (MigrationRunCommand cmd _ next) =
      runSyntax cmd *> next

-- | Given a migration, get the potential data loss, if it's run top-down
migrationDataLoss :: Migration a -> MigrationDataLoss
migrationDataLoss go = runF go (\_ -> MigrationKeepsData)
                         (\(MigrationRunCommand _ x next) ->
                            case x of
                              Nothing -> MigrationLosesData
                              _ -> next)

evaluateDatabase :: forall a. MigrationSteps () a -> a
evaluateDatabase (MigrationSteps f) = runF (runKleisli f ()) id (\(MigrationStep _ migration next) -> next (runMigration migration))
  where
    runMigration :: forall a'. Migration a' -> a'
    runMigration migration = runF migration id (\(MigrationRunCommand _ _ next) -> next)

stepNames :: forall a. MigrationSteps () a -> [Text]
stepNames (MigrationSteps f) = runF (runKleisli f ()) (\_ x -> x) (\(MigrationStep nm migration next) x -> next (runMigration migration) (x ++ [nm])) []
  where
    runMigration :: forall a'. Migration a' -> a'
    runMigration migration = runF migration id (\(MigrationRunCommand _ _ next) -> next)

-- * Checked database entities
