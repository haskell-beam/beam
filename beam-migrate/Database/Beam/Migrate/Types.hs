{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Database.Beam.Migrate.Types
  ( -- * Checked database entities
    CheckedDatabaseSettings

  , IsCheckedDatabaseEntity(..)
  , CheckedDatabaseEntityDescriptor(..)
  , CheckedDatabaseEntity(..)

  , unCheckDatabase, collectChecks

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
  , executeMigration, eraseMigrationType, migrationStep
  , upDown, migrationDataLoss

  , migrateScript, evaluateDatabase, stepNames ) where

import Database.Beam.Migrate.Types.CheckedEntities
import Database.Beam.Migrate.Types.Predicates

import Control.Monad.Free.Church
import Control.Arrow
import Control.Category (Category)

#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup
#endif
import Data.Text (Text)

-- * Migration types

-- | Represents a particular step in a migration
data MigrationStep syntax next where
    MigrationStep :: Text -> Migration syntax a -> (a -> next) -> MigrationStep syntax next
deriving instance Functor (MigrationStep syntax)

-- | A series of 'MigrationStep's that take a database from the schema in @from@
-- to the one in @to@. Use the 'migrationStep' function and the arrow interface
-- to sequence 'MigrationSteps'.
newtype MigrationSteps syntax from to = MigrationSteps (Kleisli (F (MigrationStep syntax)) from to)
  deriving (Category, Arrow)

-- | Free monadic function for 'Migration's
data MigrationF syntax next where
  MigrationRunCommand
    :: { _migrationUpCommand   :: syntax {-^ What to execute when applying the migration -}
       , _migrationDownCommand :: Maybe syntax {-^ What to execute when unapplying the migration -}
       , _migrationNext :: next }
    -> MigrationF syntax next
deriving instance Functor (MigrationF syntax)

-- | A sequence of potentially reversible schema update commands
type Migration syntax = F (MigrationF syntax)

-- | Information on whether a 'MigrationCommand' loses data. You can
-- monoidally combine these to get the potential data loss for a
-- sequence of commands.
data MigrationDataLoss
  = MigrationLosesData
    -- ^ The command loses data
  | MigrationKeepsData
    -- ^ The command keeps all data
  deriving Show

instance Semigroup MigrationDataLoss where
    (<>) = mappend

instance Monoid MigrationDataLoss where
    mempty = MigrationKeepsData
    mappend MigrationLosesData _ = MigrationLosesData
    mappend _ MigrationLosesData = MigrationLosesData
    mappend MigrationKeepsData MigrationKeepsData = MigrationKeepsData

-- | A migration command along with metadata on wheth
data MigrationCommand cmd
  = MigrationCommand
  { migrationCommand :: cmd
    -- ^ The command to run
  , migrationCommandDataLossPossible :: MigrationDataLoss
    -- ^ Information on whether the migration loses data
  } deriving Show

-- | Run the migration steps between the given indices, using a custom execution function.
runMigrationSteps :: Monad m
                  => Int -- ^ Zero-based index of the first step to run
                  -> Maybe Int -- ^ Index of the last step to run, or 'Nothing' to run every step
                  -> MigrationSteps syntax () a -- ^ The set of steps to run
                  -> (forall a'. Int -> Text -> Migration syntax a' -> m a')
                  -- ^ Callback for each step. Called with the step index, the
                  -- step description and the migration.
                  -> m a
runMigrationSteps firstIdx lastIdx (MigrationSteps steps) runMigration =
  runF (runKleisli steps ()) finish step 0
  where finish x _ = pure x
        step (MigrationStep nm doStep next) i =
          if i >= firstIdx && maybe True (i <) lastIdx
          then runMigration i nm doStep >>= \x -> next x (i + 1)
          else next (runMigrationSilenced doStep) (i + 1)

-- | Get the result of a migration, without running any steps
runMigrationSilenced :: Migration syntax a -> a
runMigrationSilenced m = runF m id step
  where
    step (MigrationRunCommand _ _ next) = next

-- | Remove the explicit source and destination schemas from a 'MigrationSteps' object
eraseMigrationType :: a -> MigrationSteps syntax a a' -> MigrationSteps syntax () ()
eraseMigrationType a (MigrationSteps steps) = MigrationSteps (arr (const a) >>> steps >>> arr (const ()))

-- | Create a 'MigrationSteps' from the given description and migration function.
migrationStep :: Text -> (a -> Migration syntax a') -> MigrationSteps syntax a a'
migrationStep stepName migration =
    MigrationSteps (Kleisli (\a -> liftF (MigrationStep stepName (migration a) id)))

-- | Given a command in the forward direction, and an optional one in the
-- reverse direction, construct a 'Migration' that performs the given
-- command. Multiple commands can be sequenced monadically.
upDown :: syntax -> Maybe syntax -> Migration syntax ()
upDown up down = liftF (MigrationRunCommand up down ())

-- | Given functions to render a migration step description and the underlying
-- syntax, create a script for the given 'MigrationSteps'.
migrateScript :: forall syntax m a. (Monoid m, Semigroup m)
              => (Text -> m)
              -- ^ Called at the beginning of each 'MigrationStep' with the step description
              -> (syntax -> m)
              -- ^ Called for each command in the migration step
              -> MigrationSteps syntax () a
              -- ^ The set of steps to run
              -> m
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

-- | Given a migration, get the potential data loss, if it's run top-down
migrationDataLoss :: Migration syntax a -> MigrationDataLoss
migrationDataLoss go = runF go (\_ -> MigrationKeepsData)
                         (\(MigrationRunCommand _ x next) ->
                            case x of
                              Nothing -> MigrationLosesData
                              _ -> next)

-- | Run a 'MigrationSteps' without executing any of the commands against a
-- database.
evaluateDatabase :: forall syntax a. MigrationSteps syntax () a -> a
evaluateDatabase (MigrationSteps f) = runF (runKleisli f ()) id (\(MigrationStep _ migration next) -> next (runMigration migration))
  where
    runMigration :: forall a'. Migration syntax a' -> a'
    runMigration migration = runF migration id (\(MigrationRunCommand _ _ next) -> next)

-- | Collect the names of all steps in hte given 'MigrationSteps'
stepNames :: forall syntax a. MigrationSteps syntax () a -> [Text]
stepNames (MigrationSteps f) = runF (runKleisli f ()) (\_ x -> x) (\(MigrationStep nm migration next) x -> next (runMigration migration) (x ++ [nm])) []
  where
    runMigration :: forall a'. Migration syntax a' -> a'
    runMigration migration = runF migration id (\(MigrationRunCommand _ _ next) -> next)

