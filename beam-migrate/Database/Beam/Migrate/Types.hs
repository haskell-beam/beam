module Database.Beam.Migrate.Types where

import Database.Beam

import Control.Monad.Free.Church

import Data.Functor
import Data.Monoid
import Data.Text (Text)

data MigrationStep syntax next where
    MigrationStep :: Text -> Migration syntax a -> (a -> next) -> MigrationStep syntax next
deriving instance Functor (MigrationStep syntax)
type MigrationSteps syntax = F (MigrationStep syntax)

data MigrationF syntax next
    = MigrationRunCommand
    { _migrationUpCommand   :: syntax {-^ What to execute when applying the migration -}
    , _migrationDownCommand :: Maybe syntax {-^ What to execute when unapplying the migration -}
    , _migrationNext :: next }
    deriving Functor
type Migration syntax = F (MigrationF syntax)

migrationStep :: Text -> Migration syntax a -> MigrationSteps syntax a
migrationStep stepName migration =
    liftF (MigrationStep stepName migration id)

upDown :: syntax -> Maybe syntax -> Migration syntax ()
upDown up down = liftF (MigrationRunCommand up down ())

migrateScript :: forall syntax m a.
                 Monoid m => (Text -> m) -> (syntax -> m) -> MigrationSteps syntax a -> m
migrateScript renderMigrationHeader renderSyntax steps =
  runF steps (\_ x -> x)
    (\(MigrationStep header migration next) x ->
       let (res, script) = renderMigration migration mempty
       in next res (x <> renderMigrationHeader header <> script)) mempty
  where
    renderMigration :: forall a. Migration syntax a -> m -> (a, m)
    renderMigration steps =
      runF steps (,)
           (\(MigrationRunCommand a _ next) x -> next (x <> renderSyntax a))
