module Database.Beam.Migrate.Types where

import Database.Beam

import Control.Monad.Church.Free

import Data.Functor

data MigrationStep syntax next where
    MigrationStep :: Text -> Migration syntax a -> (a -> next) -> MigrationStep syntax next
deriving instance MigrationStep Functor
type MigrationSteps syntax = F (MigrationStep syntax)

data MigrationF syntax next
    = MigrationRunCommand
    { _migrationUpCommand   :: syntax {-^ What to execute when applying the migration -}
    , _migrationDownCommand :: Maybe syntax {-^ What to execute when unapplying the migration -}
    , _migrationNext :: next }
    deriving Functor
type Migration syntax = F (MigrationF syntax)

migrationStep :: Text -> Migration syntax () -> MigrationSteps syntax ()
migrationStep stepName migration =
    liftF (MigrationStep stepName migration ())

upDown :: syntax -> Maybe syntax -> Migration syntax ()
upDown up down = liftF (MigrationRunCommand up down ())
