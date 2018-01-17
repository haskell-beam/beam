{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Beam.Postgres.Extensions where

import           Database.Beam
import           Database.Beam.Schema.Tables

import           Database.Beam.Postgres.Types
import           Database.Beam.Postgres.Syntax

import           Database.Beam.Migrate

import           Control.Monad

import           Data.Aeson
import qualified Data.HashSet as HS
import           Data.Hashable (Hashable)
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)

data PgExtensionEntity extension

class IsPgExtension extension where
  pgExtensionName :: Proxy extension -> Text
  pgExtensionBuild :: extension

-- | There are no fields to rename when defining entities
instance RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor Postgres (PgExtensionEntity e))) where
  renamingFields _ = FieldRenamer id

instance IsDatabaseEntity Postgres (PgExtensionEntity extension) where

  data DatabaseEntityDescriptor Postgres (PgExtensionEntity extension) where
    PgDatabaseExtension :: IsPgExtension extension
                        => Text
                        -> extension
                        -> DatabaseEntityDescriptor Postgres (PgExtensionEntity extension)
  type DatabaseEntityDefaultRequirements Postgres (PgExtensionEntity extension) =
    ( IsPgExtension extension )
  type DatabaseEntityRegularRequirements Postgres (PgExtensionEntity extension) =
    ( IsPgExtension extension )

  dbEntityName f (PgDatabaseExtension nm ext) = fmap (\nm' -> PgDatabaseExtension nm' ext) (f nm)
  dbEntityAuto _ = PgDatabaseExtension (pgExtensionName (Proxy @extension)) pgExtensionBuild

instance IsCheckedDatabaseEntity Postgres (PgExtensionEntity extension) where
  newtype CheckedDatabaseEntityDescriptor Postgres (PgExtensionEntity extension) =
    CheckedPgExtension (DatabaseEntityDescriptor Postgres (PgExtensionEntity extension))
  type CheckedDatabaseEntityDefaultRequirements Postgres (PgExtensionEntity extension) syntax =
    DatabaseEntityRegularRequirements Postgres (PgExtensionEntity extension)

  unCheck (CheckedPgExtension ext) = ext
  collectEntityChecks (CheckedPgExtension (PgDatabaseExtension {})) =
    [ SomeDatabasePredicate (PgHasExtension (pgExtensionName (Proxy @extension))) ]
  checkedDbEntityAuto _ = CheckedPgExtension . dbEntityAuto

getPgExtension :: CheckedDatabaseEntity Postgres db (PgExtensionEntity extension)
               -> extension
getPgExtension (CheckedDatabaseEntity (CheckedPgExtension (PgDatabaseExtension _ ext)) _) = ext

-- * Migration

pgCreateExtension :: forall extension db
                   . IsPgExtension extension
                  => Migration PgCommandSyntax (CheckedDatabaseEntity Postgres db (PgExtensionEntity extension))
pgCreateExtension =
  let entity = checkedDbEntityAuto (Proxy @PgCommandSyntax) ""
      extName = pgExtensionName (Proxy @extension)
  in upDown (pgCreateExtensionSyntax extName) Nothing >>
     pure (CheckedDatabaseEntity entity (collectEntityChecks entity))

pgDropExtension :: forall extension
                 . CheckedDatabaseEntityDescriptor Postgres (PgExtensionEntity extension)
                -> Migration PgCommandSyntax ()
pgDropExtension (CheckedPgExtension (PgDatabaseExtension {})) =
  upDown (pgDropExtensionSyntax (pgExtensionName (Proxy @extension))) Nothing


-- * Check

newtype PgHasExtension = PgHasExtension Text {- Extension Name -}
  deriving (Show, Eq, Generic, Hashable)
instance DatabasePredicate PgHasExtension where
  englishDescription (PgHasExtension extName) =
    "Postgres extension " ++ show extName ++ " is loaded"

  predicateSource _ = PredicateSourceBackend "postgres"
  serializePredicate (PgHasExtension nm) =
    object [ "has-postgres-extension" .= nm ]

pgExtensionActionProviders :: [ ActionProvider PgCommandSyntax ]
pgExtensionActionProviders = [ pgCreateExtensionProvider, pgDropExtensionProvider ]

pgCreateExtensionProvider, pgDropExtensionProvider :: ActionProvider PgCommandSyntax

pgCreateExtensionProvider =
  ActionProvider $ \findPre findPost ->
  do extP@(PgHasExtension ext) <- findPost
     ensuringNot_ $
       do PgHasExtension ext' <- findPre
          guard (ext == ext')

     let cmd = pgCreateExtensionSyntax ext
     pure (PotentialAction mempty (HS.fromList [p extP]) (pure cmd)
                           ("Load the postgres extension " <> ext) 1)

pgDropExtensionProvider =
  ActionProvider $ \findPre findPost ->
  do extP@(PgHasExtension ext) <- findPre
     ensuringNot_ $
       do PgHasExtension ext' <- findPost
          guard (ext == ext')

     let cmd = pgDropExtensionSyntax ext
     pure (PotentialAction (HS.fromList [p extP]) mempty (pure cmd)
                           ("Unload the postgres extension " <> ext) 1)
