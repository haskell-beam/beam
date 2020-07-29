{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

-- | Postgres extensions are run-time loadable plugins that can extend Postgres
-- functionality. Extensions are part of the database schema.
--
-- Beam fully supports including Postgres extensions in Beam databases. The
-- 'PgExtensionEntity' type constructor can be used to declare the existence of
-- the extension in a particular backend. @beam-postgres@ provides predicates
-- and checks for @beam-migrate@ which allow extensions to be included as
-- regular parts of beam migrations.
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
import           Data.Proxy
import           Data.Text (Text)
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

-- *** Embedding extensions in databases

-- | Represents an extension in a database.
--
-- For example, to include the "Database.Beam.Postgres.PgCrypto" extension in a
-- database,
--
-- @
-- import Database.Beam.Postgres.PgCrypto
--
-- data MyDatabase entity
--     = MyDatabase
--     { _table1 :: entity (TableEntity Table1)
--     , _cryptoExtension :: entity (PgExtensionEntity PgCrypto)
--     }
--
-- migratableDbSettings :: CheckedDatabaseSettings Postgres MyDatabase
-- migratableDbSettings = defaultMigratableDbSettings
--
-- dbSettings :: DatabaseSettings Postgres MyDatabase
-- dbSettings = unCheckDatabase migratableDbSettings
-- @
--
-- Note that our database now only works in the 'Postgres' backend.
--
-- Extensions are implemented as records of functions and values that expose
-- extension functionality. For example, the @pgcrypto@ extension (implemented
-- by 'PgCrypto') provides cryptographic functions. Thus, 'PgCrypto' is a record
-- of functions over 'QGenExpr' which wrap the underlying postgres
-- functionality.
--
-- You get access to these functions by retrieving them from the entity in the
-- database.
--
-- For example, to use the @pgcrypto@ extension in the database above:
--
-- @
-- let PgCrypto { pgCryptoDigestText = digestText
--              , pgCryptoCrypt = crypt } = getPgExtension (_cryptoExtension dbSettings)
-- in fmap_ (\tbl -> (tbl, crypt (_field1 tbl) (_salt tbl))) (all_ (table1 dbSettings))
-- @
--
-- To implement your own extension, create a record type, and implement the
-- 'IsPgExtension' type class.
data PgExtensionEntity extension

-- | Type class implemented by any Postgresql extension
class IsPgExtension extension where
  -- | Return the name of this extension. This should be the string that is
  -- passed to @CREATE EXTENSION@. For example, 'PgCrypto' returns @"pgcrypto"@.
  pgExtensionName :: Proxy extension -> Text

  -- | Return a value of this extension type. This should fill in all fields in
  -- the record. For example, 'PgCrypto' builds a record where each function
  -- wraps the underlying Postgres one.
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
  dbEntitySchema _ n = pure n
  dbEntityAuto _ = PgDatabaseExtension (pgExtensionName (Proxy @extension)) pgExtensionBuild

instance IsCheckedDatabaseEntity Postgres (PgExtensionEntity extension) where
  newtype CheckedDatabaseEntityDescriptor Postgres (PgExtensionEntity extension) =
    CheckedPgExtension (DatabaseEntityDescriptor Postgres (PgExtensionEntity extension))
  type CheckedDatabaseEntityDefaultRequirements Postgres (PgExtensionEntity extension) =
    DatabaseEntityRegularRequirements Postgres (PgExtensionEntity extension)

  unChecked f (CheckedPgExtension ext) = CheckedPgExtension <$> f ext
  collectEntityChecks (CheckedPgExtension (PgDatabaseExtension {})) =
    [ SomeDatabasePredicate (PgHasExtension (pgExtensionName (Proxy @extension))) ]
  checkedDbEntityAuto = CheckedPgExtension . dbEntityAuto

-- | Get the extension record from a database entity. See the documentation for
-- 'PgExtensionEntity'.
getPgExtension :: DatabaseEntity Postgres db (PgExtensionEntity extension)
               -> extension
getPgExtension (DatabaseEntity (PgDatabaseExtension _ ext)) = ext

-- *** Migrations support for extensions

-- | 'Migration' representing the Postgres @CREATE EXTENSION@ command. Because
-- the extension name is statically known by the extension type and
-- 'IsPgExtension' type class, this simply produces the checked extension
-- entity.
--
-- If you need to use the extension in subsequent migration steps, use
-- 'getPgExtension' and 'unCheck' to get access to the underlying
-- 'DatabaseEntity'.
pgCreateExtension :: forall extension db
                   . IsPgExtension extension
                  => Migration Postgres (CheckedDatabaseEntity Postgres db (PgExtensionEntity extension))
pgCreateExtension =
  let entity = checkedDbEntityAuto ""
      extName = pgExtensionName (Proxy @extension)
  in upDown (pgCreateExtensionSyntax extName) Nothing >>
     pure (CheckedDatabaseEntity entity (collectEntityChecks entity))

-- | 'Migration' representing the Postgres @DROP EXTENSION@. After this
-- executes, you should expect any further uses of the extension to fail.
-- Unfortunately, without linear types, we cannot check this.
pgDropExtension :: forall extension
                 . CheckedDatabaseEntityDescriptor Postgres (PgExtensionEntity extension)
                -> Migration Postgres ()
pgDropExtension (CheckedPgExtension (PgDatabaseExtension {})) =
  upDown (pgDropExtensionSyntax (pgExtensionName (Proxy @extension))) Nothing


-- | Postgres-specific database predicate asserting the existence of an
-- extension in the database. The 'pgExtensionActionProvider' properly provides
-- @CREATE EXTENSION@ and @DROP EXTENSION@ statements to the migration finder.
newtype PgHasExtension = PgHasExtension Text {- Extension Name -}
  deriving (Show, Eq, Generic, Hashable)
instance DatabasePredicate PgHasExtension where
  englishDescription (PgHasExtension extName) =
    "Postgres extension " ++ show extName ++ " is loaded"

  predicateSpecificity _ = PredicateSpecificityOnlyBackend "postgres"
  serializePredicate (PgHasExtension nm) =
    object [ "has-postgres-extension" .= nm ]

pgExtensionActionProvider :: ActionProvider Postgres
pgExtensionActionProvider = pgCreateExtensionProvider <> pgDropExtensionProvider

pgCreateExtensionProvider, pgDropExtensionProvider :: ActionProvider Postgres

pgCreateExtensionProvider =
  ActionProvider $ \findPre findPost ->
  do extP@(PgHasExtension ext) <- findPost
     ensuringNot_ $
       do PgHasExtension ext' <- findPre
          guard (ext == ext')

     let cmd = pgCreateExtensionSyntax ext
     pure (PotentialAction mempty (HS.fromList [p extP])
                           (pure (MigrationCommand cmd MigrationKeepsData))
                           ("Load the postgres extension " <> ext) 1)

pgDropExtensionProvider =
  ActionProvider $ \findPre findPost ->
  do extP@(PgHasExtension ext) <- findPre
     ensuringNot_ $
       do PgHasExtension ext' <- findPost
          guard (ext == ext')

     let cmd = pgDropExtensionSyntax ext
     pure (PotentialAction (HS.fromList [p extP]) mempty
                           (pure (MigrationCommand cmd MigrationKeepsData))
                           ("Unload the postgres extension " <> ext) 1)
