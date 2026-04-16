{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.TempTable
  ( -- * Temporary tables

    -- ** @CREATE TEMPORARY TABLE@
    runCreateTempTable

    -- ** Options
  , TempTableCreateMode(..)
  , TempTableOptions(..)
  , defaultTempTableOptions

    -- ** Backend support for temporary tables
  , BeamHasTempTables(..)

    -- ** Schema constraint
  , HasDefaultTempTableSchema

    -- ** Internals
  , GNullableConstraintsForSchema(..)
  , GDefaultBackendFieldSchemas(..)
  ) where

import           Control.Monad.Writer (execWriter, tell)

import           Data.Functor.Const (Const(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           GHC.Generics

import           Database.Beam.Backend.SQL
  ( MonadBeam(..), BeamSqlBackendSyntax )
import           Database.Beam.Backend.Types (Nullable)
import           Database.Beam.Migrate.Generics (HasDefaultSqlDataType(..), NullableStatus)
import           Database.Beam.Migrate.SQL.SQL92
  ( IsSql92ColumnSchemaSyntax(..)
  , IsSql92ColumnConstraintDefinitionSyntax(..)
  , IsSql92ColumnConstraintSyntax(..)
  )
import           Database.Beam.Migrate.SQL.Types
  ( BeamMigrateSqlBackend
  , BeamSqlBackendColumnSchemaSyntax
  , BeamSqlBackendColumnConstraintDefinitionSyntax
  )
import           Database.Beam.Query (HasQBuilder)
import           Database.Beam.Schema.Tables
  ( DatabaseEntity(..), DatabaseEntityDescriptor(..)
  , TableEntity
  , TableSettings, TableField(..)
  , defTblFieldSettings, allBeamValues, zipBeamFieldsM
  , Columnar'(..), Table(..)
  , GDefaultTableFieldSettings
  )

--------------------------------------------------------------------------------

-- | How to handle an existing temporary table when calling 'runCreateTempTable'.
data TempTableCreateMode
  = CreateIfNotExists
    -- ^ Emit @CREATE TEMPORARY TABLE IF NOT EXISTS@.
    -- If the table already exists, do nothing.
  | DropAndCreate
    -- ^ Emit @DROP TABLE IF EXISTS@ followed by @CREATE TEMPORARY TABLE@.
    -- The table is always (re)created fresh.
  deriving (Eq, Ord, Show)

-- | Options for 'runCreateTempTable'.
data TempTableOptions = TempTableOptions
  { tempTableCreateMode :: TempTableCreateMode
    -- ^ Whether to preserve an existing table or recreate it from scratch.
  }

-- | Default options: use @CREATE TEMPORARY TABLE IF NOT EXISTS@.
defaultTempTableOptions :: TempTableOptions
defaultTempTableOptions =
  TempTableOptions { tempTableCreateMode = CreateIfNotExists }

-- | Support for temporary tables in a Beam backend.
class (BeamMigrateSqlBackend be, HasQBuilder be) => BeamHasTempTables be where
  -- | Render a @CREATE TEMPORARY TABLE@ command.
  createTempTableCmd
    :: T.Text
       -- ^ table name
    -> [(T.Text, BeamSqlBackendColumnSchemaSyntax be)]
       -- ^ ordered (column name, column schema) pairs
    -> [T.Text]
       -- ^ primary key column names (empty for no primary key constraint)
    -> Bool
       -- ^ include @IF NOT EXISTS@?
    -> BeamSqlBackendSyntax be

  -- | Render a @DROP TABLE IF EXISTS@ command.
  dropTempTableIfExistsCmd
    :: T.Text
       -- ^ table name
    -> BeamSqlBackendSyntax be


-- | Constraint used for deriving the schema of a temporary table.
type HasDefaultTempTableSchema be tbl =
  ( BeamHasTempTables be
  , Generic (tbl (Const (BeamSqlBackendColumnSchemaSyntax be)))
  , GDefaultBackendFieldSchemas be
      (Rep (tbl Identity))
      (Rep (tbl (Const (BeamSqlBackendColumnSchemaSyntax be))))
  , Table tbl
  , Generic (TableSettings tbl)
  , GDefaultTableFieldSettings (Rep (TableSettings tbl) ())
  )

-- ---------------------------------------------------------------------------
-- CREATE TEMPORARY TABLE

-- | Execute @CREATE TEMPORARY TABLE@, returning the created temporary table
-- entity.
--
-- Note that the @db@ parameter is phantom; if you need to change it you can
-- use 'Data.Coerce.coerce'.
runCreateTempTable
  :: forall be db tbl m
  .  ( HasDefaultTempTableSchema be tbl
     , MonadBeam be m )
  => TempTableOptions
  -> T.Text -- ^ temporary table name
  -> m (DatabaseEntity be db (TableEntity tbl))
runCreateTempTable opts nm = do
  let tblSettings = defTblFieldSettings :: TableSettings tbl
      colSchemas  = deriveTblColSchemas (Proxy @be) (Proxy @tbl)

      -- Collect (columnName, columnSchema) pairs in field declaration order.
      fieldDefs :: [(T.Text, BeamSqlBackendColumnSchemaSyntax be)]
      fieldDefs = execWriter $
        zipBeamFieldsM
          (\(Columnar' tf) c@(Columnar' (Const schema)) ->
              do tell [(_fieldName tf, schema)]
                 return c
          )
          tblSettings colSchemas

      pkFields =
        allBeamValues (\(Columnar' tf) -> _fieldName tf) (primaryKey tblSettings)

  case tempTableCreateMode opts of
    CreateIfNotExists ->
      runNoReturn (createTempTableCmd @be nm fieldDefs pkFields True)
    DropAndCreate -> do
      runNoReturn (dropTempTableIfExistsCmd @be nm)
      runNoReturn (createTempTableCmd @be nm fieldDefs pkFields False)

  let entity = DatabaseEntity $ DatabaseTable
        { dbTableSchema      = Nothing
        , dbTableOrigName    = nm
        , dbTableCurrentName = nm
        , dbTableSettings    = tblSettings
        }
  pure entity

deriveTblColSchemas
  :: forall be tbl
  .  HasDefaultTempTableSchema be tbl
  => Proxy be -> Proxy tbl -> tbl (Const (BeamSqlBackendColumnSchemaSyntax be))
deriveTblColSchemas pBe _ =
  to $ gDefaultBackendFieldSchemas pBe (Proxy @(Rep (tbl Identity))) False

------------------------------------------------------------------------------
-- Generics machinery (internal)

class BeamMigrateSqlBackend be
    => GNullableConstraintsForSchema (nullable :: Bool) be where
  nullableConstraintsForSchema
    :: Proxy nullable
    -> Proxy be
    -> [BeamSqlBackendColumnConstraintDefinitionSyntax be]

instance BeamMigrateSqlBackend be
    => GNullableConstraintsForSchema 'False be where
  nullableConstraintsForSchema _ _ =
    [ constraintDefinitionSyntax Nothing notNullConstraintSyntax Nothing ]

instance BeamMigrateSqlBackend be
    => GNullableConstraintsForSchema 'True be where
  nullableConstraintsForSchema _ _ = []

class BeamMigrateSqlBackend be
    => GDefaultBackendFieldSchemas be (i :: Type -> Type) (o :: Type -> Type) where
  gDefaultBackendFieldSchemas :: Proxy be -> Proxy i -> Bool -> o ()

instance ( BeamMigrateSqlBackend be
         , GDefaultBackendFieldSchemas be xId schemaId )
    => GDefaultBackendFieldSchemas be (M1 t s xId) (M1 t s schemaId) where
  gDefaultBackendFieldSchemas pBe _ embedded =
    M1 (gDefaultBackendFieldSchemas pBe (Proxy @xId) embedded)

instance ( BeamMigrateSqlBackend be
         , GDefaultBackendFieldSchemas be aId aSchema
         , GDefaultBackendFieldSchemas be bId bSchema )
    => GDefaultBackendFieldSchemas be (aId :*: bId) (aSchema :*: bSchema) where
  gDefaultBackendFieldSchemas pBe _ embedded =
    gDefaultBackendFieldSchemas pBe (Proxy @aId) embedded :*:
    gDefaultBackendFieldSchemas pBe (Proxy @bId) embedded


instance ( HasDefaultSqlDataType be haskTy
         , GNullableConstraintsForSchema (NullableStatus haskTy) be
         , cs ~ BeamSqlBackendColumnSchemaSyntax be )
    => GDefaultBackendFieldSchemas be
         (Rec0 haskTy)
         (Rec0 (Const cs haskTy)) where
  gDefaultBackendFieldSchemas pBe _ embedded =
    K1 $ Const $ columnSchemaSyntax
      (defaultSqlDataType (Proxy @haskTy) pBe embedded)
      Nothing
      (nullableConstraintsForSchema (Proxy @(NullableStatus haskTy)) pBe)
      Nothing

instance ( BeamMigrateSqlBackend be
         , cs ~ BeamSqlBackendColumnSchemaSyntax be
         , Generic (embeddedTbl (Const cs))
         , GDefaultBackendFieldSchemas be
             (Rep (embeddedTbl Identity))
             (Rep (embeddedTbl (Const cs))) )
    => GDefaultBackendFieldSchemas be
         (Rec0 (embeddedTbl Identity))
         (Rec0 (embeddedTbl (Const cs))) where
  gDefaultBackendFieldSchemas pBe _ _ =
    K1 $ to $ gDefaultBackendFieldSchemas pBe
      (Proxy @(Rep (embeddedTbl Identity))) True

instance ( BeamMigrateSqlBackend be
         , cs ~ BeamSqlBackendColumnSchemaSyntax be
         , Generic (embeddedTbl (Nullable (Const cs)))
         , GDefaultBackendFieldSchemas be
             (Rep (embeddedTbl (Nullable Identity)))
             (Rep (embeddedTbl (Nullable (Const cs)))) )
    => GDefaultBackendFieldSchemas be
         (Rec0 (embeddedTbl (Nullable Identity)))
         (Rec0 (embeddedTbl (Nullable (Const cs)))) where
  gDefaultBackendFieldSchemas pBe _ _ =
    K1 $ to $ gDefaultBackendFieldSchemas pBe
      (Proxy @(Rep (embeddedTbl (Nullable Identity)))) True

