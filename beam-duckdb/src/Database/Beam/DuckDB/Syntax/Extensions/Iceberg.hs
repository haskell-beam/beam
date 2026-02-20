{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.DuckDB.Syntax.Extensions.Iceberg
  ( -- ** Specifying Parquet files as part of the database
    icebergTable,
    icebergTableWith,
    modifyIcebergTableFields,
    IcebergTableEntity,

    -- *** Iceberg table options
    IcebergTableOptions (..),
    defaultIcebergTableOptions,

    -- ** Querying data from a Parquet file
    allFromIceberg_,
  )
where

import Control.Monad.Free (liftF)
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.Monoid (Endo (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Beam (Beamable, DatabaseEntity, EntityModification, FieldModification, QExpr, Table, TableField, TableSettings, defTblFieldSettings, withTableModification)
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Syntax (DuckDBFromSyntax (..))
import Database.Beam.DuckDB.Syntax.Builder (emit, emit', emitChar, quotedIdentifier, sepBy)
import Database.Beam.Query.Internal (Q (..), QF (..), tableFieldsToExpressions)
import Database.Beam.Schema.Tables
  ( Columnar' (..),
    DatabaseEntity (..),
    EntityModification (..),
    FieldRenamer (..),
    GDefaultTableFieldSettings,
    IsDatabaseEntity (..),
    RenamableField (..),
    RenamableWithRule (..),
    changeBeamRep,
  )
import GHC.Generics (Generic (Rep))

-- | A phantom type tag for Apache Iceberg table entities, analogous to 'TableEntity'
-- and 'ViewEntity'.
data IcebergTableEntity (table :: (Type -> Type) -> Type)

-- | Options affecting the handling of Apache Iceberg tables.
--
-- See 'defaultIcebergTableOptions' for default options.
data IcebergTableOptions = IcebergTableOptions
  { -- | Allow scanning Iceberg tables that are moved. Default: False
    allowMovedPaths :: Maybe Bool,
    -- | Provides an explicit version string, hint file, or guessing. Default: @"?"@
    version :: Maybe Text
  }
  deriving (Eq, Show)

defaultIcebergTableOptions :: IcebergTableOptions
defaultIcebergTableOptions = IcebergTableOptions Nothing Nothing

instance (Beamable tbl) => RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor DuckDB (IcebergTableEntity tbl))) where
  renamingFields renamer =
    FieldRenamer $ \tbl ->
      tbl
        { icebergTableSettings =
            changeBeamRep
              ( \(Columnar' tblField :: Columnar' (TableField tbl) a) ->
                  Columnar'
                    ( renameField
                        (Proxy @(TableField tbl))
                        (Proxy @a)
                        renamer
                        tblField
                    ) ::
                    Columnar' (TableField tbl) a
              )
              $ icebergTableSettings tbl
        }

instance (Beamable table) => IsDatabaseEntity DuckDB (IcebergTableEntity table) where
  data DatabaseEntityDescriptor DuckDB (IcebergTableEntity table)
    = IcebergTableDescriptor
    { icebergSource :: !FilePath,
      icebergName :: !Text, -- Only exists so that renaming this entity works. However, it serves no purpose
      icebergTableOptions :: !IcebergTableOptions,
      icebergTableSettings :: !(TableSettings table)
    }

  type
    DatabaseEntityDefaultRequirements DuckDB (IcebergTableEntity table) =
      ( GDefaultTableFieldSettings (Rep (TableSettings table) ()),
        Generic (TableSettings table),
        Table table,
        Beamable table
      )
  type
    DatabaseEntityRegularRequirements DuckDB (IcebergTableEntity table) =
      (Table table, Beamable table)

  -- This 'dbEntityName' is useless. Changing it with 'modifyEntityName' does effectively nothing.
  dbEntityName f vw = fmap (\t' -> vw {icebergName = t'}) (f (icebergName vw))
  dbEntitySchema f vw = fmap (const vw) (f Nothing) -- Schema doesn't apply to Parquet files
  dbEntityAuto nm =
    IcebergTableDescriptor
      { icebergSource = Text.unpack nm,
        icebergName = nm,
        icebergTableOptions = defaultIcebergTableOptions,
        icebergTableSettings = defTblFieldSettings
      }

-- | Define an Apache Iceberg table with default table options.
--
-- See 'icebergTableWith' if you want to change the default options.
icebergTable ::
  -- | File path
  FilePath ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (IcebergTableEntity table)
icebergTable path = icebergTableWith path defaultIcebergTableOptions

-- | Define an Apache Iceberg table with options.
--
-- See 'icebergTable' if you want to use default options.
icebergTableWith ::
  -- | File path
  FilePath ->
  -- | Iceberg table options. 
  IcebergTableOptions ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (IcebergTableEntity table)
icebergTableWith path options = EntityModification $ Endo $ \(DatabaseEntity desc) ->
  DatabaseEntity
    desc
      { icebergSource = path
      , icebergTableOptions = options
      }

allFromIceberg_ ::
  (Beamable table) =>
  DatabaseEntity DuckDB db (IcebergTableEntity table) ->
  Q DuckDB db s (table (QExpr DuckDB s))
allFromIceberg_ (DatabaseEntity desc) =
  Q $
    liftF
      ( QAll
          ( \_ name ->
              DuckDBFromSyntax $
                emit "iceberg_scan("
                  <> emitChar '\''
                  <> emit (Text.pack (icebergSource desc))
                  <> emitChar '\''
                  <> emitOptions (icebergTableOptions desc)
                  <> emit ") AS "
                  <> quotedIdentifier name
          )
          (tableFieldsToExpressions (icebergTableSettings desc))
          (const Nothing)
          snd
      )
  where
    emitOptions options@(IcebergTableOptions mAllowMovedPaths mVersion) =
      if options == defaultIcebergTableOptions
        then mempty
        else emit ", " <>
      sepBy
        (emit ", ")
        ( catMaybes
            [ fmap (\x -> emit "allow_moved_paths=" <> emit' x) mAllowMovedPaths
            , fmap (\x -> emit "version='" <> emit x <> emitChar '\'') mVersion
            ]
        )

-- | Construct an 'EntityModification' to rename the fields of a 'ParquetFileEntity'
modifyIcebergTableFields ::
  (Beamable tbl) =>
  tbl (FieldModification (TableField tbl)) ->
  EntityModification (DatabaseEntity DuckDB db) be (IcebergTableEntity tbl)
modifyIcebergTableFields modFields =
  EntityModification
    ( Endo
        ( \(DatabaseEntity tbl) ->
            DatabaseEntity tbl {icebergTableSettings = withTableModification modFields (icebergTableSettings tbl)}
        )
    )
