
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.DuckDB.Syntax.Extensions.Parquet
  ( -- ** Specifying Parquet files as part of the database
    parquetFile,
    modifyParquetFileFields,
    ParquetFileEntity,

    -- ** Specifying the source of Parquet-encoded data
    ParquetSource,
    singleParquetFile,
    multipleParquetFiles,

    -- ** Querying data from a Parquet file
    allFromParquet_,
  )
where

import Control.Monad.Free (liftF)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Endo (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Beam (Beamable, DatabaseEntity, EntityModification, FieldModification, QExpr, Table, TableField, TableSettings, defTblFieldSettings, withTableModification)
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Syntax (DuckDBFromSyntax (..))
import Database.Beam.DuckDB.Syntax.Builder (emit, emitChar, quotedIdentifier, sepBy)
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
import qualified Data.List.NonEmpty as NonEmpty

-- | A phantom type tag for parquet file entities, analogous to 'TableEntity'
-- and 'ViewEntity'.
data ParquetFileEntity (table :: (Type -> Type) -> Type)

-- | All of the different ways Parquet-encoded data can be read
-- by DuckDB.
--
-- Note that this type's constructors aren't exported; use functions such as `singleParquetFile`
-- or `multipleParquetFiles` to specify this
data ParquetSource
  = SingleParquetFile FilePath
  | MultipleParquetFiles (NonEmpty FilePath)

-- | Parquet data stored in a single file. Alternatively,
-- the filepath can represent a glob pattern.
singleParquetFile :: FilePath -> ParquetSource
singleParquetFile = SingleParquetFile

-- | Multiple Parquet files. These files will be treated as a single source;
-- they must have the same schema.
multipleParquetFiles :: NonEmpty FilePath -> ParquetSource
multipleParquetFiles = MultipleParquetFiles

instance (Beamable tbl) => RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor DuckDB (ParquetFileEntity tbl))) where
  renamingFields renamer =
    FieldRenamer $ \tbl ->
      tbl
        { parquetTableSettings =
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
              $ parquetTableSettings tbl
        }

instance (Beamable table) => IsDatabaseEntity DuckDB (ParquetFileEntity table) where
  data DatabaseEntityDescriptor DuckDB (ParquetFileEntity table)
    = ParquetFileEntityDescriptor
    { parquetSource :: !ParquetSource,
      parquetName :: !Text, -- Only exists so that renaming this entity works. However, it serves no purpose
      parquetTableSettings :: !(TableSettings table)
    }

  type
    DatabaseEntityDefaultRequirements DuckDB (ParquetFileEntity table) =
      ( GDefaultTableFieldSettings (Rep (TableSettings table) ()),
        Generic (TableSettings table),
        Table table,
        Beamable table
      )
  type
    DatabaseEntityRegularRequirements DuckDB (ParquetFileEntity table) =
      (Table table, Beamable table)

  -- This 'dbEntityName' is useless. Changing it with 'modifyEntityName' does effectively nothing.
  dbEntityName f vw = fmap (\t' -> vw {parquetName = t'}) (f (parquetName vw))
  dbEntitySchema f vw = fmap (const vw) (f Nothing) -- Schema doesn't apply to Parquet files
  dbEntityAuto nm =
    ParquetFileEntityDescriptor
      { parquetSource = singleParquetFile (Text.unpack nm),
        parquetName = nm,
        parquetTableSettings = defTblFieldSettings
      }

parquetFile ::
  -- | File path or glob
  ParquetSource ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (ParquetFileEntity table)
parquetFile path = EntityModification $ Endo $ \(DatabaseEntity desc) ->
  DatabaseEntity
    desc
      { parquetSource = path
      }

allFromParquet_ ::
  (Beamable table) =>
  DatabaseEntity DuckDB db (ParquetFileEntity table) ->
  Q DuckDB db s (table (QExpr DuckDB s))
allFromParquet_ (DatabaseEntity desc) =
  Q $
    liftF
      ( QAll
          ( \_ name ->
              DuckDBFromSyntax $
                emit "read_parquet("
                  <> arg (parquetSource desc)
                  <> emit ") AS "
                  <> quotedIdentifier name
          )
          (tableFieldsToExpressions (parquetTableSettings desc))
          (const Nothing)
          snd
      )
  where
    quotePath path = mconcat [emitChar '\'', emit (Text.pack path), emitChar '\'']
    arg (SingleParquetFile path) = quotePath path
    arg (MultipleParquetFiles files) =
      emitChar '[' <> sepBy (emit ", ") (NonEmpty.toList $ fmap quotePath files) <> emitChar ']'

-- | Construct an 'EntityModification' to rename the fields of a 'ParquetFileEntity'
modifyParquetFileFields ::
  (Beamable tbl) =>
  tbl (FieldModification (TableField tbl)) ->
  EntityModification (DatabaseEntity DuckDB db) be (ParquetFileEntity tbl)
modifyParquetFileFields modFields =
  EntityModification
    ( Endo
        ( \(DatabaseEntity tbl) ->
            DatabaseEntity tbl {parquetTableSettings = withTableModification modFields (parquetTableSettings tbl)}
        )
    )
