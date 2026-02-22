{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.DuckDB.Syntax.Extensions.Parquet
  ( -- ** Specifying Parquet files as part of the database
    parquet,
    modifyParquetFields,
    ParquetEntity,

    -- ** Querying data from a Parquet file
    allFromParquet_,
  )
where

import Control.Monad.Free (liftF)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
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

-- | A phantom type tag for parquet file entities, analogous to 'TableEntity'
-- and 'ViewEntity'.
data ParquetEntity (table :: (Type -> Type) -> Type)

instance (Beamable tbl) => RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor DuckDB (ParquetEntity tbl))) where
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

instance (Beamable table) => IsDatabaseEntity DuckDB (ParquetEntity table) where
  data DatabaseEntityDescriptor DuckDB (ParquetEntity table)
    = ParquetEntityDescriptor
    { parquetSource :: !(NonEmpty FilePath),
      parquetName :: !Text, -- Only exists so that renaming this entity works. However, it serves no purpose
      parquetTableSettings :: !(TableSettings table)
    }

  type
    DatabaseEntityDefaultRequirements DuckDB (ParquetEntity table) =
      ( GDefaultTableFieldSettings (Rep (TableSettings table) ()),
        Generic (TableSettings table),
        Table table,
        Beamable table
      )
  type
    DatabaseEntityRegularRequirements DuckDB (ParquetEntity table) =
      (Table table, Beamable table)

  -- This 'dbEntityName' is useless. Changing it with 'modifyEntityName' does effectively nothing.
  dbEntityName f vw = fmap (\t' -> vw {parquetName = t'}) (f (parquetName vw))
  dbEntitySchema f vw = fmap (const vw) (f Nothing) -- Schema doesn't apply to Parquet files
  dbEntityAuto nm =
    ParquetEntityDescriptor
      { parquetSource = NonEmpty.singleton (Text.unpack nm),
        parquetName = nm,
        parquetTableSettings = defTblFieldSettings
      }

-- | Declare a Parquet file(s) or glob(s) as the source of data for a database.
-- Use 'modifyParquetFields' to specify column names, and finally query data
-- using 'allFromParquet_'.
parquet ::
  -- | File paths or glob
  NonEmpty FilePath ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (ParquetEntity table)
parquet path = EntityModification $ Endo $ \(DatabaseEntity desc) ->
  DatabaseEntity
    desc
      { parquetSource = path
      }

allFromParquet_ ::
  (Beamable table) =>
  DatabaseEntity DuckDB db (ParquetEntity table) ->
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
    arg (path :| []) = quotePath path
    arg paths =
      emitChar '[' <> sepBy (emit ", ") (NonEmpty.toList $ fmap quotePath paths) <> emitChar ']'

-- | Construct an 'EntityModification' to rename the fields of a 'ParquetEntity'
modifyParquetFields ::
  (Beamable tbl) =>
  tbl (FieldModification (TableField tbl)) ->
  EntityModification (DatabaseEntity DuckDB db) be (ParquetEntity tbl)
modifyParquetFields modFields =
  EntityModification
    ( Endo
        ( \(DatabaseEntity tbl) ->
            DatabaseEntity tbl {parquetTableSettings = withTableModification modFields (parquetTableSettings tbl)}
        )
    )
