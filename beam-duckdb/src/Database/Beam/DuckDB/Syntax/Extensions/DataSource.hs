{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.DuckDB.Syntax.Extensions.DataSource
  ( -- ** Specifying data sources as part of the database
    DataSourceEntity,
    DataSource,
    dataSource,
    modifyDataSourceFields,

    -- *** Data source configurations
    csv,
    csvWith,
    CSVOptions (..),
    defaultCSVOptions,
    icebergTable,
    icebergTableWith,
    IcebergTableOptions (..),
    defaultIcebergTableOptions,
    parquet,

    -- ** Querying data from a CSV file
    allFromDataSource_,
  )
where

import Control.Monad.Free (liftF)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Data.Monoid (Endo (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Beam (Beamable, DatabaseEntity, EntityModification, FieldModification, QExpr, Table, TableField, TableSettings, defTblFieldSettings, withTableModification)
import Database.Beam.DuckDB.Backend (DuckDB)
import Database.Beam.DuckDB.Syntax (DuckDBFromSyntax (..))
import Database.Beam.DuckDB.Syntax.Builder (DuckDBSyntax, emit, emit', emitChar, quotedIdentifier, sepBy)
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

-- | A phantom type tag for entities which are external sources of data for DuckDB
-- databases. This data can come from files (e.g. Parquet), other table formats
-- (e.g. Apache Iceberg), or even other databases.
data DataSourceEntity (table :: (Type -> Type) -> Type)

-- | Opaque type representing the different data sources for a 'DataSourceEntity'.
--
-- The only way to create a 'DataSource' is to use one of the helper functions
-- such as 'parquet', 'csv', 'icebergTable', and their variants.
data DataSource
  = -- The constructors are not exposed so that we can extend the list
    -- of supported data sources without breakage.
    UndefinedDataSource FilePath -- Only used by dbEntityAuto. Assume this is a text file.
  | Parquet (NonEmpty FilePath) -- no Parquet options supported yet
  | CSV (NonEmpty FilePath) CSVOptions
  | ApacheIceberg FilePath IcebergTableOptions

dataSource ::
  DataSource ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (DataSourceEntity table)
dataSource src =
  EntityModification $ Endo $ \(DatabaseEntity desc) ->
    DatabaseEntity
      desc
        { source = src
        }

-- | Declare a CSV file(s) or glob(s) as the source of data for a database.
--
-- Note: Be careful to sanitize any user input that is then used as
-- a data source.
csv ::
  -- | File path(s) or glob(s)
  NonEmpty FilePath ->
  DataSource
csv = flip csvWith defaultCSVOptions

-- | Declare a CSV file (or files) as the source of data for a database.
--
-- See 'csv' if you want to use default options.
-- 
-- Note: Be careful to sanitize any user input that is then used as
-- a data source.
csvWith ::
  -- | File path(s) or glob(s)
  NonEmpty FilePath ->
  -- | CSV options.
  CSVOptions ->
  DataSource
csvWith = CSV

-- | Options affecting the handling of CSV files.
--
-- See 'defaultCSVOptions' for default options.
data CSVOptions = CSVOptions
  { -- | Character used to initiate comments. Lines starting with a comment character
    --  (optionally preceded by space characters) are completely ignored; other
    --  lines containing a comment character are parsed only up to that point.
    --  Default: empty.
    comment :: Maybe Char,
    -- | Delimiter character used to separate columns within each line, e.g., , ; \t.
    --  The delimiter character can be up to 4 bytes, e.g., 🦆. Default: @","@.
    delim :: Maybe Text,
    -- | First line of each file contains the column names. Default: false.
    header :: Maybe Bool,
    -- | Ignore any parsing errors encountered. Default: false.
    ignoreErrors :: Maybe Bool
  }
  deriving (Eq, Show)

defaultCSVOptions :: CSVOptions
defaultCSVOptions =
  CSVOptions
    { comment = Nothing,
      delim = Nothing,
      header = Nothing,
      ignoreErrors = Nothing
    }

-- | Define an Apache Iceberg table with default table options.
--
-- See 'icebergTableWith' if you want to change the default options.
-- 
-- Note: Be careful to sanitize any user input that is then used as
-- a data source.
icebergTable ::
  -- | File path
  FilePath ->
  DataSource
icebergTable = flip icebergTableWith defaultIcebergTableOptions

-- | Define an Apache Iceberg table with options.
--
-- See 'icebergTable' if you want to use default options.
-- 
-- Note: Be careful to sanitize any user input that is then used as
-- a data source.
icebergTableWith ::
  -- | File path
  FilePath ->
  -- | Iceberg table options.
  IcebergTableOptions ->
  DataSource
icebergTableWith = ApacheIceberg

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

-- | Declare a Parquet file(s) or glob(s) as the source of data for a database.
-- 
-- Note: Be careful to sanitize any user input that is then used as
-- a data source.
parquet ::
  -- | File paths or glob
  NonEmpty FilePath ->
  DataSource
parquet = Parquet

instance
  (Beamable tbl) =>
  RenamableWithRule
    ( FieldRenamer
        (DatabaseEntityDescriptor DuckDB (DataSourceEntity tbl))
    )
  where
  renamingFields renamer =
    FieldRenamer $ \tbl ->
      tbl
        { tableSettings =
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
              $ tableSettings tbl
        }

instance (Beamable table) => IsDatabaseEntity DuckDB (DataSourceEntity table) where
  data DatabaseEntityDescriptor DuckDB (DataSourceEntity table)
    = DataSourceEntityDescriptor
    { source :: !DataSource,
      name :: !Text, -- Only exists so that renaming this entity works. However, it serves no purpose
      tableSettings :: !(TableSettings table)
    }

  type
    DatabaseEntityDefaultRequirements DuckDB (DataSourceEntity table) =
      ( GDefaultTableFieldSettings (Rep (TableSettings table) ()),
        Generic (TableSettings table),
        Table table,
        Beamable table
      )
  type
    DatabaseEntityRegularRequirements DuckDB (DataSourceEntity table) =
      (Table table, Beamable table)

  -- \| This 'dbEntityName' is useless. Changing it with 'modifyEntityName' does effectively nothing.
  dbEntityName f vw = fmap (\t' -> vw {name = t'}) (f (name vw))

  -- \| Schema doesn't apply to data sources
  dbEntitySchema f vw = fmap (const vw) (f Nothing)

  -- \| By default, create a data sources as if the provided name is the path
  -- to a text file. You should point the data source to the right
  -- location by using 'parquet', 'csv', 'icebergTable', or any other
  -- relevant function.
  dbEntityAuto nm =
    DataSourceEntityDescriptor
      { source = UndefinedDataSource (Text.unpack nm),
        name = nm,
        tableSettings = defTblFieldSettings
      }

-- | This is the equivalent of 'all_', but for pulling data
-- from a 'DataSourceEntity'.
allFromDataSource_ ::
  (Beamable table) =>
  DatabaseEntity DuckDB db (DataSourceEntity table) ->
  Q DuckDB db s (table (QExpr DuckDB s))
allFromDataSource_ (DatabaseEntity desc) =
  Q $
    liftF
      ( QAll
          ( \_ nm ->
              DuckDBFromSyntax $
                emitDataSource (source desc)
                  <> emit " AS "
                  <> quotedIdentifier nm
          )
          (tableFieldsToExpressions (tableSettings desc))
          (const Nothing)
          snd
      )

quotePath :: FilePath -> DuckDBSyntax
quotePath path = mconcat [emitChar '\'', emit (Text.pack path), emitChar '\'']

-- Render a list of filepaths
--
-- >>> emitPaths (NonEmpty.singleton "hello/world.txt")
-- DuckDBSyntax ("'hello/world.txt'") fromList []
-- >>> emitPaths ("hello/world.txt" :| ["foo/bar.csv"])
-- DuckDBSyntax ("['hello/world.txt', 'foo/bar.csv']") fromList []
emitPaths :: NonEmpty FilePath -> DuckDBSyntax
emitPaths (path :| []) = quotePath path
emitPaths paths =
  emitChar '[' <> sepBy (emit ", ") (NonEmpty.toList $ fmap quotePath paths) <> emitChar ']'

emitDataSource :: DataSource -> DuckDBSyntax
emitDataSource (UndefinedDataSource path) =
  emit "read_text("
    <> quotePath path
    <> emit ")"
emitDataSource (Parquet paths) =
  emit "read_parquet("
    <> emitPaths paths
    <> emitChar ')'
emitDataSource (CSV paths options) =
  emit "read_csv("
    <> emitPaths paths
    <> emitCSVOptions options
    <> emitChar ')'
emitDataSource (ApacheIceberg path options) =
  emit "iceberg_scan("
    <> emitPaths (NonEmpty.singleton path)
    <> emitIcebergOptions options
    <> emitChar ')'

emitCSVOptions :: CSVOptions -> DuckDBSyntax
emitCSVOptions options@(CSVOptions mComment mDelim mHeader mIgnoreErrors) =
  if options == defaultCSVOptions
    then mempty
    else
      emit ", "
        <> sepBy
          (emit ", ")
          ( catMaybes
              [ fmap (\x -> emit "comment='" <> emitChar x <> emitChar '\'') mComment,
                fmap (\x -> emit "delim='" <> emit x <> emitChar '\'') mDelim,
                fmap (\x -> emit "header=" <> emit' x) mHeader,
                fmap (\x -> emit "ignore_errors=" <> emit' x) mIgnoreErrors
              ]
          )

emitIcebergOptions :: IcebergTableOptions -> DuckDBSyntax
emitIcebergOptions options@(IcebergTableOptions mAllowMovedPaths mVersion) =
  if options == defaultIcebergTableOptions
    then mempty
    else
      emit ", "
        <> sepBy
          (emit ", ")
          ( catMaybes
              [ fmap (\x -> emit "allow_moved_paths=" <> emit' x) mAllowMovedPaths,
                fmap (\x -> emit "version='" <> emit x <> emitChar '\'') mVersion
              ]
          )

-- | Construct an 'EntityModification' to rename the fields of a 'DataSourceEntity'
modifyDataSourceFields ::
  (Beamable tbl) =>
  tbl (FieldModification (TableField tbl)) ->
  EntityModification (DatabaseEntity DuckDB db) be (DataSourceEntity tbl)
modifyDataSourceFields modFields =
  EntityModification
    ( Endo
        ( \(DatabaseEntity tbl) ->
            DatabaseEntity tbl {tableSettings = withTableModification modFields (tableSettings tbl)}
        )
    )
