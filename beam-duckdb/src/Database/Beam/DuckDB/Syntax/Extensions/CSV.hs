{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.DuckDB.Syntax.Extensions.CSV
  ( -- ** Specifying CSV files as part of the database
    csvFile,
    csvFileWith,
    modifyCSVFileFields,
    CSVFileEntity,

    -- *** CSV file options
    CSVFileOptions (..),
    defaultCSVFileOptions,

    -- ** Specifying the source of CSV-encoded data
    CSVSource,
    singleCSVFile,
    multipleCSVFiles,

    -- ** Querying data from a CSV file
    allFromCSV_,
  )
where

import Control.Monad.Free (liftF)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
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

-- | A phantom type tag for CSV file entities, analogous to 'TableEntity'
-- and 'ViewEntity'.
data CSVFileEntity (table :: (Type -> Type) -> Type)

-- | Options affecting the handling of CSV files.
--
-- See 'defaultCSVFileOptions' for default options.
data CSVFileOptions = CSVFileOptions
  { -- | Character used to initiate comments. Lines starting with a comment character
    -- (optionally preceded by space characters) are completely ignored; other
    -- lines containing a comment character are parsed only up to that point.
    -- Default: empty.
    comment :: Maybe Char,
    -- | Delimiter character used to separate columns within each line, e.g., , ; \t.
    -- The delimiter character can be up to 4 bytes, e.g., 🦆. Default: @","@.
    delim :: Maybe Text,
    -- | First line of each file contains the column names. Default: false.
    header :: Maybe Bool,
    -- | Ignore any parsing errors encountered. Default: false.
    ignoreErrors :: Maybe Bool
  }
  deriving (Eq, Show)

defaultCSVFileOptions :: CSVFileOptions
defaultCSVFileOptions =
  CSVFileOptions
    { comment = Nothing,
      delim = Nothing,
      header = Nothing,
      ignoreErrors = Nothing
    }

-- | All of the different ways CSV-encoded data can be read
-- by DuckDB.
--
-- Note that this type's constructors aren't exported; use functions such as `singleCSVFile`
-- or `multipleCSVFiles` to specify this
data CSVSource
  = SingleCSVFile FilePath
  | MultipleCSVFiles (NonEmpty FilePath)

-- | CSV data stored in a single file. Alternatively,
-- the filepath can represent a glob pattern.
singleCSVFile :: FilePath -> CSVSource
singleCSVFile = SingleCSVFile

-- | Multiple CSV files. These files will be treated as a single source;
-- they must have the same schema.
multipleCSVFiles :: NonEmpty FilePath -> CSVSource
multipleCSVFiles = MultipleCSVFiles

instance (Beamable tbl) => RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor DuckDB (CSVFileEntity tbl))) where
  renamingFields renamer =
    FieldRenamer $ \tbl ->
      tbl
        { csvTableSettings =
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
              $ csvTableSettings tbl
        }

instance (Beamable table) => IsDatabaseEntity DuckDB (CSVFileEntity table) where
  data DatabaseEntityDescriptor DuckDB (CSVFileEntity table)
    = ParquetFileEntityDescriptor
    { csvSource :: !CSVSource,
      csvName :: !Text, -- Only exists so that renaming this entity works. However, it serves no purpose
      csvFileOptions :: !CSVFileOptions,
      csvTableSettings :: !(TableSettings table)
    }

  type
    DatabaseEntityDefaultRequirements DuckDB (CSVFileEntity table) =
      ( GDefaultTableFieldSettings (Rep (TableSettings table) ()),
        Generic (TableSettings table),
        Table table,
        Beamable table
      )
  type
    DatabaseEntityRegularRequirements DuckDB (CSVFileEntity table) =
      (Table table, Beamable table)

  -- This 'dbEntityName' is useless. Changing it with 'modifyEntityName' does effectively nothing.
  dbEntityName f vw = fmap (\t' -> vw {csvName = t'}) (f (csvName vw))
  dbEntitySchema f vw = fmap (const vw) (f Nothing) -- Schema doesn't apply to CSV files
  dbEntityAuto nm =
    ParquetFileEntityDescriptor
      { csvSource = singleCSVFile (Text.unpack nm),
        csvName = nm,
        csvFileOptions = defaultCSVFileOptions,
        csvTableSettings = defTblFieldSettings
      }

-- | Declare a CSV file (or files) as the source of data for a database.
-- Use 'modifyCSVFileFields' to specify column names, and finally query data
-- using 'allFromCSV_'.
csvFile ::
  -- | File path or glob
  CSVSource ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (CSVFileEntity table)
csvFile path = csvFileWith path defaultCSVFileOptions

-- | Declare a CSV file (or files) as the source of data for a database.
-- Use 'modifyCSVFileFields' to specify column names, and finally query data
-- using 'allFromCSV_'.
--
-- See 'csvFile' if you want to use default options.
csvFileWith ::
  -- | File path
  CSVSource ->
  -- | Iceberg table options.
  CSVFileOptions ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (CSVFileEntity table)
csvFileWith path options = EntityModification $ Endo $ \(DatabaseEntity desc) ->
  DatabaseEntity
    desc
      { csvSource = path,
        csvFileOptions = options
      }

allFromCSV_ ::
  (Beamable table) =>
  DatabaseEntity DuckDB db (CSVFileEntity table) ->
  Q DuckDB db s (table (QExpr DuckDB s))
allFromCSV_ (DatabaseEntity desc) =
  Q $
    liftF
      ( QAll
          ( \_ name ->
              DuckDBFromSyntax $
                emit "read_csv("
                  <> arg (csvSource desc)
                  <> emitOptions (csvFileOptions desc)
                  <> emit ") AS "
                  <> quotedIdentifier name
          )
          (tableFieldsToExpressions (csvTableSettings desc))
          (const Nothing)
          snd
      )
  where
    quotePath path = mconcat [emitChar '\'', emit (Text.pack path), emitChar '\'']
    arg (SingleCSVFile path) = quotePath path
    arg (MultipleCSVFiles files) =
      emitChar '[' <> sepBy (emit ", ") (NonEmpty.toList $ fmap quotePath files) <> emitChar ']'

    emitOptions options@(CSVFileOptions mComment mDelim mHeader mIgnoreErrors) =
      if options == defaultCSVFileOptions
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

-- | Construct an 'EntityModification' to rename the fields of a 'CSVFileEntity'
modifyCSVFileFields ::
  (Beamable tbl) =>
  tbl (FieldModification (TableField tbl)) ->
  EntityModification (DatabaseEntity DuckDB db) be (CSVFileEntity tbl)
modifyCSVFileFields modFields =
  EntityModification
    ( Endo
        ( \(DatabaseEntity tbl) ->
            DatabaseEntity tbl {csvTableSettings = withTableModification modFields (csvTableSettings tbl)}
        )
    )
