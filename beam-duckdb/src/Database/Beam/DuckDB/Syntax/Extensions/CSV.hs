{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.DuckDB.Syntax.Extensions.CSV
  ( -- ** Specifying CSV files as part of the database
    csv,
    csvWith,
    modifyCSVFields,
    CSVEntity,

    -- *** CSV file options
    CSVOptions (..),
    defaultCSVOptions,

    -- ** Querying data from a CSV file
    allFromCSV_,
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
data CSVEntity (table :: (Type -> Type) -> Type)

-- | Options affecting the handling of CSV files.
--
-- See 'defaultCSVOptions' for default options.
data CSVOptions = CSVOptions
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

defaultCSVOptions :: CSVOptions
defaultCSVOptions =
  CSVOptions
    { comment = Nothing,
      delim = Nothing,
      header = Nothing,
      ignoreErrors = Nothing
    }

instance (Beamable tbl) => RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor DuckDB (CSVEntity tbl))) where
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

instance (Beamable table) => IsDatabaseEntity DuckDB (CSVEntity table) where
  data DatabaseEntityDescriptor DuckDB (CSVEntity table)
    = CSVEntityDescriptor
    { csvSource :: !(NonEmpty FilePath),
      csvName :: !Text, -- Only exists so that renaming this entity works. However, it serves no purpose
      csvFileOptions :: !CSVOptions,
      csvTableSettings :: !(TableSettings table)
    }

  type
    DatabaseEntityDefaultRequirements DuckDB (CSVEntity table) =
      ( GDefaultTableFieldSettings (Rep (TableSettings table) ()),
        Generic (TableSettings table),
        Table table,
        Beamable table
      )
  type
    DatabaseEntityRegularRequirements DuckDB (CSVEntity table) =
      (Table table, Beamable table)

  -- This 'dbEntityName' is useless. Changing it with 'modifyEntityName' does effectively nothing.
  dbEntityName f vw = fmap (\t' -> vw {csvName = t'}) (f (csvName vw))
  dbEntitySchema f vw = fmap (const vw) (f Nothing) -- Schema doesn't apply to CSV files
  dbEntityAuto nm =
    CSVEntityDescriptor
      { csvSource = NonEmpty.singleton (Text.unpack nm),
        csvName = nm,
        csvFileOptions = defaultCSVOptions,
        csvTableSettings = defTblFieldSettings
      }

-- | Declare a CSV file(s) or glob(s) as the source of data for a database.
-- Use 'modifyCSVFields' to specify column names, and finally query data
-- using 'allFromCSV_'.
csv ::
  -- | File path(s) or glob(s)
  NonEmpty FilePath ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (CSVEntity table)
csv path = csvWith path defaultCSVOptions

-- | Declare a CSV file (or files) as the source of data for a database.
-- Use 'modifyCSVFields' to specify column names, and finally query data
-- using 'allFromCSV_'.
--
-- See 'csv' if you want to use default options.
csvWith ::
  -- | File path(s) or glob(s)
  NonEmpty FilePath ->
  -- | Iceberg table options.
  CSVOptions ->
  EntityModification (DatabaseEntity DuckDB db) DuckDB (CSVEntity table)
csvWith path options = EntityModification $ Endo $ \(DatabaseEntity desc) ->
  DatabaseEntity
    desc
      { csvSource = path,
        csvFileOptions = options
      }

allFromCSV_ ::
  (Beamable table) =>
  DatabaseEntity DuckDB db (CSVEntity table) ->
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
    arg (path :| []) = quotePath path
    arg paths =
      emitChar '[' <> sepBy (emit ", ") (NonEmpty.toList $ fmap quotePath paths) <> emitChar ']'

    emitOptions options@(CSVOptions mComment mDelim mHeader mIgnoreErrors) =
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

-- | Construct an 'EntityModification' to rename the fields of a 'CSVEntity'
modifyCSVFields ::
  (Beamable tbl) =>
  tbl (FieldModification (TableField tbl)) ->
  EntityModification (DatabaseEntity DuckDB db) be (CSVEntity tbl)
modifyCSVFields modFields =
  EntityModification
    ( Endo
        ( \(DatabaseEntity tbl) ->
            DatabaseEntity tbl {csvTableSettings = withTableModification modFields (csvTableSettings tbl)}
        )
    )
