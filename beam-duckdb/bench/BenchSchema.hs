{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shared schema used by the @beam-duckdb@ row-reader benchmark.
--
-- A synthetic IMDb-style names table is defined two ways: once as a
-- @beam@ table ('ImdbNameT') and once as a flat record
-- ('StrictImdbName') whose 'FromRow' / 'ToRow' instances are written
-- by hand. This lets us compare the cost of @beam-duckdb@ row
-- reading against the @duckdb-simple@ baseline on the same data.
module BenchSchema
  ( ImdbNameT (..),
    ImdbName,
    ImdbDb (..),
    imdbDb,
    StrictImdbName (..),
    generateRow,
  )
where

import Control.DeepSeq (NFData)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
  ( Beamable,
    Columnar,
    Database,
    DatabaseSettings,
    Generic,
    Identity,
    Table (..),
    TableEntity,
    dbModification,
    defaultDbSettings,
    fieldNamed,
    modifyTableFields,
    setEntityName,
    tableModification,
    withDbModification,
  )
import Database.DuckDB.Simple.FromRow (FromRow (..), field)
import Database.DuckDB.Simple.ToField (toField)
import Database.DuckDB.Simple.ToRow (ToRow (..))

data ImdbNameT f = ImdbName
  { _nconst :: !(Columnar f Text),
    _primaryName :: !(Columnar f (Maybe Text)),
    _birthYear :: !(Columnar f (Maybe Int32)),
    _deathYear :: !(Columnar f (Maybe Int32)),
    _primaryProfession :: !(Columnar f (Maybe Text)),
    _knownForTitles :: !(Columnar f (Maybe Text))
  }
  deriving (Generic, Beamable)

type ImdbName = ImdbNameT Identity

instance Table ImdbNameT where
  data PrimaryKey ImdbNameT f = ImdbNameId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = ImdbNameId . _nconst

deriving instance Show ImdbName

deriving instance NFData ImdbName

newtype ImdbDb f = ImdbDb
  { imdbNames :: f (TableEntity ImdbNameT)
  }
  deriving (Generic)

instance Database be ImdbDb

-- | Beam database settings. Field names are mapped to lowercase column
-- names so the schema matches the one created by the benchmark setup.
imdbDb :: DatabaseSettings be ImdbDb
imdbDb =
  defaultDbSettings
    `withDbModification` dbModification
      { imdbNames =
          setEntityName "imdb_names"
            <> modifyTableFields
              tableModification
                { _nconst = fieldNamed "nconst",
                  _primaryName = fieldNamed "primary_name",
                  _birthYear = fieldNamed "birth_year",
                  _deathYear = fieldNamed "death_year",
                  _primaryProfession = fieldNamed "primary_profession",
                  _knownForTitles = fieldNamed "known_for_titles"
                }
      }

data StrictImdbName = StrictImdbName
  { siNconst :: !Text,
    siPrimaryName :: !(Maybe Text),
    siBirthYear :: !(Maybe Int32),
    siDeathYear :: !(Maybe Int32),
    siPrimaryProfession :: !(Maybe Text),
    siKnownForTitles :: !(Maybe Text)
  }
  deriving (Generic, Show)

instance NFData StrictImdbName

instance FromRow StrictImdbName where
  fromRow =
    StrictImdbName
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToRow StrictImdbName where
  toRow x =
    [ toField (siNconst x),
      toField (siPrimaryName x),
      toField (siBirthYear x),
      toField (siDeathYear x),
      toField (siPrimaryProfession x),
      toField (siKnownForTitles x)
    ]

-- | Deterministic row generator. Same input always yields the same
-- row, so repeated benchmark invocations operate on identical data.
generateRow :: Int -> StrictImdbName
generateRow i =
  StrictImdbName
    { siNconst = T.pack ("nm" <> pad7 i),
      siPrimaryName = Just (T.pack ("Person Name " <> show i)),
      siBirthYear = Just (fromIntegral (1900 + (i `mod` 100))),
      siDeathYear =
        if i `mod` 3 == 0
          then Just (fromIntegral (2000 + (i `mod` 50)))
          else Nothing,
      siPrimaryProfession = Just "actor,producer,director",
      siKnownForTitles = Just "tt0001234,tt0002345,tt0003456,tt0004567"
    }
  where
    pad7 n = let s = show n in replicate (7 - length s) '0' <> s
