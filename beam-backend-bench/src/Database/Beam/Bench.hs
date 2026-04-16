{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.Bench (
  -- * Beam table
  ImdbNameT (..),
  ImdbName,
  ImdbDb (..),
  imdbDb,

  -- * Flat record
  StrictImdbName (..),

  -- * Configuration
  defaultRowCount,
  readRowCount,
  generateRow,

  -- * SQL strings
  createTableSql,
  insertSql,
  selectSql,
) where

import Control.DeepSeq (NFData)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam (
  Beamable,
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
import System.Environment (lookupEnv)

data ImdbNameT f = ImdbName
  { _nconst :: !(Columnar f Text)
  , _primaryName :: !(Columnar f (Maybe Text))
  , _birthYear :: !(Columnar f (Maybe Int32))
  , _deathYear :: !(Columnar f (Maybe Int32))
  , _primaryProfession :: !(Columnar f (Maybe Text))
  , _knownForTitles :: !(Columnar f (Maybe Text))
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

imdbDb :: DatabaseSettings be ImdbDb
imdbDb =
  defaultDbSettings
    `withDbModification` dbModification
      { imdbNames =
          setEntityName "imdb_names"
            <> modifyTableFields
              tableModification
                { _nconst = fieldNamed "nconst"
                , _primaryName = fieldNamed "primary_name"
                , _birthYear = fieldNamed "birth_year"
                , _deathYear = fieldNamed "death_year"
                , _primaryProfession = fieldNamed "primary_profession"
                , _knownForTitles = fieldNamed "known_for_titles"
                }
      }

data StrictImdbName = StrictImdbName
  { siNconst :: !Text
  , siPrimaryName :: !(Maybe Text)
  , siBirthYear :: !(Maybe Int32)
  , siDeathYear :: !(Maybe Int32)
  , siPrimaryProfession :: !(Maybe Text)
  , siKnownForTitles :: !(Maybe Text)
  }
  deriving (Generic, Show)

instance NFData StrictImdbName

{- | Deterministic row generator. Same input always yields the same
row, so repeated benchmark invocations operate on identical data.
-}
generateRow :: Int -> StrictImdbName
generateRow i =
  StrictImdbName
    { siNconst = T.pack ("nm" <> pad7 i)
    , siPrimaryName = Just (T.pack ("Person Name " <> show i))
    , siBirthYear = Just (fromIntegral (1900 + (i `mod` 100)))
    , siDeathYear =
        if i `mod` 3 == 0
          then Just (fromIntegral (2000 + (i `mod` 50)))
          else Nothing
    , siPrimaryProfession = Just "actor,producer,director"
    , siKnownForTitles = Just "tt0001234,tt0002345,tt0003456,tt0004567"
    }
 where
  pad7 n = let s = show n in replicate (7 - length s) '0' <> s

{- | Default number of synthetic rows when @BEAM_BENCH_ROWS@ is unset.
  10,000 is a balance between signal and benchmark wall-time.
-}
defaultRowCount :: Int
defaultRowCount = 10_000

-- | Read 'BEAM_BENCH_ROWS' if set, otherwise return 'defaultRowCount'.
readRowCount :: IO Int
readRowCount = do
  mbStr <- lookupEnv "BEAM_BENCH_ROWS"
  pure $ case mbStr of
    Just s | [(n, "")] <- reads s -> n
    _ -> defaultRowCount

createTableSql :: String
createTableSql =
  unwords
    [ "CREATE TABLE imdb_names ("
    , "  nconst              TEXT NOT NULL,"
    , "  primary_name        TEXT,"
    , "  birth_year          INTEGER,"
    , "  death_year          INTEGER,"
    , "  primary_profession  TEXT,"
    , "  known_for_titles    TEXT"
    , ")"
    ]

insertSql :: String
insertSql =
  unwords
    [ "INSERT INTO imdb_names"
    , "  (nconst, primary_name, birth_year, death_year,"
    , "   primary_profession, known_for_titles)"
    , "VALUES (?, ?, ?, ?, ?, ?)"
    ]

selectSql :: String
selectSql =
  unwords
    [ "SELECT nconst, primary_name, birth_year, death_year,"
    , "       primary_profession, known_for_titles"
    , "FROM imdb_names"
    ]
