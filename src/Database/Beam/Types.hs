{-# LANGUAGE RankNTypes, GADTs #-}
module Database.Beam.Types where

import Database.Beam.Schema.Types
import Database.Beam.SQL.Types

import Control.Monad.Reader

import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Proxy
import Data.String
import Data.Typeable
import Data.List

import GHC.Generics

import Database.HDBC

data DBSchemaComparison = Migration [MigrationAction]
                        | Unknown
                          deriving Show

data MigrationAction where
    CreateTable :: Table table => Proxy table -> MigrationAction

instance Show MigrationAction where
    show (CreateTable t) = concat ["CreateTable ", unpack (dbTableName t)]

data Beam m = Beam
            { closeBeam :: m ()

            , compareSchemas :: forall d. Database d => DatabaseSchema -> Proxy d -> DBSchemaComparison
            , adjustColDescForBackend :: SQLColumnSchema -> SQLColumnSchema

            , withHDBCConnection :: forall a. (forall conn. IConnection conn => conn -> m a) -> m a }

class BeamBackend dbSettings where
    openBeam :: MonadIO m => dbSettings -> m (Beam m)
