{-# LANGUAGE RankNTypes, GADTs #-}
module Database.Beam.Types where

import Database.Beam.Schema
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

-- -- * Tables

-- data PrimaryKey = PrimaryKey

-- newtype TableResult t = TableResult (V.Vector SqlValue)
--     deriving Show

-- class Table t where
--      type Key t :: *
--      type Key t = PrimaryKey

--      type KeyField t :: *
--      type KeyField t = IntField

--      table :: t
--      default table :: (Generic t, GUnit (Rep t ())) => t
--      table = to (gUnit :: Rep t ())

--      tableName :: Proxy t -> Text
--      default tableName :: Show t => Proxy t -> Text
--      tableName _ = fromString (show (table :: t))

--      fields :: [TableField t]

-- class GUnit c where
--     gUnit :: c

-- instance GUnit (p x) => GUnit (M1 D f p x) where
--     gUnit = M1 gUnit

-- instance GUnit (p x) => GUnit (M1 C f p x) where
--     gUnit = M1 gUnit

-- instance GUnit (U1 x) where
--     gUnit = U1

-- data TableField t where
--     TableField :: Field t field => field -> TableField t

-- -- * Fields

-- class (Typeable field, Table table) => Field table field where
--     type FType table field :: *

--     field :: Proxy table -> field
--     default field :: (Generic field, GUnit (Rep field ())) => Proxy table -> field
--     field _ = to (gUnit :: Rep field ())

--     fieldName :: Proxy table -> Proxy field -> Text
--     default fieldName :: Show field => Proxy table -> Proxy field -> Text
--     fieldName _ _ = fromString (show (field (Proxy :: Proxy table) :: field))

-- fieldResultIndex :: Field table field => Proxy table -> Proxy field -> Maybe Int
-- fieldResultIndex tableProxy fieldProxy = findIndex checkFieldEq (fieldsForTableProxy tableProxy)
--     where checkFieldEq :: TableField table -> Bool
--           checkFieldEq (TableField field) = typeOf field == typeOf (unProxy fieldProxy)

--           fieldsForTableProxy :: Table t => Proxy t -> [TableField t]
--           fieldsForTableProxy _ = fields

--           unProxy :: Proxy a -> a
--           unProxy _ = undefined

-- data TextField = TextField Text
-- data IntField = IntField Int

-- -- * Mappings

-- type MappingM table = Reader (TableResult table)

-- class Table (FromTable mapping) => Mapping mapping where
--     type FromTable mapping :: *

--     fromTableResults :: MappingM (FromTable mapping) mapping

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
