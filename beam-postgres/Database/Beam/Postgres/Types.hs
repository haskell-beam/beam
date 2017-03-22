{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Postgres.Types where

import Database.Beam
import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.Builder
import Database.Beam.Backend.SQL.SQL92

import Database.PostgreSQL.LibPQ
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as PgType
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Database.PostgreSQL.Simple.HStore as Pg (HStoreMap, HStoreList)
import qualified Database.PostgreSQL.Simple.Types as Pg
import qualified Database.PostgreSQL.Simple.Range as Pg (PGRange)
import qualified Database.PostgreSQL.Simple.Time as Pg (Date, UTCTimestamp, ZonedTimestamp, LocalTimestamp)

import Control.Applicative

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Data.Int
import Data.Aeson (Value)
import Data.UUID (UUID)
import Data.CaseInsensitive (CI)
import Data.Data
import Data.Ratio (Ratio)
import Data.Typeable
import Data.Maybe
import Data.String
import Data.Functor
import Data.Monoid
import Data.Scientific (Scientific)
import Data.Vector (Vector)
import Data.Time (UTCTime, Day, TimeOfDay, LocalTime, ZonedTime)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, signed, decimal)
import qualified Data.Attoparsec.ByteString.Char8 as P

data Postgres
  = Postgres
  deriving Data

instance BeamBackend Postgres where
  -- data BackendLiteral Postgres
  --   = BackendLiteral !Format !(Maybe (Oid, ByteString))
  --   deriving (Show, Eq)
  data BackendColumnSchema Postgres
    = PgColumnSchema
    { pgColumnType :: ByteString
    , pgIsNullable :: Bool
    , pgIsPrimaryKey :: Bool }
    deriving (Show)
  type BackendFromField Postgres = Pg.FromField

instance SupportedSyntax Postgres SqlSyntaxBuilder

instance Pg.FromField x => Pg.FromField (Auto x) where
  fromField field d = fmap (Auto . Just) (Pg.fromField field d)
instance Pg.ToField x => Pg.ToField (Auto x) where
  toField (Auto x) = Pg.toField x

-- Default FromBackendRow instances for all postgresql-simple FromField instances
instance FromBackendRow Postgres Bool
instance FromBackendRow Postgres Char
instance FromBackendRow Postgres Double
instance FromBackendRow Postgres Int
instance FromBackendRow Postgres Int16
instance FromBackendRow Postgres Int32
instance FromBackendRow Postgres Int64
instance FromBackendRow Postgres Integer
instance FromBackendRow Postgres ByteString
instance FromBackendRow Postgres Scientific
instance FromBackendRow Postgres BL.ByteString
instance FromBackendRow Postgres Text
instance FromBackendRow Postgres UTCTime
instance FromBackendRow Postgres Value
instance FromBackendRow Postgres TL.Text
instance FromBackendRow Postgres Pg.Oid
instance FromBackendRow Postgres LocalTime
instance FromBackendRow Postgres ZonedTime
instance FromBackendRow Postgres TimeOfDay
instance FromBackendRow Postgres Day
instance FromBackendRow Postgres UUID
instance FromBackendRow Postgres Pg.Null
instance FromBackendRow Postgres Pg.Date
instance FromBackendRow Postgres Pg.ZonedTimestamp
instance FromBackendRow Postgres Pg.UTCTimestamp
instance FromBackendRow Postgres Pg.LocalTimestamp
instance FromBackendRow Postgres Pg.HStoreMap
instance FromBackendRow Postgres Pg.HStoreList
instance FromBackendRow Postgres [Char]
instance FromBackendRow Postgres (Ratio Integer)
instance FromBackendRow Postgres (CI Text)
instance FromBackendRow Postgres (CI TL.Text)
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Vector a)
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Pg.PGArray a)
instance FromBackendRow Postgres (Pg.Binary ByteString)
instance FromBackendRow Postgres (Pg.Binary BL.ByteString)
instance (Pg.FromField a, Typeable a) => FromBackendRow Postgres (Pg.PGRange a)
instance (Pg.FromField a, Pg.FromField b) => FromBackendRow Postgres (Either a b)

-- TODO instance for Maybe *

instance Pg.ToField SqlNull where
  toField _ = Pg.toField (Nothing :: Maybe Int)

-- instance Eq (BackendLiteral Postgres) where
--   a == b =
--     maybe False (== b) (cast a)
-- instance Show (BackendLiteral Postgres) where
--   show (BackendLiteral a) =
--     "BackendLiteral (" ++ show a ++ ")"

instance BeamSqlBackend Postgres
instance BeamSql92Backend Postgres

-- tryOids :: [PgType.TypeInfo] -> Maybe a -> (ByteString -> Maybe a) -> (ByteString -> Maybe a)
--         -> BackendLiteral Postgres -> Maybe a
-- tryOids types onNull onBinary onText (BackendLiteral fmt dat) =
--   case dat of
--     Nothing -> onNull
--     Just (oid, dat_)
--       | any ((==oid) . PgType.typoid) types ->
--           case fmt of
--             Binary -> onBinary dat_
--             Text   -> onText dat_
--       | otherwise -> Nothing

-- nonNull :: Maybe a
-- nonNull = Nothing

-- pgTextValue :: PgType.TypeInfo -> ByteString -> BackendLiteral Postgres
-- pgTextValue typeInfo bs =
--   BackendLiteral Text (Just (PgType.typoid typeInfo, bs))

-- instance FromBackendLiteral Postgres String
-- instance FromBackendLiterals Postgres String
-- instance FromBackendLiteral Postgres Text
-- instance FromBackendLiterals Postgres Text

-- instance FromBackendLiteral Postgres Bool where
--   fromBackendLiteral =
--     tryOids [PgType.bool] nonNull rdBool rdBool
--     where rdBool "t" = Just True
--           rdBool "true" = Just True
--           rdBool "f" = Just False
--           rdBool "false" = Just False
--           rdBool _ = Nothing
--   toBackendLiteral True = pgTextValue PgType.bool "true"
--   toBackendLiteral False = pgTextValue PgType.bool "false"

-- rdAtto :: Parser a -> ByteString -> Maybe a
-- rdAtto parse bs =
--   case parseOnly parse bs of
--     Left _ -> Nothing
--     Right x -> Just x

-- rdInt :: Integral a => ByteString -> Maybe a
  -- rdInt = rdAtto (signed decimal)

-- instance FromBackendLiteral Postgres Int16 where
--   fromBackendLiteral =
--     tryOids [PgType.int2] nonNull rdInt rdInt
--   toBackendLiteral x =
--     pgTextValue PgType.int2 (fromString (show x))

-- instance FromBackendLiteral Postgres Int32 where
--   fromBackendLiteral =
--     tryOids [ PgType.int2, PgType.int4 ] nonNull rdInt rdInt
--   toBackendLiteral x =
--     pgTextValue PgType.int4 (fromString (show x))

-- instance FromBackendLiteral Postgres Int64 where
--   fromBackendLiteral =
--     tryOids [ PgType.int2, PgType.int4, PgType.int8 ] nonNull rdInt rdInt
--   toBackendLiteral x =
--     pgTextValue PgType.int8 (fromString (show x))

-- instance FromBackendLiteral Postgres Float where
--   fromBackendLiteral =
--     tryOids [ PgType.float4 ] nonNull rdFloat rdFloat
--     where rdFloat = rdAtto $
--                     (P.string "NaN"       $> (0 / 0)) <|>
--                     (P.string "Infinity"  $> (1 / 0)) <|>
--                     (P.string "-Infinity" $> (-1 / 0)) <|>
--                     (realToFrac <$> P.double)
--   toBackendLiteral x =
--     pgTextValue PgType.int8 (fromString (show x))

-- instance FromBackendLiteral Postgres Double where
--   fromBackendLiteral =
--     tryOids [ PgType.float4, PgType.float8 ] nonNull rdDouble rdDouble
--     where rdDouble = rdAtto $
--                      (P.string "NaN"       $> (0 / 0)) <|>
--                      (P.string "Infinity"  $> (1 / 0)) <|>
--                      (P.string "-Infinity" $> (-1 / 0)) <|>
--                      P.double
--   toBackendLiteral x =
--     pgTextValue PgType.int8 (fromString (show x))

-- instance FromBackendLiterals Postgres Bool
-- --instance FromBackendLiterals Postgres Int8
-- instance FromBackendLiterals Postgres Int16
-- instance FromBackendLiterals Postgres Int32
-- instance FromBackendLiterals Postgres Int64
-- instance FromBackendLiterals Postgres Float
-- instance FromBackendLiterals Postgres Double

-- * Postgresql type instances

instance BeamColumnSchema (BackendColumnSchema Postgres) where
  maybeFieldSchema schema = schema { pgIsNullable = True }
  nestedSchema schema = schema { pgIsPrimaryKey = False }
  autoSchema schema = schema

-- instance Sql92Schema (BackendColumnSchema Postgres) where
--   int = PgColumnSchema "INT" False False
--   smallint = PgColumnSchema "SMALLINT" False False
--   tinyint = PgColumnSchema "TINYINT" False False
--   bigint = PgColumnSchema "BIGINT" False False

--   char x = PgColumnSchema ("CHAR(" <> fromString (show x) <> ")") False False
--   varchar Nothing = PgColumnSchema "VARCHAR" False False
--   varchar (Just x) = PgColumnSchema ("VARCHAR(" <> fromString (show x) <> ")") False False

--   float = PgColumnSchema "FLOAT" False False
--   double = PgColumnSchema "DOUBLE" False False

--   timestamp = PgColumnSchema "TIMESTAMP" False False

-- instance HasDefaultFieldSchema Postgres Int16 where
--   defFieldSchema _ = smallint
-- instance HasDefaultFieldSchema Postgres Int32 where
--   defFieldSchema _ = int
-- instance HasDefaultFieldSchema Postgres Int64 where
--   defFieldSchema _ = bigint
-- instance HasDefaultFieldSchema Postgres Text where
--   defFieldSchema _ = varchar Nothing
-- instance HasDefaultFieldSchema Postgres String where
--   defFieldSchema _ = varchar Nothing
-- instance HasDefaultFieldSchema Postgres Float where
--   defFieldSchema _ = float
-- instance HasDefaultFieldSchema Postgres Double where
--   defFieldSchema _ = double
-- instance HasDefaultFieldSchema Postgres UTCTime where
--   defFieldSchema _ = timestamp
-- instance HasDefaultFieldSchema Postgres x =>
--   HasDefaultFieldSchema Postgres (Maybe x) where
--   defFieldSchema p = maybeFieldSchema (defFieldSchema p)

-- * Build commands

--newtype PgStatementBuilder
--  = PgStatementBuilder { buildPgStatement :: Builder }

--instance Sql92Syntax PgStatementBuilder

-- * Types for postgres specific extensions

newtype PgTableSamplingMethod
  = PgTableSamplingMethod { pgTableSamplingMethodAsText :: Text }
  deriving (Show, Eq, Ord)

pgBernoulliSamplingMethod, pgSystemSamplingMethod :: PgTableSamplingMethod
pgBernoulliSamplingMethod = PgTableSamplingMethod "BERNOULLI"
pgSystemSamplingMethod = PgTableSamplingMethod "SYSTEM"
