{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Schema.SQL92 where

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL92

import Database.Beam.Schema.Tables
import Database.Beam.SQL.Types

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Error

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Text (Text, unpack)
import Data.Proxy
import Data.String
import Data.Typeable
import Data.Word
import Data.Monoid
import Data.Int

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

-- * Fields

enumSchema :: forall be a.
  (BeamSql92Backend be, Enum a, Show a, Read a, Typeable a) => FieldSchema be a
enumSchema = let schema :: (Enum a, Typeable a) => FieldSchema be a
                 schema = int { fsHumanReadable = "enumFields" }
             in schema

makeEnumValue :: (Enum a, BeamBackend be, FromBackendLiteral be Int32) => a -> BackendLiteral be
makeEnumValue = toBackendLiteral . (fromIntegral :: Int -> Int32) . fromEnum
fromEnumValue :: (Enum a, BeamBackend be, FromBackendLiteral be Int32) => BackendLiteral be -> Maybe a
fromEnumValue = fmap (toEnum . (fromIntegral :: Int32 -> Int)) . fromBackendLiteral

-- ** Int Field

int :: BeamSql92Backend be => FieldSchema be Int
int = FieldSchema
    { fsColDesc = columnSchemaWithType "INT"
    , fsHumanReadable = "int" }

int8 :: BeamSql92Backend be => FieldSchema be Int8
int8 = FieldSchema
     { fsColDesc = columnSchemaWithType "TINYINT"
     , fsHumanReadable = "int8" }

int16 :: BeamSql92Backend be => FieldSchema be Int16
int16 = FieldSchema
      { fsColDesc = columnSchemaWithType "SMALLINT"
      , fsHumanReadable = "int16" }

int32 :: BeamSql92Backend be => FieldSchema be Int32
int32 = FieldSchema
      { fsColDesc = columnSchemaWithType "INT"
      , fsHumanReadable = "int32" }

int64 :: BeamSql92Backend be => FieldSchema be Int64
int64 = FieldSchema
      { fsColDesc = columnSchemaWithType "BIGINT"
      , fsHumanReadable = "int64" }

-- ** Text field

char :: (BeamSql92Backend be, IsString s) => Word -> FieldSchema be s
char width = FieldSchema
             { fsColDesc = columnSchemaWithType ("CHAR(" <> fromString (show width) <> ")")
             , fsHumanReadable = "char " ++ show width }

varchar :: (BeamSql92Backend be, IsString s) => Maybe Word -> FieldSchema be s
varchar width = FieldSchema
                { fsColDesc = columnSchemaWithType (maybe "VARCHAR" (\width -> "VARCHAR(" <> fromString (show width) <> ")") width)
                , fsHumanReadable = "varchar " ++ show width }

dateTime :: BeamSql92Backend be => FieldSchema be UTCTime
dateTime = FieldSchema
         { fsColDesc = columnSchemaWithType "DATETIME"
         , fsHumanReadable = "dateTimeField"}

float :: BeamSql92Backend be => FieldSchema be Float
float = FieldSchema
      { fsColDesc = columnSchemaWithType "FLOAT"
      , fsHumanReadable = "float" }

double :: BeamSql92Backend be => FieldSchema be Double
double = FieldSchema
       { fsColDesc = columnSchemaWithType "DOUBLE"
       , fsHumanReadable = "double" }

-- -- ** Auto-increment fields

-- data AutoId = UnassignedId
--             | AssignedId !Int
--               deriving (Show, Read, Eq, Ord, Generic)

-- autoIdSchema :: FieldSchema AutoId
-- autoIdSchema = FieldSchema
--                { fsColDesc = SQLColumnSchema desc [SQLNotNull, SQLAutoIncrement]
--                , fsHumanReadable = "autoIdSchema" }
--     where desc = SqlColDesc
--                  { colType = SqlNumericT
--                  , colSize = Nothing
--                  , colOctetLength = Nothing
--                  , colDecDigits = Nothing
--                  , colNullable = Nothing }

-- instance BeamBackend be => FromBackendLiterals be AutoId where
--     makeSqlValues' UnassignedId = [ sqlNull ]
--     makeSqlValues' (AssignedId i) = [ sqlInteger (fromIntegral i) ]
--     fromSqlValues' = do val <- fromBackendValue <$> popSqlValue
--                         case val of
--                           Just BeamNull -> pure UnassignedId
--                           Just (BeamInteger i) -> pure (AssignedId (fromIntegral i))
--                           _ -> throwError "fromSqlValues' (AutoId): expected BeamInteger"
