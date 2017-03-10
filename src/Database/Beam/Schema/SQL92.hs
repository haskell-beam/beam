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

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

-- * Fields

enumSchema :: forall be a.
  (BeamSql92Backend be, Enum a, Show a, Read a, Typeable a) => FieldSchema be a
enumSchema = let schema :: (Enum a, Typeable a) => FieldSchema be a
                 schema = int { fsHumanReadable = "enumFields" }
             in schema

makeEnumValue :: (Enum a, BeamBackend be, FromBackendLiteral be Int) => a -> BackendLiteral be
makeEnumValue = toBackendLiteral . fromEnum
fromEnumValue :: (Enum a, BeamBackend be, FromBackendLiteral be Int) => BackendLiteral be -> Maybe a
fromEnumValue = fmap toEnum . fromBackendLiteral

-- ** Int Field

int :: BeamSql92Backend be => FieldSchema be Int
int = FieldSchema
      { fsColDesc = columnSchemaWithType "INT"
      , fsHumanReadable = "int" }

-- ** Text field

char :: BeamSql92Backend be => Word -> FieldSchema be Text
char width = FieldSchema
             { fsColDesc = columnSchemaWithType ("CHAR(" <> fromString (show width) <> ")")
             , fsHumanReadable = "char " ++ show width }

varchar :: BeamSql92Backend be => Maybe Word -> FieldSchema be Text
varchar width = FieldSchema
                { fsColDesc = columnSchemaWithType (maybe "VARCHAR" (\width -> "VARCHAR(" <> fromString (show width) <> ")") width)
                , fsHumanReadable = "varchar " ++ show width }

defaultTextSchema :: BeamSql92Backend be => FieldSchema be Text
defaultTextSchema = varchar Nothing

dateTimeSchema :: BeamSql92Backend be => FieldSchema be UTCTime
dateTimeSchema = FieldSchema
                 { fsColDesc = columnSchemaWithType "DATETIME"
                 , fsHumanReadable = "dateTimeField"}

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
