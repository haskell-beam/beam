module Database.Beam.Schema.Fields where

import Database.Beam.Schema.Tables
import Database.Beam.SQL.Types
import Database.Beam.Internal

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

import Database.HDBC ( SqlColDesc(..), SqlTypeId(..), SqlValue(..) )

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

-- * Fields

enumSchema :: (Enum a, Show a, Read a, Typeable a) => FieldSchema a
enumSchema = let schema :: (Enum a, Typeable a) => FieldSchema a
                 schema = FieldSchema
                          { fsColDesc = fsColDesc intSchema
                          , fsHumanReadable = "enumField" }
             in schema

makeEnumValue :: (Enum a, BeamBackend be) => a -> [BeamBackendValue be]
makeEnumValue x = [ sqlInteger . fromIntegral . fromEnum $ x ]
fromEnumValue :: (Enum a, BeamBackend be) => FromSqlValuesM be a
fromEnumValue = do val <- fromBackendValue <$> popSqlValue
                   case val of
                     Just (BeamInteger i) -> pure (toEnum (fromIntegral i))
                     _ -> throwError "fromEnumValue: expected BeamInteger"

-- ** Int Field

intSchema :: FieldSchema Int
intSchema = FieldSchema
          { fsColDesc = notNull
                        SqlColDesc
                        { colType = SqlNumericT
                        , colSize = Nothing
                        , colOctetLength = Nothing
                        , colDecDigits = Nothing
                        , colNullable = Nothing }
          , fsHumanReadable = "intSchema" }

instance BeamBackend be => FromSqlValues be Int where
    makeSqlValues' x = [ sqlInteger . fromIntegral $ x ]
    fromSqlValues' = do val <- fromBackendValue <$> popSqlValue
                        case val of
                          Just (BeamInteger i) -> pure (fromIntegral i)
                          _ -> throwError "fromSqlValues' (Int): expected BeamInteger"

-- ** Text field

data CharOrVarchar = Char (Maybe Int)
                   | Varchar (Maybe Int)
                     deriving Show

textSchema :: CharOrVarchar -> FieldSchema Text
textSchema charOrVarchar = FieldSchema
                           { fsColDesc = colDesc
                           , fsHumanReadable = "textSchema (" ++ show charOrVarchar ++ ")" }
    where colDesc = case charOrVarchar of
                      Char n -> notNull $
                                SqlColDesc
                                { colType = SqlCharT
                                , colSize = n
                                , colOctetLength = Nothing
                                , colDecDigits = Nothing
                                , colNullable = Nothing }
                      Varchar n -> notNull $
                                   SqlColDesc
                                   { colType = SqlVarCharT
                                   , colSize = n
                                   , colOctetLength = Nothing
                                   , colDecDigits = Nothing
                                   , colNullable = Nothing }
defaultTextSchema :: FieldSchema Text
defaultTextSchema = textSchema (Varchar Nothing)

instance BeamBackend be => FromSqlValues be Text where
    makeSqlValues' x = [ sqlString . unpack $ x ]
    fromSqlValues' = do val <- fromBackendValue <$> popSqlValue
                        case val of
                          Just (BeamString s) -> pure (fromString s)
                          Just (BeamInteger i) -> pure (fromString (show i))
                          _ -> throwError ("fromSqlValues' (Text): expected BeamString" ++ ". Got " ++ show val)

dateTimeSchema :: FieldSchema UTCTime
dateTimeSchema = FieldSchema
                 { fsColDesc = notNull $
                               SqlColDesc
                               { colType = SqlUTCDateTimeT
                               , colSize = Nothing
                               , colOctetLength = Nothing
                               , colDecDigits = Nothing
                              , colNullable = Nothing }
                 , fsHumanReadable = "dateTimeField"}

instance BeamBackend be => FromSqlValues be UTCTime where
    makeSqlValues' x = [ sqlUTCTime x ]
    fromSqlValues' = do val <- fromBackendValue <$> popSqlValue
                        case val of
                          Just (BeamUTCTime t) -> pure t
                          Just (BeamInteger i) -> pure (posixSecondsToUTCTime (fromInteger i))
                          Just (BeamString s)
                               | Just t <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" s -> pure t
                               | Just t <- parseTimeM True defaultTimeLocale (dateTimeFmt defaultTimeLocale) s -> pure t
                               | Just t <- parseTimeM True defaultTimeLocale rfc822DateFormat s -> pure t
                               | Just t <- parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) s -> pure t
                          _ -> throwError ("fromSqlValues' (UTCTime): expected BeamUTCTime. Got " ++ show val)

-- ** Auto-increment fields

data AutoId = UnassignedId
            | AssignedId !Int
              deriving (Show, Read, Eq, Ord, Generic)

autoIdSchema :: FieldSchema AutoId
autoIdSchema = FieldSchema
               { fsColDesc = SQLColumnSchema desc [SQLNotNull, SQLAutoIncrement]
               , fsHumanReadable = "autoIdSchema" }
    where desc = SqlColDesc
                 { colType = SqlNumericT
                 , colSize = Nothing
                 , colOctetLength = Nothing
                 , colDecDigits = Nothing
                 , colNullable = Nothing }

instance BeamBackend be => FromSqlValues be AutoId where
    makeSqlValues' UnassignedId = [ sqlNull ]
    makeSqlValues' (AssignedId i) = [ sqlInteger (fromIntegral i) ]
    fromSqlValues' = do val <- fromBackendValue <$> popSqlValue
                        case val of
                          Just BeamNull -> pure UnassignedId
                          Just (BeamInteger i) -> pure (AssignedId (fromIntegral i))
                          _ -> throwError "fromSqlValues' (AutoId): expected BeamInteger"
