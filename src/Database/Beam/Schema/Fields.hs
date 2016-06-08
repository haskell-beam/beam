module Database.Beam.Schema.Fields where

import Database.Beam.Schema.Tables
import Database.Beam.SQL.Types

import Data.Time.Clock
import Data.Text (Text, unpack)
import Data.Typeable

import Database.HDBC ( SqlColDesc(..), SqlTypeId(..), SqlValue(..)
                     , fromSql)

import GHC.Generics

-- * Fields

enumSchema :: Enum a => FieldSchema a
enumSchema = let schema :: Enum a => FieldSchema a
                 schema = FieldSchema
                          { fsColDesc = fsColDesc intSchema
                          , fsHumanReadable = "enumField"
                          , fsMakeSqlValue = SqlInteger . fromIntegral . fromEnum
                          , fsFromSqlValue = do val <- fromSql <$> popSqlValue
                                                pure (toEnum val) }
             in schema
instance HasDefaultFieldSchema a => HasDefaultFieldSchema (Maybe a) where
    defFieldSchema = maybeFieldSchema defFieldSchema

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
          , fsHumanReadable = "intSchema"
          , fsMakeSqlValue = SqlInteger . fromIntegral
          , fsFromSqlValue = fromSql <$> popSqlValue }
instance HasDefaultFieldSchema Int where
    defFieldSchema = intSchema
instance FromSqlValues Int

-- ** Text field

data CharOrVarchar = Char (Maybe Int)
                   | Varchar (Maybe Int)
                     deriving Show

textSchema :: CharOrVarchar -> FieldSchema Text
textSchema charOrVarchar = FieldSchema
                           { fsColDesc = colDesc
                           , fsHumanReadable = "textSchema (" ++ show charOrVarchar ++ ")"
                           , fsMakeSqlValue = SqlString . unpack
                           , fsFromSqlValue = fromSql <$> popSqlValue }
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

instance HasDefaultFieldSchema Text where
    defFieldSchema = defaultTextSchema
instance FromSqlValues Text

dateTimeSchema :: FieldSchema UTCTime
dateTimeSchema = FieldSchema
                 { fsColDesc = notNull $
                               SqlColDesc
                               { colType = SqlUTCDateTimeT
                               , colSize = Nothing
                               , colOctetLength = Nothing
                               , colDecDigits = Nothing
                              , colNullable = Nothing }
                 , fsHumanReadable = "dateTimeField"
                 , fsMakeSqlValue = SqlUTCTime
                 , fsFromSqlValue = fromSql <$> popSqlValue }

instance HasDefaultFieldSchema UTCTime where
    defFieldSchema = dateTimeSchema
instance FromSqlValues UTCTime

-- ** Auto-increment fields

data AutoId = UnassignedId
            | AssignedId !Int
              deriving (Show, Read, Eq, Ord, Generic)

autoIdSchema :: FieldSchema AutoId
autoIdSchema = FieldSchema
               { fsColDesc = SQLColumnSchema desc [SQLNotNull, SQLAutoIncrement]
               , fsHumanReadable = "autoIdSchema"
               , fsMakeSqlValue = \x -> case x of
                                          UnassignedId -> SqlNull
                                          AssignedId i -> SqlInteger (fromIntegral i)
               , fsFromSqlValue = maybe UnassignedId AssignedId . fromSql <$> popSqlValue }
    where desc = SqlColDesc
                 { colType = SqlNumericT
                 , colSize = Nothing
                 , colOctetLength = Nothing
                 , colDecDigits = Nothing
                 , colNullable = Nothing }
instance HasDefaultFieldSchema AutoId where
    defFieldSchema = autoIdSchema
instance FromSqlValues AutoId
