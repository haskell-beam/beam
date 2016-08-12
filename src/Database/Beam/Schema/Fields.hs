module Database.Beam.Schema.Fields
  (
    -- * Field schema definition
    FieldSchema(..), HasDefaultFieldSchema(..),
    FromSqlValues(..), FromSqlValuesM,

    -- * Field schemas
    -- ** Basic fields
    intSchema, enumSchema, maybeSchema,
    -- ** Character fields
    CharOrVarchar(..), textSchema,
    -- ** Date fields
    dateTimeSchema,
    -- ** Auto-increment fields
    AutoId(..), autoIdSchema,
  )
where

import Database.Beam.SQL.Types

import Data.Time.Clock
import Data.Text (Text, unpack)
import Data.Proxy
import Control.Monad.State
import Control.Monad.Except

import Database.HDBC ( SqlColDesc(..), SqlTypeId(..), SqlValue(..)
                     , fromSql)

import GHC.Generics (Generic)


data FieldSchema ty = FieldSchema {
    fsColDesc :: SQLColumnSchema
  , fsHumanReadable :: String
  , fsMakeSqlValue :: ty -> SqlValue
  , fsFromSqlValue :: FromSqlValuesM ty }
instance Show (FieldSchema ty) where
    show (FieldSchema desc hr _ _) = concat ["FieldSchema (", show desc, ") (", show hr, ") _ _"]

-- | Type class for types which can construct a default 'TableField' given a column name.
class HasDefaultFieldSchema fs where
    defFieldSchema :: FieldSchema fs

type FromSqlValuesM a = ExceptT String (State [SqlValue]) a
popSqlValue, peekSqlValue :: FromSqlValuesM SqlValue
popSqlValue = do
  st <- get
  put (tail st)
  return (head st)
peekSqlValue = head <$> get
class FromSqlValues a where
    fromSqlValues' :: FromSqlValuesM a
    valuesNeeded :: Proxy a -> Int
    valuesNeeded _ = 1

    default fromSqlValues' :: HasDefaultFieldSchema a => FromSqlValuesM a
    fromSqlValues' = fsFromSqlValue defFieldSchema

instance FromSqlValues t => FromSqlValues (Maybe t) where
    valuesNeeded (_ :: Proxy (Maybe t)) = valuesNeeded (Proxy :: Proxy t)
    fromSqlValues' = mfix $ \(_ :: Maybe t) -> do
      values <- get
      let colCount = valuesNeeded (Proxy :: Proxy t)
          colValues = take colCount values
      if all (== SqlNull) colValues
      then put (drop colCount values) >> return Nothing
      else Just <$> fromSqlValues'

instance (FromSqlValues a, FromSqlValues b) => FromSqlValues (a, b) where
    fromSqlValues' = (,) <$> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b)
instance (FromSqlValues a, FromSqlValues b, FromSqlValues c) => FromSqlValues (a, b, c) where
    fromSqlValues' = (,,) <$> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy b) + valuesNeeded (Proxy :: Proxy b) + valuesNeeded (Proxy :: Proxy c)
instance (FromSqlValues a, FromSqlValues b, FromSqlValues c, FromSqlValues d) => FromSqlValues (a, b, c, d) where
    fromSqlValues' = (,,,) <$> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b) + valuesNeeded (Proxy :: Proxy c) + valuesNeeded (Proxy :: Proxy d)
instance (FromSqlValues a, FromSqlValues b, FromSqlValues c, FromSqlValues d, FromSqlValues e) => FromSqlValues (a, b, c, d, e) where
    fromSqlValues' = (,,,,) <$> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues' <*> fromSqlValues'
    valuesNeeded _ = valuesNeeded (Proxy :: Proxy a) + valuesNeeded (Proxy :: Proxy b) + valuesNeeded (Proxy :: Proxy c) + valuesNeeded (Proxy :: Proxy d) + valuesNeeded (Proxy :: Proxy e)


maybeSchema :: FieldSchema ty -> FieldSchema (Maybe ty)
maybeSchema base = FieldSchema {
    fsColDesc = SQLColumnSchema desc (filter (/= SQLNotNull) constraints)
  , fsHumanReadable = "maybeFieldSchema (" ++ fsHumanReadable base ++ ")"
  , fsMakeSqlValue = \case
      Nothing -> SqlNull
      Just x -> fsMakeSqlValue base x
  , fsFromSqlValue = peekSqlValue >>= \case
      SqlNull -> Nothing <$ popSqlValue
      _ -> Just <$> fsFromSqlValue base }
  where
    SQLColumnSchema desc constraints = fsColDesc base
instance HasDefaultFieldSchema a => HasDefaultFieldSchema (Maybe a) where
    defFieldSchema = maybeSchema defFieldSchema

enumSchema :: Enum a => FieldSchema a
enumSchema = FieldSchema {
    fsColDesc = fsColDesc intSchema
  , fsHumanReadable = "enumField"
  , fsMakeSqlValue = SqlInteger . fromIntegral . fromEnum
  , fsFromSqlValue = fromSql <$> popSqlValue >>= pure . toEnum }

scdn :: SqlTypeId -> Maybe Int -> SqlColDesc
scdn t sz = SqlColDesc {
    colType = t
  , colSize = sz
  , colOctetLength = Nothing
  , colDecDigits = Nothing
  , colNullable = Nothing }
scd :: SqlTypeId -> SqlColDesc
scd t = scdn t Nothing
nnscd :: SqlTypeId -> SQLColumnSchema
nnscd = notNull . scd
nnscdn :: SqlTypeId -> Maybe Int -> SQLColumnSchema
nnscdn t sz = notNull $ scdn t sz

intSchema :: FieldSchema Int
intSchema = FieldSchema {
    fsColDesc = nnscd SqlNumericT
  , fsHumanReadable = "intSchema"
  , fsMakeSqlValue = SqlInteger . fromIntegral
  , fsFromSqlValue = fromSql <$> popSqlValue }
instance HasDefaultFieldSchema Int where
    defFieldSchema = intSchema
instance FromSqlValues Int

data CharOrVarchar = Char (Maybe Int)
                   | Varchar (Maybe Int)
                     deriving Show

textSchema :: CharOrVarchar -> FieldSchema Text
textSchema charOrVarchar = FieldSchema {
    fsColDesc = colDesc
  , fsHumanReadable = "textSchema (" ++ show charOrVarchar ++ ")"
  , fsMakeSqlValue = SqlString . unpack
  , fsFromSqlValue = fromSql <$> popSqlValue }
    where colDesc = case charOrVarchar of
                      Char n -> nnscdn SqlCharT n
                      Varchar n -> nnscdn SqlVarCharT n
defaultTextSchema :: FieldSchema Text
defaultTextSchema = textSchema (Varchar Nothing)

instance HasDefaultFieldSchema Text where
    defFieldSchema = defaultTextSchema
instance FromSqlValues Text

dateTimeSchema :: FieldSchema UTCTime
dateTimeSchema = FieldSchema {
    fsColDesc = nnscd SqlUTCDateTimeT
  , fsHumanReadable = "dateTimeField"
  , fsMakeSqlValue = SqlUTCTime
  , fsFromSqlValue = fromSql <$> popSqlValue }

instance HasDefaultFieldSchema UTCTime where
    defFieldSchema = dateTimeSchema
instance FromSqlValues UTCTime

data AutoId = UnassignedId
            | AssignedId !Int
              deriving (Show, Read, Eq, Ord, Generic)

autoIdSchema :: FieldSchema AutoId
autoIdSchema = FieldSchema {
    fsColDesc = SQLColumnSchema (scd SqlNumericT) [SQLNotNull, SQLAutoIncrement]
  , fsHumanReadable = "autoIdSchema"
  , fsMakeSqlValue = \case
      UnassignedId -> SqlNull
      AssignedId i -> SqlInteger (fromIntegral i)
  , fsFromSqlValue = maybe UnassignedId AssignedId . fromSql <$> popSqlValue }
instance HasDefaultFieldSchema AutoId where
    defFieldSchema = autoIdSchema
instance FromSqlValues AutoId
