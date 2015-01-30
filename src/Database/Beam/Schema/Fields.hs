{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances, RankNTypes #-}
module Database.Beam.Schema.Fields where

import Database.Beam.Schema.Locate
import Database.Beam.Schema.Tables
import Database.Beam.SQL.Types

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import Data.Time.Clock
import Data.Text (Text, unpack)
import Data.Proxy
import Data.Default
import Data.String
import Data.Typeable

import Database.HDBC ( SqlColDesc(..), SqlTypeId(..), SqlValue(..)
                     , fromSql)

import GHC.Generics hiding (R)
import qualified GHC.Generics as Generic

-- * Fields

--    fromValue :: FieldType fs -> fs

type FromSqlValuesM a = ErrorT String (State [SqlValue]) a
popSqlValue, peekSqlValue :: FromSqlValuesM SqlValue
popSqlValue = do st <- get
                 put (tail st)
                 return (head st)
peekSqlValue = head <$> get
class FromSqlValues a where
    fromSqlValues' :: FromSqlValuesM a
instance (FromSqlValues a, FromSqlValues b) => FromSqlValues (a :|: b) where
    fromSqlValues' = (:|:) <$> fromSqlValues' <*> fromSqlValues'

-- ** Relationship fields

newtype DefaultPKField table name = DefaultPKField (DefaultPrimaryKeyFor table)
    deriving Show
type instance NameFor (DefaultPKField table name) = name

instance Locator (DefaultPKField table name) Found where
    type LocateResult (DefaultPKField table name) Found = LocateResult (DefaultPrimaryKeyFor table) Found

    locate (DefaultPKField x) r = locate x r

instance FieldSchema (DefaultPKField table name) where
    data FieldSettings (DefaultPKField table name) = DefaultPKFieldSettings (FieldSettings (DefaultPrimaryKeyFor table))
                                                   deriving Show
    type FieldType (DefaultPKField table name) = FieldType (DefaultPrimaryKeyFor table)

    defSettings _ = DefaultPKFieldSettings (defSettings Proxy)

    colDescFromSettings (DefaultPKFieldSettings s) = colDescFromSettings s
    makeSqlValue (DefaultPKField x) = makeSqlValue x

    field = DefaultPKField . field
    fieldValue (DefaultPKField x) = fieldValue x

instance SchemaPart (DefaultPKField table name)
instance FromSqlValues (DefaultPKField table name) where
    fromSqlValues' = DefaultPKField <$> fromSqlValues'

-- ** Maybe field

data MaybeField field = MaybeField (Maybe field)
                        deriving Show
type instance NameFor (MaybeField field) = NameFor field

instance Locator field Found => Locator (MaybeField field) Found where
    type LocateResult (MaybeField field) Found = MaybeField field

instance (FieldSchema field, Show (FieldSettings field)) => FieldSchema (MaybeField field) where
    data FieldSettings (MaybeField field) = WrapMaybeField (FieldSettings field)
    type FieldType (MaybeField field) = Maybe (FieldType field)

    defSettings (_ :: Proxy (MaybeField field)) = WrapMaybeField (defSettings (Proxy :: Proxy field))

    colDescFromSettings (WrapMaybeField settings) = let SQLColumnSchema desc constraints = colDescFromSettings settings
                                                    in SQLColumnSchema desc (filter (/=SQLNotNull) constraints)

    makeSqlValue (MaybeField Nothing) = SqlNull
    makeSqlValue (MaybeField (Just field)) = makeSqlValue field

    fieldValue (MaybeField Nothing) = Nothing
    fieldValue (MaybeField (Just x)) = Just (fieldValue x)
    field Nothing = MaybeField Nothing
    field (Just x) = MaybeField (Just (field x))

deriving instance Show (FieldSettings field) => Show (FieldSettings (MaybeField field))

instance SchemaPart (MaybeField field)
instance FromSqlValues field => FromSqlValues (MaybeField field) where
    fromSqlValues' = do val <- peekSqlValue
                        case val of
                          SqlNull -> popSqlValue >> return (MaybeField Nothing)
                          val -> MaybeField . Just <$> fromSqlValues'

-- ** Text fields

data TextField name = TextField Text
                    deriving Show
type instance NameFor (TextField name) = name

instance Locator (TextField name) Found where
    type LocateResult (TextField name) Found = TextField name

data CharOrVarchar = Char (Maybe Int)
                   | Varchar (Maybe Int)
                     deriving Show

instance FieldSchema (TextField name) where
    data FieldSettings (TextField name) = TextFieldSettings
                                        { charOrVarChar :: CharOrVarchar }
                                        deriving Show
    type FieldType (TextField name) = Text

    defSettings _ = TextFieldSettings (Varchar Nothing)

    colDescFromSettings (TextFieldSettings (Char n)) = notNull $
                                                       SqlColDesc
                                                       { colType = SqlCharT
                                                       , colSize = n
                                                       , colOctetLength = Nothing
                                                       , colDecDigits = Nothing
                                                       , colNullable = Nothing }
    colDescFromSettings (TextFieldSettings (Varchar n)) = notNull $
                                                          SqlColDesc
                                                          { colType = SqlVarCharT
                                                          , colSize = n
                                                          , colOctetLength = Nothing
                                                          , colDecDigits = Nothing
                                                          , colNullable = Nothing }

    makeSqlValue (TextField x) = SqlString (unpack x)

    fieldValue (TextField x) = x
    field = TextField

instance SchemaPart (TextField name)
instance FromSqlValues (TextField name) where
    fromSqlValues' = TextField <$> (fromSql <$> popSqlValue)

-- ** Int fields

-- IntField is defined in Table.hs because it's used for default primary keys
type instance NameFor (IntField name) = name

instance Locator (IntField name) Found where
    type LocateResult (IntField name) Found = IntField name

instance SchemaPart (IntField name)
instance FromSqlValues (IntField name) where
    fromSqlValues' = IntField <$> (fromSql <$> popSqlValue)

-- ** Date time fields

data DateTimeField name = DateTimeField UTCTime
                          deriving Show
type instance NameFor (DateTimeField name) = name

instance Locator (DateTimeField name) Found where
    type LocateResult (DateTimeField name) Found = DateTimeField name

instance FieldSchema (DateTimeField name) where
    data FieldSettings (DateTimeField name) = DateTimeDefault
                                              deriving Show
    type FieldType (DateTimeField name) = UTCTime

    defSettings _ = DateTimeDefault
    colDescFromSettings _ = notNull $
                            SqlColDesc
                            { colType = SqlUTCDateTimeT
                            , colSize = Nothing
                            , colOctetLength = Nothing
                            , colDecDigits = Nothing
                            , colNullable = Nothing }
    makeSqlValue (DateTimeField d) = SqlUTCTime d
    field = DateTimeField
    fieldValue (DateTimeField x) = x
instance SchemaPart (DateTimeField name)
instance FromSqlValues (DateTimeField name) where
    fromSqlValues' = DateTimeField <$> (fromSql <$> popSqlValue)
