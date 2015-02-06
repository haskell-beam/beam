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

-- * Generic fields

instance Locator (Column name t) Found where
    type LocateResult (Column name t) Found = Column name t

    locate f _ = f

instance SchemaPart (Column name t)

instance Locator (ForeignKey table name) Found where
    type LocateResult (ForeignKey table name) Found = ForeignKey table name

    locate f _ = f

instance Locator (PrimaryKeySchema table) a =>
    Locator (ForeignKey table name) (Descend a) where
    type LocateResult (ForeignKey table name) (Descend a) = LocateResult (PrimaryKeySchema table) a

    locate (ForeignKey s) locator = locate s (subLocator locator)
        where subLocator :: Descend a -> a
              subLocator _ = undefined

instance (FromSqlValues (PrimaryKeySchema table), Show (PrimaryKeySchema table)) =>
    FromSqlValues (ForeignKey table name) where
    fromSqlValues' = ForeignKey <$> fromSqlValues'

-- * Relationship fields

-- instance SQLValable t => FromSqlValues (Column name t) where
--     fromSqlValues' = Column <$> fromSqlValues''

-- -- ** Relationship fields

-- newtype DefaultPKField table name = DefaultPKField (DefaultPrimaryKeyFor table)
--     deriving Show
-- type instance NameFor (DefaultPKField table name) = name

-- instance Locator (DefaultPKField table name) Found where
--     type LocateResult (DefaultPKField table name) Found = LocateResult (DefaultPrimaryKeyFor table) Found

--     locate (DefaultPKField x) r = locate x r

-- instance FieldSchema (DefaultPKField table name) where
--     data FieldSettings (DefaultPKField table name) = DefaultPKFieldSettings (FieldSettings (DefaultPrimaryKeyFor table))
--                                                    deriving Show
--     type FieldType (DefaultPKField table name) = FieldType (DefaultPrimaryKeyFor table)

--     defSettings _ = DefaultPKFieldSettings (defSettings Proxy)

--     colDescFromSettings (DefaultPKFieldSettings s) = colDescFromSettings s
--     makeSqlValue (DefaultPKField x) = makeSqlValue x

--     field = DefaultPKField . field
--     fieldValue (DefaultPKField x) = fieldValue x

-- instance SchemaPart (DefaultPKField table name)
-- instance FromSqlValues (DefaultPKField table name) where
--     fromSqlValues' = DefaultPKField <$> fromSqlValues'

-- ** Maybe field

instance FieldSchema a => FieldSchema (Maybe a) where
    data FieldSettings (Maybe a) = MaybeFieldSettings (FieldSettings a)

    defSettings = MaybeFieldSettings defSettings

    colDescFromSettings (MaybeFieldSettings settings) = let SQLColumnSchema desc constraints = colDescFromSettings settings
                                                        in SQLColumnSchema desc (filter (/=SQLNotNull) constraints)

    makeSqlValue Nothing = SqlNull
    makeSqlValue (Just x) = makeSqlValue x
    fromSqlValue = do val <- peekSqlValue
                      case val of
                        SqlNull -> Nothing <$ popSqlValue
                        val -> Just <$> fromSqlValue

deriving instance Show (FieldSettings a) => Show (FieldSettings (Maybe a))

-- ** Text field

data CharOrVarchar = Char (Maybe Int)
                   | Varchar (Maybe Int)
                     deriving Show


instance FieldSchema Text where
    data FieldSettings Text = TextFieldSettings
                            { charOrVarChar :: CharOrVarchar }
                              deriving Show

    defSettings = TextFieldSettings (Varchar Nothing)

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

    makeSqlValue x = SqlString (unpack x)
    fromSqlValue = fromSql <$> popSqlValue

-- ** Date time fields

instance FieldSchema UTCTime where
    data FieldSettings UTCTime = DateTimeDefault
                                 deriving Show

    defSettings = DateTimeDefault
    colDescFromSettings _ = notNull $
                            SqlColDesc
                            { colType = SqlUTCDateTimeT
                            , colSize = Nothing
                            , colOctetLength = Nothing
                            , colDecDigits = Nothing
                            , colNullable = Nothing }
    makeSqlValue = SqlUTCTime
    fromSqlValue = fromSql <$> popSqlValue
