{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses, EmptyDataDecls, DefaultSignatures, FlexibleContexts, FlexibleInstances, OverloadedStrings, PolyKinds, GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances, RankNTypes #-}
module Database.Beam.Schema.Fields where

import Database.Beam.Schema.Tables
import Database.Beam.SQL.Types

import Control.Applicative
import Control.Arrow
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

-- ** Enum fields

newtype BeamEnum a = BeamEnum { unBeamEnum :: a }
    deriving (Show, Typeable)

instance (Enum a, Show a, Read a, Typeable a) => FieldSchema (BeamEnum a) where
    data FieldSettings (BeamEnum a) = EnumSettings
                                    { maxNameSize :: Maybe Int }
                                      deriving Show

    defSettings = EnumSettings Nothing

    colDescFromSettings (EnumSettings nameSize) = colDescFromSettings (defSettings { charOrVarChar = Varchar nameSize })

    makeSqlValue (BeamEnum x) = SqlString (show x)
    fromSqlValue = BeamEnum . read . fromSql <$> popSqlValue

-- ** Text field

data CharOrVarchar = Char (Maybe Int)
                   | Varchar (Maybe Int)
                     deriving Show

instance FieldSchema Text where
    -- | Settings for a text field
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

-- ** Auto-increment fields

data AutoId = UnassignedId
            | AssignedId !Int
              deriving (Show, Read, Eq, Ord, Generic)

instance FieldSchema AutoId where
    data FieldSettings AutoId = AutoIdDefault
                                deriving Show
    defSettings = AutoIdDefault
    colDescFromSettings _ = SQLColumnSchema desc [SQLNotNull, SQLAutoIncrement]
        where desc = SqlColDesc
                     { colType = SqlNumericT
                     , colSize = Nothing
                     , colOctetLength = Nothing
                     , colDecDigits = Nothing
                     , colNullable = Nothing }

    makeSqlValue UnassignedId = SqlNull
    makeSqlValue (AssignedId i) = SqlInteger (fromIntegral i)
    fromSqlValue = maybe UnassignedId AssignedId . fromSql <$> popSqlValue
