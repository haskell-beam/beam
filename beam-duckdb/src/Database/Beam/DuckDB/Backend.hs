{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.DuckDB.Backend (DuckDB) where

import Data.ByteString (ByteString)
import Data.Data (Proxy (Proxy))
import Data.Functor (($>))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Beam (HasQBuilder, HasSqlEqualityCheck, HasSqlInTable (..), HasSqlQuantifiedEqualityCheck)
import Database.Beam.Backend (
    BeamBackend (..),
    BeamSqlBackend,
    BeamSqlBackendIsString,
    BeamSqlBackendSyntax,
    FromBackendRow,
    SqlNull (..),
    parseOneField,
 )
import Database.Beam.Backend.SQL (FromBackendRow (..))
import Database.Beam.DuckDB.Syntax (
    DuckDBCommandSyntax,
    DuckDBExpressionSyntax (..),
 )
import Database.Beam.DuckDB.Syntax.Builder (
    commas,
    emit,
    parens,
 )
import Database.Beam.Query.SQL92 (buildSql92Query')
import Database.Beam.Query.Types (HasQBuilder (..))
import Database.DuckDB.Simple (Null)
import Database.DuckDB.Simple.FromField (FromField)

data DuckDB

type instance BeamSqlBackendSyntax DuckDB = DuckDBCommandSyntax

instance BeamSqlBackend DuckDB

instance HasQBuilder DuckDB where
    buildSqlQuery = buildSql92Query' True

instance HasSqlInTable DuckDB where
    inRowValuesE Proxy e es =
        DuckDBExpressionSyntax $
            mconcat
                [ parens $ fromDuckDBExpression e
                , emit " IN "
                , parens $ emit "VALUES " <> commas (map fromDuckDBExpression es)
                ]

instance BeamSqlBackendIsString DuckDB Text

instance BeamSqlBackendIsString DuckDB String

instance BeamBackend DuckDB where
    type BackendFromField DuckDB = FromField

instance FromBackendRow DuckDB SqlNull where
    fromBackendRow = parseOneField @DuckDB @Null $> SqlNull

instance FromBackendRow DuckDB Bool

instance FromBackendRow DuckDB Float

instance FromBackendRow DuckDB Double

instance FromBackendRow DuckDB Integer

instance FromBackendRow DuckDB Int

instance FromBackendRow DuckDB Int8

instance FromBackendRow DuckDB Int16

instance FromBackendRow DuckDB Int32

instance FromBackendRow DuckDB Int64

instance FromBackendRow DuckDB Word

instance FromBackendRow DuckDB Word8

instance FromBackendRow DuckDB Word16

instance FromBackendRow DuckDB Word32

instance FromBackendRow DuckDB Word64

instance FromBackendRow DuckDB Text

instance FromBackendRow DuckDB ByteString

instance FromBackendRow DuckDB UUID

instance FromBackendRow DuckDB Day

instance FromBackendRow DuckDB TimeOfDay

instance FromBackendRow DuckDB LocalTime

instance FromBackendRow DuckDB UTCTime

instance HasSqlEqualityCheck DuckDB Bool

instance HasSqlEqualityCheck DuckDB Float

instance HasSqlEqualityCheck DuckDB Double

instance HasSqlEqualityCheck DuckDB Integer

instance HasSqlEqualityCheck DuckDB Int

instance HasSqlEqualityCheck DuckDB Int8

instance HasSqlEqualityCheck DuckDB Int16

instance HasSqlEqualityCheck DuckDB Int32

instance HasSqlEqualityCheck DuckDB Int64

instance HasSqlEqualityCheck DuckDB Word

instance HasSqlEqualityCheck DuckDB Word8

instance HasSqlEqualityCheck DuckDB Word16

instance HasSqlEqualityCheck DuckDB Word32

instance HasSqlEqualityCheck DuckDB Word64

instance HasSqlEqualityCheck DuckDB Text

instance HasSqlEqualityCheck DuckDB ByteString

instance HasSqlEqualityCheck DuckDB UUID

instance HasSqlEqualityCheck DuckDB Day

instance HasSqlEqualityCheck DuckDB TimeOfDay

instance HasSqlEqualityCheck DuckDB LocalTime

instance HasSqlEqualityCheck DuckDB UTCTime

instance HasSqlQuantifiedEqualityCheck DuckDB Bool

instance HasSqlQuantifiedEqualityCheck DuckDB Float

instance HasSqlQuantifiedEqualityCheck DuckDB Double

instance HasSqlQuantifiedEqualityCheck DuckDB Integer

instance HasSqlQuantifiedEqualityCheck DuckDB Int

instance HasSqlQuantifiedEqualityCheck DuckDB Int8

instance HasSqlQuantifiedEqualityCheck DuckDB Int16

instance HasSqlQuantifiedEqualityCheck DuckDB Int32

instance HasSqlQuantifiedEqualityCheck DuckDB Int64

instance HasSqlQuantifiedEqualityCheck DuckDB Word

instance HasSqlQuantifiedEqualityCheck DuckDB Word8

instance HasSqlQuantifiedEqualityCheck DuckDB Word16

instance HasSqlQuantifiedEqualityCheck DuckDB Word32

instance HasSqlQuantifiedEqualityCheck DuckDB Word64

instance HasSqlQuantifiedEqualityCheck DuckDB Text

instance HasSqlQuantifiedEqualityCheck DuckDB ByteString

instance HasSqlQuantifiedEqualityCheck DuckDB UUID

instance HasSqlQuantifiedEqualityCheck DuckDB Day

instance HasSqlQuantifiedEqualityCheck DuckDB TimeOfDay

instance HasSqlQuantifiedEqualityCheck DuckDB LocalTime

instance HasSqlQuantifiedEqualityCheck DuckDB UTCTime
