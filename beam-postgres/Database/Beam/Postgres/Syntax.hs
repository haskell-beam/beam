{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

-- | Data types for Postgres syntax. Access is given mainly for extension
-- modules. The types and definitions here are likely to change.
module Database.Beam.Postgres.Syntax where
    -- ( PgSyntaxF(..), PgSyntaxM
    -- , PgSyntax(..)

    -- , emit, emitBuilder, escapeString
    -- , escapeBytea, escapeIdentifier
    -- , pgParens

    -- , nextSyntaxStep

    -- , PgCommandSyntax(..), PgCommandType(..)
    -- , PgSelectSyntax(..), PgSelectSetQuantifierSyntax(..)
    -- , PgInsertSyntax(..)
    -- , PgDeleteSyntax(..)
    -- , PgUpdateSyntax(..)

    -- , PgExpressionSyntax(..), PgFromSyntax(..)
    -- , PgComparisonQuantifierSyntax(..)
    -- , PgExtractFieldSyntax(..)
    -- , PgProjectionSyntax(..), PgGroupingSyntax(..)
    -- , PgOrderingSyntax(..), PgValueSyntax(..)
    -- , PgTableSourceSyntax(..), PgFieldNameSyntax(..)
    -- , PgAggregationSetQuantifierSyntax(..)
    -- , PgInsertValuesSyntax(..), PgInsertOnConflictSyntax(..)
    -- , PgInsertOnConflictTargetSyntax(..), PgConflictActionSyntax(..)
    -- , PgCreateTableSyntax(..), PgTableOptionsSyntax(..), PgColumnSchemaSyntax(..)
    -- , PgDataTypeSyntax(..), PgColumnConstraintDefinitionSyntax(..), PgColumnConstraintSyntax(..)
    -- , PgTableConstraintSyntax(..), PgMatchTypeSyntax(..), PgReferentialActionSyntax(..)

    -- , PgAlterTableSyntax(..), PgAlterTableActionSyntax(..), PgAlterColumnActionSyntax(..)

    -- , PgWindowFrameSyntax(..), PgWindowFrameBoundsSyntax(..), PgWindowFrameBoundSyntax(..)

    -- , PgSelectLockingClauseSyntax(..)
    -- , PgSelectLockingStrength(..)
    -- , PgSelectLockingOptions(..)
    -- , fromPgSelectLockingClause
    -- , pgSelectStmt

    -- , PgDataTypeDescr(..)

    -- , pgCreateExtensionSyntax, pgDropExtensionSyntax

    -- , insertDefaults
    -- , pgSimpleMatchSyntax

    -- , pgSelectSetQuantifierDistinctOn

    -- , pgDataTypeJSON

    -- , pgTsQueryType, pgTsVectorType
    -- , pgJsonType, pgJsonbType, pgUuidType
    -- , pgMoneyType
    -- , pgTsQueryTypeInfo, pgTsVectorTypeInfo

    -- , pgByteaType, pgTextType, pgUnboundedArrayType
    -- , pgSerialType, pgSmallSerialType, pgBigSerialType

    -- , pgQuotedIdentifier, pgSepBy, pgDebugRenderSyntax
    -- , pgRenderSyntaxScript, pgBuildAction

    -- , pgBinOp, pgCompOp, pgUnOp, pgPostFix

    -- , pgTestSyntax

    -- , PostgresInaccessible ) where

import           Database.Beam hiding (insert)
import           Database.Beam.Backend.SQL
import           Database.Beam.Query.SQL92

-- import           Database.Beam.Migrate.SQL
-- import           Database.Beam.Migrate.SQL.Builder hiding (fromSqlConstraintAttributes)
-- import           Database.Beam.Migrate.Serialization

-- import           Database.Beam.Migrate.Generics

import           Control.Monad.Free
import           Control.Monad.Free.Church

import           Data.Aeson (Value, object, (.=))
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder, byteString, char8, toLazyByteString)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy.Char8 (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Coerce
import           Data.Hashable
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific (Scientific)
import           Data.String (IsString(..), fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import           Data.Time (LocalTime, UTCTime, ZonedTime, TimeOfDay, NominalDiffTime, Day)
import           Data.UUID (UUID)
import qualified Data.Vector as V
import           Data.Word

import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg (Oid(..), Binary(..), Null(..))
import qualified Database.PostgreSQL.Simple.Time as Pg (Date, ZonedTimestamp, LocalTimestamp, UTCTimestamp)
import qualified Database.PostgreSQL.Simple.HStore as Pg (HStoreList, HStoreMap, HStoreBuilder)

data PostgresInaccessible

-- TODO This probably shouldn't be a free monad... oh well.
data PgSyntaxF f where
  EmitByteString :: ByteString -> f -> PgSyntaxF f
  EmitBuilder    :: Builder -> f -> PgSyntaxF f

  EscapeString :: ByteString -> f -> PgSyntaxF f
  EscapeBytea  :: ByteString -> f -> PgSyntaxF f
  EscapeIdentifier :: ByteString -> f -> PgSyntaxF f
deriving instance Functor PgSyntaxF

instance Eq f => Eq (PgSyntaxF f) where
  EmitByteString b1 next1 == EmitByteString b2 next2 =
      b1 == b2 && next1 == next2
  EmitBuilder b1 next1 == EmitBuilder b2 next2 =
      toLazyByteString b1 == toLazyByteString b2 && next1 == next2
  EscapeString b1 next1 == EscapeString b2 next2 =
      b1 == b2 && next1 == next2
  EscapeBytea b1 next1 == EscapeBytea b2 next2 =
      b1 == b2 && next1 == next2
  EscapeIdentifier b1 next1 == EscapeIdentifier b2 next2 =
      b1 == b2 && next1 == next2
  _ == _ = False

instance Hashable Syntax where
  hashWithSalt salt (PgSyntax s) = runF s finish step salt
    where
      finish _ salt = hashWithSalt salt ()
      step (EmitByteString b hashRest) salt = hashRest (hashWithSalt salt (0 :: Int, b))
      step (EmitBuilder b hashRest)    salt = hashRest (hashWithSalt salt (1 :: Int, toLazyByteString b))
      step (EscapeString  b hashRest)  salt = hashRest (hashWithSalt salt (2 :: Int, b))
      step (EscapeBytea  b hashRest)   salt = hashRest (hashWithSalt salt (3 :: Int, b))
      step (EscapeIdentifier b hashRest) salt = hashRest (hashWithSalt salt (4 :: Int, b))

type PgSyntaxM = F PgSyntaxF

-- | A piece of Postgres SQL syntax, which may contain embedded escaped byte and
-- text sequences. 'PgSyntax' composes monoidally, and may be created with
-- 'emit', 'emitBuilder', 'escapeString', 'escapBytea', and 'escapeIdentifier'.
newtype Syntax
  = PgSyntax { buildPgSyntax :: PgSyntaxM () }

instance Monoid Syntax where
  mempty = PgSyntax (pure ())

instance Semigroup Syntax where
  (<>) a b = PgSyntax (buildPgSyntax a >> buildPgSyntax b)

-- instance Eq Syntax where
--   PgSyntax x == PgSyntax y = (fromF x :: Free PgSyntaxF ()) == fromF y

instance Show Syntax where
  showsPrec prec s =
    showParen (prec > 10) $
    showString "PgSyntax <" .
    shows (pgTestSyntax s) .
    showString ">"

emit :: ByteString -> Syntax
emit bs = PgSyntax (liftF (EmitByteString bs ()))

emitBuilder :: Builder -> Syntax
emitBuilder b = PgSyntax (liftF (EmitBuilder b ()))

escapeString, escapeBytea, escapeIdentifier :: ByteString -> Syntax
escapeString bs = PgSyntax (liftF (EscapeString bs ()))
escapeBytea bin = PgSyntax (liftF (EscapeBytea bin ()))
escapeIdentifier id = PgSyntax (liftF (EscapeIdentifier id ()))

nextSyntaxStep :: PgSyntaxF f -> f
nextSyntaxStep (EmitByteString _ next) = next
nextSyntaxStep (EmitBuilder _ next) = next
nextSyntaxStep (EscapeString _ next) = next
nextSyntaxStep (EscapeBytea _ next) = next
nextSyntaxStep (EscapeIdentifier _ next) = next

-- * Syntax types

data PgCommandType
    = PgCommandTypeQuery
    | PgCommandTypeDdl
    | PgCommandTypeDataUpdate
    | PgCommandTypeDataUpdateReturning
      deriving Show

-- | Representation of an arbitrary Postgres command. This is the combination of
-- the command syntax (repesented by 'PgSyntax'), as well as the type of command
-- (represented by 'PgCommandType'). The command type is necessary for us to
-- know how to retrieve results from the database.
data Command
    = PgCommandSyntax
    { pgCommandType :: PgCommandType
    , fromPgCommand :: Syntax }

-- | 'IsSql92SelectSyntax' for Postgres
newtype SelectSyntax = PgSelectSyntax { fromPgSelect :: Syntax }

newtype SelectTableSyntax = PgSelectTableSyntax { fromPgSelectTable :: Syntax }

-- | 'IsSql92InsertSyntax' for Postgres
newtype InsertSyntax = PgInsertSyntax { fromPgInsert :: Syntax }

-- | 'IsSql92DeleteSyntax' for Postgres
newtype DeleteSyntax = PgDeleteSyntax { fromPgDelete :: Syntax }

-- | 'IsSql92UpdateSyntax' for Postgres
newtype UpdateSyntax = PgUpdateSyntax { fromPgUpdate :: Syntax }

newtype ExpressionSyntax = PgExpressionSyntax { fromPgExpression :: Syntax } 
newtype AggregationSetQuantifierSyntax = PgAggregationSetQuantifierSyntax { fromPgAggregationSetQuantifier :: Syntax }
newtype SelectSetQuantifierSyntax = PgSelectSetQuantifierSyntax { fromPgSelectSetQuantifier :: Syntax }
newtype FromSyntax = PgFromSyntax { fromPgFrom :: Syntax }
newtype ComparisonQuantifierSyntax = PgComparisonQuantifierSyntax { fromPgComparisonQuantifier :: Syntax }
newtype ExtractFieldSyntax = PgExtractFieldSyntax { fromPgExtractField :: Syntax }
newtype ProjectionSyntax = PgProjectionSyntax { fromPgProjection :: Syntax }
newtype GroupingSyntax = PgGroupingSyntax { fromPgGrouping :: Syntax }
newtype ValueSyntax = PgValueSyntax { fromPgValue :: Syntax }
newtype TableSourceSyntax = PgTableSourceSyntax { fromPgTableSource :: Syntax }
newtype FieldNameSyntax = PgFieldNameSyntax { fromPgFieldName :: Syntax }
newtype InsertValuesSyntax = PgInsertValuesSyntax { fromPgInsertValues :: Syntax }
newtype InsertOnConflictSyntax = PgInsertOnConflictSyntax { fromPgInsertOnConflict :: Syntax }
newtype InsertOnConflictTargetSyntax = PgInsertOnConflictTargetSyntax { fromPgInsertOnConflictTarget :: Syntax }
newtype InsertOnConflictUpdateSyntax = PgInsertOnConflictUpdateSyntax { fromPgInsertOnConflictUpdate :: Syntax }
newtype ConflictActionSyntax = PgConflictActionSyntax { fromPgConflictAction :: Syntax }
type OrderingSyntax = ExpressionSyntax
data PgSelectLockingClauseSyntax = PgSelectLockingClauseSyntax { pgSelectLockingClauseStrength :: PgSelectLockingStrength
                                                               , pgSelectLockingTables :: [T.Text]
                                                               , pgSelectLockingClauseOptions :: Maybe PgSelectLockingOptions }

fromPgOrdering :: OrderingSyntax -> Syntax
fromPgOrdering (PgExpressionSyntax s) = s
-- fromPgOrdering (PgOrderingSyntax s (Just PgNullOrderingNullsFirst)) = s <> emit " NULLS FIRST"
-- fromPgOrdering (PgOrderingSyntax s (Just PgNullOrderingNullsLast)) = s <> emit " NULLS LAST"

data PgNullOrdering
  = PgNullOrderingNullsFirst
  | PgNullOrderingNullsLast
  deriving (Show, Eq, Generic)

fromPgSelectLockingClause :: PgSelectLockingClauseSyntax -> Syntax
fromPgSelectLockingClause s =
  emit " FOR " <>
  (case pgSelectLockingClauseStrength s of
    PgSelectLockingStrengthUpdate -> emit "UPDATE"
    PgSelectLockingStrengthNoKeyUpdate -> emit "NO KEY UPDATE"
    PgSelectLockingStrengthShare -> emit "SHARE"
    PgSelectLockingStrengthKeyShare -> emit "KEY SHARE") <>
  emitTables <>
  (maybe mempty emitOptions $ pgSelectLockingClauseOptions s)
  where
    emitTables = case pgSelectLockingTables s of
      [] -> mempty
      tableNames -> emit " OF " <> (pgSepBy (emit ", ") (map pgQuotedIdentifier tableNames))

    emitOptions PgSelectLockingOptionsNoWait = emit " NOWAIT"
    emitOptions PgSelectLockingOptionsSkipLocked = emit " SKIP LOCKED"

data PgSelectLockingStrength
  = PgSelectLockingStrengthUpdate
  | PgSelectLockingStrengthNoKeyUpdate
  | PgSelectLockingStrengthShare
  | PgSelectLockingStrengthKeyShare
  deriving (Show, Eq, Generic)

data PgSelectLockingOptions
  = PgSelectLockingOptionsNoWait
  | PgSelectLockingOptionsSkipLocked
  deriving (Show, Eq, Generic)

data PgDataTypeDescr
  = PgDataTypeDescrOid Pg.Oid (Maybe Int32)
  | PgDataTypeDescrDomain T.Text
  deriving (Show, Eq, Generic)
instance Hashable PgDataTypeDescr where
  hashWithSalt salt (PgDataTypeDescrOid (Pg.Oid oid) dim) =
    hashWithSalt salt (0 :: Int, fromIntegral oid :: Word32, dim)
  hashWithSalt salt (PgDataTypeDescrDomain t) =
    hashWithSalt salt (1 :: Int, t)

-- newtype PgCreateTableSyntax = PgCreateTableSyntax { fromPgCreateTable :: PgSyntax }
-- data PgTableOptionsSyntax = PgTableOptionsSyntax PgSyntax PgSyntax
-- newtype PgColumnSchemaSyntax = PgColumnSchemaSyntax { fromPgColumnSchema :: PgSyntax } deriving (Show, Eq)

-- data PgDataTypeSyntax
--   = PgDataTypeSyntax
--   { pgDataTypeDescr :: PgDataTypeDescr
--   , fromPgDataType :: PgSyntax
--   , pgDataTypeSerialized :: BeamSerializedDataType
--   } deriving Show

-- data PgColumnConstraintDefinitionSyntax
--   = PgColumnConstraintDefinitionSyntax
--   { fromPgColumnConstraintDefinition :: PgSyntax
--   , pgColumnConstraintDefinitionSerialized :: BeamSerializedConstraintDefinition
--   } deriving Show

-- data PgColumnConstraintSyntax
--   = PgColumnConstraintSyntax
--   { fromPgColumnConstraint :: PgSyntax
--   , pgColumnConstraintSerialized :: BeamSerializedConstraint
--   }
newtype PgTableConstraintSyntax = PgTableConstraintSyntax { fromPgTableConstraint :: Syntax }
-- data PgMatchTypeSyntax
--   = PgMatchTypeSyntax
--   { fromPgMatchType :: PgSyntax
--   , pgMatchTypeSerialized :: BeamSerializedMatchType
--   }
-- data PgReferentialActionSyntax
--   = PgReferentialActionSyntax
--   { fromPgReferentialAction :: PgSyntax
--   , pgReferentialActionSerialized :: BeamSerializedReferentialAction
--   }
newtype PgDropTableSyntax = PgDropTableSyntax { fromPgDropTable :: Syntax }
newtype PgAlterTableSyntax = PgAlterTableSyntax { fromPgAlterTable :: Syntax }
newtype PgAlterTableActionSyntax = PgAlterTableActionSyntax { fromPgAlterTableAction :: Syntax }
newtype PgAlterColumnActionSyntax = PgAlterColumnActionSyntax { fromPgAlterColumnAction :: Syntax }
newtype PgWindowFrameSyntax = PgWindowFrameSyntax { fromPgWindowFrame :: Syntax }
newtype PgWindowFrameBoundsSyntax = PgWindowFrameBoundsSyntax { fromPgWindowFrameBounds :: Syntax }
newtype PgWindowFrameBoundSyntax = PgWindowFrameBoundSyntax { fromPgWindowFrameBound :: ByteString -> Syntax }

-- instance Hashable PgDataTypeSyntax where
--   hashWithSalt salt (PgDataTypeSyntax a _ _) = hashWithSalt salt a
-- instance Eq PgDataTypeSyntax where
--   PgDataTypeSyntax a _ _ == PgDataTypeSyntax b _ _ = a == b

-- instance Eq PgColumnConstraintDefinitionSyntax where
--   PgColumnConstraintDefinitionSyntax a _ ==
--     PgColumnConstraintDefinitionSyntax b _ =
--       a == b

selectCmd :: SelectSyntax -> Command
selectCmd = PgCommandSyntax PgCommandTypeQuery      . coerce

insertCmd :: InsertSyntax -> Command
insertCmd = PgCommandSyntax PgCommandTypeDataUpdate . coerce

deleteCmd :: DeleteSyntax -> Command
deleteCmd = PgCommandSyntax PgCommandTypeDataUpdate . coerce

updateCmd :: UpdateSyntax -> Command
updateCmd = PgCommandSyntax PgCommandTypeDataUpdate . coerce

-- createTableCmd = PgCommandSyntax PgCommandTypeDdl . coerce
-- dropTableCmd   = PgCommandSyntax PgCommandTypeDdl . coerce
-- alterTableCmd  = PgCommandSyntax PgCommandTypeDdl . coerce

updateStmt tbl fields where_ =
  PgUpdateSyntax $
  emit "UPDATE " <> pgQuotedIdentifier tbl <>
  (case fields of
     [] -> mempty
     fields ->
       emit " SET " <>
       pgSepBy (emit ", ") (map (\(field, val) -> fromPgFieldName field <> emit "=" <> fromPgExpression val) fields)) <>
  maybe mempty (\where_ -> emit " WHERE " <> fromPgExpression where_) where_

deleteStmt tbl where_ =
  PgDeleteSyntax $
  emit "DELETE FROM " <> pgQuotedIdentifier tbl <>
  maybe mempty (\where_ -> emit " WHERE " <> fromPgExpression where_) where_

selectStmt tbl ordering limit offset =
  pgSelectStmt tbl ordering limit offset Nothing

selectTableStmt setQuantifier proj from where_ grouping having =
  PgSelectTableSyntax $
  emit "SELECT " <>
  maybe mempty (\setQuantifier' -> fromPgSelectSetQuantifier setQuantifier' <> emit " ") setQuantifier <>
  fromPgProjection proj <>
  (maybe mempty (emit " FROM " <> ) (coerce from)) <>
  (maybe mempty (emit " WHERE " <>) (coerce where_)) <>
  (maybe mempty (emit " GROUP BY " <>) (coerce grouping)) <>
  (maybe mempty (emit " HAVING " <>) (coerce having))

unionTables all = pgTableOp (if all then "UNION ALL" else "UNION")
intersectTables all = pgTableOp (if all then "INTERSECT ALL" else "INTERSECT")
exceptTable all = pgTableOp (if all then "EXCEPT ALL" else "EXCEPT")

groupByExpressions es =
    PgGroupingSyntax $
    pgSepBy (emit ", ") (map fromPgExpression es)

fromTable :: TableSourceSyntax -> Maybe T.Text -> FromSyntax
fromTable tableSrc Nothing = coerce tableSrc
fromTable tableSrc (Just nm) =
    PgFromSyntax $
    coerce tableSrc <> emit " AS " <> pgQuotedIdentifier nm

innerJoin a b Nothing = PgFromSyntax (fromPgFrom a <> emit " CROSS JOIN " <> fromPgFrom b)
innerJoin a b (Just e) = pgJoin "INNER JOIN" a b (Just e)

leftJoin = pgJoin "LEFT JOIN"
rightJoin = pgJoin "RIGHT JOIN"

outerJoin = pgJoin "FULL OUTER JOIN"

ascOrdering e =  e <> emit " ASC"
descOrdering e = e <> emit " DESC"

-- nullsFirstOrdering o = o { pgOrderingNullOrdering = Just PgNullOrderingNullsFirst }
-- nullsLastOrdering o = o { pgOrderingNullOrdering = Just PgNullOrderingNullsLast }

-- domainType nm = PgDataTypeSyntax (PgDataTypeDescrDomain nm) (pgQuotedIdentifier nm)
--                                  (domainType nm)

-- charType prec charSet = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bpchar) (fmap fromIntegral prec))
--                                          (emit "CHAR" <> pgOptPrec prec <> pgOptCharSet charSet)
--                                          (charType prec charSet)
-- varCharType prec charSet = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.varchar) (fmap fromIntegral prec))
--                                             (emit "VARCHAR" <> pgOptPrec prec <> pgOptCharSet charSet)
--                                             (varCharType prec charSet)
-- nationalCharType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bpchar) (fmap fromIntegral prec))
--                                          (emit "NATIONAL CHAR" <> pgOptPrec prec)
--                                          (nationalCharType prec)
-- nationalVarCharType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.varchar) (fmap fromIntegral prec))
--                                             (emit "NATIONAL CHARACTER VARYING" <> pgOptPrec prec)
--                                             (nationalVarCharType prec)

-- bitType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bit) (fmap fromIntegral prec))
--                                 (emit "BIT" <> pgOptPrec prec)
--                                 (bitType prec)
-- varBitType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.varbit) (fmap fromIntegral prec))
--                                    (emit "BIT VARYING" <> pgOptPrec prec)
--                                    (varBitType prec)

-- numericType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.numeric) (mkNumericPrec prec))
--                                     (emit "NUMERIC" <> pgOptNumericPrec prec)
--                                     (numericType prec)
-- decimalType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.numeric) (mkNumericPrec prec))
--                                     (emit "DOUBLE" <> pgOptNumericPrec prec)
--                                     (decimalType prec)

-- intType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.int4) Nothing) (emit "INT") intType
-- smallIntType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.int2) Nothing) (emit "SMALLINT") smallIntType

-- floatType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.float4) Nothing) (emit "FLOAT" <> pgOptPrec prec)
--                                   (floatType prec)
-- doubleType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.float8) Nothing) (emit "DOUBLE PRECISION") doubleType
-- realType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.float4) Nothing) (emit "REAL") realType
-- dateType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.date) Nothing) (emit "DATE") dateType
-- timeType prec withTz = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.time) Nothing)
--                                         (emit "TIME" <> pgOptPrec prec <> if withTz then emit " WITH TIME ZONE" else mempty)
--                                         (timeType prec withTz)
-- timestampType prec withTz = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid (if withTz then Pg.timestamptz else Pg.timestamp)) Nothing)
--                                              (emit "TIMESTAMP" <> pgOptPrec prec <> if withTz then emit " WITH TIME ZONE" else mempty)
--                                              (timestampType prec withTz)

-- characterLargeObjectType = pgTextType { pgDataTypeSerialized = characterLargeObjectType }
-- binaryLargeObjectType = pgByteaType { pgDataTypeSerialized = binaryLargeObjectType }
-- booleanType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bool) Nothing) (emit "BOOLEAN")
--                                booleanType
-- arrayType (PgDataTypeSyntax _ syntax serialized) sz =
--   PgDataTypeSyntax (error "TODO: array migrations")
--                    (syntax <> emit "[" <> emit (fromString (show sz)) <> emit "]")
--                    (arrayType serialized sz)
-- rowType = error "rowType"

-- bigIntType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.int8) Nothing) (emit "BIGINT") bigIntType

-- serializeDataType = fromBeamSerializedDataType . pgDataTypeSerialized

pgOptPrec :: Maybe Word -> Syntax
pgOptPrec Nothing = mempty
pgOptPrec (Just x) = emit "(" <> emit (fromString (show x)) <> emit ")"

pgOptCharSet :: Maybe T.Text -> Syntax
pgOptCharSet Nothing = mempty
pgOptCharSet (Just cs) = emit " CHARACTER SET " <> emit (TE.encodeUtf8 cs)

pgOptNumericPrec :: Maybe (Word, Maybe Word) -> Syntax
pgOptNumericPrec Nothing = mempty
pgOptNumericPrec (Just (prec, Nothing)) = pgOptPrec (Just prec)
pgOptNumericPrec (Just (prec, Just dec)) = emit "(" <> emit (fromString (show prec)) <> emit ", " <> emit (fromString (show dec)) <> emit ")"

-- pgDataTypeJSON :: Value -> BeamSerializedDataType
-- pgDataTypeJSON v = BeamSerializedDataType (beamSerializeJSON "postgres" v)

-- pgByteaType :: PgDataTypeSyntax
-- pgByteaType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bytea) Nothing) (emit "BYTEA")
--                                (pgDataTypeJSON "bytea")

-- pgSmallSerialType, pgSerialType, pgBigSerialType :: PgDataTypeSyntax
-- pgSmallSerialType = PgDataTypeSyntax (pgDataTypeDescr smallIntType) (emit "SMALLSERIAL") (pgDataTypeJSON "smallserial")
-- pgSerialType = PgDataTypeSyntax (pgDataTypeDescr intType) (emit "SERIAL") (pgDataTypeJSON "serial")
-- pgBigSerialType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.int8) Nothing) (emit "BIGSERIAL") (pgDataTypeJSON "bigserial")

-- pgUnboundedArrayType :: PgDataTypeSyntax -> PgDataTypeSyntax
-- pgUnboundedArrayType (PgDataTypeSyntax _ syntax serialized) =
--     PgDataTypeSyntax (error "Can't do array migrations yet")
--                      (syntax <> emit "[]")
--                      (pgDataTypeJSON (object [ "unbounded-array" .= fromBeamSerializedDataType serialized ]))

pgTsQueryTypeInfo :: Pg.TypeInfo
pgTsQueryTypeInfo = Pg.Basic (Pg.Oid 3615) 'U' ',' "tsquery"

-- pgTsQueryType :: PgDataTypeSyntax
-- pgTsQueryType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid pgTsQueryTypeInfo) Nothing)
--                                  (emit "TSQUERY") (pgDataTypeJSON "tsquery")

-- | Postgres TypeInfo for tsvector
-- TODO Is the Oid stable from postgres instance to postgres instance?
pgTsVectorTypeInfo :: Pg.TypeInfo
pgTsVectorTypeInfo = Pg.Basic (Pg.Oid 3614) 'U' ',' "tsvector"

-- pgTsVectorType :: PgDataTypeSyntax
-- pgTsVectorType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid pgTsVectorTypeInfo) Nothing)
--                                   (emit "TSVECTOR")
--                                   (pgDataTypeJSON "tsvector")

-- pgTextType :: PgDataTypeSyntax
-- pgTextType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.text) Nothing) (emit "TEXT")
--                               (pgDataTypeJSON "text")

-- pgJsonType, pgJsonbType :: PgDataTypeSyntax
-- pgJsonType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.json) Nothing) (emit "JSON") (pgDataTypeJSON "json")
-- pgJsonbType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.jsonb) Nothing) (emit "JSONB") (pgDataTypeJSON "jsonb")

-- pgUuidType :: PgDataTypeSyntax
-- pgUuidType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.uuid) Nothing) (emit "UUID") (pgDataTypeJSON "uuid")

-- pgMoneyType :: PgDataTypeSyntax
-- pgMoneyType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.money) Nothing) (emit "MONEY") (pgDataTypeJSON "money")

mkNumericPrec :: Maybe (Word, Maybe Word) -> Maybe Int32
mkNumericPrec Nothing = Nothing
mkNumericPrec (Just (whole, dec)) = Just $ (fromIntegral whole `shiftL` 16) .|. (fromIntegral (fromMaybe 0 dec) .&. 0xFFFF)

newtype CustomSqlSyntax =
  PgCustomExpressionSyntax { fromPgCustomExpression :: Syntax }
  deriving ( Monoid, Semigroup )
customExprSyntax = PgExpressionSyntax . fromPgCustomExpression
renderSyntax = PgCustomExpressionSyntax . pgParens . fromPgExpression

instance IsString CustomSqlSyntax where
  fromString = PgCustomExpressionSyntax . emit . fromString

quantifyOverAll = PgComparisonQuantifierSyntax (emit "ALL")
quantifyOverAny = PgComparisonQuantifierSyntax (emit "ANY")

addE = pgBinOp "+"
subE = pgBinOp "-"
mulE = pgBinOp "*"
divE = pgBinOp "/"
modE = pgBinOp "%"
orE = pgBinOp "OR"
andE = pgBinOp "AND"
likeE = pgBinOp "LIKE"
overlapsE = pgBinOp "OVERLAPS"
eqE = pgCompOp "="
neqE = pgCompOp "<>"
eqMaybeE a b _ = pgBinOp "IS NOT DISTINCT FROM" a b
neqMaybeE a b _ = pgBinOp "IS DISTINCT FROM" a b
ltE = pgCompOp "<"
gtE = pgCompOp ">"
leE = pgCompOp "<="
geE = pgCompOp ">="
negateE = pgUnOp "-"
notE = pgUnOp "NOT"
existsE select = PgExpressionSyntax (emit "EXISTS (" <> fromPgSelect select <> emit ")")
uniqueE select = PgExpressionSyntax (emit "UNIQUE (" <> fromPgSelect select <> emit ")")
isNotNullE = pgPostFix "IS NOT NULL"
isNullE = pgPostFix "IS NULL"
isTrueE = pgPostFix "IS TRUE"
isFalseE = pgPostFix "IS FALSE"
isNotTrueE = pgPostFix "IS NOT TRUE"
isNotFalseE = pgPostFix "IS NOT FALSE"
isUnknownE = pgPostFix "IS UNKNOWN"
isNotUnknownE = pgPostFix "IS NOT UNKNOWN"
betweenE a b c = PgExpressionSyntax (emit "(" <> fromPgExpression a <> emit ") BETWEEN (" <>
                                     fromPgExpression b <> emit ") AND (" <> fromPgExpression c <> emit ")")
valueE = coerce
rowE vs = PgExpressionSyntax $
          emit "(" <>
          pgSepBy (emit ", ") (coerce vs) <>
          emit ")"
quantifierListE vs =
  PgExpressionSyntax $
  emit "(VALUES " <> pgSepBy (emit ", ") (fmap (pgParens . fromPgExpression) vs) <> emit ")"
fieldE :: FieldNameSyntax -> ExpressionSyntax
fieldE = coerce
subqueryE s = PgExpressionSyntax (emit "(" <> fromPgSelect s <> emit ")")
positionE needle haystack =
    PgExpressionSyntax $
    emit "POSITION((" <> fromPgExpression needle <> emit ") IN (" <> fromPgExpression haystack <> emit "))"
nullIfE a b = PgExpressionSyntax (emit "NULLIF(" <> fromPgExpression a <> emit ", " <> fromPgExpression b <> emit ")")
absE x = PgExpressionSyntax (emit "ABS(" <> fromPgExpression x <> emit ")")
bitLengthE x = PgExpressionSyntax (emit "BIT_LENGTH(" <> fromPgExpression x <> emit ")")
charLengthE x = PgExpressionSyntax (emit "CHAR_LENGTH(" <> fromPgExpression x <> emit ")")
octetLengthE x = PgExpressionSyntax (emit "OCTET_LENGTH(" <> fromPgExpression x <> emit ")")
lowerE x = PgExpressionSyntax (emit "LOWER(" <> fromPgExpression x <> emit ")")
upperE x = PgExpressionSyntax (emit "UPPER(" <> fromPgExpression x <> emit ")")
trimE x = PgExpressionSyntax (emit "TRIM(" <> fromPgExpression x <> emit ")")
coalesceE es = PgExpressionSyntax (emit "COALESCE(" <> pgSepBy (emit ", ") (map fromPgExpression es) <> emit ")")
extractE field from = PgExpressionSyntax (emit "EXTRACT(" <> fromPgExtractField field <> emit " FROM (" <> fromPgExpression from <> emit "))")
-- castE e to = PgExpressionSyntax (emit "CAST((" <> fromPgExpression e <> emit ") AS " <> fromPgDataType to <> emit ")")
caseE cases else_ =
    PgExpressionSyntax $
    emit "CASE " <>
    foldMap (\(cond, res) -> emit "WHEN " <> fromPgExpression cond <> emit " THEN " <> fromPgExpression res <> emit " ") cases <>
    emit "ELSE " <> fromPgExpression else_ <> emit " END"

currentTimestampE = PgExpressionSyntax $ emit "CURRENT_TIMESTAMP"

defaultE = PgExpressionSyntax $ emit "DEFAULT"

inE e es = PgExpressionSyntax $ pgParens (fromPgExpression e) <> emit " IN " <>
                                pgParens (pgSepBy (emit ", ") (map fromPgExpression es))

-- instance IsSqlExpressionSyntaxStringType PgExpressionSyntax String
-- instance IsSqlExpressionSyntaxStringType PgExpressionSyntax T.Text

distinctE select = PgExpressionSyntax (emit "DISTINCT (" <> fromPgSelect select <> emit ")")
similarToE = pgBinOp "SIMILAR TO"

functionCallE name args =
  PgExpressionSyntax $
  fromPgExpression name <>
  pgParens (pgSepBy (emit ", ") (map fromPgExpression args))

instanceFieldE i nm =
  PgExpressionSyntax $
  pgParens (fromPgExpression i) <> emit "." <> escapeIdentifier (TE.encodeUtf8 nm)

refFieldE i nm =
  PgExpressionSyntax $
  pgParens (fromPgExpression i) <> emit "->" <> escapeIdentifier (TE.encodeUtf8 nm)

concatE [] = valueE (sqlValueSyntax ("" :: T.Text))
concatE [x] = x
concatE es =
  PgExpressionSyntax $
  emit "CONCAT" <> pgParens (pgSepBy (emit ", ") (map fromPgExpression es))

overE expr frame =
  PgExpressionSyntax $
  fromPgExpression expr <> emit " " <> fromPgWindowFrame frame

lnE    x = PgExpressionSyntax (emit "LN("    <> fromPgExpression x <> emit ")")
expE   x = PgExpressionSyntax (emit "EXP("   <> fromPgExpression x <> emit ")")
sqrtE  x = PgExpressionSyntax (emit "SQRT("  <> fromPgExpression x <> emit ")")
ceilE  x = PgExpressionSyntax (emit "CEIL("  <> fromPgExpression x <> emit ")")
floorE x = PgExpressionSyntax (emit "FLOOR(" <> fromPgExpression x <> emit ")")
powerE x y = PgExpressionSyntax (emit "POWER(" <> fromPgExpression x <> emit ", " <> fromPgExpression y <> emit ")")

denseRankAggE = PgExpressionSyntax $ emit "DENSE_RANK()"
percentRankAggE = PgExpressionSyntax $ emit "PERCENT_RANK()"
cumeDistAggE = PgExpressionSyntax $ emit "CUME_DIST()"

rankAggE = PgExpressionSyntax $ emit "RANK()"
filterAggE agg filter =
  PgExpressionSyntax $
  fromPgExpression agg <> emit " FILTER (WHERE " <> fromPgExpression filter <> emit ")"

stddevPopE = pgUnAgg "STDDEV_POP"
stddevSampE = pgUnAgg "STDDEV_SAMP"
varPopE = pgUnAgg "VAR_POP"
varSampE = pgUnAgg "VAR_SAMP"

covarPopE = pgBinAgg "COVAR_POP"
covarSampE = pgBinAgg "COVAR_SAMP"
corrE = pgBinAgg "CORR"
regrSlopeE = pgBinAgg "REGR_SLOPE"
regrInterceptE = pgBinAgg "REGR_INTERCEPT"
regrCountE = pgBinAgg "REGR_COUNT"
regrRSquaredE = pgBinAgg "REGR_R2"
regrAvgXE = pgBinAgg "REGR_AVGX"
regrAvgYE = pgBinAgg "REGR_AVGY"
regrSXXE = pgBinAgg "REGR_SXX"
regrSYYE = pgBinAgg "REGR_SYY"
regrSXYE = pgBinAgg "REGR_SXY"

ntileE x = PgExpressionSyntax (emit "NTILE(" <> fromPgExpression x <> emit ")")

leadE x Nothing Nothing =
  PgExpressionSyntax (emit "LEAD(" <> fromPgExpression x <> emit ")")
leadE x (Just n) Nothing =
  PgExpressionSyntax (emit "LEAD(" <> fromPgExpression x <> emit ", " <> fromPgExpression n <> emit ")")
leadE x (Just n) (Just def) =
  PgExpressionSyntax (emit "LEAD(" <> fromPgExpression x <> emit ", " <> fromPgExpression n <> emit ", " <> fromPgExpression def <> emit ")")
leadE x Nothing (Just def) =
  PgExpressionSyntax (emit "LEAD(" <> fromPgExpression x <> emit ", 1, " <> fromPgExpression def <> emit ")")

lagE x Nothing Nothing =
  PgExpressionSyntax (emit "LAG(" <> fromPgExpression x <> emit ")")
lagE x (Just n) Nothing =
  PgExpressionSyntax (emit "LAG(" <> fromPgExpression x <> emit ", " <> fromPgExpression n <> emit ")")
lagE x (Just n) (Just def) =
  PgExpressionSyntax (emit "LAG(" <> fromPgExpression x <> emit ", " <> fromPgExpression n <> emit ", " <> fromPgExpression def <> emit ")")
lagE x Nothing (Just def) =
  PgExpressionSyntax (emit "LAG(" <> fromPgExpression x <> emit ", 1, " <> fromPgExpression def <> emit ")")

firstValueE x = PgExpressionSyntax (emit "FIRST_VALUE(" <> fromPgExpression x <> emit ")")
lastValueE x = PgExpressionSyntax (emit "LAST_VALUE(" <> fromPgExpression x <> emit ")")

nthValueE x n = PgExpressionSyntax (emit "NTH_VALUE(" <> fromPgExpression x <> emit ", " <> fromPgExpression n <> emit ")")

frameSyntax partition_ ordering_ bounds_ =
  PgWindowFrameSyntax $
  emit "OVER " <>
  pgParens
  (
    maybe mempty (\p -> emit "PARTITION BY " <> pgSepBy (emit ", ") (map fromPgExpression p)) partition_ <>
    maybe mempty (\o -> emit " ORDER BY " <> pgSepBy (emit ", ") (map fromPgOrdering o)) ordering_ <>
    maybe mempty (\b -> emit " ROWS " <> fromPgWindowFrameBounds b) bounds_
  )

fromToBoundSyntax from Nothing =
  PgWindowFrameBoundsSyntax (fromPgWindowFrameBound from "PRECEDING")
fromToBoundSyntax from (Just to) =
  PgWindowFrameBoundsSyntax $
  emit "BETWEEN " <> fromPgWindowFrameBound from "PRECEDING" <> emit " AND " <> fromPgWindowFrameBound to "FOLLOWING"

unboundedSyntax = PgWindowFrameBoundSyntax $ \where_ -> emit "UNBOUNDED " <> emit where_
nrowsBoundSyntax 0 = PgWindowFrameBoundSyntax $ \_ -> emit "CURRENT ROW"
nrowsBoundSyntax n = PgWindowFrameBoundSyntax $ \where_ -> emit (fromString (show n)) <> emit " " <> emit where_

countAllE = PgExpressionSyntax (emit "COUNT(*)")
countE = pgUnAgg "COUNT"
avgE = pgUnAgg "AVG"
sumE = pgUnAgg "SUM"
minE = pgUnAgg "MIN"
maxE = pgUnAgg "MAX"

everyE = pgUnAgg "EVERY"

-- According to the note at <https://www.postgresql.org/docs/9.2/static/functions-aggregate.html>
-- the following functions are equivalent.
someE = pgUnAgg "BOOL_ANY"
anyE = pgUnAgg "BOOL_ANY"

setQuantifierDistinct = PgAggregationSetQuantifierSyntax $ emit "DISTINCT"
setQuantifierAll = PgAggregationSetQuantifierSyntax $ emit "ALL"

pgSelectSetQuantifierDistinctOn :: [ExpressionSyntax] -> SelectSetQuantifierSyntax
pgSelectSetQuantifierDistinctOn exprs =
  PgSelectSetQuantifierSyntax $
  emit "DISTINCT ON " <> pgParens (pgSepBy (emit ", ") (fromPgExpression <$> exprs))

pgUnAgg :: ByteString -> Maybe AggregationSetQuantifierSyntax -> ExpressionSyntax -> ExpressionSyntax
pgUnAgg fn q e =
  PgExpressionSyntax $
  emit fn <> emit "(" <> maybe mempty (\q -> fromPgAggregationSetQuantifier q <> emit " ") q <> fromPgExpression e <> emit ")"

pgBinAgg :: ByteString -> Maybe AggregationSetQuantifierSyntax -> ExpressionSyntax -> ExpressionSyntax
         -> ExpressionSyntax
pgBinAgg fn q x y =
  PgExpressionSyntax $
  emit fn <> emit "(" <> maybe mempty (\q -> fromPgAggregationSetQuantifier q <> emit " ") q
          <> fromPgExpression x <> emit ", " <> fromPgExpression y <> emit ")"

qualifiedField a b =
  PgFieldNameSyntax $
  pgQuotedIdentifier a <> emit "." <> pgQuotedIdentifier b
unqualifiedField = PgFieldNameSyntax . pgQuotedIdentifier

tableNamed = PgTableSourceSyntax . pgQuotedIdentifier
tableFromSubSelect s = PgTableSourceSyntax $ emit "(" <> fromPgSelect s <> emit ")"

projExprs exprs =
  PgProjectionSyntax $
  pgSepBy (emit ", ")
          (map (\(expr, nm) -> fromPgExpression expr <>
                               maybe mempty (\nm -> emit " AS " <> pgQuotedIdentifier nm) nm) exprs)

insertStmt tblName fields values =
    PgInsertSyntax $
    emit "INSERT INTO " <> pgQuotedIdentifier tblName <> emit "(" <>
    pgSepBy (emit ", ") (map pgQuotedIdentifier fields) <>
    emit ") " <> fromPgInsertValues values

insertSqlExpressions es =
    PgInsertValuesSyntax $
    emit "VALUES " <>
    pgSepBy (emit ", ")
            (map (\es -> emit "(" <> pgSepBy (emit ", ") (coerce es) <> emit ")")
                 es)
insertFromSql (PgSelectSyntax a) = PgInsertValuesSyntax a

-- insertDefaults :: SqlInsertValues tbl
-- insertDefaults = SqlInsertValues (PgInsertValuesSyntax (emit "DEFAULT VALUES"))

dropTableSyntax tblNm =
  PgDropTableSyntax $
  emit "DROP TABLE " <> pgQuotedIdentifier tblNm

alterTableSyntax tblNm action =
  PgAlterTableSyntax $
  emit "ALTER TABLE " <> pgQuotedIdentifier tblNm <> emit " " <> fromPgAlterTableAction action

alterColumnSyntax colNm action =
  PgAlterTableActionSyntax $
  emit "ALTER COLUMN " <> pgQuotedIdentifier colNm <> emit " " <> fromPgAlterColumnAction action

-- addColumnSyntax colNm schema =
--   PgAlterTableActionSyntax $
--   emit "ADD COLUMN " <> pgQuotedIdentifier colNm <> emit " " <> fromPgColumnSchema schema

dropColumnSyntax colNm =
  PgAlterTableActionSyntax $
  emit "DROP COLUMN " <> pgQuotedIdentifier colNm

renameTableToSyntax newNm =
  PgAlterTableActionSyntax $
  emit "RENAME TO " <> pgQuotedIdentifier newNm

renameColumnToSyntax oldNm newNm =
  PgAlterTableActionSyntax $
  emit "RENAME COLUMN " <> pgQuotedIdentifier oldNm <> emit " TO " <> pgQuotedIdentifier newNm

setNullSyntax = PgAlterColumnActionSyntax (emit "DROP NOT NULL")
setNotNullSyntax = PgAlterColumnActionSyntax (emit "SET NOT NULL")

-- createTableSyntax options tblNm fieldTypes constraints =
--   let (beforeOptions, afterOptions) =
--         case options of
--           Nothing -> (emit " ", emit " ")
--           Just (PgTableOptionsSyntax before after) ->
--             ( emit " " <> before <> emit " "
--             , emit " " <> after <> emit " " )
--   in PgCreateTableSyntax $
--      emit "CREATE" <> beforeOptions <> emit "TABLE " <> pgQuotedIdentifier tblNm <>
--      emit " (" <>
--      pgSepBy (emit ", ")
--              (map (\(nm, type_) -> pgQuotedIdentifier nm <> emit " " <> fromPgColumnSchema type_)  fieldTypes <>
--               map fromPgTableConstraint constraints)
--      <> emit ")" <> afterOptions

primaryKeyConstraintSyntax fieldNames =
  PgTableConstraintSyntax $
  emit "PRIMARY KEY(" <> pgSepBy (emit ", ") (map pgQuotedIdentifier fieldNames) <> emit ")"

-- instance Hashable ColumnSchemaSyntax where
--   hashWithSalt salt = hashWithSalt salt . fromPgColumnSchema
-- columnSchemaSyntax colType defaultClause constraints collation =
--   PgColumnSchemaSyntax syntax
--   where
--     syntax =
--       fromPgDataType colType <>
--       maybe mempty (\d -> emit " DEFAULT " <> fromPgExpression d) defaultClause <>
--       (case constraints of
--          [] -> mempty
--          _ -> foldMap (\c -> emit " " <> fromPgColumnConstraintDefinition c) constraints) <>
--       maybe mempty (\nm -> emit " COLLATE " <> pgQuotedIdentifier nm) collation

-- fullMatchSyntax = PgMatchTypeSyntax (emit "FULL") fullMatchSyntax
-- partialMatchSyntax = PgMatchTypeSyntax (emit "PARTIAL") partialMatchSyntax

-- pgMatchTypeJSON :: Value -> BeamSerializedMatchType
-- pgMatchTypeJSON v = BeamSerializedMatchType (beamSerializeJSON "postgres" v)

-- pgSimpleMatchSyntax :: PgMatchTypeSyntax
-- pgSimpleMatchSyntax = PgMatchTypeSyntax (emit "SIMPLE") (pgMatchTypeJSON "simple")

-- referentialActionCascadeSyntax = PgReferentialActionSyntax (emit "CASCADE") referentialActionCascadeSyntax
-- referentialActionNoActionSyntax = PgReferentialActionSyntax (emit "NO ACTION") referentialActionNoActionSyntax
-- referentialActionSetDefaultSyntax = PgReferentialActionSyntax (emit "SET DEFAULT") referentialActionSetDefaultSyntax
-- referentialActionSetNullSyntax = PgReferentialActionSyntax (emit "SET NULL") referentialActionSetNullSyntax

-- fromSqlConstraintAttributes :: SqlConstraintAttributesBuilder -> PgSyntax
-- fromSqlConstraintAttributes (SqlConstraintAttributesBuilder timing deferrable) =
--   maybe mempty timingBuilder timing <> maybe mempty deferrableBuilder deferrable
--   where timingBuilder InitiallyDeferred = emit "INITIALLY DEFERRED"
--         timingBuilder InitiallyImmediate = emit "INITIALLY IMMEDIATE"
--         deferrableBuilder False = emit "NOT DEFERRABLE"
--         deferrableBuilder True = emit "DEFERRABLE"

-- instance Hashable PgColumnConstraintDefinitionSyntax where
--   hashWithSalt salt = hashWithSalt salt . fromPgColumnConstraintDefinition

-- constraintDefinitionSyntax nm constraint attrs =
--   PgColumnConstraintDefinitionSyntax syntax
--     (constraintDefinitionSyntax nm (pgColumnConstraintSerialized constraint) (fmap sqlConstraintAttributesSerialized attrs))
--   where
--     syntax =
--       maybe mempty (\nm -> emit "CONSTRAINT " <> pgQuotedIdentifier nm <> emit " " ) nm <>
--       fromPgColumnConstraint constraint <>
--       maybe mempty (\a -> emit " " <> fromSqlConstraintAttributes a) attrs

-- serializeConstraint = fromBeamSerializedConstraintDefinition . pgColumnConstraintDefinitionSerialized

-- notNullConstraintSyntax = PgColumnConstraintSyntax (emit "NOT NULL") notNullConstraintSyntax
-- uniqueColumnConstraintSyntax = PgColumnConstraintSyntax (emit "UNIQUE") uniqueColumnConstraintSyntax
-- primaryKeyColumnConstraintSyntax = PgColumnConstraintSyntax (emit "PRIMARY KEY") primaryKeyColumnConstraintSyntax
-- checkColumnConstraintSyntax expr =
--   PgColumnConstraintSyntax (emit "CHECK(" <> fromPgExpression expr <> emit ")")
--                            (checkColumnConstraintSyntax . BeamSerializedExpression . TE.decodeUtf8 .
--                             toStrict . pgRenderSyntaxScript . fromPgExpression $ expr)
-- referencesConstraintSyntax tbl fields matchType onUpdate onDelete =
--   PgColumnConstraintSyntax syntax
--     (referencesConstraintSyntax tbl fields (fmap pgMatchTypeSerialized matchType)
--                                 (fmap pgReferentialActionSerialized onUpdate)
--                                 (fmap pgReferentialActionSerialized onDelete))
--   where
--     syntax =
--       emit "REFERENCES " <> pgQuotedIdentifier tbl <> emit "("
--       <> pgSepBy (emit ", ") (map pgQuotedIdentifier fields) <> emit ")" <>
--       maybe mempty (\m -> emit " " <> fromPgMatchType m) matchType <>
--       maybe mempty (\a -> emit " ON UPDATE " <> fromPgReferentialAction a) onUpdate <>
--       maybe mempty (\a -> emit " ON DELETE " <> fromPgReferentialAction a) onDelete

defaultPgValueSyntax :: Pg.ToField a => a -> ValueSyntax
defaultPgValueSyntax =
    PgValueSyntax . pgBuildAction . pure . Pg.toField

#define DEFAULT_SQL_SYNTAX(ty)                                  \
           instance HasSqlValueSyntax ty where    \
             sqlValueSyntax = defaultPgValueSyntax

DEFAULT_SQL_SYNTAX(Bool)
DEFAULT_SQL_SYNTAX(Double)
DEFAULT_SQL_SYNTAX(Float)
DEFAULT_SQL_SYNTAX(Int)
DEFAULT_SQL_SYNTAX(Int8)
DEFAULT_SQL_SYNTAX(Int16)
DEFAULT_SQL_SYNTAX(Int32)
DEFAULT_SQL_SYNTAX(Int64)
DEFAULT_SQL_SYNTAX(Integer)
DEFAULT_SQL_SYNTAX(Word)
DEFAULT_SQL_SYNTAX(Word8)
DEFAULT_SQL_SYNTAX(Word16)
DEFAULT_SQL_SYNTAX(Word32)
DEFAULT_SQL_SYNTAX(Word64)
DEFAULT_SQL_SYNTAX(T.Text)
DEFAULT_SQL_SYNTAX(TL.Text)
DEFAULT_SQL_SYNTAX(UTCTime)
DEFAULT_SQL_SYNTAX(Value)
DEFAULT_SQL_SYNTAX(Pg.Oid)
DEFAULT_SQL_SYNTAX(LocalTime)
DEFAULT_SQL_SYNTAX(ZonedTime)
DEFAULT_SQL_SYNTAX(TimeOfDay)
DEFAULT_SQL_SYNTAX(NominalDiffTime)
DEFAULT_SQL_SYNTAX(Day)
DEFAULT_SQL_SYNTAX(UUID)
DEFAULT_SQL_SYNTAX([Char])
DEFAULT_SQL_SYNTAX(Pg.HStoreMap)
DEFAULT_SQL_SYNTAX(Pg.HStoreList)
DEFAULT_SQL_SYNTAX(Pg.HStoreBuilder)
DEFAULT_SQL_SYNTAX(Pg.Date)
DEFAULT_SQL_SYNTAX(Pg.ZonedTimestamp)
DEFAULT_SQL_SYNTAX(Pg.LocalTimestamp)
DEFAULT_SQL_SYNTAX(Pg.UTCTimestamp)
DEFAULT_SQL_SYNTAX(Scientific)

class HasSqlValueSyntax a where
  sqlValueSyntax :: a -> ValueSyntax

instance HasSqlValueSyntax SqlNull where
  sqlValueSyntax _ = defaultPgValueSyntax Pg.Null

instance HasSqlValueSyntax x => HasSqlValueSyntax (Maybe x) where
  sqlValueSyntax Nothing = sqlValueSyntax SqlNull
  sqlValueSyntax (Just x) = sqlValueSyntax x

instance HasSqlValueSyntax B.ByteString where
  sqlValueSyntax = defaultPgValueSyntax . Pg.Binary

instance HasSqlValueSyntax BL.ByteString where
  sqlValueSyntax = defaultPgValueSyntax . Pg.Binary

pgQuotedIdentifier :: T.Text -> Syntax
pgQuotedIdentifier t =
  escapeIdentifier (TE.encodeUtf8 t)

pgParens :: Syntax -> Syntax
pgParens a = emit "(" <> a <> emit ")"

pgTableOp :: ByteString -> SelectTableSyntax -> SelectTableSyntax
          -> SelectTableSyntax
pgTableOp op tbl1 tbl2 =
    PgSelectTableSyntax $
    emit "(" <> fromPgSelectTable tbl1 <> emit ") " <> emit op <>
    emit " (" <> fromPgSelectTable tbl2 <> emit ")"

pgCompOp :: ByteString -> Maybe ComparisonQuantifierSyntax
         -> ExpressionSyntax -> ExpressionSyntax -> ExpressionSyntax
pgCompOp op quantifier a b =
  PgExpressionSyntax $
  emit "(" <> fromPgExpression a <>
  emit (") " <> op) <>
  maybe (emit " (" <> fromPgExpression b <> emit ")")
        (\q -> emit " " <> fromPgComparisonQuantifier q <> emit " " <> fromPgExpression b)
        quantifier

pgBinOp :: ByteString -> ExpressionSyntax -> ExpressionSyntax -> ExpressionSyntax
pgBinOp op a b =
  PgExpressionSyntax $
  emit "(" <> fromPgExpression a <> emit (") " <> op <> " (") <> fromPgExpression b <> emit ")"

pgPostFix, pgUnOp :: ByteString -> ExpressionSyntax -> ExpressionSyntax
pgPostFix op a =
  PgExpressionSyntax $
  emit "(" <> fromPgExpression a <> emit ") " <> emit op
pgUnOp op a =
  PgExpressionSyntax $
  emit (op <> "(") <> fromPgExpression a <> emit ")"

pgJoin :: ByteString -> FromSyntax -> FromSyntax -> Maybe ExpressionSyntax -> FromSyntax
pgJoin joinType a b Nothing =
  PgFromSyntax $
  fromPgFrom a <> emit (" " <> joinType <> " ") <> fromPgFrom b <> emit " ON TRUE"
pgJoin joinType a b (Just on) =
  PgFromSyntax $
  fromPgFrom a <> emit (" " <> joinType <> " ") <> fromPgFrom b <>
  emit " ON " <> fromPgExpression on

pgSepBy :: Syntax -> [Syntax] -> Syntax
pgSepBy _ [] = mempty
pgSepBy _ [x] = x
pgSepBy sep (x:xs) = x <> sep <> pgSepBy sep xs

pgDebugRenderSyntax :: Syntax -> IO ()
pgDebugRenderSyntax (PgSyntax p) = go p Nothing
  where go :: PgSyntaxM () -> Maybe (PgSyntaxF ()) -> IO ()
        go p = runF p finish step
        step x lastBs =
          case (x, lastBs) of
            (EmitBuilder s next, lastBs) ->
              step (EmitByteString (toStrict (toLazyByteString s)) next) lastBs
            (x, Nothing) ->
              nextSyntaxStep x (Just (fmap (const ()) x))
            (EmitByteString x next, Just (EmitByteString before _)) ->
              next (Just (EmitByteString (before <> x) ()))
            (EscapeString x next, Just (EscapeString before _)) ->
              next (Just (EscapeString (before <> x) ()))
            (EscapeBytea x next, Just (EscapeBytea before _)) ->
              next (Just (EscapeBytea (before <> x) ()))
            (EscapeIdentifier x next, Just (EscapeIdentifier before _)) ->
              next (Just (EscapeIdentifier (before <> x) ()))
            (s, Just e) ->
              renderStep e >>
              nextSyntaxStep s (Just (fmap (const ()) s))

        renderStep (EmitByteString x _) = putStrLn ("EmitByteString " <> show x)
        renderStep (EmitBuilder x _) = putStrLn ("EmitBuilder " <> show (toLazyByteString x))
        renderStep (EscapeString x _) = putStrLn ("EscapeString " <> show x)
        renderStep (EscapeBytea x _) = putStrLn ("EscapeBytea " <> show x)
        renderStep (EscapeIdentifier x _) = putStrLn ("EscapeIdentifier " <> show x)

        finish x Nothing = pure x
        finish x (Just s) = renderStep s >> pure x

pgBuildAction :: [ Pg.Action ] -> Syntax
pgBuildAction =
  foldMap $ \action ->
  case action of
    Pg.Plain x -> emitBuilder x
    Pg.Escape str -> emit "'" <> escapeString str <> emit "'"
    Pg.EscapeByteA bin -> emit "'" <> escapeBytea bin <> emit "'"
    Pg.EscapeIdentifier id -> escapeIdentifier id
    Pg.Many as -> pgBuildAction as

-- * Postgres-specific extensions

-- * Postgres specific commands

pgSelectStmt :: SelectTableSyntax
             -> [ExpressionSyntax]
             -> Maybe Integer {-^ LIMIT -}
             -> Maybe Integer {-^ OFFSET -}
             -> Maybe PgSelectLockingClauseSyntax
             -> SelectSyntax
pgSelectStmt tbl ordering limit offset locking =
    PgSelectSyntax $
    mconcat [ coerce tbl
            , case ordering of
                [] -> mempty
                ordering -> emit " ORDER BY " <> pgSepBy (emit ", ") (map fromPgOrdering ordering)
            , maybe mempty (emit . fromString . (" LIMIT " <>) . show) limit
            , maybe mempty (emit . fromString . (" OFFSET " <>) . show) offset
            , maybe mempty fromPgSelectLockingClause locking ]

-- pgCreateExtensionSyntax :: T.Text -> PgCommandSyntax
-- pgCreateExtensionSyntax extName =
--   PgCommandSyntax PgCommandTypeDdl $ emit "CREATE EXTENSION " <> pgQuotedIdentifier extName

-- pgDropExtensionSyntax :: T.Text -> PgCommandSyntax
-- pgDropExtensionSyntax extName =
--   PgCommandSyntax PgCommandTypeDdl $ emit "DROP EXTENSION " <> pgQuotedIdentifier extName

-- -- * Pg-specific Q monad


data PgEscapeType = PgEscapeString | PgEscapeBytea | PgEscapeIdentifier
  deriving (Show, Eq, Ord, Enum, Bounded)
data PgSyntaxPrim = PgSyntaxPrim (Maybe PgEscapeType) BL.ByteString deriving Show

instance IsString PgSyntaxPrim where
  fromString = PgSyntaxPrim Nothing . fromString

pgTestSyntax :: Syntax -> [ PgSyntaxPrim ]
pgTestSyntax (PgSyntax syntax) = runF syntax finish step Nothing mempty id
  where
    finish _ escapeType curBuilder a =
      let chunk = toLazyByteString curBuilder
      in if BL.null chunk then a []
         else a [ PgSyntaxPrim escapeType chunk ]

    go next curType nextType curBuilder nextBuilder a
      | curType == nextType = next curType (curBuilder <> nextBuilder) a
      | otherwise = next nextType mempty (a . (PgSyntaxPrim curType (toLazyByteString curBuilder):))

    step (EmitByteString bs next) curType curBuilder a =
      go next curType Nothing curBuilder (byteString bs) a
    step (EmitBuilder bs next) curType curBuilder a =
      go next curType Nothing curBuilder bs a
    step (EscapeString s next) curType curBuilder a =
      go next curType (Just PgEscapeString) curBuilder (byteString s) a
    step (EscapeBytea s next) curType curBuilder a =
      go next curType (Just PgEscapeBytea) curBuilder (byteString s) a
    step (EscapeIdentifier s next) curType curBuilder a =
      go next curType (Just PgEscapeIdentifier) curBuilder (byteString s) a

pgRenderSyntaxScript :: Syntax -> BL.ByteString
pgRenderSyntaxScript (PgSyntax mkQuery) =
  toLazyByteString (runF mkQuery finish step)
  where
    finish _ = mempty
    step (EmitBuilder b next) = b <> next
    step (EmitByteString b next) = byteString b <> next
    step (EscapeString b next) = escapePgString b <> next
    step (EscapeBytea b next) = escapePgBytea b <> next
    step (EscapeIdentifier b next) = escapePgIdentifier b <> next

    escapePgString b = byteString (B.concatMap (\w -> if w == '\'' then "''" else B.singleton w) b)
    escapePgBytea _ = error "escapePgBytea: no connection"
    escapePgIdentifier bs = char8 '"' <> foldMap quoteIdentifierChar (B.unpack bs) <> char8 '"'
      where
        quoteIdentifierChar '"' = char8 '"' <> char8 '"'
        quoteIdentifierChar c = char8 c


-- * Instances for 'HasDefaultSqlDataType'

-- instance HasDefaultSqlDataType PgDataTypeSyntax ByteString where
--   defaultSqlDataType _ _ = pgByteaType
-- instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax ByteString

-- instance HasDefaultSqlDataType PgDataTypeSyntax LocalTime where
--   defaultSqlDataType _ _ = timestampType Nothing False
-- instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax LocalTime

-- instance HasDefaultSqlDataType PgDataTypeSyntax (SqlSerial Int) where
--   defaultSqlDataType _ False = pgSerialType
--   defaultSqlDataType _ _ = intType
-- instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax (SqlSerial Int)

-- instance HasDefaultSqlDataType PgDataTypeSyntax UUID where
--   defaultSqlDataType _ _ = pgUuidType
-- instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax UUID

-- * Instances for 'HasSqlEqualityCheck'

#define PG_HAS_EQUALITY_CHECK(ty)                                 \
  instance HasSqlEqualityCheck (ty);           \
  instance HasSqlQuantifiedEqualityCheck (ty);

PG_HAS_EQUALITY_CHECK(Bool)
PG_HAS_EQUALITY_CHECK(Double)
PG_HAS_EQUALITY_CHECK(Float)
PG_HAS_EQUALITY_CHECK(Int)
PG_HAS_EQUALITY_CHECK(Int8)
PG_HAS_EQUALITY_CHECK(Int16)
PG_HAS_EQUALITY_CHECK(Int32)
PG_HAS_EQUALITY_CHECK(Int64)
PG_HAS_EQUALITY_CHECK(Integer)
PG_HAS_EQUALITY_CHECK(Word)
PG_HAS_EQUALITY_CHECK(Word8)
PG_HAS_EQUALITY_CHECK(Word16)
PG_HAS_EQUALITY_CHECK(Word32)
PG_HAS_EQUALITY_CHECK(Word64)
PG_HAS_EQUALITY_CHECK(T.Text)
PG_HAS_EQUALITY_CHECK(TL.Text)
PG_HAS_EQUALITY_CHECK(UTCTime)
PG_HAS_EQUALITY_CHECK(Value)
PG_HAS_EQUALITY_CHECK(Pg.Oid)
PG_HAS_EQUALITY_CHECK(LocalTime)
PG_HAS_EQUALITY_CHECK(ZonedTime)
PG_HAS_EQUALITY_CHECK(TimeOfDay)
PG_HAS_EQUALITY_CHECK(NominalDiffTime)
PG_HAS_EQUALITY_CHECK(Day)
PG_HAS_EQUALITY_CHECK(UUID)
PG_HAS_EQUALITY_CHECK([Char])
PG_HAS_EQUALITY_CHECK(Pg.HStoreMap)
PG_HAS_EQUALITY_CHECK(Pg.HStoreList)
PG_HAS_EQUALITY_CHECK(Pg.Date)
PG_HAS_EQUALITY_CHECK(Pg.ZonedTimestamp)
PG_HAS_EQUALITY_CHECK(Pg.LocalTimestamp)
PG_HAS_EQUALITY_CHECK(Pg.UTCTimestamp)
PG_HAS_EQUALITY_CHECK(Scientific)
PG_HAS_EQUALITY_CHECK(ByteString)
PG_HAS_EQUALITY_CHECK(BL.ByteString)
PG_HAS_EQUALITY_CHECK(V.Vector a)
