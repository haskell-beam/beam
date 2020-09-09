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
{-# LANGUAGE UndecidableInstances #-}

-- | Data types for Postgres syntax. Access is given mainly for extension
-- modules. The types and definitions here are likely to change.
module Database.Beam.Postgres.Syntax
    ( PgSyntaxF(..), PgSyntaxM
    , PgSyntax(..)

    , emit, emitBuilder, escapeString
    , escapeBytea, escapeIdentifier
    , pgParens

    , nextSyntaxStep

    , PgCommandSyntax(..), PgCommandType(..)
    , PgSelectSyntax(..), PgSelectSetQuantifierSyntax(..)
    , PgInsertSyntax(..)
    , PgDeleteSyntax(..)
    , PgUpdateSyntax(..)

    , PgExpressionSyntax(..), PgFromSyntax(..), PgTableNameSyntax(..)
    , PgComparisonQuantifierSyntax(..)
    , PgExtractFieldSyntax(..)
    , PgProjectionSyntax(..), PgGroupingSyntax(..)
    , PgOrderingSyntax(..), PgValueSyntax(..)
    , PgTableSourceSyntax(..), PgFieldNameSyntax(..)
    , PgAggregationSetQuantifierSyntax(..)
    , PgInsertValuesSyntax(..), PgInsertOnConflictSyntax(..)
    , PgInsertOnConflictTargetSyntax(..), PgConflictActionSyntax(..)
    , PgCreateTableSyntax(..), PgTableOptionsSyntax(..), PgColumnSchemaSyntax(..)
    , PgDataTypeSyntax(..), PgColumnConstraintDefinitionSyntax(..), PgColumnConstraintSyntax(..)
    , PgTableConstraintSyntax(..), PgMatchTypeSyntax(..), PgReferentialActionSyntax(..)

    , PgAlterTableSyntax(..), PgAlterTableActionSyntax(..), PgAlterColumnActionSyntax(..)

    , PgWindowFrameSyntax(..), PgWindowFrameBoundsSyntax(..), PgWindowFrameBoundSyntax(..)

    , PgSelectLockingClauseSyntax(..)
    , PgSelectLockingStrength(..)
    , PgSelectLockingOptions(..)
    , fromPgSelectLockingClause
    , pgSelectStmt
    , defaultPgValueSyntax

    , PgDataTypeDescr(..)
    , PgHasEnum(..)

    , pgCreateExtensionSyntax, pgDropExtensionSyntax
    , pgCreateEnumSyntax, pgDropTypeSyntax

    , pgSimpleMatchSyntax

    , pgSelectSetQuantifierDistinctOn

    , pgDataTypeJSON

    , pgTsQueryType, pgTsVectorType
    , pgJsonType, pgJsonbType, pgUuidType
    , pgMoneyType
    , pgTsQueryTypeInfo, pgTsVectorTypeInfo

    , pgByteaType, pgTextType, pgUnboundedArrayType
    , pgSerialType, pgSmallSerialType, pgBigSerialType

    , pgPointType, pgLineType, pgLineSegmentType, pgBoxType

    , pgQuotedIdentifier, pgSepBy, pgDebugRenderSyntax
    , pgRenderSyntaxScript, pgBuildAction

    , pgBinOp, pgCompOp, pgUnOp, pgPostFix

    , pgTestSyntax

    , PostgresInaccessible ) where

import           Database.Beam hiding (insert)
import           Database.Beam.Backend.Internal.Compat
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate
import           Database.Beam.Migrate.SQL.Builder hiding (fromSqlConstraintAttributes)
import           Database.Beam.Migrate.Serialization

import           Control.Monad (guard)
import           Control.Monad.Free
import           Control.Monad.Free.Church

import           Data.Aeson (Value, object, (.=))
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder, byteString, char8, toLazyByteString)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy.Char8 (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Coerce
import           Data.Functor.Classes
import           Data.Hashable
import           Data.Int
import           Data.Maybe
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif
import           Data.Scientific (Scientific)
import           Data.String (IsString(..), fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import           Data.Time (LocalTime, UTCTime, TimeOfDay, NominalDiffTime, Day)
import           Data.UUID.Types (UUID)
import           Data.Word
import qualified Data.Vector as V
import           GHC.TypeLits

import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg (Oid(..), Binary(..), Null(..))
import qualified Database.PostgreSQL.Simple.Time as Pg (Date, LocalTimestamp, UTCTimestamp)
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

instance Eq1 PgSyntaxF where
  liftEq eq (EmitByteString b1 next1) (EmitByteString b2 next2) =
      b1 == b2 && next1 `eq` next2
  liftEq eq (EmitBuilder b1 next1) (EmitBuilder b2 next2) =
      toLazyByteString b1 == toLazyByteString b2 && next1 `eq` next2
  liftEq eq (EscapeString b1 next1) (EscapeString b2 next2) =
      b1 == b2 && next1 `eq` next2
  liftEq eq (EscapeBytea b1 next1) (EscapeBytea b2 next2) =
      b1 == b2 && next1 `eq` next2
  liftEq eq (EscapeIdentifier b1 next1) (EscapeIdentifier b2 next2) =
      b1 == b2 && next1 `eq` next2
  liftEq _ _ _ = False

instance Eq f => Eq (PgSyntaxF f) where
  (==) = eq1

instance Hashable PgSyntax where
  hashWithSalt salt (PgSyntax s) = runF s finish step salt
    where
      finish _ salt = hashWithSalt salt ()
      step (EmitByteString b hashRest) salt = hashRest (hashWithSalt salt (0 :: Int, b))
      step (EmitBuilder b hashRest)    salt = hashRest (hashWithSalt salt (1 :: Int, toLazyByteString b))
      step (EscapeString  b hashRest)  salt = hashRest (hashWithSalt salt (2 :: Int, b))
      step (EscapeBytea  b hashRest)   salt = hashRest (hashWithSalt salt (3 :: Int, b))
      step (EscapeIdentifier b hashRest) salt = hashRest (hashWithSalt salt (4 :: Int, b))

instance Sql92DisplaySyntax PgSyntax where
  displaySyntax = BL.unpack . pgRenderSyntaxScript

type PgSyntaxM = F PgSyntaxF

-- | A piece of Postgres SQL syntax, which may contain embedded escaped byte and
-- text sequences. 'PgSyntax' composes monoidally, and may be created with
-- 'emit', 'emitBuilder', 'escapeString', 'escapBytea', and 'escapeIdentifier'.
newtype PgSyntax
  = PgSyntax { buildPgSyntax :: PgSyntaxM () }

instance Semigroup PgSyntax where
  a <> b = PgSyntax (buildPgSyntax a >> buildPgSyntax b)

instance Monoid PgSyntax where
  mempty = PgSyntax (pure ())
  mappend = (<>)

instance Eq PgSyntax where
  PgSyntax x == PgSyntax y = (fromF x :: Free PgSyntaxF ()) == fromF y

instance Show PgSyntax where
  showsPrec prec s =
    showParen (prec > 10) $
    showString "PgSyntax <" .
    shows (pgTestSyntax s) .
    showString ">"

emit :: ByteString -> PgSyntax
emit bs = PgSyntax (liftF (EmitByteString bs ()))

emitBuilder :: Builder -> PgSyntax
emitBuilder b = PgSyntax (liftF (EmitBuilder b ()))

escapeString, escapeBytea, escapeIdentifier :: ByteString -> PgSyntax
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
data PgCommandSyntax
    = PgCommandSyntax
    { pgCommandType :: PgCommandType
    , fromPgCommand :: PgSyntax }

-- | 'IsSql92SelectSyntax' for Postgres
newtype PgSelectSyntax = PgSelectSyntax { fromPgSelect :: PgSyntax }

newtype PgSelectTableSyntax = PgSelectTableSyntax { fromPgSelectTable :: PgSyntax }

-- | 'IsSql92InsertSyntax' for Postgres
newtype PgInsertSyntax = PgInsertSyntax { fromPgInsert :: PgSyntax }

-- | 'IsSql92DeleteSyntax' for Postgres
newtype PgDeleteSyntax = PgDeleteSyntax { fromPgDelete :: PgSyntax }

-- | 'IsSql92UpdateSyntax' for Postgres
newtype PgUpdateSyntax = PgUpdateSyntax { fromPgUpdate :: PgSyntax }

newtype PgExpressionSyntax = PgExpressionSyntax { fromPgExpression :: PgSyntax } deriving Eq
newtype PgAggregationSetQuantifierSyntax = PgAggregationSetQuantifierSyntax { fromPgAggregationSetQuantifier :: PgSyntax }
newtype PgSelectSetQuantifierSyntax = PgSelectSetQuantifierSyntax { fromPgSelectSetQuantifier :: PgSyntax }
newtype PgFromSyntax = PgFromSyntax { fromPgFrom :: PgSyntax }
newtype PgTableNameSyntax = PgTableNameSyntax { fromPgTableName :: PgSyntax }
newtype PgComparisonQuantifierSyntax = PgComparisonQuantifierSyntax { fromPgComparisonQuantifier :: PgSyntax }
newtype PgExtractFieldSyntax = PgExtractFieldSyntax { fromPgExtractField :: PgSyntax }
newtype PgProjectionSyntax = PgProjectionSyntax { fromPgProjection :: PgSyntax }
newtype PgGroupingSyntax = PgGroupingSyntax { fromPgGrouping :: PgSyntax }
newtype PgValueSyntax = PgValueSyntax { fromPgValue :: PgSyntax }
newtype PgTableSourceSyntax = PgTableSourceSyntax { fromPgTableSource :: PgSyntax }
newtype PgFieldNameSyntax = PgFieldNameSyntax { fromPgFieldName :: PgSyntax }
newtype PgInsertValuesSyntax = PgInsertValuesSyntax { fromPgInsertValues :: PgSyntax }
newtype PgInsertOnConflictSyntax = PgInsertOnConflictSyntax { fromPgInsertOnConflict :: PgSyntax }
newtype PgInsertOnConflictTargetSyntax = PgInsertOnConflictTargetSyntax { fromPgInsertOnConflictTarget :: PgSyntax }
newtype PgInsertOnConflictUpdateSyntax = PgInsertOnConflictUpdateSyntax { fromPgInsertOnConflictUpdate :: PgSyntax }
newtype PgConflictActionSyntax = PgConflictActionSyntax { fromPgConflictAction :: PgSyntax }
data PgOrderingSyntax = PgOrderingSyntax { pgOrderingSyntax :: PgSyntax, pgOrderingNullOrdering :: Maybe PgNullOrdering }
data PgSelectLockingClauseSyntax = PgSelectLockingClauseSyntax { pgSelectLockingClauseStrength :: PgSelectLockingStrength
                                                               , pgSelectLockingTables :: [T.Text]
                                                               , pgSelectLockingClauseOptions :: Maybe PgSelectLockingOptions }
newtype PgCommonTableExpressionSyntax
    = PgCommonTableExpressionSyntax { fromPgCommonTableExpression :: PgSyntax }

fromPgOrdering :: PgOrderingSyntax -> PgSyntax
fromPgOrdering (PgOrderingSyntax s Nothing) = s
fromPgOrdering (PgOrderingSyntax s (Just PgNullOrderingNullsFirst)) = s <> emit " NULLS FIRST"
fromPgOrdering (PgOrderingSyntax s (Just PgNullOrderingNullsLast)) = s <> emit " NULLS LAST"

data PgNullOrdering
  = PgNullOrderingNullsFirst
  | PgNullOrderingNullsLast
  deriving (Show, Eq, Generic)

fromPgSelectLockingClause :: PgSelectLockingClauseSyntax -> PgSyntax
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

-- | Specifies the level of lock that will be taken against a row. See
-- <https://www.postgresql.org/docs/current/static/explicit-locking.html#LOCKING-ROWS the manual section>
-- for more information.
data PgSelectLockingStrength
  = PgSelectLockingStrengthUpdate
  -- ^ @UPDATE@
  | PgSelectLockingStrengthNoKeyUpdate
  -- ^ @NO KEY UPDATE@
  | PgSelectLockingStrengthShare
  -- ^ @SHARE@
  | PgSelectLockingStrengthKeyShare
  -- ^ @KEY SHARE@
  deriving (Show, Eq, Generic)

-- | Specifies how we should handle lock conflicts.
--
-- See
-- <https://www.postgresql.org/docs/9.5/static/sql-select.html#SQL-FOR-UPDATE-SHARE the manual section>
-- for more information
data PgSelectLockingOptions
  = PgSelectLockingOptionsNoWait
  -- ^ @NOWAIT@. Report an error rather than waiting for the lock
  | PgSelectLockingOptionsSkipLocked
  -- ^ @SKIP LOCKED@. Rather than wait for a lock, skip the row instead
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

newtype PgCreateTableSyntax = PgCreateTableSyntax { fromPgCreateTable :: PgSyntax }
data PgTableOptionsSyntax = PgTableOptionsSyntax PgSyntax PgSyntax
newtype PgColumnSchemaSyntax = PgColumnSchemaSyntax { fromPgColumnSchema :: PgSyntax } deriving (Show, Eq)
instance Sql92DisplaySyntax PgColumnSchemaSyntax where
  displaySyntax = displaySyntax . fromPgColumnSchema

data PgDataTypeSyntax
  = PgDataTypeSyntax
  { pgDataTypeDescr :: PgDataTypeDescr
  , fromPgDataType :: PgSyntax
  , pgDataTypeSerialized :: BeamSerializedDataType
  } deriving Show
instance Sql92DisplaySyntax PgDataTypeSyntax where
  displaySyntax = displaySyntax . fromPgDataType

data PgColumnConstraintDefinitionSyntax
  = PgColumnConstraintDefinitionSyntax
  { fromPgColumnConstraintDefinition :: PgSyntax
  , pgColumnConstraintDefinitionSerialized :: BeamSerializedConstraintDefinition
  } deriving Show
instance Sql92DisplaySyntax PgColumnConstraintDefinitionSyntax where
  displaySyntax = displaySyntax . fromPgColumnConstraintDefinition

data PgColumnConstraintSyntax
  = PgColumnConstraintSyntax
  { fromPgColumnConstraint :: PgSyntax
  , pgColumnConstraintSerialized :: BeamSerializedConstraint
  }
newtype PgTableConstraintSyntax = PgTableConstraintSyntax { fromPgTableConstraint :: PgSyntax }
data PgMatchTypeSyntax
  = PgMatchTypeSyntax
  { fromPgMatchType :: PgSyntax
  , pgMatchTypeSerialized :: BeamSerializedMatchType
  }
data PgReferentialActionSyntax
  = PgReferentialActionSyntax
  { fromPgReferentialAction :: PgSyntax
  , pgReferentialActionSerialized :: BeamSerializedReferentialAction
  }
newtype PgDropTableSyntax = PgDropTableSyntax { fromPgDropTable :: PgSyntax }
newtype PgAlterTableSyntax = PgAlterTableSyntax { fromPgAlterTable :: PgSyntax }
newtype PgAlterTableActionSyntax = PgAlterTableActionSyntax { fromPgAlterTableAction :: PgSyntax }
newtype PgAlterColumnActionSyntax = PgAlterColumnActionSyntax { fromPgAlterColumnAction :: PgSyntax }
newtype PgWindowFrameSyntax = PgWindowFrameSyntax { fromPgWindowFrame :: PgSyntax }
newtype PgWindowFrameBoundsSyntax = PgWindowFrameBoundsSyntax { fromPgWindowFrameBounds :: PgSyntax }
newtype PgWindowFrameBoundSyntax = PgWindowFrameBoundSyntax { fromPgWindowFrameBound :: ByteString -> PgSyntax }

instance Hashable PgDataTypeSyntax where
  hashWithSalt salt (PgDataTypeSyntax a _ _) = hashWithSalt salt a
instance Eq PgDataTypeSyntax where
  PgDataTypeSyntax a _ _ == PgDataTypeSyntax b _ _ = a == b

instance HasDataTypeCreatedCheck PgDataTypeSyntax where
  dataTypeHasBeenCreated (PgDataTypeSyntax (PgDataTypeDescrOid {}) _ _) _ = True
  dataTypeHasBeenCreated (PgDataTypeSyntax (PgDataTypeDescrDomain d) _ _) pre =
    not . null $
    do PgHasEnum nm _ <- pre
       guard (nm == d)

instance Eq PgColumnConstraintDefinitionSyntax where
  PgColumnConstraintDefinitionSyntax a _ ==
    PgColumnConstraintDefinitionSyntax b _ =
      a == b

instance IsSql92Syntax PgCommandSyntax where
  type Sql92SelectSyntax PgCommandSyntax = PgSelectSyntax
  type Sql92InsertSyntax PgCommandSyntax = PgInsertSyntax
  type Sql92UpdateSyntax PgCommandSyntax = PgUpdateSyntax
  type Sql92DeleteSyntax PgCommandSyntax = PgDeleteSyntax

  selectCmd = PgCommandSyntax PgCommandTypeQuery      . coerce
  insertCmd = PgCommandSyntax PgCommandTypeDataUpdate . coerce
  deleteCmd = PgCommandSyntax PgCommandTypeDataUpdate . coerce
  updateCmd = PgCommandSyntax PgCommandTypeDataUpdate . coerce

instance IsSql92DdlCommandSyntax PgCommandSyntax where
  type Sql92DdlCommandCreateTableSyntax PgCommandSyntax = PgCreateTableSyntax
  type Sql92DdlCommandDropTableSyntax PgCommandSyntax = PgDropTableSyntax
  type Sql92DdlCommandAlterTableSyntax PgCommandSyntax = PgAlterTableSyntax

  createTableCmd = PgCommandSyntax PgCommandTypeDdl . coerce
  dropTableCmd   = PgCommandSyntax PgCommandTypeDdl . coerce
  alterTableCmd  = PgCommandSyntax PgCommandTypeDdl . coerce

instance IsSql92TableNameSyntax PgTableNameSyntax where
  tableName Nothing t = PgTableNameSyntax (pgQuotedIdentifier t)
  tableName (Just s) t = PgTableNameSyntax (pgQuotedIdentifier s <> emit "." <> pgQuotedIdentifier t)

instance IsSql92UpdateSyntax PgUpdateSyntax where
  type Sql92UpdateFieldNameSyntax PgUpdateSyntax = PgFieldNameSyntax
  type Sql92UpdateExpressionSyntax PgUpdateSyntax = PgExpressionSyntax
  type Sql92UpdateTableNameSyntax PgUpdateSyntax = PgTableNameSyntax

  updateStmt tbl fields where_ =
    PgUpdateSyntax $
    emit "UPDATE " <> fromPgTableName tbl <>
    (case fields of
       [] -> mempty
       fields ->
         emit " SET " <>
         pgSepBy (emit ", ") (map (\(field, val) -> fromPgFieldName field <> emit "=" <> fromPgExpression val) fields)) <>
    maybe mempty (\where_ -> emit " WHERE " <> fromPgExpression where_) where_

instance IsSql92DeleteSyntax PgDeleteSyntax where
  type Sql92DeleteExpressionSyntax PgDeleteSyntax = PgExpressionSyntax
  type Sql92DeleteTableNameSyntax PgDeleteSyntax = PgTableNameSyntax

  deleteStmt tbl alias where_ =
    PgDeleteSyntax $
    emit "DELETE FROM " <> fromPgTableName tbl <>
    maybe mempty (\alias_ -> emit " AS " <> pgQuotedIdentifier alias_) alias <>
    maybe mempty (\where_ -> emit " WHERE " <> fromPgExpression where_) where_

  deleteSupportsAlias _ = True

instance IsSql92SelectSyntax PgSelectSyntax where
  type Sql92SelectSelectTableSyntax PgSelectSyntax = PgSelectTableSyntax
  type Sql92SelectOrderingSyntax PgSelectSyntax = PgOrderingSyntax

  selectStmt tbl ordering limit offset =
    pgSelectStmt tbl ordering limit offset Nothing

instance IsSql92SelectTableSyntax PgSelectTableSyntax where
  type Sql92SelectTableSelectSyntax PgSelectTableSyntax = PgSelectSyntax
  type Sql92SelectTableExpressionSyntax PgSelectTableSyntax = PgExpressionSyntax
  type Sql92SelectTableProjectionSyntax PgSelectTableSyntax = PgProjectionSyntax
  type Sql92SelectTableFromSyntax PgSelectTableSyntax = PgFromSyntax
  type Sql92SelectTableGroupingSyntax PgSelectTableSyntax = PgGroupingSyntax
  type Sql92SelectTableSetQuantifierSyntax PgSelectTableSyntax = PgSelectSetQuantifierSyntax

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

instance IsSql92GroupingSyntax PgGroupingSyntax where
  type Sql92GroupingExpressionSyntax PgGroupingSyntax = PgExpressionSyntax

  groupByExpressions es =
      PgGroupingSyntax $
      pgSepBy (emit ", ") (map fromPgExpression es)

instance IsSql92FromSyntax PgFromSyntax where
  type Sql92FromExpressionSyntax PgFromSyntax = PgExpressionSyntax
  type Sql92FromTableSourceSyntax PgFromSyntax = PgTableSourceSyntax

  fromTable tableSrc Nothing = coerce tableSrc
  fromTable tableSrc (Just (nm, colNms)) =
      PgFromSyntax $
      coerce tableSrc <> emit " AS " <> pgQuotedIdentifier nm <>
      maybe mempty (\colNms' -> pgParens (pgSepBy (emit ",") (map pgQuotedIdentifier colNms'))) colNms

  innerJoin a b Nothing = PgFromSyntax (fromPgFrom a <> emit " CROSS JOIN " <> fromPgFrom b)
  innerJoin a b (Just e) = pgJoin "INNER JOIN" a b (Just e)

  leftJoin = pgJoin "LEFT JOIN"
  rightJoin = pgJoin "RIGHT JOIN"

instance IsSql92FromOuterJoinSyntax PgFromSyntax where
  outerJoin = pgJoin "FULL OUTER JOIN"

instance IsSql92OrderingSyntax PgOrderingSyntax where
  type Sql92OrderingExpressionSyntax PgOrderingSyntax = PgExpressionSyntax

  ascOrdering e = PgOrderingSyntax (fromPgExpression e <> emit " ASC") Nothing
  descOrdering e = PgOrderingSyntax (fromPgExpression e <> emit " DESC") Nothing

instance IsSql2003OrderingElementaryOLAPOperationsSyntax PgOrderingSyntax where
  nullsFirstOrdering o = o { pgOrderingNullOrdering = Just PgNullOrderingNullsFirst }
  nullsLastOrdering o = o { pgOrderingNullOrdering = Just PgNullOrderingNullsLast }

instance IsSql92DataTypeSyntax PgDataTypeSyntax where
  domainType nm = PgDataTypeSyntax (PgDataTypeDescrDomain nm) (pgQuotedIdentifier nm)
                                   (domainType nm)

  charType prec charSet = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bpchar) (Just (fromIntegral (fromMaybe 1 prec))))
                                           (emit "CHAR" <> pgOptPrec prec <> pgOptCharSet charSet)
                                           (charType prec charSet)
  varCharType prec charSet = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.varchar) (fmap fromIntegral prec))
                                              (emit "VARCHAR" <> pgOptPrec prec <> pgOptCharSet charSet)
                                              (varCharType prec charSet)
  nationalCharType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bpchar) (fmap fromIntegral prec))
                                           (emit "NATIONAL CHAR" <> pgOptPrec prec)
                                           (nationalCharType prec)
  nationalVarCharType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.varchar) (fmap fromIntegral prec))
                                              (emit "NATIONAL CHARACTER VARYING" <> pgOptPrec prec)
                                              (nationalVarCharType prec)

  bitType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bit) (fmap fromIntegral prec))
                                  (emit "BIT" <> pgOptPrec prec)
                                  (bitType prec)
  varBitType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.varbit) (fmap fromIntegral prec))
                                     (emit "BIT VARYING" <> pgOptPrec prec)
                                     (varBitType prec)

  numericType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.numeric) (mkNumericPrec prec))
                                      (emit "NUMERIC" <> pgOptNumericPrec prec)
                                      (numericType prec)
  decimalType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.numeric) (mkNumericPrec prec))
                                      (emit "DECIMAL" <> pgOptNumericPrec prec)
                                      (decimalType prec)

  intType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.int4) Nothing) (emit "INT") intType
  smallIntType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.int2) Nothing) (emit "SMALLINT") smallIntType

  floatType prec = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.float4) Nothing) (emit "FLOAT" <> pgOptPrec prec)
                                    (floatType prec)
  doubleType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.float8) Nothing) (emit "DOUBLE PRECISION") doubleType
  realType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.float4) Nothing) (emit "REAL") realType
  dateType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.date) Nothing) (emit "DATE") dateType
  timeType prec withTz = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.time) Nothing)
                                          (emit "TIME" <> pgOptPrec prec <> if withTz then emit " WITH TIME ZONE" else mempty)
                                          (timeType prec withTz)
  timestampType prec withTz = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid (if withTz then Pg.timestamptz else Pg.timestamp)) Nothing)
                                               (emit "TIMESTAMP" <> pgOptPrec prec <> if withTz then emit " WITH TIME ZONE" else mempty)
                                               (timestampType prec withTz)

instance IsSql99DataTypeSyntax PgDataTypeSyntax where
  characterLargeObjectType = pgTextType { pgDataTypeSerialized = characterLargeObjectType }
  binaryLargeObjectType = pgByteaType { pgDataTypeSerialized = binaryLargeObjectType }
  booleanType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bool) Nothing) (emit "BOOLEAN")
                                 booleanType
  arrayType (PgDataTypeSyntax _ syntax serialized) sz =
    PgDataTypeSyntax (error "TODO: array migrations")
                     (syntax <> emit "[" <> emit (fromString (show sz)) <> emit "]")
                     (arrayType serialized sz)
  rowType = error "rowType"

instance IsSql99CommonTableExpressionSelectSyntax PgSelectSyntax where
    type Sql99SelectCTESyntax PgSelectSyntax = PgCommonTableExpressionSyntax

    withSyntax ctes (PgSelectSyntax select) =
        PgSelectSyntax $
        emit "WITH " <>
        pgSepBy (emit ", ") (map fromPgCommonTableExpression ctes) <>
        select

instance IsSql99RecursiveCommonTableExpressionSelectSyntax PgSelectSyntax where
    withRecursiveSyntax ctes (PgSelectSyntax select) =
        PgSelectSyntax $
        emit "WITH RECURSIVE " <>
        pgSepBy (emit ", ") (map fromPgCommonTableExpression ctes) <>
        select

instance IsSql99CommonTableExpressionSyntax PgCommonTableExpressionSyntax where
    type Sql99CTESelectSyntax PgCommonTableExpressionSyntax = PgSelectSyntax

    cteSubquerySyntax tbl fields (PgSelectSyntax select) =
        PgCommonTableExpressionSyntax $
        pgQuotedIdentifier tbl <> pgParens (pgSepBy (emit ",") (map pgQuotedIdentifier fields)) <>
        emit " AS " <> pgParens select

instance IsSql2008BigIntDataTypeSyntax PgDataTypeSyntax where
  bigIntType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.int8) Nothing) (emit "BIGINT") bigIntType

instance Sql92SerializableDataTypeSyntax PgDataTypeSyntax where
  serializeDataType = fromBeamSerializedDataType . pgDataTypeSerialized

pgOptPrec :: Maybe Word -> PgSyntax
pgOptPrec Nothing = mempty
pgOptPrec (Just x) = emit "(" <> emit (fromString (show x)) <> emit ")"

pgOptCharSet :: Maybe T.Text -> PgSyntax
pgOptCharSet Nothing = mempty
pgOptCharSet (Just cs) = emit " CHARACTER SET " <> emit (TE.encodeUtf8 cs)

pgOptNumericPrec :: Maybe (Word, Maybe Word) -> PgSyntax
pgOptNumericPrec Nothing = mempty
pgOptNumericPrec (Just (prec, Nothing)) = pgOptPrec (Just prec)
pgOptNumericPrec (Just (prec, Just dec)) = emit "(" <> emit (fromString (show prec)) <> emit ", " <> emit (fromString (show dec)) <> emit ")"

pgDataTypeJSON :: Value -> BeamSerializedDataType
pgDataTypeJSON v = BeamSerializedDataType (beamSerializeJSON "postgres" v)

pgByteaType :: PgDataTypeSyntax
pgByteaType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.bytea) Nothing) (emit "BYTEA")
                               (pgDataTypeJSON "bytea")

pgSmallSerialType, pgSerialType, pgBigSerialType :: PgDataTypeSyntax
pgSmallSerialType = PgDataTypeSyntax (pgDataTypeDescr smallIntType) (emit "SMALLSERIAL") (pgDataTypeJSON "smallserial")
pgSerialType = PgDataTypeSyntax (pgDataTypeDescr intType) (emit "SERIAL") (pgDataTypeJSON "serial")
pgBigSerialType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.int8) Nothing) (emit "BIGSERIAL") (pgDataTypeJSON "bigserial")

pgPointType, pgLineType, pgLineSegmentType, pgBoxType :: PgDataTypeSyntax
pgPointType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.point) Nothing) (emit "POINT") (pgDataTypeJSON "point")
pgLineType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.line) Nothing) (emit "LINE") (pgDataTypeJSON "line")
pgLineSegmentType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.lseg) Nothing) (emit "LSEG") (pgDataTypeJSON "lseg")
pgBoxType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.box) Nothing) (emit "BOX") (pgDataTypeJSON "box")

pgUnboundedArrayType :: PgDataTypeSyntax -> PgDataTypeSyntax
pgUnboundedArrayType (PgDataTypeSyntax _ syntax serialized) =
    PgDataTypeSyntax (error "Can't do array migrations yet")
                     (syntax <> emit "[]")
                     (pgDataTypeJSON (object [ "unbounded-array" .= fromBeamSerializedDataType serialized ]))

pgTsQueryTypeInfo :: Pg.TypeInfo
pgTsQueryTypeInfo = Pg.Basic (Pg.Oid 3615) 'U' ',' "tsquery"

pgTsQueryType :: PgDataTypeSyntax
pgTsQueryType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid pgTsQueryTypeInfo) Nothing)
                                 (emit "TSQUERY") (pgDataTypeJSON "tsquery")

-- | Postgres TypeInfo for tsvector
-- TODO Is the Oid stable from postgres instance to postgres instance?
pgTsVectorTypeInfo :: Pg.TypeInfo
pgTsVectorTypeInfo = Pg.Basic (Pg.Oid 3614) 'U' ',' "tsvector"

pgTsVectorType :: PgDataTypeSyntax
pgTsVectorType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid pgTsVectorTypeInfo) Nothing)
                                  (emit "TSVECTOR")
                                  (pgDataTypeJSON "tsvector")

pgTextType :: PgDataTypeSyntax
pgTextType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.text) Nothing) (emit "TEXT")
                              (pgDataTypeJSON "text")

pgJsonType, pgJsonbType :: PgDataTypeSyntax
pgJsonType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.json) Nothing) (emit "JSON") (pgDataTypeJSON "json")
pgJsonbType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.jsonb) Nothing) (emit "JSONB") (pgDataTypeJSON "jsonb")

pgUuidType :: PgDataTypeSyntax
pgUuidType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.uuid) Nothing) (emit "UUID") (pgDataTypeJSON "uuid")

pgMoneyType :: PgDataTypeSyntax
pgMoneyType = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.money) Nothing) (emit "MONEY") (pgDataTypeJSON "money")

mkNumericPrec :: Maybe (Word, Maybe Word) -> Maybe Int32
mkNumericPrec Nothing = Nothing
mkNumericPrec (Just (whole, dec)) = Just $ (fromIntegral whole `shiftL` 16) .|. (fromIntegral (fromMaybe 0 dec) .&. 0xFFFF)

instance IsCustomSqlSyntax PgExpressionSyntax where
  newtype CustomSqlSyntax PgExpressionSyntax =
    PgCustomExpressionSyntax { fromPgCustomExpression :: PgSyntax }
    deriving Monoid
  customExprSyntax = PgExpressionSyntax . fromPgCustomExpression
  renderSyntax = PgCustomExpressionSyntax . pgParens . fromPgExpression

instance Semigroup (CustomSqlSyntax PgExpressionSyntax) where
  (<>) = mappend

instance IsString (CustomSqlSyntax PgExpressionSyntax) where
  fromString = PgCustomExpressionSyntax . emit . fromString

instance IsSql92QuantifierSyntax PgComparisonQuantifierSyntax where
  quantifyOverAll = PgComparisonQuantifierSyntax (emit "ALL")
  quantifyOverAny = PgComparisonQuantifierSyntax (emit "ANY")

instance IsSql92ExtractFieldSyntax PgExtractFieldSyntax where
  secondsField = PgExtractFieldSyntax (emit "SECOND")
  minutesField = PgExtractFieldSyntax (emit "MINUTE")
  hourField    = PgExtractFieldSyntax (emit "HOUR")
  dayField     = PgExtractFieldSyntax (emit "DAY")
  monthField   = PgExtractFieldSyntax (emit "MONTH")
  yearField    = PgExtractFieldSyntax (emit "YEAR")

instance IsSql92ExpressionSyntax PgExpressionSyntax where
  type Sql92ExpressionValueSyntax PgExpressionSyntax = PgValueSyntax
  type Sql92ExpressionSelectSyntax PgExpressionSyntax = PgSelectSyntax
  type Sql92ExpressionFieldNameSyntax PgExpressionSyntax = PgFieldNameSyntax
  type Sql92ExpressionQuantifierSyntax PgExpressionSyntax = PgComparisonQuantifierSyntax
  type Sql92ExpressionCastTargetSyntax PgExpressionSyntax = PgDataTypeSyntax
  type Sql92ExpressionExtractFieldSyntax PgExpressionSyntax = PgExtractFieldSyntax

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
  castE e to = PgExpressionSyntax (emit "CAST((" <> fromPgExpression e <> emit ") AS " <> fromPgDataType to <> emit ")")
  caseE cases else_ =
      PgExpressionSyntax $
      emit "CASE " <>
      foldMap (\(cond, res) -> emit "WHEN " <> fromPgExpression cond <> emit " THEN " <> fromPgExpression res <> emit " ") cases <>
      emit "ELSE " <> fromPgExpression else_ <> emit " END"

  currentTimestampE = PgExpressionSyntax $ emit "CURRENT_TIMESTAMP"

  defaultE = PgExpressionSyntax $ emit "DEFAULT"

  inE e es = PgExpressionSyntax $ pgParens (fromPgExpression e) <> emit " IN " <>
                                  pgParens (pgSepBy (emit ", ") (map fromPgExpression es))

instance IsSql99FunctionExpressionSyntax PgExpressionSyntax where
  functionCallE name args =
    PgExpressionSyntax $
    fromPgExpression name <>
    pgParens (pgSepBy (emit ", ") (map fromPgExpression args))
  functionNameE nm = PgExpressionSyntax (emit (TE.encodeUtf8 nm))

instance IsSql99ExpressionSyntax PgExpressionSyntax where
  distinctE select = PgExpressionSyntax (emit "DISTINCT (" <> fromPgSelect select <> emit ")")
  similarToE = pgBinOp "SIMILAR TO"

  instanceFieldE i nm =
    PgExpressionSyntax $
    pgParens (fromPgExpression i) <> emit "." <> escapeIdentifier (TE.encodeUtf8 nm)

  refFieldE i nm =
    PgExpressionSyntax $
    pgParens (fromPgExpression i) <> emit "->" <> escapeIdentifier (TE.encodeUtf8 nm)

instance IsSql99ConcatExpressionSyntax PgExpressionSyntax where
  concatE [] = valueE (sqlValueSyntax ("" :: T.Text))
  concatE [x] = x
  concatE es =
    PgExpressionSyntax $
    emit "CONCAT" <> pgParens (pgSepBy (emit ", ") (map fromPgExpression es))

instance IsSql2003ExpressionSyntax PgExpressionSyntax where
  type Sql2003ExpressionWindowFrameSyntax PgExpressionSyntax =
    PgWindowFrameSyntax

  overE expr frame =
    PgExpressionSyntax $
    fromPgExpression expr <> emit " " <> fromPgWindowFrame frame
  rowNumberE = PgExpressionSyntax $ emit "ROW_NUMBER()"

instance IsSql2003EnhancedNumericFunctionsExpressionSyntax PgExpressionSyntax where
  lnE    x = PgExpressionSyntax (emit "LN("    <> fromPgExpression x <> emit ")")
  expE   x = PgExpressionSyntax (emit "EXP("   <> fromPgExpression x <> emit ")")
  sqrtE  x = PgExpressionSyntax (emit "SQRT("  <> fromPgExpression x <> emit ")")
  ceilE  x = PgExpressionSyntax (emit "CEIL("  <> fromPgExpression x <> emit ")")
  floorE x = PgExpressionSyntax (emit "FLOOR(" <> fromPgExpression x <> emit ")")
  powerE x y = PgExpressionSyntax (emit "POWER(" <> fromPgExpression x <> emit ", " <> fromPgExpression y <> emit ")")

instance IsSql2003ExpressionAdvancedOLAPOperationsSyntax PgExpressionSyntax where
  denseRankAggE = PgExpressionSyntax $ emit "DENSE_RANK()"
  percentRankAggE = PgExpressionSyntax $ emit "PERCENT_RANK()"
  cumeDistAggE = PgExpressionSyntax $ emit "CUME_DIST()"

instance IsSql2003ExpressionElementaryOLAPOperationsSyntax PgExpressionSyntax where
  rankAggE = PgExpressionSyntax $ emit "RANK()"
  filterAggE agg filter =
    PgExpressionSyntax $
    fromPgExpression agg <> emit " FILTER (WHERE " <> fromPgExpression filter <> emit ")"

instance IsSql2003EnhancedNumericFunctionsAggregationExpressionSyntax PgExpressionSyntax where
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

instance IsSql2003NtileExpressionSyntax PgExpressionSyntax where
  ntileE x = PgExpressionSyntax (emit "NTILE(" <> fromPgExpression x <> emit ")")

instance IsSql2003LeadAndLagExpressionSyntax PgExpressionSyntax where
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

instance IsSql2003FirstValueAndLastValueExpressionSyntax PgExpressionSyntax where
  firstValueE x = PgExpressionSyntax (emit "FIRST_VALUE(" <> fromPgExpression x <> emit ")")
  lastValueE x = PgExpressionSyntax (emit "LAST_VALUE(" <> fromPgExpression x <> emit ")")

instance IsSql2003NthValueExpressionSyntax PgExpressionSyntax where
  nthValueE x n = PgExpressionSyntax (emit "NTH_VALUE(" <> fromPgExpression x <> emit ", " <> fromPgExpression n <> emit ")")

instance IsSql2003WindowFrameSyntax PgWindowFrameSyntax where
  type Sql2003WindowFrameExpressionSyntax PgWindowFrameSyntax = PgExpressionSyntax
  type Sql2003WindowFrameOrderingSyntax PgWindowFrameSyntax = PgOrderingSyntax
  type Sql2003WindowFrameBoundsSyntax PgWindowFrameSyntax = PgWindowFrameBoundsSyntax

  frameSyntax partition_ ordering_ bounds_ =
    PgWindowFrameSyntax $
    emit "OVER " <>
    pgParens
    (
      maybe mempty (\p -> emit "PARTITION BY " <> pgSepBy (emit ", ") (map fromPgExpression p)) partition_ <>
      maybe mempty (\o -> emit " ORDER BY " <> pgSepBy (emit ", ") (map fromPgOrdering o)) ordering_ <>
      maybe mempty (\b -> emit " ROWS " <> fromPgWindowFrameBounds b) bounds_
    )

instance IsSql2003WindowFrameBoundsSyntax PgWindowFrameBoundsSyntax where
  type Sql2003WindowFrameBoundsBoundSyntax PgWindowFrameBoundsSyntax = PgWindowFrameBoundSyntax

  fromToBoundSyntax from Nothing =
    PgWindowFrameBoundsSyntax (fromPgWindowFrameBound from "PRECEDING")
  fromToBoundSyntax from (Just to) =
    PgWindowFrameBoundsSyntax $
    emit "BETWEEN " <> fromPgWindowFrameBound from "PRECEDING" <> emit " AND " <> fromPgWindowFrameBound to "FOLLOWING"

instance IsSql2003WindowFrameBoundSyntax PgWindowFrameBoundSyntax where
  unboundedSyntax = PgWindowFrameBoundSyntax $ \where_ -> emit "UNBOUNDED " <> emit where_
  nrowsBoundSyntax 0 = PgWindowFrameBoundSyntax $ \_ -> emit "CURRENT ROW"
  nrowsBoundSyntax n = PgWindowFrameBoundSyntax $ \where_ -> emit (fromString (show n)) <> emit " " <> emit where_

instance IsSql92AggregationExpressionSyntax PgExpressionSyntax where
  type Sql92AggregationSetQuantifierSyntax PgExpressionSyntax = PgAggregationSetQuantifierSyntax

  countAllE = PgExpressionSyntax (emit "COUNT(*)")
  countE = pgUnAgg "COUNT"
  avgE = pgUnAgg "AVG"
  sumE = pgUnAgg "SUM"
  minE = pgUnAgg "MIN"
  maxE = pgUnAgg "MAX"

instance IsSql99AggregationExpressionSyntax PgExpressionSyntax where
  everyE = pgUnAgg "EVERY"

  -- According to the note at <https://www.postgresql.org/docs/9.2/static/functions-aggregate.html>
  -- the following functions are equivalent.
  someE = pgUnAgg "BOOL_ANY"
  anyE = pgUnAgg "BOOL_ANY"

instance IsSql92AggregationSetQuantifierSyntax PgAggregationSetQuantifierSyntax where
  setQuantifierDistinct = PgAggregationSetQuantifierSyntax $ emit "DISTINCT"
  setQuantifierAll = PgAggregationSetQuantifierSyntax $ emit "ALL"

instance IsSql92AggregationSetQuantifierSyntax PgSelectSetQuantifierSyntax where
  setQuantifierDistinct = PgSelectSetQuantifierSyntax $ emit "DISTINCT"
  setQuantifierAll = PgSelectSetQuantifierSyntax $ emit "ALL"

pgSelectSetQuantifierDistinctOn :: [PgExpressionSyntax] -> PgSelectSetQuantifierSyntax
pgSelectSetQuantifierDistinctOn exprs =
  PgSelectSetQuantifierSyntax $
  emit "DISTINCT ON " <> pgParens (pgSepBy (emit ", ") (fromPgExpression <$> exprs))

pgUnAgg :: ByteString -> Maybe PgAggregationSetQuantifierSyntax -> PgExpressionSyntax -> PgExpressionSyntax
pgUnAgg fn q e =
  PgExpressionSyntax $
  emit fn <> emit "(" <> maybe mempty (\q -> fromPgAggregationSetQuantifier q <> emit " ") q <> fromPgExpression e <> emit ")"

pgBinAgg :: ByteString -> Maybe PgAggregationSetQuantifierSyntax -> PgExpressionSyntax -> PgExpressionSyntax
         -> PgExpressionSyntax
pgBinAgg fn q x y =
  PgExpressionSyntax $
  emit fn <> emit "(" <> maybe mempty (\q -> fromPgAggregationSetQuantifier q <> emit " ") q
          <> fromPgExpression x <> emit ", " <> fromPgExpression y <> emit ")"

instance IsSql92FieldNameSyntax PgFieldNameSyntax where
  qualifiedField a b =
    PgFieldNameSyntax $
    pgQuotedIdentifier a <> emit "." <> pgQuotedIdentifier b
  unqualifiedField = PgFieldNameSyntax . pgQuotedIdentifier

instance IsSql92TableSourceSyntax PgTableSourceSyntax where
  type Sql92TableSourceSelectSyntax PgTableSourceSyntax = PgSelectSyntax
  type Sql92TableSourceExpressionSyntax PgTableSourceSyntax = PgExpressionSyntax
  type Sql92TableSourceTableNameSyntax PgTableSourceSyntax = PgTableNameSyntax

  tableNamed = PgTableSourceSyntax . fromPgTableName
  tableFromSubSelect s = PgTableSourceSyntax $ emit "(" <> fromPgSelect s <> emit ")"
  tableFromValues vss = PgTableSourceSyntax . pgParens $
                        emit "VALUES " <>
                        pgSepBy (emit ", ")
                                (map (\vs -> pgParens (pgSepBy (emit ", ")
                                                               (map fromPgExpression vs))) vss)

instance IsSql92ProjectionSyntax PgProjectionSyntax where
  type Sql92ProjectionExpressionSyntax PgProjectionSyntax = PgExpressionSyntax

  projExprs exprs =
    PgProjectionSyntax $
    pgSepBy (emit ", ")
            (map (\(expr, nm) -> fromPgExpression expr <>
                                 maybe mempty (\nm -> emit " AS " <> pgQuotedIdentifier nm) nm) exprs)

instance IsSql92InsertSyntax PgInsertSyntax where
  type Sql92InsertTableNameSyntax PgInsertSyntax = PgTableNameSyntax
  type Sql92InsertValuesSyntax PgInsertSyntax = PgInsertValuesSyntax

  insertStmt tblName fields values =
      PgInsertSyntax $
      emit "INSERT INTO " <> fromPgTableName tblName <> emit "(" <>
      pgSepBy (emit ", ") (map pgQuotedIdentifier fields) <>
      emit ") " <> fromPgInsertValues values

instance IsSql92InsertValuesSyntax PgInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax PgInsertValuesSyntax = PgExpressionSyntax
  type Sql92InsertValuesSelectSyntax PgInsertValuesSyntax = PgSelectSyntax

  insertSqlExpressions es =
      PgInsertValuesSyntax $
      emit "VALUES " <>
      pgSepBy (emit ", ")
              (map (\es -> emit "(" <> pgSepBy (emit ", ") (coerce es) <> emit ")")
                   es)
  insertFromSql (PgSelectSyntax a) = PgInsertValuesSyntax a

instance IsSql92DropTableSyntax PgDropTableSyntax where
  type Sql92DropTableTableNameSyntax PgDropTableSyntax = PgTableNameSyntax

  dropTableSyntax tblNm =
    PgDropTableSyntax $
    emit "DROP TABLE " <> fromPgTableName tblNm

instance IsSql92AlterTableSyntax PgAlterTableSyntax where
  type Sql92AlterTableAlterTableActionSyntax PgAlterTableSyntax = PgAlterTableActionSyntax
  type Sql92AlterTableTableNameSyntax PgAlterTableSyntax = PgTableNameSyntax

  alterTableSyntax tblNm action =
    PgAlterTableSyntax $
    emit "ALTER TABLE " <> fromPgTableName tblNm <> emit " " <> fromPgAlterTableAction action

instance IsSql92AlterTableActionSyntax PgAlterTableActionSyntax where
  type Sql92AlterTableAlterColumnActionSyntax PgAlterTableActionSyntax = PgAlterColumnActionSyntax
  type Sql92AlterTableColumnSchemaSyntax PgAlterTableActionSyntax = PgColumnSchemaSyntax

  alterColumnSyntax colNm action =
    PgAlterTableActionSyntax $
    emit "ALTER COLUMN " <> pgQuotedIdentifier colNm <> emit " " <> fromPgAlterColumnAction action

  addColumnSyntax colNm schema =
    PgAlterTableActionSyntax $
    emit "ADD COLUMN " <> pgQuotedIdentifier colNm <> emit " " <> fromPgColumnSchema schema

  dropColumnSyntax colNm =
    PgAlterTableActionSyntax $
    emit "DROP COLUMN " <> pgQuotedIdentifier colNm

  renameTableToSyntax newNm =
    PgAlterTableActionSyntax $
    emit "RENAME TO " <> pgQuotedIdentifier newNm

  renameColumnToSyntax oldNm newNm =
    PgAlterTableActionSyntax $
    emit "RENAME COLUMN " <> pgQuotedIdentifier oldNm <> emit " TO " <> pgQuotedIdentifier newNm

instance IsSql92AlterColumnActionSyntax PgAlterColumnActionSyntax where
  setNullSyntax = PgAlterColumnActionSyntax (emit "DROP NOT NULL")
  setNotNullSyntax = PgAlterColumnActionSyntax (emit "SET NOT NULL")

instance IsSql92CreateTableSyntax PgCreateTableSyntax where
  type Sql92CreateTableTableNameSyntax PgCreateTableSyntax = PgTableNameSyntax
  type Sql92CreateTableColumnSchemaSyntax PgCreateTableSyntax = PgColumnSchemaSyntax
  type Sql92CreateTableTableConstraintSyntax PgCreateTableSyntax = PgTableConstraintSyntax
  type Sql92CreateTableOptionsSyntax PgCreateTableSyntax = PgTableOptionsSyntax

  createTableSyntax options tblNm fieldTypes constraints =
    let (beforeOptions, afterOptions) =
          case options of
            Nothing -> (emit " ", emit " ")
            Just (PgTableOptionsSyntax before after) ->
              ( emit " " <> before <> emit " "
              , emit " " <> after <> emit " " )
    in PgCreateTableSyntax $
       emit "CREATE" <> beforeOptions <> emit "TABLE " <> fromPgTableName tblNm <>
       emit " (" <>
       pgSepBy (emit ", ")
               (map (\(nm, type_) -> pgQuotedIdentifier nm <> emit " " <> fromPgColumnSchema type_)  fieldTypes <>
                map fromPgTableConstraint constraints)
       <> emit ")" <> afterOptions

instance IsSql92TableConstraintSyntax PgTableConstraintSyntax where
  primaryKeyConstraintSyntax fieldNames =
    PgTableConstraintSyntax $
    emit "PRIMARY KEY(" <> pgSepBy (emit ", ") (map pgQuotedIdentifier fieldNames) <> emit ")"

instance Hashable PgColumnSchemaSyntax where
  hashWithSalt salt = hashWithSalt salt . fromPgColumnSchema
instance IsSql92ColumnSchemaSyntax PgColumnSchemaSyntax where
  type Sql92ColumnSchemaColumnTypeSyntax PgColumnSchemaSyntax = PgDataTypeSyntax
  type Sql92ColumnSchemaExpressionSyntax PgColumnSchemaSyntax = PgExpressionSyntax
  type Sql92ColumnSchemaColumnConstraintDefinitionSyntax PgColumnSchemaSyntax = PgColumnConstraintDefinitionSyntax

  columnSchemaSyntax colType defaultClause constraints collation =
    PgColumnSchemaSyntax syntax
    where
      syntax =
        fromPgDataType colType <>
        maybe mempty (\d -> emit " DEFAULT " <> fromPgExpression d) defaultClause <>
        (case constraints of
           [] -> mempty
           _ -> foldMap (\c -> emit " " <> fromPgColumnConstraintDefinition c) constraints) <>
        maybe mempty (\nm -> emit " COLLATE " <> pgQuotedIdentifier nm) collation

instance IsSql92MatchTypeSyntax PgMatchTypeSyntax where
  fullMatchSyntax = PgMatchTypeSyntax (emit "FULL") fullMatchSyntax
  partialMatchSyntax = PgMatchTypeSyntax (emit "PARTIAL") partialMatchSyntax

pgMatchTypeJSON :: Value -> BeamSerializedMatchType
pgMatchTypeJSON v = BeamSerializedMatchType (beamSerializeJSON "postgres" v)

pgSimpleMatchSyntax :: PgMatchTypeSyntax
pgSimpleMatchSyntax = PgMatchTypeSyntax (emit "SIMPLE") (pgMatchTypeJSON "simple")

instance IsSql92ReferentialActionSyntax PgReferentialActionSyntax where
  referentialActionCascadeSyntax = PgReferentialActionSyntax (emit "CASCADE") referentialActionCascadeSyntax
  referentialActionNoActionSyntax = PgReferentialActionSyntax (emit "NO ACTION") referentialActionNoActionSyntax
  referentialActionSetDefaultSyntax = PgReferentialActionSyntax (emit "SET DEFAULT") referentialActionSetDefaultSyntax
  referentialActionSetNullSyntax = PgReferentialActionSyntax (emit "SET NULL") referentialActionSetNullSyntax

fromSqlConstraintAttributes :: SqlConstraintAttributesBuilder -> PgSyntax
fromSqlConstraintAttributes (SqlConstraintAttributesBuilder timing deferrable) =
  maybe mempty timingBuilder timing <> maybe mempty deferrableBuilder deferrable
  where timingBuilder InitiallyDeferred = emit "INITIALLY DEFERRED"
        timingBuilder InitiallyImmediate = emit "INITIALLY IMMEDIATE"
        deferrableBuilder False = emit "NOT DEFERRABLE"
        deferrableBuilder True = emit "DEFERRABLE"

instance Hashable PgColumnConstraintDefinitionSyntax where
  hashWithSalt salt = hashWithSalt salt . fromPgColumnConstraintDefinition

instance IsSql92ColumnConstraintDefinitionSyntax PgColumnConstraintDefinitionSyntax where
  type Sql92ColumnConstraintDefinitionConstraintSyntax PgColumnConstraintDefinitionSyntax = PgColumnConstraintSyntax
  type Sql92ColumnConstraintDefinitionAttributesSyntax PgColumnConstraintDefinitionSyntax = SqlConstraintAttributesBuilder

  constraintDefinitionSyntax nm constraint attrs =
    PgColumnConstraintDefinitionSyntax syntax
      (constraintDefinitionSyntax nm (pgColumnConstraintSerialized constraint) (fmap sqlConstraintAttributesSerialized attrs))
    where
      syntax =
        maybe mempty (\nm -> emit "CONSTRAINT " <> pgQuotedIdentifier nm <> emit " " ) nm <>
        fromPgColumnConstraint constraint <>
        maybe mempty (\a -> emit " " <> fromSqlConstraintAttributes a) attrs

instance Sql92SerializableConstraintDefinitionSyntax PgColumnConstraintDefinitionSyntax where
  serializeConstraint = fromBeamSerializedConstraintDefinition . pgColumnConstraintDefinitionSerialized

instance IsSql92ColumnConstraintSyntax PgColumnConstraintSyntax where
  type Sql92ColumnConstraintMatchTypeSyntax PgColumnConstraintSyntax = PgMatchTypeSyntax
  type Sql92ColumnConstraintReferentialActionSyntax PgColumnConstraintSyntax = PgReferentialActionSyntax
  type Sql92ColumnConstraintExpressionSyntax PgColumnConstraintSyntax = PgExpressionSyntax

  notNullConstraintSyntax = PgColumnConstraintSyntax (emit "NOT NULL") notNullConstraintSyntax
  uniqueColumnConstraintSyntax = PgColumnConstraintSyntax (emit "UNIQUE") uniqueColumnConstraintSyntax
  primaryKeyColumnConstraintSyntax = PgColumnConstraintSyntax (emit "PRIMARY KEY") primaryKeyColumnConstraintSyntax
  checkColumnConstraintSyntax expr =
    PgColumnConstraintSyntax (emit "CHECK(" <> fromPgExpression expr <> emit ")")
                             (checkColumnConstraintSyntax . BeamSerializedExpression . TE.decodeUtf8 .
                              toStrict . pgRenderSyntaxScript . fromPgExpression $ expr)
  referencesConstraintSyntax tbl fields matchType onUpdate onDelete =
    PgColumnConstraintSyntax syntax
      (referencesConstraintSyntax tbl fields (fmap pgMatchTypeSerialized matchType)
                                  (fmap pgReferentialActionSerialized onUpdate)
                                  (fmap pgReferentialActionSerialized onDelete))
    where
      syntax =
        emit "REFERENCES " <> pgQuotedIdentifier tbl <> emit "("
        <> pgSepBy (emit ", ") (map pgQuotedIdentifier fields) <> emit ")" <>
        maybe mempty (\m -> emit " " <> fromPgMatchType m) matchType <>
        maybe mempty (\a -> emit " ON UPDATE " <> fromPgReferentialAction a) onUpdate <>
        maybe mempty (\a -> emit " ON DELETE " <> fromPgReferentialAction a) onDelete

defaultPgValueSyntax :: Pg.ToField a => a -> PgValueSyntax
defaultPgValueSyntax =
    PgValueSyntax . pgBuildAction . pure . Pg.toField

-- Database Predicates

data PgHasEnum = PgHasEnum T.Text {- Enumeration name -} [T.Text] {- enum values -}
    deriving (Show, Eq, Generic)
instance Hashable PgHasEnum
instance DatabasePredicate PgHasEnum where
    englishDescription (PgHasEnum enumName values) =
        "Has postgres enumeration " ++ show enumName ++ " with values " ++ show values

    predicateSpecificity _ = PredicateSpecificityOnlyBackend "postgres"
    serializePredicate (PgHasEnum name values) =
        object [ "has-postgres-enum" .= object [ "name" .= name
                                               , "values" .= values ] ]

#define DEFAULT_SQL_SYNTAX(ty)                                  \
           instance HasSqlValueSyntax PgValueSyntax ty where    \
             sqlValueSyntax = defaultPgValueSyntax

DEFAULT_SQL_SYNTAX(Bool)
DEFAULT_SQL_SYNTAX(Double)
DEFAULT_SQL_SYNTAX(Float)
DEFAULT_SQL_SYNTAX(Int8)
DEFAULT_SQL_SYNTAX(Int16)
DEFAULT_SQL_SYNTAX(Int32)
DEFAULT_SQL_SYNTAX(Int64)
DEFAULT_SQL_SYNTAX(Integer)
DEFAULT_SQL_SYNTAX(Word8)
DEFAULT_SQL_SYNTAX(Word16)
DEFAULT_SQL_SYNTAX(Word32)
DEFAULT_SQL_SYNTAX(Word64)
DEFAULT_SQL_SYNTAX(T.Text)
DEFAULT_SQL_SYNTAX(TL.Text)
DEFAULT_SQL_SYNTAX(Value)
DEFAULT_SQL_SYNTAX(Pg.Oid)
DEFAULT_SQL_SYNTAX(LocalTime)
DEFAULT_SQL_SYNTAX(UTCTime)
DEFAULT_SQL_SYNTAX(TimeOfDay)
DEFAULT_SQL_SYNTAX(NominalDiffTime)
DEFAULT_SQL_SYNTAX(Day)
DEFAULT_SQL_SYNTAX(UUID)
DEFAULT_SQL_SYNTAX([Char])
DEFAULT_SQL_SYNTAX(Pg.HStoreMap)
DEFAULT_SQL_SYNTAX(Pg.HStoreList)
DEFAULT_SQL_SYNTAX(Pg.HStoreBuilder)
DEFAULT_SQL_SYNTAX(Pg.Date)
DEFAULT_SQL_SYNTAX(Pg.LocalTimestamp)
DEFAULT_SQL_SYNTAX(Pg.UTCTimestamp)
DEFAULT_SQL_SYNTAX(Scientific)

instance HasSqlValueSyntax PgValueSyntax (CI T.Text) where
  sqlValueSyntax = sqlValueSyntax . CI.original
instance HasSqlValueSyntax PgValueSyntax (CI TL.Text) where
  sqlValueSyntax = sqlValueSyntax . CI.original

instance HasSqlValueSyntax PgValueSyntax SqlNull where
  sqlValueSyntax _ = defaultPgValueSyntax Pg.Null

instance HasSqlValueSyntax PgValueSyntax x => HasSqlValueSyntax PgValueSyntax (Maybe x) where
  sqlValueSyntax Nothing = sqlValueSyntax SqlNull
  sqlValueSyntax (Just x) = sqlValueSyntax x

instance HasSqlValueSyntax PgValueSyntax B.ByteString where
  sqlValueSyntax = defaultPgValueSyntax . Pg.Binary

instance HasSqlValueSyntax PgValueSyntax BL.ByteString where
  sqlValueSyntax = defaultPgValueSyntax . Pg.Binary

instance Pg.ToField a => HasSqlValueSyntax PgValueSyntax (V.Vector a) where
  sqlValueSyntax = defaultPgValueSyntax

instance TypeError (PreferExplicitSize Int Int32) => HasSqlValueSyntax PgValueSyntax Int where
  sqlValueSyntax = defaultPgValueSyntax

instance TypeError (PreferExplicitSize Word Word32) => HasSqlValueSyntax PgValueSyntax Word where
  sqlValueSyntax = defaultPgValueSyntax

pgQuotedIdentifier :: T.Text -> PgSyntax
pgQuotedIdentifier t =
  escapeIdentifier (TE.encodeUtf8 t)

pgParens :: PgSyntax -> PgSyntax
pgParens a = emit "(" <> a <> emit ")"

pgTableOp :: ByteString -> PgSelectTableSyntax -> PgSelectTableSyntax
          -> PgSelectTableSyntax
pgTableOp op tbl1 tbl2 =
    PgSelectTableSyntax $
    emit "(" <> fromPgSelectTable tbl1 <> emit ") " <> emit op <>
    emit " (" <> fromPgSelectTable tbl2 <> emit ")"

pgCompOp :: ByteString -> Maybe PgComparisonQuantifierSyntax
         -> PgExpressionSyntax -> PgExpressionSyntax -> PgExpressionSyntax
pgCompOp op quantifier a b =
  PgExpressionSyntax $
  emit "(" <> fromPgExpression a <>
  emit (") " <> op) <>
  maybe (emit " (" <> fromPgExpression b <> emit ")")
        (\q -> emit " " <> fromPgComparisonQuantifier q <> emit " " <> fromPgExpression b)
        quantifier

pgBinOp :: ByteString -> PgExpressionSyntax -> PgExpressionSyntax -> PgExpressionSyntax
pgBinOp op a b =
  PgExpressionSyntax $
  emit "(" <> fromPgExpression a <> emit (") " <> op <> " (") <> fromPgExpression b <> emit ")"

pgPostFix, pgUnOp :: ByteString -> PgExpressionSyntax -> PgExpressionSyntax
pgPostFix op a =
  PgExpressionSyntax $
  emit "(" <> fromPgExpression a <> emit ") " <> emit op
pgUnOp op a =
  PgExpressionSyntax $
  emit (op <> "(") <> fromPgExpression a <> emit ")"

pgJoin :: ByteString -> PgFromSyntax -> PgFromSyntax -> Maybe PgExpressionSyntax -> PgFromSyntax
pgJoin joinType a b Nothing =
  PgFromSyntax $
  fromPgFrom a <> emit (" " <> joinType <> " ") <> fromPgFrom b <> emit " ON TRUE"
pgJoin joinType a b (Just on) =
  PgFromSyntax $
  fromPgFrom a <> emit (" " <> joinType <> " ") <> fromPgFrom b <>
  emit " ON " <> fromPgExpression on

pgSepBy :: PgSyntax -> [PgSyntax] -> PgSyntax
pgSepBy _ [] = mempty
pgSepBy _ [x] = x
pgSepBy sep (x:xs) = x <> sep <> pgSepBy sep xs

pgDebugRenderSyntax :: PgSyntax -> IO ()
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

pgBuildAction :: [ Pg.Action ] -> PgSyntax
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

pgSelectStmt :: PgSelectTableSyntax
             -> [PgOrderingSyntax]
             -> Maybe Integer {-^ LIMIT -}
             -> Maybe Integer {-^ OFFSET -}
             -> Maybe PgSelectLockingClauseSyntax
             -> PgSelectSyntax
pgSelectStmt tbl ordering limit offset locking =
    PgSelectSyntax $
    mconcat [ coerce tbl
            , case ordering of
                [] -> mempty
                ordering -> emit " ORDER BY " <> pgSepBy (emit ", ") (map fromPgOrdering ordering)
            , maybe mempty (emit . fromString . (" LIMIT " <>) . show) limit
            , maybe mempty (emit . fromString . (" OFFSET " <>) . show) offset
            , maybe mempty fromPgSelectLockingClause locking ]

pgCreateExtensionSyntax :: T.Text -> PgCommandSyntax
pgCreateExtensionSyntax extName =
  PgCommandSyntax PgCommandTypeDdl $ emit "CREATE EXTENSION " <> pgQuotedIdentifier extName

pgDropExtensionSyntax :: T.Text -> PgCommandSyntax
pgDropExtensionSyntax extName =
  PgCommandSyntax PgCommandTypeDdl $ emit "DROP EXTENSION " <> pgQuotedIdentifier extName

pgCreateEnumSyntax :: T.Text -> [PgValueSyntax] -> PgCommandSyntax
pgCreateEnumSyntax enumName vals =
    PgCommandSyntax PgCommandTypeDdl $
    emit "CREATE TYPE " <> pgQuotedIdentifier enumName <> emit " AS ENUM(" <>
    pgSepBy (emit ", ") (fmap fromPgValue vals) <> emit ")"

pgDropTypeSyntax :: T.Text -> PgCommandSyntax
pgDropTypeSyntax typeName =
    PgCommandSyntax PgCommandTypeDdl $
    emit "DROP TYPE " <> pgQuotedIdentifier typeName

-- -- * Pg-specific Q monad


data PgEscapeType = PgEscapeString | PgEscapeBytea | PgEscapeIdentifier
  deriving (Show, Eq, Ord, Enum, Bounded)
data PgSyntaxPrim = PgSyntaxPrim (Maybe PgEscapeType) BL.ByteString deriving Show

instance IsString PgSyntaxPrim where
  fromString = PgSyntaxPrim Nothing . fromString

pgTestSyntax :: PgSyntax -> [ PgSyntaxPrim ]
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

pgRenderSyntaxScript :: PgSyntax -> BL.ByteString
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

