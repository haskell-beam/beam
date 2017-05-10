{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

-- | Postgres-specific types, functions, and operators
module Database.Beam.Postgres.PgSpecific where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types
import           Database.Beam.Query.Internal

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import           Data.Hashable
import           Data.Monoid
import           Data.Proxy
import           Data.String
import           Data.Type.Bool
import qualified Data.Vector as V
import qualified Data.Text as T

import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg

import           GHC.TypeLits
import           GHC.Exts hiding (toList)

-- * TsVector type

newtype TsVectorConfig = TsVectorConfig ByteString
  deriving (Show, Eq, Ord, IsString)
newtype TsVector = TsVector ByteString
  deriving (Show, Eq, Ord)

-- | Postgres TypeInfo for tsvector
-- TODO Is the Oid stable from postgres instance to postgres instance?
tsvectorType :: Pg.TypeInfo
tsvectorType = Pg.Basic (Pg.Oid 3614) 'U' ',' "tsvector"

instance Pg.FromField TsVector where
  fromField field d =
    if Pg.typeOid field /= Pg.typoid tsvectorType
    then Pg.returnError Pg.Incompatible field ""
    else case d of
           Just d' -> pure (TsVector d')
           Nothing -> Pg.returnError Pg.UnexpectedNull field ""

instance Pg.ToField TsVector where
  toField (TsVector d) =
    Pg.Many [ Pg.Plain "($$"
            , Pg.Plain (byteString d)
            , Pg.Plain "$$::tsvector)" ]

english :: TsVectorConfig
english = TsVectorConfig "english"

toTsVector :: IsSqlExpressionSyntaxStringType PgExpressionSyntax str
           => Maybe TsVectorConfig -> QGenExpr context PgExpressionSyntax s str
           -> QGenExpr context PgExpressionSyntax s TsVector
toTsVector Nothing (QExpr (PgExpressionSyntax x)) =
  QExpr . PgExpressionSyntax $
  emit "to_tsquery(" <> x <> emit ")"
toTsVector (Just (TsVectorConfig configNm)) (QExpr (PgExpressionSyntax x)) =
  QExpr . PgExpressionSyntax $
  emit "to_tsquery('" <> escapeString configNm <> emit "', " <> x <> emit ")"

(@@) :: QGenExpr context PgExpressionSyntax s TsVector
     -> QGenExpr context PgExpressionSyntax s TsQuery
     -> QGenExpr context PgExpressionSyntax s Bool
QExpr vec @@ QExpr q =
  QExpr (pgBinOp "@@" vec q)

-- * TsQuery type

newtype TsQuery = TsQuery ByteString
  deriving (Show, Eq, Ord)

tsqueryType :: Pg.TypeInfo
tsqueryType = Pg.Basic (Pg.Oid 3615) 'U' ',' "tsquery"

instance Pg.FromField TsQuery where
  fromField field d =
    if Pg.typeOid field /= Pg.typoid tsqueryType
    then Pg.returnError Pg.Incompatible field ""
    else case d of
           Just d' -> pure (TsQuery d')
           Nothing -> Pg.returnError Pg.UnexpectedNull field ""

-- * Array operators

-- TODO this should be robust to slices
(!.) :: Integral ix
     => QGenExpr context PgExpressionSyntax s ix
     -> QGenExpr context PgExpressionSyntax s (V.Vector a)
     -> QGenExpr context PgExpressionSyntax s a
QExpr (PgExpressionSyntax v) !. QExpr (PgExpressionSyntax ix) =
  QExpr . PgExpressionSyntax $
  emit "(" <> v <> emit ")[" <> ix <> emit "]"

arrayDims_ :: IsSqlExpressionSyntaxStringType PgExpressionSyntax text
           => QGenExpr context PgExpressionSyntax s (V.Vector a)
           -> QGenExpr context PgExpressionSyntax s text
arrayDims_ (QExpr (PgExpressionSyntax v)) = QExpr (PgExpressionSyntax (emit "array_dims(" <> v <> emit ")"))

type family CountDims (v :: *) :: Nat where
  CountDims (V.Vector a) = 1 + CountDims a
  CountDims a = 0
type family WithinBounds (dim :: Nat) (v :: *) :: Constraint where
  WithinBounds dim v =
    If ((dim <=? CountDims v) && (1 <=? dim))
       (() :: Constraint)
       (TypeError ( ('Text "Dimension " ':<>: 'ShowType dim ':<>: 'Text " is out of bounds.") ':$$:
                    ('Text "The type " ':<>: 'ShowType v ':<>: 'Text " has " ':<>: 'ShowType (CountDims v) ':<>: 'Text " dimension(s).") ':$$:
                    ('Text "Hint: The dimension should be a natural between 1 and " ':<>: 'ShowType (CountDims v)) ))

arrayUpper_, arrayLower_
  :: forall (dim :: Nat) context num v s.
     (KnownNat dim, WithinBounds dim (V.Vector v), Integral num)
  => QGenExpr context PgExpressionSyntax s (V.Vector v)
  -> QGenExpr context PgExpressionSyntax s num
arrayUpper_ v =
  unsafeRetype (arrayUpperUnsafe_ v (val_ (natVal (Proxy @dim) :: Integer)) :: QGenExpr context PgExpressionSyntax s (Maybe Integer))
arrayLower_ v =
  unsafeRetype (arrayLowerUnsafe_ v (val_ (natVal (Proxy @dim) :: Integer)) :: QGenExpr context PgExpressionSyntax s (Maybe Integer))

arrayUpperUnsafe_, arrayLowerUnsafe_
  :: (Integral dim, Integral length)
  => QGenExpr context PgExpressionSyntax s (V.Vector v)
  -> QGenExpr context PgExpressionSyntax s dim
  -> QGenExpr context PgExpressionSyntax s (Maybe length)
arrayUpperUnsafe_ (QExpr v) (QExpr dim) =
  QExpr . PgExpressionSyntax $
  emit "array_upper(" <> fromPgExpression v <> emit ", " <> fromPgExpression dim <> emit ")"
arrayLowerUnsafe_ (QExpr v) (QExpr dim) =
  QExpr . PgExpressionSyntax $
  emit "array_lower(" <> fromPgExpression v <> emit ", " <> fromPgExpression dim <> emit ")"

-- * Array expressions

data PgArrayValueContext

class PgIsArrayContext ctxt where
  mkArraySyntax :: Proxy ctxt -> PgSyntax -> PgSyntax
  mkArraySyntax _ s = emit "ARRAY" <> s
instance PgIsArrayContext PgArrayValueContext where
  mkArraySyntax _ = id
instance PgIsArrayContext QValueContext
instance PgIsArrayContext QAggregateContext
instance PgIsArrayContext QWindowingContext

array_ :: forall context f s a.
          (PgIsArrayContext context, Foldable f)
       => f (QGenExpr PgArrayValueContext PgExpressionSyntax s a)
       -> QGenExpr context PgExpressionSyntax s (V.Vector a)
array_ vs =
  QExpr . PgExpressionSyntax . mkArraySyntax (Proxy @context) $
  emit "[" <>
  pgSepBy (emit ", ") (map (\(QExpr e) -> fromPgExpression e) (toList vs)) <>
  emit "]"

-- * JSON

newtype PgJSON a = PgJSON a
  deriving ( Show, Eq, Ord, Hashable, Monoid )

instance (Typeable x, FromJSON x) => Pg.FromField (PgJSON x) where
  fromField field d =
    if Pg.typeOid field /= Pg.typoid Pg.json
    then Pg.returnError Pg.Incompatible field ""
    else case decodeStrict =<< d of
           Just d' -> pure (PgJSON d')
           Nothing -> Pg.returnError Pg.UnexpectedNull field ""

instance (Typeable a, FromJSON a) => FromBackendRow Postgres (PgJSON a)
instance ToJSON a => HasSqlValueSyntax PgValueSyntax (PgJSON a) where
  sqlValueSyntax (PgJSON a) =
    PgValueSyntax $
    escapeString (BL.toStrict (encode a)) <> emit "::json"

newtype PgJSONB a = PgJSONB a
  deriving ( Show, Eq, Ord, Hashable, Monoid )

instance (Typeable x, FromJSON x) => Pg.FromField (PgJSONB x) where
  fromField field d =
    if Pg.typeOid field /= Pg.typoid Pg.jsonb
    then Pg.returnError Pg.Incompatible field ""
    else case decodeStrict =<< d of
           Just d' -> pure (PgJSONB d')
           Nothing -> Pg.returnError Pg.UnexpectedNull field ""

instance (Typeable a, FromJSON a) => FromBackendRow Postgres (PgJSONB a)
instance ToJSON a => HasSqlValueSyntax PgValueSyntax (PgJSONB a) where
  sqlValueSyntax (PgJSONB a) =
    PgValueSyntax $
    escapeString (BL.toStrict (encode a)) <> emit "::jsonb"

class IsPgJSON (json :: * -> *) where
--  pgJsonEach     :: QGenExpr ctxt PgExpressionSyntax s (json a) -> Q PgSelectSyntax s (PgJSONTable (QGenExpr ctxt PgExpressionSyntax s))
--  pgJsonEachText :: QGenExpr ctxt PgExpressionSyntax s (json a) -> Q PgSelectSyntax s (PgJSONTable (QGenExpr ctxt PgExpressionSyntax s))
--  pgJsonKeys     :: QGenExpr ctxt PgExpressionSyntax s (json a) -> Q PgSelectSyntax s (PgJSONKeysTable (QGenExpr ctxt PgExpressionSyntax s))
--  pgJsonArrayElements :: QGenExpr ctxt PgExpressionSyntax s (json a) -> Q PgSelectSyntax s (PgJSONValuesTable (QGenExpr ctxt PgExpressionSyntax s))
--  pgJsonArrayElementsText :: GenExpr ctxt PgExpressionSyntax s (json a) -> Q PgSelectSyntax s (PgJSONValuesTextTable (QGenExpr ctxt PgExpressionSyntax s))
--  pgJsonToRecord
--  pgJsonToRecordSet
  pgJsonTypeOf :: QGenExpr ctxt PgExpressionSyntax s (json a) -> QGenExpr ctxt PgExpressionSyntax s T.Text
  pgJsonStripNulls :: QGenExpr ctxt PgExpressionSyntax s (json a) -> QGenExpr ctxt PgExpressionSyntax s (json b)

  pgJsonAgg :: QExpr PgExpressionSyntax s a -> QAgg PgExpressionSyntax s (json a)
  pgJsonObjectAgg :: QExpr PgExpressionSyntax s key -> QExpr PgExpressionSyntax s value
                  -> QAgg PgExpressionSyntax s (json a)

instance IsPgJSON PgJSON where
  pgJsonTypeOf (QExpr a) =
    QExpr . PgExpressionSyntax $
    emit "json_typeof" <> pgParens (fromPgExpression a)

  pgJsonStripNulls (QExpr a) =
    QExpr . PgExpressionSyntax $
    emit "json_strip_nulls" <> pgParens (fromPgExpression a)

  pgJsonAgg (QExpr a) =
    QExpr . PgExpressionSyntax $
    emit "json_agg" <> pgParens (fromPgExpression a)

  pgJsonObjectAgg (QExpr keys) (QExpr values) =
    QExpr . PgExpressionSyntax $
    emit "json_object_agg" <> pgParens (fromPgExpression keys <> emit ", " <> fromPgExpression values)

instance IsPgJSON PgJSONB where
  pgJsonTypeOf (QExpr a) =
    QExpr . PgExpressionSyntax $
    emit "jsonb_typeof" <> pgParens (fromPgExpression a)

  pgJsonStripNulls (QExpr a) =
    QExpr . PgExpressionSyntax $
    emit "jsonb_strip_nulls" <> pgParens (fromPgExpression a)

  pgJsonAgg (QExpr a) =
    QExpr . PgExpressionSyntax $
    emit "jsonb_agg" <> pgParens (fromPgExpression a)

  pgJsonObjectAgg (QExpr keys) (QExpr values) =
    QExpr . PgExpressionSyntax $
    emit "jsonb_object_agg" <> pgParens (fromPgExpression keys <> emit ", " <> fromPgExpression values)

(@>), (<@) :: IsPgJSON json
           => QGenExpr ctxt PgExpressionSyntax s (json a)
           -> QGenExpr ctxt PgExpressionSyntax s (json b)
           -> QGenExpr ctxt PgExpressionSyntax s Bool
QExpr a @> QExpr b =
  QExpr (pgBinOp "@>" a b)
QExpr a <@ QExpr b =
  QExpr (pgBinOp "<@" a b)

(->#) :: IsPgJSON json
      => QGenExpr ctxt PgExpressionSyntax s (json a)
      -> QGenExpr ctxt PgExpressionSyntax s Int
      -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a -># QExpr b =
  QExpr (pgBinOp "->" a b)

(->$) :: IsPgJSON json
      => QGenExpr ctxt PgExpressionSyntax s (json a)
      -> QGenExpr ctxt PgExpressionSyntax s T.Text
      -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a ->$ QExpr b =
  QExpr (pgBinOp "->" a b)

(->>#) :: IsPgJSON json
       => QGenExpr ctxt PgExpressionSyntax s (json a)
       -> QGenExpr ctxt PgExpressionSyntax s Int
       -> QGenExpr ctxt PgExpressionSyntax s T.Text
QExpr a ->># QExpr b =
  QExpr (pgBinOp "->>" a b)

(->>$) :: IsPgJSON json
       => QGenExpr ctxt PgExpressionSyntax s (json a)
       -> QGenExpr ctxt PgExpressionSyntax s T.Text
       -> QGenExpr ctxt PgExpressionSyntax s T.Text
QExpr a ->>$ QExpr b =
  QExpr (pgBinOp "->>" a b)

(#>) :: IsPgJSON json
     => QGenExpr ctxt PgExpressionSyntax s (json a)
     -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
     -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a #> QExpr b =
  QExpr (pgBinOp "#>" a b)

(#>>) :: IsPgJSON json
      => QGenExpr ctxt PgExpressionSyntax s (json a)
      -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
      -> QGenExpr ctxt PgExpressionSyntax s T.Text
QExpr a #>> QExpr b =
  QExpr (pgBinOp "#>" a b)

(?) :: IsPgJSON json
    => QGenExpr ctxt PgExpressionSyntax s (json a)
    -> QGenExpr ctxt PgExpressionSyntax s T.Text
    -> QGenExpr ctxt PgExpressionSyntax s Bool
QExpr a ? QExpr b =
  QExpr (pgBinOp "?" a b)

(?|), (?&) :: IsPgJSON json
           => QGenExpr ctxt PgExpressionSyntax s (json a)
           -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
           -> QGenExpr ctxt PgExpressionSyntax s Bool
QExpr a ?| QExpr b =
  QExpr (pgBinOp "?|" a b)
QExpr a ?& QExpr b =
  QExpr (pgBinOp "?&" a b)

withoutKey :: IsPgJSON json
           => QGenExpr ctxt PgExpressionSyntax s (json a)
           -> QGenExpr ctxt PgExpressionSyntax s T.Text
           -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a `withoutKey` QExpr b =
  QExpr (pgBinOp "-" a b)

withoutIdx :: IsPgJSON json
           => QGenExpr ctxt PgExpressionSyntax s (json a)
           -> QGenExpr ctxt PgExpressionSyntax s Int
           -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a `withoutIdx` QExpr b =
  QExpr (pgBinOp "-" a b)

withoutKeys :: IsPgJSON json
            => QGenExpr ctxt PgExpressionSyntax s (json a)
            -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
            -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a `withoutKeys` QExpr b =
  QExpr (pgBinOp "#-" a b)

pgJsonArrayLength :: IsPgJSON json => QGenExpr ctxt PgExpressionSyntax s (json a)
                  -> QGenExpr ctxt PgExpressionSyntax s Int
pgJsonArrayLength (QExpr a) =
  QExpr . PgExpressionSyntax $
  emit "json_array_length(" <> fromPgExpression a <> emit ")"

pgJsonbUpdate, pgJsonbSet
  :: QGenExpr ctxt PgExpressionSyntax s (PgJSONB a)
  -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
  -> QGenExpr ctxt PgExpressionSyntax s (PgJSONB b)
  -> QGenExpr ctxt PgExpressionSyntax s (PgJSONB a)
pgJsonbUpdate (QExpr a) (QExpr path) (QExpr newVal) =
  QExpr . PgExpressionSyntax $
  emit "jsonb_set" <> pgParens (fromPgExpression a <> emit ", " <> fromPgExpression path <> emit ", " <> fromPgExpression newVal)
pgJsonbSet (QExpr a) (QExpr path) (QExpr newVal) =
  QExpr . PgExpressionSyntax $
  emit "jsonb_set" <> pgParens (fromPgExpression a <> emit ", " <> fromPgExpression path <> emit ", " <> fromPgExpression newVal <> emit ", true")

pgJsonbPretty :: QGenExpr ctxt PgExpressionSyntax s (PgJSONB a)
              -> QGenExpr ctxt PgExpressionSyntax s T.Text
pgJsonbPretty (QExpr a) =
  QExpr . PgExpressionSyntax $
  emit "jsonb_pretty" <> pgParens (fromPgExpression a)

-- * Postgresql aggregates

pgArrayAgg :: QExpr PgExpressionSyntax s a
           -> QAgg PgExpressionSyntax s (V.Vector a)
pgArrayAgg (QExpr a) =
  QExpr . PgExpressionSyntax $
  emit "array_agg" <> pgParens (fromPgExpression a)

pgStringAgg :: QExpr PgExpressionSyntax s T.Text
            -> QExpr PgExpressionSyntax s T.Text
            -> QAgg PgExpressionSyntax s T.Text
pgStringAgg (QExpr v) (QExpr delim) =
  QExpr . PgExpressionSyntax $
  emit "string_agg" <> pgParens (fromPgExpression v <> emit ", " <> fromPgExpression delim)