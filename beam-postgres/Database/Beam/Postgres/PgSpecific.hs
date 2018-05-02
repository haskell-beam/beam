{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}

-- | Postgres-specific types, functions, and operators
module Database.Beam.Postgres.PgSpecific
  ( -- ** Full-text search
    -- $full-text-search

    -- *** @TSVECTOR@ data type
    TsVectorConfig, TsVector(..)
  , toTsVector, english

    -- *** @TSQUERY@ data type
  , TsQuery(..), (@@)
  , toTsQuery

    -- ** @JSON@ and @JSONB@ data types
    -- $json
  , PgJSON(..), PgJSONB(..)
  , IsPgJSON(..)
  , PgJSONEach(..), PgJSONKey(..), PgJSONElement(..)

  , (@>), (<@), (->#), (->$)
  , (->>#), (->>$), (#>), (#>>)
  , (?), (?|), (?&)

  , withoutKey, withoutIdx
  , withoutKeys

  , pgJsonArrayLength
  , pgJsonbUpdate, pgJsonbSet
  , pgJsonbPretty

    -- ** @MONEY@ data type
  , PgMoney(..), pgMoney

  , pgScaleMoney_
  , pgDivideMoney_, pgDivideMoneys_

  , pgAddMoney_, pgSubtractMoney_
  , pgSumMoneyOver_, pgAvgMoneyOver_
  , pgSumMoney_, pgAvgMoney_

    -- ** Set-valued functions
    -- $set-valued-funs
  , PgSetOf, pgUnnest
  , pgUnnestArray, pgUnnestArrayWithOrdinality

    -- ** @ARRAY@ types
    -- $arrays
  , PgArrayValueContext, PgIsArrayContext

    -- *** Building @ARRAY@s
  , array_, arrayOf_, (++.)
  , pgArrayAgg, pgArrayAggOver

    -- *** Array operators and functions
  , (!.), arrayDims_
  , arrayUpper_, arrayLower_
  , arrayUpperUnsafe_, arrayLowerUnsafe_
  , arrayLength_, arrayLengthUnsafe_

  , isSupersetOf_, isSubsetOf_

    -- ** Postgres functions and aggregates
  , pgBoolOr, pgBoolAnd, pgStringAgg, pgStringAggOver

  , pgNubBy_

  , now_, ilike_
  )
where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate ( HasDefaultSqlDataType(..)
                                       , HasDefaultSqlDataTypeConstraints(..) )
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types
import           Database.Beam.Query.Internal
import           Database.Beam.Schema.Tables

import           Control.Monad.Free
import           Control.Monad.State.Strict (evalState, put, get)

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import           Data.Hashable
import           Data.Proxy
import           Data.Scientific (Scientific, formatScientific, FPFormat(Fixed))
import           Data.String
import qualified Data.Text as T
import           Data.Time (LocalTime)
import           Data.Type.Bool
import qualified Data.Vector as V
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg

import           GHC.TypeLits
import           GHC.Exts hiding (toList)

-- ** Postgres-specific functions

-- | Postgres @NOW()@ function. Returns the server's timestamp
now_ :: QExpr PgExpressionSyntax s LocalTime
now_ = QExpr (\_ -> PgExpressionSyntax (emit "NOW()"))

-- | Postgres @ILIKE@ operator. A case-insensitive version of 'like_'.
ilike_ :: IsSqlExpressionSyntaxStringType PgExpressionSyntax text
       => QExpr PgExpressionSyntax s text
       -> QExpr PgExpressionSyntax s text
       -> QExpr PgExpressionSyntax s Bool
ilike_ (QExpr a) (QExpr b) = QExpr (pgBinOp "ILIKE" <$> a <*> b)

-- ** TsVector type

-- | The type of a document preprocessed for full-text search. The contained
-- 'ByteString' is the Postgres representation of the @TSVECTOR@ type. Use
-- 'toTsVector' to construct these on-the-fly from strings.
--
-- When this field is embedded in a beam table, 'defaultMigratableDbSettings'
-- will give the column the postgres @TSVECTOR@ type.
newtype TsVector = TsVector ByteString
  deriving (Show, Eq, Ord)

-- | The identifier of a Postgres text search configuration.
--
-- Use the 'IsString' instance to construct new values of this type
newtype TsVectorConfig = TsVectorConfig ByteString
  deriving (Show, Eq, Ord, IsString)

instance Pg.FromField TsVector where
  fromField field d =
    if Pg.typeOid field /= Pg.typoid pgTsVectorTypeInfo
    then Pg.returnError Pg.Incompatible field ""
    else case d of
           Just d' -> pure (TsVector d')
           Nothing -> Pg.returnError Pg.UnexpectedNull field ""

instance Pg.ToField TsVector where
  toField (TsVector d) =
    Pg.Many [ Pg.Plain "($$"
            , Pg.Plain (byteString d)
            , Pg.Plain "$$::tsvector)" ]

instance FromBackendRow Postgres TsVector

instance HasSqlEqualityCheck PgExpressionSyntax TsVectorConfig
instance HasSqlQuantifiedEqualityCheck PgExpressionSyntax TsVectorConfig

instance HasSqlEqualityCheck PgExpressionSyntax TsVector
instance HasSqlQuantifiedEqualityCheck PgExpressionSyntax TsVector

-- | A full-text search configuration with sensible defaults for english
english :: TsVectorConfig
english = TsVectorConfig "english"

-- | The Postgres @to_tsvector@ function. Given a configuration and string,
-- return the @TSVECTOR@ that represents the contents of the string.
toTsVector :: IsSqlExpressionSyntaxStringType PgExpressionSyntax str
           => Maybe TsVectorConfig -> QGenExpr context PgExpressionSyntax s str
           -> QGenExpr context PgExpressionSyntax s TsVector
toTsVector Nothing (QExpr x) =
  QExpr (fmap (\(PgExpressionSyntax x') ->
                 PgExpressionSyntax $
                 emit "to_tsvector(" <> x' <> emit ")") x)
toTsVector (Just (TsVectorConfig configNm)) (QExpr x) =
  QExpr (fmap (\(PgExpressionSyntax x') -> PgExpressionSyntax $
                 emit "to_tsvector('" <> escapeString configNm <> emit "', " <> x' <> emit ")") x)

-- | Determine if the given @TSQUERY@ matches the document represented by the
-- @TSVECTOR@. Behaves exactly like the similarly-named operator in postgres.
(@@) :: QGenExpr context PgExpressionSyntax s TsVector
     -> QGenExpr context PgExpressionSyntax s TsQuery
     -> QGenExpr context PgExpressionSyntax s Bool
QExpr vec @@ QExpr q =
  QExpr (pgBinOp "@@" <$> vec <*> q)

-- ** TsQuery type

-- | A query that can be run against a document contained in a 'TsVector'.
--
-- When this field is embedded in a beam table, 'defaultMigratableDbSettings'
-- will give the column the postgres @TSVECTOR@ type
newtype TsQuery = TsQuery ByteString
  deriving (Show, Eq, Ord)

instance HasSqlEqualityCheck PgExpressionSyntax TsQuery
instance HasSqlQuantifiedEqualityCheck PgExpressionSyntax TsQuery

instance Pg.FromField TsQuery where
  fromField field d =
    if Pg.typeOid field /= Pg.typoid pgTsQueryTypeInfo
    then Pg.returnError Pg.Incompatible field ""
    else case d of
           Just d' -> pure (TsQuery d')
           Nothing -> Pg.returnError Pg.UnexpectedNull field ""

instance FromBackendRow Postgres TsQuery

-- | The Postgres @to_tsquery@ function. Given a configuration and string,
-- return the @TSQUERY@ that represents the contents of the string.
toTsQuery :: IsSqlExpressionSyntaxStringType PgExpressionSyntax str
           => Maybe TsVectorConfig -> QGenExpr context PgExpressionSyntax s str
           -> QGenExpr context PgExpressionSyntax s TsQuery
toTsQuery Nothing (QExpr x) =
  QExpr (fmap (\(PgExpressionSyntax x') ->
                 PgExpressionSyntax $
                 emit "to_tsquery(" <> x' <> emit ")") x)
toTsQuery (Just (TsVectorConfig configNm)) (QExpr x) =
  QExpr (fmap (\(PgExpressionSyntax x') -> PgExpressionSyntax $
                 emit "to_tsquery('" <> escapeString configNm <> emit "', " <> x' <> emit ")") x)

-- ** Array operators

-- TODO this should be robust to slices

-- | Index into the given array. This translates to the @<array>[<index>]@
-- syntax in postgres. The beam operator name has been chosen to match the
-- 'Data.Vector.(!)' operator.
(!.) :: Integral ix
     => QGenExpr context PgExpressionSyntax s (V.Vector a)
     -> QGenExpr context PgExpressionSyntax s ix
     -> QGenExpr context PgExpressionSyntax s a
QExpr v !. QExpr ix =
  QExpr (index <$> v <*> ix)
  where
    index (PgExpressionSyntax v') (PgExpressionSyntax ix') =
      PgExpressionSyntax (emit "(" <> v' <> emit ")[" <> ix' <> emit "]")

-- | Postgres @array_dims()@ function. Returns a textual representation of the
-- dimensions of the array.
arrayDims_ :: IsSqlExpressionSyntaxStringType PgExpressionSyntax text
           => QGenExpr context PgExpressionSyntax s (V.Vector a)
           -> QGenExpr context PgExpressionSyntax s text
arrayDims_ (QExpr v) = QExpr (fmap (\(PgExpressionSyntax v') -> PgExpressionSyntax (emit "array_dims(" <> v' <> emit ")")) v)

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

-- | Return the upper or lower bound of the given array at the given dimension
--  (statically supplied as a type application on a 'GHC.TypeLits.Nat'). Note
--  that beam will attempt to statically determine if the dimension is in range.
--  GHC errors will be thrown if this cannot be proved.
--
-- For example, to get the upper bound of the 2nd-dimension of an array:
--
-- @
-- arrayUpper_ @2 vectorValuedExpression
-- @
arrayUpper_, arrayLower_
  :: forall (dim :: Nat) context num v s.
     (KnownNat dim, WithinBounds dim (V.Vector v), Integral num)
  => QGenExpr context PgExpressionSyntax s (V.Vector v)
  -> QGenExpr context PgExpressionSyntax s num
arrayUpper_ v =
  unsafeRetype (arrayUpperUnsafe_ v (val_ (natVal (Proxy @dim) :: Integer)) :: QGenExpr context PgExpressionSyntax s (Maybe Integer))
arrayLower_ v =
  unsafeRetype (arrayLowerUnsafe_ v (val_ (natVal (Proxy @dim) :: Integer)) :: QGenExpr context PgExpressionSyntax s (Maybe Integer))

-- | These functions can be used to find the lower and upper bounds of an array
-- where the dimension number is not known until run-time. They are marked
-- unsafe because they may cause query processing to fail at runtime, even if
-- they typecheck successfully.
arrayUpperUnsafe_, arrayLowerUnsafe_
  :: (Integral dim, Integral length)
  => QGenExpr context PgExpressionSyntax s (V.Vector v)
  -> QGenExpr context PgExpressionSyntax s dim
  -> QGenExpr context PgExpressionSyntax s (Maybe length)
arrayUpperUnsafe_ (QExpr v) (QExpr dim) =
  QExpr (fmap (PgExpressionSyntax . mconcat) . sequenceA $
         [ pure (emit "array_upper(")
         , fromPgExpression <$> v
         , pure (emit ", ")
         , fromPgExpression <$> dim
         , pure (emit ")") ])
arrayLowerUnsafe_ (QExpr v) (QExpr dim) =
  QExpr (fmap (PgExpressionSyntax . mconcat) . sequenceA $
         [ pure (emit "array_lower(")
         , fromPgExpression <$> v
         , pure (emit ", ")
         , fromPgExpression <$> dim
         , pure (emit ")") ])

-- | Get the size of the array at the given (statically known) dimension,
-- provided as a type-level 'Nat'. Like the 'arrayUpper_' and 'arrayLower_'
-- functions,throws a compile-time error if the dimension is out of bounds.
arrayLength_
  :: forall (dim :: Nat) ctxt num v s.
     (KnownNat dim, WithinBounds dim (V.Vector v), Integral num)
  => QGenExpr ctxt PgExpressionSyntax s (V.Vector v)
  -> QGenExpr ctxt PgExpressionSyntax s num
arrayLength_ v =
  unsafeRetype (arrayLengthUnsafe_ v (val_ (natVal (Proxy @dim) :: Integer)) :: QGenExpr ctxt PgExpressionSyntax s (Maybe Integer))

-- | Get the size of an array at a dimension not known until run-time. Marked
-- unsafe as this may cause runtime errors even if it type checks.
arrayLengthUnsafe_
  :: (Integral dim, Integral num)
  => QGenExpr ctxt PgExpressionSyntax s (V.Vector v)
  -> QGenExpr ctxt PgExpressionSyntax s dim
  -> QGenExpr ctxt PgExpressionSyntax s (Maybe num)
arrayLengthUnsafe_ (QExpr a) (QExpr dim) =
  QExpr $ fmap (PgExpressionSyntax . mconcat) $ sequenceA $
  [ pure (emit "array_length(")
  , fromPgExpression <$> a
  , pure (emit ", ")
  , fromPgExpression <$> dim
  , pure (emit ")") ]

-- | The Postgres @&#x40;>@ operator. Returns true if every member of the second
-- array is present in the first.
isSupersetOf_ :: QGenExpr ctxt PgExpressionSyntax s (V.Vector a)
              -> QGenExpr ctxt PgExpressionSyntax s (V.Vector a)
              -> QGenExpr ctxt PgExpressionSyntax s Bool
isSupersetOf_ (QExpr haystack) (QExpr needles) =
  QExpr (pgBinOp "@>" <$> haystack <*> needles)

-- | The Postgres @<&#x40;@ operator. Returns true if every member of the first
-- array is present in the second.
isSubsetOf_ :: QGenExpr ctxt PgExpressionSyntax s (V.Vector a)
            -> QGenExpr ctxt PgExpressionSyntax s (V.Vector a)
            -> QGenExpr ctxt PgExpressionSyntax s Bool
isSubsetOf_ (QExpr needles) (QExpr haystack) =
  QExpr (pgBinOp "<@" <$> needles <*> haystack)

-- | Postgres @||@ operator. Concatenates two vectors and returns their result.
(++.) :: QGenExpr ctxt PgExpressionSyntax s (V.Vector a)
      -> QGenExpr ctxt PgExpressionSyntax s (V.Vector a)
      -> QGenExpr ctxt PgExpressionSyntax s (V.Vector a)
QExpr a ++. QExpr b =
  QExpr (pgBinOp "||" <$> a <*> b)

-- ** Array expressions

-- | An expression context that determines which types of expressions can be put
-- inside an array element. Any scalar, aggregate, or window expression can be
-- placed within an array.
data PgArrayValueContext

-- | If you are extending beam-postgres and provide another expression context
-- that can be represented in an array, provide an empty instance of this class.
class PgIsArrayContext ctxt where
  mkArraySyntax :: Proxy ctxt -> PgSyntax -> PgSyntax
  mkArraySyntax _ s = emit "ARRAY" <> s
instance PgIsArrayContext PgArrayValueContext where
  mkArraySyntax _ = id
instance PgIsArrayContext QValueContext
instance PgIsArrayContext QAggregateContext
instance PgIsArrayContext QWindowingContext

-- | Build a 1-dimensional postgres array from an arbitrary 'Foldable'
-- containing expressions.
array_ :: forall context f s a.
          (PgIsArrayContext context, Foldable f)
       => f (QGenExpr PgArrayValueContext PgExpressionSyntax s a)
       -> QGenExpr context PgExpressionSyntax s (V.Vector a)
array_ vs =
  QExpr $ fmap (PgExpressionSyntax . mkArraySyntax (Proxy @context) . mconcat) $
  sequenceA [ pure (emit "[")
            , pgSepBy (emit ", ") <$> mapM (\(QExpr e) -> fromPgExpression <$> e) (toList vs)
            , pure (emit "]") ]

-- | Build a 1-dimensional postgres array from a subquery
arrayOf_ :: Q PgSelectSyntax db s (QExpr PgExpressionSyntax s a)
         -> QGenExpr context PgExpressionSyntax s (V.Vector a)
arrayOf_ q =
  let QExpr sub = subquery_ q
  in QExpr (\t -> let PgExpressionSyntax sub' = sub t
                  in PgExpressionSyntax (emit "ARRAY(" <> sub' <> emit ")"))

-- ** JSON

-- | The Postgres @JSON@ type, which stores textual values that represent JSON
-- objects. The type parameter indicates the Haskell type which the JSON
-- encodes. This type must be a member of 'FromJSON' and 'ToJSON' in order for
-- deserialization and serialization to work as expected.
--
-- The 'defaultMigratableDbSettings' function automatically assigns the postgres
-- @JSON@ type to fields with this type.
newtype PgJSON a = PgJSON a
  deriving ( Show, Eq, Ord, Hashable, Monoid )

instance HasSqlEqualityCheck PgExpressionSyntax (PgJSON a)
instance HasSqlQuantifiedEqualityCheck PgExpressionSyntax (PgJSON a)

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
    emit "'" <> escapeString (BL.toStrict (encode a)) <> emit "'::json"

-- | The Postgres @JSONB@ type, which stores JSON-encoded data in a
-- postgres-specific binary format. Like 'PgJSON', the type parameter indicates
-- the Hgaskell type which the JSON encodes.
--
-- Fields with this type are automatically given the Postgres @JSONB@ type
newtype PgJSONB a = PgJSONB a
  deriving ( Show, Eq, Ord, Hashable, Monoid )

instance HasSqlEqualityCheck PgExpressionSyntax (PgJSONB a)
instance HasSqlQuantifiedEqualityCheck PgExpressionSyntax (PgJSONB a)

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
    emit "'" <> escapeString (BL.toStrict (encode a)) <> emit "'::jsonb"

-- | Key-value pair, used as output of 'pgJsonEachText' and 'pgJsonEach'
data PgJSONEach valType f
  = PgJSONEach
  { pgJsonEachKey :: C f T.Text
  , pgJsonEachValue :: C f valType
  } deriving Generic
instance Beamable (PgJSONEach valType)

-- | Output row of 'pgJsonKeys'
data PgJSONKey f = PgJSONKey { pgJsonKey :: C f T.Text }
  deriving Generic
instance Beamable PgJSONKey

-- | Output row of 'pgJsonArrayElements' and 'pgJsonArrayElementsText'
data PgJSONElement a f = PgJSONElement { pgJsonElement :: C f a }
  deriving Generic
instance Beamable (PgJSONElement a)

-- | Postgres provides separate @json_@ and @jsonb_@ functions. However, we know
-- what we're dealing with based on the type of data, so we can be less obtuse.
--
-- For more information on how these functions behave, see the Postgres manual
-- section on
-- <https://www.postgresql.org/docs/current/static/functions-json.html JSON>.
--
class IsPgJSON (json :: * -> *) where
  -- | The @json_each@ or @jsonb_each@ function. Values returned as @json@ or
  -- @jsonb@ respectively. Use 'pgUnnest' to join against the result
  pgJsonEach     :: QGenExpr ctxt PgExpressionSyntax s (json a)
                 -> QGenExpr ctxt PgExpressionSyntax s (PgSetOf (PgJSONEach (json Value)))

  -- | Like 'pgJsonEach', but returning text values instead
  pgJsonEachText :: QGenExpr ctxt PgExpressionSyntax s (json a)
                 -> QGenExpr ctxt PgExpressionSyntax s (PgSetOf (PgJSONEach T.Text))

  -- | The @json_object_keys@ and @jsonb_object_keys@ function. Use 'pgUnnest'
  -- to join against the result.
  pgJsonKeys     :: QGenExpr ctxt PgExpressionSyntax s (json a)
                 -> QGenExpr ctxt PgExpressionSyntax s (PgSetOf PgJSONKey)

  -- | The @json_array_elements@ and @jsonb_array_elements@ function. Use
  -- 'pgUnnest' to join against the result
  pgJsonArrayElements :: QGenExpr ctxt PgExpressionSyntax s (json a)
                      -> QGenExpr ctxt PgExpressionSyntax s (PgSetOf (PgJSONElement (json Value)))

  -- | Like 'pgJsonArrayElements', but returning the values as 'T.Text'
  pgJsonArrayElementsText :: QGenExpr ctxt PgExpressionSyntax s (json a)
                          -> QGenExpr ctxt PgExpressionSyntax s (PgSetOf (PgJSONElement T.Text))
--  pgJsonToRecord
--  pgJsonToRecordSet

  -- | The @json_typeof@ or @jsonb_typeof@ function
  pgJsonTypeOf :: QGenExpr ctxt PgExpressionSyntax s (json a) -> QGenExpr ctxt PgExpressionSyntax s T.Text

  -- | The @json_strip_nulls@ or @jsonb_strip_nulls@ function.
  pgJsonStripNulls :: QGenExpr ctxt PgExpressionSyntax s (json a) -> QGenExpr ctxt PgExpressionSyntax s (json b)

  -- | The @json_agg@ or @jsonb_agg@ aggregate.
  pgJsonAgg :: QExpr PgExpressionSyntax s a -> QAgg PgExpressionSyntax s (json a)

  -- | The @json_object_agg@ or @jsonb_object_agg@. The first argument gives the
  -- key source and the second the corresponding values.
  pgJsonObjectAgg :: QExpr PgExpressionSyntax s key -> QExpr PgExpressionSyntax s value
                  -> QAgg PgExpressionSyntax s (json a)

instance IsPgJSON PgJSON where
  pgJsonEach (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_each") . pgParens . fromPgExpression) a

  pgJsonEachText (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_each_text") . pgParens . fromPgExpression) a

  pgJsonKeys (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_object_keys") . pgParens . fromPgExpression) a

  pgJsonArrayElements (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_array_elements") . pgParens . fromPgExpression) a

  pgJsonArrayElementsText (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_array_elements_text") . pgParens . fromPgExpression) a

  pgJsonTypeOf (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_typeof") . pgParens . fromPgExpression) a

  pgJsonStripNulls (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_strip_nulls") . pgParens . fromPgExpression) a

  pgJsonAgg (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_agg") . pgParens . fromPgExpression) a

  pgJsonObjectAgg (QExpr keys) (QExpr values) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "json_object_agg") . pgParens . mconcat) $
    sequenceA $ [ fromPgExpression <$> keys, pure (emit ", ")
                , fromPgExpression <$> values ]

instance IsPgJSON PgJSONB where
  pgJsonEach (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_each") . pgParens . fromPgExpression) a

  pgJsonEachText (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_each_text") . pgParens . fromPgExpression) a

  pgJsonKeys (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_object_keys") . pgParens . fromPgExpression) a

  pgJsonArrayElements (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_array_elements") . pgParens . fromPgExpression) a

  pgJsonArrayElementsText (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_array_elements_text") . pgParens . fromPgExpression) a

  pgJsonTypeOf (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_typeof") . pgParens . fromPgExpression) a

  pgJsonStripNulls (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_strip_nulls") . pgParens . fromPgExpression) a

  pgJsonAgg (QExpr a) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_agg") . pgParens . fromPgExpression) a

  pgJsonObjectAgg (QExpr keys) (QExpr values) =
    QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_object_agg") . pgParens . mconcat) $
    sequenceA $ [ fromPgExpression <$> keys, pure (emit ", ")
                , fromPgExpression <$> values ]

-- | Postgres @&#x40;>@ and @<&#x40;@ operators for JSON. Return true if the
-- json object pointed to by the arrow is completely contained in the other. See
-- the Postgres documentation for more in formation on what this means.
(@>), (<@) :: IsPgJSON json
           => QGenExpr ctxt PgExpressionSyntax s (json a)
           -> QGenExpr ctxt PgExpressionSyntax s (json b)
           -> QGenExpr ctxt PgExpressionSyntax s Bool
QExpr a @> QExpr b =
  QExpr (pgBinOp "@>" <$> a <*> b)
QExpr a <@ QExpr b =
  QExpr (pgBinOp "<@" <$> a <*> b)

-- | Access a JSON array by index. Corresponds to the Postgres @->@ operator.
-- See '(->$)' for the corresponding operator for object access.
(->#) :: IsPgJSON json
      => QGenExpr ctxt PgExpressionSyntax s (json a)
      -> QGenExpr ctxt PgExpressionSyntax s Int
      -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a -># QExpr b =
  QExpr (pgBinOp "->" <$> a <*> b)

-- | Acces a JSON object by key. Corresponds to the Postgres @->@ operator. See
-- '(->#)' for the corresponding operator for arrays.
(->$) :: IsPgJSON json
      => QGenExpr ctxt PgExpressionSyntax s (json a)
      -> QGenExpr ctxt PgExpressionSyntax s T.Text
      -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a ->$ QExpr b =
  QExpr (pgBinOp "->" <$> a <*> b)

-- | Access a JSON array by index, returning the embedded object as a string.
-- Corresponds to the Postgres @->>@ operator. See '(->>$)' for the
-- corresponding operator on objects.
(->>#) :: IsPgJSON json
       => QGenExpr ctxt PgExpressionSyntax s (json a)
       -> QGenExpr ctxt PgExpressionSyntax s Int
       -> QGenExpr ctxt PgExpressionSyntax s T.Text
QExpr a ->># QExpr b =
  QExpr (pgBinOp "->>" <$> a <*> b)

-- | Access a JSON object by key, returning the embedded object as a string.
-- Corresponds to the Postgres @->>@ operator. See '(->>#)' for the
-- corresponding operator on arrays.
(->>$) :: IsPgJSON json
       => QGenExpr ctxt PgExpressionSyntax s (json a)
       -> QGenExpr ctxt PgExpressionSyntax s T.Text
       -> QGenExpr ctxt PgExpressionSyntax s T.Text
QExpr a ->>$ QExpr b =
  QExpr (pgBinOp "->>" <$> a <*> b)

-- | Access a deeply nested JSON object. The first argument is the JSON object
-- to look within, the second is the path of keys from the first argument to the
-- target. Returns the result as a new json value. Note that the postgres
-- function allows etiher string keys or integer indices, but this function only
-- allows string keys. PRs to improve this functionality are welcome.
(#>) :: IsPgJSON json
     => QGenExpr ctxt PgExpressionSyntax s (json a)
     -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
     -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a #> QExpr b =
  QExpr (pgBinOp "#>" <$> a <*> b)

-- | Like '(#>)' but returns the result as a string.
(#>>) :: IsPgJSON json
      => QGenExpr ctxt PgExpressionSyntax s (json a)
      -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
      -> QGenExpr ctxt PgExpressionSyntax s T.Text
QExpr a #>> QExpr b =
  QExpr (pgBinOp "#>>" <$> a <*> b)

-- | Postgres @?@ operator. Checks if the given string exists as top-level key
-- of the json object.
(?) :: IsPgJSON json
    => QGenExpr ctxt PgExpressionSyntax s (json a)
    -> QGenExpr ctxt PgExpressionSyntax s T.Text
    -> QGenExpr ctxt PgExpressionSyntax s Bool
QExpr a ? QExpr b =
  QExpr (pgBinOp "?" <$> a <*> b)

-- | Postgres @?|@ and @?&@ operators. Check if any or all of the given strings
-- exist as top-level keys of the json object respectively.
(?|), (?&) :: IsPgJSON json
           => QGenExpr ctxt PgExpressionSyntax s (json a)
           -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
           -> QGenExpr ctxt PgExpressionSyntax s Bool
QExpr a ?| QExpr b =
  QExpr (pgBinOp "?|" <$> a <*> b)
QExpr a ?& QExpr b =
  QExpr (pgBinOp "?&" <$> a <*> b)

-- | Postgres @-@ operator on json objects. Returns the supplied json object
-- with the supplied key deleted. See 'withoutIdx' for the corresponding
-- operator on arrays.
withoutKey :: IsPgJSON json
           => QGenExpr ctxt PgExpressionSyntax s (json a)
           -> QGenExpr ctxt PgExpressionSyntax s T.Text
           -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a `withoutKey` QExpr b =
  QExpr (pgBinOp "-" <$> a <*> b)

-- | Postgres @-@ operator on json arrays. See 'withoutKey' for the
-- corresponding operator on objects.
withoutIdx :: IsPgJSON json
           => QGenExpr ctxt PgExpressionSyntax s (json a)
           -> QGenExpr ctxt PgExpressionSyntax s Int
           -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a `withoutIdx` QExpr b =
  QExpr (pgBinOp "-" <$> a <*> b)

-- | Postgres @#-@ operator. Removes all the keys specificied from the JSON
-- object and returns the result.
withoutKeys :: IsPgJSON json
            => QGenExpr ctxt PgExpressionSyntax s (json a)
            -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
            -> QGenExpr ctxt PgExpressionSyntax s (json b)
QExpr a `withoutKeys` QExpr b =
  QExpr (pgBinOp "#-" <$> a <*> b)

-- | Postgres @json_array_length@ function. The supplied json object should be
-- an array, but this isn't checked at compile-time.
pgJsonArrayLength :: IsPgJSON json => QGenExpr ctxt PgExpressionSyntax s (json a)
                  -> QGenExpr ctxt PgExpressionSyntax s Int
pgJsonArrayLength (QExpr a) =
  QExpr $ \tbl ->
  PgExpressionSyntax (emit "json_array_length(" <> fromPgExpression (a tbl) <> emit ")")

-- | The postgres @jsonb_set@ function. 'pgJsonUpdate' expects the value
-- specified by the path in the second argument to exist. If it does not, the
-- first argument is not modified. 'pgJsonbSet' will create any intermediate
-- objects necessary. This corresponds to the @create_missing@ argument of
-- @jsonb_set@ being set to false or true respectively.
pgJsonbUpdate, pgJsonbSet
  :: QGenExpr ctxt PgExpressionSyntax s (PgJSONB a)
  -> QGenExpr ctxt PgExpressionSyntax s (V.Vector T.Text)
  -> QGenExpr ctxt PgExpressionSyntax s (PgJSONB b)
  -> QGenExpr ctxt PgExpressionSyntax s (PgJSONB a)
pgJsonbUpdate (QExpr a) (QExpr path) (QExpr newVal) =
  QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_set") . pgParens . mconcat) $ sequenceA $
  [ fromPgExpression <$> a, pure (emit ", "), fromPgExpression <$> path, pure (emit ", "), fromPgExpression <$> newVal ]
pgJsonbSet (QExpr a) (QExpr path) (QExpr newVal) =
  QExpr $ fmap (PgExpressionSyntax . mappend (emit "jsonb_set") . pgParens . mconcat) $ sequenceA $
  [ fromPgExpression <$> a, pure (emit ", "), fromPgExpression <$> path, pure (emit ", "), fromPgExpression <$> newVal, pure (emit ", true") ]

-- | Postgres @jsonb_pretty@ function
pgJsonbPretty :: QGenExpr ctxt PgExpressionSyntax s (PgJSONB a)
              -> QGenExpr ctxt PgExpressionSyntax s T.Text
pgJsonbPretty (QExpr a) =
  QExpr (\tbl -> PgExpressionSyntax (emit "jsonb_pretty" <> pgParens (fromPgExpression (a tbl))))

-- ** Postgresql aggregates

-- | An aggregate that adds each value to the resulting array. See 'pgArrayOver'
-- if you want to specify a quantifier. Corresponds to the Postgres @ARRAY_AGG@
-- function.
pgArrayAgg :: QExpr PgExpressionSyntax s a
           -> QAgg PgExpressionSyntax s (V.Vector a)
pgArrayAgg = pgArrayAggOver allInGroup_

-- | Postgres @ARRAY_AGG@ with an explicit quantifier. Includes each row that
-- meets the quantification criteria in the result.
pgArrayAggOver :: Maybe PgAggregationSetQuantifierSyntax
               -> QExpr PgExpressionSyntax s a
               -> QAgg PgExpressionSyntax s (V.Vector a)
pgArrayAggOver quantifier (QExpr a) =
  QExpr $ \tbl ->
  PgExpressionSyntax $
    emit "array_agg" <>
    pgParens ( maybe mempty (\q -> fromPgAggregationSetQuantifier q <> emit " ") quantifier <>
               fromPgExpression (a tbl))

-- | Postgres @bool_or@ aggregate. Returns true if any of the rows are true.
pgBoolOr :: QExpr PgExpressionSyntax s a
         -> QAgg PgExpressionSyntax s (Maybe Bool)
pgBoolOr (QExpr a) =
  QExpr $ \tbl -> PgExpressionSyntax $
  emit "bool_or" <> pgParens (fromPgExpression (a tbl))

-- | Postgres @bool_and@ aggregate. Returns false unless every row is true.
pgBoolAnd :: QExpr PgExpressionSyntax s a
          -> QAgg PgExpressionSyntax s (Maybe Bool)
pgBoolAnd (QExpr a) =
  QExpr $ \tbl -> PgExpressionSyntax $
  emit "bool_and" <> pgParens (fromPgExpression (a tbl))

-- *** String aggregations

-- | Joins the string value in each row of the first argument, using the second
-- argument as a delimiter. See 'pgStringAggOver' if you want to provide
-- explicit quantification.
pgStringAgg :: IsSqlExpressionSyntaxStringType PgExpressionSyntax str
            => QExpr PgExpressionSyntax s str
            -> QExpr PgExpressionSyntax s str
            -> QAgg PgExpressionSyntax s (Maybe str)
pgStringAgg = pgStringAggOver allInGroup_

-- | The Postgres @string_agg@ function, with an explicit quantifier. Joins the
-- values of the second argument using the delimiter given by the third.
pgStringAggOver :: IsSqlExpressionSyntaxStringType PgExpressionSyntax str
                => Maybe PgAggregationSetQuantifierSyntax
                -> QExpr PgExpressionSyntax s str
                -> QExpr PgExpressionSyntax s str
                -> QAgg PgExpressionSyntax s (Maybe str)
pgStringAggOver quantifier (QExpr v) (QExpr delim) =
  QExpr $ \tbl -> PgExpressionSyntax $
  emit "string_agg" <>
  pgParens ( maybe mempty (\q -> fromPgAggregationSetQuantifier q <> emit " ") quantifier <>
             fromPgExpression (v tbl) <> emit ", " <>
             fromPgExpression (delim tbl))

-- ** Postgresql SELECT DISTINCT ON

-- | Modify a query to only return rows where the supplied key function returns
-- a unique value. This corresponds to the Postgres @DISTINCT ON@ support.
pgNubBy_ :: ( Projectible PgExpressionSyntax key
            , Projectible PgExpressionSyntax r )
         => (r -> key)
         -> Q PgSelectSyntax db s r
         -> Q PgSelectSyntax db s r
pgNubBy_ mkKey (Q q) = Q $ liftF (QDistinct (\r pfx -> pgSelectSetQuantifierDistinctOn (project (mkKey r) pfx)) q id)

-- ** PostgreSql @MONEY@ data type

-- | Postgres @MONEY@ data type. A simple wrapper over 'ByteString', because
--   Postgres money format is locale-dependent, and we don't handle currency
--   symbol placement, digit grouping, or decimal separation.
--
--   The 'pgMoney' function can be used to convert a number to 'PgMoney'.
newtype PgMoney = PgMoney { fromPgMoney :: ByteString }
  deriving (Show, Read, Eq, Ord)

instance Pg.FromField PgMoney where
 fromField field Nothing = Pg.returnError Pg.UnexpectedNull field ""
 fromField field (Just d) =
   if Pg.typeOid field /= Pg.typoid Pg.money
   then Pg.returnError Pg.Incompatible field ""
   else pure (PgMoney d)
instance Pg.ToField PgMoney where
  toField (PgMoney a) = Pg.toField a

instance HasSqlEqualityCheck PgExpressionSyntax PgMoney
instance HasSqlQuantifiedEqualityCheck PgExpressionSyntax PgMoney

instance FromBackendRow Postgres PgMoney
instance HasSqlValueSyntax PgValueSyntax PgMoney where
  sqlValueSyntax (PgMoney a) = sqlValueSyntax a

-- | Attempt to pack a floating point value as a 'PgMoney' value, paying no
-- attention to the locale-dependent currency symbol, digit grouping, or decimal
-- point. This will use the @.@ symbol as the decimal separator.
pgMoney :: Real a => a -> PgMoney
pgMoney val = PgMoney (BC.pack (formatScientific Fixed Nothing exactVal))
  where
    exactVal = fromRational (toRational val) :: Scientific

-- | Multiply a @MONEY@ value by a numeric value. Corresponds to the Postgres
-- @*@ operator.
pgScaleMoney_ :: Num a
              => QGenExpr context PgExpressionSyntax s a
              -> QGenExpr context PgExpressionSyntax s PgMoney
              -> QGenExpr context PgExpressionSyntax s PgMoney
pgScaleMoney_ (QExpr scale) (QExpr v) =
  QExpr (pgBinOp "*" <$> scale <*> v)

-- | Divide a @MONEY@ value by a numeric value. Corresponds to Postgres @/@
-- where the numerator has type @MONEY@ and the denominator is a number. If you
-- would like to divide two @MONEY@ values and have their units cancel out, use
-- 'pgDivideMoneys_'.
pgDivideMoney_ :: Num a
               => QGenExpr context PgExpressionSyntax s PgMoney
               -> QGenExpr context PgExpressionSyntax s a
               -> QGenExpr context PgExpressionSyntax s PgMoney
pgDivideMoney_ (QExpr v) (QExpr scale) =
  QExpr (pgBinOp "/" <$> v <*> scale)

-- | Dividing two @MONEY@ value results in a number. Corresponds to Postgres @/@
-- on two @MONEY@ values. If you would like to divide @MONEY@ by a scalar, use 'pgDivideMoney_'
pgDivideMoneys_ :: Num a
                => QGenExpr context PgExpressionSyntax s PgMoney
                -> QGenExpr context PgExpressionSyntax s PgMoney
                -> QGenExpr context PgExpressionSyntax s a
pgDivideMoneys_ (QExpr a) (QExpr b) =
  QExpr (pgBinOp "/" <$> a <*> b)

-- | Postgres @+@ and @-@ operators on money.
pgAddMoney_, pgSubtractMoney_
  :: QGenExpr context PgExpressionSyntax s PgMoney
  -> QGenExpr context PgExpressionSyntax s PgMoney
  -> QGenExpr context PgExpressionSyntax s PgMoney
pgAddMoney_ (QExpr a) (QExpr b) =
  QExpr (pgBinOp "+" <$> a <*> b)
pgSubtractMoney_ (QExpr a) (QExpr b) =
  QExpr (pgBinOp "-" <$> a <*> b)

-- | The Postgres @MONEY@ type can be summed or averaged in an aggregation.
-- These functions provide the quantified aggregations. See 'pgSumMoney_' and
-- 'pgAvgMoney_' for the unquantified versions.
pgSumMoneyOver_, pgAvgMoneyOver_
  :: Maybe PgAggregationSetQuantifierSyntax
  -> QExpr PgExpressionSyntax s PgMoney -> QExpr PgExpressionSyntax s PgMoney
pgSumMoneyOver_ q (QExpr a) = QExpr (sumE q <$> a)
pgAvgMoneyOver_ q (QExpr a) = QExpr (avgE q <$> a)

-- | The Postgres @MONEY@ type can be summed or averaged in an aggregation. To
-- provide an explicit quantification, see 'pgSumMoneyOver_' and
-- 'pgAvgMoneyOver_'.
pgSumMoney_, pgAvgMoney_ :: QExpr PgExpressionSyntax s PgMoney
                         -> QExpr PgExpressionSyntax s PgMoney
pgSumMoney_ = pgSumMoneyOver_ allInGroup_
pgAvgMoney_ = pgAvgMoneyOver_ allInGroup_

-- ** Set-valued functions

data PgSetOf (tbl :: (* -> *) -> *)

pgUnnest' :: forall tbl db s
           . Beamable tbl
          => (TablePrefix -> PgSyntax)
          -> Q PgSelectSyntax db s (QExprTable PgExpressionSyntax s tbl)
pgUnnest' q =
  Q (liftF (QAll (\pfx alias ->
                    PgFromSyntax . mconcat $
                    [ q pfx, emit " "
                    , pgQuotedIdentifier alias
                    , pgParens (pgSepBy (emit ", ") (allBeamValues (\(Columnar' (TableField nm)) -> pgQuotedIdentifier nm) tblFields))
                    ])
                 tblFields
                 (\_ -> Nothing) snd))
  where
    tblFields :: TableSettings tbl
    tblFields =
      evalState (zipBeamFieldsM (\_ _ ->
                                   do i <- get
                                      put (i + 1)
                                      pure (Columnar' (TableField (fromString ("r" ++ show i)))))
                                tblSkeleton tblSkeleton) (0 :: Int)

pgUnnest :: forall tbl db s
          . Beamable tbl
         => QExpr PgExpressionSyntax s (PgSetOf tbl)
         -> Q PgSelectSyntax db s (QExprTable PgExpressionSyntax s tbl)
pgUnnest (QExpr q) =
  pgUnnest' (\t -> pgParens (fromPgExpression (q t)))

data PgUnnestArrayTbl a f = PgUnnestArrayTbl (C f a)
  deriving Generic
instance Beamable (PgUnnestArrayTbl a)

pgUnnestArray :: QExpr PgExpressionSyntax s (V.Vector a)
              -> Q PgSelectSyntax db s (QExpr PgExpressionSyntax s a)
pgUnnestArray (QExpr q) =
  fmap (\(PgUnnestArrayTbl x) -> x) $
  pgUnnest' (\t -> emit "UNNEST" <> pgParens (fromPgExpression (q t)))

data PgUnnestArrayWithOrdinalityTbl a f = PgUnnestArrayWithOrdinalityTbl (C f Int) (C f a)
  deriving Generic
instance Beamable (PgUnnestArrayWithOrdinalityTbl a)

pgUnnestArrayWithOrdinality :: QExpr PgExpressionSyntax s (V.Vector a)
                            -> Q PgSelectSyntax db s (QExpr PgExpressionSyntax s Int, QExpr PgExpressionSyntax s a)
pgUnnestArrayWithOrdinality (QExpr q) =
  fmap (\(PgUnnestArrayWithOrdinalityTbl i x) -> (i, x)) $
  pgUnnest' (\t -> emit "UNNEST" <> pgParens (fromPgExpression (q t)) <> emit " WITH ORDINALITY")

instance HasDefaultSqlDataType PgDataTypeSyntax TsQuery where
  defaultSqlDataType _ _ = pgTsQueryType
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax TsQuery

instance HasDefaultSqlDataType PgDataTypeSyntax TsVector where
  defaultSqlDataType _ _ = pgTsVectorType
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax TsVector

instance HasDefaultSqlDataType PgDataTypeSyntax (PgJSON a) where
  defaultSqlDataType _ _ = pgJsonType
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax (PgJSON a)

instance HasDefaultSqlDataType PgDataTypeSyntax (PgJSONB a) where
  defaultSqlDataType _ _ = pgJsonbType
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax (PgJSONB a)

instance HasDefaultSqlDataType PgDataTypeSyntax PgMoney where
  defaultSqlDataType _ _ = pgMoneyType
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax PgMoney

instance HasDefaultSqlDataType PgDataTypeSyntax a => HasDefaultSqlDataType PgDataTypeSyntax (V.Vector a) where
  defaultSqlDataType _ embedded = pgUnboundedArrayType (defaultSqlDataType (Proxy :: Proxy a) embedded)
instance HasDefaultSqlDataTypeConstraints PgColumnSchemaSyntax (V.Vector a)

-- $full-text-search
--
-- Postgres has comprehensive, and thus complicated, support for full text
-- search. The types and functions in this section map closely to the underlying
-- Postgres API, which is described in the
-- <https://www.postgresql.org/docs/current/static/textsearch-intro.html documentation>.
--

-- $arrays
--
-- The functions and types in this section map Postgres @ARRAY@ types to
-- Haskell. An array is serialized and deserialized to a 'Data.Vector.Vector'
-- object. This type most closely matches the semantics of Postgres @ARRAY@s. In
-- general, the names of functions in this section closely match names of the
-- native Postgres functions they map to. As with most beam expression
-- functions, names are suffixed with an underscore and CamelCased.
--
-- Note that Postgres supports arbitrary nesting of vectors. For example, two,
-- three, or higher dimensional arrays can be expressed, manipulated, and stored
-- in tables. Beam fully supports this use case. A two-dimensional postgres
-- array is represented as @Vector (Vector a)@. Simply nest another 'Vector' for
-- higher dimensions. Some functions that return data on arrays expect a
-- dimension number as a parameter. Since beam can check the dimension at
-- compile time, these functions expect a type-level 'Nat' in the expression
-- DSL. The unsafe versions of these functions are also provided with the
-- @Unsafe_@ suffix. The safe versions are guaranteed not to fail at run-time
-- due to dimension mismatches, the unsafe ones may.
--
-- For more information on Postgres array support, refer to the postgres
-- <https://www.postgresql.org/docs/current/static/functions-array.html manual>.

-- $json
--
-- Postgres supports storing JSON in columns, as either a text-based type
-- (@JSON@) or a specialized binary encoding (@JSONB@). @beam-postgres@
-- accordingly provides the 'PgJSON' and 'PgJSONB' data types. Each of these
-- types takes a type parameter indicating the Haskell object represented by the
-- JSON object stored in the column. In order for serialization to work, be sure
-- to provide 'FromJSON' and 'ToJSON' instances for this type. If you do not
-- know the shape of the data stored, substitute 'Value' for this type
-- parameter.
--
-- For more information on Psotgres json support see the postgres
-- <https://www.postgresql.org/docs/current/static/functions-json.html manual>.


-- $set-valued-funs
--
-- Postgres supports functions that returns /sets/. We can join directly against
-- these sets or arrays. @beam-postgres@ supports this feature via the
-- 'pgUnnest' and 'pgUnnestArray' functions.
--
-- Any function that returns a set can be typed as an expression returning
-- 'PgSetOf'. This polymorphic type takes one argument, which is a 'Beamable'
-- type that represents the shape of the data in the rows. For example, the
-- @json_each@ function returns a key and a value, so the corresponding
-- @beam-postgres@ function ('pgJsonEach') returns a value of type 'PgSetOf
-- (PgJSONEach Value)', which represents a set containing 'PgJSONEach'
-- rows. 'PgJSONEach' is a table with a column for keys ('pgJsonEachKey') and
-- one for values ('pgJsonEachValue').
--
-- Any 'PgSetOf' value can be introduced into the 'Q' monad using the 'pgUnnest'
-- function.
--
-- Postgres arrays (represented by the 'V.Vector' type) can also be joined
-- against using the 'pgUnnestArray' function. This directly corresponds to the
-- SQL @UNNEST@ keyword. Unlike sets, arrays have a sense of order. The
-- 'pgUnnestArrayWithOrdinality' function allows you to join against the
-- elements of an array along with its index. This corresponds to the
-- @UNNEST .. WITH ORDINALITY@ clause.

