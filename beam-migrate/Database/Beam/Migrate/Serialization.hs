{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- | Serialization and deserialization helpers for beam data types.
--
-- Used to read and write machine-readable schema descriptions.

module Database.Beam.Migrate.Serialization
       ( -- * Serialization helpers
         -- $serialization
         BeamSerializedDataType(..)
       , BeamSerializedConstraintDefinition(..)
       , BeamSerializedConstraintAttributes(..)
       , BeamSerializedConstraint(..)
       , BeamSerializedMatchType(..)
       , BeamSerializedReferentialAction(..)
       , BeamSerializedExpression(..)

       , beamSerializeJSON, serializePrecAndDecimal

       -- * Deserialization helpers
       -- $deserialization

       , BeamDeserializers(..)

       , beamDeserialize, beamDeserializeMaybe
       , beamDeserializer, sql92Deserializers
       , sql99DataTypeDeserializers
       , sql2003BinaryAndVarBinaryDataTypeDeserializers
       , sql2008BigIntDataTypeDeserializers
       ) where

import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.SQL.SQL92
import           Database.Beam.Migrate.SQL.Types

import           Control.Applicative
import           Control.Monad

import           Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as DAK
#endif
import           Data.Aeson.Types (Parser)
import qualified Data.Dependent.Map as D
import qualified Data.GADT.Compare as D
import           Data.Text (Text, unpack)
import           Data.Typeable (Typeable, (:~:)( Refl ), eqT, typeRep, typeOf)
import qualified Data.Vector as V
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

-- * Serialization helpers

-- | An 'IsSql92DataTypeSyntax' for JSON. Supports all superclasses of
-- `IsSql92DataTypeSyntax` declared in @beam-core@.
newtype BeamSerializedDataType
  = BeamSerializedDataType { fromBeamSerializedDataType :: Value }
  deriving (Show, Eq)

instance IsSql92DataTypeSyntax BeamSerializedDataType where
  domainType nm = BeamSerializedDataType (object [ "domain" .= nm])
  charType prec collation =
    BeamSerializedDataType (object [ "char" .= object [ "prec" .= prec
                                                      , "collation" .= collation ]])
  varCharType prec collation =
    BeamSerializedDataType (object [ "varchar" .= object [ "prec" .= prec
                                                         , "collation" .= collation ]])
  nationalCharType prec =
    BeamSerializedDataType (object [ "national-char" .= object [ "prec" .= prec ]])
  nationalVarCharType prec =
    BeamSerializedDataType (object [ "national-varchar" .= object [ "prec" .= prec ]])

  bitType prec =
    BeamSerializedDataType (object [ "bit" .= object [ "prec" .= prec ]])
  varBitType prec =
    BeamSerializedDataType (object [ "varbit" .= object [ "prec" .= prec ]])

  numericType precAndDecimal =
    BeamSerializedDataType (object [ "numeric" .= serializePrecAndDecimal precAndDecimal ])
  decimalType precAndDecimal =
    BeamSerializedDataType (object [ "decimal" .= serializePrecAndDecimal precAndDecimal ])

  intType = BeamSerializedDataType "int"
  smallIntType = BeamSerializedDataType "smallint"
  floatType prec =
    BeamSerializedDataType (object [ "float" .= object [ "prec" .= prec ] ])
  doubleType = BeamSerializedDataType "double"
  realType = BeamSerializedDataType "real"

  dateType = BeamSerializedDataType "date"
  timeType prec withTz =
    BeamSerializedDataType (object [ "time" .= object [ "prec" .= prec
                                                      , "timezone" .= withTz ]])
  timestampType prec withTz =
    BeamSerializedDataType (object [ "timestamp" .= object [ "prec" .= prec
                                                           , "timezone" .= withTz ]])

instance IsSql99DataTypeSyntax BeamSerializedDataType where
  characterLargeObjectType = BeamSerializedDataType "clob"
  binaryLargeObjectType = BeamSerializedDataType "blob"
  booleanType = BeamSerializedDataType "boolean"
  arrayType ty count = BeamSerializedDataType (object [ "array" .= object [ "of" .= ty
                                                                          , "count" .= count ]])
  rowType tys = BeamSerializedDataType (object [ "row" .= tys ])

instance IsSql2003BinaryAndVarBinaryDataTypeSyntax BeamSerializedDataType where
  binaryType sz = BeamSerializedDataType (object [ "binary" .= sz ])
  varBinaryType sz = BeamSerializedDataType (object [ "varbinary" .= sz ])

instance IsSql2008BigIntDataTypeSyntax BeamSerializedDataType where
  bigIntType = BeamSerializedDataType "bigint"

instance ToJSON BeamSerializedDataType where
  toJSON = fromBeamSerializedDataType

-- | 'IsSql92ColumnConstraintDefinitionSyntax' type for JSON
newtype BeamSerializedConstraintDefinition
  = BeamSerializedConstraintDefinition
  { fromBeamSerializedConstraintDefinition :: Value
  } deriving (Show, Eq)

-- | 'IsSql92ConstraintAttributesSyntax' type for JSON
newtype BeamSerializedConstraintAttributes
  = BeamSerializedConstraintAttributes
  { fromBeamSerializedConstraintAttributes :: [ Value ]
  } deriving (Show, Eq, Monoid, Semigroup)

-- | 'IsSql92ColumnConstraintSyntax' type for JSON
newtype BeamSerializedConstraint
  = BeamSerializedConstraint
  { fromBeamSerializedConstraint :: Value
  } deriving (Show, Eq)

-- | 'IsSql92MatchTypeSyntax' type for JSON
newtype BeamSerializedMatchType
  = BeamSerializedMatchType
  { fromBeamSerializedMatchType :: Value
  } deriving (Show, Eq)

-- | 'IsSql92ReferentialActionSyntax' type for JSON
newtype BeamSerializedReferentialAction
  = BeamSerializedReferentialAction
  { fromBeamSerializedReferentialAction :: Value
  } deriving (Show, Eq)

-- | 'IsSql92ExpressionSyntax' is too complex for us to store in JSON.
-- Additionally, many backends provide substantial amounts of extensions to the
-- syntax that would make storing this highly unfeasible. Expressions are
-- therefore represented as their full text rendering.
--
-- This means that expressions only match as equal if they match /exactly/.
-- While this may seem overly pedantic, it's not much of a concern if your
-- migrations are generated solely by @beam-migrate@. If you've modified the
-- schema yourself, you may have to use 'IsCustomSqlSyntax' to provide an exact
-- expression.
newtype BeamSerializedExpression
  = BeamSerializedExpression
  { fromBeamSerializedExpression :: Text
  } deriving (Show, Eq)

instance IsSql92ColumnConstraintDefinitionSyntax BeamSerializedConstraintDefinition where
  type Sql92ColumnConstraintDefinitionAttributesSyntax BeamSerializedConstraintDefinition =
    BeamSerializedConstraintAttributes
  type Sql92ColumnConstraintDefinitionConstraintSyntax BeamSerializedConstraintDefinition =
    BeamSerializedConstraint

  constraintDefinitionSyntax nm constraint attrs =
    BeamSerializedConstraintDefinition $
    object [ "name" .= nm
           , "attributes" .= fmap fromBeamSerializedConstraintAttributes attrs
           , "constraint" .= fromBeamSerializedConstraint constraint ]

instance IsSql92ColumnConstraintSyntax BeamSerializedConstraint where
  type Sql92ColumnConstraintMatchTypeSyntax BeamSerializedConstraint =
    BeamSerializedMatchType
  type Sql92ColumnConstraintReferentialActionSyntax BeamSerializedConstraint =
    BeamSerializedReferentialAction
  type Sql92ColumnConstraintExpressionSyntax BeamSerializedConstraint =
    BeamSerializedExpression

  notNullConstraintSyntax = BeamSerializedConstraint "not-null"
  uniqueColumnConstraintSyntax = BeamSerializedConstraint "unique"
  primaryKeyColumnConstraintSyntax = BeamSerializedConstraint "primary-key"
  checkColumnConstraintSyntax e = BeamSerializedConstraint (object [ "check-column" .= fromBeamSerializedExpression e])
  referencesConstraintSyntax tbl fields matchType onUpdate onDelete =
    BeamSerializedConstraint (object [ "references" .=
                                         object [ "table" .= tbl, "fields" .= fields
                                                , "match-type" .= fmap fromBeamSerializedMatchType matchType
                                                , "on-update"  .= fmap fromBeamSerializedReferentialAction onUpdate
                                                , "on-delete"  .= fmap fromBeamSerializedReferentialAction onDelete ] ])

instance IsSql92MatchTypeSyntax BeamSerializedMatchType where
  fullMatchSyntax = BeamSerializedMatchType "full"
  partialMatchSyntax = BeamSerializedMatchType "partial"

instance IsSql92ReferentialActionSyntax BeamSerializedReferentialAction where
  referentialActionCascadeSyntax = BeamSerializedReferentialAction "cascade"
  referentialActionSetNullSyntax = BeamSerializedReferentialAction "set-null"
  referentialActionSetDefaultSyntax = BeamSerializedReferentialAction "set-default"
  referentialActionNoActionSyntax = BeamSerializedReferentialAction "nothing"

instance IsSql92ConstraintAttributesSyntax BeamSerializedConstraintAttributes where
  initiallyDeferredAttributeSyntax = BeamSerializedConstraintAttributes [ "initially-deferred" ]
  initiallyImmediateAttributeSyntax = BeamSerializedConstraintAttributes [ "initially-immediate" ]
  notDeferrableAttributeSyntax = BeamSerializedConstraintAttributes [ "not-deferrable" ]
  deferrableAttributeSyntax = BeamSerializedConstraintAttributes [ "deferrable" ]

-- | Some backends serialize data that can only be read by that backend. If so,
-- they should wrap these data in 'beamSerializeJSON', which provides a standard
-- syntax for specifying backend specific data, as well as which backend the
-- data are valid for.
--
-- The first argument is a string that is unique to a given backend
beamSerializeJSON :: Text -> Value -> Value
beamSerializeJSON backend v =
  object [ "be-specific" .= backend
         , "be-data" .= v ]

-- | Helper for serializing the precision and decimal count parameters to
-- 'decimalType', etc.
serializePrecAndDecimal :: Maybe (Word, Maybe Word) -> Value
serializePrecAndDecimal Nothing =
  object []
serializePrecAndDecimal (Just (prec, Nothing)) =
  object [ "prec" .= prec ]
serializePrecAndDecimal (Just (prec, Just decimal)) =
  object [ "prec" .= prec
         , "decimal" .= decimal ]

-- * Deserialization helpers

-- ** Data types

newtype BeamDeserializer syntax
  = BeamDeserializer (forall be. BeamDeserializers be -> Value -> Parser syntax)

-- | Provides a collection of deserializers from aeson 'Value's for arbitrary
-- types. The @cmd@ type parameter is a phantom type parameter. Notionally, all
-- deserializers within this 'BeamDeserializers' relate to the @cmd@ syntax.
newtype BeamDeserializers be
  = BeamDeserializers
  { beamArbitraryDeserializers :: D.DMap BeamDeserializerLabel BeamDeserializer
  }

instance Semigroup (BeamDeserializer be) where
  (<>) = mappend

instance Monoid (BeamDeserializer be) where
  mempty = BeamDeserializer (const (const mzero))
  mappend (BeamDeserializer a) (BeamDeserializer b) =
    BeamDeserializer $ \d o ->
    a d o <|> b d o

instance Semigroup (BeamDeserializers be) where
  (<>) = mappend

instance Monoid (BeamDeserializers be) where
  mempty = BeamDeserializers mempty
  mappend (BeamDeserializers a) (BeamDeserializers b) =
    BeamDeserializers (D.unionWithKey (const mappend) a b)

-- | Helper function to deserialize data from a 'Maybe' 'Value'.
--
-- @
-- beamDeserializeMaybe _ Nothing = pure Nothing
-- beamDeserializeMaybe d (Just v) = Just <$> beamDeserialize d v
-- @
--
beamDeserializeMaybe :: Typeable a
                     => BeamDeserializers be
                     -> Maybe Value
                     -> Parser (Maybe a)
beamDeserializeMaybe _ Nothing = pure Nothing
beamDeserializeMaybe d (Just v) =
  Just <$> beamDeserialize d v

-- | Deserialize the requested type from the given deserializers and aeson 'Value'.
beamDeserialize :: forall a be. Typeable a
                => BeamDeserializers be -> Value
                -> Parser a
beamDeserialize allD@(BeamDeserializers d) v =
  case D.lookup (BeamDeserializerLabel :: BeamDeserializerLabel a) d of
    Nothing -> fail ("beamDeserialize: No deserializer for " ++ show (typeOf (undefined :: a)))
    Just (BeamDeserializer doParse) ->
      doParse allD v

data BeamDeserializerLabel ty where
  BeamDeserializerLabel :: Typeable ty
                        => BeamDeserializerLabel ty
instance D.GEq BeamDeserializerLabel where
  geq a b =
    case D.gcompare a b of
      D.GEQ -> Just Refl
      _ -> Nothing
instance D.GCompare BeamDeserializerLabel where
  gcompare a@(BeamDeserializerLabel :: BeamDeserializerLabel a)
           b@(BeamDeserializerLabel :: BeamDeserializerLabel b) =
    case eqT of
      Just (Refl :: a :~: b)-> D.GEQ
      Nothing ->
        case compare (typeRep a) (typeRep b) of
          LT -> D.GLT
          GT -> D.GGT
          EQ -> error "Impossible"

beamDeserializer :: Typeable ty
                 => (forall be'. BeamDeserializers be' -> Value -> Parser ty)
                 -> BeamDeserializers be
beamDeserializer parse =
  BeamDeserializers (D.singleton BeamDeserializerLabel (BeamDeserializer parse))

-- | Deserializers for SQL92 syntaxes
sql92Deserializers :: forall be
                    . BeamMigrateSqlBackend be
                   => BeamDeserializers be
sql92Deserializers = mconcat
                   [ beamDeserializer deserializeSql92DataType
                   , beamDeserializer deserializeSql92ConstraintDefinition
                   , beamDeserializer deserializeSql92Constraint
                   , beamDeserializer deserializeSql92MatchType
                   , beamDeserializer deserializeSql92ReferentialAction
                   , beamDeserializer deserializeSql92Attributes ]
  where
#if MIN_VERSION_aeson(2,0,0)
    makeKey = DAK.fromText
#else
    makeKey = id
#endif
    parseSub nm o key parse =
      withObject (unpack (nm <> "." <> key)) parse =<< o .: makeKey key

    deserializeSql92DataType :: BeamDeserializers be' -> Value
                             -> Parser (BeamSqlBackendDataTypeSyntax be)
    deserializeSql92DataType _ o =
      deserializeSql92DataTypeObject o <|>
      deserializeSql92DataTypeScalar o

    deserializeSql92DataTypeScalar "int" = pure intType
    deserializeSql92DataTypeScalar "smallint" = pure smallIntType
    deserializeSql92DataTypeScalar "double" = pure dateType
    deserializeSql92DataTypeScalar "real" = pure realType
    deserializeSql92DataTypeScalar "date" = pure dateType
    deserializeSql92DataTypeScalar _ = mzero

    deserializeSql92DataTypeObject =
      withObject "Sql92DataType" $ \o ->
      let (==>) = parseSub "Sql92DataType" o
      in (domainType <$> o .: "domain") <|>
         ("char" ==> \v ->
             charType <$> v .: "prec" <*> v .: "collation") <|>
         ("varchar" ==> \v ->
             varCharType <$> v .: "prec" <*> v .: "collation") <|>
         ("national-char" ==> \v ->
             nationalCharType <$> v .: "prec") <|>
         ("national-varchar" ==> \v ->
             nationalVarCharType <$> v .: "prec") <|>
         ("bit" ==> \v ->
             bitType <$> v .: "prec") <|>
         ("varbit" ==> \v ->
             varBitType <$> v .: "prec") <|>
         ("numeric" ==> \v ->
             numericType <$> deserializePrecAndDecimal v) <|>
         ("decimal" ==> \v ->
             decimalType <$> deserializePrecAndDecimal v) <|>
         ("float" ==> \v ->
             floatType <$> v .: "prec") <|>
         ("time" ==> \v ->
             timeType <$> v .: "prec" <*> v .: "timezone") <|>
         ("timestamp" ==> \v ->
             timestampType <$> v .: "prec" <*> v .: "timezone")

    deserializePrecAndDecimal o =
      Just <$> (((,) <$> o .: "prec" <*> (Just <$> o .: "decimal")) <|>
                ((,Nothing) <$> o .: "prec")) <|>
      pure Nothing

    deserializeSql92ConstraintDefinition :: BeamDeserializers be' -> Value
                                         -> Parser (BeamSqlBackendColumnConstraintDefinitionSyntax be)
    deserializeSql92ConstraintDefinition d =
      withObject "Sql92ColumnConstraintDefinition" $ \o ->
      constraintDefinitionSyntax <$> o .: "name"
                                 <*> (beamDeserialize d =<< o .: "constraint")
                                 <*> (beamDeserializeMaybe d =<< o .: "attributes")

    deserializeSql92Constraint :: BeamDeserializers be' -> Value
                               -> Parser (BeamSqlBackendConstraintSyntax be)
    deserializeSql92Constraint d o =
      case o of
        "not-null" -> pure notNullConstraintSyntax
        "unique" -> pure uniqueColumnConstraintSyntax
        _ -> withObject "Sql92ColumnConstraint" parseObject o
      where
        parseObject v =
          let (==>) = parseSub "Sql92ColumnConstraint" v
          in checkColumnConstraintSyntax <$> (beamDeserialize d =<< v .: "check-column") <|>
             ("references" ==> \v' ->
                 referencesConstraintSyntax <$> v' .: "table" <*> v' .: "fields"
                                            <*> (beamDeserializeMaybe d =<< v' .: "match-type")
                                            <*> (beamDeserializeMaybe d =<< v' .: "on-update")
                                            <*> (beamDeserializeMaybe d =<< v' .: "on-delete"))

    deserializeSql92MatchType :: BeamDeserializers be' -> Value
                              -> Parser (BeamSqlBackendMatchTypeSyntax be)
    deserializeSql92MatchType _ v =
      case v of
        "full" -> pure fullMatchSyntax
        "partial" -> pure partialMatchSyntax
        _ -> mzero

    deserializeSql92ReferentialAction :: BeamDeserializers be' -> Value
                                      -> Parser (BeamSqlBackendReferentialActionSyntax be)
    deserializeSql92ReferentialAction _ v =
      case v of
        "cascade" -> pure referentialActionCascadeSyntax
        "set-null" -> pure referentialActionSetNullSyntax
        "set-default" -> pure referentialActionSetDefaultSyntax
        "nothing" -> pure referentialActionNoActionSyntax
        _ -> mzero

    deserializeSql92Attributes :: BeamDeserializers be' -> Value
                               -> Parser (BeamSqlBackendConstraintAttributesSyntax be)
    deserializeSql92Attributes _ =
      withArray "Sql92Attributes" $ \a ->
      pure (foldr (\o accum ->
                      case o of
                        "initially-deferred" -> initiallyDeferredAttributeSyntax <> accum
                        "initially-immediate" -> initiallyImmediateAttributeSyntax <> accum
                        "not-deferrable" -> notDeferrableAttributeSyntax <> accum
                        "deferrable" -> deferrableAttributeSyntax <> accum
                        _ -> accum
                  ) mempty a)

-- | Deserializes data types that are instances of 'IsSql99DataTypeSyntax'
sql99DataTypeDeserializers
  :: forall be
   . BeamMigrateSql99Backend be
  => BeamDeserializers be
sql99DataTypeDeserializers =
  beamDeserializer $ \d v ->
  fmap (id @(BeamSqlBackendDataTypeSyntax be)) $
  case v of
    "clob" -> pure characterLargeObjectType
    "blob" -> pure binaryLargeObjectType
    _ -> withObject "Sql99DataType" (parseObject d) v
  where
    parseObject d v =
      arrayType <$> (beamDeserialize d =<< v .: "of") <*> v .: "count" <|>
      rowType <$> (do rowTypes <- v .: "row"
                      let parseArray a =
                            forM (V.toList a) $ \a' -> do
                            (nm, a'') <- parseJSON a'
                            (nm,) <$> beamDeserialize d a''
                      withArray "Sql99DataType.rowType" parseArray rowTypes)

-- | Deserialize data types that are instances of 'IsSql2003BinaryAndVarBinaryDataTypeSyntax'
sql2003BinaryAndVarBinaryDataTypeDeserializers
  :: forall be
   . ( BeamMigrateSqlBackend be, BeamSqlT021Backend be )
  => BeamDeserializers be
sql2003BinaryAndVarBinaryDataTypeDeserializers =
  beamDeserializer $ \_ v ->
  fmap (id @(BeamSqlBackendDataTypeSyntax be)) $
  withObject "Sql2003DataType"
    (\o -> (binaryType <$> o .: "binary") <|>
           (varBinaryType <$> o .: "varbinary"))
    v

-- | Deserialize data types that are instance of 'IsSql2008BigIntDataTypeSyntax'
sql2008BigIntDataTypeDeserializers
  :: forall be
   . ( BeamMigrateSqlBackend be, BeamSqlT071Backend be )
  => BeamDeserializers be
sql2008BigIntDataTypeDeserializers =
  beamDeserializer $ \_ v ->
  fmap (id @(BeamSqlBackendDataTypeSyntax be)) $
  case v of
    "bigint" -> pure bigIntType
    _ -> fail "Sql2008DataType.bigint: expected 'bigint'"

-- $serialization
--   Below we provide various instances of Beam SQL syntax types that produce an
--   aeson 'Value' that reflects the call tree. This allows us to read back
--   these data types in various syntaxes.
--
--   Because these are formatted as standard beam syntaxes, backends can easily
--   serialize their data to disk. For an example of what we mean by this, see
--   the instance of 'IsSql92DataTypeSyntax' for 'SqliteDataTypeSyntax' in
--   @beam-sqlite@.


-- $deserialization
--
--   Deserialization requires that knowledge of every type of data we can
--   deserialize is stored in one place. While this is not much of an issue when
--   compiling full Haskell applications, due to the type class mechanism,
--   beam-migrate tools load backends dynamically. This means that we need a
--   separate way to discover deserialization instances for types we care about.
--
--   Values of the 'BeamDeserializers' type represent a set of deserializers all
--   related to one kind of command syntax. You can ask for the deserializers to
--   deserialize any type from an aeson 'Value'. The deserialization will
--   succeed only if a deserializer for the requested type exists and the
--   deserializer was able to parse the 'Value'.
--
--   'BeamDeserializers' compose monoidally. Thus, you can extend any
--   'BeamDeserializers' with your own custom deserializers, by 'mappend'ing it
--   with a new 'BeamDeserializers', created by calling 'beamDeserializer'.
