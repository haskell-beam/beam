{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines common database checks that can be asserted against a database
module Database.Beam.Migrate.Checks where

import           Database.Beam.Migrate.Types.Predicates
import           Database.Beam.Migrate.SQL.SQL92
import           Database.Beam.Backend.SQL

import           Control.Applicative
import           Control.Monad

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Dependent.Map as D
import qualified Data.GADT.Compare as D
import           Data.Hashable
import           Data.Monoid
import           Data.Text (Text, unpack)
import           Data.Typeable (Typeable, (:~:)( Refl ), eqT, cast, typeRep, typeOf)
import qualified Data.Vector as V

import           GHC.Generics hiding (prec)

-- * Table checks

data TableExistsPredicate = TableExistsPredicate Text {- Table name -}
  deriving (Show, Eq, Ord, Typeable, Generic)
instance Hashable TableExistsPredicate
instance DatabasePredicate TableExistsPredicate where
  englishDescription (TableExistsPredicate t) =
    "Table " <> show t <> " must exist"

  serializePredicate (TableExistsPredicate t) =
    object [ "table-exists" .= t ]

  predicateSource _ = PredicateSourceCurBackend

data TableHasColumn syntax where
  TableHasColumn
    :: Typeable (Sql92ColumnSchemaColumnTypeSyntax syntax)
    => Text {- Table name -}
    -> Text {- Column name -}
    -> Sql92ColumnSchemaColumnTypeSyntax syntax {- Data type -}
--    -> Maybe HsDataType
    -> TableHasColumn syntax
instance Hashable (Sql92ColumnSchemaColumnTypeSyntax syntax) => Hashable (TableHasColumn syntax) where
  hashWithSalt salt (TableHasColumn t c s) = hashWithSalt salt (t, c, s)
instance Eq (Sql92ColumnSchemaColumnTypeSyntax syntax) => Eq (TableHasColumn syntax) where
  TableHasColumn aTbl aCol aDt == TableHasColumn bTbl bCol bDt =
    aTbl == bTbl && aCol == bCol && aDt == bDt
instance ( Typeable syntax
         , Sql92SerializableDataTypeSyntax (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Hashable (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Sql92DisplaySyntax (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Eq (Sql92ColumnSchemaColumnTypeSyntax syntax) ) =>
  DatabasePredicate (TableHasColumn syntax) where
  englishDescription (TableHasColumn tbl col type_) =
    "Table " <> show tbl <> " must have a column " <> show col <> " of " <> displaySyntax type_

  predicateSource _ = PredicateSourceCurBackend

  serializePredicate (TableHasColumn tbl col type_) =
    object [ "has-column" .= object [ "table" .= tbl, "column" .= col
                                    , "type" .= serializeDataType type_ ]]

  predicateCascadesDropOn (TableHasColumn tblNm _ _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

data TableColumnHasConstraint syntax
  = TableColumnHasConstraint Text {- Table name -} Text {- Column name -}
                             (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
  deriving Generic
instance Hashable (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) => Hashable (TableColumnHasConstraint syntax)
deriving instance Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) => Eq (TableColumnHasConstraint syntax)
instance ( Typeable syntax
         , Sql92SerializableConstraintDefinitionSyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Hashable (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Sql92DisplaySyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) ) =>
         DatabasePredicate (TableColumnHasConstraint syntax) where
  englishDescription (TableColumnHasConstraint tbl col cns) =
    "Column " <> show tbl <> "." <> show col <> " has constraint " <> displaySyntax cns

  predicateSource _ = PredicateSourceCurBackend
  serializePredicate (TableColumnHasConstraint tbl col cns) =
    object [ "has-column-constraint" .= object [ "table" .= tbl, "column" .= col
                                               , "constraint" .= serializeConstraint cns ] ]

  predicateCascadesDropOn (TableColumnHasConstraint tblNm colNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | Just (TableHasColumn tblNm' colNm' _ :: TableHasColumn syntax) <- cast p' = tblNm' == tblNm && colNm' == colNm
    | otherwise = False

data TableHasPrimaryKey
  = TableHasPrimaryKey Text {- Table name -}
                       [Text] {- Column names -}
  deriving (Show, Eq, Generic)
instance Hashable TableHasPrimaryKey
instance DatabasePredicate TableHasPrimaryKey where
  englishDescription (TableHasPrimaryKey tblName colNames) =
    "Table " <> show tblName <> " has primary key " <> show colNames

  predicateSource _ = PredicateSourceCurBackend

  serializePredicate (TableHasPrimaryKey tbl cols) =
    object [ "has-primary-key" .= object [ "table" .= tbl
                                         , "columns" .= cols ] ]

  predicateCascadesDropOn (TableHasPrimaryKey tblNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

-- * Serialization helpers

beamSerializeJSON :: Text -> Value -> Value
beamSerializeJSON backend v =
  object [ "be-specific" .= backend
         , "be-data" .= v ]

newtype BeamSerializedDataType
  = BeamSerializedDataType { fromBeamSerializedDataType :: Value }
  deriving (Show, Eq)

serializePrecAndDecimal :: Maybe (Word, Maybe Word) -> Value
serializePrecAndDecimal Nothing =
  object []
serializePrecAndDecimal (Just (prec, Nothing)) =
  object [ "prec" .= prec ]
serializePrecAndDecimal (Just (prec, Just decimal)) =
  object [ "prec" .= prec
         , "decimal" .= decimal ]

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

instance ToJSON BeamSerializedDataType where
  toJSON = fromBeamSerializedDataType

newtype BeamSerializedConstraintDefinition
  = BeamSerializedConstraintDefinition
  { fromBeamSerializedConstraintDefinition :: Value
  } deriving (Show, Eq)
newtype BeamSerializedConstraintAttributes
  = BeamSerializedConstraintAttributes
  { fromBeamSerializedConstraintAttributes :: [ Value ]
  } deriving (Show, Eq, Monoid)
newtype BeamSerializedConstraint
  = BeamSerializedConstraint
  { fromBeamSerializedConstraint :: Value
  } deriving (Show, Eq)
newtype BeamSerializedMatchType
  = BeamSerializedMatchType
  { fromBeamSerializedMatchType :: Value
  } deriving (Show, Eq)
newtype BeamSerializedReferentialAction
  = BeamSerializedReferentialAction
  { fromBeamSerializedReferentialAction :: Value
  } deriving (Show, Eq)
newtype BeamSerializedExpression
  = BeamSerializedExpression
  { fromBeamSerializedExpression :: Text
  } deriving (Show, Eq)

beamDeserializeMaybe :: Typeable a
                     => BeamDeserializers cmd
                     -> Maybe Value
                     -> Parser (Maybe a)
beamDeserializeMaybe _ Nothing = pure Nothing
beamDeserializeMaybe d (Just v) =
  Just <$> beamDeserialize d v

beamDeserialize :: forall a cmd. Typeable a
                => BeamDeserializers cmd -> Value
                -> Parser a
beamDeserialize allD@(BeamDeserializers d) v =
  case D.lookup (BeamDeserializerLabel :: BeamDeserializerLabel a) d of
    Nothing -> fail ("beamDeserialize: No deserializer for " ++ show (typeOf (undefined :: a)))
    Just (BeamDeserializer doParse) ->
      doParse allD v

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

-- * Deserialization helpers

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

newtype BeamDeserializer syntax
  = BeamDeserializer (forall cmd. BeamDeserializers cmd -> Value -> Parser syntax)

newtype BeamDeserializers cmd
  = BeamDeserializers
  { beamArbitraryDeserializers :: D.DMap BeamDeserializerLabel BeamDeserializer
  }

instance Monoid (BeamDeserializer cmd) where
  mempty = BeamDeserializer (const (const mzero))
  mappend (BeamDeserializer a) (BeamDeserializer b) =
    BeamDeserializer $ \d o ->
    a d o <|> b d o

instance Monoid (BeamDeserializers cmd) where
  mempty = BeamDeserializers mempty
  mappend (BeamDeserializers a) (BeamDeserializers b) =
    BeamDeserializers (D.unionWithKey (const mappend) a b)

beamDeserializer :: Typeable ty
                 => (forall cmd'. BeamDeserializers cmd' -> Value -> Parser ty)
                 -> BeamDeserializers cmd
beamDeserializer parse =
  BeamDeserializers (D.singleton BeamDeserializerLabel (BeamDeserializer parse))

sql92Deserializers :: forall cmd
                    . IsSql92DdlCommandSyntax cmd
                   => BeamDeserializers cmd
sql92Deserializers = mconcat
                     [ beamDeserializer deserializeSql92DataType
                     , beamDeserializer deserializeSql92ConstraintDefinition
                     , beamDeserializer deserializeSql92Constraint
                     , beamDeserializer deserializeSql92MatchType
                     , beamDeserializer deserializeSql92ReferentialAction
                     , beamDeserializer deserializeSql92Attributes ]
  where
    parseSub nm o key parse =
      withObject (unpack (nm <> "." <> key)) parse =<< o .: key

    deserializeSql92DataType :: BeamDeserializers cmd' -> Value
                             -> Parser (Sql92DdlCommandDataTypeSyntax cmd)
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

    deserializeSql92ConstraintDefinition :: BeamDeserializers cmd' -> Value
                                         -> Parser (Sql92DdlCommandConstraintDefinitionSyntax cmd)
    deserializeSql92ConstraintDefinition d =
      withObject "Sql92ColumnConstraintDefinition" $ \o ->
      constraintDefinitionSyntax <$> o .: "name"
                                 <*> (beamDeserialize d =<< o .: "constraint")
                                 <*> (beamDeserializeMaybe d =<< o .: "attributes")

    deserializeSql92Constraint :: BeamDeserializers cmd' -> Value
                               -> Parser (Sql92DdlCommandColumnConstraintSyntax cmd)
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

    deserializeSql92MatchType :: BeamDeserializers cmd' -> Value
                              -> Parser (Sql92DdlCommandMatchTypeSyntax cmd)
    deserializeSql92MatchType _ v =
      case v of
        "full" -> pure fullMatchSyntax
        "partial" -> pure partialMatchSyntax
        _ -> mzero

    deserializeSql92ReferentialAction :: BeamDeserializers cmd' -> Value
                                      -> Parser (Sql92DdlCommandReferentialActionSyntax cmd)
    deserializeSql92ReferentialAction _ v =
      case v of
        "cascade" -> pure referentialActionCascadeSyntax
        "set-null" -> pure referentialActionSetNullSyntax
        "set-default" -> pure referentialActionSetDefaultSyntax
        "nothing" -> pure referentialActionNoActionSyntax
        _ -> mzero

    deserializeSql92Attributes :: BeamDeserializers cmd' -> Value
                               -> Parser (Sql92DdlCommandConstraintAttributesSyntax cmd)
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

sql99DataTypeDeserializers
  :: forall cmd
   . ( IsSql92DdlCommandSyntax cmd
     , IsSql99DataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd) )
  => BeamDeserializers cmd
sql99DataTypeDeserializers =
  beamDeserializer $ \d v ->
  fmap (id @(Sql92DdlCommandDataTypeSyntax cmd)) $
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

beamCheckDeserializers
  :: forall cmd
   . ( IsSql92DdlCommandSyntax cmd
     , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
     , Sql92SerializableConstraintDefinitionSyntax (Sql92DdlCommandConstraintDefinitionSyntax cmd) )
  => BeamDeserializers cmd
beamCheckDeserializers = mconcat
  [ beamDeserializer (const deserializeTableExistsPredicate)
  , beamDeserializer (const deserializeTableHasPrimaryKeyPredicate)
  , beamDeserializer deserializeTableHasColumnPredicate
  , beamDeserializer deserializeTableColumnHasConstraintPredicate
  ]
  where
    deserializeTableExistsPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableExistsPredicate =
      withObject "TableExistPredicate" $ \v ->
      SomeDatabasePredicate <$> (TableExistsPredicate <$> v .: "table-exists")

    deserializeTableHasPrimaryKeyPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableHasPrimaryKeyPredicate =
      withObject "TableHasPrimaryKey" $ \v ->
      v .: "has-primary-key" >>=
      (withObject "TableHasPrimaryKey" $ \v' ->
       SomeDatabasePredicate <$> (TableHasPrimaryKey <$> v' .: "table" <*> v' .: "columns"))

    deserializeTableHasColumnPredicate :: BeamDeserializers cmd'
                                       -> Value -> Parser SomeDatabasePredicate
    deserializeTableHasColumnPredicate d =
      withObject "TableHasColumn" $ \v ->
      v .: "has-column" >>=
      (withObject "TableHasColumn" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd)))
         (TableHasColumn <$> v' .: "table" <*> v' .: "column"
                         <*> (beamDeserialize d =<< v' .: "type")))

    deserializeTableColumnHasConstraintPredicate :: BeamDeserializers cmd'
                                                 -> Value -> Parser SomeDatabasePredicate
    deserializeTableColumnHasConstraintPredicate d =
      withObject "TableColumnHasConstraint" $ \v ->
      v .: "has-column-constraint" >>=
      (withObject "TableColumnHasConstraint" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd)))
         (TableColumnHasConstraint <$> v' .: "table" <*> v' .: "column"
                                   <*> (beamDeserialize d =<< v' .: "constraint")))
