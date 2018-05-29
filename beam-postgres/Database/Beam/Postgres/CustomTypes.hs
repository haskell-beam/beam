{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
module Database.Beam.Postgres.CustomTypes
    ( PgType, PgTypeCheck(..)
    , PgDataTypeSchema

    , IsPgCustomDataType(..)

    , PgHasEnum(..)

    , HasSqlValueSyntax, FromBackendRow

    , pgCustomEnumSchema, pgBoundedEnumSchema

    , pgCustomEnumActionProvider
    , pgCreateEnumActionProvider
    , pgDropEnumActionProvider

    , pgChecksForTypeSchema

    , pgEnumValueSyntax, pgParseEnum

    , createEnum
    , beamTypeForCustomPg
    ) where

import           Database.Beam
import           Database.Beam.Schema.Tables
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate
import           Database.Beam.Postgres.Types
import           Database.Beam.Postgres.Syntax

import           Control.Monad
import           Control.Monad.Free.Church
import           Data.Aeson (object, (.=))
import           Data.Functor.Const
import qualified Data.HashSet as HS
import           Data.Hashable (Hashable)
import           Data.Proxy (Proxy(..))
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE

data PgType a
newtype PgTypeCheck = PgTypeCheck (Text -> SomeDatabasePredicate)

data PgDataTypeSchema a where
    PgDataTypeEnum :: HasSqlValueSyntax PgValueSyntax a => [a] -> PgDataTypeSchema a

class IsPgCustomDataType a where
    pgDataTypeName :: Proxy a -> Text
    pgDataTypeDescription :: PgDataTypeSchema a

data PgHasEnum = PgHasEnum Text {- Enumeration name -} [Text] {- enum values -}
    deriving (Show, Eq, Generic, Hashable)
instance DatabasePredicate PgHasEnum where
    englishDescription (PgHasEnum enumName values) =
        "Has postgres enumeration " ++ show enumName ++ " with values " ++ show values

    predicateSpecificity _ = PredicateSpecificityOnlyBackend "postgres"
    serializePredicate (PgHasEnum name values) =
        object [ "has-postgres-enum" .= object [ "name" .= name
                                               , "values" .= values ] ]

pgCustomEnumSchema :: HasSqlValueSyntax PgValueSyntax a => [a] -> PgDataTypeSchema a
pgCustomEnumSchema = PgDataTypeEnum

pgBoundedEnumSchema :: ( Enum a, Bounded a, HasSqlValueSyntax PgValueSyntax a )
                    => PgDataTypeSchema a
pgBoundedEnumSchema = pgCustomEnumSchema [minBound..maxBound]

pgCustomEnumActionProvider :: ActionProvider Postgres
pgCustomEnumActionProvider = pgCreateEnumActionProvider <> pgDropEnumActionProvider

pgCreateEnumActionProvider :: ActionProvider Postgres
pgCreateEnumActionProvider =
  ActionProvider $ \findPre findPost ->
  do enumP@(PgHasEnum nm vals) <- findPost
     ensuringNot_ $
      do (PgHasEnum beforeNm _) <- findPre
         guard (beforeNm == nm)

     let cmd = pgCreateEnumSyntax nm (fmap sqlValueSyntax vals)
     pure (PotentialAction mempty (HS.fromList [p enumP])
                           (pure (MigrationCommand cmd MigrationKeepsData))
                           ("Create the enumeration " <> nm) 1)

pgDropEnumActionProvider :: ActionProvider Postgres
pgDropEnumActionProvider =
  ActionProvider $ \findPre findPost ->
  do enumP@(PgHasEnum nm _) <- findPre
     ensuringNot_ $
      do (PgHasEnum afterNm _) <- findPost
         guard (afterNm == nm)

     let cmd = pgDropTypeSyntax nm
     pure (PotentialAction (HS.fromList [p enumP]) mempty
                           (pure (MigrationCommand cmd MigrationKeepsData))
                           ("Drop the enumeration type " <> nm) 1)

pgChecksForTypeSchema :: PgDataTypeSchema a -> [ PgTypeCheck ]
pgChecksForTypeSchema (PgDataTypeEnum vals) =
  let valTxts = map encodeToString vals

      -- TODO better reporting
      encodeToString val =
        let PgValueSyntax (PgSyntax syntax) = sqlValueSyntax val
        in runF syntax (\_ -> error "Expecting a simple text encoding for enumeration type")
                       (\case
                           EscapeString s _ -> TE.decodeUtf8 s
                           _ -> error "Expecting a simple text encoding for enumeration type")
  in [ PgTypeCheck (\nm -> p (PgHasEnum nm valTxts)) ]

instance IsDatabaseEntity Postgres (PgType a) where

  data DatabaseEntityDescriptor Postgres (PgType a) where
      PgTypeDescriptor :: Text -> PgDataTypeSyntax
                       -> DatabaseEntityDescriptor Postgres (PgType a)

  type DatabaseEntityDefaultRequirements Postgres (PgType a) =
      ( HasSqlValueSyntax PgValueSyntax a
      , FromBackendRow Postgres a
      , IsPgCustomDataType a)

  type DatabaseEntityRegularRequirements Postgres (PgType a) =
      ( HasSqlValueSyntax PgValueSyntax a
      , FromBackendRow Postgres a )

  dbEntityName f (PgTypeDescriptor nm ty) = (\nm' -> PgTypeDescriptor nm' ty) <$> f nm
  dbEntityAuto _ = PgTypeDescriptor typeName
                                    (PgDataTypeSyntax (PgDataTypeDescrDomain typeName)
                                                      (pgQuotedIdentifier typeName)
                                                      (pgDataTypeJSON (object [ "customType" .= typeName])))
      where
        typeName = pgDataTypeName (Proxy @a)

instance IsCheckedDatabaseEntity Postgres (PgType a) where
    data CheckedDatabaseEntityDescriptor Postgres (PgType a) where
        CheckedPgTypeDescriptor :: DatabaseEntityDescriptor Postgres (PgType a)
                                -> [ PgTypeCheck ]
                                -> CheckedDatabaseEntityDescriptor Postgres (PgType a)
    type CheckedDatabaseEntityDefaultRequirements Postgres (PgType a) =
        DatabaseEntityDefaultRequirements Postgres (PgType a)

    unCheck (CheckedPgTypeDescriptor ty _) = ty
    collectEntityChecks (CheckedPgTypeDescriptor e chks) =
        fmap (\(PgTypeCheck mkCheck) -> mkCheck (getConst (dbEntityName Const e))) chks
    checkedDbEntityAuto nm = CheckedPgTypeDescriptor (dbEntityAuto nm)
                                                     (pgChecksForTypeSchema (pgDataTypeDescription @a))

instance RenamableWithRule (FieldRenamer (DatabaseEntityDescriptor Postgres (PgType a))) where
    renamingFields _ = FieldRenamer id

createEnum :: forall a db
            . ( HasSqlValueSyntax PgValueSyntax a
              , Enum a, Bounded a )
           => Text -> Migration Postgres (CheckedDatabaseEntity Postgres db (PgType a))
createEnum nm = do
  upDown (pgCreateEnumSyntax nm (fmap sqlValueSyntax [minBound..(maxBound::a)]))
         (Just (pgDropTypeSyntax nm))

  let tyDesc = PgTypeDescriptor nm $
               PgDataTypeSyntax (PgDataTypeDescrDomain nm)
                                (pgQuotedIdentifier nm)
                                (pgDataTypeJSON (object [ "customType" .= nm ]))

  pure (CheckedDatabaseEntity
          (CheckedPgTypeDescriptor tyDesc
             (pgChecksForTypeSchema (PgDataTypeEnum [minBound..maxBound::a])))
          [])


pgEnumValueSyntax :: (a -> String) -> a -> PgValueSyntax
pgEnumValueSyntax namer = sqlValueSyntax . namer

pgParseEnum :: (Enum a, Bounded a) => (a -> String)
            -> FromBackendRowM Postgres a
pgParseEnum namer =
  let allNames = map (\x -> (namer x, x)) [minBound..maxBound]
  in do
    name <- fromBackendRow
    case lookup name allNames of
      Nothing -> fail ("Invalid postgres enumeration value: " ++ name)
      Just  v -> pure v

beamTypeForCustomPg :: CheckedDatabaseEntity Postgres db (PgType a) -> DataType Postgres a
beamTypeForCustomPg (CheckedDatabaseEntity (CheckedPgTypeDescriptor (PgTypeDescriptor _ dt) _) _)
    = DataType dt
