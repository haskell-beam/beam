{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Beam.Migrate.Backend where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Checks
import           Database.Beam.Migrate.SQL
import           Database.Beam.Migrate.Types ( SomeDatabasePredicate(..), MigrationSteps
                                             , CheckedDatabaseSettings, SomeDatabasePredicate)

import           Database.Beam.Haskell.Syntax

import           Control.Applicative


import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import           Data.Text (Text)
import           Data.Time

import           Data.Typeable

type DdlError = String
type BeamMigrateRunTransaction m options =
  forall a. options -> m a -> IO (Either String a)

newtype HaskellPredicateConverter = HaskellPredicateConverter (SomeDatabasePredicate -> Maybe SomeDatabasePredicate)

instance Monoid HaskellPredicateConverter where
  mempty = HaskellPredicateConverter $ \_ -> Nothing
  mappend (HaskellPredicateConverter a) (HaskellPredicateConverter b) =
    HaskellPredicateConverter $ \r -> a r <|> b r

data SomeCheckedDatabase be where
  SomeCheckedDatabase :: Database db
                      => CheckedDatabaseSettings be db
                      -> SomeCheckedDatabase be

data BeamMigrationBackend be commandSyntax hdl where
  BeamMigrationBackend ::
    ( MonadBeam commandSyntax be hdl m
    , Typeable be
    , HasQBuilder (Sql92SelectSyntax commandSyntax)
    , HasSqlValueSyntax (Sql92ValueSyntax commandSyntax) LocalTime
    , HasSqlValueSyntax (Sql92ValueSyntax commandSyntax) (Maybe LocalTime)
    , HasSqlValueSyntax (Sql92ValueSyntax commandSyntax) Text
    , HasSqlValueSyntax (Sql92ValueSyntax commandSyntax) SqlNull
    , IsSql92Syntax commandSyntax
    , Sql92SanityCheck commandSyntax, Sql92SaneDdlCommandSyntax commandSyntax
    , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax commandSyntax)
    , Sql92ReasonableMarshaller be ) =>
    { backendName :: String
    , backendConnStringExplanation :: String
    , backendRenderSteps :: forall a. MigrationSteps commandSyntax () a -> BL.ByteString
    , backendGetDbConstraints :: m [ SomeDatabasePredicate ]
    , backendPredicateParsers :: BeamDeserializers commandSyntax
    , backendRenderSyntax :: commandSyntax -> String
    , backendFileExtension :: String
    , backendConvertToHaskell :: HaskellPredicateConverter
    , backendActionProviders :: [ ActionProvider commandSyntax ]
    , backendTransact :: forall a. String -> m a -> IO (Either DdlError a)
    } -> BeamMigrationBackend be commandSyntax hdl

data SomeBeamMigrationBackend where
  SomeBeamMigrationBackend :: ( Typeable commandSyntax
                              , IsSql92DdlCommandSyntax commandSyntax
                              , IsSql92Syntax commandSyntax
                              , Sql92SanityCheck commandSyntax ) =>
                              BeamMigrationBackend be commandSyntax hdl
                           -> SomeBeamMigrationBackend

easyHsPredicateConverter :: forall columnSchemaSyntax
                          . Typeable columnSchemaSyntax
                         => (Sql92ColumnSchemaColumnTypeSyntax columnSchemaSyntax -> Maybe HsDataType)
                         -> HaskellPredicateConverter
easyHsPredicateConverter convType =
  simpleHsConverter @TableExistsPredicate <>
  simpleHsConverter @TableHasPrimaryKey   <>
  hasColumnConverter @columnSchemaSyntax convType

hasColumnConverter :: forall columnSchemaSyntax
                    . Typeable columnSchemaSyntax
                   => (Sql92ColumnSchemaColumnTypeSyntax columnSchemaSyntax -> Maybe HsDataType)
                   -> HaskellPredicateConverter
hasColumnConverter convType =
  hsPredicateConverter $
  \(TableHasColumn tbl col ty :: TableHasColumn columnSchemaSyntax) ->
    fmap SomeDatabasePredicate (TableHasColumn tbl col <$> convType ty :: Maybe (TableHasColumn HsColumnSchema))

simpleHsConverter :: forall pred. Typeable pred => HaskellPredicateConverter
simpleHsConverter =
  HaskellPredicateConverter $ \orig@(SomeDatabasePredicate p') ->
  case cast p' of
    Nothing -> Nothing
    Just (_ :: pred) -> Just orig

hsPredicateConverter :: Typeable pred => (pred -> Maybe SomeDatabasePredicate) -> HaskellPredicateConverter
hsPredicateConverter f =
  HaskellPredicateConverter $ \(SomeDatabasePredicate p') ->
  case cast p' of
    Nothing -> Nothing
    Just p'' -> f p''

