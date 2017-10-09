module Database.Beam.Migrate.Types.Predicates where

import Database.Beam

import Data.Aeson
import Data.Text (Text)
import Data.Hashable
import Data.Typeable

import GHC.Generics

data PredicateSource
  = PredicateSourceBackend String
  | PredicateSourceCurBackend
  deriving (Show, Eq, Generic)
instance Hashable PredicateSource

class (Typeable p, Hashable p, Eq p) => DatabasePredicate p where
  englishDescription :: p -> String

  predicateSource :: proxy p -> PredicateSource

  serializePredicate :: p -> Value

  predicateCascadesDropOn :: DatabasePredicate p' => p -> p' -> Bool
  predicateCascadesDropOn _ _ = False

data SomeDatabasePredicate where
  SomeDatabasePredicate :: DatabasePredicate p =>
                           p -> SomeDatabasePredicate
instance Show SomeDatabasePredicate where
  showsPrec _ (SomeDatabasePredicate p') =
    ('(':) . shows (typeOf p') . (": " ++) . (englishDescription p' ++) . (')':)
instance Eq SomeDatabasePredicate where
  SomeDatabasePredicate a == SomeDatabasePredicate b =
    case cast a of
      Nothing -> False
      Just a' -> a' == b
instance Hashable SomeDatabasePredicate where
  hashWithSalt salt (SomeDatabasePredicate p') = hashWithSalt salt (typeOf p', p')

p :: DatabasePredicate p => p -> SomeDatabasePredicate
p = SomeDatabasePredicate

newtype TableCheck tbl = TableCheck (Text -> tbl (TableField tbl) -> SomeDatabasePredicate)
newtype DomainCheck = DomainCheck (Text -> SomeDatabasePredicate)
newtype FieldCheck = FieldCheck (Text -> Text -> SomeDatabasePredicate)

instance ToJSON PredicateSource where
  toJSON PredicateSourceCurBackend = "all"
  toJSON (PredicateSourceBackend s)  = object [ "backend" .= toJSON s ]
instance FromJSON PredicateSource where
  parseJSON "all" = pure PredicateSourceCurBackend
  parseJSON (Object o) = PredicateSourceBackend <$> o .: "backend"
  parseJSON _ = fail "PredicateSource"
