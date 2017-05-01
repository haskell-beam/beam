module Database.Beam.Migrate.Types.Predicates where

import Data.Text (Text)
import Data.Hashable
import Data.Typeable

class (Show p, Typeable p, Hashable p, Eq p) => DatabasePredicate p where
  englishDescription :: p -> String

  predicateCascadesDropOn :: DatabasePredicate p' => p -> p' -> Bool
  predicateCascadesDropOn _ _ = False


data SomeDatabasePredicate where
  SomeDatabasePredicate :: DatabasePredicate p =>
                           p -> SomeDatabasePredicate
instance Show SomeDatabasePredicate where
  showsPrec _ (SomeDatabasePredicate p) =
    showParen True $
    shows p .
    showString " :: " .
    shows (typeOf p)
instance Eq SomeDatabasePredicate where
  SomeDatabasePredicate a == SomeDatabasePredicate b =
    case cast a of
      Nothing -> False
      Just a' -> a' == b
instance Hashable SomeDatabasePredicate where
  hashWithSalt salt (SomeDatabasePredicate p) = hashWithSalt salt (typeOf p, p)

p :: DatabasePredicate p => p -> SomeDatabasePredicate
p = SomeDatabasePredicate

newtype TableCheck = TableCheck (Text -> SomeDatabasePredicate)
newtype DomainCheck = DomainCheck (Text -> SomeDatabasePredicate)
newtype FieldCheck = FieldCheck (Text -> Text -> SomeDatabasePredicate)
