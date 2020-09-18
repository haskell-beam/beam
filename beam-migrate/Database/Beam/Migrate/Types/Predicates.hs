{-# LANGUAGE CPP #-}

-- | Common 'DatabasePredicate's used for defining schemas
module Database.Beam.Migrate.Types.Predicates where

import Database.Beam
import Database.Beam.Backend.SQL.SQL92 (IsSql92TableNameSyntax(..))
import Database.Beam.Schema.Tables

import Control.DeepSeq

import Data.Aeson
import Data.Text (Text)
import Data.Hashable
import Data.Typeable

#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import Lens.Micro ((^.))

-- * Predicates

-- | A predicate is a type that describes some condition that the database
-- schema must meet. Beam represents database schemas as the set of all
-- predicates that apply to a database schema. The 'Hashable' and 'Eq' instances
-- allow us to build 'HashSet's of predicates to represent schemas in this way.
class (Typeable p, Hashable p, Eq p) => DatabasePredicate p where
  -- | An english language description of this predicate. For example, "There is
  -- a table named 'TableName'"
  englishDescription :: p -> String

  -- | Whether or not this predicate applies to all backends or only one
  -- backend. This is used when attempting to translate schemas between
  -- backends. If you are unsure, provide 'PredicateSpecificityOnlyBackend'
  -- along with an identifier unique to your backend.
  predicateSpecificity :: proxy p -> PredicateSpecificity

  -- | Serialize a predicate to a JSON 'Value'.
  serializePredicate :: p -> Value

  -- | Some predicates require other predicates to be true. For example, in
  -- order for a table to have a column, that table must exist. This function
  -- takes in the current predicate and another arbitrary database predicate. It
  -- should return 'True' if this predicate needs the other predicate to be true
  -- in order to exist.
  --
  -- By default, this simply returns 'False', which makes sense for many
  -- predicates.
  predicateCascadesDropOn :: DatabasePredicate p' => p -> p' -> Bool
  predicateCascadesDropOn _ _ = False

-- | A Database predicate is a value of any type which satisfies
-- 'DatabasePredicate'. We often want to store these in lists and sets, so we
-- need a monomorphic container that can store these polymorphic values.
data SomeDatabasePredicate where
  SomeDatabasePredicate :: DatabasePredicate p
                        => p -> SomeDatabasePredicate

instance NFData SomeDatabasePredicate where
  rnf p' = p' `seq` ()

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

-- | Some predicates make sense in any backend. Others only make sense in one.
-- This denotes the difference.
data PredicateSpecificity
  = PredicateSpecificityOnlyBackend String
  | PredicateSpecificityAllBackends
  deriving (Show, Eq, Generic)
instance Hashable PredicateSpecificity

instance ToJSON PredicateSpecificity where
  toJSON PredicateSpecificityAllBackends = "all"
  toJSON (PredicateSpecificityOnlyBackend s)  = object [ "backend" .= toJSON s ]
instance FromJSON PredicateSpecificity where
  parseJSON "all" = pure PredicateSpecificityAllBackends
  parseJSON (Object o) = PredicateSpecificityOnlyBackend <$> o .: "backend"
  parseJSON _ = fail "PredicateSource"

-- | Convenience synonym for 'SomeDatabasePredicate'
p :: DatabasePredicate p => p -> SomeDatabasePredicate
p = SomeDatabasePredicate

-- * Entity checks
--
--   When building checked database schemas, oftentimes the names of entities
--   may change. For example, a 'defaulMigratableDbSettings' object can have its
--   tables renamed. The checks need to update in order to reflect these name
--   changes. The following types represent predicates whose names have not yet
--   been determined.

-- | A name in a schema
data QualifiedName = QualifiedName (Maybe Text) Text
  deriving (Show, Eq, Ord)

instance ToJSON QualifiedName where
  toJSON (QualifiedName Nothing t) = toJSON t
  toJSON (QualifiedName (Just s) t) = object [ "schema" .= s, "name" .= t ]

instance FromJSON QualifiedName where
  parseJSON s@(String {}) = QualifiedName Nothing <$> parseJSON s
  parseJSON (Object o) = QualifiedName <$> o .: "schema" <*> o .: "name"
  parseJSON _ = fail "QualifiedName: expects either string or {schema: ..., name: ...}"

instance Hashable QualifiedName where
  hashWithSalt s (QualifiedName sch t) =
    hashWithSalt s (sch, t)

qname :: IsDatabaseEntity be entity => DatabaseEntityDescriptor be entity -> QualifiedName
qname e = QualifiedName (e ^. dbEntitySchema) (e ^. dbEntityName)

qnameAsText :: QualifiedName -> Text
qnameAsText (QualifiedName Nothing tbl) = tbl
qnameAsText (QualifiedName (Just sch) tbl) = sch <> "." <> tbl

qnameAsTableName :: IsSql92TableNameSyntax syntax => QualifiedName -> syntax
qnameAsTableName (QualifiedName sch t) = tableName sch t

-- | An optional predicate that depends on the name of a table as well as its fields
newtype TableCheck = TableCheck (forall tbl. Table tbl => QualifiedName -> tbl (TableField tbl) -> Maybe SomeDatabasePredicate)

-- | A predicate that depends on the name of a domain type
newtype DomainCheck = DomainCheck (QualifiedName -> SomeDatabasePredicate)

-- | A predicate that depends on the name of a table and one of its fields
newtype FieldCheck = FieldCheck (QualifiedName -> Text -> SomeDatabasePredicate)
