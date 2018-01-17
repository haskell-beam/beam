{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.SQL.Types
  ( TableSchema, TableFieldSchema(..)
  , FieldSchema(..), DataType(..), Constraint(..)
  , FieldReturnType(..)

  , field

  , defaultTo_, notNull
  , int, smallint, bigint
  , char, varchar, double
  , characterLargeObject, binaryLargeObject, array
  , boolean, numeric, date, time
  , timestamp, timestamptz
  , binary, varbinary

  , maybeType, autoType ) where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate.Checks
import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92

import Data.Text (Text)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Time (LocalTime, TimeOfDay)
import Data.Scientific (Scientific)

import GHC.TypeLits

type TableSchema fieldSchemaSyntax tbl =
    tbl (TableFieldSchema fieldSchemaSyntax)

data TableFieldSchema fieldSchemaSyntax a
    = TableFieldSchema Text (FieldSchema fieldSchemaSyntax a) [FieldCheck]

newtype FieldSchema syntax a = FieldSchema syntax
  deriving (Show, Eq)
newtype DefaultValue syntax a = DefaultValue (Sql92ColumnSchemaExpressionSyntax syntax)
newtype Constraint syntax = Constraint (Sql92ColumnConstraintDefinitionConstraintSyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax))
newtype DataType syntax a = DataType syntax
instance Sql92DisplaySyntax syntax => Show (DataType syntax a) where
  show (DataType syntax) = "DataType (" ++ displaySyntax syntax ++ ")"

instance Eq syntax => Eq (DataType syntax a) where
  DataType a == DataType b = a == b

defaultTo_ :: IsSql92ExpressionSyntax (Sql92ColumnSchemaExpressionSyntax syntax) =>
              (forall s. QExpr (Sql92ColumnSchemaExpressionSyntax syntax) s a)
           -> DefaultValue syntax a
defaultTo_ (QExpr e) =
  DefaultValue (e "t")

notNull :: IsSql92ColumnSchemaSyntax syntax => Constraint syntax
notNull = Constraint notNullConstraintSyntax

smallint, int :: (IsSql92DataTypeSyntax syntax, Integral a) => DataType syntax a
int = DataType intType
smallint = DataType smallIntType

bigint :: (IsSql2008BigIntDataTypeSyntax syntax, Integral a) => DataType syntax a
bigint = DataType bigIntType

-- TODO is Integer the right type to use here?
binary, varbinary
  :: IsSql2003BinaryAndVarBinaryDataTypeSyntax syntax
  => Maybe Word -> DataType syntax Integer
binary prec = DataType (binaryType prec)
varbinary prec = DataType (varBinaryType prec)

-- TODO should this be Day or something?
date :: IsSql92DataTypeSyntax syntax => DataType syntax LocalTime
date = DataType dateType

char, varchar :: IsSql92DataTypeSyntax syntax => Maybe Word -> DataType syntax Text
char prec = DataType (charType prec Nothing)
varchar prec = DataType (varCharType prec Nothing)

double :: IsSql92DataTypeSyntax syntax => DataType syntax Double
double = DataType doubleType

numeric :: IsSql92DataTypeSyntax syntax => Maybe (Word, Maybe Word) -> DataType syntax Scientific
numeric x = DataType (numericType x)

timestamp, timestamptz :: IsSql92DataTypeSyntax syntax => DataType syntax LocalTime
timestamptz = DataType (timestampType Nothing True)
timestamp = DataType (timestampType Nothing False)

time :: IsSql92DataTypeSyntax syntax => Maybe Word -> DataType syntax TimeOfDay
time prec = DataType (timeType prec False)

boolean :: IsSql99DataTypeSyntax syntax => DataType syntax Bool
boolean = DataType booleanType

characterLargeObject :: IsSql99DataTypeSyntax syntax => DataType syntax Text
characterLargeObject = DataType characterLargeObjectType

binaryLargeObject :: IsSql99DataTypeSyntax syntax => DataType syntax ByteString
binaryLargeObject = DataType binaryLargeObjectType

array :: (Typeable a, IsSql99DataTypeSyntax syntax)
      => DataType syntax a -> Int
      -> DataType syntax (Vector a)
array (DataType ty) sz = DataType (arrayType ty sz)

maybeType :: DataType syntax a -> DataType syntax (Maybe a)
maybeType (DataType sqlTy) = DataType sqlTy

autoType :: DataType syntax a -> DataType syntax (Auto a)
autoType (DataType sqlTy) = DataType sqlTy

-- * Hygienic(?) field definitions

class FieldReturnType (defaultGiven :: Bool) (collationGiven :: Bool) syntax resTy a | a -> syntax resTy where
  field' :: IsSql92ColumnSchemaSyntax syntax =>
            Proxy defaultGiven -> Proxy collationGiven
         -> Text
         -> Sql92ColumnSchemaColumnTypeSyntax syntax
         -> Maybe (Sql92ColumnSchemaExpressionSyntax syntax)
         -> Maybe Text -> [ Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax ]
         -> a

instance FieldReturnType 'True collationGiven syntax resTy a =>
  FieldReturnType 'False collationGiven syntax resTy (DefaultValue syntax resTy -> a) where
  field' _ collationGiven nm ty _ collation constraints (DefaultValue e) =
    field' (Proxy @'True) collationGiven nm ty (Just e) collation constraints

instance FieldReturnType defaultGiven collationGiven syntax resTy a =>
  FieldReturnType defaultGiven collationGiven syntax resTy (Constraint syntax -> a) where
  field' defaultGiven collationGiven nm ty default_' collation constraints (Constraint e) =
    field' defaultGiven collationGiven nm ty default_' collation (constraints ++ [ constraintDefinitionSyntax Nothing e Nothing ])

instance ( FieldReturnType 'True collationGiven syntax resTy a
         , TypeError ('Text "Only one DEFAULT clause can be given per 'field' invocation") ) =>
  FieldReturnType 'True collationGiven syntax resTy (DefaultValue syntax resTy -> a) where

  field' = error "Unreachable because of GHC Custom Type Errors"

instance ( FieldReturnType defaultGiven collationGiven syntax resTy a
         , TypeError ('Text "Only one type declaration allowed per 'field' invocation")) =>
  FieldReturnType defaultGiven collationGiven syntax resTy (DataType syntax' x -> a) where
  field' = error "Unreachable because of GHC Custom Type Errors"

instance ( Typeable syntax, Typeable (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Sql92DisplaySyntax (Sql92ColumnSchemaColumnTypeSyntax syntax), Eq (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Sql92DisplaySyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax), Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , IsSql92ColumnSchemaSyntax syntax
         , Sql92SerializableConstraintDefinitionSyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , Sql92SerializableDataTypeSyntax (Sql92ColumnSchemaColumnTypeSyntax syntax) ) =>
  FieldReturnType defaultGiven collationGiven syntax resTy (TableFieldSchema syntax resTy) where
  field' _ _ nm ty default_' collation constraints =
    TableFieldSchema nm (FieldSchema (columnSchemaSyntax ty default_' constraints collation)) checks
    where checks = [ FieldCheck (\tbl field'' -> SomeDatabasePredicate (TableHasColumn tbl field'' ty :: TableHasColumn syntax)) ] ++
                   map (\cns -> FieldCheck (\tbl field'' -> SomeDatabasePredicate (TableColumnHasConstraint tbl field'' cns :: TableColumnHasConstraint syntax))) constraints

field :: ( IsSql92ColumnSchemaSyntax syntax ) =>
  FieldReturnType 'False 'False syntax resTy a => Text -> DataType (Sql92ColumnSchemaColumnTypeSyntax syntax) resTy -> a
field name (DataType ty) = field' (Proxy @'False) (Proxy @'False) name ty Nothing Nothing []
