{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Migrate.SQL.Types
  ( TableSchema(..), TableFieldSchema(..)
  , FieldSchema(..), DataType(..), Constraint(..)
  , FieldReturnType(..)

  , field

  , defaultTo_, notNull
  , int, smallint, char, varchar, double
  , numeric, date
  , timestamp, timestamptz

  , maybeType, autoType ) where

import Database.Beam
import Database.Beam.Query.Internal
import Database.Beam.Backend.SQL
import Database.Beam.Migrate.Checks
import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92
--import Database.Beam.Migrate.SQL.Internal

import Database.Beam.Haskell.Syntax

import Data.Text (Text)
import Data.Typeable
import Data.Proxy
import Data.Time (LocalTime)
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
data DataType syntax a = DataType syntax (Text -> Text -> HaskellDataType)
instance Show syntax => Show (DataType syntax a) where
  show (DataType syntax _) = "DataType (" ++ show syntax ++ ")"

instance Eq syntax => Eq (DataType syntax a) where
  DataType a _ == DataType b _ = a == b

defaultTo_ :: IsSql92ExpressionSyntax (Sql92ColumnSchemaExpressionSyntax syntax) =>
              (forall s. QExpr (Sql92ColumnSchemaExpressionSyntax syntax) s a)
           -> DefaultValue syntax a
defaultTo_ (QExpr e) =
  DefaultValue e

notNull :: IsSql92ColumnSchemaSyntax syntax => Constraint syntax
notNull = Constraint notNullConstraintSyntax

smallint, int :: forall a syntax. (IsSql92DataTypeSyntax syntax, Integral a, Typeable a) => DataType syntax a
int = DataType intType (stdTypeHs "int" (show (typeRep (Proxy @a))))
smallint = DataType smallIntType (stdTypeHs "smallint" (show (typeRep (Proxy @a))))

-- TODO should this be Day or something?
date :: IsSql92DataTypeSyntax syntax => DataType syntax LocalTime
date = DataType dateType (stdTypeHs "date" "LocalTime")

char, varchar :: IsSql92DataTypeSyntax syntax => Maybe Word -> DataType syntax Text
char prec = DataType (charType prec Nothing) (complexTypeHs (app (fun "char") [ maybeHs (fmap intHs prec) ]) "Text")
varchar prec = DataType (varCharType prec Nothing)  (complexTypeHs (app (fun "varchar") [ maybeHs (fmap intHs prec) ]) "Text")

double :: IsSql92DataTypeSyntax syntax => DataType syntax Double
double = DataType doubleType (error "TODO")

numeric :: IsSql92DataTypeSyntax syntax => Maybe (Word, Maybe Word) -> DataType syntax Scientific
numeric x = DataType (numericType x) (error "TODO")

timestamp, timestamptz :: IsSql92DataTypeSyntax syntax => DataType syntax LocalTime
timestamptz = DataType (timestampType Nothing True) (stdTypeHs "timestamptz" "LocalTime")
timestamp = DataType (timestampType Nothing False) (stdTypeHs "timestamp" "LocalTime")

maybeType :: DataType syntax a -> DataType syntax (Maybe a)
maybeType (DataType sqlTy hsTy) = DataType sqlTy hsTy

autoType :: DataType syntax a -> DataType syntax (Auto a)
autoType (DataType sqlTy hsTy) = DataType sqlTy hsTy

-- * Hygienic(?) field definitions

class FieldReturnType (defaultGiven :: Bool) (collationGiven :: Bool) syntax resTy a | a -> syntax resTy where
  field' :: IsSql92ColumnSchemaSyntax syntax =>
            Proxy defaultGiven -> Proxy collationGiven
         -> Text
         -> Sql92ColumnSchemaColumnTypeSyntax syntax
         -> (Text -> Text -> HaskellDataType)
         -> Maybe (Sql92ColumnSchemaExpressionSyntax syntax)
         -> Maybe Text -> [ Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax ]
         -> a

instance FieldReturnType 'True collationGiven syntax resTy a =>
  FieldReturnType 'False collationGiven syntax resTy (DefaultValue syntax resTy -> a) where
  field' defaultGiven collationGiven nm ty hsTy _ collation constraints (DefaultValue e) =
    field' (Proxy @'True) collationGiven nm ty hsTy (Just e) collation constraints

instance FieldReturnType defaultGiven collationGiven syntax resTy a =>
  FieldReturnType defaultGiven collationGiven syntax resTy (Constraint syntax -> a) where
  field' defaultGiven collationGiven nm ty hsTy default_ collation constraints (Constraint e) =
    field' defaultGiven collationGiven nm ty hsTy default_ collation (constraints ++ [ constraintDefinitionSyntax Nothing e Nothing ])

instance ( FieldReturnType 'True collationGiven syntax resTy a
         , TypeError ('Text "Only one DEFAULT clause can be given per 'field' invocation") ) =>
  FieldReturnType 'True collationGiven syntax resTy (DefaultValue syntax resTy -> a) where

  field' = error "Unreachable because of GHC Custom Type Errors"

instance ( FieldReturnType defaultGiven collationGiven syntax resTy a
         , TypeError ('Text "Only one type declaration allowed per 'field' invocation")) =>
  FieldReturnType defaultGiven collationGiven syntax resTy (DataType syntax' x -> a) where
  field' = error "Unreachable because of GHC Custom Type Errors"

instance ( Typeable syntax, Typeable (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Show (Sql92ColumnSchemaColumnTypeSyntax syntax), Eq (Sql92ColumnSchemaColumnTypeSyntax syntax)
         , Show (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax), Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
         , IsSql92ColumnSchemaSyntax syntax ) =>
  FieldReturnType defaultGiven collationGiven syntax resTy (TableFieldSchema syntax resTy) where
  field' _ _ nm ty hsTy default_ collation constraints =
    TableFieldSchema nm (FieldSchema (columnSchemaSyntax ty default_ constraints collation)) checks
    where checks = [ FieldCheck (\tbl field -> SomeDatabasePredicate (TableHasColumn tbl field ty (hsTy tbl field) :: TableHasColumn syntax)) ] ++
                   map (\cns -> FieldCheck (\tbl field -> SomeDatabasePredicate (TableColumnHasConstraint tbl field cns :: TableColumnHasConstraint syntax))) constraints

field :: ( IsSql92ColumnSchemaSyntax syntax ) =>
  FieldReturnType 'False 'False syntax resTy a => Text -> DataType (Sql92ColumnSchemaColumnTypeSyntax syntax) ty -> a
field name (DataType ty haskTy) = field' (Proxy @'False) (Proxy @'False) name ty haskTy Nothing Nothing []
