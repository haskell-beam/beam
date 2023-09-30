{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

-- | Finally-tagless encoding of SQL92 DDL commands.
--
--  If you're writing a beam backend driver and you want to support migrations,
--  making an instance of your command syntax for 'IsSql92DdlCommandSyntax' and
--  making it satisfy 'Sql92SaneDdlCommandSyntax'.
module Database.Beam.Migrate.SQL.SQL92 where

import Database.Beam.Backend.SQL.SQL92

import Data.Aeson (Value)
import Data.Hashable
import Data.Text (Text)
import Data.Typeable
#if ! MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

import GHC.Types (Type)

-- * Convenience type synonyms

-- | Syntax equalities that any reasonable DDL syntax would follow,
-- including equalities between beam-migrate and beam-core types.
type Sql92SaneDdlCommandSyntax cmd =
  ( Sql92SaneDdlCommandSyntaxMigrateOnly cmd
  , Sql92ExpressionCastTargetSyntax (Sql92ExpressionSyntax cmd) ~
      Sql92DdlCommandDataTypeSyntax cmd
  , Sql92ColumnSchemaExpressionSyntax (Sql92DdlCommandColumnSchemaSyntax cmd) ~
      Sql92ExpressionSyntax cmd )

-- | Syntax equalities for any reasonable DDL syntax, only including
-- types defined here.
type Sql92SaneDdlCommandSyntaxMigrateOnly cmd =
  ( IsSql92DdlCommandSyntax cmd
  , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax cmd)
  , Sql92SerializableConstraintDefinitionSyntax (Sql92DdlCommandConstraintDefinitionSyntax cmd)
  , Typeable (Sql92DdlCommandColumnSchemaSyntax cmd)
  , Sql92AlterTableColumnSchemaSyntax
      (Sql92AlterTableAlterTableActionSyntax (Sql92DdlCommandAlterTableSyntax cmd)) ~
      Sql92CreateTableColumnSchemaSyntax (Sql92DdlCommandCreateTableSyntax cmd)
  )

type Sql92DdlCommandDataTypeSyntax syntax =
  Sql92ColumnSchemaColumnTypeSyntax (Sql92DdlCommandColumnSchemaSyntax syntax)
type Sql92DdlCommandColumnSchemaSyntax syntax = Sql92CreateTableColumnSchemaSyntax (Sql92DdlCommandCreateTableSyntax syntax)
type Sql92DdlCommandConstraintDefinitionSyntax syntax =
  Sql92ColumnSchemaColumnConstraintDefinitionSyntax (Sql92DdlCommandColumnSchemaSyntax syntax)
type Sql92DdlColumnSchemaConstraintSyntax syntax =
  Sql92ColumnConstraintDefinitionConstraintSyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax)
type Sql92DdlCommandColumnConstraintSyntax syntax =
  Sql92DdlColumnSchemaConstraintSyntax (Sql92DdlCommandColumnSchemaSyntax syntax)
type Sql92DdlCommandMatchTypeSyntax syntax =
  Sql92ColumnConstraintMatchTypeSyntax (Sql92DdlCommandColumnConstraintSyntax syntax)
type Sql92DdlCommandReferentialActionSyntax syntax =
  Sql92ColumnConstraintReferentialActionSyntax (Sql92DdlCommandColumnConstraintSyntax syntax)
type Sql92DdlCommandConstraintAttributesSyntax syntax =
  Sql92ColumnConstraintDefinitionAttributesSyntax (Sql92DdlCommandConstraintDefinitionSyntax syntax)
type Sql92DdlCommandAlterTableActionSyntax syntax =
  Sql92AlterTableAlterTableActionSyntax (Sql92DdlCommandAlterTableSyntax syntax)

class ( IsSql92CreateTableSyntax (Sql92DdlCommandCreateTableSyntax syntax)
      , IsSql92DropTableSyntax (Sql92DdlCommandDropTableSyntax syntax)
      , IsSql92AlterTableSyntax (Sql92DdlCommandAlterTableSyntax syntax)) =>
  IsSql92DdlCommandSyntax syntax where
  type Sql92DdlCommandCreateTableSyntax syntax :: Type
  type Sql92DdlCommandAlterTableSyntax syntax :: Type
  type Sql92DdlCommandDropTableSyntax syntax :: Type

  createTableCmd :: Sql92DdlCommandCreateTableSyntax syntax -> syntax
  dropTableCmd   :: Sql92DdlCommandDropTableSyntax syntax -> syntax
  alterTableCmd  :: Sql92DdlCommandAlterTableSyntax syntax -> syntax

class ( IsSql92TableConstraintSyntax (Sql92CreateTableTableConstraintSyntax syntax)
      , IsSql92ColumnSchemaSyntax (Sql92CreateTableColumnSchemaSyntax syntax)
      , IsSql92TableNameSyntax (Sql92CreateTableTableNameSyntax syntax) ) =>
    IsSql92CreateTableSyntax syntax where

  type Sql92CreateTableTableNameSyntax syntax :: Type
  type Sql92CreateTableColumnSchemaSyntax syntax :: Type
  type Sql92CreateTableTableConstraintSyntax syntax :: Type
  type Sql92CreateTableOptionsSyntax syntax :: Type

  createTableSyntax :: Maybe (Sql92CreateTableOptionsSyntax syntax)
                    -> Sql92CreateTableTableNameSyntax syntax
                    -> [ (Text, Sql92CreateTableColumnSchemaSyntax syntax) ]
                    -> [ Sql92CreateTableTableConstraintSyntax syntax ]
                    -> syntax

class IsSql92TableNameSyntax (Sql92DropTableTableNameSyntax syntax) =>
  IsSql92DropTableSyntax syntax where

  type Sql92DropTableTableNameSyntax syntax :: Type
  dropTableSyntax :: Sql92DropTableTableNameSyntax syntax -> syntax

class ( IsSql92TableNameSyntax (Sql92AlterTableTableNameSyntax syntax),
        IsSql92AlterTableActionSyntax (Sql92AlterTableAlterTableActionSyntax syntax) ) =>
  IsSql92AlterTableSyntax syntax where

  type Sql92AlterTableTableNameSyntax syntax :: Type
  type Sql92AlterTableAlterTableActionSyntax syntax :: Type

  alterTableSyntax :: Sql92AlterTableTableNameSyntax syntax -> Sql92AlterTableAlterTableActionSyntax syntax
                   -> syntax

class ( IsSql92ColumnSchemaSyntax (Sql92AlterTableColumnSchemaSyntax syntax)
      , IsSql92AlterColumnActionSyntax (Sql92AlterTableAlterColumnActionSyntax syntax) ) =>
  IsSql92AlterTableActionSyntax syntax where
  type Sql92AlterTableAlterColumnActionSyntax syntax :: Type
  type Sql92AlterTableColumnSchemaSyntax syntax :: Type
  alterColumnSyntax :: Text -> Sql92AlterTableAlterColumnActionSyntax syntax
                    -> syntax
  addColumnSyntax :: Text -> Sql92AlterTableColumnSchemaSyntax syntax -> syntax
  dropColumnSyntax :: Text -> syntax
  renameTableToSyntax :: Text -> syntax
  renameColumnToSyntax :: Text -> Text -> syntax

class IsSql92AlterColumnActionSyntax syntax where
  setNotNullSyntax, setNullSyntax :: syntax

class ( IsSql92ColumnConstraintDefinitionSyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema)
      , IsSql92DataTypeSyntax (Sql92ColumnSchemaColumnTypeSyntax columnSchema)
      , Typeable (Sql92ColumnSchemaColumnTypeSyntax columnSchema)
      , Sql92DisplaySyntax (Sql92ColumnSchemaColumnTypeSyntax columnSchema)
      , Hashable (Sql92ColumnSchemaColumnTypeSyntax columnSchema)
      , Eq (Sql92ColumnSchemaColumnTypeSyntax columnSchema)
      , Sql92DisplaySyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema)
      , Eq (Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema)
      , Hashable (Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema)
      , IsSql92ExpressionSyntax (Sql92ColumnSchemaExpressionSyntax columnSchema)
      , Typeable columnSchema, Sql92DisplaySyntax columnSchema, Eq columnSchema, Hashable columnSchema ) =>
  IsSql92ColumnSchemaSyntax columnSchema where
  type Sql92ColumnSchemaColumnTypeSyntax columnSchema :: Type
  type Sql92ColumnSchemaExpressionSyntax columnSchema :: Type
  type Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema :: Type

  columnSchemaSyntax :: Sql92ColumnSchemaColumnTypeSyntax columnSchema {-^ Column type -}
                     -> Maybe (Sql92ColumnSchemaExpressionSyntax columnSchema) {-^ Default value -}
                     -> [ Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema ] {-^ Column constraints -}
                     -> Maybe Text {-^ Default collation -}
                     -> columnSchema

class Typeable constraint => IsSql92TableConstraintSyntax constraint where
  primaryKeyConstraintSyntax :: [ Text ] -> constraint

class Typeable match => IsSql92MatchTypeSyntax match where
  fullMatchSyntax :: match
  partialMatchSyntax :: match

class Typeable refAction => IsSql92ReferentialActionSyntax refAction where
  referentialActionCascadeSyntax :: refAction
  referentialActionSetNullSyntax :: refAction
  referentialActionSetDefaultSyntax :: refAction
  referentialActionNoActionSyntax :: refAction

class ( IsSql92ColumnConstraintSyntax (Sql92ColumnConstraintDefinitionConstraintSyntax constraint)
      , IsSql92ConstraintAttributesSyntax (Sql92ColumnConstraintDefinitionAttributesSyntax constraint)
      , Typeable constraint ) =>
      IsSql92ColumnConstraintDefinitionSyntax constraint where
  type Sql92ColumnConstraintDefinitionConstraintSyntax constraint :: Type
  type Sql92ColumnConstraintDefinitionAttributesSyntax constraint :: Type

  constraintDefinitionSyntax :: Maybe Text -> Sql92ColumnConstraintDefinitionConstraintSyntax constraint
                             -> Maybe (Sql92ColumnConstraintDefinitionAttributesSyntax constraint)
                             -> constraint

class (Semigroup attrs, Monoid attrs, Typeable attrs) => IsSql92ConstraintAttributesSyntax attrs where
  initiallyDeferredAttributeSyntax :: attrs
  initiallyImmediateAttributeSyntax :: attrs
  notDeferrableAttributeSyntax :: attrs
  deferrableAttributeSyntax :: attrs

class ( IsSql92MatchTypeSyntax (Sql92ColumnConstraintMatchTypeSyntax constraint)
      , IsSql92ReferentialActionSyntax (Sql92ColumnConstraintReferentialActionSyntax constraint)
      , Typeable (Sql92ColumnConstraintExpressionSyntax constraint)
      , Typeable constraint ) =>
  IsSql92ColumnConstraintSyntax constraint where
  type Sql92ColumnConstraintMatchTypeSyntax constraint :: Type
  type Sql92ColumnConstraintReferentialActionSyntax constraint :: Type
  type Sql92ColumnConstraintExpressionSyntax constraint :: Type

  notNullConstraintSyntax :: constraint
  uniqueColumnConstraintSyntax :: constraint
  primaryKeyColumnConstraintSyntax :: constraint
  checkColumnConstraintSyntax :: Sql92ColumnConstraintExpressionSyntax constraint -> constraint
  referencesConstraintSyntax :: Text -> [ Text ]
                             -> Maybe (Sql92ColumnConstraintMatchTypeSyntax constraint)
                             -> Maybe (Sql92ColumnConstraintReferentialActionSyntax constraint) {-^ On update -}
                             -> Maybe (Sql92ColumnConstraintReferentialActionSyntax constraint) {-^ On delete -}
                             -> constraint

-- | 'IsSql92DataTypeSyntax'es that can be serialized to JSON
class Sql92SerializableDataTypeSyntax dataType where
  serializeDataType :: dataType -> Value

-- | 'IsSql92ColumnConstraintDefinitionSyntax'es that can be serialized to JSON
class Sql92SerializableConstraintDefinitionSyntax constraint where
  serializeConstraint :: constraint -> Value
