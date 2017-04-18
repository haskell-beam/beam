module Database.Beam.Migrate.SQL.SQL92 where

import Database.Beam.Backend.SQL.SQL92

import Data.Text (Text)
import Data.Typeable

type Sql92DdlCommandDataTypeSyntax syntax =
  Sql92ColumnSchemaColumnTypeSyntax (Sql92DdlCommandColumnSchemaSyntax syntax)
type Sql92DdlCommandColumnSchemaSyntax syntax = Sql92CreateTableColumnSchemaSyntax (Sql92DdlCommandCreateTableSyntax syntax)

class (IsSql92CreateTableSyntax (Sql92DdlCommandCreateTableSyntax syntax)) =>
  IsSql92DdlCommandSyntax syntax where
  type Sql92DdlCommandCreateTableSyntax syntax :: *

  createTableCmd :: Sql92DdlCommandCreateTableSyntax syntax -> syntax

class ( IsSql92TableConstraintSyntax (Sql92CreateTableTableConstraintSyntax syntax)
      , IsSql92ColumnSchemaSyntax (Sql92CreateTableColumnSchemaSyntax syntax) ) =>
    IsSql92CreateTableSyntax syntax where
  type Sql92CreateTableColumnSchemaSyntax syntax :: *
  type Sql92CreateTableTableConstraintSyntax syntax :: *
  type Sql92CreateTableOptionsSyntax syntax :: *

  createTableSyntax :: Maybe (Sql92CreateTableOptionsSyntax syntax)
                    -> Text
                    -> [(Text, Sql92CreateTableColumnSchemaSyntax syntax)]
                    -> [ Sql92CreateTableTableConstraintSyntax syntax ]
                    -> syntax

class ( IsSql92ColumnConstraintDefinitionSyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema)
      , IsSql92DataTypeSyntax (Sql92ColumnSchemaColumnTypeSyntax columnSchema)
      , Show (Sql92ColumnSchemaColumnTypeSyntax columnSchema)
      , Eq (Sql92ColumnSchemaColumnTypeSyntax columnSchema)
      , IsSql92ExpressionSyntax (Sql92ColumnSchemaExpressionSyntax columnSchema)
      , Typeable columnSchema, Show columnSchema, Eq columnSchema ) =>
  IsSql92ColumnSchemaSyntax columnSchema where
  type Sql92ColumnSchemaColumnTypeSyntax columnSchema :: *
  type Sql92ColumnSchemaExpressionSyntax columnSchema :: *
  type Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema :: *

  columnSchemaSyntax :: Sql92ColumnSchemaColumnTypeSyntax columnSchema {-^ Column type -}
                     -> Maybe (Sql92ColumnSchemaExpressionSyntax columnSchema) {-^ Default value -}
                     -> [ Sql92ColumnSchemaColumnConstraintDefinitionSyntax columnSchema ] {-^ Column constraints -}
                     -> Maybe Text {-^ Default collation -}
                     -> columnSchema

class IsSql92TableConstraintSyntax constraint where
  primaryKeyConstraintSyntax :: [ Text ] -> constraint

class IsSql92MatchTypeSyntax match where
  fullMatchSyntax :: match
  partialMatchSyntax :: match

class IsSql92ReferentialActionSyntax refAction where
  referentialActionCascadeSyntax :: refAction
  referentialActionSetNullSyntax :: refAction
  referentialActionSetDefaultSyntax :: refAction
  referentialActionNoActionSyntax :: refAction

class ( IsSql92ColumnConstraintSyntax (Sql92ColumnConstraintDefinitionConstraintSyntax constraint)
      , IsSql92ConstraintAttributesSyntax (Sql92ColumnConstraintDefinitionAttributesSyntax constraint)) =>
      IsSql92ColumnConstraintDefinitionSyntax constraint where
  type Sql92ColumnConstraintDefinitionConstraintSyntax constraint :: *
  type Sql92ColumnConstraintDefinitionAttributesSyntax constraint :: *

  constraintDefinitionSyntax :: Maybe Text -> Sql92ColumnConstraintDefinitionConstraintSyntax constraint
                             -> Maybe (Sql92ColumnConstraintDefinitionAttributesSyntax constraint)
                             -> constraint

class Monoid attrs => IsSql92ConstraintAttributesSyntax attrs where
  initiallyDeferredAttributeSyntax :: attrs
  initiallyImmediateAttributeSyntax :: attrs
  notDeferrableAttributeSyntax :: attrs
  deferrableAttributeSyntax :: attrs

class ( IsSql92MatchTypeSyntax (Sql92ColumnConstraintMatchTypeSyntax constraint)
      , IsSql92ReferentialActionSyntax (Sql92ColumnConstraintReferentialActionSyntax constraint) )=>
  IsSql92ColumnConstraintSyntax constraint where
  type Sql92ColumnConstraintMatchTypeSyntax constraint :: *
  type Sql92ColumnConstraintReferentialActionSyntax constraint :: *
  type Sql92ColumnConstraintExpressionSyntax constraint :: *

  notNullConstraintSyntax :: constraint
  uniqueColumnConstraintSyntax :: constraint
  primaryKeyColumnConstraintSyntax :: constraint
  checkColumnConstraintSyntax :: Sql92ColumnConstraintExpressionSyntax constraint -> constraint
  referencesConstraintSyntax :: Text -> [ Text ]
                             -> Maybe (Sql92ColumnConstraintMatchTypeSyntax constraint)
                             -> Maybe (Sql92ColumnConstraintReferentialActionSyntax constraint) {-^ On update -}
                             -> Maybe (Sql92ColumnConstraintReferentialActionSyntax constraint) {-^ On delete -}
                             -> constraint
