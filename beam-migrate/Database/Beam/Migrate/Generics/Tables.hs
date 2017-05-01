module Database.Beam.Migrate.Generics.Tables where

import Database.Beam
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Schema.Tables

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.Checks

import Data.Proxy
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Int

import GHC.Generics

class IsSql92DdlCommandSyntax syntax => GMigratableTableSettings syntax s (i :: * -> *) where
  gDefaultTblSettingsChecks :: Proxy syntax -> Proxy i -> s () -> [TableCheck]

instance (IsSql92DdlCommandSyntax syntax, GMigratableTableSettings syntax xStgs xId) =>
  GMigratableTableSettings syntax (M1 t s xStgs) (M1 t s xId) where
  gDefaultTblSettingsChecks syntax Proxy (M1 x) =
    gDefaultTblSettingsChecks syntax (Proxy @xId) x

instance ( IsSql92DdlCommandSyntax syntax
         , GMigratableTableSettings syntax aStgs aId
         , GMigratableTableSettings syntax bStgs bId ) =>
  GMigratableTableSettings syntax (aStgs :*: bStgs) (aId :*: bId) where
  gDefaultTblSettingsChecks syntax Proxy (a :*: b) =
    gDefaultTblSettingsChecks syntax (Proxy @aId) a ++
    gDefaultTblSettingsChecks syntax (Proxy @bId) b

instance ( HasDefaultSqlDataType (Sql92DdlCommandDataTypeSyntax syntax) haskTy
         , HasNullableConstraint (NullableStatus haskTy) (Sql92DdlCommandColumnSchemaSyntax syntax)
         , IsSql92DdlCommandSyntax syntax ) =>
  GMigratableTableSettings syntax (Rec0 (TableField tbl x)) (Rec0 haskTy) where

  gDefaultTblSettingsChecks _ _ (K1 (TableField nm)) =
    nullableConstraint nm (Proxy @(NullableStatus haskTy)) (Proxy @(Sql92DdlCommandColumnSchemaSyntax syntax)) ++
    [ TableCheck (\tblNm -> p (TableHasColumn tblNm nm (defaultSqlDataType (Proxy @haskTy)) :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax syntax))) ]

-- * Nullability check

type family NullableStatus (x :: *) :: Bool where
  NullableStatus (Maybe x) = 'True
  NullableStatus x = 'False

class IsSql92ColumnSchemaSyntax syntax => HasNullableConstraint (x :: Bool) syntax where
  nullableConstraint :: Text -> Proxy x -> Proxy syntax -> [ TableCheck ]
instance IsSql92ColumnSchemaSyntax syntax =>
  HasNullableConstraint 'False syntax where
  nullableConstraint colNm _ _ =
    let c = constraintDefinitionSyntax Nothing notNullConstraintSyntax Nothing
    in [ TableCheck $ \tblNm -> p (TableColumnHasConstraint tblNm colNm c :: TableColumnHasConstraint syntax) ]
instance IsSql92ColumnSchemaSyntax syntax =>
  HasNullableConstraint 'True syntax where
  nullableConstraint _ _ _ = []

-- * Default data types

class IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax ty where
  defaultSqlDataType :: Proxy ty -> dataTypeSyntax

instance (IsSql92DataTypeSyntax dataTypeSyntax, HasDefaultSqlDataType dataTypeSyntax ty) =>
  HasDefaultSqlDataType dataTypeSyntax (Auto ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)

instance (IsSql92DataTypeSyntax dataTypeSyntax, HasDefaultSqlDataType dataTypeSyntax ty) =>
  HasDefaultSqlDataType dataTypeSyntax (Maybe ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)

-- TODO Not sure if individual databases will want to customize these types

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Int where
  defaultSqlDataType _ = intType
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Int32 where
  defaultSqlDataType _ = intType
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Int16 where
  defaultSqlDataType _ = intType

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Text where
  defaultSqlDataType _ = varCharType Nothing Nothing

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Double where
  defaultSqlDataType _ = realType
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Scientific where
  defaultSqlDataType _ = numericType (Just (20, Just 10))
