{-# LANGUAGE ConstraintKinds #-}

module Database.Beam.Migrate.Generics.Tables where

import Database.Beam
import Database.Beam.Backend.SQL.Types
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Backend.SQL.SQL2003
import Database.Beam.Schema.Tables

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.Checks

import Data.Proxy
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Time.Calendar (Day)
import Data.Int
import Data.Word

import GHC.Generics

class IsSql92DdlCommandSyntax syntax => GMigratableTableSettings syntax s (i :: * -> *) where
  gDefaultTblSettingsChecks :: Proxy syntax -> Proxy i -> Bool -> s () -> [TableCheck]

instance (IsSql92DdlCommandSyntax syntax, GMigratableTableSettings syntax xStgs xId) =>
  GMigratableTableSettings syntax (M1 t s xStgs) (M1 t s xId) where
  gDefaultTblSettingsChecks syntax Proxy embedded (M1 x) =
    gDefaultTblSettingsChecks syntax (Proxy @xId) embedded x

instance ( IsSql92DdlCommandSyntax syntax
         , GMigratableTableSettings syntax aStgs aId
         , GMigratableTableSettings syntax bStgs bId ) =>
  GMigratableTableSettings syntax (aStgs :*: bStgs) (aId :*: bId) where
  gDefaultTblSettingsChecks syntax Proxy embedded (a :*: b) =
    gDefaultTblSettingsChecks syntax (Proxy @aId) embedded a ++
    gDefaultTblSettingsChecks syntax (Proxy @bId) embedded b

instance ( HasDefaultSqlDataType (Sql92DdlCommandDataTypeSyntax syntax) haskTy
         , HasDefaultSqlDataTypeConstraints (Sql92DdlCommandColumnSchemaSyntax syntax) haskTy
         , HasNullableConstraint (NullableStatus haskTy) (Sql92DdlCommandColumnSchemaSyntax syntax)
         , IsSql92DdlCommandSyntax syntax ) =>
  GMigratableTableSettings syntax (Rec0 (TableField tbl x)) (Rec0 haskTy) where

  gDefaultTblSettingsChecks _ _ embedded (K1 (TableField nm)) =
    nullableConstraint nm (Proxy @(NullableStatus haskTy)) (Proxy @(Sql92DdlCommandColumnSchemaSyntax syntax)) ++
    defaultSqlDataTypeConstraints (Proxy @haskTy) (Proxy @(Sql92DdlCommandColumnSchemaSyntax syntax)) nm embedded ++
    [ TableCheck (\tblNm -> p (TableHasColumn tblNm nm (defaultSqlDataType (Proxy @haskTy) embedded) :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax syntax))) ]

instance ( Generic (embeddedTbl (TableField tbl))
         , IsSql92DdlCommandSyntax syntax
         , GMigratableTableSettings syntax (Rep (embeddedTbl (TableField tbl))) (Rep (embeddedTbl Identity)) ) =>
  GMigratableTableSettings syntax (Rec0 (embeddedTbl (TableField tbl))) (Rec0 (embeddedTbl Identity)) where

  gDefaultTblSettingsChecks syntax _ _   (K1 embeddedTbl) =
    gDefaultTblSettingsChecks syntax (Proxy :: Proxy (Rep (embeddedTbl Identity))) True (from embeddedTbl)

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

class IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax ty where
  defaultSqlDataTypeConstraints :: Proxy ty -> Proxy columnSchemaSyntax -> Text -> Bool {-^ Embedded -} -> [ TableCheck ]
  defaultSqlDataTypeConstraints _ _ _ _ = []

class IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax ty where
  defaultSqlDataType :: Proxy ty -> Bool {-^ Embedded -} -> dataTypeSyntax

instance (IsSql92DataTypeSyntax dataTypeSyntax, HasDefaultSqlDataType dataTypeSyntax ty) =>
  HasDefaultSqlDataType dataTypeSyntax (Auto ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)
instance (IsSql92ColumnSchemaSyntax columnSchemaSyntax, HasDefaultSqlDataTypeConstraints columnSchemaSyntax ty) =>
  HasDefaultSqlDataTypeConstraints columnSchemaSyntax (Auto ty) where
  defaultSqlDataTypeConstraints _ = defaultSqlDataTypeConstraints (Proxy @ty)

instance (IsSql92DataTypeSyntax dataTypeSyntax, HasDefaultSqlDataType dataTypeSyntax ty) =>
  HasDefaultSqlDataType dataTypeSyntax (Maybe ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)
instance (IsSql92ColumnSchemaSyntax columnSchemaSyntax, HasDefaultSqlDataTypeConstraints columnSchemaSyntax ty) =>
  HasDefaultSqlDataTypeConstraints columnSchemaSyntax (Maybe ty) where
  defaultSqlDataTypeConstraints _ = defaultSqlDataTypeConstraints (Proxy @ty)

-- TODO Not sure if individual databases will want to customize these types

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Int where
  defaultSqlDataType _ _ = intType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Int
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Int32 where
  defaultSqlDataType _ _ = intType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Int32
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Int16 where
  defaultSqlDataType _ _ = intType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Int16

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Word where
  defaultSqlDataType _ _ = numericType (Just (10, Nothing))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Word
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Word32 where
  defaultSqlDataType _ _ = numericType (Just (10, Nothing))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Word32
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Word16 where
  defaultSqlDataType _ _ = numericType (Just (5, Nothing))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Word16

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Text where
  defaultSqlDataType _ _ = varCharType Nothing Nothing
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Text
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax SqlBitString where
  defaultSqlDataType _ _ = varBitType Nothing
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax SqlBitString

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Double where
  defaultSqlDataType _ _ = realType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Double
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Scientific where
  defaultSqlDataType _ _ = numericType (Just (20, Just 10))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Scientific

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Day where
  defaultSqlDataType _ _ = dateType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Day

type Sql92HasDefaultDataType syntax ty =
  ( HasDefaultSqlDataType (Sql92DdlCommandDataTypeSyntax syntax) ty
  , HasDefaultSqlDataTypeConstraints (Sql92DdlCommandColumnSchemaSyntax syntax) ty )
