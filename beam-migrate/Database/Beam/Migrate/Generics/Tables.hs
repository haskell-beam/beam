{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Support for defaulting checked tables
module Database.Beam.Migrate.Generics.Tables
  ( -- * Field data type defaulting
    HasDefaultSqlDataType(..)
  , HasDefaultSqlDataTypeConstraints(..)
  , Sql92HasDefaultDataType

  -- * Internal
  , GMigratableTableSettings(..)
  ) where

import Database.Beam
import Database.Beam.Backend.SQL.Types
import Database.Beam.Backend.SQL.SQL2003

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.Checks

import Control.Applicative (Const(..))

import Data.Proxy
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Time.Calendar (Day)
import Data.Time (TimeOfDay)
import Data.Int
import Data.Word

import GHC.Generics

class IsSql92DdlCommandSyntax syntax => GMigratableTableSettings syntax (i :: * -> *) fieldCheck where
  gDefaultTblSettingsChecks :: Proxy syntax -> Proxy i -> Bool -> fieldCheck ()

instance (IsSql92DdlCommandSyntax syntax, GMigratableTableSettings syntax xId fieldCheckId) =>
  GMigratableTableSettings syntax (M1 t s xId) (M1 t s fieldCheckId) where
  gDefaultTblSettingsChecks syntax Proxy embedded =
    M1 (gDefaultTblSettingsChecks syntax (Proxy @xId) embedded)

instance ( IsSql92DdlCommandSyntax syntax
         , GMigratableTableSettings syntax aId aFieldCheck
         , GMigratableTableSettings syntax bId bFieldCheck ) =>
  GMigratableTableSettings syntax (aId :*: bId) (aFieldCheck :*: bFieldCheck) where
  gDefaultTblSettingsChecks syntax Proxy embedded =
    gDefaultTblSettingsChecks syntax (Proxy @aId) embedded :*:
    gDefaultTblSettingsChecks syntax (Proxy @bId) embedded

instance ( HasDefaultSqlDataType (Sql92DdlCommandDataTypeSyntax syntax) haskTy
         , HasDefaultSqlDataTypeConstraints (Sql92DdlCommandColumnSchemaSyntax syntax) haskTy
         , HasNullableConstraint (NullableStatus haskTy) (Sql92DdlCommandColumnSchemaSyntax syntax)

         , IsSql92DdlCommandSyntax syntax
         , Sql92SerializableDataTypeSyntax (Sql92DdlCommandDataTypeSyntax syntax) ) =>
  GMigratableTableSettings syntax (Rec0 haskTy) (Rec0 (Const [FieldCheck] haskTy)) where

  gDefaultTblSettingsChecks _ _ embedded =
    K1 (Const (nullableConstraint (Proxy @(NullableStatus haskTy)) (Proxy @(Sql92DdlCommandColumnSchemaSyntax syntax)) ++
               defaultSqlDataTypeConstraints (Proxy @haskTy) (Proxy @(Sql92DdlCommandColumnSchemaSyntax syntax)) embedded ++
               [ FieldCheck (\tblNm nm -> p (TableHasColumn tblNm nm (defaultSqlDataType (Proxy @haskTy) embedded)
                                              :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax syntax))) ]))

instance ( Generic (embeddedTbl (Const [FieldCheck]))
         , IsSql92DdlCommandSyntax syntax
         , GMigratableTableSettings syntax (Rep (embeddedTbl Identity)) (Rep (embeddedTbl (Const [FieldCheck]))) ) =>
  GMigratableTableSettings syntax (Rec0 (embeddedTbl Identity)) (Rec0 (embeddedTbl (Const [FieldCheck]))) where

  gDefaultTblSettingsChecks syntax _ _ =
    K1 (to (gDefaultTblSettingsChecks syntax (Proxy :: Proxy (Rep (embeddedTbl Identity))) True))

instance ( Generic (embeddedTbl (Nullable (Const [FieldCheck])))
         , IsSql92DdlCommandSyntax syntax
         , GMigratableTableSettings syntax (Rep (embeddedTbl (Nullable Identity))) (Rep (embeddedTbl (Nullable (Const [FieldCheck])))) ) =>
  GMigratableTableSettings syntax (Rec0 (embeddedTbl (Nullable Identity))) (Rec0 (embeddedTbl (Nullable (Const [FieldCheck])))) where

  gDefaultTblSettingsChecks syntax _ _ =
    K1 (to (gDefaultTblSettingsChecks syntax (Proxy :: Proxy (Rep (embeddedTbl (Nullable Identity)))) True))

-- * Nullability check

type family NullableStatus (x :: *) :: Bool where
  NullableStatus (Maybe x) = 'True
  NullableStatus x = 'False

class IsSql92ColumnSchemaSyntax syntax => HasNullableConstraint (x :: Bool) syntax where
  nullableConstraint :: Proxy x -> Proxy syntax -> [ FieldCheck ]

instance ( IsSql92ColumnSchemaSyntax syntax
         , Sql92SerializableConstraintDefinitionSyntax (Sql92ColumnSchemaColumnConstraintDefinitionSyntax syntax) ) =>
  HasNullableConstraint 'False syntax where
  nullableConstraint _ _ =
    let c = constraintDefinitionSyntax Nothing notNullConstraintSyntax Nothing
    in [ FieldCheck $ \tblNm colNm -> p (TableColumnHasConstraint tblNm colNm c :: TableColumnHasConstraint syntax) ]
instance IsSql92ColumnSchemaSyntax syntax =>
  HasNullableConstraint 'True syntax where
  nullableConstraint _ _ = []

-- * Default data types

-- | Certain data types also come along with constraints. For example, @SERIAL@
-- types in Postgres generate an automatic @DEFAULT@ constraint.
--
-- You need an instance of this class for any type for which you want beam to be
-- able to infer the SQL data type. If your data type does not have any
-- constraint requirements, you can just declare an empty instance
class IsSql92ColumnSchemaSyntax columnSchemaSyntax =>
  HasDefaultSqlDataTypeConstraints columnSchemaSyntax ty where

  -- | Provide arbitrary constraints on a field of the requested type. See
  -- 'FieldCheck' for more information on the formatting of constraints.
  defaultSqlDataTypeConstraints
    :: Proxy ty                 -- ^ Concrete representation of the type
    -> Proxy columnSchemaSyntax -- ^ Concrete representation of the syntax
    -> Bool                     -- ^ 'True' if this field is embedded in a
                                --   foreign key, 'False' otherwise. For
                                --   example, @SERIAL@ types in postgres get a
                                --   @DEFAULT@ constraint, but @SERIAL@ types in
                                --   a foreign key do not.
    -> [ FieldCheck ]
  defaultSqlDataTypeConstraints _ _ _ = []

-- | Used to define a default SQL data type for a haskell type in a particular
-- data type syntax.
--
-- Beam defines instances for several standard SQL types, which are polymorphic
-- over any standard data type syntax. Backends or extensions which provide
-- custom types should instantiate instances of this class and
-- 'HasDefaultSqlDataTypeConstraints' for any types they provide for which they
-- would like checked schema migrations
class IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax ty where

  -- | Provide a data type for the given type
  defaultSqlDataType :: Proxy ty       -- ^ Concrete representation of the type
                     -> Bool           -- ^ 'True' if this field is in an embedded
                                       --   key or table, 'False' otherwise
                     -> dataTypeSyntax

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
instance IsSql2008BigIntDataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Int64 where
    defaultSqlDataType _ _ = bigIntType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Int64

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Word where
  defaultSqlDataType _ _ = numericType (Just (10, Nothing))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Word

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Word16 where
  defaultSqlDataType _ _ = numericType (Just (5, Nothing))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Word16
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Word32 where
  defaultSqlDataType _ _ = numericType (Just (10, Nothing))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Word32
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Word64 where
  defaultSqlDataType _ _ = numericType (Just (20, Nothing))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Word64

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Text where
  defaultSqlDataType _ _ = varCharType Nothing Nothing
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Text
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax SqlBitString where
  defaultSqlDataType _ _ = varBitType Nothing
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax SqlBitString

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Double where
  defaultSqlDataType _ _ = doubleType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Double
instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Scientific where
  defaultSqlDataType _ _ = numericType (Just (20, Just 10))
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Scientific

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Day where
  defaultSqlDataType _ _ = dateType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Day

instance IsSql92DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax TimeOfDay where
  defaultSqlDataType _ _ = timeType Nothing False
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax TimeOfDay

instance IsSql99DataTypeSyntax dataTypeSyntax => HasDefaultSqlDataType dataTypeSyntax Bool where
  defaultSqlDataType _ _ = booleanType
instance IsSql92ColumnSchemaSyntax columnSchemaSyntax => HasDefaultSqlDataTypeConstraints columnSchemaSyntax Bool

-- | Constraint synonym to use if you want to assert that a particular
-- 'IsSql92Syntax' syntax supports defaulting for a particular data type
type Sql92HasDefaultDataType syntax ty =
  ( HasDefaultSqlDataType (Sql92DdlCommandDataTypeSyntax syntax) ty
  , HasDefaultSqlDataTypeConstraints (Sql92DdlCommandColumnSchemaSyntax syntax) ty )
