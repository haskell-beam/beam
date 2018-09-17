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

  , HasNullableConstraint, NullableStatus
  ) where

import Database.Beam
import Database.Beam.Backend.SQL

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.SQL.Types
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

class BeamMigrateSqlBackend be => GMigratableTableSettings be (i :: * -> *) fieldCheck where
  gDefaultTblSettingsChecks :: Proxy be -> Proxy i -> Bool -> fieldCheck ()

instance (BeamMigrateSqlBackend be, GMigratableTableSettings be xId fieldCheckId) =>
  GMigratableTableSettings be (M1 t s xId) (M1 t s fieldCheckId) where
  gDefaultTblSettingsChecks be Proxy embedded =
    M1 (gDefaultTblSettingsChecks be (Proxy @xId) embedded)

instance ( BeamMigrateSqlBackend be
         , GMigratableTableSettings be aId aFieldCheck
         , GMigratableTableSettings be bId bFieldCheck ) =>
  GMigratableTableSettings be (aId :*: bId) (aFieldCheck :*: bFieldCheck) where
  gDefaultTblSettingsChecks be Proxy embedded =
    gDefaultTblSettingsChecks be (Proxy @aId) embedded :*:
    gDefaultTblSettingsChecks be (Proxy @bId) embedded

instance ( HasDefaultSqlDataType be haskTy
         , HasDefaultSqlDataTypeConstraints be haskTy
         , HasNullableConstraint (NullableStatus haskTy) be

         , Typeable be, BeamMigrateSqlBackend be ) =>
  GMigratableTableSettings be (Rec0 haskTy) (Rec0 (Const [FieldCheck] haskTy)) where

  gDefaultTblSettingsChecks _ _ embedded =
    K1 (Const (nullableConstraint (Proxy @(NullableStatus haskTy)) (Proxy @be) ++
               defaultSqlDataTypeConstraints (Proxy @haskTy) (Proxy @be) embedded ++
               [ FieldCheck (\tblNm nm -> p (TableHasColumn tblNm nm (defaultSqlDataType (Proxy @haskTy) (Proxy @be) embedded)
                                              :: TableHasColumn be )) ]))

instance ( Generic (embeddedTbl (Const [FieldCheck]))
         , BeamMigrateSqlBackend be
         , GMigratableTableSettings be (Rep (embeddedTbl Identity)) (Rep (embeddedTbl (Const [FieldCheck]))) ) =>
  GMigratableTableSettings be (Rec0 (embeddedTbl Identity)) (Rec0 (embeddedTbl (Const [FieldCheck]))) where

  gDefaultTblSettingsChecks be _ _ =
    K1 (to (gDefaultTblSettingsChecks be (Proxy :: Proxy (Rep (embeddedTbl Identity))) True))

instance ( Generic (embeddedTbl (Nullable (Const [FieldCheck])))
         , BeamMigrateSqlBackend be
         , GMigratableTableSettings be (Rep (embeddedTbl (Nullable Identity))) (Rep (embeddedTbl (Nullable (Const [FieldCheck])))) ) =>
  GMigratableTableSettings be (Rec0 (embeddedTbl (Nullable Identity))) (Rec0 (embeddedTbl (Nullable (Const [FieldCheck])))) where

  gDefaultTblSettingsChecks be _ _ =
    K1 (to (gDefaultTblSettingsChecks be (Proxy :: Proxy (Rep (embeddedTbl (Nullable Identity)))) True))

-- * Nullability check

type family NullableStatus (x :: *) :: Bool where
  NullableStatus (Maybe x) = 'True
  NullableStatus x = 'False

class BeamMigrateSqlBackend be => HasNullableConstraint (x :: Bool) be where
  nullableConstraint :: Proxy x -> Proxy be -> [ FieldCheck ]

instance ( Typeable be, BeamMigrateSqlBackend be ) =>
  HasNullableConstraint 'False be where
  nullableConstraint _ _ =
    let c = constraintDefinitionSyntax Nothing notNullConstraintSyntax Nothing
    in [ FieldCheck $ \tblNm colNm -> p (TableColumnHasConstraint tblNm colNm c :: TableColumnHasConstraint be) ]
instance BeamMigrateSqlBackend be =>
  HasNullableConstraint 'True be where
  nullableConstraint _ _ = []

-- * Default data types

-- | Certain data types also come along with constraints. For example, @SERIAL@
-- types in Postgres generate an automatic @DEFAULT@ constraint.
--
-- You need an instance of this class for any type for which you want beam to be
-- able to infer the SQL data type. If your data type does not have any
-- constraint requirements, you can just declare an empty instance
class BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be ty where

  -- | Provide arbitrary constraints on a field of the requested type. See
  -- 'FieldCheck' for more information on the formatting of constraints.
  defaultSqlDataTypeConstraints
    :: Proxy ty -- ^ Concrete representation of the type
    -> Proxy be -- ^ Concrete representation of the backend
    -> Bool     -- ^ 'True' if this field is embedded in a
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
class BeamMigrateSqlBackend be => HasDefaultSqlDataType be ty where

  -- | Provide a data type for the given type
  defaultSqlDataType :: Proxy ty       -- ^ Concrete representation of the type
                     -> Proxy be       -- ^ Concrete representation of the backend
                     -> Bool           -- ^ 'True' if this field is in an embedded
                                       --   key or table, 'False' otherwise
                     -> BeamSqlBackendDataTypeSyntax be

instance (BeamMigrateSqlBackend be, HasDefaultSqlDataType be ty) =>
  HasDefaultSqlDataType be (Maybe ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)
instance (BeamMigrateSqlBackend be, HasDefaultSqlDataTypeConstraints be ty) =>
  HasDefaultSqlDataTypeConstraints be (Maybe ty) where
  defaultSqlDataTypeConstraints _ = defaultSqlDataTypeConstraints (Proxy @ty)

-- TODO Not sure if individual databases will want to customize these types

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Int where
  defaultSqlDataType _ _ _ = intType
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Int
instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Int32 where
  defaultSqlDataType _ _ _ = intType
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Int32
instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Int16 where
  defaultSqlDataType _ _ _ = intType
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Int16
instance ( BeamMigrateSqlBackend be, BeamSqlT071Backend be ) => HasDefaultSqlDataType be Int64 where
    defaultSqlDataType _ _ _ = bigIntType
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Int64

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Word where
  defaultSqlDataType _ _ _ = numericType (Just (10, Nothing))
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Word

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Word16 where
  defaultSqlDataType _ _ _ = numericType (Just (5, Nothing))
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Word16
instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Word32 where
  defaultSqlDataType _ _ _ = numericType (Just (10, Nothing))
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Word32
instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Word64 where
  defaultSqlDataType _ _ _ = numericType (Just (20, Nothing))
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Word64

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Text where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Text
instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be SqlBitString where
  defaultSqlDataType _ _ _ = varBitType Nothing
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be SqlBitString

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Double where
  defaultSqlDataType _ _ _ = doubleType
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Double
instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Scientific where
  defaultSqlDataType _ _ _ = numericType (Just (20, Just 10))
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Scientific

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Day where
  defaultSqlDataType _ _ _ = dateType
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Day

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be TimeOfDay where
  defaultSqlDataType _ _ _ = timeType Nothing False
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be TimeOfDay

instance BeamMigrateSql99Backend be => HasDefaultSqlDataType be Bool where
  defaultSqlDataType _ _ _ = booleanType
instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be Bool

-- | Constraint synonym to use if you want to assert that a particular
-- 'IsSql92Syntax' syntax supports defaulting for a particular data type
type Sql92HasDefaultDataType syntax ty =
  ( HasDefaultSqlDataType (Sql92DdlCommandDataTypeSyntax syntax) ty
  , HasDefaultSqlDataTypeConstraints (Sql92DdlCommandColumnSchemaSyntax syntax) ty )
