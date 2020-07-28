{-# LANGUAGE UndecidableInstances #-}

-- | Checked database types
module Database.Beam.Migrate.Types.CheckedEntities where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Schema.Tables

import Database.Beam.Migrate.Checks
import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.Types.Predicates

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Identity

import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.String

import GHC.Types
import GHC.Generics

import Lens.Micro (Lens', (&), (^.), (.~), (%~))

-- * Checked Database Entities

-- | Like 'IsDatabaseEntity' in @beam-core@, but for entities against which we
-- can generate 'DatabasePredicate's. Conceptually, this is the same as
-- 'IsDatabaseEntity', but with one extra function to generate
-- 'DatabasePredicate's from the description.
class IsDatabaseEntity be entity => IsCheckedDatabaseEntity be entity where
  -- | The type of the descriptor for this checked entity. Usually this wraps
  -- the corresponding 'DatabaseEntityDescriptor' from 'IsDatabaseEntity', along
  -- with some mechanism for generating 'DatabasePredicate's.
  data CheckedDatabaseEntityDescriptor be entity :: *

  -- | Like 'DatabaseEntityDefaultRequirements' but for checked entities
  type CheckedDatabaseEntityDefaultRequirements be entity :: Constraint

  -- | Produce the corresponding 'DatabaseEntityDescriptor'
  unCheck :: CheckedDatabaseEntityDescriptor be entity -> DatabaseEntityDescriptor be entity
  unCheck d = d ^. unChecked

  -- | A lens to access the internal unchecked descriptor
  unChecked :: Lens' (CheckedDatabaseEntityDescriptor be entity) (DatabaseEntityDescriptor be entity)

  -- | Produce the set of 'DatabasePredicate's that apply to this entity
  collectEntityChecks :: CheckedDatabaseEntityDescriptor be entity -> [ SomeDatabasePredicate ]

  -- | Like 'dbEntityAuto' but for checked databases. Most often, this wraps
  -- 'dbEntityAuto' and provides some means to generate 'DatabasePredicate's
  checkedDbEntityAuto :: CheckedDatabaseEntityDefaultRequirements be entity
                      => Text -> CheckedDatabaseEntityDescriptor be entity

-- | Like 'DatabaseEntity' but for checked databases
data CheckedDatabaseEntity be (db :: (* -> *) -> *) entityType where
  CheckedDatabaseEntity :: IsCheckedDatabaseEntity be entityType
                        => CheckedDatabaseEntityDescriptor be entityType
                        -> [ SomeDatabasePredicate ]
                        -> CheckedDatabaseEntity be db entityType

-- | The type of a checked database descriptor. Conceptually, this is just a
-- 'DatabaseSettings' with a set of predicates. Use 'unCheckDatabase' to get the
-- regular 'DatabaseSettings' object and 'collectChecks' to access the
-- predicates.
type CheckedDatabaseSettings be db = db (CheckedDatabaseEntity be db)

renameCheckedEntity :: (Text -> Text) -> EntityModification (CheckedDatabaseEntity be db) be ent
renameCheckedEntity renamer =
  EntityModification (Endo (\(CheckedDatabaseEntity desc checks) -> (CheckedDatabaseEntity (desc & unChecked . dbEntityName %~ renamer) checks)))

-- | Convert a 'CheckedDatabaseSettings' to a regular 'DatabaseSettings'. The
-- return value is suitable for use in any regular beam query or DML statement.
unCheckDatabase :: forall be db. Database be db => CheckedDatabaseSettings be db -> DatabaseSettings be db
unCheckDatabase db = runIdentity $ zipTables (Proxy @be) (\(CheckedDatabaseEntity x _) _ -> pure $ DatabaseEntity (unCheck x)) db db

-- | A @beam-migrate@ database schema is defined completely by the set of
-- predicates that apply to it. This function allows you to access this
-- definition for a 'CheckedDatabaseSettings' object.
collectChecks :: forall be db. Database be  db => CheckedDatabaseSettings be db -> [ SomeDatabasePredicate ]
collectChecks db = let (_ :: CheckedDatabaseSettings be db, a) =
                         runWriter $ zipTables (Proxy @be)
                           (\(CheckedDatabaseEntity entity cs :: CheckedDatabaseEntity be db entityType) b ->
                              do tell (collectEntityChecks entity)
                                 tell cs
                                 pure b) db db
                   in a

instance IsCheckedDatabaseEntity be (DomainTypeEntity ty) where
  data CheckedDatabaseEntityDescriptor be (DomainTypeEntity ty) =
    CheckedDatabaseDomainType (DatabaseEntityDescriptor be (DomainTypeEntity ty))
                              [ DomainCheck ]
  type CheckedDatabaseEntityDefaultRequirements be (DomainTypeEntity ty) =
    DatabaseEntityDefaultRequirements be (DomainTypeEntity ty)

  unChecked f (CheckedDatabaseDomainType x cks) = fmap (\x' -> CheckedDatabaseDomainType x' cks) (f x)
  collectEntityChecks (CheckedDatabaseDomainType dt domainChecks) =
    map (\(DomainCheck mkCheck) -> mkCheck (qname dt)) domainChecks
  checkedDbEntityAuto domTypeName =
    CheckedDatabaseDomainType (dbEntityAuto domTypeName) []

instance Beamable tbl => IsCheckedDatabaseEntity be (TableEntity tbl) where
  data CheckedDatabaseEntityDescriptor be (TableEntity tbl) where
    CheckedDatabaseTable :: Table tbl
                         => DatabaseEntityDescriptor be (TableEntity tbl)
                         -> [ TableCheck ]
                         -> tbl (Const [FieldCheck])
                         -> CheckedDatabaseEntityDescriptor be (TableEntity tbl)

  type CheckedDatabaseEntityDefaultRequirements be (TableEntity tbl)  =
    ( DatabaseEntityDefaultRequirements be (TableEntity tbl)
    , Generic (tbl (Const [FieldCheck]))
    , GMigratableTableSettings be (Rep (tbl Identity)) (Rep (tbl (Const [FieldCheck])))
    , BeamSqlBackend be )

  unChecked f (CheckedDatabaseTable x cks fcks) = fmap (\x' -> CheckedDatabaseTable x' cks fcks) (f x)
  collectEntityChecks (CheckedDatabaseTable dt tblChecks tblFieldChecks) =
    catMaybes (map (\(TableCheck mkCheck) -> mkCheck (qname dt) (dbTableSettings dt)) tblChecks) <>
    execWriter (zipBeamFieldsM (\(Columnar' fd) c@(Columnar' (Const fieldChecks)) ->
                                    tell (map (\(FieldCheck mkCheck) -> mkCheck (qname dt) (fd ^. fieldName)) fieldChecks) >>
                                    pure c)
                               (dbTableSettings dt) tblFieldChecks)

  checkedDbEntityAuto tblTypeName =
    let tblChecks =
          [ TableCheck $ \tblName _ ->
              Just (SomeDatabasePredicate (TableExistsPredicate tblName))
          , TableCheck $ \tblName tblFields ->
              case allBeamValues (\(Columnar' fd) -> fd ^. fieldName) (primaryKey tblFields) of
                [] -> Nothing
                pkFields -> Just (SomeDatabasePredicate (TableHasPrimaryKey tblName pkFields))
          ]

        fieldChecks = to (gDefaultTblSettingsChecks (Proxy @be) (Proxy @(Rep (tbl Identity))) False)
    in CheckedDatabaseTable (dbEntityAuto tblTypeName) tblChecks fieldChecks

-- | Purposefully opaque type describing how to modify a table field. Used to
-- parameterize the second argument to 'modifyCheckedTable'. For now, the only
-- way to construct a value is the 'IsString' instance, which allows you to
-- rename the field.
data CheckedFieldModification tbl a
  = CheckedFieldModification
      (TableField tbl a -> TableField tbl a)
      ([FieldCheck] -> [FieldCheck])

checkedFieldNamed :: Text -> CheckedFieldModification tbl a
checkedFieldNamed t = CheckedFieldModification (fieldName .~ t) id

instance IsString (CheckedFieldModification tbl a) where
  fromString = checkedFieldNamed . fromString

instance Beamable tbl => RenamableWithRule (tbl (CheckedFieldModification tbl)) where
  renamingFields renamer =
    runIdentity $
    zipBeamFieldsM (\(Columnar' _ :: Columnar' Ignored x) (Columnar' _ :: Columnar' Ignored x) ->
                       pure (Columnar' (CheckedFieldModification (renameField (Proxy @(TableField tbl)) (Proxy @x) renamer) id :: CheckedFieldModification tbl x) ::
                               Columnar' (CheckedFieldModification tbl) x))
                   (undefined :: TableSkeleton tbl) (undefined :: TableSkeleton tbl)

-- | Modify a checked table.
--
--   The first argument is a function that takes the original table name as
--   input and produces a new table name.
--
--   The second argument gives instructions on how to rename each field in the
--   table. Use 'checkedTableModification' to create a value of this type which
--   does no renaming. Each field in the table supplied here has the type
--   'CheckedFieldModification'. Most commonly, the programmer will use the
--   @OverloadedStrings@ instance to provide a new name.
--
-- == Examples
--
--    Rename a table, without renaming any of its fields:
--
-- @
-- modifyCheckedTable (\_ -> "NewTblNm") checkedTableModification
-- @
--
--    Modify a table, renaming the field called @_field1@ in Haskell to
--    "FirstName". Note that below, @"FirstName"@ represents a
--    'CheckedFieldModification' object.
--
-- @
-- modifyCheckedTable id (checkedTableModification { _field1 = "FirstName" })
-- @

modifyCheckedTable
  :: ( Text -> Text )
  -> tbl (CheckedFieldModification tbl)
  -> EntityModification (CheckedDatabaseEntity be db) be (TableEntity tbl)
modifyCheckedTable renamer modFields =
  EntityModification $ Endo $
  \(CheckedDatabaseEntity (CheckedDatabaseTable dt tblChecks fieldChecks) extraChecks) ->
    let fields' =
          runIdentity $
          zipBeamFieldsM (\(Columnar' (CheckedFieldModification fieldMod _)) (Columnar' field) ->
                             pure $ Columnar' (fieldMod field))
                         modFields (dbTableSettings dt)
        fieldChecks' =
          runIdentity $
          zipBeamFieldsM (\(Columnar' (CheckedFieldModification _ csMod)) (Columnar' (Const cs)) ->
                             pure $ Columnar' (Const (csMod cs)))
                         modFields fieldChecks
    in CheckedDatabaseEntity (CheckedDatabaseTable
                                (dt { dbTableCurrentName = renamer (dbTableCurrentName dt)
                                    , dbTableSettings = fields'})
                                tblChecks fieldChecks') extraChecks

-- | Produce a table field modification that does nothing
--
--   Most commonly supplied as the second argument to 'modifyCheckedTable' when
--   you just want to rename the table, not the fields.
checkedTableModification :: forall tbl. Beamable tbl => tbl (CheckedFieldModification tbl)
checkedTableModification =
  runIdentity $
  zipBeamFieldsM (\(Columnar' _ :: Columnar' Ignored x) (Columnar' _ :: Columnar' Ignored x) ->
                    pure (Columnar' (CheckedFieldModification id id :: CheckedFieldModification tbl x)))
                 (undefined :: TableSkeleton tbl) (undefined :: TableSkeleton tbl)
