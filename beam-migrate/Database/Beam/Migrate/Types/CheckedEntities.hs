{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Migrate.Types.CheckedEntities where

import Database.Beam
import Database.Beam.Schema.Tables

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.Checks

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Identity

import Data.Proxy
import Data.Text (Text)
import Data.String

import GHC.Types
import GHC.Generics

-- * Checked Database Entities

class IsDatabaseEntity be entity => IsCheckedDatabaseEntity be entity where
  data CheckedDatabaseEntityDescriptor be entity :: *
  type CheckedDatabaseEntityDefaultRequirements be entity syntax :: Constraint

  unCheck :: CheckedDatabaseEntityDescriptor be entity -> DatabaseEntityDescriptor be entity
  collectEntityChecks :: CheckedDatabaseEntityDescriptor be entity -> [ SomeDatabasePredicate ]

  checkedDbEntityAuto :: CheckedDatabaseEntityDefaultRequirements be entity syntax
                      => Proxy syntax -> Text -> CheckedDatabaseEntityDescriptor be entity

data CheckedDatabaseEntity be (db :: (* -> *) -> *) entityType where
  CheckedDatabaseEntity :: IsCheckedDatabaseEntity be entityType
                        => CheckedDatabaseEntityDescriptor be entityType
                        -> [ SomeDatabasePredicate ]
                        -> CheckedDatabaseEntity be db entityType

type CheckedDatabaseSettings be db = db (CheckedDatabaseEntity be db)

unCheckDatabase :: forall be db. Database db => CheckedDatabaseSettings be db -> DatabaseSettings be db
unCheckDatabase db = runIdentity $ zipTables (Proxy @be) (\(CheckedDatabaseEntity x _) _ -> pure $ DatabaseEntity (unCheck x)) db db

collectChecks :: forall be db. Database db => CheckedDatabaseSettings be db -> [ SomeDatabasePredicate ]
collectChecks db = let x :: CheckedDatabaseSettings be db
                       (x, a) = runWriter $ zipTables (Proxy @be) 
                                  (\(CheckedDatabaseEntity entity cs :: CheckedDatabaseEntity be db entityType) b ->
                                     do tell (collectEntityChecks entity)
                                        tell cs
                                        pure b) db db
                   in a

instance IsCheckedDatabaseEntity be (DomainTypeEntity ty) where
  data CheckedDatabaseEntityDescriptor be (DomainTypeEntity ty) =
    CheckedDatabaseDomainType (DatabaseEntityDescriptor be (DomainTypeEntity ty))
                              [ DomainCheck ]
  type CheckedDatabaseEntityDefaultRequirements be (DomainTypeEntity ty) syntax =
    DatabaseEntityDefaultRequirements be (DomainTypeEntity ty)

  unCheck (CheckedDatabaseDomainType x _) = x
  collectEntityChecks (CheckedDatabaseDomainType (DatabaseDomainType domName) domainChecks) =
    map (\(DomainCheck mkCheck) -> mkCheck domName) domainChecks
  checkedDbEntityAuto _ domTypeName =
    CheckedDatabaseDomainType (dbEntityAuto domTypeName) []

instance Beamable tbl => IsCheckedDatabaseEntity be (TableEntity tbl) where
  data CheckedDatabaseEntityDescriptor be (TableEntity tbl) where
    CheckedDatabaseTable :: Table tbl
                         => DatabaseEntityDescriptor be (TableEntity tbl)
                         -> [ TableCheck tbl ]
                         -> tbl (Const [FieldCheck])
                         -> CheckedDatabaseEntityDescriptor be (TableEntity tbl)

  type CheckedDatabaseEntityDefaultRequirements be (TableEntity tbl) syntax =
    ( DatabaseEntityDefaultRequirements be (TableEntity tbl)
    , Generic (tbl (Const [FieldCheck]))
    , GMigratableTableSettings syntax (Rep (tbl Identity)) (Rep (tbl (Const [FieldCheck])))
    , IsSql92DdlCommandSyntax syntax )

  unCheck (CheckedDatabaseTable x _ _) = x

  collectEntityChecks (CheckedDatabaseTable (DatabaseTable tbl tblFields) tblChecks fieldChecks) =
    map (\(TableCheck mkCheck) -> mkCheck tbl tblFields) tblChecks <>
    execWriter (zipBeamFieldsM (\(Columnar' (TableField fieldNm)) c@(Columnar' (Const fieldChecks)) ->
                                    tell (map (\(FieldCheck mkCheck) -> mkCheck tbl fieldNm) fieldChecks) >>
                                    pure c)
                               tblFields fieldChecks)

  checkedDbEntityAuto syntax tblTypeName =
    let tblChecks =
          [ TableCheck (\tblName _ -> SomeDatabasePredicate (TableExistsPredicate tblName))
          , TableCheck (\tblName tblFields ->
                           let pk = allBeamValues (\(Columnar' (TableField x)) -> x) (primaryKey tblFields)
                           in SomeDatabasePredicate (TableHasPrimaryKey tblName pk)) ]

        fieldChecks = to (gDefaultTblSettingsChecks syntax (Proxy @(Rep (tbl Identity))) False)
    in CheckedDatabaseTable (dbEntityAuto tblTypeName) tblChecks fieldChecks

data CheckedFieldModification tbl a
  = CheckedFieldModification
      (TableField tbl a -> TableField tbl a)
      ([FieldCheck] -> [FieldCheck])

instance IsString (CheckedFieldModification tbl a) where
  fromString s = CheckedFieldModification (const . TableField . fromString $ s) id

instance Beamable tbl => RenamableWithRule (tbl (CheckedFieldModification tbl)) where
  renamingFields renamer =
    runIdentity $
    zipBeamFieldsM (\(Columnar' _ :: Columnar' Ignored x) (Columnar' _ :: Columnar' Ignored x) ->
                       pure (Columnar' (CheckedFieldModification (renameField (Proxy @(TableField tbl)) (Proxy @x) renamer) id :: CheckedFieldModification tbl x) ::
                               Columnar' (CheckedFieldModification tbl) x))
                   (undefined :: TableSkeleton tbl) (undefined :: TableSkeleton tbl)

modifyCheckedTable
  :: ( Text -> Text )
  -> tbl (CheckedFieldModification tbl)
  -> EntityModification (CheckedDatabaseEntity be db) be (TableEntity tbl)
modifyCheckedTable renamer modFields =
  EntityModification (\(CheckedDatabaseEntity (CheckedDatabaseTable (DatabaseTable nm fields) tblChecks fieldChecks) extraChecks) ->
                          let fields' = runIdentity $
                                        zipBeamFieldsM (\(Columnar' (CheckedFieldModification mod _)) (Columnar' field) ->
                                                           pure $ Columnar' (mod field))
                                                       modFields fields
                              fieldChecks' = runIdentity $
                                             zipBeamFieldsM (\(Columnar' (CheckedFieldModification _ mod)) (Columnar' (Const cs)) ->
                                                                pure $ Columnar' (Const (mod cs)))
                                                            modFields fieldChecks
                          in CheckedDatabaseEntity (CheckedDatabaseTable (DatabaseTable (renamer nm) fields') tblChecks fieldChecks') extraChecks)

checkedTableModification :: forall tbl. Beamable tbl => tbl (CheckedFieldModification tbl)
checkedTableModification =
  runIdentity $
  zipBeamFieldsM (\(Columnar' _ :: Columnar' Ignored x) (Columnar' _ :: Columnar' Ignored x) ->
                    pure (Columnar' (CheckedFieldModification id id :: CheckedFieldModification tbl x)))
                 (undefined :: TableSkeleton tbl) (undefined :: TableSkeleton tbl)
