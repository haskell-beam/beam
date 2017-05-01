{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Migrate.Types.CheckedEntities where

import Database.Beam
import Database.Beam.Schema.Tables

import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Migrate.Generics.Tables
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.Checks

import Control.Monad.Writer
import Control.Monad.Identity

import Data.Proxy
import Data.Text (Text)

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

instance IsCheckedDatabaseEntity be (TableEntity tbl) where
  data CheckedDatabaseEntityDescriptor be (TableEntity tbl) where
    CheckedDatabaseTable :: Table tbl => DatabaseEntityDescriptor be (TableEntity tbl)
                         -> [ TableCheck ] -> CheckedDatabaseEntityDescriptor be (TableEntity tbl)

  type CheckedDatabaseEntityDefaultRequirements be (TableEntity tbl) syntax =
    ( DatabaseEntityDefaultRequirements be (TableEntity tbl)
    , GMigratableTableSettings syntax (Rep (TableSettings tbl)) (Rep (tbl Identity))
    , IsSql92DdlCommandSyntax syntax )

  unCheck (CheckedDatabaseTable x _) = x
  collectEntityChecks (CheckedDatabaseTable (DatabaseTable tbl _) tblChecks) =
    map (\(TableCheck mkCheck) -> mkCheck tbl) tblChecks
  checkedDbEntityAuto syntax tblTypeName =
    let tbl'@(DatabaseTable _ tblSettings) = dbEntityAuto tblTypeName
        pk = allBeamValues (\(Columnar' (TableField x)) -> x) (primaryKey tblSettings)
    in CheckedDatabaseTable tbl' ( TableCheck (\tblName -> SomeDatabasePredicate (TableExistsPredicate tblName))
                                 : TableCheck (\tblName -> SomeDatabasePredicate (TableHasPrimaryKey tblName pk))
                                 : gDefaultTblSettingsChecks syntax (Proxy @(Rep (tbl Identity))) (from tblSettings) )

