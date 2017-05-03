{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( -- * Query type
      module Database.Beam.Query.Types

    -- * General query combinators
    , module Database.Beam.Query.CustomSQL
    , module Database.Beam.Query.Relationships
    , module Database.Beam.Query.Operator
    , module Database.Beam.Query.Combinators
    , module Database.Beam.Query.Aggregate

    , QAggregateContext, QGroupingContext, QValueContext
    , QWindowingContext, QWindowFrameContext

    , SqlEq(..), SqlOrd(..)
    , SqlEqQuantified(..), SqlOrdQuantified(..)
    , QQuantified
    , anyOf_, allOf_

    , SqlSelect(..)
    , select
    , runSelectReturningList
    , runSelectReturningOne
    , dumpSqlSelect

    , SqlInsert(..)
    , insert
    , runInsert

    , SqlInsertValues(..)
    , insertExpressions
    , insertValues
    , insertFrom

    , SqlUpdate(..)
    , update
    , runUpdate

    , SqlDelete(..)
    , delete
    , runDelete ) where

import Database.Beam.Query.Aggregate
import Database.Beam.Query.Combinators
import Database.Beam.Query.CustomSQL
import Database.Beam.Query.Internal
import Database.Beam.Query.Ord
import Database.Beam.Query.Relationships
import Database.Beam.Query.Operator
import Database.Beam.Query.Types

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.Builder
import Database.Beam.Schema.Tables

import Control.Monad.Identity

-- * Query

data QueryInaccessible

-- * SELECT

newtype SqlSelect select a
    = SqlSelect select

select :: forall q syntax db s res.
          ( ProjectibleInSelectSyntax syntax res
          , IsSql92SelectSyntax syntax
          , HasQBuilder syntax ) =>
          Q syntax db QueryInaccessible res -> SqlSelect syntax (QExprToIdentity res)
select q =
  SqlSelect (buildSqlQuery q)

runSelectReturningList ::
  (IsSql92Syntax cmd, MonadBeam cmd be hdl m, FromBackendRow be a) =>
  SqlSelect (Sql92SelectSyntax cmd) a -> m [ a ]
runSelectReturningList (SqlSelect select) =
  runReturningList (selectCmd select)

runSelectReturningOne ::
  (IsSql92Syntax cmd, MonadBeam cmd be hdl m, FromBackendRow be a) =>
  SqlSelect (Sql92SelectSyntax cmd) a -> m (Maybe a)
runSelectReturningOne (SqlSelect select) =
  runReturningOne (selectCmd select)

dumpSqlSelect :: ProjectibleInSelectSyntax SqlSyntaxBuilder res =>
                 Q SqlSyntaxBuilder db QueryInaccessible res -> IO ()
dumpSqlSelect q =
    let SqlSelect s = select q
    in putStrLn (renderSql s)

-- * INSERT

newtype SqlInsert syntax = SqlInsert syntax

insert :: IsSql92InsertSyntax syntax =>
          DatabaseEntity be db (TableEntity table)
       -> SqlInsertValues (Sql92InsertValuesSyntax syntax) table
       -> SqlInsert syntax
insert (DatabaseEntity (DatabaseTable tblNm tblSettings)) (SqlInsertValues insertValues) =
    SqlInsert (insertStmt tblNm tblFields insertValues)
  where
    tblFields = allBeamValues (\(Columnar' f) -> _fieldName f) tblSettings

runInsert :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m)
          => SqlInsert (Sql92InsertSyntax cmd) -> m ()
runInsert (SqlInsert insert) = runNoReturn (insertCmd insert)

newtype SqlInsertValues insertValues (tbl :: (* -> *) -> *)
    = SqlInsertValues insertValues

insertExpressions ::
    forall syntax table.
    ( Beamable table
    , IsSql92InsertValuesSyntax syntax ) =>
    (forall s. [ table (QExpr (Sql92InsertValuesExpressionSyntax syntax) s) ]) ->
    SqlInsertValues syntax table
insertExpressions tbls =
    SqlInsertValues $
    insertSqlExpressions (map mkSqlExprs tbls)
    where
      mkSqlExprs :: forall s. table (QExpr (Sql92InsertValuesExpressionSyntax syntax) s) -> [Sql92InsertValuesExpressionSyntax syntax]
      mkSqlExprs = allBeamValues (\(Columnar' (QExpr x)) -> x)

insertValues ::
    forall table syntax.
    ( Beamable table
    , IsSql92InsertValuesSyntax syntax
    , FieldsFulfillConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax syntax))) table) =>
    [ table Identity ] -> SqlInsertValues syntax table
insertValues x = insertExpressions (map val_ x :: forall s. [table (QExpr (Sql92InsertValuesExpressionSyntax syntax) s) ])

insertFrom ::
    IsSql92InsertValuesSyntax syntax =>
    SqlSelect (Sql92InsertValuesSelectSyntax syntax) (table Identity) -> SqlInsertValues syntax table
insertFrom (SqlSelect select) = SqlInsertValues . insertFromSql $ select

-- * UPDATE

newtype SqlUpdate syntax (table :: (* -> *) -> *) = SqlUpdate syntax

update :: ( Beamable table
          , IsSql92UpdateSyntax syntax) =>
          DatabaseEntity be db (TableEntity table)
       -> (forall s. table (QField s) -> [ QAssignment (Sql92UpdateFieldNameSyntax syntax) (Sql92UpdateExpressionSyntax syntax) s ])
       -> (forall s. table (QExpr (Sql92UpdateExpressionSyntax syntax) s) -> QExpr (Sql92UpdateExpressionSyntax syntax) s Bool)
       -> SqlUpdate syntax table
update (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkAssignments mkWhere =
  SqlUpdate (updateStmt tblNm assignments (Just where_))
  where
    assignments = concatMap (\(QAssignment as) -> as) (mkAssignments tblFields)
    QExpr where_ = mkWhere tblFieldExprs

    tblFields = changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QField tblNm name)) tblSettings
    tblFieldExprs = changeBeamRep (\(Columnar' (QField _ nm)) -> Columnar' (QExpr (fieldE (unqualifiedField nm)))) tblFields

runUpdate :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m)
          => SqlUpdate (Sql92UpdateSyntax cmd) tbl -> m ()
runUpdate (SqlUpdate update) = runNoReturn (updateCmd update)

-- * DELETE

newtype SqlDelete syntax (table :: (* -> *) -> *) = SqlDelete syntax

delete :: IsSql92DeleteSyntax delete
       => DatabaseEntity be db (TableEntity table)
       -> (forall s. table (QExpr (Sql92DeleteExpressionSyntax delete) s) -> QExpr (Sql92DeleteExpressionSyntax delete) s Bool)
       -> SqlDelete delete table
delete (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkWhere =
  SqlDelete (deleteStmt tblNm (Just where_))
  where
    QExpr where_ = mkWhere (changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QExpr (fieldE (unqualifiedField name)))) tblSettings)

runDelete :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m)
          => SqlDelete (Sql92DeleteSyntax cmd) table -> m ()
runDelete (SqlDelete delete) = runNoReturn (deleteCmd delete)
