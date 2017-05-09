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
    , select, lookup
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
    , update, save
    , runUpdate

    , SqlDelete(..)
    , delete
    , runDelete ) where

import Prelude hiding (lookup)

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
import Control.Monad.Writer

-- * Query

data QueryInaccessible

-- * SELECT

newtype SqlSelect select a
    = SqlSelect select

select :: forall syntax db res.
          ( ProjectibleInSelectSyntax syntax res
          , IsSql92SelectSyntax syntax
          , HasQBuilder syntax ) =>
          Q syntax db QueryInaccessible res -> SqlSelect syntax (QExprToIdentity res)
select q =
  SqlSelect (buildSqlQuery q)

lookup :: ( HasQBuilder syntax
          , Sql92SelectSanityCheck syntax

          , SqlValableTable (PrimaryKey table) (Sql92SelectExpressionSyntax syntax)
          , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) Bool

          , Beamable table, Table table

          , Database db )
       => DatabaseEntity be db (TableEntity table)
       -> PrimaryKey table Identity
       -> SqlSelect syntax (table Identity)
lookup tbl tblKey =
  select $
  filter_ (\t -> pk t ==. val_ tblKey) $
  all_ tbl

runSelectReturningList ::
  (IsSql92Syntax cmd, MonadBeam cmd be hdl m, FromBackendRow be a) =>
  SqlSelect (Sql92SelectSyntax cmd) a -> m [ a ]
runSelectReturningList (SqlSelect s) =
  runReturningList (selectCmd s)

runSelectReturningOne ::
  (IsSql92Syntax cmd, MonadBeam cmd be hdl m, FromBackendRow be a) =>
  SqlSelect (Sql92SelectSyntax cmd) a -> m (Maybe a)
runSelectReturningOne (SqlSelect s) =
  runReturningOne (selectCmd s)

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
insert (DatabaseEntity (DatabaseTable tblNm tblSettings)) (SqlInsertValues vs) =
    SqlInsert (insertStmt tblNm tblFields vs)
  where
    tblFields = allBeamValues (\(Columnar' f) -> _fieldName f) tblSettings

runInsert :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m)
          => SqlInsert (Sql92InsertSyntax cmd) -> m ()
runInsert (SqlInsert i) = runNoReturn (insertCmd i)

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
insertFrom (SqlSelect s) = SqlInsertValues (insertFromSql s)

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

save :: forall table syntax be db.
        ( Table table
        , IsSql92UpdateSyntax syntax

        , SqlValableTable (PrimaryKey table) (Sql92UpdateExpressionSyntax syntax)
        , SqlValableTable table (Sql92UpdateExpressionSyntax syntax)

        , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92UpdateExpressionSyntax syntax)) Bool
        )
     => DatabaseEntity be db (TableEntity table)
     -> table Identity -> SqlUpdate syntax table
save tbl@(DatabaseEntity (DatabaseTable _ tblSettings)) v =
  update tbl (\(tblField :: table (QField s)) ->
                execWriter $
                zipBeamFieldsM
                  (\(Columnar' field) c@(Columnar' value) ->
                     do when (qFieldName field `notElem` primaryKeyFieldNames) $
                          tell [ field <-. value ]
                        pure c)
                  tblField (val_ v :: table (QExpr (Sql92UpdateExpressionSyntax syntax) s)))
             (\tblE -> primaryKey tblE ==. val_ (primaryKey v))

  where
    primaryKeyFieldNames =
      allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm) (primaryKey tblSettings)

runUpdate :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m)
          => SqlUpdate (Sql92UpdateSyntax cmd) tbl -> m ()
runUpdate (SqlUpdate u) = runNoReturn (updateCmd u)

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
runDelete (SqlDelete d) = runNoReturn (deleteCmd d)
