{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query
    ( -- * Query type
      module Database.Beam.Query.Types

    -- ** Query expression contexts
    -- | A context is a type-level value that signifies where an expression can
    --   be used. For example, 'QExpr' corresponds to 'QGenExpr's that result in
    --   values. In reality, 'QExpr' is really 'QGenExpr' parameterized over the
    --   'QValueContext'. Similarly, 'QAgg' represents expressions that contain
    --   aggregates, but it is just 'QGenExpr' parameterized over
    --   'QAggregateContext'
    , QAggregateContext, QGroupingContext, QValueContext
    , QWindowingContext, QWindowFrameContext

    , module Database.Beam.Query.Combinators

    , module Database.Beam.Query.Relationships

    -- * Operators
    , module Database.Beam.Query.Operator

    -- ** Unquantified comparison operators
    , SqlEq(..), SqlOrd(..)

    -- ** Quantified Comparison Operators #quantified-comparison-operator#
    , SqlEqQuantified(..), SqlOrdQuantified(..)
    , QQuantified
    , anyOf_, allOf_

    , module Database.Beam.Query.Aggregate

    , module Database.Beam.Query.CustomSQL

    -- * SQL Command construction and execution
    -- ** @SELECT@
    , SqlSelect(..)
    , select, lookup
    , runSelectReturningList
    , runSelectReturningOne
    , dumpSqlSelect

    -- ** @INSERT@
    , SqlInsert(..)
    , insert
    , runInsert

    , SqlInsertValues(..)
    , insertExpressions
    , insertValues
    , insertFrom

    -- ** @UPDATE@
    , SqlUpdate(..)
    , update, save
    , runUpdate

    -- ** @DELETE@
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

-- | Represents a select statement over the syntax 'select' that will return
--   rows of type 'a'.
newtype SqlSelect select a
    = SqlSelect select

-- | Build a 'SqlSelect' for the given 'Q'.
select :: forall syntax db res.
          ( ProjectibleInSelectSyntax syntax res
          , IsSql92SelectSyntax syntax
          , HasQBuilder syntax ) =>
          Q syntax db QueryInaccessible res -> SqlSelect syntax (QExprToIdentity res)
select q =
  SqlSelect (buildSqlQuery q)

-- | Convenience function to generate a 'SqlSelect' that looks up a table row
--   given a primary key.
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

-- | Run a 'SqlSelect' in a 'MonadBeam' and get the results as a list
runSelectReturningList ::
  (IsSql92Syntax cmd, MonadBeam cmd be hdl m, FromBackendRow be a) =>
  SqlSelect (Sql92SelectSyntax cmd) a -> m [ a ]
runSelectReturningList (SqlSelect s) =
  runReturningList (selectCmd s)

-- | Run a 'SqlSelect' in a 'MonadBeam' and get the unique result, if there is
--   one. Both no results as well as more than one result cause this to return
--   'Nothing'.
runSelectReturningOne ::
  (IsSql92Syntax cmd, MonadBeam cmd be hdl m, FromBackendRow be a) =>
  SqlSelect (Sql92SelectSyntax cmd) a -> m (Maybe a)
runSelectReturningOne (SqlSelect s) =
  runReturningOne (selectCmd s)

-- | Use a special debug syntax to print out an ANSI Standard @SELECT@ statement
--   that may be generated for a given 'Q'.
dumpSqlSelect :: ProjectibleInSelectSyntax SqlSyntaxBuilder res =>
                 Q SqlSyntaxBuilder db QueryInaccessible res -> IO ()
dumpSqlSelect q =
    let SqlSelect s = select q
    in putStrLn (renderSql s)

-- * INSERT

-- | Represents a SQL @INSERT@ command that has not yet been run
newtype SqlInsert syntax = SqlInsert syntax

-- | Generate a 'SqlInsert' given a table and a source of values.
insert :: IsSql92InsertSyntax syntax =>
          DatabaseEntity be db (TableEntity table)
          -- ^ Table to insert into
       -> SqlInsertValues (Sql92InsertValuesSyntax syntax) table
          -- ^ Values to insert. See 'insertValues', 'insertExpressions', and 'insertFrom' for possibilities.
       -> SqlInsert syntax
insert (DatabaseEntity (DatabaseTable tblNm tblSettings)) (SqlInsertValues vs) =
    SqlInsert (insertStmt tblNm tblFields vs)
  where
    tblFields = allBeamValues (\(Columnar' f) -> _fieldName f) tblSettings

-- | Run a 'SqlInsert' in a 'MonadBeam'
runInsert :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m)
          => SqlInsert (Sql92InsertSyntax cmd) -> m ()
runInsert (SqlInsert i) = runNoReturn (insertCmd i)

-- | Represents a source of values that can be inserted into a table shaped like
--   'tbl'.
newtype SqlInsertValues insertValues (tbl :: (* -> *) -> *)
    = SqlInsertValues insertValues

-- | Build a 'SqlInsertValues' from series of expressions
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

-- | Build a 'SqlInsertValues' from concrete table values
insertValues ::
    forall table syntax.
    ( Beamable table
    , IsSql92InsertValuesSyntax syntax
    , FieldsFulfillConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax syntax))) table) =>
    [ table Identity ] -> SqlInsertValues syntax table
insertValues x = insertExpressions (map val_ x :: forall s. [table (QExpr (Sql92InsertValuesExpressionSyntax syntax) s) ])

-- | Build a 'SqlInsertValues' from a 'SqlSelect' that returns the samae table
insertFrom ::
    IsSql92InsertValuesSyntax syntax =>
    SqlSelect (Sql92InsertValuesSelectSyntax syntax) (table Identity) -> SqlInsertValues syntax table
insertFrom (SqlSelect s) = SqlInsertValues (insertFromSql s)

-- * UPDATE

-- | Represents a SQL @UPDATE@ statement for the given @table@.
newtype SqlUpdate syntax (table :: (* -> *) -> *) = SqlUpdate syntax

-- | Build a 'SqlUpdate' given a table, a list of assignments, and a way to
--   build a @WHERE@ clause.
--
--   See the '(<-.)' operator for ways to build assignments. The argument to the
--   second argument is a the table parameterized over 'QField', which
--   represents the left hand side of assignments. Sometimes, you'd like to also
--   get the current value of a particular column. You can use the 'current_'
--   function to convert a 'QField' to a 'QExpr'.
update :: ( Beamable table
          , IsSql92UpdateSyntax syntax) =>
          DatabaseEntity be db (TableEntity table)
          -- ^ The table to insert into
       -> (forall s. table (QField s) -> [ QAssignment (Sql92UpdateFieldNameSyntax syntax) (Sql92UpdateExpressionSyntax syntax) s ])
          -- ^ A sequence of assignments to make.
       -> (forall s. table (QExpr (Sql92UpdateExpressionSyntax syntax) s) -> QExpr (Sql92UpdateExpressionSyntax syntax) s Bool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlUpdate syntax table
update (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkAssignments mkWhere =
  SqlUpdate (updateStmt tblNm assignments (Just where_))
  where
    assignments = concatMap (\(QAssignment as) -> as) (mkAssignments tblFields)
    QExpr where_ = mkWhere tblFieldExprs

    tblFields = changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QField tblNm name)) tblSettings
    tblFieldExprs = changeBeamRep (\(Columnar' (QField _ nm)) -> Columnar' (QExpr (fieldE (unqualifiedField nm)))) tblFields

-- | Generate a 'SqlUpdate' that will update the given table with the given value.
--
--   The SQL @UPDATE@ that is generated will set every non-primary key field for
--   the row where each primary key field is exactly what is given.
--
--   Note: This is a pure SQL @UPDATE@ command. This does not upsert or merge values.
save :: forall table syntax be db.
        ( Table table
        , IsSql92UpdateSyntax syntax

        , SqlValableTable (PrimaryKey table) (Sql92UpdateExpressionSyntax syntax)
        , SqlValableTable table (Sql92UpdateExpressionSyntax syntax)

        , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92UpdateExpressionSyntax syntax)) Bool
        )
     => DatabaseEntity be db (TableEntity table)
        -- ^ Table to update
     -> table Identity
        -- ^ Value to set to
     -> SqlUpdate syntax table
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

-- | Run a 'SqlUpdate' in a 'MonadBeam'.
runUpdate :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m)
          => SqlUpdate (Sql92UpdateSyntax cmd) tbl -> m ()
runUpdate (SqlUpdate u) = runNoReturn (updateCmd u)

-- * DELETE

-- | Represents a SQL @DELETE@ statement for the given @table@
newtype SqlDelete syntax (table :: (* -> *) -> *) = SqlDelete syntax

-- | Build a 'SqlDelete' from a table and a way to build a @WHERE@ clause
delete :: IsSql92DeleteSyntax delete
       => DatabaseEntity be db (TableEntity table)
          -- ^ Table to delete from
       -> (forall s. table (QExpr (Sql92DeleteExpressionSyntax delete) s) -> QExpr (Sql92DeleteExpressionSyntax delete) s Bool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlDelete delete table
delete (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkWhere =
  SqlDelete (deleteStmt tblNm (Just where_))
  where
    QExpr where_ = mkWhere (changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QExpr (fieldE (unqualifiedField name)))) tblSettings)

-- | Run a 'SqlDelete' in a 'MonadBeam'
runDelete :: (IsSql92Syntax cmd, MonadBeam cmd be hdl m)
          => SqlDelete (Sql92DeleteSyntax cmd) table -> m ()
runDelete (SqlDelete d) = runNoReturn (deleteCmd d)
