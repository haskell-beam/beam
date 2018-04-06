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

    , QGenExprTable, QExprTable

    , module Database.Beam.Query.Combinators
    , module Database.Beam.Query.Extensions

    , module Database.Beam.Query.Relationships

    -- * Operators
    , module Database.Beam.Query.Operator

    -- ** ANSI SQL Booleans
    , Beam.SqlBool
    , isTrue_, isNotTrue_
    , isFalse_, isNotFalse_
    , isUnknown_, isNotUnknown_
    , unknownAs_, sqlBool_

    -- ** Unquantified comparison operators
    , HasSqlEqualityCheck(..), HasSqlQuantifiedEqualityCheck(..)
    , SqlEq(..), SqlOrd(..)

    -- ** Quantified Comparison Operators #quantified-comparison-operator#
    , SqlEqQuantified(..), SqlOrdQuantified(..)
    , QQuantified
    , anyOf_, allOf_, anyIn_, allIn_
    , between_
    , in_

    , module Database.Beam.Query.Aggregate

    , module Database.Beam.Query.CustomSQL

    -- * SQL Command construction and execution
    -- ** @SELECT@
    , SqlSelect(..)
    , select, lookup_
    , runSelectReturningList
    , runSelectReturningOne
    -- , dumpSqlSelect

    -- ** @INSERT@
    , SqlInsert(..)
    , insert, insertOnly
    , runInsert

    , SqlInsertValues(..)
    , insertExpressions
    , insertValues
    , insertFrom
    , insertData

    -- ** @UPDATE@
    , SqlUpdate(..)
    , update, save
    , runUpdate

    -- ** @DELETE@
    , SqlDelete(..)
    , delete
    , runDelete
    ) where

import Prelude hiding (lookup)

import Database.Beam.Syntax
import Database.Beam.Query.Aggregate
import Database.Beam.Query.Combinators
import Database.Beam.Query.CustomSQL
import Database.Beam.Query.Extensions
import Database.Beam.Query.Internal
import Database.Beam.Query.Operator hiding (SqlBool)
import qualified Database.Beam.Query.Operator as Beam
import Database.Beam.Query.Ord
import Database.Beam.Query.Relationships
import Database.Beam.Query.Types -- (QGenExpr) -- hide QGenExpr constructor
import Database.Beam.Query.Types hiding (QGenExpr)

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Schema.Tables

import Control.Monad.Identity
import Control.Monad.Writer

import Data.Text (Text)
import Data.Proxy

-- * Query

data QueryInaccessible

-- | A version of the table where each field is a 'QGenExpr'
type QGenExprTable ctxt s tbl = tbl (QGenExpr ctxt s)

type QExprTable s tbl = QGenExprTable QValueContext s tbl

-- * SELECT

-- | Represents a select statement over the syntax 'select' that will return
--   rows of type 'a'.
newtype SqlSelect a
    = SqlSelect SelectSyntax

-- | Build a 'SqlSelect' for the given 'Q'.
select :: Projectible ExpressionSyntax res => Q db QueryInaccessible res -> SqlSelect (QExprToIdentity res)
select q =
  SqlSelect (buildSqlQuery "t" q)

-- | Convenience function to generate a 'SqlSelect' that looks up a table row
--   given a primary key.
lookup_ :: ( SqlValableTable (PrimaryKey table)
           , HasTableEquality (PrimaryKey table)
           , Beamable table, Table table 
           , Database db )
        => DatabaseEntity db (TableEntity table)
        -> PrimaryKey table Identity
        -> SqlSelect (table Identity)
lookup_ tbl tblKey =
  select $
  filter_ (\t -> pk t ==. val_ tblKey) $
  all_ tbl

-- | Run a 'SqlSelect' in a 'MonadBeam' and get the results as a list
runSelectReturningList ::
  (MonadBeam hdl m, FromBackendRow a) =>
  SqlSelect a -> m [ a ]
runSelectReturningList (SqlSelect s) =
  runReturningList (selectCmd s)

-- | Run a 'SqlSelect' in a 'MonadBeam' and get the unique result, if there is
--   one. Both no results as well as more than one result cause this to return
--   'Nothing'.
runSelectReturningOne ::
  (MonadBeam hdl m, FromBackendRow a) =>
  SqlSelect a -> m (Maybe a)
runSelectReturningOne (SqlSelect s) =
  runReturningOne (selectCmd s)

-- -- | Use a special debug syntax to print out an ANSI Standard @SELECT@ statement
-- --   that may be generated for a given 'Q'.
-- dumpSqlSelect :: ProjectibleInSelectSyntax SqlSyntaxBuilder res =>
--                  Q SqlSyntaxBuilder db QueryInaccessible res -> IO ()
-- dumpSqlSelect q =
--     let SqlSelect s = select q
--     in putStrLn (renderSql s)

-- * INSERT

-- | Represents a SQL @INSERT@ command that has not yet been run
data SqlInsert
  = SqlInsert InsertSyntax
  | SqlInsertNoRows

-- | Generate a 'SqlInsert' over only certain fields of a table
insertOnly :: ( Projectible Text (QExprToField r) )
           => DatabaseEntity db (TableEntity table)
              -- ^ Table to insert into
           -> (table (QField s) -> QExprToField r)
           -> SqlInsertValues r
              -- ^ Values to insert. See 'insertValues', 'insertExpressions', 'insertData', and 'insertFrom' for possibilities.
           -> SqlInsert
insertOnly _ _ SqlInsertValuesEmpty = SqlInsertNoRows
insertOnly (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkProj (SqlInsertValues vs) =
    SqlInsert (insertStmt tblNm proj vs)
  where
    tblFields = changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QField False tblNm name)) tblSettings
    proj = execWriter (project' (Proxy @AnyType) (\_ f -> tell [f ""] >> pure f)
                                (mkProj tblFields))

-- | Generate a 'SqlInsert' given a table and a source of values.
insert :: ( Projectible Text (table (QField s)) )
       => DatabaseEntity db (TableEntity table)
          -- ^ Table to insert into
       -> SqlInsertValues (table (QExpr s))
          -- ^ Values to insert. See 'insertValues', 'insertExpressions', and 'insertFrom' for possibilities.
       -> SqlInsert
insert tbl values = insertOnly tbl id values

-- | Run a 'SqlInsert' in a 'MonadBeam'
runInsert :: MonadBeam hdl m
          => SqlInsert -> m ()
runInsert SqlInsertNoRows = pure ()
runInsert (SqlInsert i) = runNoReturn (insertCmd i)

-- | Represents a source of values that can be inserted into a table shaped like
--   'tbl'.
data SqlInsertValues proj --(tbl :: (* -> *) -> *)
    = SqlInsertValues InsertValuesSyntax
    | SqlInsertValuesEmpty

-- | Build a 'SqlInsertValues' from series of expressions in tables
insertExpressions ::
    forall table s.
    ( Beamable table ) =>
    (forall s'. [ table (QExpr s') ]) ->
    SqlInsertValues (table (QExpr s))
insertExpressions tbls =
  case sqlExprs of
    [] -> SqlInsertValuesEmpty
    _  -> SqlInsertValues (insertSqlExpressions sqlExprs)
    where
      sqlExprs = map mkSqlExprs tbls

      mkSqlExprs :: forall s'. table (QExpr s') -> [ExpressionSyntax]
      mkSqlExprs = allBeamValues (\(Columnar' (QExpr x)) -> x "t")

-- | Build a 'SqlInsertValues' from concrete table values
insertValues ::
    forall table s.
    ( Beamable table
    , FieldsFulfillConstraint HasSqlValueSyntax table) =>
    [ table Identity ] -> SqlInsertValues (table (QExpr s))
insertValues x = insertExpressions (map val_ x :: forall s'. [table (QExpr s') ])

-- | Build a 'SqlInsertValues' from arbitrarily shaped data containing expressions
insertData :: forall r
            . Projectible ExpressionSyntax r
           => [ r ] -> SqlInsertValues r
insertData rows =
  case rows of
    [] -> SqlInsertValuesEmpty
    _  -> SqlInsertValues (insertSqlExpressions (map mkSqlExprs rows))
  where
    mkSqlExprs :: r -> [ExpressionSyntax]
    mkSqlExprs r = execWriter (project' (Proxy @AnyType) (\_ s -> tell [ s "t" ] >> pure s) r)

-- | Build a 'SqlInsertValues' from a 'SqlSelect' that returns the same table
insertFrom
    :: Projectible ExpressionSyntax r
    => Q db QueryInaccessible r
    -> SqlInsertValues r
insertFrom s = SqlInsertValues (insertFromSql (buildSqlQuery "t" s))

-- -- * UPDATE

-- | Represents a SQL @UPDATE@ statement for the given @table@.
data SqlUpdate (table :: (* -> *) -> *)
  = SqlUpdate UpdateSyntax
  | SqlIdentityUpdate -- An update with no assignments

-- | Build a 'SqlUpdate' given a table, a list of assignments, and a way to
--   build a @WHERE@ clause.
--
--   See the '(<-.)' operator for ways to build assignments. The argument to the
--   second argument is a the table parameterized over 'QField', which
--   represents the left hand side of assignments. Sometimes, you'd like to also
--   get the current value of a particular column. You can use the 'current_'
--   function to convert a 'QField' to a 'QExpr'.
update :: ( Beamable table ) =>
          DatabaseEntity db (TableEntity table)
          -- ^ The table to insert into
       -> (forall s. table (QField s) -> [ QAssignment s ])
          -- ^ A sequence of assignments to make.
       -> (forall s. table (QExpr s) -> QExpr s Bool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlUpdate table
update (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkAssignments mkWhere =
  case assignments of
    [] -> SqlIdentityUpdate
    _  -> SqlUpdate (updateStmt tblNm assignments (Just (where_ "t")))
  where
    assignments = concatMap (\(QAssignment as) -> as) (mkAssignments tblFields)
    QExpr where_ = mkWhere tblFieldExprs

    tblFields = changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QField False tblNm name)) tblSettings
    tblFieldExprs = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (pure (fieldE (unqualifiedField nm))))) tblFields

-- | Generate a 'SqlUpdate' that will update the given table with the given value.
--
--   The SQL @UPDATE@ that is generated will set every non-primary key field for
--   the row where each primary key field is exactly what is given.
--
--   Note: This is a pure SQL @UPDATE@ command. This does not upsert or merge values.
save :: forall table db.
        ( Table table
        , SqlValableTable (PrimaryKey table)
        , SqlValableTable table

        , HasTableEquality (PrimaryKey table)

        , HasSqlValueSyntax Bool
        )
     => DatabaseEntity db (TableEntity table)
        -- ^ Table to update
     -> table Identity
        -- ^ Value to set to
     -> SqlUpdate table
save tbl@(DatabaseEntity (DatabaseTable _ tblSettings)) v =
  update tbl (\(tblField :: table (QField s)) ->
                execWriter $
                zipBeamFieldsM
                  (\(Columnar' field) c@(Columnar' value) ->
                     do when (qFieldName field `notElem` primaryKeyFieldNames) $
                          tell [ field <-. value ]
                        pure c)
                  tblField (val_ v :: table (QExpr s)))
             (\tblE -> primaryKey tblE ==. val_ (primaryKey v))

  where
    primaryKeyFieldNames =
      allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm) (primaryKey tblSettings)

-- | Run a 'SqlUpdate' in a 'MonadBeam'.
runUpdate :: (MonadBeam hdl m)
          => SqlUpdate tbl -> m ()
runUpdate (SqlUpdate u) = runNoReturn (updateCmd u)
runUpdate SqlIdentityUpdate = pure ()

-- * DELETE

-- | Represents a SQL @DELETE@ statement for the given @table@
newtype SqlDelete (table :: (* -> *) -> *) = SqlDelete DeleteSyntax

-- | Build a 'SqlDelete' from a table and a way to build a @WHERE@ clause
delete :: DatabaseEntity db (TableEntity table)
          -- ^ Table to delete from
       -> (forall s. table (QExpr s) -> QExpr s Bool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlDelete table
delete (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkWhere =
  SqlDelete (deleteStmt tblNm (Just (where_ "t")))
  where
    QExpr where_ = mkWhere (changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QExpr (pure (fieldE (unqualifiedField name))))) tblSettings)

-- | Run a 'SqlDelete' in a 'MonadBeam'
runDelete :: (MonadBeam hdl m) => SqlDelete table -> m ()
runDelete (SqlDelete d) = runNoReturn (deleteCmd d)
