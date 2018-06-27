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

    , QAssignment, QField

    , QBaseScope

    , module Database.Beam.Query.Combinators
    , module Database.Beam.Query.Extensions

    , module Database.Beam.Query.Relationships

    , module Database.Beam.Query.CTE

    -- * Operators
    , module Database.Beam.Query.Operator

    -- ** ANSI SQL Booleans
    , Beam.SqlBool
    , isTrue_, isNotTrue_
    , isFalse_, isNotFalse_
    , isUnknown_, isNotUnknown_
    , unknownAs_, sqlBool_
    , possiblyNullBool_
    , fromPossiblyNullBool_

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

    , module Database.Beam.Query.DataTypes

    -- * SQL Command construction and execution
    -- ** @SELECT@
    , SqlSelect(..)
    , select, selectWith, lookup_
    , runSelectReturningList
    , runSelectReturningOne
    , dumpSqlSelect

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
    , runDelete ) where

import Prelude hiding (lookup)

import Database.Beam.Query.Aggregate
import Database.Beam.Query.Combinators
import Database.Beam.Query.CTE ( With, ReusableQ, selecting, reuse )
import qualified Database.Beam.Query.CTE as CTE
import Database.Beam.Query.CustomSQL
import Database.Beam.Query.DataTypes
import Database.Beam.Query.Extensions
import Database.Beam.Query.Internal
import Database.Beam.Query.Operator hiding (SqlBool)
import qualified Database.Beam.Query.Operator as Beam
import Database.Beam.Query.Ord
import Database.Beam.Query.Relationships
import Database.Beam.Query.Types (QGenExpr) -- hide QGenExpr constructor
import Database.Beam.Query.Types hiding (QGenExpr)

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.Builder
import Database.Beam.Schema.Tables

import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State.Strict

import Data.Text (Text)
import Data.Proxy

-- * Query

data QBaseScope

-- | A version of the table where each field is a 'QGenExpr'
type QGenExprTable ctxt be s tbl = tbl (QGenExpr ctxt be s)

type QExprTable

-- * SELECT

-- | Represents a select statement in the given backend, returning
-- rows of type 'a'.
newtype SqlSelect be a
    = SqlSelect (BeamSqlBackendSelectSyntax be)

-- | Build a 'SqlSelect' for the given 'Q'.
select :: forall be db res
        . ( BeamSqlBackend be, HasQBuilder be, Projectible be res )
       => Q be db QBaseScope res -> SqlSelect be (QExprToIdentity res)
select q =
  SqlSelect (buildSqlQuery "t" q)

-- | Create a 'SqlSelect' for a query which may have common table
-- expressions. See the documentation of 'With' for more details.
selectWith :: forall be db res
            . ( BeamSqlBackend be, BeamSql99CommonTableExpressionBackend be
              , HasQBuilder be, Projectible be res )
           => With be db (Q be db QBaseScope res) -> SqlSelect be (QExprToIdentity res)
selectWith (CTE.With mkQ) =
    let (q, (recursiveness, ctes)) = evalState (runWriterT mkQ) 0
    in case recursiveness of
         CTE.Nonrecursive -> SqlSelect (withSyntax ctes
                                                   (buildSqlQuery "t" q))
         CTE.Recursive    -> SqlSelect (withRecursiveSyntax ctes
                                                            (buildSqlQuery "t" q))

-- | Convenience function to generate a 'SqlSelect' that looks up a table row
--   given a primary key.
lookup_ :: ( Database be db, Table table

           , BeamSqlBackend be, HasQBuilder be
           , SqlValableTable (PrimaryKey table) be
           , HasTableEquality be (PrimaryKey table)
           )
        => DatabaseEntity be db (TableEntity table)
        -> PrimaryKey table Identity
        -> SqlSelect be (table Identity)
lookup_ tbl tblKey =
  select $
  filter_ (\t -> pk t ==. val_ tblKey) $
  all_ tbl

-- | Run a 'SqlSelect' in a 'MonadBeam' and get the results as a list
runSelectReturningList ::
  (MonadBeam be m, BeamSqlBackend be, FromBackendRow be a) =>
  SqlSelect be a -> m [ a ]
runSelectReturningList (SqlSelect s) =
  runReturningList (selectCmd s)

-- | Run a 'SqlSelect' in a 'MonadBeam' and get the unique result, if there is
--   one. Both no results as well as more than one result cause this to return
--   'Nothing'.
runSelectReturningOne ::
  (MonadBeam be m, BeamSqlBackend be, FromBackendRow be a) =>
  SqlSelect be a -> m (Maybe a)
runSelectReturningOne (SqlSelect s) =
  runReturningOne (selectCmd s)

-- | Use a special debug syntax to print out an ANSI Standard @SELECT@ statement
--   that may be generated for a given 'Q'.
dumpSqlSelect :: Projectible (MockSqlBackend SqlSyntaxBuilder) res
              => Q (MockSqlBackend SqlSyntaxBuilder) db QBaseScope res -> IO ()
dumpSqlSelect q =
    let SqlSelect s = select q
    in putStrLn (renderSql s)

-- * INSERT

-- | Represents a SQL @INSERT@ command that has not yet been run
data SqlInsert be
  = SqlInsert (BeamSqlBackendInsertSyntax be)
  | SqlInsertNoRows

-- | Generate a 'SqlInsert' over only certain fields of a table
insertOnly :: ( BeamSqlBackend be, ProjectibleWithPredicate AnyType () Text (QExprToField r) )
           => DatabaseEntity be db (TableEntity table)
              -- ^ Table to insert into
           -> (table (QField s) -> QExprToField r)
           -> SqlInsertValues be r
              -- ^ Values to insert. See 'insertValues', 'insertExpressions', 'insertData', and 'insertFrom' for possibilities.
           -> SqlInsert be
insertOnly _ _ SqlInsertValuesEmpty = SqlInsertNoRows
insertOnly (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkProj (SqlInsertValues vs) =
    SqlInsert (insertStmt tblNm proj vs)
  where
    tblFields = changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QField False tblNm name)) tblSettings
    proj = execWriter (project' (Proxy @AnyType) (Proxy @((), Text))
                                (\_ _ f -> tell [f] >> pure f)
                                (mkProj tblFields))

-- | Generate a 'SqlInsert' given a table and a source of values.
insert :: ( BeamSqlBackend be, ProjectibleWithPredicate AnyType () Text (table (QField s)) )
       => DatabaseEntity be db (TableEntity table)
          -- ^ Table to insert into
       -> SqlInsertValues be (table (QExpr be s))
          -- ^ Values to insert. See 'insertValues', 'insertExpressions', and 'insertFrom' for possibilities.
       -> SqlInsert be
insert tbl values = insertOnly tbl id values

-- | Run a 'SqlInsert' in a 'MonadBeam'
runInsert :: (BeamSqlBackend be, MonadBeam be m)
          => SqlInsert be -> m ()
runInsert SqlInsertNoRows = pure ()
runInsert (SqlInsert i) = runNoReturn (insertCmd i)

-- | Represents a source of values that can be inserted into a table shaped like
--   'tbl'.
data SqlInsertValues be proj --(tbl :: (* -> *) -> *)
    = SqlInsertValues (BeamSqlBackendInsertValuesSyntax be)
    | SqlInsertValuesEmpty

-- | Build a 'SqlInsertValues' from series of expressions in tables
insertExpressions :: forall be table s
                   . ( BeamSqlBackend be, Beamable table )
                  => (forall s'. [ table (QExpr be s') ])
                  -> SqlInsertValues be (table (QExpr be s))
insertExpressions tbls =
  case sqlExprs of
    [] -> SqlInsertValuesEmpty
    _  -> SqlInsertValues (insertSqlExpressions sqlExprs)
    where
      sqlExprs = map mkSqlExprs tbls

      mkSqlExprs :: forall s'. table (QExpr be s') -> [ BeamSqlBackendExpressionSyntax be ]
      mkSqlExprs = allBeamValues (\(Columnar' (QExpr x)) -> x "t")

-- | Build a 'SqlInsertValues' from concrete table values
insertValues :: forall be table s
              . ( BeamSqlBackend be, Beamable table
                , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) table )
             => [ table Identity ]
             -> SqlInsertValues be (table (QExpr be s))
insertValues x = insertExpressions (map val_ x :: forall s'. [table (QExpr be s') ])

-- | Build a 'SqlInsertValues' from arbitrarily shaped data containing expressions
insertData :: forall be r
            . ( Projectible be r, BeamSqlBackend be )
           => [ r ] -> SqlInsertValues be r
insertData rows =
  case rows of
    [] -> SqlInsertValuesEmpty
    _  -> SqlInsertValues (insertSqlExpressions (map (\row -> project (Proxy @be) row "t") rows))

-- | Build a 'SqlInsertValues' from a 'SqlSelect' that returns the same table
insertFrom :: ( BeamSqlBackend be, HasQBuilder be
              , Projectible be r )
           => Q be db QBaseScope r
           -> SqlInsertValues be r
insertFrom s = SqlInsertValues (insertFromSql (buildSqlQuery "t" s))

-- * UPDATE

-- | Represents a SQL @UPDATE@ statement for the given @table@.
data SqlUpdate be (table :: (* -> *) -> *)
  = SqlUpdate (BeamSqlBackendUpdateSyntax be)
  | SqlIdentityUpdate -- An update with no assignments

-- | Build a 'SqlUpdate' given a table, a list of assignments, and a way to
--   build a @WHERE@ clause.
--
--   See the '(<-.)' operator for ways to build assignments. The argument to the
--   second argument is a the table parameterized over 'QField', which
--   represents the left hand side of assignments. Sometimes, you'd like to also
--   get the current value of a particular column. You can use the 'current_'
--   function to convert a 'QField' to a 'QExpr'.
update :: ( BeamSqlBackend be, Beamable table )
       => DatabaseEntity be db (TableEntity table)
          -- ^ The table to insert into
       -> (forall s. table (QField s) -> [ QAssignment be s ])
          -- ^ A sequence of assignments to make.
       -> (forall s. table (QExpr be s) -> QExpr be s Bool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlUpdate be table
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
save :: forall table be db.
        ( Table table
        , BeamSqlBackend be

        , SqlValableTable (PrimaryKey table) be
        , SqlValableTable table be

        , HasTableEquality be (PrimaryKey table)
        )
     => DatabaseEntity be db (TableEntity table)
        -- ^ Table to update
     -> table Identity
        -- ^ Value to set to
     -> SqlUpdate be table
save tbl@(DatabaseEntity (DatabaseTable _ tblSettings)) v =
  update tbl (\(tblField :: table (QField s)) ->
                execWriter $
                zipBeamFieldsM
                  (\(Columnar' field) c@(Columnar' value) ->
                     do when (qFieldName field `notElem` primaryKeyFieldNames) $
                          tell [ field <-. value ]
                        pure c)
                  tblField (val_ v :: table (QExpr be s)))
             (\tblE -> primaryKey tblE ==. val_ (primaryKey v))

  where
    primaryKeyFieldNames =
      allBeamValues (\(Columnar' (TableField fieldNm)) -> fieldNm) (primaryKey tblSettings)

-- | Run a 'SqlUpdate' in a 'MonadBeam'.
runUpdate :: (BeamSqlBackend be, MonadBeam be m)
          => SqlUpdate be tbl -> m ()
runUpdate (SqlUpdate u) = runNoReturn (updateCmd u)
runUpdate SqlIdentityUpdate = pure ()

-- * DELETE

-- | Represents a SQL @DELETE@ statement for the given @table@
newtype SqlDelete be (table :: (* -> *) -> *) = SqlDelete (BeamSqlBackendDeleteSyntax be)

-- | Build a 'SqlDelete' from a table and a way to build a @WHERE@ clause
delete :: forall be db table
        . BeamSqlBackend be
       => DatabaseEntity be db (TableEntity table)
          -- ^ Table to delete from
       -> (forall s. (forall s'. table (QExpr be s')) -> QExpr be s Bool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlDelete be table
delete (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkWhere =
  SqlDelete (deleteStmt tblNm alias (Just (where_ "t")))
  where
    supportsAlias = deleteSupportsAlias (Proxy @(BeamSqlBackendDeleteSyntax be))

    tgtName = "delete_target"
    alias = if supportsAlias then Just tgtName else Nothing
    mkField = if supportsAlias then qualifiedField tgtName else unqualifiedField

    QExpr where_ = mkWhere (changeBeamRep (\(Columnar' (TableField name)) -> Columnar' (QExpr (pure (fieldE (mkField name))))) tblSettings)

-- | Run a 'SqlDelete' in a 'MonadBeam'
runDelete :: (BeamSqlBackend be, MonadBeam be m)
          => SqlDelete be table -> m ()
runDelete (SqlDelete d) = runNoReturn (deleteCmd d)
