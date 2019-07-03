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

    , QAssignment, QField, QFieldAssignment

    , QBaseScope

    , module Database.Beam.Query.Combinators
    , module Database.Beam.Query.Extensions

    , module Database.Beam.Query.Relationships

    , module Database.Beam.Query.CTE

    , module Database.Beam.Query.Extract

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
    , SqlEq(..), SqlOrd(..), SqlIn(..)
    , HasSqlInTable

    -- ** Quantified Comparison Operators #quantified-comparison-operator#
    , SqlEqQuantified(..), SqlOrdQuantified(..)
    , QQuantified
    , anyOf_, allOf_, anyIn_, allIn_
    , between_

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
    , updateTable, set, setFieldsTo
    , toNewValue, toOldValue, toUpdatedValue
    , toUpdatedValueMaybe
    , updateRow, updateTableRow
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
import Database.Beam.Query.Extract
import Database.Beam.Query.Internal
import Database.Beam.Query.Operator hiding (SqlBool)
import qualified Database.Beam.Query.Operator as Beam
import Database.Beam.Query.Ord
import Database.Beam.Query.Relationships
import Database.Beam.Query.Types (QGenExpr) -- hide QGenExpr constructor
import Database.Beam.Query.Types hiding (QGenExpr)

import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.Builder
import Database.Beam.Schema.Tables

import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State.Strict

import Data.Functor.Const (Const(..))
import Data.Text (Text)
import Data.Proxy

import Lens.Micro ((^.))

-- * Query

data QBaseScope

-- | A version of the table where each field is a 'QGenExpr'
type QGenExprTable ctxt be s tbl = tbl (QGenExpr ctxt be s)

type QExprTable be s tbl = QGenExprTable QValueContext be s tbl

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
           , SqlValableTable be (PrimaryKey table)
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
data SqlInsert be (table :: (* -> *) -> *)
  = SqlInsert !(TableSettings table) !(BeamSqlBackendInsertSyntax be)
  | SqlInsertNoRows

-- | Generate a 'SqlInsert' over only certain fields of a table
insertOnly :: ( BeamSqlBackend be, ProjectibleWithPredicate AnyType () Text (QExprToField r) )
           => DatabaseEntity be db (TableEntity table)
              -- ^ Table to insert into
           -> (table (QField s) -> QExprToField r)
           -> SqlInsertValues be r
              -- ^ Values to insert. See 'insertValues', 'insertExpressions', 'insertData', and 'insertFrom' for possibilities.
           -> SqlInsert be table
insertOnly _ _ SqlInsertValuesEmpty = SqlInsertNoRows
insertOnly (DatabaseEntity dt@(DatabaseTable {})) mkProj (SqlInsertValues vs) =
    SqlInsert (dbTableSettings dt) (insertStmt (tableNameFromEntity dt) proj vs)
  where
    tblFields = changeBeamRep (\(Columnar' fd) -> Columnar' (QField False (dbTableCurrentName dt) (fd ^. fieldName)))
                              (dbTableSettings dt)
    proj = execWriter (project' (Proxy @AnyType) (Proxy @((), Text))
                                (\_ _ f -> tell [f] >> pure f)
                                (mkProj tblFields))

-- | Generate a 'SqlInsert' given a table and a source of values.
insert :: ( BeamSqlBackend be, ProjectibleWithPredicate AnyType () Text (table (QField s)) )
       => DatabaseEntity be db (TableEntity table)
          -- ^ Table to insert into
       -> SqlInsertValues be (table (QExpr be s))
          -- ^ Values to insert. See 'insertValues', 'insertExpressions', and 'insertFrom' for possibilities.
       -> SqlInsert be table
insert tbl values = insertOnly tbl id values

-- | Run a 'SqlInsert' in a 'MonadBeam'
runInsert :: (BeamSqlBackend be, MonadBeam be m)
          => SqlInsert be table -> m ()
runInsert SqlInsertNoRows = pure ()
runInsert (SqlInsert _ i) = runNoReturn (insertCmd i)

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
  = SqlUpdate !(TableSettings table) !(BeamSqlBackendUpdateSyntax be)
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
       -> (forall s. table (QField s) -> QAssignment be s)
          -- ^ A sequence of assignments to make.
       -> (forall s. table (QExpr be s) -> QExpr be s Bool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlUpdate be table
update (DatabaseEntity dt@(DatabaseTable {})) mkAssignments mkWhere =
  case assignments of
    [] -> SqlIdentityUpdate
    _  -> SqlUpdate (dbTableSettings dt)
                    (updateStmt (tableNameFromEntity dt)
                       assignments (Just (where_ "t")))
  where
    QAssignment assignments = mkAssignments tblFields
    QExpr where_ = mkWhere tblFieldExprs

    tblFields = changeBeamRep (\(Columnar' fd) -> Columnar' (QField False (dbTableCurrentName dt) (fd ^. fieldName)))
                              (dbTableSettings dt)
    tblFieldExprs = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (pure (fieldE (unqualifiedField nm))))) tblFields

-- | A specialization of 'update' that matches the given (already existing) row
updateRow :: ( BeamSqlBackend be, Table table
             , HasTableEquality be (PrimaryKey table)
             , SqlValableTable be (PrimaryKey table) )
          => DatabaseEntity be db (TableEntity table)
             -- ^ The table to insert into
          -> table Identity
             -- ^ The row to update
          -> (forall s. table (QField s) -> QAssignment be s)
             -- ^ A sequence of assignments to make.
          -> SqlUpdate be table
updateRow tbl row update' =
  update tbl update' (references_ (val_ (pk row)))

-- | A specialization of 'update' that is more convenient for normal tables.
updateTable :: forall table db be
             . ( BeamSqlBackend be, Beamable table )
            => DatabaseEntity be db (TableEntity table)
               -- ^ The table to update
            -> table (QFieldAssignment be table)
               -- ^ Updates to be made (use 'set' to construct an empty field)
            -> (forall s. table (QExpr be s) -> QExpr be s Bool)
            -> SqlUpdate be table
updateTable tblEntity assignments mkWhere =
  let mkAssignments :: forall s. table (QField s) -> QAssignment be s
      mkAssignments tblFields =
        let tblExprs = changeBeamRep (\(Columnar' fd) -> Columnar' (current_ fd)) tblFields
        in execWriter $
           zipBeamFieldsM
             (\(Columnar' field :: Columnar' (QField s) a)
               c@(Columnar' (QFieldAssignment mkAssignment)) ->
                case mkAssignment tblExprs of
                  Nothing -> pure c
                  Just newValue -> do
                    tell (field <-. newValue)
                    pure c)
             tblFields assignments

  in update tblEntity mkAssignments mkWhere

-- | Convenience form of 'updateTable' that generates a @WHERE@ clause
-- that matches only the already existing entity
updateTableRow :: ( BeamSqlBackend be, Table table
                  , HasTableEquality be (PrimaryKey table)
                  , SqlValableTable be (PrimaryKey table) )
               => DatabaseEntity be db (TableEntity table)
                  -- ^ The table to update
               -> table Identity
                  -- ^ The row to update
               -> table (QFieldAssignment be table)
                  -- ^ Updates to be made (use 'set' to construct an empty field)
               -> SqlUpdate be table
updateTableRow tbl row update' =
  updateTable tbl update' (references_ (val_ (pk row)))

set :: forall table be table'. Beamable table => table (QFieldAssignment be table')
set = changeBeamRep (\_ -> Columnar' (QFieldAssignment (\_ -> Nothing))) (tblSkeleton :: TableSkeleton table)

setFieldsTo :: forall table be table'
             . Table table => (forall s. table (QExpr be s)) -> table (QFieldAssignment be table')
setFieldsTo tbl =

  runIdentity $
  zipBeamFieldsM (\(Columnar' (Const columnIx))
                   (Columnar' (QExpr newValue)) ->
                    if columnIx `elem` primaryKeyIndices
                    then pure $ Columnar' toOldValue
                    else pure $ Columnar' (toNewValue (QExpr newValue)))
                 indexedTable tbl

  where
    indexedTable :: table (Const Int)
    indexedTable =
      flip evalState 0 $
      zipBeamFieldsM (\_ _ -> do
                         n <- get
                         put (n + 1)
                         return (Columnar' (Const n)))
        (tblSkeleton :: TableSkeleton table) (tblSkeleton :: TableSkeleton table)

    primaryKeyIndices :: [ Int ]
    primaryKeyIndices = allBeamValues (\(Columnar' (Const ix)) -> ix) (primaryKey indexedTable)

-- | Use with 'set' to set a field to an explicit new value that does
-- not depend on any other value
toNewValue :: (forall s. QExpr be s a) -> QFieldAssignment be table a
toNewValue newVal = toUpdatedValue (\_ -> newVal)

-- | Use with 'set' to not modify the field
toOldValue :: QFieldAssignment be table a
toOldValue = toUpdatedValueMaybe (\_ -> Nothing)

-- | Use with 'set' to set a field to a new value that is calculated
-- based on one or more fields from the existing row
toUpdatedValue :: (forall s. table (QExpr be s) -> QExpr be s a) -> QFieldAssignment be table a
toUpdatedValue mkNewVal = toUpdatedValueMaybe (Just <$> mkNewVal)

-- | Use with 'set' to optionally set a fiield to a new value,
-- calculated based on one or more fields from the existing row
toUpdatedValueMaybe :: (forall s. table (QExpr be s) -> Maybe (QExpr be s a)) -> QFieldAssignment be table a
toUpdatedValueMaybe = QFieldAssignment

-- | Generate a 'SqlUpdate' that will update the given table row with the given value.
--
--   The SQL @UPDATE@ that is generated will set every non-primary key field for
--   the row where each primary key field is exactly what is given.
--
--   Note: This is a pure SQL @UPDATE@ command. This does not upsert or merge values.
save :: forall table be db.
        ( Table table
        , BeamSqlBackend be

        , SqlValableTable be (PrimaryKey table)
        , SqlValableTable be table

        , HasTableEquality be (PrimaryKey table)
        )
     => DatabaseEntity be db (TableEntity table)
        -- ^ Table to update
     -> table Identity
        -- ^ Value to set to
     -> SqlUpdate be table
save tbl v =
  updateTableRow tbl v
    (setFieldsTo (val_ v))

-- | Run a 'SqlUpdate' in a 'MonadBeam'.
runUpdate :: (BeamSqlBackend be, MonadBeam be m)
          => SqlUpdate be tbl -> m ()
runUpdate (SqlUpdate _ u) = runNoReturn (updateCmd u)
runUpdate SqlIdentityUpdate = pure ()

-- * DELETE

-- | Represents a SQL @DELETE@ statement for the given @table@
data SqlDelete be (table :: (* -> *) -> *)
  = SqlDelete !(TableSettings table) !(BeamSqlBackendDeleteSyntax be)

-- | Build a 'SqlDelete' from a table and a way to build a @WHERE@ clause
delete :: forall be db table
        . BeamSqlBackend be
       => DatabaseEntity be db (TableEntity table)
          -- ^ Table to delete from
       -> (forall s. (forall s'. table (QExpr be s')) -> QExpr be s Bool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlDelete be table
delete (DatabaseEntity dt@(DatabaseTable {})) mkWhere =
  SqlDelete (dbTableSettings dt)
            (deleteStmt (tableNameFromEntity dt) alias (Just (where_ "t")))
  where
    supportsAlias = deleteSupportsAlias (Proxy @(BeamSqlBackendDeleteSyntax be))

    tgtName = "delete_target"
    alias = if supportsAlias then Just tgtName else Nothing
    mkField = if supportsAlias then qualifiedField tgtName else unqualifiedField

    QExpr where_ = mkWhere (changeBeamRep (\(Columnar' fd) -> Columnar' (QExpr (pure (fieldE (mkField (fd ^. fieldName))))))
                             (dbTableSettings dt))

-- | Run a 'SqlDelete' in a 'MonadBeam'
runDelete :: (BeamSqlBackend be, MonadBeam be m)
          => SqlDelete be table -> m ()
runDelete (SqlDelete _ d) = runNoReturn (deleteCmd d)
