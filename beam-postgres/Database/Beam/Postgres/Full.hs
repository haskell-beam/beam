{-# LANGUAGE LambdaCase #-}

-- | Module providing (almost) full support for Postgres query and data
-- manipulation statements. These functions shadow the functions in
-- "Database.Beam.Query" and provide a strict superset of functionality. They
-- map 1-to-1 with the underlying Postgres support.
module Database.Beam.Postgres.Full where

import           Database.Beam hiding (insertValues)
import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema.Tables

import           Database.Beam.Postgres.Types
import           Database.Beam.Postgres.Syntax

import           Data.Monoid ((<>))
import qualified Data.Text as T

-- * @INSERT@

-- | A @beam-postgres@-specific version of 'Database.Beam.Query.insert', which
-- provides fuller support for the much richer Postgres @INSERT@ syntax. This
-- allows you to specify @ON CONFLICT@ actions. For even more complete support,
-- see 'insertReturning'.
insert :: DatabaseEntity Postgres db (TableEntity table)
       -> SqlInsertValues PgInsertValuesSyntax table
       -> PgInsertOnConflict table
       -> SqlInsert PgInsertSyntax
insert tbl values onConflict_ =
  case insertReturning tbl values onConflict_
         (Nothing :: Maybe (table (QExpr PgExpressionSyntax PostgresInaccessible) -> QExpr PgExpressionSyntax PostgresInaccessible Int)) of
    PgInsertReturning a ->
      SqlInsert (PgInsertSyntax a)
    PgInsertReturningEmpty ->
      SqlInsertNoRows

-- | The most general kind of @INSERT@ that postgres can perform
data PgInsertReturning a
  = PgInsertReturning PgSyntax
  | PgInsertReturningEmpty

-- | The full Postgres @INSERT@ syntax, supporting conflict actions and the
-- @RETURNING CLAUSE@. See 'PgInsertOnConflict' for how to specify a conflict
-- action or provide 'onConflictDefault' to preserve the behavior without any
-- @ON CONFLICT@ clause. The last argument takes a newly inserted row and
-- returns the expression to be returned as part of the @RETURNING@ clause. For
-- a backend-agnostic version of this functionality see
-- 'MonadBeamInsertReturning'. Use 'runInsertReturning' to get the results.
insertReturning :: Projectible PgExpressionSyntax a
                => DatabaseEntity Postgres be (TableEntity table)
                -> SqlInsertValues PgInsertValuesSyntax table
                -> PgInsertOnConflict table
                -> Maybe (table (QExpr PgExpressionSyntax PostgresInaccessible) -> a)
                -> PgInsertReturning (QExprToIdentity a)

insertReturning _ SqlInsertValuesEmpty _ _ = PgInsertReturningEmpty
insertReturning (DatabaseEntity (DatabaseTable tblNm tblSettings))
                (SqlInsertValues (PgInsertValuesSyntax insertValues_))
                (PgInsertOnConflict mkOnConflict)
                returning =
  PgInsertReturning $
  emit "INSERT INTO " <> pgQuotedIdentifier tblNm <>
  emit "(" <> pgSepBy (emit ", ") (allBeamValues (\(Columnar' f) -> pgQuotedIdentifier (_fieldName f)) tblSettings) <> emit ") " <>
  insertValues_ <> emit " " <> fromPgInsertOnConflict (mkOnConflict tblFields) <>
  (case returning of
     Nothing -> mempty
     Just mkProjection ->
         emit " RETURNING "<>
         pgSepBy (emit ", ") (map fromPgExpression (project (mkProjection tblQ) "t")))
   where
     tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (\_ -> fieldE (unqualifiedField (_fieldName f))))) tblSettings
     tblFields = changeBeamRep (\(Columnar' f) -> Columnar' (QField True tblNm (_fieldName f))) tblSettings

-- ** @ON CONFLICT@ clause

-- | What to do when an @INSERT@ statement inserts a row into the table @tbl@
-- that violates a constraint.
newtype PgInsertOnConflict (tbl :: (* -> *) -> *) =
    PgInsertOnConflict (tbl (QField PostgresInaccessible) -> PgInsertOnConflictSyntax)

-- | Specifies the kind of constraint that must be violated for the action to occur
newtype PgInsertOnConflictTarget (tbl :: (* -> *) -> *) =
    PgInsertOnConflictTarget (tbl (QExpr PgExpressionSyntax PostgresInaccessible) -> PgInsertOnConflictTargetSyntax)

-- | A description of what to do when a constraint or index is violated.
newtype PgConflictAction (tbl :: (* -> *) -> *) =
    PgConflictAction (tbl (QField PostgresInaccessible) -> PgConflictActionSyntax)

-- | By default, Postgres will throw an error when a conflict is detected. This
-- preserves that functionality.
onConflictDefault :: PgInsertOnConflict tbl
onConflictDefault = PgInsertOnConflict (\_ -> PgInsertOnConflictSyntax mempty)

-- | Tells postgres what to do on an @INSERT@ conflict. The first argument is
-- the type of conflict to provide an action for. For example, to only provide
-- an action for certain fields, use 'conflictingFields'. Or to only provide an
-- action over certain fields where a particular condition is met, use
-- 'conflictingFields'. If you have a particular constraint violation in mind,
-- use 'conflictingConstraint'. To perform an action on any conflict, use
-- 'anyConflict'.
--
-- See the
-- <https://www.postgresql.org/docs/current/static/sql-insert.html Postgres documentation>.
onConflict :: Beamable tbl
           => PgInsertOnConflictTarget tbl
           -> PgConflictAction tbl
           -> PgInsertOnConflict tbl
onConflict (PgInsertOnConflictTarget tgt) (PgConflictAction update_) =
  PgInsertOnConflict $ \tbl ->
  let exprTbl = changeBeamRep (\(Columnar' (QField _ _ nm)) ->
                                 Columnar' (QExpr (\_ -> fieldE (unqualifiedField nm))))
                              tbl
  in PgInsertOnConflictSyntax $
     emit "ON CONFLICT " <> fromPgInsertOnConflictTarget (tgt exprTbl)
                         <> fromPgConflictAction (update_ tbl)

-- | Perform the conflict action when any constraint or index conflict occurs.
-- Syntactically, this is the @ON CONFLICT@ clause, without any /conflict target/.
anyConflict :: PgInsertOnConflictTarget tbl
anyConflict = PgInsertOnConflictTarget (\_ -> PgInsertOnConflictTargetSyntax mempty)

-- | Perform the conflict action only when these fields conflict. The first
-- argument gets the current row as a table of expressions. Return the conflict
-- key. For more information, see the @beam-postgres@ manual.
conflictingFields :: Projectible PgExpressionSyntax proj
                  => (tbl (QExpr PgExpressionSyntax PostgresInaccessible) -> proj)
                  -> PgInsertOnConflictTarget tbl
conflictingFields makeProjection =
  PgInsertOnConflictTarget $ \tbl ->
  PgInsertOnConflictTargetSyntax $
  pgParens (pgSepBy (emit ", ") (map fromPgExpression (project (makeProjection tbl) "t"))) <> emit " "

-- | Like 'conflictingFields', but only perform the action if the condition
-- given in the second argument is met. See the postgres
-- <https://www.postgresql.org/docs/current/static/sql-insert.html manual> for
-- more information.
conflictingFieldsWhere :: Projectible PgExpressionSyntax proj
                       => (tbl (QExpr PgExpressionSyntax PostgresInaccessible) -> proj)
                       -> (tbl (QExpr PgExpressionSyntax PostgresInaccessible) ->
                           QExpr PgExpressionSyntax PostgresInaccessible Bool)
                       -> PgInsertOnConflictTarget tbl
conflictingFieldsWhere makeProjection makeWhere =
  PgInsertOnConflictTarget $ \tbl ->
  PgInsertOnConflictTargetSyntax $
  pgParens (pgSepBy (emit ", ") (map fromPgExpression (project (makeProjection tbl) "t"))) <>
  emit " WHERE " <>
  pgParens (let QExpr mkE = makeWhere tbl
                PgExpressionSyntax e = mkE "t"
            in e) <>
  emit " "

-- | Perform the action only if the given named constraint is violated
conflictingConstraint :: T.Text -> PgInsertOnConflictTarget tbl
conflictingConstraint nm =
  PgInsertOnConflictTarget $ \_ ->
  PgInsertOnConflictTargetSyntax $
  emit "ON CONSTRAINT " <> pgQuotedIdentifier nm <> emit " "

-- | The Postgres @DO NOTHING@ action
onConflictDoNothing :: PgConflictAction tbl
onConflictDoNothing = PgConflictAction $ \_ -> PgConflictActionSyntax (emit "DO NOTHING")

-- | The Postgres @DO UPDATE SET@ action, without the @WHERE@ clause. The
-- argument takes an updatable row (like the one used in 'update') and the
-- conflicting row. Use 'current_' on the first argument to get the current
-- value of the row in the database.
onConflictUpdateSet :: Beamable tbl
                    => (tbl (QField PostgresInaccessible) ->
                        tbl (QExpr PgExpressionSyntax PostgresInaccessible)  ->
                        [ QAssignment PgFieldNameSyntax PgExpressionSyntax PostgresInaccessible ])
                    -> PgConflictAction tbl
onConflictUpdateSet mkAssignments =
  PgConflictAction $ \tbl ->
  let assignments = mkAssignments tbl tblExcluded
      tblExcluded = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField "excluded" nm)))) tbl

      assignmentSyntaxes = do
        QAssignment assignments' <- assignments
        (fieldNm, expr) <- assignments'
        pure (fromPgFieldName fieldNm <> emit "=" <> pgParens (fromPgExpression expr))
  in PgConflictActionSyntax $
     emit "DO UPDATE SET " <> pgSepBy (emit ", ") assignmentSyntaxes

-- | The Postgres @DO UPDATE SET@ action, with the @WHERE@ clause. This is like
-- 'onConflictUpdateSet', but only rows satisfying the given condition are
-- updated. Sometimes this results in more efficient locking. See the Postgres
-- <https://www.postgresql.org/docs/current/static/sql-insert.html manual> for
-- more information.
onConflictUpdateSetWhere :: Beamable tbl
                         => (tbl (QField PostgresInaccessible) ->
                             tbl (QExpr PgExpressionSyntax PostgresInaccessible)  ->
                             [ QAssignment PgFieldNameSyntax PgExpressionSyntax PostgresInaccessible ])
                         -> (tbl (QExpr PgExpressionSyntax PostgresInaccessible) -> QExpr PgExpressionSyntax PostgresInaccessible Bool)
                         -> PgConflictAction tbl
onConflictUpdateSetWhere mkAssignments where_ =
  PgConflictAction $ \tbl ->
  let assignments = mkAssignments tbl tblExcluded
      QExpr where_' = where_ (changeBeamRep (\(Columnar' f) -> Columnar' (current_ f)) tbl)
      tblExcluded = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField "excluded" nm)))) tbl

      assignmentSyntaxes = do
        QAssignment assignments' <- assignments
        (fieldNm, expr) <- assignments'
        pure (fromPgFieldName fieldNm <> emit "=" <> pgParens (fromPgExpression expr))
  in PgConflictActionSyntax $
     emit "DO UPDATE SET " <> pgSepBy (emit ", ") assignmentSyntaxes <> emit " WHERE " <> fromPgExpression (where_' "t")

-- | Sometimes you want to update certain columns in the row. Given a
-- projection from a row to the fields you want, Beam can auto-generate
-- an assignment that assigns the corresponding fields of the conflicting row.
onConflictUpdateInstead :: (Beamable tbl, Projectible T.Text proj)
                        => (tbl (QExpr T.Text PostgresInaccessible) -> proj)
                        -> PgConflictAction tbl
onConflictUpdateInstead mkProj =
  onConflictUpdateSet $ \tbl _ ->
  let tblFields = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> nm))) tbl
      proj = project (mkProj tblFields) "t"

  in map (\fieldNm -> QAssignment [ (unqualifiedField fieldNm, fieldE (qualifiedField "excluded" fieldNm)) ]) proj

-- | Sometimes you want to update every value in the row. Beam can auto-generate
-- an assignment that assigns the conflicting row to every field in the database
-- row. This may not always be what you want.
onConflictSetAll :: (Beamable tbl, Projectible T.Text (tbl (QExpr T.Text PostgresInaccessible)))
                 => PgConflictAction tbl
onConflictSetAll = onConflictUpdateInstead id

-- * @UPDATE@

-- | The most general kind of @UPDATE@ that postgres can perform
data PgUpdateReturning a
  = PgUpdateReturning PgSyntax
  | PgUpdateReturningEmpty

-- | Postgres @UPDATE ... RETURNING@ statement support. The last
-- argument takes the newly inserted row and returns the values to be
-- returned. Use 'runUpdateReturning' to get the results.
updateReturning :: Projectible PgExpressionSyntax a
                => DatabaseEntity Postgres be (TableEntity table)
                -> (forall s. table (QField s) -> [ QAssignment PgFieldNameSyntax PgExpressionSyntax s ])
                -> (forall s. table (QExpr PgExpressionSyntax s) -> QExpr PgExpressionSyntax s Bool)
                -> (table (QExpr PgExpressionSyntax PostgresInaccessible) -> a)
                -> PgUpdateReturning (QExprToIdentity a)
updateReturning table@(DatabaseEntity (DatabaseTable _ tblSettings))
                mkAssignments
                mkWhere
                mkProjection =
  case update table mkAssignments mkWhere of
    SqlUpdate pgUpdate ->
      PgUpdateReturning $
      fromPgUpdate pgUpdate <>
      emit " RETURNING " <>
      pgSepBy (emit ", ") (map fromPgExpression (project (mkProjection tblQ) "t"))

    SqlIdentityUpdate -> PgUpdateReturningEmpty
  where
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings

-- * @DELETE@

-- | The most general kind of @DELETE@ that postgres can perform
newtype PgDeleteReturning a = PgDeleteReturning PgSyntax

-- | Postgres @DELETE ... RETURNING@ statement support. The last
-- argument takes the newly inserted row and returns the values to be
-- returned. Use 'runDeleteReturning' to get the results.
deleteReturning :: Projectible PgExpressionSyntax a
                => DatabaseEntity Postgres be (TableEntity table)
                -> (forall s. table (QExpr PgExpressionSyntax s) -> QExpr PgExpressionSyntax s Bool)
                -> (table (QExpr PgExpressionSyntax PostgresInaccessible) -> a)
                -> PgDeleteReturning (QExprToIdentity a)
deleteReturning table@(DatabaseEntity (DatabaseTable _ tblSettings))
                mkWhere
                mkProjection =
  PgDeleteReturning $
  fromPgDelete pgDelete <>
  emit " RETURNING " <>
  pgSepBy (emit ", ") (map fromPgExpression (project (mkProjection tblQ) "t"))
  where
    SqlDelete pgDelete = delete table mkWhere
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings
