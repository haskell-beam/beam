{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- | Module providing (almost) full support for Postgres query and data
-- manipulation statements. These functions shadow the functions in
-- "Database.Beam.Query" and provide a strict superset of functionality. They
-- map 1-to-1 with the underlying Postgres support.
module Database.Beam.Postgres.Full
  ( -- * Additional @SELECT@ features

    -- ** @SELECT@ Locking clause
    PgWithLocking, PgLockedTables
  , PgSelectLockingStrength(..), PgSelectLockingOptions(..)
  , lockingAllTablesFor_, lockingFor_

  , locked_, lockAll_, withLocks_

  -- ** Lateral joins
  , lateral_

  -- * @INSERT@ and @INSERT RETURNING@
  , insert, insertReturning
  , insertDefaults

  , PgInsertReturning(..)

  -- ** Specifying conflict actions

  , PgInsertOnConflict(..), PgInsertOnConflictTarget(..)
  , PgConflictAction(..)

  , onConflictDefault, onConflict, anyConflict, conflictingFields
  , conflictingFieldsWhere, conflictingConstraint
  , onConflictDoNothing, onConflictUpdateSet
  , onConflictUpdateSetWhere, onConflictUpdateInstead
  , onConflictSetAll

  -- * @UPDATE RETURNING@
  , PgUpdateReturning(..)
  , updateReturning

  -- * @DELETE RETURNING@
  , PgDeleteReturning(..)
  , deleteReturning
  ) where

import           Database.Beam hiding (insert, insertValues)
import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema.Tables

import           Database.Beam.Postgres.Types
import           Database.Beam.Postgres.Syntax

import           Control.Monad.Free.Church
import           Control.Monad.Writer (execWriter, tell)

import           Data.Functor.Const
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

-- * @SELECT@

-- | An explicit lock against some tables. You can create a value of this type using the 'locked_'
-- function. You can combine these values monoidally to combine multiple locks for use with the
-- 'withLocks_' function.
newtype PgLockedTables s = PgLockedTables [ T.Text ]
instance Monoid (PgLockedTables s) where
  mempty = PgLockedTables []
  mappend (PgLockedTables a) (PgLockedTables b) = PgLockedTables (a <> b)

-- | Combines the result of a query along with a set of locked tables. Used as a
-- return value for the 'lockingFor_' function.
data PgWithLocking s a = PgWithLocking (PgLockedTables s) a
instance ProjectibleWithPredicate c be res a => ProjectibleWithPredicate c be res (PgWithLocking s a) where
  project' p be mutateM (PgWithLocking tbls a) =
    PgWithLocking tbls <$> project' p be mutateM a

  projectSkeleton' ctxt be mkM =
    PgWithLocking mempty <$> projectSkeleton' ctxt be mkM

-- | Use with 'lockingFor_' to lock all tables mentioned in the query
lockAll_ :: a -> PgWithLocking s a
lockAll_ = PgWithLocking mempty

-- | Return and lock the given tables. Typically used as an infix operator. See the
-- <http://tathougies.github.io/beam/user-guide/backends/beam-postgres/ the user guide> for usage
-- examples
withLocks_ :: a -> PgLockedTables s -> PgWithLocking s a
withLocks_ = flip PgWithLocking

-- | Join with a table while locking it explicitly. Provides a 'PgLockedTables' value that can be
-- used with 'withLocks_' to explicitly lock a table during a @SELECT@ statement
locked_ :: Database Postgres db
        => DatabaseEntity Postgres db (TableEntity tbl)
        -> Q Postgres db s (PgLockedTables s, tbl (QExpr Postgres s))
locked_ (DatabaseEntity (DatabaseTable tblNm tblSettings)) = do
  (nm, joined) <- Q (liftF (QAll (\_ -> fromTable (tableNamed tblNm) . Just . (,Nothing))
                                 (tableFieldsToExpressions tblSettings)
                                 (\_ -> Nothing) id))
  pure (PgLockedTables [nm], joined)

-- | Lock some tables during the execution of a query. This is rather complicated, and there are
-- several usage examples in
-- <http://tathougies.github.io/beam/user-guide/backends/beam-postgres/ the user guide>
--
-- The Postgres locking clause is rather complex, and beam currently does not check several
-- pre-conditions. It is assumed you kinda know what you're doing.
--
-- Things which postgres doesn't like, but beam will do
--
-- * Using aggregates within a query that has a locking clause
-- * Using @UNION@, @INTERSECT@, or @EXCEPT@
--
--   See <https://www.postgresql.org/docs/10/static/sql-select.html#SQL-FOR-UPDATE-SHARE here> for
--   more details.
--
-- This function accepts a locking strength (@UPDATE@, @SHARE@, @KEY SHARE@, etc), an optional
-- locking option (@NOWAIT@ or @SKIP LOCKED@), and a query whose rows to lock. The query should
-- return its result wrapped in 'PgWithLocking', via the `withLocks_` or `lockAll_` function.
--
-- If you want to use the most common behavior (lock all rows in every table mentioned), the
-- 'lockingAllTablesFor_' function may be what you're after.
lockingFor_ :: ( Database Postgres db, Projectible Postgres a )
            => PgSelectLockingStrength
            -> Maybe PgSelectLockingOptions
            -> Q Postgres db (QNested s) (PgWithLocking (QNested s) a)
            -> Q Postgres db s a
lockingFor_ lockStrength mLockOptions (Q q) =
  Q (liftF (QForceSelect (\(PgWithLocking (PgLockedTables tblNms) _) tbl ords limit offset ->
                            let locking = PgSelectLockingClauseSyntax lockStrength tblNms mLockOptions
                            in pgSelectStmt tbl ords limit offset (Just locking))
                         q (\(PgWithLocking _ a) -> a)))

-- | Like 'lockingFor_', but does not require an explicit set of locked tables. This produces an
-- empty @FOR .. OF@ clause.
lockingAllTablesFor_ :: ( Database Postgres db, Projectible Postgres a )
                     => PgSelectLockingStrength
                     -> Maybe PgSelectLockingOptions
                     -> Q Postgres db (QNested s) a
                     -> Q Postgres db s a
lockingAllTablesFor_ lockStrength mLockOptions q =
  lockingFor_ lockStrength mLockOptions (lockAll_ <$> q)

-- * @INSERT@

-- | The Postgres @DEFAULT VALUES@ clause for the @INSERT@ command.
insertDefaults :: SqlInsertValues Postgres tbl
insertDefaults = SqlInsertValues (PgInsertValuesSyntax (emit "DEFAULT VALUES"))

-- | A @beam-postgres@-specific version of 'Database.Beam.Query.insert', which
-- provides fuller support for the much richer Postgres @INSERT@ syntax. This
-- allows you to specify @ON CONFLICT@ actions. For even more complete support,
-- see 'insertReturning'.
insert :: DatabaseEntity Postgres db (TableEntity table)
       -> SqlInsertValues Postgres (table (QExpr Postgres s)) -- TODO arbitrary projectibles
       -> PgInsertOnConflict table
       -> SqlInsert Postgres
insert tbl values onConflict_ =
  case insertReturning tbl values onConflict_
         (Nothing :: Maybe (table (QExpr Postgres PostgresInaccessible) -> QExpr Postgres PostgresInaccessible Int)) of
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
insertReturning :: Projectible Postgres a
                => DatabaseEntity Postgres be (TableEntity table)
                -> SqlInsertValues Postgres (table (QExpr Postgres s))
                -> PgInsertOnConflict table
                -> Maybe (table (QExpr Postgres PostgresInaccessible) -> a)
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
         pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t")))
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
    PgInsertOnConflictTarget (tbl (QExpr Postgres PostgresInaccessible) -> PgInsertOnConflictTargetSyntax)

-- | A description of what to do when a constraint or index is violated.
newtype PgConflictAction (tbl :: (* -> *) -> *) =
    PgConflictAction (tbl (QField PostgresInaccessible) -> PgConflictActionSyntax)

lateral_ :: forall s a b db
          . ( ThreadRewritable s a, ThreadRewritable (QNested s) b, Projectible Postgres b )
         => a -> (WithRewrittenThread s (QNested s) a -> Q Postgres db (QNested s) b)
         -> Q Postgres db s (WithRewrittenThread (QNested s) s b)
lateral_ using mkSubquery = do
  let Q subquery = mkSubquery (rewriteThread (Proxy @(QNested s)) using)
  Q (liftF (QArbitraryJoin subquery
                           (\a b on' ->
                              case on' of
                                Nothing ->
                                  PgFromSyntax $
                                  fromPgFrom a <> emit " CROSS JOIN LATERAL " <> fromPgFrom b
                                Just on'' ->
                                  PgFromSyntax $
                                  fromPgFrom a <> emit " JOIN LATERAL " <> fromPgFrom b <> emit " ON " <> fromPgExpression on'')
                           (\_ -> Nothing)
                           (rewriteThread (Proxy @s))))

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
conflictingFields :: Projectible Postgres proj
                  => (tbl (QExpr Postgres PostgresInaccessible) -> proj)
                  -> PgInsertOnConflictTarget tbl
conflictingFields makeProjection =
  PgInsertOnConflictTarget $ \tbl ->
  PgInsertOnConflictTargetSyntax $
  pgParens (pgSepBy (emit ", ") $
            map fromPgExpression $
            project (Proxy @Postgres) (makeProjection tbl) "t") <>
  emit " "

-- | Like 'conflictingFields', but only perform the action if the condition
-- given in the second argument is met. See the postgres
-- <https://www.postgresql.org/docs/current/static/sql-insert.html manual> for
-- more information.
conflictingFieldsWhere :: Projectible Postgres proj
                       => (tbl (QExpr Postgres PostgresInaccessible) -> proj)
                       -> (tbl (QExpr Postgres PostgresInaccessible) ->
                           QExpr Postgres PostgresInaccessible Bool)
                       -> PgInsertOnConflictTarget tbl
conflictingFieldsWhere makeProjection makeWhere =
  PgInsertOnConflictTarget $ \tbl ->
  PgInsertOnConflictTargetSyntax $
  pgParens (pgSepBy (emit ", ") $
            map fromPgExpression (project (Proxy @Postgres)
                                          (makeProjection tbl) "t")) <>
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
                        tbl (QExpr Postgres PostgresInaccessible)  ->
                        [ QAssignment Postgres PostgresInaccessible ])
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
                             tbl (QExpr Postgres PostgresInaccessible)  ->
                             [ QAssignment Postgres PostgresInaccessible ])
                         -> (tbl (QExpr Postgres PostgresInaccessible) -> QExpr Postgres PostgresInaccessible Bool)
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
onConflictUpdateInstead :: (Beamable tbl, ProjectibleWithPredicate AnyType () T.Text proj)
                        => (tbl (Const T.Text) -> proj)
                        -> PgConflictAction tbl
onConflictUpdateInstead mkProj =
  onConflictUpdateSet $ \tbl _ ->
  let tblFields = changeBeamRep (\(Columnar' (QField _ _ nm) :: Columnar' (QField PostgresInaccessible) a) -> Columnar' (Const nm) :: Columnar' (Const T.Text) a) tbl
      proj = execWriter (project' (Proxy @AnyType) (Proxy @((), T.Text))
                                  (\_ _ e -> tell [e] >> pure e)
                                  (mkProj tblFields))

  in map (\fieldNm -> QAssignment [ (unqualifiedField fieldNm, fieldE (qualifiedField "excluded" fieldNm)) ]) proj

-- | Sometimes you want to update every value in the row. Beam can auto-generate
-- an assignment that assigns the conflicting row to every field in the database
-- row. This may not always be what you want.
onConflictSetAll :: ( Beamable tbl
                    , ProjectibleWithPredicate AnyType () T.Text (tbl (Const T.Text)) )
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
updateReturning :: Projectible Postgres a
                => DatabaseEntity Postgres be (TableEntity table)
                -> (forall s. table (QField s) -> [ QAssignment Postgres s ])
                -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
                -> (table (QExpr Postgres PostgresInaccessible) -> a)
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
      pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t"))

    SqlIdentityUpdate -> PgUpdateReturningEmpty
  where
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings

-- * @DELETE@

-- | The most general kind of @DELETE@ that postgres can perform
newtype PgDeleteReturning a = PgDeleteReturning PgSyntax

-- | Postgres @DELETE ... RETURNING@ statement support. The last
-- argument takes the newly inserted row and returns the values to be
-- returned. Use 'runDeleteReturning' to get the results.
deleteReturning :: Projectible Postgres a
                => DatabaseEntity Postgres be (TableEntity table)
                -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
                -> (table (QExpr Postgres PostgresInaccessible) -> a)
                -> PgDeleteReturning (QExprToIdentity a)
deleteReturning table@(DatabaseEntity (DatabaseTable _ tblSettings))
                mkWhere
                mkProjection =
  PgDeleteReturning $
  fromPgDelete pgDelete <>
  emit " RETURNING " <>
  pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t"))
  where
    SqlDelete pgDelete = delete table mkWhere
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings
