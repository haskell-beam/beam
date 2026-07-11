{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

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

  -- ** @WITH@ statement consumers
  , pgSelectWith, pgInsertWith, pgUpdateWith, pgDeleteWith

  -- ** Lateral joins
  , lateral_

  -- * @INSERT@ and @INSERT RETURNING@
  , insert, insertReturning, cteInsertReturning
  , insertDefaults
  , runPgInsertReturningList

  , PgInsertReturning(..)

  -- ** Specifying conflict actions

  , PgInsertOnConflict(..)

  , onConflictDefault, onConflict
  , conflictingConstraint
  , BeamHasInsertOnConflict(..)
  , onConflictUpdateAll
  , onConflictUpdateInstead

  -- * @UPDATE RETURNING@
  , PgUpdateReturning(..)
  , runPgUpdateReturningList
  , updateReturning, cteUpdateReturning

  -- * @DELETE RETURNING@
  , PgDeleteReturning(..)
  , runPgDeleteReturningList
  , deleteReturning, cteDeleteReturning

  -- * Generalized @RETURNING@
  , PgReturning(..)
  ) where

import           Database.Beam hiding (insert, insertValues)
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.BeamExtensions
import qualified Database.Beam.Query.CTE as CTE
import           Database.Beam.Query.Internal
import           Database.Beam.Schema.Tables

import           Database.Beam.Postgres.Types
import           Database.Beam.Postgres.Syntax

import           Control.Monad.Free.Church
import           Control.Monad.State.Strict (evalState)
import           Control.Monad.Writer (runWriterT)

import           Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T

-- * @SELECT@

-- | An explicit lock against some tables. You can create a value of this type using the 'locked_'
-- function. You can combine these values monoidally to combine multiple locks for use with the
-- 'withLocks_' function.
newtype PgLockedTables s = PgLockedTables [ T.Text ]
  deriving (Semigroup, Monoid)

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
-- <https://haskell-beam.github.io/beam/user-guide/backends/beam-postgres/ the user guide> for usage
-- examples
withLocks_ :: a -> PgLockedTables s -> PgWithLocking s a
withLocks_ = flip PgWithLocking

-- | Join with a table while locking it explicitly. Provides a 'PgLockedTables' value that can be
-- used with 'withLocks_' to explicitly lock a table during a @SELECT@ statement
locked_ :: (Beamable tbl, Database Postgres db)
        => DatabaseEntity Postgres db (TableEntity tbl)
        -> Q Postgres db s (PgLockedTables s, tbl (QExpr Postgres s))
locked_ (DatabaseEntity dt) = do
  (nm, joined) <- Q (liftF (QAll (\_ -> fromTable (tableNamed (tableName (dbTableSchema dt) (dbTableCurrentName dt))) .
                                        Just . (,Nothing))
                                 (tableFieldsToExpressions (dbTableSettings dt))
                                 (\_ -> Nothing) id))
  pure (PgLockedTables [nm], joined)

-- | Lock some tables during the execution of a query. This is rather complicated, and there are
-- several usage examples in
-- <https://haskell-beam.github.io/beam/user-guide/backends/beam-postgres/ the user guide>
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
lockingFor_ :: forall a db s
             . ( Database Postgres db, Projectible Postgres a, ThreadRewritable (QNested s) a )
            => PgSelectLockingStrength
            -> Maybe PgSelectLockingOptions
            -> Q Postgres db (QNested s) (PgWithLocking (QNested s) a)
            -> Q Postgres db s (WithRewrittenThread (QNested s) s a)
lockingFor_ lockStrength mLockOptions (Q q) =
  Q (liftF (QForceSelect (\(PgWithLocking (PgLockedTables tblNms) _) tbl ords limit offset ->
                            let locking = PgSelectLockingClauseSyntax lockStrength tblNms mLockOptions
                            in pgSelectStmt tbl ords limit offset (Just locking))
                         q (\(PgWithLocking _ a) -> rewriteThread (Proxy @s) a)))

-- | Like 'lockingFor_', but does not require an explicit set of locked tables. This produces an
-- empty @FOR .. OF@ clause.
lockingAllTablesFor_ :: ( Database Postgres db, Projectible Postgres a, ThreadRewritable (QNested s) a )
                     => PgSelectLockingStrength
                     -> Maybe PgSelectLockingOptions
                     -> Q Postgres db (QNested s) a
                     -> Q Postgres db s (WithRewrittenThread (QNested s) s a)
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
       -> SqlInsert Postgres table
insert tbl@(DatabaseEntity dt@(DatabaseTable {})) values onConflict_ =
  case insertReturning tbl values onConflict_
         (Nothing :: Maybe (table (QExpr Postgres PostgresInaccessible) -> QExpr Postgres PostgresInaccessible Int)) of
    PgInsertReturning a ->
      SqlInsert (dbTableSettings dt) (PgInsertSyntax a)
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
insertReturning (DatabaseEntity tbl@(DatabaseTable {}))
                (SqlInsertValues (PgInsertValuesSyntax insertValues_))
                (PgInsertOnConflict mkOnConflict)
                mMkProjection =
  PgInsertReturning $
  emit "INSERT INTO " <> fromPgTableName (tableName (dbTableSchema tbl) (dbTableCurrentName tbl)) <>
  emit "(" <> pgSepBy (emit ", ") (allBeamValues (\(Columnar' f) -> pgQuotedIdentifier (_fieldName f)) tblSettings) <> emit ") " <>
  insertValues_ <> emit " " <> fromPgInsertOnConflict (mkOnConflict tblFields) <>
  (case mMkProjection of
     Nothing -> mempty
     Just mkProjection ->
         emit " RETURNING " <>
         pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t")))
   where
     tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (\_ -> fieldE (unqualifiedField (_fieldName f))))) tblSettings
     tblFields = changeBeamRep (\(Columnar' f) -> Columnar' (QField True (dbTableCurrentName tbl) (_fieldName f))) tblSettings

     tblSettings = dbTableSettings tbl

-- | Introduce a PostgreSQL @INSERT ... RETURNING@ statement as a
-- data-modifying common table expression. The returned value can be used in a
-- subsequent query with 'reuse'.
--
-- Returns 'Nothing' when the supplied insert values are empty, because in that
-- case there is no statement or common table expression to reuse.
-- Data-modifying CTEs are restricted to top-level 'selectWith' blocks and
-- cannot be used with 'pgSelectWith'.
--
-- For example, this inserts a row once and makes the rows produced by
-- @RETURNING@ available to the final query:
--
-- > selectWith $ do
-- >   inserted <- cteInsertReturning
-- >     users
-- >     (insertValues [newUser])
-- >     onConflictDefault
-- >     id
-- >   case inserted of
-- >     Nothing -> pure noRowsQuery
-- >     Just rows -> pure (reuse rows)
--
-- The generated statement has the shape:
--
-- @
-- WITH cte0 AS (INSERT INTO users ... RETURNING ...)
-- SELECT ... FROM cte0
-- @
cteInsertReturning
  :: ( Projectible Postgres a
     , ThreadRewritable PostgresInaccessible a
     , Projectible Postgres (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)
     , ThreadRewritable CTE.QAnyScope (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)
     )
  => DatabaseEntity Postgres db (TableEntity table)
  -> SqlInsertValues Postgres (table (QExpr Postgres s))
  -> PgInsertOnConflict table
  -> (table (QExpr Postgres PostgresInaccessible) -> a)
  -> With Postgres db 'CTE.CteTopLevelOnly (Maybe (ReusableQ Postgres db (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)))
cteInsertReturning table values onConflict_ mkProjection =
  case insertReturning table values onConflict_ (Just mkProjection) of
    PgInsertReturningEmpty -> pure Nothing
    PgInsertReturning syntax ->
      Just <$> CTE.dataModifyingCte
        (PgDataModifyingCommonTableExpressionSyntax syntax)

runPgInsertReturningList
  :: ( MonadBeam be m
     , BeamSqlBackendSyntax be ~ PgCommandSyntax
     , FromBackendRow be a
     )
  => PgInsertReturning a
  -> m [a]
runPgInsertReturningList = \case
  PgInsertReturningEmpty -> pure []
  PgInsertReturning syntax -> runReturningList $ PgCommandSyntax PgCommandTypeDataUpdateReturning syntax

-- ** @ON CONFLICT@ clause

-- | What to do when an @INSERT@ statement inserts a row into the table @tbl@
-- that violates a constraint.
newtype PgInsertOnConflict (tbl :: (Type -> Type) -> Type) =
    PgInsertOnConflict (tbl (QField QInternal) -> PgInsertOnConflictSyntax)

-- | Postgres @LATERAL JOIN@ support
--
-- Allows the use of variables introduced on the left side of a @JOIN@ to be used on the right hand
-- side.
--
-- Because of the default scoping rules, we can't use the typical monadic bind (@>>=@) operator to
-- create this join.
--
-- Instead, 'lateral_'  takes two  arguments. The first  is the  left hand side  of the  @JOIN@. The
-- second is a function that  takes the result of the first join and  uses those variables to create
-- the right hand side.
--
-- For example, to join table A with a subquery that returns the first three rows in B which matches
-- a column in A, ordered by another column in B:
--
-- > lateral_ (_tableA database) $ \tblA ->
-- >   limit_ 3 $
-- >   ordering_ (\(_, b) -> asc_ (_bField2 b)) $ do
-- >     b <- _tableB database
-- >     guard_ (_bField1 b ==. _aField1 a)
-- >     pure (a, b0
lateral_ :: forall s a b db
          . ( ThreadRewritable s a, ThreadRewritable (QNested s) b, Projectible Postgres b )
         => a -> (WithRewrittenThread s (QNested s) a -> Q Postgres db (QNested s) b)
         -> Q Postgres db s (WithRewrittenThread (QNested s) s b)
lateral_ using mkSubquery = do
  let Q subquery = mkSubquery (rewriteThread (Proxy @(QNested s)) using)
  Q (liftF (QArbitraryJoin subquery
                           "lat_"
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

-- | The SQL standard only allows CTE expressions (WITH expressions)
-- at the top-level. Postgres allows you to embed these within a
-- subquery.
--
-- For example,
--
-- @
-- SELECT a.column1, b.column2 FROM (WITH RECURSIVE ... ) a JOIN b
-- @
--
-- @beam-core@ offers 'selectWith' to produce a top-level 'SqlSelect'
-- but these cannot be turned into 'Q' objects for use within joins.
-- The 'pgSelectWith' function is more flexible. Its 'CteNestedAllowed' index
-- statically prevents PostgreSQL data-modifying CTEs from being embedded here;
-- those must be consumed by top-level 'selectWith'.
--
-- > select $ pgSelectWith $ do
-- >   reusableRows <- selecting someQuery
-- >   pure (reuse reusableRows)
--
-- This can produce a subquery such as:
--
-- @
-- SELECT ... FROM (WITH cte0 AS (SELECT ...) SELECT ... FROM cte0) AS nested
-- @
--
-- Replacing 'selecting' above with 'cteDeleteReturning', for example, does not
-- type-check because PostgreSQL requires data-modifying CTEs at the top level.
pgSelectWith :: forall db s res
              . Projectible Postgres res
             => With Postgres db 'CTE.CteNestedAllowed (Q Postgres db s res) -> Q Postgres db s res
pgSelectWith with =
    let (q, (recursiveness, mctes)) = evalState (runWriterT (CTE.runWith with)) 0
        fromSyntax tblPfx =
            case (recursiveness, nonEmpty mctes) of
              (CTE.Nonrecursive, Just ctes) -> withSyntax (NonEmpty.toList ctes) (buildSqlQuery tblPfx q)
              (CTE.Recursive, Just ctes) -> withRecursiveSyntax (NonEmpty.toList ctes) (buildSqlQuery tblPfx q)
               -- If there are no subqueries, we don't want to generate
               -- an empty 'WITH' statement, which would be malformed.
               -- 
               -- see: https://github.com/haskell-beam/beam/issues/760
              (_, Nothing) -> buildSqlQuery tblPfx q
    in Q (liftF (QAll (\tblPfx tName ->
                           let (_, names) = mkFieldNames @Postgres @res (qualifiedField tName)
                           in fromTable (PgTableSourceSyntax $
                                         mconcat [ emit "(", fromPgSelect (fromSyntax tblPfx), emit ")" ])
                                        (Just (tName, Just names)))
                      (\tName ->
                           let (projection, _) = mkFieldNames @Postgres @res (qualifiedField tName)
                           in projection)
                      (const Nothing)
                      snd))

-- | Attach a common-table-expression block to a top-level PostgreSQL
-- @INSERT@ statement.
--
-- Unlike 'pgSelectWith', this is a top-level statement consumer and therefore
-- accepts both 'CteNestedAllowed' and 'CteTopLevelOnly' blocks. The final
-- insert can read reusable rows produced by either SELECT CTEs or
-- data-modifying CTEs:
--
-- > pgInsertWith $ do
-- >   rows <- selecting sourceQuery
-- >   pure $ insert destination (insertFrom (reuse rows)) onConflictDefault
--
-- This produces a statement with the following shape:
--
-- @
-- WITH cte0 AS (SELECT ...)
-- INSERT INTO destination ... SELECT ... FROM cte0
-- @
--
-- If the final insert has no rows, the result remains 'SqlInsertNoRows'. There
-- is then no terminal statement to which PostgreSQL could attach the @WITH@
-- block, so none of its CTE bodies are executed.
--
-- Apply 'returning' to the resulting 'SqlInsert' when the terminal statement
-- should return rows.
pgInsertWith
  :: With Postgres db placement (SqlInsert Postgres table)
  -> SqlInsert Postgres table
pgInsertWith with =
  case runPgWith with of
    (SqlInsertNoRows, _, _) -> SqlInsertNoRows
    (SqlInsert settings (PgInsertSyntax statement), recursive, ctes) ->
      SqlInsert settings (PgInsertSyntax (pgWithSyntax recursive ctes statement))

-- | Attach a common-table-expression block to a top-level PostgreSQL
-- @UPDATE@ statement.
--
-- Reusable CTE rows can be referenced from the final update predicate, for
-- example through 'exists_':
--
-- > pgUpdateWith $ do
-- >   wanted <- selecting wantedUsers
-- >   pure $ update users
-- >     (\user -> userEnabled user <-. val_ False)
-- >     (\user -> exists_ $ do
-- >        candidate <- reuse wanted
-- >        guard_ (userId user ==. userId candidate))
--
-- An identity update remains 'SqlIdentityUpdate'; as with an empty insert,
-- there is no terminal PostgreSQL statement and the accumulated CTEs are not
-- executed.
--
-- Apply 'returning' to the resulting 'SqlUpdate' when the terminal statement
-- should return rows.
pgUpdateWith
  :: With Postgres db placement (SqlUpdate Postgres table)
  -> SqlUpdate Postgres table
pgUpdateWith with =
  case runPgWith with of
    (SqlIdentityUpdate, _, _) -> SqlIdentityUpdate
    (SqlUpdate settings (PgUpdateSyntax statement), recursive, ctes) ->
      SqlUpdate settings (PgUpdateSyntax (pgWithSyntax recursive ctes statement))

-- | Attach a common-table-expression block to a top-level PostgreSQL
-- @DELETE@ statement.
--
-- > pgDeleteWith $ do
-- >   expired <- selecting expiredUsers
-- >   pure $ delete users $ \user -> exists_ $ do
-- >     candidate <- reuse expired
-- >     guard_ (userId user ==. userId candidate)
--
-- Since 'SqlDelete' always contains a statement, the accumulated CTE block is
-- always preserved.
-- Apply 'returning' to the result when the terminal statement should return
-- deleted rows.
pgDeleteWith
  :: With Postgres db placement (SqlDelete Postgres table)
  -> SqlDelete Postgres table
pgDeleteWith with =
  case runPgWith with of
    (SqlDelete settings (PgDeleteSyntax statement), recursive, ctes) ->
      SqlDelete settings (PgDeleteSyntax (pgWithSyntax recursive ctes statement))

-- Evaluate a PostgreSQL CTE builder once and retain the information required
-- by each top-level statement consumer. Keeping this helper local ensures that
-- the backend-independent CTE API does not acquire PostgreSQL command types.
runPgWith
  :: With Postgres db placement a
  -> (a, Bool, [BeamSql99BackendCTESyntax Postgres])
runPgWith with =
  let (result, (recursiveness, ctes)) =
        evalState (runWriterT (CTE.runWith with)) 0
      recursive = case recursiveness of
        CTE.Nonrecursive -> False
        CTE.Recursive -> True
  in (result, recursive, ctes)

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
           => SqlConflictTarget Postgres tbl
           -> SqlConflictAction Postgres tbl
           -> PgInsertOnConflict tbl
onConflict (PgInsertOnConflictTarget tgt) (PgConflictAction update_) =
  PgInsertOnConflict $ \tbl ->
  let exprTbl = changeBeamRep (\(Columnar' (QField _ _ nm)) ->
                                 Columnar' (QExpr (\_ -> fieldE (unqualifiedField nm))))
                              tbl
  in PgInsertOnConflictSyntax $
     emit "ON CONFLICT " <> fromPgInsertOnConflictTarget (tgt exprTbl)
                         <> fromPgConflictAction (update_ tbl)

-- | Perform the action only if the given named constraint is violated
conflictingConstraint :: T.Text -> SqlConflictTarget Postgres tbl
conflictingConstraint nm =
  PgInsertOnConflictTarget $ \_ ->
  PgInsertOnConflictTargetSyntax $
  emit "ON CONSTRAINT " <> pgQuotedIdentifier nm <> emit " "

-- * @UPDATE@

-- | The most general kind of @UPDATE@ that postgres can perform
--
-- You can build this from a 'SqlUpdate' by using 'returning'
--
-- > update tbl where `returning` projection
--
-- Run the result with 'runPgUpdateReturningList'
data PgUpdateReturning a
  = PgUpdateReturning PgSyntax
  | PgUpdateReturningEmpty

-- | Postgres @UPDATE ... RETURNING@ statement support. The last
-- argument takes the newly inserted row and returns the values to be
-- returned. Use 'runUpdateReturning' to get the results.
updateReturning :: Projectible Postgres a
                => DatabaseEntity Postgres be (TableEntity table)
                -> (forall s. table (QField s) -> QAssignment Postgres s)
                -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
                -> (table (QExpr Postgres PostgresInaccessible) -> a)
                -> PgUpdateReturning (QExprToIdentity a)
updateReturning table@(DatabaseEntity (DatabaseTable { dbTableSettings = tblSettings }))
                mkAssignments
                mkWhere
                mkProjection =
  case update table mkAssignments mkWhere of
    SqlUpdate _ pgUpdate ->
      PgUpdateReturning $
      fromPgUpdate pgUpdate <>
      emit " RETURNING " <>
      pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t"))

    SqlIdentityUpdate -> PgUpdateReturningEmpty
  where
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings

-- | Introduce a PostgreSQL @UPDATE ... RETURNING@ statement as a
-- data-modifying common table expression. The returned value can be used in a
-- subsequent query with 'reuse'.
--
-- Returns 'Nothing' when the assignments form an identity update, because in
-- that case there is no statement or common table expression to reuse.
-- Data-modifying CTEs are restricted to top-level 'selectWith' blocks and
-- cannot be used with 'pgSelectWith'.
--
-- > selectWith $ do
-- >   updated <- cteUpdateReturning
-- >     users
-- >     (\user -> userEnabled user <-. val_ False)
-- >     (\user -> userId user ==. val_ wantedUserId)
-- >     id
-- >   case updated of
-- >     Nothing -> pure noRowsQuery
-- >     Just rows -> pure (reuse rows)
--
-- This renders the update once inside @WITH@ and reads its @RETURNING@ rows
-- through the reusable CTE name.
cteUpdateReturning
  :: ( Projectible Postgres a
     , ThreadRewritable PostgresInaccessible a
     , Projectible Postgres (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)
     , ThreadRewritable CTE.QAnyScope (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)
     )
  => DatabaseEntity Postgres db (TableEntity table)
  -> (forall s. table (QField s) -> QAssignment Postgres s)
  -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
  -> (table (QExpr Postgres PostgresInaccessible) -> a)
  -> With Postgres db 'CTE.CteTopLevelOnly (Maybe (ReusableQ Postgres db (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)))
cteUpdateReturning table mkAssignments mkWhere mkProjection =
  case updateReturning table mkAssignments mkWhere mkProjection of
    PgUpdateReturningEmpty -> pure Nothing
    PgUpdateReturning syntax ->
      Just <$> CTE.dataModifyingCte
        (PgDataModifyingCommonTableExpressionSyntax syntax)

runPgUpdateReturningList
  :: ( MonadBeam be m
     , BeamSqlBackendSyntax be ~ PgCommandSyntax
     , FromBackendRow be a
     )
  => PgUpdateReturning a
  -> m [a]
runPgUpdateReturningList = \case
  PgUpdateReturningEmpty -> pure []
  PgUpdateReturning syntax -> runReturningList $ PgCommandSyntax PgCommandTypeDataUpdateReturning syntax

-- * @DELETE@

-- | The most general kind of @DELETE@ that postgres can perform
--
-- You can build this from a 'SqlDelete' by using 'returning'
--
-- > delete tbl where `returning` projection
--
-- Run the result with 'runPgDeleteReturningList'
newtype PgDeleteReturning a = PgDeleteReturning PgSyntax

-- | Postgres @DELETE ... RETURNING@ statement support. The last
-- argument takes the newly inserted row and returns the values to be
-- returned. Use 'runDeleteReturning' to get the results.
deleteReturning :: Projectible Postgres a
                => DatabaseEntity Postgres be (TableEntity table)
                -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
                -> (table (QExpr Postgres PostgresInaccessible) -> a)
                -> PgDeleteReturning (QExprToIdentity a)
deleteReturning table@(DatabaseEntity (DatabaseTable { dbTableSettings = tblSettings }))
                mkWhere
                mkProjection =
  PgDeleteReturning $
  fromPgDelete pgDelete <>
  emit " RETURNING " <>
  pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t"))
  where
    SqlDelete _ pgDelete = delete table $ \t -> mkWhere t
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings

-- | Introduce a PostgreSQL @DELETE ... RETURNING@ statement as a
-- data-modifying common table expression. The returned value can be used in a
-- subsequent query with 'reuse'.
--
-- Data-modifying CTEs are restricted to top-level 'selectWith' blocks and
-- cannot be used with 'pgSelectWith'.
--
-- Unlike insert and update, delete always has a statement to introduce, so no
-- 'Maybe' is required:
--
-- > selectWith $ do
-- >   deleted <- cteDeleteReturning
-- >     users
-- >     (\user -> userExpired user ==. val_ True)
-- >     id
-- >   pure (reuse deleted)
--
-- The final query observes the deleted rows through @DELETE ... RETURNING@.
-- This is also the supported way to communicate between data-modifying CTEs,
-- since PostgreSQL executes sibling statements against the same snapshot.
cteDeleteReturning
  :: ( Projectible Postgres a
     , ThreadRewritable PostgresInaccessible a
     , Projectible Postgres (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)
     , ThreadRewritable CTE.QAnyScope (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)
     )
  => DatabaseEntity Postgres db (TableEntity table)
  -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
  -> (table (QExpr Postgres PostgresInaccessible) -> a)
  -> With Postgres db 'CTE.CteTopLevelOnly (ReusableQ Postgres db (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a))
cteDeleteReturning table mkWhere mkProjection =
  let PgDeleteReturning syntax = deleteReturning table mkWhere mkProjection
  in CTE.dataModifyingCte
       (PgDataModifyingCommonTableExpressionSyntax syntax)

runPgDeleteReturningList
  :: ( MonadBeam be m
     , BeamSqlBackendSyntax be ~ PgCommandSyntax
     , FromBackendRow be a
     )
  => PgDeleteReturning a
  -> m [a]
runPgDeleteReturningList (PgDeleteReturning syntax) = runReturningList $ PgCommandSyntax PgCommandTypeDataUpdateReturning syntax

-- * General @RETURNING@ support

class PgReturning cmd where
  type PgReturningType cmd :: Type -> Type

  returning :: (Beamable tbl, Projectible Postgres a)
            => cmd Postgres tbl -> (tbl (QExpr Postgres PostgresInaccessible) -> a)
            -> PgReturningType cmd (QExprToIdentity a)

instance PgReturning SqlInsert where
  type PgReturningType SqlInsert = PgInsertReturning

  returning SqlInsertNoRows _ = PgInsertReturningEmpty
  returning (SqlInsert tblSettings (PgInsertSyntax syntax)) mkProjection =
    PgInsertReturning $
    syntax <> emit " RETURNING " <>
    pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t"))

    where
      tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr . pure . fieldE . unqualifiedField . _fieldName $ f)) tblSettings

instance PgReturning SqlUpdate where
  type PgReturningType SqlUpdate = PgUpdateReturning

  returning SqlIdentityUpdate _ = PgUpdateReturningEmpty
  returning (SqlUpdate tblSettings (PgUpdateSyntax syntax)) mkProjection =
    PgUpdateReturning $
    syntax <> emit " RETURNING " <>
    pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t"))

    where
      tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr . pure . fieldE . unqualifiedField . _fieldName $ f)) tblSettings

instance PgReturning SqlDelete where
  type PgReturningType SqlDelete = PgDeleteReturning

  returning (SqlDelete tblSettings (PgDeleteSyntax syntax)) mkProjection =
    PgDeleteReturning $
    syntax <> emit " RETURNING " <>
    pgSepBy (emit ", ") (map fromPgExpression (project (Proxy @Postgres) (mkProjection tblQ) "t"))

    where
      tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr . pure . fieldE . unqualifiedField . _fieldName $ f)) tblSettings

instance BeamHasInsertOnConflict Postgres where
  newtype SqlConflictTarget Postgres table =
    PgInsertOnConflictTarget (table (QExpr Postgres QInternal) -> PgInsertOnConflictTargetSyntax)
  newtype SqlConflictAction Postgres table =
    PgConflictAction (table (QField QInternal) -> PgConflictActionSyntax)

  insertOnConflict tbl vs target action = insert tbl vs $ onConflict target action

  -- | Perform the conflict action when any constraint or index conflict occurs.
  -- Syntactically, this is the @ON CONFLICT@ clause, without any /conflict target/.
  anyConflict = PgInsertOnConflictTarget (\_ -> PgInsertOnConflictTargetSyntax mempty)

  -- | The Postgres @DO NOTHING@ action
  onConflictDoNothing = PgConflictAction $ \_ -> PgConflictActionSyntax (emit "DO NOTHING")

  -- | The Postgres @DO UPDATE SET@ action, without the @WHERE@ clause. The
  -- argument takes an updatable row (like the one used in 'update') and the
  -- conflicting row. Use 'current_' on the first argument to get the current
  -- value of the row in the database.
  onConflictUpdateSet mkAssignments =
    PgConflictAction $ \tbl ->
    let QAssignment assignments = mkAssignments tbl tblExcluded
        tblExcluded = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField "excluded" nm)))) tbl

        assignmentSyntaxes =
          [ fromPgFieldName fieldNm <> emit "=" <> pgParens (fromPgExpression expr)
          | (fieldNm, expr) <- assignments ]
    in PgConflictActionSyntax $
       emit "DO UPDATE SET " <> pgSepBy (emit ", ") assignmentSyntaxes

  -- | The Postgres @DO UPDATE SET@ action, with the @WHERE@ clause. This is like
  -- 'onConflictUpdateSet', but only rows satisfying the given condition are
  -- updated. Sometimes this results in more efficient locking. See the Postgres
  -- <https://www.postgresql.org/docs/current/static/sql-insert.html manual> for
  -- more information.
  onConflictUpdateSetWhere mkAssignments where_ =
    PgConflictAction $ \tbl ->
    let QAssignment assignments = mkAssignments tbl tblExcluded
        QExpr where_' = where_ tbl tblExcluded
        tblExcluded = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField "excluded" nm)))) tbl

        assignmentSyntaxes =
          [ fromPgFieldName fieldNm <> emit "=" <> pgParens (fromPgExpression expr)
          | (fieldNm, expr) <- assignments ]
    in PgConflictActionSyntax $
       emit "DO UPDATE SET " <> pgSepBy (emit ", ") assignmentSyntaxes <> emit " WHERE " <> fromPgExpression (where_' "t")

  -- | Perform the conflict action only when these fields conflict. The first
  -- argument gets the current row as a table of expressions. Return the conflict
  -- key. For more information, see the @beam-postgres@ manual.
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
