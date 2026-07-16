{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Module providing (almost) full support for Postgres query and data
-- manipulation statements. These functions shadow the functions in
-- "Database.Beam.Query" and provide a strict superset of functionality. They
-- map 1-to-1 with the underlying Postgres support.
--
-- PostgreSQL-specific common table expressions use the placement-indexed
-- 'PgWith' builder. It supports explicit SELECT materialization and both
-- returning and side-effect-only data-modifying CTEs without changing the
-- portable CTE API in beam-core.
module Database.Beam.Postgres.Full
  ( -- * Additional @SELECT@ features

    -- ** @SELECT@ Locking clause
    PgWithLocking, PgLockedTables
  , PgSelectLockingStrength(..), PgSelectLockingOptions(..)
  , lockingAllTablesFor_, lockingFor_

  , locked_, lockAll_, withLocks_

  -- ** Common table expressions
  , PgCtePlacement(..), PgWith
  , pgLiftWith, pgToTopLevel
  , PgCteMaterialization(..), pgSelecting, pgSelectingWith
  , pgSelectWith, pgSelectWithNested, pgSelectWithTopLevel
  , pgInsertWith, pgUpdateWith, pgDeleteWith

  -- ** Lateral joins
  , lateral_

  -- * @INSERT@ and @INSERT RETURNING@
  , insert, insertReturning, cteInsert, cteInsertReturning
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
  , updateReturning, cteUpdate, cteUpdateReturning

  -- * @DELETE RETURNING@
  , PgDeleteReturning(..)
  , runPgDeleteReturningList
  , deleteReturning, cteDelete, cteDeleteReturning

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

import           Control.Monad.Fix (MonadFix(..))
import           Control.Monad.Free.Church
import           Control.Monad.State.Strict (evalState, get, put)
import           Control.Monad.Writer (runWriterT, tell)

import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T

-- * @SELECT@

-- | Whether every CTE in a PostgreSQL @WITH@ block may appear in a nested
-- query, or whether the block must be attached to a top-level statement.
--
-- PostgreSQL permits SELECT CTEs in nested queries, but permits data-modifying
-- CTEs only in a @WITH@ clause attached to the top-level statement. The index
-- on 'PgWith' records that rule for the builders in this module, so invalid
-- nesting is rejected by Haskell rather than by PostgreSQL. See PostgreSQL's
-- <https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-MODIFYING data-modifying WITH documentation>.
--
-- @since 0.6.3.0
data PgCtePlacement
  = PgCteNestedAllowed
    -- ^ The block contains only CTEs which may be nested.
  | PgCteTopLevelOnly
    -- ^ The block contains a data-modifying CTE and must remain top-level.

-- | A PostgreSQL-specific CTE builder.
--
-- This newtype uses beam-core's existing 'With' action and CTE accumulator. It
-- adds a placement index for PostgreSQL data-modifying CTEs without introducing
-- a second name supply or syntax writer. Consequently a lifted portable helper
-- and a native PostgreSQL CTE allocate names from the same sequence.
--
-- Use 'pgSelecting' for SELECT CTEs, 'cteInsertReturning',
-- 'cteUpdateReturning', or 'cteDeleteReturning' for reusable modifying CTEs,
-- and 'cteInsert', 'cteUpdate', or 'cteDelete' for side-effect-only CTEs.
-- Consume the completed block with 'pgSelectWithTopLevel', 'pgInsertWith',
-- 'pgUpdateWith', or 'pgDeleteWith'.
--
-- @since 0.6.3.0
newtype PgWith db (placement :: PgCtePlacement) a =
  PgWith { unPgWith :: With Postgres db a }
  deriving (Functor, Applicative, Monad)

-- The placement parameter is phantom at runtime. A nominal role prevents
-- Data.Coerce from relabelling a top-level-only block as nested-safe.
type role PgWith nominal nominal nominal

-- Recursive knots are deliberately restricted to SELECT-only construction.
-- Complete such a knot first and then use 'pgToTopLevel' before adding a
-- data-modifying CTE which consumes its rows.
instance MonadFix (PgWith db 'PgCteNestedAllowed) where
  mfix f = PgWith (mfix (unPgWith . f))

-- | Lift an existing portable PostgreSQL 'With' helper into 'PgWith'.
--
-- Helpers constructed with the portable 'selecting' API contain SELECT
-- statements, so they are valid at either placement. The lifted action uses
-- the surrounding 'PgWith' name supply; lifting a helper which defines several
-- CTEs therefore cannot collide with CTEs defined before or after it.
--
-- > native <- pgSelecting nativeQuery
-- > rows <- pgLiftWith existingSelectCtes
-- > changed <- cteDeleteReturning table predicate id
-- > pure $ do
-- >   nativeRow <- reuse native
-- >   row <- reuse rows
-- >   changedRow <- reuse changed
-- >   pure (nativeRow, row, changedRow)
--
-- If @existingSelectCtes@ defines two CTEs and one native CTE precedes it,
-- Beam generates one block whose names continue through the lifted helper:
--
-- @
-- WITH "cte0"("res0") AS (SELECT ...),
--      "cte1"("res0") AS (SELECT ...),
--      "cte2"("res0") AS (SELECT ... FROM "cte1"),
--      "cte3"("res0", "res1") AS
--        (DELETE FROM "table" ... RETURNING "id", "value")
-- SELECT ... FROM "cte0" CROSS JOIN "cte2" CROSS JOIN "cte3"
-- @
--
-- The 'With' constructor is public for low-level extension code. 'pgLiftWith'
-- assumes such code preserves the portable API's SELECT-only invariant.
--
-- @since 0.6.3.0
pgLiftWith :: With Postgres db a -> PgWith db placement a
pgLiftWith = PgWith

-- | Promote a completed nested-safe block for composition with
-- data-modifying CTEs.
--
-- This operation is one-way. In particular, there is no public operation for
-- converting 'PgCteTopLevelOnly' back to 'PgCteNestedAllowed'.
--
-- > pgSelectWithTopLevel $ do
-- >   recursiveRows <- pgToTopLevel $ mdo
-- >     rows <- pgSelecting recursiveQuery
-- >     pure rows
-- >   cteDelete table $ \row -> exists_ $ do
-- >     recursiveRow <- reuse recursiveRows
-- >     guard_ (rowId row ==. rowId recursiveRow)
-- >   pure finalQuery
--
-- The promotion changes no SQL. It permits the subsequent modifying CTE, so
-- the complete block has the following form:
--
-- @
-- WITH RECURSIVE "cte0"("res0") AS
--        (SELECT ... UNION ALL SELECT ... FROM "cte0"),
--      "cte1" AS
--        (DELETE FROM "table"
--         WHERE EXISTS (SELECT ... FROM "cte0"))
-- SELECT ...
-- @
--
-- @since 0.6.3.0
pgToTopLevel
  :: PgWith db 'PgCteNestedAllowed a
  -> PgWith db 'PgCteTopLevelOnly a
pgToTopLevel (PgWith with) = PgWith with

-- | PostgreSQL's materialization policy for a SELECT CTE.
--
-- Explicit materialization control is available in PostgreSQL 12 and later.
-- 'PgCteDefault' emits no modifier and therefore retains PostgreSQL's normal
-- planner behaviour and compatibility with earlier server versions.
-- See PostgreSQL's
-- <https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-CTE-MATERIALIZATION CTE materialization documentation>.
--
-- @since 0.6.3.0
data PgCteMaterialization
  = PgCteDefault
    -- ^ Let PostgreSQL decide whether to fold or materialize the CTE.
  | PgCteMaterialized
    -- ^ Emit @MATERIALIZED@, requesting separate calculation of the CTE. This
    -- can act as an optimization fence or prevent duplicated computation.
  | PgCteNotMaterialized
    -- ^ Emit @NOT MATERIALIZED@, allowing the CTE and parent query to be
    -- optimized together. PostgreSQL ignores this for recursive or
    -- non-side-effect-free queries.
  deriving (Eq, Show)

-- | Introduce a SELECT query as a reusable PostgreSQL CTE using the server's
-- default materialization policy.
--
-- This is the usual PostgreSQL-specific counterpart of 'selecting'. Use
-- 'pgSelectingWith' when the planner boundary should be controlled explicitly.
--
-- > rows <- pgSelecting sourceQuery
-- > pure (reuse rows)
--
-- With a top-level SELECT consumer this produces:
--
-- @
-- WITH "cte0"("res0", "res1") AS (SELECT ...)
-- SELECT "t0"."res0", "t0"."res1" FROM "cte0" AS "t0"
-- @
--
-- @since 0.6.3.0
pgSelecting
  :: ( Projectible Postgres res
     , ThreadRewritable CTE.QAnyScope res )
  => Q Postgres db CTE.QAnyScope res
  -> PgWith db placement (ReusableQ Postgres db res)
pgSelecting = pgSelectingWith PgCteDefault

-- | Introduce a SELECT query as a reusable PostgreSQL CTE with an explicit
-- materialization policy.
--
-- For example:
--
-- > expensive <- pgSelectingWith PgCteMaterialized expensiveQuery
-- > pure $ do
-- >   left <- reuse expensive
-- >   right <- reuse expensive
-- >   guard_ (leftId left ==. rightId right)
-- >   pure (left, right)
--
-- With 'pgSelectWithTopLevel', this produces a statement shaped like:
--
-- @
-- WITH "cte0"("res0", "res1") AS MATERIALIZED (SELECT ...)
-- SELECT ...
-- FROM "cte0" AS "t0" CROSS JOIN "cte0" AS "t1"
-- WHERE "t0"."res0" = "t1"."res0"
-- @
--
-- @NOT MATERIALIZED@ may allow restrictions in the parent query to reach the
-- CTE, but may also duplicate its computation when it is referenced more than
-- once. PostgreSQL ignores @NOT MATERIALIZED@ when folding would not be
-- semantically valid, for example for a recursive query or a query containing
-- volatile functions.
--
-- A projection with no fields is represented by omitting the CTE column-alias
-- list. PostgreSQL then treats the CTE as a degree-zero relation: it has no
-- columns, but it retains the row cardinality of @query@. For example, a query
-- which produces two empty rows has the following shape:
--
-- @
-- WITH "cte0" AS MATERIALIZED (SELECT FROM ...)
-- SELECT FROM "cte0" AS "t0"
-- @
--
-- Reusing such a CTE remains meaningful in joins, @EXISTS@, and aggregates
-- even though no value can be projected from an individual row.
--
-- @since 0.6.3.0
pgSelectingWith
  :: forall res db placement
   . ( Projectible Postgres res
     , ThreadRewritable CTE.QAnyScope res )
  => PgCteMaterialization
  -> Q Postgres db CTE.QAnyScope res
  -> PgWith db placement (ReusableQ Postgres db res)
pgSelectingWith materialization q = do
  tblNm <- pgRegisterCte $ \name ->
    let (_ :: res, fields) = mkFieldNames @Postgres (qualifiedField name)
        body = fromPgSelect (buildSqlQuery (name <> "_") q)
    in case nonEmpty fields of
         Nothing -> pgCteSyntax name Nothing materialization body
         Just fields' -> pgOutputCteSyntax name fields' materialization body
  pure (CTE.reusableForCTE tblNm)

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

-- | Introduce a PostgreSQL @INSERT@ statement as a side-effect-only CTE.
--
-- The CTE has no @RETURNING@ clause and therefore produces no reusable
-- relation; PostgreSQL nevertheless executes it exactly once and to
-- completion when the surrounding top-level statement executes.
--
-- > pgSelectWithTopLevel $ do
-- >   cteInsert users (insertValues [newUser]) onConflictDefault
-- >   pure finalQuery
--
-- This produces SQL shaped like:
--
-- @
-- WITH cte0 AS (INSERT INTO users ...)
-- SELECT ...
-- @
--
-- Empty insert values register no CTE. The result is still conservatively
-- 'PgCteTopLevelOnly', because the placement index cannot vary with the
-- supplied values.
--
-- @since 0.6.3.0
cteInsert
  :: DatabaseEntity Postgres db (TableEntity table)
  -> SqlInsertValues Postgres (table (QExpr Postgres s))
  -> PgInsertOnConflict table
  -> PgWith db 'PgCteTopLevelOnly ()
cteInsert table values onConflict_ =
  case insert table values onConflict_ of
    SqlInsertNoRows -> pure ()
    SqlInsert _ (PgInsertSyntax syntax) -> pgDataModifyingCte_ syntax

-- | Introduce a PostgreSQL @INSERT ... RETURNING@ statement as a
-- data-modifying common table expression. The returned value can be used in a
-- subsequent query with 'reuse'.
--
-- Returns 'Nothing' when the supplied insert values are empty, because in that
-- case there is no statement or common table expression to reuse.
-- Data-modifying CTEs are restricted to top-level 'PgWith' blocks and cannot
-- be passed to 'pgSelectWithNested'.
--
-- For example, this inserts a row once and makes the rows produced by
-- @RETURNING@ available to the final query:
--
-- > pgSelectWithTopLevel $ do
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
-- WITH "cte0"("res0", ...) AS
--        (INSERT INTO "users" ... RETURNING ...)
-- SELECT ... FROM "cte0" AS "t0"
-- @
--
-- The projection may contain no fields. In that case Beam preserves one
-- degree-zero result row per inserted row. PostgreSQL requires at least one
-- @RETURNING@ expression, so the CTE contains a private boolean sentinel while
-- the final SELECT exposes no columns:
--
-- @
-- WITH "cte0"("res0") AS
--        (INSERT INTO "users" ... RETURNING NULL::boolean)
-- SELECT FROM "cte0" AS "t0"
-- @
--
-- The sentinel is not part of the returned Haskell value. If neither the final
-- statement nor another CTE needs the inserted-row output, prefer 'cteInsert'.
-- It omits @RETURNING@ and produces no reusable result.
--
-- @since 0.6.3.0
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
  -> PgWith db 'PgCteTopLevelOnly (Maybe (ReusableQ Postgres db (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)))
cteInsertReturning table values onConflict_ mkProjection =
  case insertReturning table values onConflict_ (Just mkProjection) of
    PgInsertReturningEmpty -> pure Nothing
    PgInsertReturning syntax ->
      Just <$> pgDataModifyingCte syntax

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

-- | Embed a portable SELECT CTE block within a PostgreSQL subquery.
--
-- For example,
--
-- @
-- SELECT a.column1, b.column2 FROM (WITH RECURSIVE ... ) a JOIN b
-- @
--
-- @beam-core@'s 'selectWith' produces a top-level 'SqlSelect', which cannot be
-- used as a 'Q' value within a join. PostgreSQL accepts a SELECT-only @WITH@
-- query in that subquery position, and 'pgSelectWith' exposes that placement.
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
pgSelectWith :: forall db s res
              . Projectible Postgres res
             => With Postgres db (Q Postgres db s res) -> Q Postgres db s res
pgSelectWith = pgSelectWith_

-- | Embed a nested-safe PostgreSQL-specific CTE block in a query.
--
-- This is the 'PgWith' counterpart of 'pgSelectWith'. It supports
-- 'pgSelectingWith', including explicit materialization, while its placement
-- index rejects data-modifying CTEs because PostgreSQL accepts those only in a
-- @WITH@ clause attached to the top-level statement.
--
-- > select $ pgSelectWithNested $ do
-- >   rows <- pgSelectingWith PgCteMaterialized sourceQuery
-- >   pure (reuse rows)
--
-- This produces a derived table containing the complete nested @WITH@ query:
--
-- @
-- SELECT "t0"."res0", "t0"."res1"
-- FROM (WITH "cte0"("res0", "res1") AS MATERIALIZED (SELECT ...)
--       SELECT "sub_t0"."res0", "sub_t0"."res1"
--       FROM "cte0" AS "sub_t0") AS "t0"("res0", "res1")
-- @
--
-- @since 0.6.3.0
pgSelectWithNested
  :: forall db s res
   . Projectible Postgres res
  => PgWith db 'PgCteNestedAllowed (Q Postgres db s res)
  -> Q Postgres db s res
pgSelectWithNested = pgSelectWith_ . unPgWith

-- Shared implementation for the compatible portable and PostgreSQL-specific
-- nested APIs. Keeping the syntax conversion here avoids evaluating or
-- traversing a PgWith block a second time.
pgSelectWith_
  :: forall db s res
   . Projectible Postgres res
  => With Postgres db (Q Postgres db s res)
  -> Q Postgres db s res
pgSelectWith_ (CTE.With mkQ) =
    let (q, (recursiveness, mctes)) = evalState (runWriterT mkQ) 0
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

-- | Attach a PostgreSQL-specific CTE block to a top-level @SELECT@ statement.
--
-- Unlike 'pgSelectWith', this consumes 'PgWith' and can therefore safely
-- accept data-modifying CTEs. SELECT-only blocks work as well, so callers can
-- use one terminal function while a workflow grows from portable SELECT CTEs
-- to PostgreSQL-specific operations.
--
-- > pgSelectWithTopLevel $ do
-- >   selected <- pgSelecting sourceQuery
-- >   deleted <- cteDeleteReturning target predicate id
-- >   pure $ do
-- >     source <- reuse selected
-- >     removed <- reuse deleted
-- >     pure (source, removed)
--
-- This produces one statement of the following form:
--
-- @
-- WITH "cte0"("res0", "res1") AS (SELECT ...),
--      "cte1"("res0", "res1") AS
--        (DELETE FROM "target" ... RETURNING "id", "value")
-- SELECT ... FROM "cte0" CROSS JOIN "cte1"
-- @
--
-- The complete @WITH ... SELECT ...@ is one 'SqlSelect' and is sent to
-- PostgreSQL in a single round trip.
--
-- @since 0.6.3.0
pgSelectWithTopLevel
  :: Projectible Postgres res
  => PgWith db placement (Q Postgres db QBaseScope res)
  -> SqlSelect Postgres (QExprToIdentity res)
pgSelectWithTopLevel = selectWith . unPgWith

-- | Attach a common-table-expression block to a top-level PostgreSQL
-- @INSERT@ statement.
--
-- Unlike 'pgSelectWithNested', this is a top-level statement consumer and
-- therefore accepts both 'PgCteNestedAllowed' and 'PgCteTopLevelOnly' blocks.
-- The final insert can read reusable rows produced by either SELECT CTEs or
-- data-modifying CTEs:
--
-- > pgInsertWith $ do
-- >   rows <- pgSelecting sourceQuery
-- >   pure $ insert destination (insertFrom (reuse rows)) onConflictDefault
--
-- This produces a statement with the following shape:
--
-- @
-- WITH "cte0"("res0", "res1") AS (SELECT ...)
-- INSERT INTO "destination"("id", "value")
-- SELECT "t0"."res0", "t0"."res1" FROM "cte0" AS "t0"
-- @
--
-- If the final insert has no rows, the result remains 'SqlInsertNoRows'. There
-- is then no terminal statement to which PostgreSQL could attach the @WITH@
-- block, so none of its CTE bodies are executed.
--
-- Apply 'returning' to the resulting 'SqlInsert' when the terminal statement
-- should return rows.
--
-- @since 0.6.3.0
pgInsertWith
  :: PgWith db placement (SqlInsert Postgres table)
  -> SqlInsert Postgres table
pgInsertWith with =
  case runPgWith with of
    (SqlInsertNoRows, _, _) -> SqlInsertNoRows
    (SqlInsert settings (PgInsertSyntax statement), recursiveness, ctes) ->
      SqlInsert settings (PgInsertSyntax (pgWithSyntax recursiveness ctes statement))

-- | Attach a common-table-expression block to a top-level PostgreSQL
-- @UPDATE@ statement.
--
-- Reusable CTE rows can be referenced from the final update predicate, for
-- example through 'exists_':
--
-- > pgUpdateWith $ do
-- >   wanted <- pgSelecting wantedUsers
-- >   pure $ update users
-- >     (\user -> userEnabled user <-. val_ False)
-- >     (\user -> exists_ $ do
-- >        candidate <- reuse wanted
-- >        guard_ (userId user ==. userId candidate))
--
-- This produces SQL of the following form:
--
-- @
-- WITH "cte0"("res0") AS (SELECT ... AS "res0")
-- UPDATE "users" SET "enabled"=FALSE
-- WHERE EXISTS
--   (SELECT "t0"."res0" FROM "cte0" AS "t0"
--    WHERE "id" = "t0"."res0")
-- @
--
-- An identity update remains 'SqlIdentityUpdate'; as with an empty insert,
-- there is no terminal PostgreSQL statement and the accumulated CTEs are not
-- executed.
--
-- Apply 'returning' to the resulting 'SqlUpdate' when the terminal statement
-- should return rows.
--
-- @since 0.6.3.0
pgUpdateWith
  :: PgWith db placement (SqlUpdate Postgres table)
  -> SqlUpdate Postgres table
pgUpdateWith with =
  case runPgWith with of
    (SqlIdentityUpdate, _, _) -> SqlIdentityUpdate
    (SqlUpdate settings (PgUpdateSyntax statement), recursiveness, ctes) ->
      SqlUpdate settings (PgUpdateSyntax (pgWithSyntax recursiveness ctes statement))

-- | Attach a common-table-expression block to a top-level PostgreSQL
-- @DELETE@ statement.
--
-- > pgDeleteWith $ do
-- >   expired <- pgSelecting expiredUsers
-- >   pure $ delete users $ \user -> exists_ $ do
-- >     candidate <- reuse expired
-- >     guard_ (userId user ==. userId candidate)
--
-- This produces SQL of the following form:
--
-- @
-- WITH "cte0"("res0") AS (SELECT ... AS "res0")
-- DELETE FROM "users" AS "delete_target"
-- WHERE EXISTS
--   (SELECT "t0"."res0" FROM "cte0" AS "t0"
--    WHERE "delete_target"."id" = "t0"."res0")
-- @
--
-- Since 'SqlDelete' always contains a statement, the accumulated CTE block is
-- always preserved.
-- Apply 'returning' to the result when the terminal statement should return
-- deleted rows.
--
-- @since 0.6.3.0
pgDeleteWith
  :: PgWith db placement (SqlDelete Postgres table)
  -> SqlDelete Postgres table
pgDeleteWith with =
  case runPgWith with of
    (SqlDelete settings (PgDeleteSyntax statement), recursiveness, ctes) ->
      SqlDelete settings (PgDeleteSyntax (pgWithSyntax recursiveness ctes statement))

-- Allocate a name and append one PostgreSQL CTE definition to beam-core's
-- existing writer. All PgWith constructors use this path so lifted and native
-- actions share one monotonically increasing name supply.
pgRegisterCte
  :: (Text -> PgCommonTableExpressionSyntax)
  -> PgWith db placement Text
pgRegisterCte mkCte = PgWith . CTE.With $ do
  cteId <- get
  put (cteId + 1)

  let tblNm = fromString ("cte" ++ show cteId)
  tell (CTE.Nonrecursive, [mkCte tblNm])
  pure tblNm

-- Construct a reusable CTE with a statically non-empty physical output. Keeping
-- the invariant in the type prevents callers from accidentally rendering the
-- invalid PostgreSQL spelling @name() AS (...)@.
pgOutputCteSyntax
  :: Text
  -> NonEmpty Text
  -> PgCteMaterialization
  -> PgSyntax
  -> PgCommonTableExpressionSyntax
pgOutputCteSyntax name fields materialization body =
  pgCteSyntax name (Just fields) materialization body

-- Render the common outer shape for SELECT, returning DML, and
-- side-effect-only DML CTEs. A missing column list is used for degree-zero
-- SELECT CTEs and for modifying statements without @RETURNING@. Materialization
-- is deliberately passed as PgCteDefault for every DML caller: PostgreSQL's
-- materialization controls apply to SELECT CTE folding, while modifying CTEs
-- always execute exactly once and to completion.
pgCteSyntax
  :: Text
  -> Maybe (NonEmpty Text)
  -> PgCteMaterialization
  -> PgSyntax
  -> PgCommonTableExpressionSyntax
pgCteSyntax name fields materialization body =
  PgCommonTableExpressionSyntax $
    pgQuotedIdentifier name <>
    maybe mempty
      (pgParens . pgSepBy (emit ",") . map pgQuotedIdentifier . NonEmpty.toList)
      fields <>
    emit " AS" <>
    materializationSyntax materialization <>
    emit " " <>
    pgParens body
  where
    materializationSyntax PgCteDefault = mempty
    materializationSyntax PgCteMaterialized = emit " MATERIALIZED"
    materializationSyntax PgCteNotMaterialized = emit " NOT MATERIALIZED"

-- Register a modifying CTE with @RETURNING@ output and construct the reusable
-- relation which refers to its generated name.
--
-- PostgreSQL requires @RETURNING@ to contain at least one expression. The
-- existing INSERT, UPDATE, and DELETE returning renderers end in the keyword
-- and a space when Beam's logical projection has no fields. In that case this
-- CTE-specific path appends one private, constant boolean expression and gives
-- it the physical name @res0@. 'CTE.reusableForCTE' is still instantiated at
-- the original zero-field result type, so final Beam SELECTs project no
-- physical columns and the sentinel is never exposed to result decoding.
--
-- One sentinel row is produced for every affected row. Consequently the
-- degree-zero relation preserves the modifying statement's cardinality when it
-- is reused by joins, @EXISTS@, or aggregates.
pgDataModifyingCte
  :: forall res db
   . ( Projectible Postgres res
     , ThreadRewritable CTE.QAnyScope res )
  => PgSyntax
  -> PgWith db 'PgCteTopLevelOnly (ReusableQ Postgres db res)
pgDataModifyingCte body = do
  tblNm <- pgRegisterCte $ \name ->
    let (_ :: res, fields) = mkFieldNames @Postgres (qualifiedField name)
    in case nonEmpty fields of
         Nothing ->
           pgOutputCteSyntax
             name
             ("res0" :| [])
             PgCteDefault
             (body <> emit "NULL::boolean")
         Just fields' -> pgOutputCteSyntax name fields' PgCteDefault body
  pure (CTE.reusableForCTE tblNm)

-- Register a modifying CTE without @RETURNING@. PostgreSQL executes the body,
-- but the CTE forms no temporary table and therefore has no result which can be
-- passed to 'reuse'. This accounts for both the unit result and the absence of
-- a column-alias list.
pgDataModifyingCte_
  :: PgSyntax
  -> PgWith db 'PgCteTopLevelOnly ()
pgDataModifyingCte_ body = do
  _ <- pgRegisterCte $ \name ->
    pgCteSyntax name Nothing PgCteDefault body
  pure ()

-- Evaluate a PostgreSQL CTE builder once and retain the information required
-- by each top-level statement consumer. Keeping this helper local ensures that
-- the backend-independent CTE API does not acquire PostgreSQL command types.
runPgWith
  :: PgWith db placement a
  -> (a, PgCteRecursiveness, [BeamSql99BackendCTESyntax Postgres])
runPgWith (PgWith with) =
  let (result, (recursiveness, ctes)) =
        evalState (runWriterT (CTE.runWith with)) 0
      pgRecursiveness = case recursiveness of
        CTE.Nonrecursive -> PgCteNonrecursive
        CTE.Recursive -> PgCteRecursive
  in (result, pgRecursiveness, ctes)

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

-- | Introduce a PostgreSQL @UPDATE@ statement as a side-effect-only CTE.
--
-- Since no @RETURNING@ clause is emitted, the result is @()@ and cannot be
-- passed to 'reuse'. PostgreSQL still executes the update exactly once when
-- the surrounding top-level statement executes:
--
-- > pgDeleteWith $ do
-- >   cteUpdate users
-- >     (\user -> userActive user <-. val_ False)
-- >     (\user -> userLastSeen user <. val_ cutoff)
-- >   pure (delete sessions expiredSession)
--
-- This produces one side-effect-only definition before the terminal delete:
--
-- @
-- WITH "cte0" AS
--        (UPDATE "users" SET "active"=FALSE WHERE "last_seen" < ...)
-- DELETE FROM "sessions" AS "delete_target" WHERE ...
-- @
--
-- An identity assignment registers no CTE. As with 'cteInsert', its type
-- remains 'PgCteTopLevelOnly' independently of that value-level result.
--
-- @since 0.6.3.0
cteUpdate
  :: DatabaseEntity Postgres db (TableEntity table)
  -> (forall s. table (QField s) -> QAssignment Postgres s)
  -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
  -> PgWith db 'PgCteTopLevelOnly ()
cteUpdate table@(DatabaseEntity (DatabaseTable {})) mkAssignments mkWhere =
  case update table mkAssignments mkWhere of
    SqlIdentityUpdate -> pure ()
    SqlUpdate _ (PgUpdateSyntax syntax) -> pgDataModifyingCte_ syntax

-- | Introduce a PostgreSQL @UPDATE ... RETURNING@ statement as a
-- data-modifying common table expression. The returned value can be used in a
-- subsequent query with 'reuse'.
--
-- Returns 'Nothing' when the assignments form an identity update, because in
-- that case there is no statement or common table expression to reuse.
-- Data-modifying CTEs are restricted to top-level 'PgWith' blocks and cannot
-- be used with 'pgSelectWithNested'.
--
-- > pgSelectWithTopLevel $ do
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
-- through the reusable CTE name:
--
-- @
-- WITH "cte0"("res0", "res1") AS
--        (UPDATE "users" SET "enabled"=FALSE
--         WHERE "id" = ... RETURNING "id", "enabled")
-- SELECT "t0"."res0", "t0"."res1" FROM "cte0" AS "t0"
-- @
--
-- As with 'cteInsertReturning', a projection containing no fields is supported.
-- Beam emits @RETURNING NULL::boolean@ inside the CTE and @SELECT FROM "cte0"@
-- outside it, retaining one zero-field row per updated row without exposing the
-- private sentinel. If neither the final statement nor another CTE needs the
-- updated-row output, use 'cteUpdate' instead.
--
-- @since 0.6.3.0
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
  -> PgWith db 'PgCteTopLevelOnly (Maybe (ReusableQ Postgres db (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)))
cteUpdateReturning table mkAssignments mkWhere mkProjection =
  case updateReturning table mkAssignments mkWhere mkProjection of
    PgUpdateReturningEmpty -> pure Nothing
    PgUpdateReturning syntax ->
      Just <$> pgDataModifyingCte syntax

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

-- | Introduce a PostgreSQL @DELETE@ statement as a side-effect-only CTE.
--
-- The deletion executes exactly once even when the terminal statement does not
-- refer to it. Without @RETURNING@ it produces no reusable relation:
--
-- > pgInsertWith $ do
-- >   cteDelete stagingRows isExpired
-- >   pure (insert archive newRows onConflictDefault)
--
-- This renders a definition without an empty column list, followed by the
-- terminal insert:
--
-- @
-- WITH "cte0" AS
--        (DELETE FROM "staging_rows" AS "delete_target" WHERE ...)
-- INSERT INTO "archive" ...
-- @
--
-- Sibling modifying CTEs use the same PostgreSQL snapshot and cannot observe
-- one another's table changes. Use their @RETURNING@ output when one operation
-- needs to communicate rows to another.
--
-- @since 0.6.3.0
cteDelete
  :: DatabaseEntity Postgres db (TableEntity table)
  -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
  -> PgWith db 'PgCteTopLevelOnly ()
cteDelete table mkWhere =
  case delete table (\row -> mkWhere row) of
    SqlDelete _ (PgDeleteSyntax syntax) -> pgDataModifyingCte_ syntax

-- | Introduce a PostgreSQL @DELETE ... RETURNING@ statement as a
-- data-modifying common table expression. The returned value can be used in a
-- subsequent query with 'reuse'.
--
-- Data-modifying CTEs are restricted to top-level 'PgWith' blocks and cannot
-- be used with 'pgSelectWithNested'.
--
-- Unlike insert and update, delete always has a statement to introduce, so no
-- 'Maybe' is required:
--
-- > pgSelectWithTopLevel $ do
-- >   deleted <- cteDeleteReturning
-- >     users
-- >     (\user -> userExpired user ==. val_ True)
-- >     id
-- >   pure (reuse deleted)
--
-- The corresponding SQL has the following form:
--
-- @
-- WITH "cte0"("res0", "res1") AS
--        (DELETE FROM "users" AS "delete_target"
--         WHERE "delete_target"."expired" = TRUE
--         RETURNING "id", "expired")
-- SELECT "t0"."res0", "t0"."res1" FROM "cte0" AS "t0"
-- @
--
-- The final query observes the deleted rows through @DELETE ... RETURNING@.
-- This is also the supported way to communicate between data-modifying CTEs,
-- since PostgreSQL executes sibling statements against the same snapshot.
-- A projection containing no fields is also reusable: Beam emits a private
-- @NULL::boolean@ returning expression and an outer zero-column SELECT, so its
-- row count still equals the number of deleted rows. If neither the final
-- statement nor another CTE needs the deleted-row output, use 'cteDelete'
-- instead.
--
-- @since 0.6.3.0
cteDeleteReturning
  :: ( Projectible Postgres a
     , ThreadRewritable PostgresInaccessible a
     , Projectible Postgres (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)
     , ThreadRewritable CTE.QAnyScope (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a)
     )
  => DatabaseEntity Postgres db (TableEntity table)
  -> (forall s. table (QExpr Postgres s) -> QExpr Postgres s Bool)
  -> (table (QExpr Postgres PostgresInaccessible) -> a)
  -> PgWith db 'PgCteTopLevelOnly (ReusableQ Postgres db (WithRewrittenThread PostgresInaccessible CTE.QAnyScope a))
cteDeleteReturning table mkWhere mkProjection =
  let PgDeleteReturning syntax = deleteReturning table mkWhere mkProjection
  in pgDataModifyingCte syntax

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
