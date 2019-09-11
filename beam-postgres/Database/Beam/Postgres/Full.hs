{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , runPgInsertReturningList

  , PgInsertReturning(..)

  -- ** Specifying conflict actions

  , PgInsertOnConflict(..), PgInsertOnConflictTarget(..)
  , PgConflictAction(..)

  , onConflictDefault, onConflict
  , conflictingConstraint
  , onConflictUpdateSet
  , onConflictUpdateSetWhere, onConflictUpdateInstead

  -- * @UPDATE RETURNING@
  , PgUpdateReturning(..)
  , runPgUpdateReturningList
  , updateReturning

  -- * @DELETE RETURNING@
  , PgDeleteReturning(..)
  , runPgDeleteReturningList
  , deleteReturning

  -- * Generalized @RETURNING@
  , PgReturning(..)
  ) where

import           Database.Beam hiding (insert, insertValues)
import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Schema.Tables

import           Database.Beam.Postgres.Types
import           Database.Beam.Postgres.Syntax

import           Control.Monad.Free.Church

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
-- <http://tathougies.github.io/beam/user-guide/backends/beam-postgres/ the user guide> for usage
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
newtype PgInsertOnConflict (tbl :: (* -> *) -> *) =
    PgInsertOnConflict (tbl (QField QInternal) -> PgInsertOnConflictSyntax)

-- | Specifies the kind of constraint that must be violated for the action to occur
newtype PgInsertOnConflictTarget (tbl :: (* -> *) -> *) =
    PgInsertOnConflictTarget (tbl (QExpr Postgres QInternal) -> PgInsertOnConflictTargetSyntax)

-- | A description of what to do when a constraint or index is violated.
newtype PgConflictAction (tbl :: (* -> *) -> *) =
    PgConflictAction (tbl (QField QInternal) -> PgConflictActionSyntax)

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

-- | Perform the action only if the given named constraint is violated
conflictingConstraint :: T.Text -> PgInsertOnConflictTarget tbl
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
    SqlDelete _ pgDelete = delete table mkWhere
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings

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
  type PgReturningType cmd :: * -> *

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
