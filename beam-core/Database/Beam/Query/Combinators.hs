{-# LANGUAGE UndecidableInstances, FunctionalDependencies, TypeApplications #-}
module Database.Beam.Query.Combinators
    ( all_, join_, guard_, related_, relatedBy_, lookup_
    , leftJoin_, perhapsAll_
    , SqlReferences(..)
    , SqlJustable(..)
    , SqlDeconstructMaybe(..)
    , SqlOrderable

    , limit_, offset_

    , exists_

    , (<.), (>.), (<=.), (>=.), (==.), (&&.), (||.), not_, div_, mod_
    , HaskellLiteralForQExpr, SqlValable(..)

    -- * SQL GROUP BY and aggregation
--    , aggregate, SqlGroupable(..)
--    , sum_, count_

    -- * SQL ORDER BY
    , orderBy, asc_, desc_


    -- * SQL subqueries
--    , subquery_
    ) where

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL92

import Database.Beam.Query.Internal
import Database.Beam.Query.Types

import Database.Beam.Schema.Tables
import Database.Beam.SQL

import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Identity

import Data.Monoid
import Data.String
import Data.Maybe
import Data.Proxy
import Data.Text (Text, unpack)
import Data.Coerce

import GHC.Generics

instance Sql92Syntax syntax => IsString (QExpr syntax be s Text) where
    fromString = QExpr . valueE (Proxy @syntax) . stringV (Proxy @syntax)
instance (Num a, Sql92Syntax syntax) => Num (QExpr syntax be s a) where
    fromInteger x = let res :: QExpr syntax be s a
                        res = QExpr (valueE (Proxy @syntax) (numericV (Proxy @syntax) x))
                    in res
    QExpr a + QExpr b = QExpr (addE (Proxy @syntax) a b)
    QExpr a - QExpr b = QExpr (subE (Proxy @syntax) a b)
    QExpr a * QExpr b = QExpr (mulE (Proxy @syntax) a b)
    negate (QExpr a) = QExpr (negateE (Proxy @syntax) a)
    abs (QExpr x) = QExpr (absE (Proxy @syntax) x)
    signum x = error "signum: not defined for QExpr. Use CASE...WHEN"
-- instance (BeamBackend be, FromBackendLiteral be String) => IsString (Aggregation be s Text) where
--     fromString = ProjectAgg . SQLValE . SQLValue
-- instance (Num a, BeamBackend be, FromBackendLiteral be Integer) => Num (Aggregation be s a) where
--     fromInteger x = ProjectAgg (SQLValE (SQLValue x))
--     ProjectAgg a + ProjectAgg b = ProjectAgg (SQLBinOpE "+" a b)
--     ProjectAgg a - ProjectAgg b = ProjectAgg (SQLBinOpE "-" a b)
--     ProjectAgg a * ProjectAgg b = ProjectAgg (SQLBinOpE "*" a b)
--     negate (ProjectAgg a) = ProjectAgg (SQLUnOpE "-" a)
--     abs (ProjectAgg x) = ProjectAgg (SQLFuncE "ABS" [x])
--     signum x = error "signum: not defined for Aggregation. Use CASE...WHEN"

-- | Introduce all entries of a table into the 'Q' monad
all_ :: forall be db table syntax s.
        ( Database db, Sql92Syntax syntax, Table table )
       => DatabaseTable be db table -> Q syntax be db s (table (QExpr syntax be s))
all_ tbl = buildJoin tbl Nothing

-- | Introduce all entries of a table into the 'Q' monad based on the given SQLExprbuildJoin :: forall db syntax table be s.
join_ :: ( Database db, Sql92Syntax syntax, Table table ) =>
         DatabaseTable be db table -> QExpr syntax be s Bool
      -> Q syntax be db s (table (QExpr syntax be s))
join_ tbl on = buildJoin tbl (Just on)

buildJoin :: forall db syntax table be s.
         ( Database db, Sql92Syntax syntax, Table table ) =>
         DatabaseTable be db table -> Maybe (QExpr syntax be s Bool)
      -> Q syntax be db s (table (QExpr syntax be s))
buildJoin (DatabaseTable table name tableSettings :: DatabaseTable be db table) on =
    do curTbl <- gets qbNextTblRef
       modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                 , qbFrom = from
                                 , qbWhere = where_ } ->
           let (from', where') = case from of
                                   Nothing -> ( Just newSource
                                              , case on of
                                                  Nothing -> where_
                                                  Just (QExpr on') -> andE (Proxy @syntax) where_ on')
                                   Just from -> ( Just (innerJoin (Proxy @syntax) from newSource $
                                                        case on of
                                                          Nothing -> Nothing
                                                          Just (QExpr on) -> Just on)
                                                , where_ )
               newSource = fromTable (Proxy @syntax)
                               (tableNamed (Proxy @syntax) name)
                               (Just (fromString ("t" <> show curTbl)))
           in qb { qbNextTblRef = curTbl + 1
                 , qbFrom = from'
                 , qbWhere = where' }

       let mkScopedField :: Columnar' (TableField be table) a -> Columnar' (QExpr syntax be s) a
           mkScopedField (Columnar' f) = Columnar' (QExpr (qualifiedFieldE (Proxy @syntax) (fromString ("t" <> show curTbl)) (_fieldName f)))
       pure (changeBeamRep mkScopedField tableSettings)

-- | Introduce a table using a left join. Because this is not an inner join, the resulting table is
-- made nullable. This means that each field that would normally have type 'QExpr x' will now have
-- type 'QExpr (Maybe x)'.
leftJoin_ ::
  forall be db table syntax s.
  ( Sql92Syntax syntax, Database db, Table table ) =>
  DatabaseTable be db table -> QExpr syntax be s Bool -> Q syntax be db s (table (Nullable (QExpr syntax be s)))
leftJoin_ (DatabaseTable table name tableSettings :: DatabaseTable be db table) (QExpr on) =
    do curTbl <- gets qbNextTblRef
       modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                 , qbFrom = from } ->
                let from' = case from of
                              Nothing -> error "leftJoin_: empty select source"
                              Just from -> leftJoin (Proxy @syntax) from newSource (Just on)
                    newSource = fromTable (Proxy @syntax)
                                    (tableNamed (Proxy @syntax) name)
                                    (Just (fromString ("t" <> show curTbl)))
                in qb { qbNextTblRef = curTbl + 1
                      , qbFrom = Just from' }

       let mkScopedField :: Columnar' (TableField be table) a -> Columnar' (Nullable (QExpr syntax be s)) a
           mkScopedField (Columnar' f) = Columnar' (QExpr (qualifiedFieldE (Proxy @syntax) (fromString ("t" <> show curTbl)) (_fieldName f)))
       pure (changeBeamRep mkScopedField tableSettings)

-- | Only allow results for which the 'QExpr' yields 'True'
guard_ :: forall syntax be db s.
  Sql92Syntax syntax =>
  QExpr syntax be s Bool -> Q syntax be db s ()
guard_ (QExpr guardE') = modify $ \qb@QueryBuilder { qbWhere = guardE } ->
  qb { qbWhere = andE (Proxy @syntax) guardE guardE' }

-- | Introduce all entries of the given table which are referenced by the given 'PrimaryKey'
related_ :: ( Sql92Syntax syntax, Database db, Table rel )
            => DatabaseTable be db rel -> PrimaryKey rel (QExpr syntax be s)
         -> Q syntax be db s (rel (QExpr syntax be s))
related_ (relTbl :: DatabaseTable be db rel) pk =
    mdo rel <- join_ relTbl (pk ==. primaryKey rel)
        pure rel

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_ :: ( Database db, Table rel, Sql92Syntax syntax )
             => DatabaseTable be db rel -> (rel (QExpr syntax be s) -> QExpr syntax be s Bool) -> Q syntax be db s (rel (QExpr syntax be s))
relatedBy_ (relTbl :: DatabaseTable be db rel) mkOn =
    mdo rel <- join_ relTbl (mkOn rel)
        pure rel

-- | Introduce related entries of the given table, or if no related entries exist, introduce the null table
perhapsAll_ :: ( Database db, Table rel, Sql92Syntax syntax ) =>
               DatabaseTable be db rel -> (rel (Nullable (QExpr syntax be s)) -> QExpr syntax be s Bool)
            -> Q syntax be db s (rel (Nullable (QExpr syntax be s)))
perhapsAll_ relTbl expr =
    mdo rel <- leftJoin_ relTbl (expr rel)
        pure rel

-- | Synonym for 'related_'
lookup_ :: ( Database db, Table rel, Sql92Syntax syntax )
           => DatabaseTable be db rel -> PrimaryKey rel (QExpr syntax be s) -> Q syntax be db s (rel (QExpr syntax be s))
lookup_ = related_

class Sql92Syntax syntax => SqlReferences be syntax f s | f -> syntax, f -> be where
    -- | Check that the 'PrimaryKey' given matches the table. Polymorphic so it works over both
    -- regular tables and those that have been made nullable by 'leftJoin_'.
    references_ :: Table tbl => PrimaryKey tbl f -> tbl f -> QExpr syntax be s Bool
instance Sql92Syntax syntax => SqlReferences be syntax (QExpr syntax be s) s where
    references_ pk (tbl :: tbl (QExpr syntax be s)) = pk ==. primaryKey tbl
instance Sql92Syntax syntax => SqlReferences be syntax (Nullable (QExpr syntax be s)) s where
    references_ pk (tbl :: tbl (Nullable (QExpr syntax be s))) = pk ==. primaryKey tbl

-- | Limit the number of results returned by a query.
--
--   The resulting query is a top-level one that must be passed to 'query', 'queryList', or 'subquery_'. See 'TopLevelQ' for details.
limit_ :: IsQuery q => Integer -> q syntax be db s a -> TopLevelQ syntax be db s a
limit_ limit' q =
    TopLevelQ $
    do res <- toQ q
       modify $ \qb ->
           let qbLimit' = case qbLimit qb of
                            Nothing -> Just limit'
                            Just limit -> Just (min limit limit')
           in qb { qbLimit = qbLimit' }
       pure res

-- | Drop the first `offset'` results.
--
--   The resulting query is a top-level one that must be passed to 'query', 'queryList', or 'subquery_'. See 'TopLevelQ' for details.
offset_ :: IsQuery q => Integer -> q syntax be db s a -> TopLevelQ syntax be db s a
offset_ offset' q =
    TopLevelQ $
    do res <- toQ q
       modify $ \qb ->
           let qbOffset' = case qbOffset qb of
                             Nothing -> Just offset'
                             Just offset -> Just (offset + offset')
           in qb { qbOffset = qbOffset' }
       pure res

-- | Use the SQL exists operator to determine if the given query returns any results
exists_ :: forall q syntax be db s a.
  (IsQuery q, Sql92Syntax syntax, Projectible syntax a) => q syntax be db s a -> QExpr syntax be s Bool
exists_ q = let (_, _, selectCmd) = buildSql92Query (Proxy @syntax) (toQ q) 0
            in QExpr (existsE (Proxy @syntax) selectCmd)

-- ** Combinators for boolean expressions

class Sql92Syntax syntax => SqlOrd be syntax a s | a -> syntax, a -> be where
    (==.), (/=.) :: a -> a -> QExpr syntax be s Bool
    a /=. b = not_ (a ==. b)

instance Sql92Syntax syntax => SqlOrd be syntax (QExpr syntax be s a) s where
    (==.) = qBinOpE eqE
    (/=.) = qBinOpE neqE

newtype QExprBool syntax be s a = QExprBool (QExpr syntax be s Bool)

-- instance {-# OVERLAPPING #-} (BeamBackend be, Table tbl) => SqlOrd be (PrimaryKey tbl (QExpr be s)) s where
--     a ==. b = let pkCmp = runIdentity (zipPkM (\(Columnar' x) (Columnar' y) -> return (Columnar' (QExprBool (x ==. y))) ) a b) :: PrimaryKey tbl (QExprBool be s)
--               in foldr (&&.)  (QExpr (SQLValE (sqlBool True))) (pkAllValues (\(Columnar' (QExprBool x)) -> x) pkCmp)
instance ( Sql92Syntax syntax, Beamable tbl ) => SqlOrd be syntax (tbl (QExpr syntax be s)) s where
    a ==. b = let baseE = QExpr (valueE (Proxy @syntax) (trueV (Proxy @syntax)))
                  (_, e) = runState (zipBeamFieldsM
                                     (\x'@(Columnar' x) (Columnar' y) -> do
                                         modify (\expr -> expr &&. x ==. y)
                                         return x') a b) baseE
              in e

-- instance {-# OVERLAPPING #-} (BeamBackend be, Table tbl) => SqlOrd be (PrimaryKey tbl (Nullable (QExpr be s))) s where
--     a ==. b = let pkCmp = runIdentity (zipPkM (\(Columnar' x) (Columnar' y) -> return (Columnar' (QExprBool (x ==. y))) ) a b) :: PrimaryKey tbl (QExprBool be s)
--               in foldr (&&.) (QExpr (SQLValE (sqlBool True))) (pkAllValues (\(Columnar' (QExprBool x)) -> x) pkCmp)
instance ( Sql92Syntax syntax, Beamable tbl)
    => SqlOrd be syntax (tbl (Nullable (QExpr syntax be s))) s where
    a ==. b = let baseE = QExpr (valueE (Proxy @syntax) (trueV (Proxy @syntax)))
                  (_, e) = runState (zipBeamFieldsM
                                      (\x'@(Columnar' x) (Columnar' y) -> do
                                          modify (\expr -> expr &&. x ==. y)
                                          return x') a b) baseE
              in e

qBinOpE :: forall syntax be s a b c. Sql92Syntax syntax =>
           (Proxy syntax -> Sql92ExpressionSyntax syntax -> Sql92ExpressionSyntax syntax -> Sql92ExpressionSyntax syntax)
        -> QExpr syntax be s a -> QExpr syntax be s b -> QExpr syntax be s c
qBinOpE mkOpE (QExpr a) (QExpr b) = QExpr (mkOpE (Proxy @syntax) a b)

(<.), (>.), (<=.), (>=.) ::
  Sql92Syntax syntax => QExpr syntax be s a -> QExpr syntax be s a -> QExpr syntax be s Bool
(<.) = qBinOpE ltE
(>.) = qBinOpE gtE
(<=.) = qBinOpE leE
(>=.) = qBinOpE geE

(&&.), (||.) ::
  Sql92Syntax syntax => QExpr syntax be s Bool -> QExpr syntax be s Bool -> QExpr syntax be s Bool
(&&.) = qBinOpE andE
(||.) = qBinOpE orE

infixr 3 &&.
infixr 2 ||.
infix 4 ==., /=.

not_ :: forall syntax be s.
  Sql92Syntax syntax => QExpr syntax be s Bool -> QExpr syntax be s Bool
not_ (QExpr a) = QExpr (notE (Proxy @syntax) a)

mod_, div_ ::
  (Integral a, Sql92Syntax syntax) =>
  QExpr syntax be s a -> QExpr syntax be s a -> QExpr syntax be s a
div_ = qBinOpE divE
mod_ = qBinOpE modE

-- * Marshalling between Haskell literals and QExprs

type family HaskellLiteralForQExpr x
type instance HaskellLiteralForQExpr (QExpr syntax be s a) = a
type instance HaskellLiteralForQExpr (table (QExpr syntax be s)) = table Identity

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a



-- -- NOTE: This shouldn't cause problems because both overlapping instances are in the same module.
-- --       GHC should prefer the PrimaryKey one for primary keys and the table one for everything else.
-- --       AFAICT, PrimaryKey tbl QExpr ~ tbl QExpr is impossible
-- instance {-# INCOHERENT #-} ( Table tbl
--                              , MakeSqlValues be (PrimaryKey tbl) )
--     => SqlValable (PrimaryKey tbl (QExpr be s)) where

--     val_ = changeBeamRep valToQExpr . makeSqlValues (Proxy :: Proxy be)
--         where valToQExpr :: Columnar' (SqlValue' be) a -> Columnar' (QExpr be s) a
--               valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))
-- instance ( MakeBackendLiterals be tbl, Beamable tbl )
--     => SqlValable (tbl (QExpr syntax be s)) where

--     val_ = changeBeamRep valToQExpr . makeBackendLiterals
--         where valToQExpr :: Columnar' (BackendLiteral' be) a -> Columnar' (QExpr be s) a
--               valToQExpr (Columnar' (BackendLiteral' v)) = Columnar' (QExpr (SQLValE (SQLValue v)))

-- * Aggregators

-- class BeamSqlBackend be => Aggregating be agg s | agg -> s, agg -> be where
--     type LiftAggregationsToQExpr agg s

--     aggToSql :: Proxy s -> agg -> SQLGrouping be
--     liftAggToQExpr :: Proxy s -> agg -> LiftAggregationsToQExpr agg s
-- instance ( Table t, BeamSqlBackend be ) => Aggregating be (t (Aggregation be s)) s where
--     type LiftAggregationsToQExpr (t (Aggregation be s)) s = t (QExpr be s)
--     aggToSql s table = mconcat (allBeamValues (\(Columnar' x) -> aggToSql s x) table)
--     liftAggToQExpr s = changeBeamRep (\(Columnar' x) -> Columnar' (liftAggToQExpr s x))
-- instance BeamSqlBackend be => Aggregating be (Aggregation be s a) s where
--     type LiftAggregationsToQExpr (Aggregation be s a) s = QExpr be s a
--     aggToSql _ (GroupAgg e) = let eSql = optimizeExpr' e
--                               in mempty { sqlGroupBy = [eSql] }
--     aggToSql _ (ProjectAgg _) = mempty

--     liftAggToQExpr _ (GroupAgg e) = QExpr e
--     liftAggToQExpr _ (ProjectAgg e) = QExpr e
-- instance (Aggregating be a s, Aggregating be b s) => Aggregating be (a, b) s where
--     type LiftAggregationsToQExpr (a, b) s = ( LiftAggregationsToQExpr a s
--                                             , LiftAggregationsToQExpr b s)
--     aggToSql s (a, b) = aggToSql s a <> aggToSql s b
--     liftAggToQExpr s (a, b) = (liftAggToQExpr s a, liftAggToQExpr s b)
-- instance (Aggregating be a s, Aggregating be b s, Aggregating be c s) => Aggregating be (a, b, c) s where
--     type LiftAggregationsToQExpr (a, b, c) s = ( LiftAggregationsToQExpr a s
--                                                , LiftAggregationsToQExpr b s
--                                                , LiftAggregationsToQExpr c s )
--     aggToSql s (a, b, c) = aggToSql s a <> aggToSql s b <> aggToSql s c
--     liftAggToQExpr s (a, b, c) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c)
-- instance (Aggregating be a s, Aggregating be b s, Aggregating be c s, Aggregating be d s) => Aggregating be (a, b, c, d) s where
--     type LiftAggregationsToQExpr (a, b, c, d) s = ( LiftAggregationsToQExpr a s
--                                                   , LiftAggregationsToQExpr b s
--                                                   , LiftAggregationsToQExpr c s
--                                                   , LiftAggregationsToQExpr d s)
--     aggToSql s (a, b, c, d) = aggToSql s a <> aggToSql s b <> aggToSql s c <> aggToSql s d
--     liftAggToQExpr s (a, b, c, d) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c, liftAggToQExpr s d)
-- instance (Aggregating be a s, Aggregating be b s, Aggregating be c s, Aggregating be d s, Aggregating be e s) => Aggregating be (a, b, c, d, e) s where
--     type LiftAggregationsToQExpr (a, b, c, d, e) s = ( LiftAggregationsToQExpr a s
--                                                      , LiftAggregationsToQExpr b s
--                                                      , LiftAggregationsToQExpr c s
--                                                      , LiftAggregationsToQExpr d s
--                                                      , LiftAggregationsToQExpr e s)
--     aggToSql s (a, b, c, d, e) = aggToSql s a <> aggToSql s b <> aggToSql s c <> aggToSql s d <> aggToSql s e
--     liftAggToQExpr s (a, b, c, d, e) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c, liftAggToQExpr s d, liftAggToQExpr s e)

-- -- | Type class for things that can be used as the basis of a grouping in a SQL GROUP BY
-- -- clause. This includes 'QExpr a', 'Table's, and 'PrimaryKey's. Because the given object forms the
-- -- basis of the group, its value is available for use in the result set.
-- class SqlGroupable a where
--     type GroupResult a

--     -- | When included in an 'Aggregating' expression, causes the results to be grouped by the
--     -- given column.
--     group_ :: a -> GroupResult a
-- instance SqlGroupable (QExpr be s a) where
--     type GroupResult (QExpr be s a) = Aggregation be s a
--     group_ (QExpr a) = GroupAgg a
-- -- instance {-# OVERLAPPING #-} Table t => SqlGroupable (PrimaryKey t (QExpr be s)) where
-- --     type GroupResult (PrimaryKey t (QExpr be s)) = PrimaryKey t (Aggregation be s)
-- --     group_ = pkChangeRep (\(Columnar' (QExpr e)) -> Columnar' (GroupAgg e))
-- instance Table t => SqlGroupable (t (QExpr be s)) where
--     type GroupResult (t (QExpr be s)) = t (Aggregation be s)
--     group_ = changeBeamRep (\(Columnar' (QExpr e)) -> Columnar' (GroupAgg e))

-- sum_ :: Num a => QExpr be s a -> Aggregation be s a
-- sum_ (QExpr over) = ProjectAgg (SQLFuncE "SUM" [over])

-- count_ :: QExpr be s a -> Aggregation be s Int
-- count_ (QExpr over) = ProjectAgg (SQLFuncE "COUNT" [over])

-- -- | Return a 'TopLevelQ' that will aggregate over the results of the original query. The
-- -- aggregation function (first argument) should accept the output of the query, and return a member
-- -- of the 'Aggregating' class which will become the result of the new query. See the 'group_'
-- -- combinator as well as the various aggregation combinators ('sum_', 'count_', etc.)
-- --
-- -- For example,
-- --
-- -- > aggregate (\employee -> (group_ (_employeeRegion employee), count_ (_employeeId employee))) (all_ employeesTable)
-- --
-- -- will group the result of the `all_ employeesTable` query using the `_employeeRegion` record
-- -- field, and then count up the number of employees for each region.
-- aggregate :: (Projectible be a, Aggregating be agg s) => (a -> agg) -> Q be db s a -> TopLevelQ be db s (LiftAggregationsToQExpr agg s)
-- aggregate (aggregator :: a -> agg) (q :: Q be db s a) =
--     TopLevelQ $
--     do res <- q

--        curTbl <- gets qbNextTblRef
--        let aggregation = aggregator res
--            grouping' = aggToSql (Proxy :: Proxy s) aggregation
--        modify $ \qb -> case sqlGroupBy grouping' of
--                          [] -> qb
--                          _ -> case qbGrouping qb of
--                                 Nothing -> qb { qbGrouping = Just grouping' }
--                                 Just grouping -> qb { qbGrouping = Just (grouping <> grouping') }
--        pure (liftAggToQExpr (Proxy :: Proxy s) aggregation)

-- * Order bys

class SqlOrderable syntax a | a -> syntax where
    makeSQLOrdering :: a -> [Sql92OrderingSyntax syntax]
--instance SqlOrderable syntax (Sql92OrderingSyntax syntax) where
--    makeSQLOrdering x = [x]
instance SqlOrderable syntax a => SqlOrderable syntax [a] where
    makeSQLOrdering = concatMap makeSQLOrdering
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b ) => SqlOrderable syntax (a, b) where
    makeSQLOrdering (a, b) = makeSQLOrdering a <> makeSQLOrdering b
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c ) => SqlOrderable syntax (a, b, c) where
    makeSQLOrdering (a, b, c) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c
         , SqlOrderable syntax d ) => SqlOrderable syntax (a, b, c, d) where
    makeSQLOrdering (a, b, c, d) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c
         , SqlOrderable syntax d
         , SqlOrderable syntax e ) => SqlOrderable syntax (a, b, c, d, e) where
    makeSQLOrdering (a, b, c, d, e) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d <> makeSQLOrdering e

-- | Order by certain expressions, either ascending ('asc_') or descending ('desc_')
orderBy :: (SqlOrderable syntax ordering, IsQuery q) => (a -> ordering) -> q syntax be db s a -> TopLevelQ syntax be db s a
orderBy orderer q =
    TopLevelQ $
    do res <- toQ q
       let ordering = makeSQLOrdering (orderer res)
       modify $ \qb -> qb { qbOrdering = qbOrdering qb <> ordering }
       pure res

desc_, asc_ :: forall syntax be s a.
  Sql92Syntax syntax => QExpr syntax be s a -> Sql92OrderingSyntax syntax
asc_ (QExpr e) = ascOrdering (Proxy @syntax) e
desc_ (QExpr e) = descOrdering (Proxy @syntax) e

-- * Subqueries

class Subqueryable syntax a s | a -> s, a -> syntax where
    type Unnested a s
    subqueryProjections :: Proxy s -> a
                        -> RWS (Text, Int) [Sql92AliasingSyntax syntax (Sql92ExpressionSyntax syntax)] Int
                               (Unnested a s)
instance Sql92Syntax syntax => Subqueryable syntax (QExpr syntax be (QNested s) a) s where
    type Unnested (QExpr syntax be (QNested s) a) s = QExpr syntax be s a
    subqueryProjections s (QExpr e) =
        do i <- state (\i -> (i, i+1))
           (tblName, tblOrd) <- ask
           let fieldName = fromString ("e" <> show i)
           tell [aliasExpr (Proxy @syntax) e (Just fieldName)]
           pure (QExpr (qualifiedFieldE (Proxy @syntax) (fromString ("t" <> show tblOrd)) fieldName))
instance ( Subqueryable be a s
         , Subqueryable be b s ) =>
    Subqueryable be (a, b) s where
    type Unnested (a, b) s = (Unnested a s, Unnested b s)
    subqueryProjections s (a, b) =
        (,) <$> subqueryProjections s a
            <*> subqueryProjections s b
instance ( Subqueryable be a s
         , Subqueryable be b s
         , Subqueryable be c s ) =>
    Subqueryable be (a, b, c) s where
    type Unnested (a, b, c) s = (Unnested a s, Unnested b s, Unnested c s)
    subqueryProjections s (a, b, c) =
        (,,) <$> subqueryProjections s a
             <*> subqueryProjections s b
             <*> subqueryProjections s c
instance ( Subqueryable be a s
         , Subqueryable be b s
         , Subqueryable be c s
         , Subqueryable be d s ) =>
    Subqueryable be (a, b, c, d) s where
    type Unnested (a, b, c, d) s = (Unnested a s, Unnested b s, Unnested c s, Unnested d s)
    subqueryProjections s (a, b, c, d) =
        (,,,) <$> subqueryProjections s a
              <*> subqueryProjections s b
              <*> subqueryProjections s c
              <*> subqueryProjections s d
instance ( Subqueryable be a s
         , Subqueryable be b s
         , Subqueryable be c s
         , Subqueryable be d s
         , Subqueryable be e s ) =>
    Subqueryable be (a, b, c, d, e) s where
    type Unnested (a, b, c, d, e) s = (Unnested a s, Unnested b s, Unnested c s, Unnested d s, Unnested e s)
    subqueryProjections s (a, b, c, d, e) =
        (,,,,) <$> subqueryProjections s a
               <*> subqueryProjections s b
               <*> subqueryProjections s c
               <*> subqueryProjections s d
               <*> subqueryProjections s e

-- -- | Run the given 'Q'-like object as a subquery, joining the results with the current result
-- -- set. This allows embedding of 'TopLevelQ's inside 'Q's or other 'TopLevelQ's.
-- subquery_ :: (IsQuery q, Projectible be a, Subqueryable be a s, BeamSqlBackend be) => q be db (QNested s) a -> Q be db s (Unnested a s)
-- subquery_ (q :: q be db (QNested s) a) =
--     do curTbl <- gets qbNextTblRef

--        let (res, curTbl', select') = queryToSQL' (toQ q) curTbl

--            subTblName = fromString ("t" <> show curTbl)
--            (res', projection') = evalRWS (subqueryProjections (Proxy :: Proxy s) res) (subTblName, curTbl') 0

--            select'' = select' { selProjection = SQLProj projection' }

--        modify $ \qb@QueryBuilder { qbFrom = from } ->
--                  let from' = case from of
--                                Nothing -> Just newSource
--                                Just from -> Just (SQLJoin SQLInnerJoin from newSource (SQLValE (SQLValue True)))
--                      newSource = SQLFromSource (SQLAliased (SQLSourceSelect select'') (Just (fromString ("t" <> show curTbl'))))
--                  in qb { qbNextTblRef = curTbl' + 1
--                        , qbFrom = from' }

--        pure res'

-- * Nullable conversions

-- | Type class for things that can be nullable. This includes 'QExpr (Maybe a)', 'tbl (Nullable
-- QExpr)', and 'PrimaryKey tbl (Nullable QExpr)'
class SqlJustable a b | b -> a where

    -- | Given something of type 'QExpr a', 'tbl QExpr', or 'PrimaryKey tbl QExpr', turn it into a
    -- 'QExpr (Maybe a)', 'tbl (Nullable QExpr)', or 'PrimaryKey t (Nullable QExpr)' respectively
    -- that contains the same values.
    just_ :: a -> b

    -- | Return either a 'QExpr (Maybe x)' representing 'Nothing' or a nullable 'Table' or
    -- 'PrimaryKey' filled with 'Nothing'.
    nothing_ :: b

instance Sql92Syntax syntax => SqlJustable (QExpr syntax be s a) (QExpr syntax be s (Maybe a)) where
    just_ (QExpr e) = QExpr e
    nothing_ = QExpr (valueE (Proxy @syntax) (nullV (Proxy @syntax)))

instance {-# OVERLAPPING #-} ( Table t, Sql92Syntax syntax ) => SqlJustable (PrimaryKey t (QExpr syntax be s)) (PrimaryKey t (Nullable (QExpr syntax be s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' nothing_) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} ( Table t, Sql92Syntax syntax) => SqlJustable (t (QExpr syntax be s)) (t (Nullable (QExpr syntax be s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' nothing_) (tblSkeleton :: TableSkeleton t)

instance {-# OVERLAPPING #-} Table t => SqlJustable (PrimaryKey t Identity) (PrimaryKey t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' Nothing) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} Table t => SqlJustable (t Identity) (t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' q) -> Columnar' Nothing) (tblSkeleton :: TableSkeleton t)

-- * Nullable checking

-- | Type class for anything which can be checked for null-ness. This includes 'QExpr (Maybe a)' as
-- well as 'Table's or 'PrimaryKey's over 'Nullable QExpr'.
class Sql92Syntax syntax => SqlDeconstructMaybe syntax a nonNullA s | a s -> syntax, a -> nonNullA, a -> s, nonNullA -> s where
    -- | Returns a 'QExpr' that evaluates to true when the first argument is not null
    isJust_ :: a -> QExpr syntax be s Bool

    -- | Returns a 'QExpr' that evaluates to true when the first argument is null
    isNothing_ :: a -> QExpr syntax be s Bool

    -- | Given an object (third argument) which may or may not be null, return the default value if
    -- null (first argument), or transform the value that could be null to yield the result of the
    -- expression (second argument)
    maybe_ :: QExpr syntax be s y -> (nonNullA -> QExpr syntax be s y) -> a -> QExpr syntax be s y

instance Sql92Syntax syntax => SqlDeconstructMaybe syntax (QExpr syntax be s (Maybe x)) (QExpr syntax be s x) s where
    isJust_ (QExpr x) = QExpr (isJustE (Proxy @syntax) x)
    isNothing_ (QExpr x) = QExpr (isNothingE (Proxy @syntax) x)

    maybe_ (QExpr onNothing) onJust (QExpr e) = let QExpr onJust' = onJust (QExpr e)
                                                in QExpr (caseE (Proxy @syntax) [(isJustE (Proxy @syntax) e, onJust')] onNothing)

-- instance {-# OVERLAPPING #-} ( BeamBackend be, Table t
--                              , AllBeamValues t (QExprBool be s)
--                              , HasBeamFields t (Nullable (QExpr be s)) (Nullable (QExpr be s)) (QExprBool be s) )
--     => SqlDeconstructMaybe be (PrimaryKey t (Nullable (QExpr be s))) (PrimaryKey t (QExpr be s)) s where
--     isJust_ pk = let fieldsAreJust = changeBeamRep (\(Columnar' x) -> Columnar' (QExprBool (isJust_ x))) pk :: PrimaryKey t (QExprBool be s)
--                  in foldr (&&.) (QExpr (SQLValE (sqlBool True))) (allBeamValues (\(Columnar' (QExprBool e)) -> e) fieldsAreJust)
--     isNothing_ pk = let fieldsAreNothing = changeBeamRep (\(Columnar' x) -> Columnar' (QExprBool (isNothing_ x))) pk :: PrimaryKey t (QExprBool be s)
--                     in foldr (&&.) (QExpr (SQLValE (sqlBool True))) (allBeamValues (\(Columnar' (QExprBool e)) -> e) fieldsAreNothing)
--     maybe_ = undefined

instance ( Sql92Syntax syntax, Beamable t )
    => SqlDeconstructMaybe syntax (t (Nullable (QExpr syntax be s))) (t (QExpr syntax be s)) s where
    isJust_ t = foldr (&&.) (QExpr (valueE (Proxy @syntax) (trueV (Proxy @syntax)))) (allBeamValues (\(Columnar' e) -> isJust_ e) t)
    isNothing_ t = foldr (&&.) (QExpr (valueE (Proxy @syntax) (trueV (Proxy @syntax)))) (allBeamValues (\(Columnar' e) -> isNothing_ e) t)
    maybe_ = undefined

class BeamUnwrapMaybe c where
    beamUnwrapMaybe :: Columnar' (Nullable c) x -> Columnar' c x
instance BeamUnwrapMaybe (QExpr syntax be s) where
    beamUnwrapMaybe (Columnar' (QExpr e)) = Columnar' (QExpr e)
instance BeamUnwrapMaybe c => BeamUnwrapMaybe (Nullable c) where
    beamUnwrapMaybe (Columnar' x :: Columnar' (Nullable (Nullable c)) x) =
        let Columnar' x' = beamUnwrapMaybe (Columnar' x :: Columnar' (Nullable c) (Maybe x)) :: Columnar' c (Maybe x)

            xCol :: Columnar' (Nullable c) x
            xCol = Columnar' x'
        in xCol

-- instance {-# OVERLAPPING #-} ( SqlDeconstructMaybe be (PrimaryKey t (Nullable c)) res s, Table t, BeamUnwrapMaybe (Nullable c)) => SqlDeconstructMaybe be (PrimaryKey t (Nullable (Nullable c))) res s where
--     isJust_ t = isJust_ (changeBeamRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
--     isNothing_ t = isNothing_ (changeBeamRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
--     maybe_ = undefined

instance ( SqlDeconstructMaybe be (t (Nullable c)) res s, BeamUnwrapMaybe (Nullable c)
         , Beamable t) => SqlDeconstructMaybe be (t (Nullable (Nullable c))) res s where
    isJust_ t = isJust_ (changeBeamRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
    isNothing_ t = isNothing_ (changeBeamRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
    maybe_ = undefined
