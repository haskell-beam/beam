{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
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
    , HaskellLiteralForQExpr(..), SqlValable(..)

    -- * SQL GROUP BY and aggregation
    , aggregate, SqlGroupable(..)
    , sum_, count_

    -- * SQL ORDER BY
    , orderBy, asc_, desc_


    -- * SQL subqueries
    , subquery_ ) where

import Database.Beam.Query.Internal
import Database.Beam.Query.Types

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Fields
import Database.Beam.SQL
import Database.HDBC

import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Identity

import Data.Monoid
import Data.String
import Data.Maybe
import Data.Proxy
import Data.Convertible
import Data.Text (Text)
import Data.Coerce

instance IsString (QExpr s Text) where
    fromString = QExpr . SQLValE . SqlString
instance (Num a, Convertible a SqlValue) => Num (QExpr s a) where
    fromInteger x = let res :: QExpr s a
                        res = val_ (fromInteger x)
                    in res
    QExpr a + QExpr b = QExpr (SQLBinOpE "+" a b)
    QExpr a - QExpr b = QExpr (SQLBinOpE "-" a b)
    QExpr a * QExpr b = QExpr (SQLBinOpE "*" a b)
    negate (QExpr a) = QExpr (SQLUnOpE "-" a)
    abs (QExpr x) = QExpr (SQLFuncE "ABS" [x])
    signum x = error "signum: not defined for QExpr. Use CASE...WHEN"
instance IsString (Aggregation s Text) where
    fromString = ProjectAgg . SQLValE . SqlString
instance (Num a, Convertible a SqlValue) => Num (Aggregation s a) where
    fromInteger x = ProjectAgg (SQLValE (convert (fromInteger x :: a)))
    ProjectAgg a + ProjectAgg b = ProjectAgg (SQLBinOpE "+" a b)
    ProjectAgg a - ProjectAgg b = ProjectAgg (SQLBinOpE "-" a b)
    ProjectAgg a * ProjectAgg b = ProjectAgg (SQLBinOpE "*" a b)
    negate (ProjectAgg a) = ProjectAgg (SQLUnOpE "-" a)
    abs (ProjectAgg x) = ProjectAgg (SQLFuncE "ABS" [x])
    signum x = error "signum: not defined for Aggregation. Use CASE...WHEN"

-- | Introduce all entries of a table into the 'Q' monad
all_ :: Database db => DatabaseTable db table -> Q db s (table (QExpr s))
all_ tbl = join_ tbl (val_ True)

-- | Introduce all entries of a table into the 'Q' monad based on the given SQLExpr
join_ :: Database db => DatabaseTable db table -> QExpr s Bool -> Q db s (table (QExpr s))
join_ (DatabaseTable table name :: DatabaseTable db table) (QExpr on) =
    do curTbl <- gets qbNextTblRef
       modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                 , qbFrom = from
                                 , qbWhere = where_ } ->
           let (from', where') = case from of
                                   Nothing -> (Just newSource, SQLBinOpE "AND" where_ on)
                                   Just from -> ( Just (SQLJoin SQLInnerJoin from newSource (optimizeExpr' on)),
                                                  where_ )
               newSource = SQLFromSource (SQLAliased (SQLSourceTable name) (Just (fromString ("t" <> show curTbl))))
           in qb { qbNextTblRef = curTbl + 1
                 , qbFrom = from'
                 , qbWhere = where' }

       let tableSettings :: TableSettings table
           tableSettings = tblFieldSettings

           mkScopedField :: Columnar' (TableField table) a -> Columnar' (QExpr s) a
           mkScopedField (Columnar' f) = Columnar' (QExpr (SQLFieldE (QField name (Just curTbl) (_fieldName f))))
       pure (changeRep mkScopedField tableSettings)

-- | Introduce a table using a left join. Because this is not an inner join, the resulting table is
-- made nullable. This means that each field that would normally have type 'QExpr x' will now have
-- type 'QExpr (Maybe x)'.
leftJoin_ :: Database db => DatabaseTable db table -> QExpr s Bool -> Q db s (table (Nullable (QExpr s)))
leftJoin_ (DatabaseTable table name :: DatabaseTable db table) on =
    do curTbl <- gets qbNextTblRef
       modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                 , qbFrom = from } ->
                let from' = case from of
                              Nothing -> error "leftJoin_: empty select source"
                              Just from -> SQLJoin SQLLeftJoin from newSource (optimizeExpr on)
                    newSource = SQLFromSource (SQLAliased (SQLSourceTable name) (Just (fromString ("t" <> show curTbl))))
                in qb { qbNextTblRef = curTbl + 1
                      , qbFrom = Just from' }

       let tableSettings :: TableSettings table
           tableSettings = tblFieldSettings

           mkScopedField :: Columnar' (TableField table) a -> Columnar' (Nullable (QExpr s)) a
           mkScopedField (Columnar' f) = Columnar' (QExpr (SQLFieldE (QField name (Just curTbl) (_fieldName f))))
       pure (changeRep mkScopedField tableSettings)

-- | Only allow results for which the 'QExpr' yields 'True'
guard_ :: QExpr s Bool -> Q db s ()
guard_ (QExpr guardE') = modify $ \qb@QueryBuilder { qbWhere = guardE } -> qb { qbWhere = SQLBinOpE "AND" guardE guardE' }

-- | Introduce all entries of the given table which are referenced by the given 'PrimaryKey'
related_ :: (Database db, Table rel) => DatabaseTable db rel -> PrimaryKey rel (QExpr s) -> Q db s (rel (QExpr s))
related_ (relTbl :: DatabaseTable db rel) pk =
    mdo rel <- join_ relTbl (pk ==. primaryKey rel)
        pure rel

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_ :: (Database db, Table rel) => DatabaseTable db rel -> (rel (QExpr s) -> QExpr s Bool) -> Q db s (rel (QExpr s))
relatedBy_ (relTbl :: DatabaseTable db rel) mkOn =
    mdo rel <- join_ relTbl (mkOn rel)
        pure rel

-- | Introduce related entries of the given table, or if no related entries exist, introduce the null table
perhapsAll_ :: (Database db, Table rel) => DatabaseTable db rel -> (rel (Nullable (QExpr s)) -> QExpr s Bool) -> Q db s (rel (Nullable (QExpr s)))
perhapsAll_ relTbl expr =
    mdo rel <- leftJoin_ relTbl (expr rel)
        pure rel

-- | Synonym for 'related_'
lookup_ :: (Database db, Table rel) => DatabaseTable db rel -> PrimaryKey rel (QExpr s) -> Q db s (rel (QExpr s))
lookup_ = related_

class SqlReferences f s where
    -- | Check that the 'PrimaryKey' given matches the table. Polymorphic so it works over both
    -- regular tables and those that have been made nullable by 'leftJoin_'.
    references_ :: Table tbl => PrimaryKey tbl f -> tbl f -> QExpr s Bool
instance SqlReferences (QExpr s) s where
    references_ pk (tbl :: tbl (QExpr s)) = pk ==. primaryKey tbl
instance SqlReferences (Nullable (QExpr s)) s where
    references_ pk (tbl :: tbl (Nullable (QExpr s))) = pk ==. primaryKey tbl

-- | Limit the number of results returned by a query.
--
--   The resulting query is a top-level one that must be passed to 'query', 'queryList', or 'subquery_'. See 'TopLevelQ' for details.
limit_ :: IsQuery q => Integer -> q db s a -> TopLevelQ db s a
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
offset_ :: IsQuery q => Integer -> q db s a -> TopLevelQ db s a
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
exists_ :: (IsQuery q, Projectible a) => q db s a -> QExpr s Bool
exists_ q = let (_, _, selectCmd) = queryToSQL' (toQ q) 0
            in QExpr (SQLExistsE selectCmd)

-- ** Combinators for boolean expressions

class SqlOrd a s where
    (==.), (/=.) :: a -> a -> QExpr s Bool
    a /=. b = not_ (a ==. b)

instance SqlOrd (QExpr s a) s where
    (==.) = binOpE "=="
    (/=.) = binOpE "<>"

newtype QExprBool s a = QExprBool (QExpr s Bool)

instance {-# OVERLAPPING #-} Table tbl => SqlOrd (PrimaryKey tbl (QExpr s)) s where
    a ==. b = let pkCmp = runIdentity (zipPkM (\(Columnar' x) (Columnar' y) -> return (Columnar' (QExprBool (x ==. y))) ) a b) :: PrimaryKey tbl (QExprBool s)
              in foldr (&&.) (val_ True) (pkAllValues (\(Columnar' (QExprBool x)) -> x) pkCmp)
instance {-# OVERLAPPING #-} Table tbl => SqlOrd (tbl (QExpr s)) s where
    a ==. b = let tblCmp = runIdentity (zipTablesM (\(Columnar' x) (Columnar' y) -> return (Columnar' (QExprBool (x ==. y))) ) a b) :: tbl (QExprBool s)
              in foldr (&&.) (val_ True) (fieldAllValues (\(Columnar' (QExprBool x)) -> x) tblCmp)

instance {-# OVERLAPPING #-} Table tbl => SqlOrd (PrimaryKey tbl (Nullable (QExpr s))) s where
    a ==. b = let pkCmp = runIdentity (zipPkM (\(Columnar' x) (Columnar' y) -> return (Columnar' (QExprBool (x ==. y))) ) a b) :: PrimaryKey tbl (QExprBool s)
              in foldr (&&.) (val_ True) (pkAllValues (\(Columnar' (QExprBool x)) -> x) pkCmp)
instance {-# OVERLAPPING #-} Table tbl => SqlOrd (tbl (Nullable (QExpr s))) s where
    a ==. b = let tblCmp = runIdentity (zipTablesM (\(Columnar' x) (Columnar' y) -> return (Columnar' (QExprBool (x ==. y))) ) a b) :: tbl (QExprBool s)
              in foldr (&&.) (val_ True) (fieldAllValues (\(Columnar' (QExprBool x)) -> x) tblCmp)

binOpE op (QExpr a) (QExpr b) = QExpr (SQLBinOpE op a b)

(<.), (>.), (<=.), (>=.) :: QExpr s a -> QExpr s a -> QExpr s Bool
(<.) = binOpE "<"
(>.) = binOpE ">"
(<=.) = binOpE "<="
(>=.) = binOpE ">="

(&&.), (||.) :: QExpr s Bool -> QExpr s Bool -> QExpr s Bool
(&&.) = binOpE "AND"
(||.) = binOpE "OR"

infixr 3 &&.
infixr 2 ||.
infix 4 ==., /=.

not_ :: QExpr s Bool -> QExpr s Bool
not_ (QExpr a) = QExpr (SQLUnOpE "NOT" a)

mod_, div_ :: Integral a => QExpr s a -> QExpr s a -> QExpr s a
div_ = binOpE "/"
mod_ = binOpE "%"

-- * Marshalling between Haskell literals and QExprs

type family HaskellLiteralForQExpr x
type instance HaskellLiteralForQExpr (QExpr s a) = a
type instance HaskellLiteralForQExpr (table (QExpr s)) = table Identity

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a
instance Convertible a SqlValue => SqlValable (QExpr s a) where
    val_ = QExpr . SQLValE . convert

-- NOTE: This shouldn't cause problems because both overlapping instances are in the same module.
--       GHC should prefer the PrimaryKey one for primary keys and the table one for everything else.
--       AFAICT, PrimaryKey tbl QExpr ~ tbl QExpr is impossible
instance {-# OVERLAPPING #-} Table tbl => SqlValable (PrimaryKey tbl (QExpr s)) where
    val_ = pkChangeRep valToQExpr . pkMakeSqlValues
        where valToQExpr :: Columnar' SqlValue' a -> Columnar' (QExpr s) a
              valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))
instance {-# OVERLAPPING #-} Table tbl => SqlValable (tbl (QExpr s)) where
    val_ = changeRep valToQExpr . makeSqlValues
        where valToQExpr :: Columnar' SqlValue' a -> Columnar' (QExpr s) a
              valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))

-- * Aggregators

class Aggregating agg s | agg -> s where
    type LiftAggregationsToQExpr agg s

    aggToSql :: Proxy s -> agg -> SQLGrouping
    liftAggToQExpr :: Proxy s -> agg -> LiftAggregationsToQExpr agg s
instance Table t => Aggregating (t (Aggregation s)) s where
    type LiftAggregationsToQExpr (t (Aggregation s)) s = t (QExpr s)
    aggToSql s table = mconcat (fieldAllValues (\(Columnar' x) -> aggToSql s x) table)
    liftAggToQExpr s = changeRep (\(Columnar' x) -> Columnar' (liftAggToQExpr s x))
instance Aggregating (Aggregation s a) s where
    type LiftAggregationsToQExpr (Aggregation s a) s = QExpr s a
    aggToSql _ (GroupAgg e) = let eSql = optimizeExpr' e
                              in mempty { sqlGroupBy = [eSql] }
    aggToSql _ (ProjectAgg _) = mempty

    liftAggToQExpr _ (GroupAgg e) = QExpr e
    liftAggToQExpr _ (ProjectAgg e) = QExpr e
instance (Aggregating a s, Aggregating b s) => Aggregating (a, b) s where
    type LiftAggregationsToQExpr (a, b) s = ( LiftAggregationsToQExpr a s
                                            , LiftAggregationsToQExpr b s)
    aggToSql s (a, b) = aggToSql s a <> aggToSql s b
    liftAggToQExpr s (a, b) = (liftAggToQExpr s a, liftAggToQExpr s b)
instance (Aggregating a s, Aggregating b s, Aggregating c s) => Aggregating (a, b, c) s where
    type LiftAggregationsToQExpr (a, b, c) s = ( LiftAggregationsToQExpr a s
                                               , LiftAggregationsToQExpr b s
                                               , LiftAggregationsToQExpr c s )
    aggToSql s (a, b, c) = aggToSql s a <> aggToSql s b <> aggToSql s c
    liftAggToQExpr s (a, b, c) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c)
instance (Aggregating a s, Aggregating b s, Aggregating c s, Aggregating d s) => Aggregating (a, b, c, d) s where
    type LiftAggregationsToQExpr (a, b, c, d) s = ( LiftAggregationsToQExpr a s
                                                  , LiftAggregationsToQExpr b s
                                                  , LiftAggregationsToQExpr c s
                                                  , LiftAggregationsToQExpr d s)
    aggToSql s (a, b, c, d) = aggToSql s a <> aggToSql s b <> aggToSql s c <> aggToSql s d
    liftAggToQExpr s (a, b, c, d) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c, liftAggToQExpr s d)
instance (Aggregating a s, Aggregating b s, Aggregating c s, Aggregating d s, Aggregating e s) => Aggregating (a, b, c, d, e) s where
    type LiftAggregationsToQExpr (a, b, c, d, e) s = ( LiftAggregationsToQExpr a s
                                                     , LiftAggregationsToQExpr b s
                                                     , LiftAggregationsToQExpr c s
                                                     , LiftAggregationsToQExpr d s
                                                     , LiftAggregationsToQExpr e s)
    aggToSql s (a, b, c, d, e) = aggToSql s a <> aggToSql s b <> aggToSql s c <> aggToSql s d <> aggToSql s e
    liftAggToQExpr s (a, b, c, d, e) = (liftAggToQExpr s a, liftAggToQExpr s b, liftAggToQExpr s c, liftAggToQExpr s d, liftAggToQExpr s e)

-- | Type class for things that can be used as the basis of a grouping in a SQL GROUP BY
-- clause. This includes 'QExpr a', 'Table's, and 'PrimaryKey's. Because the given object forms the
-- basis of the group, its value is available for use in the result set.
class SqlGroupable a where
    type GroupResult a

    -- | When included in an 'Aggregating' expression, causes the results to be grouped by the
    -- given column.
    group_ :: a -> GroupResult a
instance SqlGroupable (QExpr s a) where
    type GroupResult (QExpr s a) = Aggregation s a
    group_ (QExpr a) = GroupAgg a
instance {-# OVERLAPPING #-} Table t => SqlGroupable (PrimaryKey t (QExpr s)) where
    type GroupResult (PrimaryKey t (QExpr s)) = PrimaryKey t (Aggregation s)
    group_ = pkChangeRep (\(Columnar' (QExpr e)) -> Columnar' (GroupAgg e))
instance {-# OVERLAPPING #-} Table t => SqlGroupable (t (QExpr s)) where
    type GroupResult (t (QExpr s)) = t (Aggregation s)
    group_ = changeRep (\(Columnar' (QExpr e)) -> Columnar' (GroupAgg e))

sum_ :: Num a => QExpr s a -> Aggregation s a
sum_ (QExpr over) = ProjectAgg (SQLFuncE "SUM" [over])

count_ :: QExpr s a -> Aggregation s Int
count_ (QExpr over) = ProjectAgg (SQLFuncE "COUNT" [over])

-- | Return a 'TopLevelQ' that will aggregate over the results of the original query. The
-- aggregation function (first argument) should accept the output of the query, and return a member
-- of the 'Aggregating' class which will become the result of the new query. See the 'group_'
-- combinator as well as the various aggregation combinators ('sum_', 'count_', etc.)
--
-- For example,
--
-- > aggregate (\employee -> (group_ (_employeeRegion employee), count_ (_employeeId employee))) (all_ employeesTable)
--
-- will group the result of the `all_ employeesTable` query using the `_employeeRegion` record
-- field, and then count up the number of employees for each region.
aggregate :: (Projectible a, Aggregating agg s) => (a -> agg) -> Q db s a -> TopLevelQ db s (LiftAggregationsToQExpr agg s)
aggregate (aggregator :: a -> agg) (q :: Q db s a) =
    TopLevelQ $
    do res <- q

       curTbl <- gets qbNextTblRef
       let aggregation = aggregator res
           grouping' = aggToSql (Proxy :: Proxy s) aggregation
       modify $ \qb -> case sqlGroupBy grouping' of
                         [] -> qb
                         _ -> case qbGrouping qb of
                                Nothing -> qb { qbGrouping = Just grouping' }
                                Just grouping -> qb { qbGrouping = Just (grouping <> grouping') }
       pure (liftAggToQExpr (Proxy :: Proxy s) aggregation)

-- * Order bys

class SqlOrderable a where
    makeSQLOrdering :: a -> [SQLOrdering]
instance SqlOrderable SQLOrdering where
    makeSQLOrdering x = [x]
instance SqlOrderable a => SqlOrderable [a] where
    makeSQLOrdering = concatMap makeSQLOrdering
instance ( SqlOrderable a
         , SqlOrderable b ) => SqlOrderable (a, b) where
    makeSQLOrdering (a, b) = makeSQLOrdering a <> makeSQLOrdering b
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c ) => SqlOrderable (a, b, c) where
    makeSQLOrdering (a, b, c) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c
         , SqlOrderable d ) => SqlOrderable (a, b, c, d) where
    makeSQLOrdering (a, b, c, d) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c
         , SqlOrderable d
         , SqlOrderable e ) => SqlOrderable (a, b, c, d, e) where
    makeSQLOrdering (a, b, c, d, e) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d <> makeSQLOrdering e

-- | Order by certain expressions, either ascending ('asc_') or descending ('desc_')
orderBy :: (SqlOrderable ordering, IsQuery q) => (a -> ordering) -> q db s a -> TopLevelQ db s a
orderBy orderer q =
    TopLevelQ $
    do res <- toQ q
       let ordering = makeSQLOrdering (orderer res)
       modify $ \qb -> qb { qbOrdering = qbOrdering qb <> ordering }
       pure res

desc_, asc_ :: QExpr s a -> SQLOrdering
asc_ e = Asc (optimizeExpr e)
desc_ e = Desc (optimizeExpr e)

-- * Subqueries

class Subqueryable a s where
    type Unnested a s
    subqueryProjections :: Proxy s -> a -> RWS (Text, Int) [SQLAliased SQLExpr] Int (Unnested a s)
instance Subqueryable (QExpr (QNested s) a) s where
    type Unnested (QExpr (QNested s) a) s = QExpr s a
    subqueryProjections s e =
        do i <- state (\i -> (i, i+1))
           (tblName, tblOrd) <- ask
           let fieldName = fromString ("e" <> show i)
           tell [SQLAliased (optimizeExpr e) (Just fieldName)]
           pure (QExpr (SQLFieldE (QField tblName (Just tblOrd) fieldName)))
instance ( Subqueryable a s
         , Subqueryable b s ) =>
    Subqueryable (a, b) s where
    type Unnested (a, b) s = (Unnested a s, Unnested b s)
    subqueryProjections s (a, b) =
        (,) <$> subqueryProjections s a
            <*> subqueryProjections s b
instance ( Subqueryable a s
         , Subqueryable b s
         , Subqueryable c s ) =>
    Subqueryable (a, b, c) s where
    type Unnested (a, b, c) s = (Unnested a s, Unnested b s, Unnested c s)
    subqueryProjections s (a, b, c) =
        (,,) <$> subqueryProjections s a
             <*> subqueryProjections s b
             <*> subqueryProjections s c
instance ( Subqueryable a s
         , Subqueryable b s
         , Subqueryable c s
         , Subqueryable d s ) =>
    Subqueryable (a, b, c, d) s where
    type Unnested (a, b, c, d) s = (Unnested a s, Unnested b s, Unnested c s, Unnested d s)
    subqueryProjections s (a, b, c, d) =
        (,,,) <$> subqueryProjections s a
              <*> subqueryProjections s b
              <*> subqueryProjections s c
              <*> subqueryProjections s d
instance ( Subqueryable a s
         , Subqueryable b s
         , Subqueryable c s
         , Subqueryable d s
         , Subqueryable e s ) =>
    Subqueryable (a, b, c, d, e) s where
    type Unnested (a, b, c, d, e) s = (Unnested a s, Unnested b s, Unnested c s, Unnested d s, Unnested e s)
    subqueryProjections s (a, b, c, d, e) =
        (,,,,) <$> subqueryProjections s a
               <*> subqueryProjections s b
               <*> subqueryProjections s c
               <*> subqueryProjections s d
               <*> subqueryProjections s e

-- | Run the given 'Q'-like object as a subquery, joining the results with the current result
-- set. This allows embedding of 'TopLevelQ's inside 'Q's or other 'TopLevelQ's.
subquery_ :: (IsQuery q, Projectible a, Subqueryable a s) => q db (QNested s) a -> Q db s (Unnested a s)
subquery_ (q :: q db (QNested s) a) =
    do curTbl <- gets qbNextTblRef

       let (res, curTbl', select') = queryToSQL' (toQ q) curTbl

           subTblName = fromString ("t" <> show curTbl)
           (res', projection') = evalRWS (subqueryProjections (Proxy :: Proxy s) res) (subTblName, curTbl') 0

           select'' = select' { selProjection = SQLProj projection' }

       modify $ \qb@QueryBuilder { qbFrom = from } ->
                 let from' = case from of
                               Nothing -> Just newSource
                               Just from -> Just (SQLJoin SQLInnerJoin from newSource (SQLValE (SqlBool True)))
                     newSource = SQLFromSource (SQLAliased (SQLSourceSelect select'') (Just (fromString ("t" <> show curTbl'))))
                 in qb { qbNextTblRef = curTbl' + 1
                       , qbFrom = from' }

       pure res'

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

instance SqlJustable (QExpr s a) (QExpr s (Maybe a)) where
    just_ (QExpr e) = QExpr e
    nothing_ = QExpr (SQLValE SqlNull)

instance {-# OVERLAPPING #-} Table t => SqlJustable (PrimaryKey t (QExpr s)) (PrimaryKey t (Nullable (QExpr s))) where
    just_ = pkChangeRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = pkChangeRep (\(Columnar' q) -> Columnar' nothing_) (primaryKey (tblFieldSettings :: TableSettings t))

instance {-# OVERLAPPING #-} Table t => SqlJustable (t (QExpr s)) (t (Nullable (QExpr s))) where
    just_ = changeRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeRep (\(Columnar' q) -> Columnar' nothing_) (tblFieldSettings :: TableSettings t)

instance {-# OVERLAPPING #-} Table t => SqlJustable (PrimaryKey t Identity) (PrimaryKey t (Nullable Identity)) where
    just_ = pkChangeRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = pkChangeRep (\(Columnar' q) -> Columnar' Nothing) (primaryKey (tblFieldSettings :: TableSettings t))

instance {-# OVERLAPPING #-} Table t => SqlJustable (t Identity) (t (Nullable Identity)) where
    just_ = changeRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeRep (\(Columnar' q) -> Columnar' Nothing) (tblFieldSettings :: TableSettings t)

-- * Nullable checking

-- | Type class for anything which can be checked for null-ness. This includes 'QExpr (Maybe a)' as
-- well as 'Table's or 'PrimaryKey's over 'Nullable QExpr'.
class SqlDeconstructMaybe a nonNullA s | a -> nonNullA where
    -- | Returns a 'QExpr' that evaluates to true when the first argument is not null
    isJust_ :: a -> QExpr s Bool

    -- | Returns a 'QExpr' that evaluates to true when the first argument is null
    isNothing_ :: a -> QExpr s Bool

    -- | Given an object (third argument) which may or may not be null, return the default value if
    -- null (first argument), or transform the value that could be null to yield the result of the
    -- expression (second argument)
    maybe_ :: QExpr s y -> (nonNullA -> QExpr s y) -> a -> QExpr s y

instance SqlDeconstructMaybe (QExpr s (Maybe x)) (QExpr s x) s where
    isJust_ (QExpr x) = QExpr (SQLIsJustE x)
    isNothing_ (QExpr x) = QExpr (SQLIsNothingE x)

    maybe_ (QExpr onNothing) onJust (QExpr e) = let QExpr onJust' = onJust (QExpr e)
                                                in QExpr (SQLCaseE [(SQLIsJustE e, onJust')] onNothing)

instance {-# OVERLAPPING #-} Table t => SqlDeconstructMaybe (PrimaryKey t (Nullable (QExpr s))) (PrimaryKey t (QExpr s)) s where
    isJust_ pk = let fieldsAreJust = pkChangeRep (\(Columnar' x) -> Columnar' (QExprBool (isJust_ x))) pk :: PrimaryKey t (QExprBool s)
                 in foldr (&&.) (val_ True) (pkAllValues (\(Columnar' (QExprBool e)) -> e) fieldsAreJust)
    isNothing_ pk = let fieldsAreNothing = pkChangeRep (\(Columnar' x) -> Columnar' (QExprBool (isNothing_ x))) pk :: PrimaryKey t (QExprBool s)
                    in foldr (&&.) (val_ True) (pkAllValues (\(Columnar' (QExprBool e)) -> e) fieldsAreNothing)
    maybe_ = undefined

instance {-# OVERLAPPING #-} Table t  => SqlDeconstructMaybe (t (Nullable (QExpr s))) (t (QExpr s)) s where
    isJust_ t = let fieldsAreJust = changeRep (\(Columnar' x) -> Columnar' (QExprBool (isJust_ x))) t :: t (QExprBool s)
                in foldr (&&.) (val_ True) (fieldAllValues (\(Columnar' (QExprBool e)) -> e) fieldsAreJust)
    isNothing_ t = let fieldsAreNothing = changeRep (\(Columnar' x) -> Columnar' (QExprBool (isNothing_ x))) t :: t (QExprBool s)
                   in foldr (&&.) (val_ True) (fieldAllValues (\(Columnar' (QExprBool e)) -> e) fieldsAreNothing)
    maybe_ = undefined

class BeamUnwrapMaybe c where
    beamUnwrapMaybe :: Columnar' (Nullable c) x -> Columnar' c x
instance BeamUnwrapMaybe (QExpr s) where
    beamUnwrapMaybe (Columnar' (QExpr e)) = Columnar' (QExpr e)
instance BeamUnwrapMaybe c => BeamUnwrapMaybe (Nullable c) where
    beamUnwrapMaybe (Columnar' x :: Columnar' (Nullable (Nullable c)) x) =
        let Columnar' x' = beamUnwrapMaybe (Columnar' x :: Columnar' (Nullable c) (Maybe x)) :: Columnar' c (Maybe x)

            xCol :: Columnar' (Nullable c) x
            xCol = Columnar' x'
        in xCol

instance {-# OVERLAPPING #-} ( SqlDeconstructMaybe (PrimaryKey t (Nullable c)) res s, Table t, BeamUnwrapMaybe (Nullable c)) => SqlDeconstructMaybe (PrimaryKey t (Nullable (Nullable c))) res s where
    isJust_ t = isJust_ (pkChangeRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
    isNothing_ t = isNothing_ (pkChangeRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
    maybe_ = undefined

instance {-# OVERLAPPING #-} ( SqlDeconstructMaybe (t (Nullable c)) res s, Table t, BeamUnwrapMaybe (Nullable c)) => SqlDeconstructMaybe (t (Nullable (Nullable c))) res s where
    isJust_ t = isJust_ (changeRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
    isNothing_ t = isNothing_ (changeRep (\(f :: Columnar' (Nullable (Nullable c)) x) -> beamUnwrapMaybe f :: Columnar' (Nullable c) x) t)
    maybe_ = undefined
