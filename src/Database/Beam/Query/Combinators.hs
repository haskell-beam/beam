module Database.Beam.Query.Combinators
    ( all_, join_, guard_, related_

    , limit_, offset_

    , (<.), (>.), (<=.), (>=.), (==.), (&&.), (||.)
    , val_, enum_

    , aggregate
    , group_, sum_, count_

    , orderBy, asc_, desc_

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
import Data.Typeable
import Data.Convertible
import Data.Text (Text)
import Data.Coerce

-- | Introduce all entries of a table into the 'Q' monad
all_ :: Database db => DatabaseTable db table -> Q db s (table QExpr)
all_ tbl = join_ tbl (ValE (SqlBool True))

-- | Introduce all entries of a table into the 'Q' monad based on the given SQLExpr
join_ :: Database db => DatabaseTable db table -> QExpr Bool -> Q db s (table QExpr)
join_ (DatabaseTable table name :: DatabaseTable db table) on =
    do curTbl <- gets qbNextTblRef
       modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                 , qbFrom = from } ->
           let from' = case from of
                         Nothing -> Just newSource
                         Just from -> Just (SQLJoin SQLInnerJoin from newSource (optimizeExpr on))
               newSource = SQLFromSource (SQLAliased (SQLSourceTable name) (Just (fromString ("t" <> show curTbl))))
           in qb { qbNextTblRef = curTbl + 1
                 , qbFrom = from' }

       let tableSettings :: TableSettings table
           tableSettings = tblFieldSettings

           mkScopedField :: Columnar' (TableField table) a -> Columnar' QExpr a
           mkScopedField (Columnar' f) = Columnar' (FieldE name curTbl (_fieldName f))
       pure (changeRep mkScopedField tableSettings)

-- | Only allow results for which the 'QExpr' yields 'True'
guard_ :: QExpr Bool -> Q db s ()
guard_ guardE' = modify $ \qb@QueryBuilder { qbWhere = guardE } -> qb { qbWhere = AndE guardE guardE' }

-- | Introduce all entries of the given table which are referenced by the given 'ForeignKey'
related_ :: (Database db, Table rel) => DatabaseTable db rel -> ForeignKey rel QExpr -> Q db s (rel QExpr)
related_ (relTbl :: DatabaseTable db rel) (ForeignKey pk) =
    mdo rel <- join_ relTbl (pkEqExpr (Proxy :: Proxy rel)  pk (primaryKey rel))
        pure rel

pkEqExpr :: Table tbl => Proxy tbl -> PrimaryKey tbl QExpr -> PrimaryKey tbl QExpr -> QExpr Bool
pkEqExpr tbl a b = foldr AndE (ValE (SqlBool True)) (catMaybes (zipWith eqE (pkAllValues tbl genQExpr a) (pkAllValues tbl genQExpr b)))
    where eqE :: GenQExpr -> GenQExpr -> Maybe (QExpr Bool)
          eqE (GenQExpr a) (GenQExpr b) =
              case cast a of
                Nothing -> Nothing
                Just a -> Just (a `EqE` b)

          genQExpr :: Typeable a => Columnar' QExpr a -> GenQExpr
          genQExpr (Columnar' x) = GenQExpr x

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

-- ** Combinators for boolean expressions

(<.), (>.), (<=.), (>=.), (==.), (/=.) :: Typeable a => QExpr a -> QExpr a -> QExpr Bool
(==.) = EqE
(/=.) = NeqE
(<.) = LtE
(>.) = GtE
(<=.) = LeE
(>=.) = GeE

(&&.), (||.) :: QExpr Bool -> QExpr Bool -> QExpr Bool
(&&.) = AndE
(||.) = OrE

infixr 3 &&.
infixr 2 ||.
infix 4 ==., /=.

enum_ :: Show a => a -> QExpr (BeamEnum a)
enum_ = ValE . SqlString . show
val_ :: Convertible a SqlValue => a -> QExpr a
val_ = ValE . convert

-- * Aggregators

class Aggregating agg where
    type LiftAggregationsToQExpr agg

    aggToSql :: agg -> SQLGrouping
    liftAggToQExpr :: agg -> LiftAggregationsToQExpr agg
instance Table t => Aggregating (t Aggregation) where
    type LiftAggregationsToQExpr (t Aggregation) = t QExpr
    aggToSql table = mconcat (fieldAllValues (\(Columnar' x) -> aggToSql x) table)
    liftAggToQExpr = changeRep (\(Columnar' x) -> Columnar' (liftAggToQExpr x))
instance Aggregating (Aggregation a) where
    type LiftAggregationsToQExpr (Aggregation a) = QExpr a
    aggToSql (GroupAgg e) = let eSql = optimizeExpr e
                            in mempty { sqlGroupBy = [eSql] }
    aggToSql (GenericAgg f qExprs) = mempty

    liftAggToQExpr (GroupAgg e) = e
    liftAggToQExpr (GenericAgg f qExprs) = FuncE f qExprs
instance (Aggregating a, Aggregating b) => Aggregating (a, b) where
    type LiftAggregationsToQExpr (a, b) = ( LiftAggregationsToQExpr a
                                          , LiftAggregationsToQExpr b )
    aggToSql (a, b) = aggToSql a <> aggToSql b
    liftAggToQExpr (a, b) = (liftAggToQExpr a, liftAggToQExpr b)
instance (Aggregating a, Aggregating b, Aggregating c) => Aggregating (a, b, c) where
    type LiftAggregationsToQExpr (a, b, c) = ( LiftAggregationsToQExpr a
                                             , LiftAggregationsToQExpr b
                                             , LiftAggregationsToQExpr c )
    aggToSql (a, b, c) = aggToSql a <> aggToSql b <> aggToSql c
    liftAggToQExpr (a, b, c) = (liftAggToQExpr a, liftAggToQExpr b, liftAggToQExpr c)
instance (Aggregating a, Aggregating b, Aggregating c, Aggregating d) => Aggregating (a, b, c, d) where
    type LiftAggregationsToQExpr (a, b, c, d) = ( LiftAggregationsToQExpr a
                                                , LiftAggregationsToQExpr b
                                                , LiftAggregationsToQExpr c
                                                , LiftAggregationsToQExpr d )
    aggToSql (a, b, c, d) = aggToSql a <> aggToSql b <> aggToSql c <> aggToSql d
    liftAggToQExpr (a, b, c, d) = (liftAggToQExpr a, liftAggToQExpr b, liftAggToQExpr c, liftAggToQExpr d)
instance (Aggregating a, Aggregating b, Aggregating c, Aggregating d, Aggregating e) => Aggregating (a, b, c, d, e) where
    type LiftAggregationsToQExpr (a, b, c, d, e) = ( LiftAggregationsToQExpr a
                                                   , LiftAggregationsToQExpr b
                                                   , LiftAggregationsToQExpr c
                                                   , LiftAggregationsToQExpr d
                                                   , LiftAggregationsToQExpr e )
    aggToSql (a, b, c, d, e) = aggToSql a <> aggToSql b <> aggToSql c <> aggToSql d <> aggToSql e
    liftAggToQExpr (a, b, c, d, e) = (liftAggToQExpr a, liftAggToQExpr b, liftAggToQExpr c, liftAggToQExpr d, liftAggToQExpr e)

group_ :: QExpr a -> Aggregation a
group_ = GroupAgg

sum_ :: (Num a, Typeable a) => QExpr a -> Aggregation a
sum_ over = GenericAgg "SUM" [GenQExpr over]

count_ :: Typeable a => QExpr a -> Aggregation Int
count_ over = GenericAgg "COUNT" [GenQExpr over]

aggregate :: (Projectible a, Aggregating agg) => (a -> agg) -> (forall s. Q db s a) -> Q db s (LiftAggregationsToQExpr agg)
aggregate aggregator q =
    do res <- q

       curTbl <- gets qbNextTblRef
       let aggregation = aggregator res
           grouping' = aggToSql aggregation
       modify $ \qb -> case sqlGroupBy grouping' of
                         [] -> qb
                         _ -> case qbGrouping qb of
                                Nothing -> qb { qbGrouping = Just grouping' }
                                Just grouping -> qb { qbGrouping = Just (grouping <> grouping') }
       pure (liftAggToQExpr aggregation)

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

orderBy :: (SqlOrderable ordering, IsQuery q) => (a -> ordering) -> q db s a -> TopLevelQ db s a
orderBy orderer q =
    TopLevelQ $
    do res <- toQ q
       let ordering = makeSQLOrdering (orderer res)
       modify $ \qb -> qb { qbOrdering = qbOrdering qb <> ordering }
       pure res

desc_, asc_ :: QExpr a -> SQLOrdering
asc_ e = Asc (optimizeExpr e)
desc_ e = Desc (optimizeExpr e)

-- * Subqueries

class Subqueryable a where
    subqueryProjections :: a -> RWS (Text, Int) [SQLAliased SQLExpr] Int a
instance Subqueryable (QExpr a) where
    subqueryProjections e =
        do i <- state (\i -> (i, i+1))
           (tblName, tblOrd) <- ask
           let fieldName = fromString ("e" <> show i)
           tell [SQLAliased (optimizeExpr e) (Just fieldName)]
           pure (FieldE tblName tblOrd fieldName)
instance ( Subqueryable a
         , Subqueryable b ) =>
    Subqueryable (a, b) where
    subqueryProjections (a, b) =
        (,) <$> subqueryProjections a
            <*> subqueryProjections b
instance ( Subqueryable a
         , Subqueryable b
         , Subqueryable c ) =>
    Subqueryable (a, b, c) where
    subqueryProjections (a, b, c) =
        (,,) <$> subqueryProjections a
             <*> subqueryProjections b
             <*> subqueryProjections c
instance ( Subqueryable a
         , Subqueryable b
         , Subqueryable c
         , Subqueryable d ) =>
    Subqueryable (a, b, c, d) where
    subqueryProjections (a, b, c, d) =
        (,,,) <$> subqueryProjections a
              <*> subqueryProjections b
              <*> subqueryProjections c
              <*> subqueryProjections d
instance ( Subqueryable a
         , Subqueryable b
         , Subqueryable c
         , Subqueryable d
         , Subqueryable e ) =>
    Subqueryable (a, b, c, d, e) where
    subqueryProjections (a, b, c, d, e) =
        (,,,,) <$> subqueryProjections a
               <*> subqueryProjections b
               <*> subqueryProjections c
               <*> subqueryProjections d
               <*> subqueryProjections e

subquery_ :: (IsQuery q, Projectible a, Subqueryable a) => (forall s. q db s a) -> Q db s a
subquery_ q = do curTbl <- gets qbNextTblRef

                 let (res, select') = queryToSQL' (toQ q)

                     subTblName = fromString ("t" <> show curTbl)
                     (res', projection') = evalRWS (subqueryProjections res) (subTblName, curTbl) 0

                     select'' = select' { selProjection = SQLProj projection' }

                 modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                           , qbFrom = from } ->
                           let from' = case from of
                                         Nothing -> Just newSource
                                         Just from -> Just (SQLJoin SQLInnerJoin from newSource (SQLValE (SqlBool True)))
                               newSource = SQLFromSource (SQLAliased (SQLSourceSelect select'') (Just (fromString ("t" <> show curTbl))))
                           in qb { qbNextTblRef = curTbl + 1
                                 , qbFrom = from' }

                 pure res'
