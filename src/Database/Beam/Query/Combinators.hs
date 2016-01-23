{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Query.Combinators
    ( all_, join_, guard_, related_, lookup_
    , references_

    , limit_, offset_

    , (<.), (>.), (<=.), (>=.), (==.), (&&.), (||.), not_, div_, mod_
    , HaskellLiteralForQExpr(..), SqlValable(..)
    , enum_

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
import Data.Convertible
import Data.Text (Text)
import Data.Coerce

instance IsString (QExpr Text) where
    fromString = QExpr . SQLValE . SqlString
instance (Num a, Convertible a SqlValue) => Num (QExpr a) where
    fromInteger x = let res :: QExpr a
                        res = val_ (fromInteger x)
                    in res
    QExpr a + QExpr b = QExpr (SQLBinOpE "+" a b)
    QExpr a - QExpr b = QExpr (SQLBinOpE "-" a b)
    QExpr a * QExpr b = QExpr (SQLBinOpE "*" a b)
    negate (QExpr a) = QExpr (SQLUnOpE "-" a)
    abs (QExpr x) = QExpr (SQLFuncE "ABS" [x])
    signum x = error "signum: not defined for QExpr. Use CASE...WHEN"

-- | Introduce all entries of a table into the 'Q' monad
all_ :: Database db => DatabaseTable db table -> Q db s (table QExpr)
all_ tbl = join_ tbl (val_ True)

-- | Introduce all entries of a table into the 'Q' monad based on the given SQLExpr
join_ :: Database db => DatabaseTable db table -> QExpr Bool -> Q db s (table QExpr)
join_ (DatabaseTable table name :: DatabaseTable db table) on =
    do curTbl <- gets qbNextTblRef
       modify $ \qb@QueryBuilder { qbNextTblRef = curTbl
                                 , qbFrom = from
                                 , qbWhere = where_ } ->
           let (from', where') = case from of
                                   Nothing -> (Just newSource, where_ &&. on)
                                   Just from -> ( Just (SQLJoin SQLInnerJoin from newSource (optimizeExpr on)),
                                                  where_ )
               newSource = SQLFromSource (SQLAliased (SQLSourceTable name) (Just (fromString ("t" <> show curTbl))))
           in qb { qbNextTblRef = curTbl + 1
                 , qbFrom = from'
                 , qbWhere = where' }

       let tableSettings :: TableSettings table
           tableSettings = tblFieldSettings

           mkScopedField :: Columnar' (TableField table) a -> Columnar' QExpr a
           mkScopedField (Columnar' f) = Columnar' (QExpr (SQLFieldE (QField name (Just curTbl) (_fieldName f))))
       pure (changeRep mkScopedField tableSettings)

-- | Only allow results for which the 'QExpr' yields 'True'
guard_ :: QExpr Bool -> Q db s ()
guard_ guardE' = modify $ \qb@QueryBuilder { qbWhere = guardE } -> qb { qbWhere = guardE &&. guardE' }

-- | Introduce all entries of the given table which are referenced by the given 'ForeignKey'
related_ :: (Database db, Table rel) => DatabaseTable db rel -> PrimaryKey rel QExpr -> Q db s (rel QExpr)
related_ (relTbl :: DatabaseTable db rel) pk =
    mdo rel <- join_ relTbl (pk ==. primaryKey rel)
        pure rel

lookup_ :: (Database db, Table rel) => DatabaseTable db rel -> PrimaryKey rel QExpr -> Q db s (rel QExpr)
lookup_ = related_

references_ :: Table tbl => PrimaryKey tbl QExpr -> tbl QExpr -> QExpr Bool
references_ pk (tbl :: tbl QExpr) = pk ==. primaryKey tbl

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

class SqlOrd a where
    decomposeToGenQExprs :: a -> [SQLExpr' QField]
    (==.), (/=.) :: a -> a -> QExpr Bool
    a ==. b = let aExprs = decomposeToGenQExprs a
                  bExprs = decomposeToGenQExprs b

                  combine :: SQLExpr' QField -> SQLExpr' QField -> QExpr Bool
                  combine a b = QExpr a ==. QExpr b
              in foldr (&&.) (val_ True) (zipWith combine aExprs bExprs)
    a /=. b = not_ (a ==. b)

instance SqlOrd (QExpr a) where
    decomposeToGenQExprs (QExpr e) = [e]
    (==.) = binOpE "=="
    (/=.) = binOpE "<>"

instance {-# OVERLAPPING #-} Table tbl => SqlOrd (PrimaryKey tbl QExpr) where
    decomposeToGenQExprs = pkAllValues (\(Columnar' (QExpr e)) -> e)
instance {-# OVERLAPPING #-} Table tbl => SqlOrd (tbl QExpr) where
    decomposeToGenQExprs = fieldAllValues (\(Columnar' (QExpr e)) -> e)

binOpE op (QExpr a) (QExpr b) = QExpr (SQLBinOpE op a b)

(<.), (>.), (<=.), (>=.) :: QExpr a -> QExpr a -> QExpr Bool
(<.) = binOpE "<"
(>.) = binOpE ">"
(<=.) = binOpE "<="
(>=.) = binOpE ">="

(&&.), (||.) :: QExpr Bool -> QExpr Bool -> QExpr Bool
(&&.) = binOpE "AND"
(||.) = binOpE "OR"

infixr 3 &&.
infixr 2 ||.
infix 4 ==., /=.

not_ :: QExpr Bool -> QExpr Bool
not_ (QExpr a) = QExpr (SQLUnOpE "NOT" a)

mod_, div_ :: Integral a => QExpr a -> QExpr a -> QExpr a
div_ = binOpE "/"
mod_ = binOpE "%"

enum_ :: Show a => a -> QExpr (BeamEnum a)
enum_ = QExpr . SQLValE . SqlString . show

-- * Marshalling between Haskell literals and QExprs

type family HaskellLiteralForQExpr x
type instance HaskellLiteralForQExpr (QExpr a) = a
type instance HaskellLiteralForQExpr (table QExpr) = table Identity

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a
instance Convertible a SqlValue => SqlValable (QExpr a) where
    val_ = QExpr . SQLValE . convert

-- NOTE: This shouldn't cause problems because both overlapping instances are in the same module.
--       GHC should prefer the PrimaryKey one for primary keys and the table one for everything else.
--       AFAICT, PrimaryKey tbl QExpr ~ tbl QExpr is impossible
instance {-# OVERLAPPING #-} Table tbl => SqlValable (PrimaryKey tbl QExpr) where
    val_ = pkChangeRep valToQExpr . pkMakeSqlValues
        where valToQExpr :: Columnar' SqlValue' a -> Columnar' QExpr a
              valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))
instance {-# OVERLAPPING #-} Table tbl => SqlValable (tbl QExpr) where
    val_ = changeRep valToQExpr . makeSqlValues
        where valToQExpr :: Columnar' SqlValue' a -> Columnar' QExpr a
              valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))

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
    aggToSql (GroupAgg e) = let eSql = optimizeExpr' e
                            in mempty { sqlGroupBy = [eSql] }
    aggToSql (GenericAgg f qExprs) = mempty

    liftAggToQExpr (GroupAgg e) = QExpr e
    liftAggToQExpr (GenericAgg f qExprs) = QExpr (SQLFuncE f qExprs)
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
group_ (QExpr a) = GroupAgg a

sum_ :: Num a => QExpr a -> Aggregation a
sum_ (QExpr over) = GenericAgg "SUM" [over]

count_ :: QExpr a -> Aggregation Int
count_ (QExpr over) = GenericAgg "COUNT" [over]

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
           pure (QExpr (SQLFieldE (QField tblName (Just tblOrd) fieldName)))
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
