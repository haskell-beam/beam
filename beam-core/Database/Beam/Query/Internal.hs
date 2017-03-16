{-# LANGUAGE FunctionalDependencies, UndecidableInstances, TypeApplications, DeriveFunctor #-}
module Database.Beam.Query.Internal where

import           Database.Beam.Backend.Types
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL92
import           Database.Beam.Schema

import           Data.Monoid
import           Data.String
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.DList as DList
import           Data.Typeable

import           Control.Applicative
import           Control.Monad.Free.Church
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer

type ProjectibleInSelectSyntax syntax a =
  ( IsSql92SelectSyntax syntax
  , Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax) ~ Sql92SelectExpressionSyntax syntax
  , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a )
-- | Type class for any query like entity, currently `Q` and `TopLevelQ`
--class IsQuery q where
--  toSelectBuilder :: (ProjectibleInSelectSyntax syntax s a, IsSql92SelectSyntax syntax) => q syntax db s a -> SelectBuilder syntax db s a

-- newtype Q select db s a = Q { runQ :: F (QueryBuilderF select) a }

-- class (IsSql92SelectSyntax select, Functor (QueryBuilderF select)) => HasQueryBuilder select where
--   type QueryBuilderF select :: * -> *

--   introduceQ :: Beamable tbl => Proxy select -> Text -> QueryBuilderF select (tbl (QExpr expr s))
--   introduceQ :: Proxy select
--              -> F (QueryBuilderF select) a
--              -> Maybe (Sql92SelectExpressionSyntax select)
--              -> QueryBuilderF select a
--   unionQ,  :: Proxy select
--            -> F ( QueryBuilderF select) a -> F (QueryBuilderF select) a
--            -> QueryBuilderF select a

data QF select db s next where
  QAll :: DatabaseTable be db table
       -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (Sql92SelectExpressionSyntax select))
       -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> next) -> QF select db s next
  QLeftJoin :: DatabaseTable be db table
            -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (Sql92SelectExpressionSyntax select))
            -> (table (Nullable (QExpr (Sql92SelectExpressionSyntax select) s)) -> next) -> QF select db s next
  QGuard :: Sql92SelectExpressionSyntax select -> next -> QF select db s next

  QLimit :: Projectible (Sql92SelectExpressionSyntax select) r => Integer -> QM select db s r -> (r -> next) -> QF select db s next
  QOffset :: Projectible (Sql92SelectExpressionSyntax select) r => Integer -> QM select db s r -> (r -> next) -> QF select db s next

  QUnion :: Bool -> QM select db s r -> QM select db s r -> (r -> next) -> QF select db s next
  QIntersect :: Bool -> QM select db s r -> QM select db s r -> (r -> next) -> QF select db s next
  QExcept :: Bool -> QM select db s r -> QM select db s r -> (r -> next) -> QF select db s next
  QOrderBy :: [ Sql92SelectOrderingSyntax select ] -> QM select db s a -> (a -> next) -> QF select db s next
  QAggregate :: Projectible (Sql92SelectExpressionSyntax select) a => Sql92SelectGroupingSyntax select -> QM select db s a -> (a -> next) -> QF select db s next
deriving instance Functor (QF select db s)

type QM select db s = F (QF select db s)

-- | The type of queries over the database `db` returning results of type `a`. The `s` argument is a
-- threading argument meant to restrict cross-usage of `QExpr`s although this is not yet
-- implemented.
newtype Q syntax (db :: (((* -> *) -> *) -> *) -> *) s a
  = Q { runQ :: QM syntax db s a } -- State (QueryBuilder syntax) a}
    deriving (Monad, Applicative, Functor)

data QInternal

-- instance IsQuery Q where
--   toSelectBuilder = SelectBuilderQ
-- instance IsQuery SelectBuilder where
--   toSelectBuilder q = q
-- data SelectBuilder syntax (db :: (((* -> *) -> *) -> *) -> *) s a where
--   SelectBuilderQ :: ( IsSql92SelectSyntax syntax
--                     , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) s a ) =>
--                     Q syntax db s a -> SelectBuilder syntax db s a
--   SelectBuilderSelectSyntax :: a -> Sql92SelectSelectTableSyntax syntax -> SelectBuilder syntax db s a
--   SelectBuilderTopLevel ::
--     { sbLimit, sbOffset :: Maybe Integer
--     , sbOrdering        :: [ Sql92SelectOrderingSyntax syntax ]
--     , sbTable           :: SelectBuilder syntax db s a } ->
--     SelectBuilder syntax db s a

-- -- | Wrapper for 'Q's that have been modified in such a way that they can no longer be joined against
-- --   without the use of 'subquery_'. 'TopLevelQ' is also an instance of 'IsQuery', and so can be passed
-- --   directly to 'query' or 'queryList'
-- newtype TopLevelQ syntax db s a = TopLevelQ (Q syntax db s a)

data QNested s

data QueryBuilder select
  = QueryBuilder
  { qbNextTblRef :: Int
  , qbFrom  :: Maybe (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
  , qbWhere :: Maybe (Sql92SelectExpressionSyntax select) }

data GroupingBuilder select
  = GroupingBuilder
  { gbGrouping :: Maybe (Sql92SelectGroupingSyntax select)
  , gbHaving :: Maybe (Sql92SelectExpressionSyntax select)
  , gbTableSource :: QueryBuilder select }

-- * QExpr type

data QField = QField
            { qFieldTblName :: T.Text
            , qFieldTblOrd  :: Maybe Int
            , qFieldName    :: T.Text }
              deriving (Show, Eq, Ord)

-- | The type of lifted beam expressions that will yield the haskell type `t` when run with
-- `queryList` or `query`. In the future, this will include a thread argument meant to prevent
-- cross-usage of expressions, but this is unimplemented for technical reasons.
newtype QExpr syntax s t = QExpr syntax
deriving instance Show syntax => Show (QExpr syntax s t)
deriving instance Eq syntax => Eq (QExpr syntax s t)

instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) [Char] ) =>
    IsString (QExpr syntax s Text) where
    fromString = QExpr . valueE . sqlValueSyntax
instance (Num a
         , IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a) =>
    Num (QExpr syntax s a) where
    fromInteger x = let res :: QExpr syntax s a
                        res = QExpr (valueE (sqlValueSyntax (fromIntegral x :: a)))
                    in res
    QExpr a + QExpr b = QExpr (addE a b)
    QExpr a - QExpr b = QExpr (subE a b)
    QExpr a * QExpr b = QExpr (mulE a b)
    negate (QExpr a) = QExpr (negateE a)
    abs (QExpr x) = QExpr (absE x)
    signum _ = error "signum: not defined for QExpr. Use CASE...WHEN"

instance ( Fractional a
         , IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a ) =>
  Fractional (QExpr syntax s a) where

  QExpr a / QExpr b = QExpr (divE a b)
  recip = (1.0 /)

  fromRational = QExpr . valueE . sqlValueSyntax . (id :: a -> a) . fromRational

-- * Aggregations

data Aggregation syntax s a
  = GroupAgg syntax --(Sql92ExpressionSyntax syntax)
  | ProjectAgg syntax --(Sql92ExpressionSyntax syntax)

-- * Sql Projections
--

-- | Typeclass for all haskell data types that can be used to create a projection in a SQL select
-- statement. This includes all tables as well as all tuple classes. Projections are only defined on
-- tuples up to size 5. If you need more, follow the implementations here.

class Projectible syntax a | a -> syntax where
  project' :: Monad m => (syntax -> m syntax) -> a -> m a
instance Beamable t => Projectible syntax (t (QExpr syntax s)) where
  project' mutateM a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ ->
                      Columnar' . QExpr <$> mutateM e) a a
instance Beamable t => Projectible syntax (t (Nullable (QExpr syntax s))) where
  project' mutateM a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ ->
                      Columnar' . QExpr <$> mutateM e) a a
instance Projectible syntax (QExpr syntax s a) where
  project' mkE (QExpr a) = QExpr <$> mkE a
instance ( Projectible syntax a, Projectible syntax b ) =>
  Projectible syntax (a, b) where

  project' mkE (a, b) = (,) <$> project' mkE a <*> project' mkE b
instance ( Projectible syntax a, Projectible syntax b, Projectible syntax c ) =>
  Projectible syntax (a, b, c) where

  project' mkE (a, b, c) = (,,) <$> project' mkE a <*> project' mkE b <*> project' mkE c
instance ( Projectible syntax a, Projectible syntax b, Projectible syntax c
         , Projectible syntax d ) =>
  Projectible syntax (a, b, c, d) where

  project' mkE (a, b, c, d) = (,,,) <$> project' mkE a <*> project' mkE b <*> project' mkE c
                                    <*> project' mkE d
instance ( Projectible syntax a, Projectible syntax b, Projectible syntax c
         , Projectible syntax d, Projectible syntax e ) =>
  Projectible syntax (a, b, c, d, e) where

  project' mkE (a, b, c, d, e) = (,,,,) <$> project' mkE a <*> project' mkE b <*> project' mkE c
                                        <*> project' mkE d <*> project' mkE e
instance ( Projectible syntax a, Projectible syntax b, Projectible syntax c
         , Projectible syntax d, Projectible syntax e, Projectible syntax f ) =>
  Projectible syntax (a, b, c, d, e, f) where

  project' mkE  (a, b, c, d, e, f) = (,,,,,) <$> project' mkE a <*> project' mkE b <*> project' mkE c
                                             <*> project' mkE d <*> project' mkE e <*> project' mkE f

project :: Projectible syntax a => a -> [ syntax ]
project = DList.toList . execWriter . project' (\e -> tell (DList.singleton e) >> pure e)
reproject :: (IsSql92ExpressionSyntax syntax, Projectible syntax a) =>
             (Int -> syntax) -> a -> a
reproject mkField a =
  evalState (project' (\_ -> state (\i -> (i, i + 1)) >>= pure . mkField) a) 0

-- class IsSql92ExpressionSyntax syntax => Projectible syntax a where
--     project :: a -> [syntax]
-- instance (Typeable a, IsSql92ExpressionSyntax syntax) => Projectible syntax (QExpr syntax s a) where
--     project (QExpr x) = [x]
-- instance ( IsSql92ExpressionSyntax syntax
--          , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SQLNull) =>
--     Projectible syntax () where

--     project () = [valueE (sqlValueSyntax SQLNull)]
-- instance (Projectible syntax a, Projectible syntax b) => Projectible syntax (a, b) where
--     project (a, b) = project a ++ project b
-- instance ( Projectible syntax a
--          , Projectible syntax b
--          , Projectible syntax c ) => Projectible syntax (a, b, c) where
--     project (a, b, c) = project a ++ project b ++ project c
-- instance ( Projectible syntax a
--          , Projectible syntax b
--          , Projectible syntax c
--          , Projectible syntax d ) => Projectible syntax (a, b, c, d) where
--     project (a, b, c, d) = project a ++ project b ++ project c ++ project d
-- instance ( Projectible syntax a
--          , Projectible syntax b
--          , Projectible syntax c
--          , Projectible syntax d
--          , Projectible syntax e ) => Projectible syntax (a, b, c, d, e) where
--     project (a, b, c, d, e) = project a ++ project b ++ project c ++ project d ++ project e

-- instance (Beamable t, IsSql92ExpressionSyntax syntax)
--     => Projectible syntax (t (QExpr syntax s)) where
--     project t = allBeamValues (\(Columnar' (QExpr e)) -> e) t
-- instance (Beamable t, IsSql92ExpressionSyntax syntax) => Projectible syntax (t (Nullable (QExpr syntax s))) where
--     project t = allBeamValues (\(Columnar' (QExpr e)) -> e) t

-- tableVal :: Table tbl => tbl Identity -> tbl (QExpr s)
-- tableVal = changeRep valToQExpr . makeSqlValues
--     where valToQExpr :: Columnar' SqlValue' a -> Columnar' (QExpr s) a
--           valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))
