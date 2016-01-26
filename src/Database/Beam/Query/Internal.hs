module Database.Beam.Query.Internal where

import Database.Beam.SQL.Types
import Database.Beam.Schema

import qualified Data.Text as T
import Data.Typeable
import Data.Coerce

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer hiding (All)
import Control.Monad.Identity

import Database.HDBC

-- | Type class for any query like entity, currently `Q` and `TopLevelQ`
class IsQuery q where
    toQ :: q db s a -> Q db s a

-- | The type of queries over the database `db` returning results of type `a`. The `s` argument is a
-- threading argument meant to restrict cross-usage of `QExpr`s although this is not yet
-- implemented.
newtype Q (db :: (((* -> *) -> *) -> *) -> *) s a = Q { runQ :: State QueryBuilder a}
    deriving (Monad, Applicative, Functor, MonadFix, MonadState QueryBuilder)

-- | Wrapper for 'Q's that have been modified in such a way that they can no longer be joined against
--   without the use of 'subquery_'. 'TopLevelQ' is also an instance of 'IsQuery', and so can be passed
--   directly to 'query' or 'queryList'
newtype TopLevelQ db s a = TopLevelQ (Q db s a)

data QNested s

data QueryBuilder = QueryBuilder
                  { qbNextTblRef :: Int
                  , qbFrom  :: Maybe SQLFrom
                  , qbWhere :: SQLExpr' QField

                  , qbLimit  :: Maybe Integer
                  , qbOffset :: Maybe Integer
                  , qbOrdering :: [SQLOrdering]
                  , qbGrouping :: Maybe SQLGrouping }

-- * QExpr type

data QField = QField
            { qFieldTblName :: T.Text
            , qFieldTblOrd  :: Maybe Int
            , qFieldName    :: T.Text }
              deriving (Show, Eq, Ord)

-- | The type of lifted beam expressions that will yield the haskell type `t` when run with
-- `queryList` or `query`. In the future, this will include a thread argument meant to prevent
-- cross-usage of expressions, but this is unimplemented for technical reasons.
newtype QExpr s t = QExpr (SQLExpr' QField)
    deriving (Show, Eq)

-- * Aggregations

data Aggregation s a = GroupAgg (SQLExpr' QField)
                     | ProjectAgg (SQLExpr' QField)

-- * Sql Projections
--

-- | Typeclass for all haskell data types that can be used to create a projection in a SQL select
-- statement. This includes all tables as well as all tuple classes. Projections are only defined on
-- tuples up to size 5. If you need more, follow the implementations here.

class Projectible a where
    project :: a -> [SQLExpr' QField]
instance Typeable a => Projectible (QExpr s a) where
    project (QExpr x) = [x]
instance Projectible () where
    project () = [SQLValE (SqlInteger 1)]
instance (Projectible a, Projectible b) => Projectible (a, b) where
    project (a, b) = project a ++ project b
instance ( Projectible a
         , Projectible b
         , Projectible c ) => Projectible (a, b, c) where
    project (a, b, c) = project a ++ project b ++ project c
instance ( Projectible a
         , Projectible b
         , Projectible c
         , Projectible d ) => Projectible (a, b, c, d) where
    project (a, b, c, d) = project a ++ project b ++ project c ++ project d
instance ( Projectible a
         , Projectible b
         , Projectible c
         , Projectible d
         , Projectible e ) => Projectible (a, b, c, d, e) where
    project (a, b, c, d, e) = project a ++ project b ++ project c ++ project d ++ project e

instance Table t => Projectible (t (QExpr s)) where
    project t = fieldAllValues (\(Columnar' (QExpr e)) -> e) t
instance Table t => Projectible (t (Nullable (QExpr s))) where
    project t = fieldAllValues (\(Columnar' (QExpr e)) -> e) t

tableVal :: Table tbl => tbl Identity -> tbl (QExpr s)
tableVal = changeRep valToQExpr . makeSqlValues
    where valToQExpr :: Columnar' SqlValue' a -> Columnar' (QExpr s) a
          valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))
