{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module Database.Beam.Query.Internal where

import Database.Beam.Backend.Types
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
    toQ :: q be db s a -> Q be db s a

-- | The type of queries over the database `db` returning results of type `a`. The `s` argument is a
-- threading argument meant to restrict cross-usage of `QExpr`s although this is not yet
-- implemented.
newtype Q be (db :: (((* -> *) -> *) -> *) -> *) s a = Q { runQ :: State (QueryBuilder be) a}
    deriving (Monad, Applicative, Functor, MonadFix, MonadState (QueryBuilder be))

-- | Wrapper for 'Q's that have been modified in such a way that they can no longer be joined against
--   without the use of 'subquery_'. 'TopLevelQ' is also an instance of 'IsQuery', and so can be passed
--   directly to 'query' or 'queryList'
newtype TopLevelQ be db s a = TopLevelQ (Q be db s a)

data QNested s

data QueryBuilder be = QueryBuilder
                     { qbNextTblRef :: Int
                     , qbFrom  :: Maybe (SQLFrom be)
                     , qbWhere :: SQLExpr' be QField

                     , qbLimit  :: Maybe Integer
                     , qbOffset :: Maybe Integer
                     , qbOrdering :: [SQLOrdering be]
                     , qbGrouping :: Maybe (SQLGrouping be) }

-- * QExpr type

data QField = QField
            { qFieldTblName :: T.Text
            , qFieldTblOrd  :: Maybe Int
            , qFieldName    :: T.Text }
              deriving (Show, Eq, Ord)

-- | The type of lifted beam expressions that will yield the haskell type `t` when run with
-- `queryList` or `query`. In the future, this will include a thread argument meant to prevent
-- cross-usage of expressions, but this is unimplemented for technical reasons.
newtype QExpr be s t = QExpr (SQLExpr' be QField)
deriving instance BeamBackend be => Show (QExpr be s t)
deriving instance BeamBackend be => Eq (QExpr be s t)

-- * Aggregations

data Aggregation be s a = GroupAgg (SQLExpr' be QField)
                        | ProjectAgg (SQLExpr' be QField)

-- * Sql Projections
--

-- | Typeclass for all haskell data types that can be used to create a projection in a SQL select
-- statement. This includes all tables as well as all tuple classes. Projections are only defined on
-- tuples up to size 5. If you need more, follow the implementations here.

class BeamBackend be => Projectible be a where
    project :: a -> [SQLExpr' be QField]
instance (Typeable a, BeamBackend be) => Projectible be (QExpr be s a) where
    project (QExpr x) = [x]
instance BeamBackend be => Projectible be () where
    project () = [SQLValE backendNull]
instance (Projectible be a, Projectible be b) => Projectible be (a, b) where
    project (a, b) = project a ++ project b
instance ( Projectible be a
         , Projectible be b
         , Projectible be c ) => Projectible be (a, b, c) where
    project (a, b, c) = project a ++ project b ++ project c
instance ( Projectible be a
         , Projectible be b
         , Projectible be c
         , Projectible be d ) => Projectible be (a, b, c, d) where
    project (a, b, c, d) = project a ++ project b ++ project c ++ project d
instance ( Projectible be a
         , Projectible be b
         , Projectible be c
         , Projectible be d
         , Projectible be e ) => Projectible be (a, b, c, d, e) where
    project (a, b, c, d, e) = project a ++ project b ++ project c ++ project d ++ project e

instance (Beamable t, BeamBackend be)
    => Projectible be (t (QExpr be s)) where
    project t = allBeamValues (\(Columnar' (QExpr e)) -> e) t
instance (Beamable t, BeamBackend be) => Projectible be (t (Nullable (QExpr be s))) where
    project t = allBeamValues (\(Columnar' (QExpr e)) -> e) t

-- tableVal :: Table tbl => tbl Identity -> tbl (QExpr s)
-- tableVal = changeRep valToQExpr . makeSqlValues
--     where valToQExpr :: Columnar' SqlValue' a -> Columnar' (QExpr s) a
--           valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))
