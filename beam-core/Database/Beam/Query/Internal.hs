{-# LANGUAGE FunctionalDependencies, UndecidableInstances, TypeApplications #-}
module Database.Beam.Query.Internal where

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL92
import Database.Beam.SQL.Types
import Database.Beam.Schema

import qualified Data.Text as T
import Data.Typeable
import Data.Coerce

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer hiding (All)
import Control.Monad.Identity

-- | Type class for any query like entity, currently `Q` and `TopLevelQ`
class IsQuery q where
    toQ :: q syntax be db s a -> Q syntax be db s a

-- | The type of queries over the database `db` returning results of type `a`. The `s` argument is a
-- threading argument meant to restrict cross-usage of `QExpr`s although this is not yet
-- implemented.
newtype Q syntax be (db :: (((* -> *) -> *) -> *) -> *) s a = Q { runQ :: State (QueryBuilder syntax be) a}
    deriving (Monad, Applicative, Functor, MonadFix, MonadState (QueryBuilder syntax be))

-- | Wrapper for 'Q's that have been modified in such a way that they can no longer be joined against
--   without the use of 'subquery_'. 'TopLevelQ' is also an instance of 'IsQuery', and so can be passed
--   directly to 'query' or 'queryList'
newtype TopLevelQ syntax be db s a = TopLevelQ (Q syntax be db s a)

data QNested s

data QueryBuilder syntax be
  = QueryBuilder
  { qbNextTblRef :: Int
  , qbFrom  :: Maybe (Sql92FromSyntax syntax)
  , qbWhere :: Sql92ExpressionSyntax syntax

  , qbLimit  :: Maybe Integer
  , qbOffset :: Maybe Integer
  , qbOrdering :: [Sql92OrderingSyntax syntax]
  , qbGrouping :: Maybe (Sql92GroupingSyntax syntax) }

-- * QExpr type

data QField = QField
            { qFieldTblName :: T.Text
            , qFieldTblOrd  :: Maybe Int
            , qFieldName    :: T.Text }
              deriving (Show, Eq, Ord)

-- | The type of lifted beam expressions that will yield the haskell type `t` when run with
-- `queryList` or `query`. In the future, this will include a thread argument meant to prevent
-- cross-usage of expressions, but this is unimplemented for technical reasons.
newtype QExpr syntax be s t = QExpr (Sql92ExpressionSyntax syntax)
deriving instance Show (Sql92ExpressionSyntax syntax) => Show (QExpr syntax be s t)
deriving instance Eq (Sql92ExpressionSyntax syntax) => Eq (QExpr syntax be s t)

-- * Aggregations

data Aggregation syntax be s a
  = GroupAgg (Sql92ExpressionSyntax syntax)
  | ProjectAgg (Sql92ExpressionSyntax syntax)

-- * Sql Projections
--

-- | Typeclass for all haskell data types that can be used to create a projection in a SQL select
-- statement. This includes all tables as well as all tuple classes. Projections are only defined on
-- tuples up to size 5. If you need more, follow the implementations here.

class Sql92Syntax syntax => Projectible syntax a where
    project :: Proxy syntax -> a -> [Sql92ExpressionSyntax syntax]
instance (Typeable a, Sql92Syntax syntax) => Projectible syntax (QExpr syntax be s a) where
    project p (QExpr x) = [x]
instance Sql92Syntax syntax => Projectible syntax () where
    project p () = [valueE (Proxy @syntax) (nullV (Proxy @syntax))]
instance (Projectible syntax a, Projectible syntax b) => Projectible syntax (a, b) where
    project p (a, b) = project p a ++ project p b
instance ( Projectible syntax a
         , Projectible syntax b
         , Projectible syntax c ) => Projectible syntax (a, b, c) where
    project p (a, b, c) = project p a ++ project p b ++ project p c
instance ( Projectible syntax a
         , Projectible syntax b
         , Projectible syntax c
         , Projectible syntax d ) => Projectible syntax (a, b, c, d) where
    project p (a, b, c, d) = project p a ++ project p b ++ project p c ++ project p d
instance ( Projectible syntax a
         , Projectible syntax b
         , Projectible syntax c
         , Projectible syntax d
         , Projectible syntax e ) => Projectible syntax (a, b, c, d, e) where
    project p (a, b, c, d, e) = project p a ++ project p b ++ project p c ++ project p d ++ project p e

instance (Beamable t, Sql92Syntax syntax)
    => Projectible syntax (t (QExpr syntax be s)) where
    project p t = allBeamValues (\(Columnar' (QExpr e)) -> e) t
instance (Beamable t, Sql92Syntax syntax) => Projectible syntax (t (Nullable (QExpr syntax be s))) where
    project p t = allBeamValues (\(Columnar' (QExpr e)) -> e) t

-- tableVal :: Table tbl => tbl Identity -> tbl (QExpr s)
-- tableVal = changeRep valToQExpr . makeSqlValues
--     where valToQExpr :: Columnar' SqlValue' a -> Columnar' (QExpr s) a
--           valToQExpr (Columnar' (SqlValue' v)) = Columnar' (QExpr (SQLValE v))
