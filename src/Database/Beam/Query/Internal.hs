module Database.Beam.Query.Internal where

import Database.Beam.SQL.Types
import Database.Beam.Schema

import qualified Data.Text as T
import Data.Typeable

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer hiding (All)
import Control.Monad.Identity

import Database.HDBC

class IsQuery q where
    toQ :: q db s a -> Q db s a

newtype Q (db :: (((* -> *) -> *) -> *) -> *) s a = Q { runQ :: State QueryBuilder a}
    deriving (Monad, Applicative, Functor, MonadFix, MonadState QueryBuilder)

-- | Wrapper for 'Q's that have been modified in such a way that they can no longer be joined against
--   without the use of 'subquery'. 'TopLevelQ' is also an instance of 'IsQuery', and so can be passed
--   directly to 'query' or 'queryList'
newtype TopLevelQ db s a = TopLevelQ (Q db s a)

data GenQExpr where
    GenQExpr :: Typeable t => QExpr t -> GenQExpr

data QueryBuilder = QueryBuilder
                  { qbNextTblRef :: Int
                  , qbFrom  :: Maybe SQLFrom
                  , qbWhere :: QExpr Bool

                  , qbLimit  :: Maybe Integer
                  , qbOffset :: Maybe Integer
                  , qbOrdering :: [SQLOrdering]
                  , qbGrouping :: Maybe SQLGrouping }

-- * QExpr type

data QExpr t where
    FieldE :: { eFieldTblName :: T.Text
              , eFieldTblOrd  :: Int
              , eFieldName    :: T.Text } -> QExpr t

    OrE :: QExpr Bool -> QExpr Bool -> QExpr Bool
    AndE :: QExpr Bool -> QExpr Bool -> QExpr Bool

    EqE, NeqE, LtE, GtE, LeE, GeE :: Typeable a => QExpr a -> QExpr a -> QExpr Bool

    ValE :: SqlValue -> QExpr a

    FuncE :: T.Text -> [GenQExpr] -> QExpr a

    -- JustE :: Show a => QExpr a -> QExpr (Maybe a)
    -- NothingE :: QExpr (Maybe a)

    -- IsNothingE :: Typeable a => QExpr (Maybe a) -> QExpr Bool
    -- IsJustE :: Typeable a => QExpr (Maybe a) -> QExpr Bool

    -- InE :: (Typeable a, Show a) => QExpr a -> QExpr [a] -> QExpr Bool

    -- ListE :: [QExpr a] -> QExpr [a]

-- * QExpr ordering

instance Eq GenQExpr where
    GenQExpr a == GenQExpr b =
        case cast a of
          Nothing -> False
          Just a -> a == b

binOpEq :: (Typeable a, Typeable b) => QExpr a -> QExpr a -> QExpr b -> QExpr b -> Bool
binOpEq a1 b1 a2 b2 =
    case cast (a1, b1) of
      Just (a1, b1) -> a1 == a2 && b1 == b2
      Nothing -> False

instance Eq (QExpr t) where
    FieldE tblName1 tblOrd1 fieldName1 == FieldE tblName2 tblOrd2 fieldName2 =
        tblName1 == tblName2 && tblOrd1 == tblOrd2 && fieldName1 == fieldName2

    AndE a1 b1 == AndE a2 b2 = a1 == a2 && b1 == b2
    OrE a1 b1 == OrE a2 b2 = a1 == a2 && b1 == b2

    EqE a1 b1 == EqE a2 b2 = binOpEq a1 b1 a2 b2
    NeqE a1 b1 == NeqE a2 b2 = binOpEq a1 b1 a2 b2
    LtE a1 b1 == LtE a2 b2 = binOpEq a1 b1 a2 b2
    GtE a1 b1 == GtE a2 b2 = binOpEq a1 b1 a2 b2
    LeE a1 b1 == LeE a2 b2 = binOpEq a1 b1 a2 b2
    GeE a1 b1 == GeE a2 b2 = binOpEq a1 b1 a2 b2

    ValE a == ValE b = a == b

    FuncE af aArgs == FuncE bf bArgs = af == bf && aArgs == bArgs

    -- JustE a == JustE b = a == b
    -- NothingE == NothingE = True

    -- IsNothingE a == IsNothingE b = case cast a of
    --                                  Just a' -> a' == b
    --                                  Nothing -> False
    -- IsJustE a == IsJustE b = case cast a of
    --                            Just a' -> a' == b
    --                            Nothing -> False
    -- InE a as == InE b bs = case cast (a, as) of
    --                          Nothing -> False
    --                          Just (a', as') -> a' == b && as' == bs
    -- ListE a == ListE b = a == b

    _ == _ = False

-- * Sql Projections
--
--   Here we define a typeclass for all haskell data types that can be used
--   to create a projection in a SQL select statement. This includes all tables
--   as well as all tuple classes. Projections are only defined on tuples up to
--   size 5. If you need more, follow the implementations here.

class Projectible a where
    project :: a -> [GenQExpr]
instance Typeable a => Projectible (QExpr a) where
    project x = [GenQExpr x]
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

instance Table t => Projectible (t QExpr) where
    project t = fieldAllValues (\(Columnar' q) -> GenQExpr q) t
