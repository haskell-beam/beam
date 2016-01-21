{-# LANGUAGE GADTs #-}
module Database.Beam.Query.Internal where

import Database.Beam.SQL.Types
import Database.Beam.Schema

import qualified Data.Text as T
import Data.Typeable

import Database.HDBC

data GenQExpr where
    GenQExpr :: Typeable t => QExpr t -> GenQExpr

data QueryBuilder = QueryBuilder
                  { qbNextTblRef :: Int
                  , qbFrom  :: Maybe SQLFrom
                  , qbWhere :: QExpr Bool

                  , qbLimit  :: Maybe Integer
                  , qbOffset :: Maybe Integer }

-- * QExpr type

data QExpr t where
    FieldE :: { eFieldTblName :: T.Text
              , eFieldTblOrd  :: Int
              , eFieldName    :: T.Text } -> QExpr t
    -- MaybeFieldE :: (Table table, Typeable c, Typeable ty) => Maybe (ScopedField table c ty) -> QExpr (Maybe (ColumnType c ty))

    OrE :: QExpr Bool -> QExpr Bool -> QExpr Bool
    AndE :: QExpr Bool -> QExpr Bool -> QExpr Bool

    EqE, NeqE, LtE, GtE, LeE, GeE :: Typeable a => QExpr a -> QExpr a -> QExpr Bool

    ValE :: SqlValue -> QExpr a

    -- JustE :: Show a => QExpr a -> QExpr (Maybe a)
    -- NothingE :: QExpr (Maybe a)

    -- IsNothingE :: Typeable a => QExpr (Maybe a) -> QExpr Bool
    -- IsJustE :: Typeable a => QExpr (Maybe a) -> QExpr Bool

    -- InE :: (Typeable a, Show a) => QExpr a -> QExpr [a] -> QExpr Bool

    -- ListE :: [QExpr a] -> QExpr [a]

    -- -- Aggregate functions
    -- CountE :: Typeable a => QExpr a -> QExpr Int
    -- MinE, MaxE, SumE, AverageE :: Typeable a => QExpr a -> QExpr a

    -- -- TODO Hack
    -- RefE :: Int -> QExpr a -> QExpr b

-- * QExpr ordering

instance Eq (QExpr t) where
    FieldE tblName1 tblOrd1 fieldName1 == FieldE tblName2 tblOrd2 fieldName2 =
        tblName1 == tblName2 && tblOrd1 == tblOrd2 && fieldName1 == fieldName2
    -- MaybeFieldE q1 == MaybeFieldE q2 =
    --     case cast q1 of
    --       Nothing -> False
    --       Just q1 -> q1 == q2

    AndE a1 b1 == AndE a2 b2 = a1 == a2 && b1 == b2
    OrE a1 b1 == OrE a2 b2 = a1 == a2 && b1 == b2

    EqE a1 b1 == EqE a2 b2 = case cast (a1, b1) of
                               Just (a1, b1)
                                   | a1 == a2 && b1 == b2 -> True
                                   | otherwise -> False
                               Nothing -> False

    ValE a == ValE b = a == b

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

    -- CountE a == CountE b = case cast a of
    --                          Nothing -> False
    --                          Just a' -> a' == b
    -- MinE a == MinE b = case cast a of
    --                      Nothing -> False
    --                      Just a' -> a' == b
    -- MaxE a == MaxE b = case cast a of
    --                      Nothing -> False
    --                      Just a' -> a' == b
    -- SumE a == SumE b = case cast a of
    --                      Nothing -> False
    --                      Just a' -> a' == b
    -- AverageE a == AverageE b = case cast a of
    --                              Nothing -> False
    --                              Just a' -> a' == b

    _ == _ = False

-- * Sql Projections
--
--   Here we define a typeclass for all haskell data types that can be used
--   to create a projection in a SQL select statement. This includes all tables
--   as well as all tuple classes. Projections are only defined on tuples up to
--   size 5. If you need more, follow the implementations here.

class Projectible a where
    project :: a -> [GenQExpr]

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
    project t = allValues (\(Columnar' q) -> GenQExpr q) t
