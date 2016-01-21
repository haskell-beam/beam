{-# LANGUAGE RecursiveDo, ScopedTypeVariables, GADTs #-}
module Database.Beam.Query.Combinators
    ( all_, join_, guard_, related_

    , limit_, offset_

    , (<#), (>#), (<=#), (>=#), (==#), (&&#), (||#)
    , val_, enum_ ) where

import Database.Beam.Query.Internal
import Database.Beam.Query.Types

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Fields
import Database.Beam.SQL
import Database.HDBC

import Control.Monad.State
import Control.Monad.Identity

import Data.Monoid
import Data.String
import Data.Maybe
import Data.Typeable
import Data.Convertible

all_ :: Database db => DatabaseTable db table -> Q db s (table QExpr)
all_ tbl = join_' tbl (SQLValE (SqlBool True))

join_' :: Database db => DatabaseTable db table -> SQLExpr -> Q db s (table QExpr)
join_' (DatabaseTable table name :: DatabaseTable db table) on =
    do curTbl <- gets qbNextTblRef
       modify $ \qb@(QueryBuilder { qbNextTblRef = curTbl
                                  , qbFrom = from }) ->
           let from' = case from of
                         Nothing -> Just newSource
                         Just from -> Just (SQLJoin SQLInnerJoin from newSource on)
               newSource = SQLFromSource (SQLAliased (SQLSourceTable name) (Just (fromString ("t" <> show curTbl))))
           in qb { qbNextTblRef = curTbl + 1
                 , qbFrom = from' }

       let tableSettings :: TableSettings table
           tableSettings = tblFieldSettings

           mkScopedField :: Columnar' (TableField table) a -> Columnar' QExpr a
           mkScopedField (Columnar' f) = Columnar' (FieldE name curTbl (_fieldName f))
       pure (changeRep mkScopedField tableSettings)

join_ :: Database db => DatabaseTable db table -> QExpr Bool -> Q db s (table QExpr)
join_ dbTbl on = let onSql = mkSqlExpr (runIdentity (rewriteExprM (pure . allExprOpts) on))
                 in join_' dbTbl onSql

guard_ :: QExpr Bool -> Q db s ()
guard_ guardE' = modify $ \qb@(QueryBuilder { qbWhere = guardE }) -> qb { qbWhere = AndE guardE guardE' }

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

limit_ :: Integer -> Q db s a -> Q db s a
limit_ limit' q =
    do res <- q
       modify $ \qb ->
           let qbLimit' = case qbLimit qb of
                            Nothing -> Just limit'
                            Just limit -> Just (min limit limit')
           in qb { qbLimit = qbLimit' }
       pure res

offset_ :: Integer -> Q db s a -> Q db s a
offset_ offset' q =
    do res <- q
       modify $ \qb ->
           let qbOffset' = case qbOffset qb of
                             Nothing -> Just offset'
                             Just offset -> Just (offset + offset')
           in qb { qbOffset = qbOffset' }
       pure res

-- ** Combinators for boolean expressions

(<#), (>#), (<=#), (>=#), (==#) :: (Typeable a, Show a) => QExpr a -> QExpr a -> QExpr Bool
(==#) = EqE
(<#) = LtE
(>#) = GtE
(<=#) = LeE
(>=#) = GeE

(&&#), (||#) :: QExpr Bool -> QExpr Bool -> QExpr Bool
(&&#) = AndE
(||#) = OrE

infixr 3 &&#
infixr 2 ||#
infix 4 ==#

enum_ :: Show a => a -> QExpr (BeamEnum a)
enum_ = ValE . SqlString . show
val_ :: Convertible a SqlValue => a -> QExpr a
val_ = ValE . convert
