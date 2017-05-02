-- | Defines classen 'SqlEq' and 'SqlOrd' that can be used to perform equality
--   and comparison operations on certain expressions.
--
--   In particular, any 'Beamable' value over 'QGenExpr' or any 'QGenExpr'
--   object can be compared for equality and inequality using the '(==.)' and
--   '(/=.)' operators respectively.
--
--   Simple (scalar) 'QGenExpr's can be compared using the '(<.)', '(>.)',
--   '(<=.)', and '(>=.)' operators respectively.
--
--   The "Quantified Comparison Syntax" (i.e., @.. > ANY (..)@) is supported
--   using the corresponding operators suffixed with a @*@ before the dot. For
--   example, @x == ANY(SELECT ..)@ can be written.
--
-- > x ==*. anyOf_ ..
--
--   Or, for example, @x > ALL(SELECT ..)@ can be written
--
-- > x >*. anyOf_ ..
module Database.Beam.Query.Ord
  ( SqlEq(..), SqlEqQuantified(..)
  , SqlOrd(..), SqlOrdQuantified(..)
  , QQuantified(..)

  , anyOf_, allOf_, between_ ) where

import Database.Beam.Query.Internal
import Database.Beam.Query.Types
import Database.Beam.Query.Operator

import Database.Beam.Schema.Tables
import Database.Beam.Backend.SQL

import Control.Monad.State

import Data.Maybe

data QQuantified expr s r
  = QQuantified (Sql92ExpressionQuantifierSyntax expr) expr

allOf_, anyOf_
  :: forall s r select expr db.
   ( ThreadRewritable (QNested s) r
   , ProjectibleInSelectSyntax select r
   , IsSql92SelectSyntax select
   , IsSql92ExpressionSyntax expr
   , HasQBuilder select
   , Sql92ExpressionSelectSyntax expr ~ select )
  => Q select db (QNested s) r
  -> QQuantified expr s (WithRewrittenThread (QNested s) s r)
allOf_ s = QQuantified quantifyOverAll (subqueryE (buildSqlQuery s))
anyOf_ s = QQuantified quantifyOverAny (subqueryE (buildSqlQuery s))

between_ :: IsSql92ExpressionSyntax syntax
         => QGenExpr context syntax s a -> QGenExpr context syntax s a
         -> QGenExpr context syntax s a -> QGenExpr context syntax s Bool
between_ (QExpr a) (QExpr min_) (QExpr max_) =
  QExpr (betweenE a min_ max_)

class SqlEq expr a | a -> expr where
  (==.), (/=.) :: a -> a -> expr Bool

class SqlEq expr a => SqlEqQuantified expr quantified a | a -> expr quantified where

  (==*.), (/=*.) :: a -> quantified -> expr Bool

infix 4 ==., /=., ==*., /=*.
infix 4 <., >., <=., >=.
infix 4 <*., >*., <=*., >=*.

instance IsSql92ExpressionSyntax syntax =>
  SqlEq (QGenExpr context syntax s) (QGenExpr context syntax s a) where

  (==.) = qBinOpE (eqE Nothing)
  (/=.) = qBinOpE (neqE Nothing)

instance IsSql92ExpressionSyntax syntax =>
  SqlEqQuantified (QGenExpr context syntax s) (QQuantified syntax s a) (QGenExpr context syntax s a) where

  a ==*. QQuantified q b = qBinOpE (eqE (Just q)) a (QExpr b)
  a /=*. QQuantified q b = qBinOpE (neqE (Just q)) a (QExpr b)

instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable tbl ) =>
         SqlEq (QGenExpr context syntax s) (tbl (QGenExpr context syntax s)) where

  a ==. b = let (_, e) = runState (zipBeamFieldsM
                                   (\x'@(Columnar' x) (Columnar' y) ->
                                       do modify (\expr ->
                                                    case expr of
                                                      Nothing -> Just $ x ==. y
                                                      Just expr -> Just $ expr &&. x ==. y)
                                          return x') a b) Nothing
            in fromMaybe (QExpr (valueE (sqlValueSyntax True))) e
  a /=. b = not_ (a ==. b)

instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable tbl)
    => SqlEq (QGenExpr context syntax s) (tbl (Nullable (QGenExpr context syntax s))) where

  a ==. b = let (_, e) = runState (zipBeamFieldsM
                                      (\x'@(Columnar' x) (Columnar' y) -> do
                                          modify (\expr ->
                                                    case expr of
                                                      Nothing -> Just $ x ==. y
                                                      Just expr -> Just $ expr &&. x ==. y)
                                          return x') a b) Nothing
            in fromMaybe (QExpr (valueE (sqlValueSyntax True))) e
  a /=. b = not_ (a ==. b)

-- * Comparisons

class SqlEq expr e => SqlOrd expr e | e -> expr where

  (<.), (>.), (<=.), (>=.) :: e -> e -> expr Bool

class (SqlOrd expr e, SqlEqQuantified expr quantified e) =>
  SqlOrdQuantified expr quantified e | e -> expr quantified where

  (<*.), (>*.), (<=*.), (>=*.) :: e -> quantified  -> expr Bool

instance IsSql92ExpressionSyntax syntax =>
  SqlOrd (QGenExpr context syntax s) (QGenExpr context syntax s a) where

  (<.) = qBinOpE (ltE Nothing)
  (>.) = qBinOpE (gtE Nothing)
  (<=.) = qBinOpE (leE Nothing)
  (>=.) = qBinOpE (geE Nothing)

instance IsSql92ExpressionSyntax syntax =>
  SqlOrdQuantified (QGenExpr context syntax s) (QQuantified syntax s a) (QGenExpr context syntax s a) where
  a <*. QQuantified q b = qBinOpE (ltE (Just q)) a (QExpr b)
  a <=*. QQuantified q b = qBinOpE (leE (Just q)) a (QExpr b)
  a >*. QQuantified q b = qBinOpE (gtE (Just q)) a (QExpr b)
  a >=*. QQuantified q b = qBinOpE (geE (Just q)) a (QExpr b)
