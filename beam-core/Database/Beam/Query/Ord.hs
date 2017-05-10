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

-- | A data structure representing the set to quantify a comparison operator over.
data QQuantified expr s r
  = QQuantified (Sql92ExpressionQuantifierSyntax expr) expr

-- | A 'QQuantified' representing a SQL @ALL(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
allOf_
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

-- | A 'QQuantified' representing a SQL @ANY(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
anyOf_
  :: forall s r select expr db.
   ( ThreadRewritable (QNested s) r
   , ProjectibleInSelectSyntax select r
   , IsSql92SelectSyntax select
   , IsSql92ExpressionSyntax expr
   , HasQBuilder select
   , Sql92ExpressionSelectSyntax expr ~ select )
  => Q select db (QNested s) r
  -> QQuantified expr s (WithRewrittenThread (QNested s) s r)
anyOf_ s = QQuantified quantifyOverAny (subqueryE (buildSqlQuery s))

-- | SQL @BETWEEN@ clause
between_ :: IsSql92ExpressionSyntax syntax
         => QGenExpr context syntax s a -> QGenExpr context syntax s a
         -> QGenExpr context syntax s a -> QGenExpr context syntax s Bool
between_ (QExpr a) (QExpr min_) (QExpr max_) =
  QExpr (betweenE a min_ max_)

-- | Class for expression types or expression containers for which there is a
--   notion of equality.
--
--   Instances are provided to check the equality of expressions of the same
--   type as well as entire 'Beamable' types parameterized over 'QGenExpr'
class SqlEq expr a | a -> expr where
  -- | Given two expressions, returns whether they are equal
  (==.) :: a -> a -> expr Bool
  -- | Given two expressions, returns whether they are not equal
  (/=.) :: a -> a -> expr Bool

-- | Class for expression types for which there is a notion of /quantified/
--   equality.
class SqlEq expr a => SqlEqQuantified expr quantified a | a -> expr quantified where

  (==*.), (/=*.) :: a -> quantified -> expr Bool

infix 4 ==., /=., ==*., /=*.
infix 4 <., >., <=., >=.
infix 4 <*., >*., <=*., >=*.

-- | Compare two arbitrary expressions (of the same type) for equality
instance IsSql92ExpressionSyntax syntax =>
  SqlEq (QGenExpr context syntax s) (QGenExpr context syntax s a) where

  (==.) = qBinOpE (eqE Nothing)
  (/=.) = qBinOpE (neqE Nothing)

-- | Two arbitrary expressions can be quantifiably compared for equality.
instance IsSql92ExpressionSyntax syntax =>
  SqlEqQuantified (QGenExpr context syntax s) (QQuantified syntax s a) (QGenExpr context syntax s a) where

  a ==*. QQuantified q b = qBinOpE (eqE (Just q)) a (QExpr b)
  a /=*. QQuantified q b = qBinOpE (neqE (Just q)) a (QExpr b)

-- | Compare two arbitrary 'Beamable' types containing 'QGenExpr's for equality.
instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable tbl ) =>
         SqlEq (QGenExpr context syntax s) (tbl (QGenExpr context syntax s)) where

  a ==. b = let (_, e) = runState (zipBeamFieldsM
                                   (\x'@(Columnar' x) (Columnar' y) ->
                                       do modify (\expr ->
                                                    case expr of
                                                      Nothing -> Just $ x ==. y
                                                      Just expr' -> Just $ expr' &&. x ==. y)
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
                                                      Just expr' -> Just $ expr' &&. x ==. y)
                                          return x') a b) Nothing
            in fromMaybe (QExpr (valueE (sqlValueSyntax True))) e
  a /=. b = not_ (a ==. b)

-- * Comparisons

-- | Class for expression types or expression containers for which there is a
--   notion of ordering.
--
--   Instances are provided to check the ordering of expressions of the same
--   type. Since there is no universal notion of ordering for an arbitrary
--   number of expressions, no instance is provided for 'Beamable' types.
class SqlEq expr e => SqlOrd expr e | e -> expr where

  (<.), (>.), (<=.), (>=.) :: e -> e -> expr Bool

-- | Class for things which can be /quantifiably/ compared.
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
