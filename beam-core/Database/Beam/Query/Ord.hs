{-# LANGUAGE UndecidableInstances #-}

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
-- > x >*. allOf_ ..
module Database.Beam.Query.Ord
  ( SqlEq(..), SqlEqQuantified(..)
  , SqlOrd(..), SqlOrdQuantified(..)
  , QQuantified(..)

  , HasSqlEqualityCheck(..), HasSqlQuantifiedEqualityCheck(..)
  , HasTableEquality, HasTableEqualityNullable

  , isTrue_, isNotTrue_
  , isFalse_, isNotFalse_
  , isUnknown_, isNotUnknown_
  , unknownAs_

  , anyOf_, anyIn_
  , allOf_, allIn_

  , between_

  , in_ ) where

import Database.Beam.Query.Internal
import Database.Beam.Query.Types
import Database.Beam.Query.Operator

import Database.Beam.Schema.Tables
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.AST (Expression)
import Database.Beam.Backend.SQL.Builder (SqlSyntaxBuilder)

import Control.Applicative
import Control.Monad.State

import Data.Maybe
import Data.Proxy
import Data.Kind
import Data.Word
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime, LocalTime, Day, TimeOfDay)

import GHC.TypeLits

-- | A data structure representing the set to quantify a comparison operator over.
data QQuantified expr s r
  = QQuantified (Sql92ExpressionQuantifierSyntax expr) (WithExprContext expr)

-- | SQL @IS TRUE@ operator
isTrue_ :: IsSql92ExpressionSyntax syntax
        => QGenExpr context syntax s SqlBool -> QGenExpr context syntax s Bool
isTrue_ (QExpr s) = QExpr (fmap isTrueE s)

-- | SQL @IS NOT TRUE@ operator
isNotTrue_ :: IsSql92ExpressionSyntax syntax
           => QGenExpr context syntax s SqlBool -> QGenExpr context syntax s Bool
isNotTrue_ (QExpr s) = QExpr (fmap isNotTrueE s)

-- | SQL @IS FALSE@ operator
isFalse_ :: IsSql92ExpressionSyntax syntax
         => QGenExpr context syntax s SqlBool -> QGenExpr context syntax s Bool
isFalse_ (QExpr s) = QExpr (fmap isFalseE s)

-- | SQL @IS NOT FALSE@ operator
isNotFalse_ :: IsSql92ExpressionSyntax syntax
            => QGenExpr context syntax s SqlBool -> QGenExpr context syntax s Bool
isNotFalse_ (QExpr s) = QExpr (fmap isNotFalseE s)

-- | SQL @IS UNKNOWN@ operator
isUnknown_ :: IsSql92ExpressionSyntax syntax
           => QGenExpr context syntax s SqlBool -> QGenExpr context syntax s Bool
isUnknown_ (QExpr s) = QExpr (fmap isUnknownE s)

-- | SQL @IS NOT UNKNOWN@ operator
isNotUnknown_ :: IsSql92ExpressionSyntax syntax
              => QGenExpr context syntax s SqlBool -> QGenExpr context syntax s Bool
isNotUnknown_ (QExpr s) = QExpr (fmap isNotUnknownE s)

-- | Return the first argument if the expression has the unknown SQL value
unknownAs_ :: IsSql92ExpressionSyntax syntax
           => Bool -> QGenExpr context syntax s SqlBool -> QGenExpr context syntax s Bool
unknownAs_ False = isTrue_ -- If unknown is being treated as false, then return true only if the expression is true
unknownAs_ True  = isNotFalse_ -- If unknown is being treated as true, then return true only if the expression is not false

-- | A 'QQuantified' representing a SQL @ALL(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts a subquery. Use 'allIn_' for an explicit list
allOf_
  :: forall s a select expr db.
   ( IsSql92SelectSyntax select
   , IsSql92ExpressionSyntax expr
   , HasQBuilder select
   , Sql92ExpressionSelectSyntax expr ~ select )
  => Q select db (QNested s) (QExpr (Sql92SelectExpressionSyntax select) (QNested s) a)
  -> QQuantified expr s a
allOf_ s = QQuantified quantifyOverAll (\tblPfx -> subqueryE (buildSqlQuery tblPfx s))

-- | A 'QQuantified' representing a SQL @ALL(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts an explicit list of typed expressions. Use 'allOf_' for
--   a subquery
allIn_
  :: forall s a expr
   . ( IsSql92ExpressionSyntax expr )
  => [QExpr expr s a]
  -> QQuantified expr s a
allIn_ es = QQuantified quantifyOverAll (quantifierListE <$> mapM (\(QExpr e) -> e) es)

-- | A 'QQuantified' representing a SQL @ANY(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts a subquery. Use 'anyIn_' for an explicit list
anyOf_
  :: forall s a select expr db.
   ( IsSql92SelectSyntax select
   , IsSql92ExpressionSyntax expr
   , HasQBuilder select
   , Sql92ExpressionSelectSyntax expr ~ select )
  => Q select db (QNested s) (QExpr (Sql92SelectExpressionSyntax select) (QNested s) a)
  -> QQuantified expr s a
anyOf_ s = QQuantified quantifyOverAny (\tblPfx -> subqueryE (buildSqlQuery tblPfx s))

-- | A 'QQuantified' representing a SQL @ANY(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts an explicit list of typed expressions. Use 'anyOf_' for
--   a subquery
anyIn_
  :: forall s a expr
   . ( IsSql92ExpressionSyntax expr )
  => [QExpr expr s a]
  -> QQuantified expr s a
anyIn_ es = QQuantified quantifyOverAny (quantifierListE <$> mapM (\(QExpr e) -> e) es)

-- | SQL @BETWEEN@ clause
between_ :: IsSql92ExpressionSyntax syntax
         => QGenExpr context syntax s a -> QGenExpr context syntax s a
         -> QGenExpr context syntax s a -> QGenExpr context syntax s Bool
between_ (QExpr a) (QExpr min_) (QExpr max_) =
  QExpr (liftA3 betweenE a min_ max_)

-- | SQL @IN@ predicate
in_ :: ( IsSql92ExpressionSyntax syntax
       , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool )
    => QGenExpr context syntax s a
    -> [ QGenExpr context syntax s a ]
    -> QGenExpr context syntax s Bool
in_ _ [] = QExpr (pure (valueE (sqlValueSyntax False)))
in_ (QExpr row) options = QExpr (inE <$> row <*> mapM (\(QExpr o) -> o) options)

-- | Class for expression types or expression containers for which there is a
--   notion of equality.
--
--   Instances are provided to check the equality of expressions of the same
--   type as well as entire 'Beamable' types parameterized over 'QGenExpr'
class SqlEq expr a | a -> expr where
  -- | Given two expressions, returns whether they are equal, using Haskell semantics (NULLs handled properly)
  (==.) :: a -> a -> expr Bool
  -- | Given two expressions, returns whether they are not equal, using Haskell semantics (NULLs handled properly)
  (/=.) :: a -> a -> expr Bool

  -- | Given two expressions, returns the /SQL tri-state boolean/ when compared for equality
  (==?.) :: a -> a -> expr SqlBool

  -- | Given two expressions, returns the /SQL tri-state boolean/ when compared for inequality
  (/=?.) :: a -> a -> expr SqlBool

-- | Class for expression types for which there is a notion of /quantified/
--   equality.
class SqlEq expr a => SqlEqQuantified expr quantified a | a -> expr quantified where

  -- | Quantified equality and inequality using /SQL semantics/ (tri-state boolean)
  (==*.), (/=*.) :: a -> quantified -> expr SqlBool

infix 4 ==., /=., ==*., /=*.
infix 4 <., >., <=., >=.
infix 4 <*., >*., <=*., >=*.

-- | Class for Haskell types that can be compared for equality in the given expression syntax
class (IsSql92ExpressionSyntax syntax, HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool) =>
  HasSqlEqualityCheck syntax a where

  sqlEqE, sqlNeqE :: Proxy a -> syntax -> syntax -> syntax
  sqlEqE _ = eqE Nothing
  sqlNeqE _ = neqE Nothing

  -- | Tri-state equality
  sqlEqTriE, sqlNeqTriE :: Proxy a -> syntax -> syntax -> syntax
  sqlEqTriE _ = eqE Nothing
  sqlNeqTriE _ = neqE Nothing

type family CanCheckMaybeEquality a :: Constraint where
  CanCheckMaybeEquality (Maybe a) =
    TypeError ('Text "Attempt to check equality of nested Maybe." ':$$:
               'Text "Beam can only reasonably check equality of a single nesting of Maybe.")
  CanCheckMaybeEquality a = ()

instance (HasSqlEqualityCheck syntax a, CanCheckMaybeEquality a) => HasSqlEqualityCheck syntax (Maybe a) where
  sqlEqE _ a b = eqMaybeE a b (sqlEqE (Proxy @a) a b)
  sqlNeqE _ a b = neqMaybeE a b (sqlNeqE (Proxy @a) a b)

instance HasSqlEqualityCheck syntax a => HasSqlEqualityCheck syntax (SqlSerial a) where
  sqlEqE _ = sqlEqE (Proxy @a)
  sqlNeqE _ = sqlNeqE (Proxy @a)

  sqlEqTriE _ = sqlEqTriE (Proxy @a)
  sqlNeqTriE _ = sqlNeqTriE (Proxy @a)

-- | Class for Haskell types that can be compared for quantified equality in the given expression syntax
class HasSqlEqualityCheck syntax a => HasSqlQuantifiedEqualityCheck syntax a where
  sqlQEqE, sqlQNeqE :: Proxy a -> Maybe (Sql92ExpressionQuantifierSyntax syntax)
                    -> syntax -> syntax -> syntax
  sqlQEqE _ = eqE
  sqlQNeqE _ = neqE

instance (HasSqlQuantifiedEqualityCheck syntax a, CanCheckMaybeEquality a) => HasSqlQuantifiedEqualityCheck syntax (Maybe a) where
  sqlQEqE _ = sqlQEqE (Proxy @a)
  sqlQNeqE _ = sqlQNeqE (Proxy @a)

instance HasSqlQuantifiedEqualityCheck syntax a => HasSqlQuantifiedEqualityCheck syntax (SqlSerial a) where
  sqlQEqE _ = sqlQEqE (Proxy @a)
  sqlQNeqE _ = sqlQNeqE (Proxy @a)

-- | Compare two arbitrary expressions (of the same type) for equality
instance ( IsSql92ExpressionSyntax syntax, HasSqlEqualityCheck syntax a ) =>
  SqlEq (QGenExpr context syntax s) (QGenExpr context syntax s a) where

  (==.) = qBinOpE (sqlEqE (Proxy @a))
  (/=.) = qBinOpE (sqlNeqE (Proxy @a))

  (==?.) = qBinOpE (sqlEqTriE (Proxy @a))
  (/=?.) = qBinOpE (sqlNeqTriE (Proxy @a))

-- | Two arbitrary expressions can be quantifiably compared for equality.
instance ( IsSql92ExpressionSyntax syntax, HasSqlQuantifiedEqualityCheck syntax a ) =>
  SqlEqQuantified (QGenExpr context syntax s) (QQuantified syntax s a) (QGenExpr context syntax s a) where

  a ==*. QQuantified q b = qBinOpE (sqlQEqE (Proxy @a) (Just q)) a (QExpr b)
  a /=*. QQuantified q b = qBinOpE (sqlQNeqE (Proxy @a) (Just q)) a (QExpr b)

-- | Constraint synonym to check if two tables can be compared for equality
type HasTableEquality expr tbl =
  (FieldsFulfillConstraint (HasSqlEqualityCheck expr) tbl, Beamable tbl)
type HasTableEqualityNullable expr tbl =
  (FieldsFulfillConstraintNullable (HasSqlEqualityCheck expr) tbl, Beamable tbl)

-- | Compare two arbitrary 'Beamable' types containing 'QGenExpr's for equality.
instance ( IsSql92ExpressionSyntax syntax, FieldsFulfillConstraint (HasSqlEqualityCheck syntax) tbl
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable tbl ) =>
         SqlEq (QGenExpr context syntax s) (tbl (QGenExpr context syntax s)) where

  a ==. b = let (_, e) = runState (zipBeamFieldsM
                                   (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
                                       do modify (\expr ->
                                                    case expr of
                                                      Nothing -> Just $ x ==. y
                                                      Just expr' -> Just $ expr' &&. x ==. y)
                                          return x') (withConstraints @(HasSqlEqualityCheck syntax) `alongsideTable` a) b) Nothing
            in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=. b = not_ (a ==. b)

  a ==?. b = let (_, e) = runState (zipBeamFieldsM
                                    (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
                                        do modify (\expr ->
                                                     case expr of
                                                       Nothing -> Just $ x ==?. y
                                                       Just expr' -> Just $ expr' &&?. x ==?. y)
                                           return x') (withConstraints @(HasSqlEqualityCheck syntax) `alongsideTable` a) b) Nothing
            in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=?. b = sqlNot_ (a ==?. b)

instance ( IsSql92ExpressionSyntax syntax
         , FieldsFulfillConstraintNullable (HasSqlEqualityCheck syntax) tbl
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable tbl)
    => SqlEq (QGenExpr context syntax s) (tbl (Nullable (QGenExpr context syntax s))) where

  a ==. b = let (_, e) = runState (zipBeamFieldsM
                                      (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) -> do
                                          modify (\expr ->
                                                    case expr of
                                                      Nothing -> Just $ x ==. y
                                                      Just expr' -> Just $ expr' &&. x ==. y)
                                          return x')
                                      (withNullableConstraints @(HasSqlEqualityCheck syntax) `alongsideTable` a) b) Nothing
            in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=. b = not_ (a ==. b)

  a ==?. b = let (_, e) = runState (zipBeamFieldsM
                                    (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
                                        do modify (\expr ->
                                                     case expr of
                                                       Nothing -> Just $ x ==?. y
                                                       Just expr' -> Just $ expr' &&?. x ==?. y)
                                           return x') (withNullableConstraints @(HasSqlEqualityCheck syntax) `alongsideTable` a) b) Nothing
            in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=?. b = sqlNot_ (a ==?. b)


-- * Comparisons

-- | Class for expression types or expression containers for which there is a
--   notion of ordering.
--
--   Instances are provided to check the ordering of expressions of the same
--   type. Since there is no universal notion of ordering for an arbitrary
--   number of expressions, no instance is provided for 'Beamable' types.
class SqlOrd expr e | e -> expr where

  (<.), (>.), (<=.), (>=.) :: e -> e -> expr Bool

-- | Class for things which can be /quantifiably/ compared.
class SqlOrd expr e =>
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

instance HasSqlEqualityCheck Expression Text
instance HasSqlEqualityCheck Expression Integer
instance HasSqlEqualityCheck Expression Int
instance HasSqlEqualityCheck Expression Int8
instance HasSqlEqualityCheck Expression Int16
instance HasSqlEqualityCheck Expression Int32
instance HasSqlEqualityCheck Expression Int64
instance HasSqlEqualityCheck Expression Word
instance HasSqlEqualityCheck Expression Word8
instance HasSqlEqualityCheck Expression Word16
instance HasSqlEqualityCheck Expression Word32
instance HasSqlEqualityCheck Expression Word64
instance HasSqlEqualityCheck Expression Double
instance HasSqlEqualityCheck Expression Float
instance HasSqlEqualityCheck Expression Bool
instance HasSqlEqualityCheck Expression UTCTime
instance HasSqlEqualityCheck Expression LocalTime
instance HasSqlEqualityCheck Expression Day
instance HasSqlEqualityCheck Expression TimeOfDay

instance HasSqlQuantifiedEqualityCheck Expression Text
instance HasSqlQuantifiedEqualityCheck Expression Integer
instance HasSqlQuantifiedEqualityCheck Expression Int
instance HasSqlQuantifiedEqualityCheck Expression Int8
instance HasSqlQuantifiedEqualityCheck Expression Int16
instance HasSqlQuantifiedEqualityCheck Expression Int32
instance HasSqlQuantifiedEqualityCheck Expression Int64
instance HasSqlQuantifiedEqualityCheck Expression Word
instance HasSqlQuantifiedEqualityCheck Expression Word8
instance HasSqlQuantifiedEqualityCheck Expression Word16
instance HasSqlQuantifiedEqualityCheck Expression Word32
instance HasSqlQuantifiedEqualityCheck Expression Word64
instance HasSqlQuantifiedEqualityCheck Expression Double
instance HasSqlQuantifiedEqualityCheck Expression Float
instance HasSqlQuantifiedEqualityCheck Expression Bool
instance HasSqlQuantifiedEqualityCheck Expression UTCTime
instance HasSqlQuantifiedEqualityCheck Expression LocalTime
instance HasSqlQuantifiedEqualityCheck Expression Day
instance HasSqlQuantifiedEqualityCheck Expression TimeOfDay

instance HasSqlEqualityCheck SqlSyntaxBuilder Text
instance HasSqlEqualityCheck SqlSyntaxBuilder Integer
instance HasSqlEqualityCheck SqlSyntaxBuilder Int
instance HasSqlEqualityCheck SqlSyntaxBuilder Int8
instance HasSqlEqualityCheck SqlSyntaxBuilder Int16
instance HasSqlEqualityCheck SqlSyntaxBuilder Int32
instance HasSqlEqualityCheck SqlSyntaxBuilder Int64
instance HasSqlEqualityCheck SqlSyntaxBuilder Word
instance HasSqlEqualityCheck SqlSyntaxBuilder Word8
instance HasSqlEqualityCheck SqlSyntaxBuilder Word16
instance HasSqlEqualityCheck SqlSyntaxBuilder Word32
instance HasSqlEqualityCheck SqlSyntaxBuilder Word64
instance HasSqlEqualityCheck SqlSyntaxBuilder Double
instance HasSqlEqualityCheck SqlSyntaxBuilder Float
instance HasSqlEqualityCheck SqlSyntaxBuilder Bool
instance HasSqlEqualityCheck SqlSyntaxBuilder UTCTime
instance HasSqlEqualityCheck SqlSyntaxBuilder LocalTime
instance HasSqlEqualityCheck SqlSyntaxBuilder Day
instance HasSqlEqualityCheck SqlSyntaxBuilder TimeOfDay

instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Text
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Integer
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Int
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Int8
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Int16
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Int32
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Int64
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Word
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Word8
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Word16
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Word32
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Word64
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Double
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Float
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Bool
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder UTCTime
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder LocalTime
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder Day
instance HasSqlQuantifiedEqualityCheck SqlSyntaxBuilder TimeOfDay
