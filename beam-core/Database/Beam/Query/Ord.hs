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
  , unknownAs_, sqlBool_
  , possiblyNullBool_

  , anyOf_, anyIn_
  , allOf_, allIn_

  , between_

  , in_ ) where

import Database.Beam.Syntax
import Database.Beam.Query.Internal
import Database.Beam.Query.Types
import Database.Beam.Query.Operator

import Database.Beam.Schema.Tables
import Database.Beam.Backend.SQL
-- import Database.Beam.Backend.SQL.AST (Expression)
-- import Database.Beam.Backend.SQL.Builder (SqlSyntaxBuilder)

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
data QQuantified s r
  = QQuantified ComparisonQuantifierSyntax (WithExprContext ExpressionSyntax)

-- | Convert a /known not null/ bool to a 'SqlBool'. See 'unknownAs_' for the inverse
sqlBool_ :: QGenExpr context s Bool -> QGenExpr context s SqlBool
sqlBool_ (QExpr s) = QExpr s

-- | SQL @IS TRUE@ operator
isTrue_ :: ExpressionContext context => QGenExpr context s SqlBool -> QGenExpr context s Bool
isTrue_ (QExpr s) = QExpr (fmap isTrueE s)

-- | SQL @IS NOT TRUE@ operator
isNotTrue_ :: ExpressionContext context => QGenExpr context s SqlBool -> QGenExpr context s Bool
isNotTrue_ (QExpr s) = QExpr (fmap isNotTrueE s)

-- | SQL @IS FALSE@ operator
isFalse_ :: ExpressionContext context => QGenExpr context s SqlBool -> QGenExpr context s Bool
isFalse_ (QExpr s) = QExpr (fmap isFalseE s)

-- | SQL @IS NOT FALSE@ operator
isNotFalse_ :: ExpressionContext context => QGenExpr context s SqlBool -> QGenExpr context s Bool
isNotFalse_ (QExpr s) = QExpr (fmap isNotFalseE s)

-- | SQL @IS UNKNOWN@ operator
isUnknown_ :: ExpressionContext context => QGenExpr context s SqlBool -> QGenExpr context s Bool
isUnknown_ (QExpr s) = QExpr (fmap isUnknownE s)

-- | SQL @IS NOT UNKNOWN@ operator
isNotUnknown_ :: ExpressionContext context => QGenExpr context s SqlBool -> QGenExpr context s Bool
isNotUnknown_ (QExpr s) = QExpr (fmap isNotUnknownE s)

-- | Return the first argument if the expression has the unknown SQL value
-- See 'sqlBool_' for the inverse
unknownAs_ :: ExpressionContext context => Bool -> QGenExpr context s SqlBool -> QGenExpr context s Bool
unknownAs_ False = isTrue_ -- If unknown is being treated as false, then return true only if the expression is true
unknownAs_ True  = isNotFalse_ -- If unknown is being treated as true, then return true only if the expression is not false

-- | Retrieve a 'SqlBool' value as a potentially @NULL@ 'Bool'. This
-- is useful if you want to get the value of a SQL boolean expression
-- directly, without having to specify what to do on @UNKNOWN@. Note
-- that both @NULL@ and @UNKNOWN@ will be returned as 'Nothing'.
possiblyNullBool_ :: QGenExpr context s SqlBool -> QGenExpr context s (Maybe Bool)
possiblyNullBool_ (QExpr e) = QExpr e

-- | A 'QQuantified' representing a SQL @ALL(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts a subquery. Use 'allIn_' for an explicit list
allOf_
  :: Q db (QNested s) (QExpr (QNested s) a)
  -> QQuantified s a
allOf_ s = QQuantified quantifyOverAll (\tblPfx -> subqueryE (buildSqlQuery tblPfx s))

-- | A 'QQuantified' representing a SQL @ALL(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts an explicit list of typed expressions. Use 'allOf_' for
--   a subquery
allIn_ :: [QExpr s a] -> QQuantified s a
allIn_ es = QQuantified quantifyOverAll (quantifierListE <$> mapM (\(QExpr e) -> e) es)

-- | A 'QQuantified' representing a SQL @ANY(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts a subquery. Use 'anyIn_' for an explicit list
anyOf_ :: Q db (QNested s) (QExpr (QNested s) a) -> QQuantified s a
anyOf_ s = QQuantified quantifyOverAny (\tblPfx -> subqueryE (buildSqlQuery tblPfx s))

-- | A 'QQuantified' representing a SQL @ANY(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts an explicit list of typed expressions. Use 'anyOf_' for
--   a subquery
anyIn_ :: [QExpr s a] -> QQuantified s a
anyIn_ es = QQuantified quantifyOverAny (quantifierListE <$> mapM (\(QExpr e) -> e) es)

-- | SQL @BETWEEN@ clause
between_ :: ExpressionContext context => QGenExpr context s a -> QGenExpr context s a
         -> QGenExpr context s a -> QGenExpr context s Bool
between_ (QExpr a) (QExpr min_) (QExpr max_) =
  QExpr (liftA3 betweenE a min_ max_)

-- | SQL @IN@ predicate
in_ :: ExpressionContext context => QGenExpr context s a -> [ QGenExpr context s a ] -> QGenExpr context s Bool
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
class HasSqlEqualityCheck a where 
  sqlEqE, sqlNeqE :: Proxy a -> ExpressionSyntax -> ExpressionSyntax -> ExpressionSyntax
  sqlEqE _ = eqE Nothing
  sqlNeqE _ = neqE Nothing

  -- | Tri-state equality
  sqlEqTriE, sqlNeqTriE :: Proxy a -> ExpressionSyntax -> ExpressionSyntax -> ExpressionSyntax
  sqlEqTriE _ = eqE Nothing
  sqlNeqTriE _ = neqE Nothing

type family CanCheckMaybeEquality a :: Constraint where
  CanCheckMaybeEquality (Maybe a) =
    TypeError ('Text "Attempt to check equality of nested Maybe." ':$$:
               'Text "Beam can only reasonably check equality of a single nesting of Maybe.")
  CanCheckMaybeEquality a = ()

instance (HasSqlEqualityCheck a, CanCheckMaybeEquality a) => HasSqlEqualityCheck (Maybe a) where
  sqlEqE _ a b = eqMaybeE a b (sqlEqE (Proxy @a) a b)
  sqlNeqE _ a b = neqMaybeE a b (sqlNeqE (Proxy @a) a b)

instance HasSqlEqualityCheck a => HasSqlEqualityCheck (SqlSerial a) where
  sqlEqE _ = sqlEqE (Proxy @a)
  sqlNeqE _ = sqlNeqE (Proxy @a)

  sqlEqTriE _ = sqlEqTriE (Proxy @a)
  sqlNeqTriE _ = sqlNeqTriE (Proxy @a)

-- | Class for Haskell types that can be compared for quantified equality in the given expression syntax
class HasSqlEqualityCheck a => HasSqlQuantifiedEqualityCheck a where
  sqlQEqE, sqlQNeqE :: Proxy a -> Maybe ComparisonQuantifierSyntax
                    -> ExpressionSyntax -> ExpressionSyntax -> ExpressionSyntax
  sqlQEqE _ = eqE
  sqlQNeqE _ = neqE

instance (HasSqlQuantifiedEqualityCheck a, CanCheckMaybeEquality a) => HasSqlQuantifiedEqualityCheck (Maybe a) where
  sqlQEqE _ = sqlQEqE (Proxy @a)
  sqlQNeqE _ = sqlQNeqE (Proxy @a)

instance HasSqlQuantifiedEqualityCheck a => HasSqlQuantifiedEqualityCheck (SqlSerial a) where
  sqlQEqE _ = sqlQEqE (Proxy @a)
  sqlQNeqE _ = sqlQNeqE (Proxy @a)

-- | Compare two arbitrary expressions (of the same type) for equality
instance ( HasSqlEqualityCheck a, ExpressionContext context ) =>
  SqlEq (QGenExpr context s) (QGenExpr context s a) where

  (==.) = qBinOpE (sqlEqE (Proxy @a))
  (/=.) = qBinOpE (sqlNeqE (Proxy @a))

  (==?.) = qBinOpE (sqlEqTriE (Proxy @a))
  (/=?.) = qBinOpE (sqlNeqTriE (Proxy @a))

-- | Two arbitrary expressions can be quantifiably compared for equality.
instance ( HasSqlQuantifiedEqualityCheck a, ExpressionContext context ) => 
  SqlEqQuantified (QGenExpr context s) (QQuantified s a) (QGenExpr context s a) where

  a ==*. QQuantified q b = qBinOpE (sqlQEqE (Proxy @a) (Just q)) a (QExpr b)
  a /=*. QQuantified q b = qBinOpE (sqlQNeqE (Proxy @a) (Just q)) a (QExpr b)

-- | Constraint synonym to check if two tables can be compared for equality
type HasTableEquality tbl =
  (FieldsFulfillConstraint HasSqlEqualityCheck tbl, Beamable tbl)
type HasTableEqualityNullable expr tbl =
  (FieldsFulfillConstraintNullable HasSqlEqualityCheck tbl, Beamable tbl)

-- | Compare two arbitrary 'Beamable' types containing 'QGenExpr's for equality.
instance ( FieldsFulfillConstraint HasSqlEqualityCheck tbl
         , Beamable tbl
         , ExpressionContext context
         ) =>
         SqlEq (QGenExpr context s) (tbl (QGenExpr context s)) where

--   a ==. b = let (_, e) = runState (zipBeamFieldsM
--                                    (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
--                                        do modify (\expr ->
--                                                     case expr of
--                                                       Nothing -> Just $ x ==. y
--                                                       Just expr' -> Just $ expr' &&. x ==. y)
--                                           return x') (withConstraints @HasSqlEqualityCheck `alongsideTable` a) b) Nothing
--             in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=. b = not_ (a ==. b)

  -- a ==?. b = let (_, e) = runState (zipBeamFieldsM
  --                                   (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
  --                                       do modify (\expr ->
  --                                                    case expr of
  --                                                      Nothing -> Just $ x ==?. y
  --                                                      Just expr' -> Just $ expr' &&?. x ==?. y)
  --                                          return x') (withConstraints @HasSqlEqualityCheck  `alongsideTable` a) b) Nothing
  --           in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=?. b = sqlNot_ (a ==?. b)

instance ( FieldsFulfillConstraintNullable HasSqlEqualityCheck tbl
         , Beamable tbl)
    => SqlEq (QGenExpr context s) (tbl (Nullable (QGenExpr context s))) where

--   a ==. b = let (_, e) = runState (zipBeamFieldsM
--                                       (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) -> do
--                                           modify (\expr ->
--                                                     case expr of
--                                                       Nothing -> Just $ x ==. y
--                                                       Just expr' -> Just $ expr' &&. x ==. y)
--                                           return x')
--                                       (withNullableConstraints @(HasSqlEqualityCheck syntax) `alongsideTable` a) b) Nothing
--             in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
--   a /=. b = not_ (a ==. b)

--   a ==?. b = let (_, e) = runState (zipBeamFieldsM
--                                     (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
--                                         do modify (\expr ->
--                                                      case expr of
--                                                        Nothing -> Just $ x ==?. y
--                                                        Just expr' -> Just $ expr' &&?. x ==?. y)
--                                            return x') (withNullableConstraints @(HasSqlEqualityCheck syntax) `alongsideTable` a) b) Nothing
--             in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
--   a /=?. b = sqlNot_ (a ==?. b)


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

instance ExpressionContext context => SqlOrd (QGenExpr context s) (QGenExpr context s a) where

  (<.) = qBinOpE (ltE Nothing)
  (>.) = qBinOpE (gtE Nothing)
  (<=.) = qBinOpE (leE Nothing)
  (>=.) = qBinOpE (geE Nothing)

instance ExpressionContext context => SqlOrdQuantified (QGenExpr context s) (QQuantified s a) (QGenExpr context s a) where
  a <*. QQuantified q b = qBinOpE (ltE (Just q)) a (QExpr b)
  a <=*. QQuantified q b = qBinOpE (leE (Just q)) a (QExpr b)
  a >*. QQuantified q b = qBinOpE (gtE (Just q)) a (QExpr b)
  a >=*. QQuantified q b = qBinOpE (geE (Just q)) a (QExpr b)
