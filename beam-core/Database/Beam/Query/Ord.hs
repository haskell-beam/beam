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
  ( SqlEq(..), SqlEqQuantified(..), SqlIn(..)
  , HasSqlInTable
  , SqlOrd(..), SqlOrdQuantified(..)
  , QQuantified(..)

  , HasSqlEqualityCheck(..), HasSqlQuantifiedEqualityCheck(..)
  , HasTableEquality, HasTableEqualityNullable

  , isTrue_, isNotTrue_
  , isFalse_, isNotFalse_
  , isUnknown_, isNotUnknown_
  , unknownAs_, sqlBool_
  , possiblyNullBool_
  , fromPossiblyNullBool_

  , anyOf_, anyIn_
  , allOf_, allIn_

  , between_
  ) where

import Database.Beam.Query.Internal
import Database.Beam.Query.Types
import Database.Beam.Query.Operator

import Database.Beam.Schema.Tables
import Database.Beam.Backend.SQL
-- import Database.Beam.Backend.SQL.AST (Expression)
--import Database.Beam.Backend.SQL.Builder (SqlSyntaxBackend)

import Control.Applicative
import Control.Monad.State

import Data.Maybe
import Data.Proxy
import Data.Kind
import Data.Word
import Data.Int
import Data.Tagged
import Data.Text (Text)
import Data.Time (UTCTime, LocalTime, Day, TimeOfDay)

import GHC.TypeLits

-- | A data structure representing the set to quantify a comparison operator over.
data QQuantified be s r
  = QQuantified (BeamSqlBackendExpressionQuantifierSyntax be) (WithExprContext (BeamSqlBackendExpressionSyntax be))

-- | Convert a /known not null/ bool to a 'SqlBool'. See 'unknownAs_' for the inverse
sqlBool_ :: QGenExpr context syntax s Bool -> QGenExpr context syntax s SqlBool
sqlBool_ (QExpr s) = QExpr s

-- | SQL @IS TRUE@ operator
isTrue_ :: BeamSqlBackend be
        => QGenExpr context be s SqlBool -> QGenExpr context be s Bool
isTrue_ (QExpr s) = QExpr (fmap isTrueE s)

-- | SQL @IS NOT TRUE@ operator
isNotTrue_ :: BeamSqlBackend be
           => QGenExpr context be s SqlBool -> QGenExpr context be s Bool
isNotTrue_ (QExpr s) = QExpr (fmap isNotTrueE s)

-- | SQL @IS FALSE@ operator
isFalse_ :: BeamSqlBackend be
         => QGenExpr context be s SqlBool -> QGenExpr context be s Bool
isFalse_ (QExpr s) = QExpr (fmap isFalseE s)

-- | SQL @IS NOT FALSE@ operator
isNotFalse_ :: BeamSqlBackend be
            => QGenExpr context be s SqlBool -> QGenExpr context be s Bool
isNotFalse_ (QExpr s) = QExpr (fmap isNotFalseE s)

-- | SQL @IS UNKNOWN@ operator
isUnknown_ :: BeamSqlBackend be
           => QGenExpr context be s SqlBool -> QGenExpr context be s Bool
isUnknown_ (QExpr s) = QExpr (fmap isUnknownE s)

-- | SQL @IS NOT UNKNOWN@ operator
isNotUnknown_ :: BeamSqlBackend be
              => QGenExpr context be s SqlBool -> QGenExpr context be s Bool
isNotUnknown_ (QExpr s) = QExpr (fmap isNotUnknownE s)

-- | Return the first argument if the expression has the unknown SQL value
-- See 'sqlBool_' for the inverse
unknownAs_ :: BeamSqlBackend be
           => Bool -> QGenExpr context be s SqlBool -> QGenExpr context be s Bool
unknownAs_ False = isTrue_ -- If unknown is being treated as false, then return true only if the expression is true
unknownAs_ True  = isNotFalse_ -- If unknown is being treated as true, then return true only if the expression is not false

-- | Retrieve a 'SqlBool' value as a potentially @NULL@ 'Bool'. This
-- is useful if you want to get the value of a SQL boolean expression
-- directly, without having to specify what to do on @UNKNOWN@. Note
-- that both @NULL@ and @UNKNOWN@ will be returned as 'Nothing'.
possiblyNullBool_ :: QGenExpr context be s SqlBool -> QGenExpr context be s (Maybe Bool)
possiblyNullBool_ (QExpr e) = QExpr e

-- | Convert a possibly @NULL@ 'Bool' to a 'SqlBool'.
fromPossiblyNullBool_ :: QGenExpr context be s (Maybe Bool) -> QGenExpr context be s SqlBool
fromPossiblyNullBool_ (QExpr e) = QExpr e

-- | A 'QQuantified' representing a SQL @ALL(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts a subquery. Use 'allIn_' for an explicit list
allOf_
  :: forall s a be db
   . ( BeamSqlBackend be, HasQBuilder be )
  => Q be db (QNested s) (QExpr be (QNested s) a)
  -> QQuantified be s a
allOf_ s = QQuantified quantifyOverAll (\tblPfx -> subqueryE (buildSqlQuery tblPfx s))

-- | A 'QQuantified' representing a SQL @ALL(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts an explicit list of typed expressions. Use 'allOf_' for
--   a subquery
allIn_
  :: forall s a be
   . BeamSqlBackend be
  => [QExpr be s a]
  -> QQuantified be s a
allIn_ es = QQuantified quantifyOverAll (quantifierListE <$> mapM (\(QExpr e) -> e) es)

-- | A 'QQuantified' representing a SQL @ANY(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts a subquery. Use 'anyIn_' for an explicit list
anyOf_
  :: forall s a be db
   . ( BeamSqlBackend be, HasQBuilder be )
  => Q be db (QNested s) (QExpr be (QNested s) a)
  -> QQuantified be s a
anyOf_ s = QQuantified quantifyOverAny (\tblPfx -> subqueryE (buildSqlQuery tblPfx s))

-- | A 'QQuantified' representing a SQL @ANY(..)@ for use with a
--   <#quantified-comparison-operator quantified comparison operator>
--
--   Accepts an explicit list of typed expressions. Use 'anyOf_' for
--   a subquery
anyIn_
  :: forall s a be
   . BeamSqlBackend be
  => [QExpr be s a]
  -> QQuantified be s a
anyIn_ es = QQuantified quantifyOverAny (quantifierListE <$> mapM (\(QExpr e) -> e) es)

-- | SQL @BETWEEN@ clause
between_ :: BeamSqlBackend be
         => QGenExpr context be s a -> QGenExpr context be s a
         -> QGenExpr context be s a -> QGenExpr context be s Bool
between_ (QExpr a) (QExpr min_) (QExpr max_) =
  QExpr (liftA3 betweenE a min_ max_)

class SqlIn expr a | a -> expr where
  -- | SQL @IN@ predicate
  in_ :: a -> [ a ] -> expr Bool

instance BeamSqlBackend be => SqlIn (QGenExpr context be s) (QGenExpr context be s a) where
  in_ _ [] = QExpr (pure (valueE (sqlValueSyntax False)))
  in_ (QExpr row) options = QExpr (inE <$> row <*> mapM (\(QExpr o) -> o) options)

-- | Class for backends which support SQL @IN@ on lists of tuples, which is not
-- part of ANSI SQL. This is useful for @IN@ on primary keys.
class BeamSqlBackend be => HasSqlInTable be where

instance ( HasSqlInTable be, Beamable table ) =>
  SqlIn (QGenExpr context be s) (table (QGenExpr context be s)) where

  in_ _ [] = QExpr (pure (valueE (sqlValueSyntax False)))
  in_ row options = QExpr (inE <$> toExpr row <*> (mapM toExpr options))
    where toExpr :: table (QGenExpr context be s) -> TablePrefix -> BeamSqlBackendExpressionSyntax be
          toExpr = fmap rowE . sequence . allBeamValues (\(Columnar' (QExpr x)) -> x)

infix 4 `between_`, `in_`

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

-- | Class for Haskell types that can be compared for equality in the given backend
class BeamSqlBackend be => HasSqlEqualityCheck be a where

  sqlEqE, sqlNeqE :: Proxy a -> Proxy be
                  -> BeamSqlBackendExpressionSyntax be
                  -> BeamSqlBackendExpressionSyntax be
                  -> BeamSqlBackendExpressionSyntax be
  sqlEqE _ _ = eqE Nothing
  sqlNeqE _ _ = neqE Nothing

  -- | Tri-state equality
  sqlEqTriE, sqlNeqTriE :: Proxy a -> Proxy be
                        -> BeamSqlBackendExpressionSyntax be
                        -> BeamSqlBackendExpressionSyntax be
                        -> BeamSqlBackendExpressionSyntax be
  sqlEqTriE _ _ = eqE Nothing
  sqlNeqTriE _ _ = neqE Nothing

type family CanCheckMaybeEquality a :: Constraint where
  CanCheckMaybeEquality (Maybe a) =
    TypeError ('Text "Attempt to check equality of nested Maybe." ':$$:
               'Text "Beam can only reasonably check equality of a single nesting of Maybe.")
  CanCheckMaybeEquality a = ()

instance (HasSqlEqualityCheck be a, CanCheckMaybeEquality a) => HasSqlEqualityCheck be (Maybe a) where
  sqlEqE _ _ a b = eqMaybeE a b (sqlEqE (Proxy @a) (Proxy @be) a b)
  sqlNeqE _ _ a b = neqMaybeE a b (sqlNeqE (Proxy @a) (Proxy @be) a b)

instance HasSqlEqualityCheck be a => HasSqlEqualityCheck be (SqlSerial a) where
  sqlEqE _ = sqlEqE (Proxy @a)
  sqlNeqE _ = sqlNeqE (Proxy @a)

  sqlEqTriE _ = sqlEqTriE (Proxy @a)
  sqlNeqTriE _ = sqlNeqTriE (Proxy @a)

-- | Class for Haskell types that can be compared for quantified equality in the given backend
class HasSqlEqualityCheck be a => HasSqlQuantifiedEqualityCheck be a where
  sqlQEqE, sqlQNeqE :: Proxy a -> Proxy be
                    -> Maybe (BeamSqlBackendExpressionQuantifierSyntax be)
                    -> BeamSqlBackendExpressionSyntax be
                    -> BeamSqlBackendExpressionSyntax be
                    -> BeamSqlBackendExpressionSyntax be
  sqlQEqE _ _ = eqE
  sqlQNeqE _ _ = neqE

instance (HasSqlQuantifiedEqualityCheck syntax a, CanCheckMaybeEquality a) => HasSqlQuantifiedEqualityCheck syntax (Maybe a) where
  sqlQEqE _ = sqlQEqE (Proxy @a)
  sqlQNeqE _ = sqlQNeqE (Proxy @a)

instance HasSqlQuantifiedEqualityCheck syntax a => HasSqlQuantifiedEqualityCheck syntax (SqlSerial a) where
  sqlQEqE _ = sqlQEqE (Proxy @a)
  sqlQNeqE _ = sqlQNeqE (Proxy @a)

-- | Compare two arbitrary expressions (of the same type) for equality
instance ( BeamSqlBackend be, HasSqlEqualityCheck be a ) =>
  SqlEq (QGenExpr context be s) (QGenExpr context be s a) where

  (==.) = qBinOpE (sqlEqE (Proxy @a) (Proxy @be))
  (/=.) = qBinOpE (sqlNeqE (Proxy @a) (Proxy @be))

  (==?.) = qBinOpE (sqlEqTriE (Proxy @a) (Proxy @be))
  (/=?.) = qBinOpE (sqlNeqTriE (Proxy @a) (Proxy @be))

-- | Two arbitrary expressions can be quantifiably compared for equality.
instance ( BeamSqlBackend be, HasSqlQuantifiedEqualityCheck be a ) =>
  SqlEqQuantified (QGenExpr context be s) (QQuantified be s a) (QGenExpr context be s a) where

  a ==*. QQuantified q b = qBinOpE (sqlQEqE (Proxy @a) (Proxy @be) (Just q)) a (QExpr b)
  a /=*. QQuantified q b = qBinOpE (sqlQNeqE (Proxy @a) (Proxy @be) (Just q)) a (QExpr b)

-- | Constraint synonym to check if two tables can be compared for equality
type HasTableEquality be tbl =
  (FieldsFulfillConstraint (HasSqlEqualityCheck be) tbl, Beamable tbl)
type HasTableEqualityNullable be tbl =
  (FieldsFulfillConstraintNullable (HasSqlEqualityCheck be) tbl, Beamable tbl)

-- | Compare two arbitrary 'Beamable' types containing 'QGenExpr's for equality.
instance ( BeamSqlBackend be, Beamable tbl
         , FieldsFulfillConstraint (HasSqlEqualityCheck be) tbl ) =>
         SqlEq (QGenExpr context be s) (tbl (QGenExpr context be s)) where

  a ==. b = let (_, e) = runState (zipBeamFieldsM
                                   (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
                                       do modify (\expr ->
                                                    case expr of
                                                      Nothing -> Just $ x ==. y
                                                      Just expr' -> Just $ expr' &&. x ==. y)
                                          return x') (withConstraints @(HasSqlEqualityCheck be) `alongsideTable` a) b) Nothing
            in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=. b = not_ (a ==. b)

  a ==?. b = let (_, e) = runState (zipBeamFieldsM
                                    (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
                                        do modify (\expr ->
                                                     case expr of
                                                       Nothing -> Just $ x ==?. y
                                                       Just expr' -> Just $ expr' &&?. x ==?. y)
                                           return x') (withConstraints @(HasSqlEqualityCheck be) `alongsideTable` a) b) Nothing
            in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=?. b = sqlNot_ (a ==?. b)

instance ( BeamSqlBackend be, Beamable tbl
         , FieldsFulfillConstraintNullable (HasSqlEqualityCheck be) tbl )
    => SqlEq (QGenExpr context be s) (tbl (Nullable (QGenExpr context be s))) where

  a ==. b = let (_, e) = runState (zipBeamFieldsM
                                      (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) -> do
                                          modify (\expr ->
                                                    case expr of
                                                      Nothing -> Just $ x ==. y
                                                      Just expr' -> Just $ expr' &&. x ==. y)
                                          return x')
                                      (withNullableConstraints @(HasSqlEqualityCheck be) `alongsideTable` a) b) Nothing
            in fromMaybe (QExpr (\_ -> valueE (sqlValueSyntax True))) e
  a /=. b = not_ (a ==. b)

  a ==?. b = let (_, e) = runState (zipBeamFieldsM
                                    (\x'@(Columnar' (Columnar' (WithConstraint _) :*: Columnar' x)) (Columnar' y) ->
                                        do modify (\expr ->
                                                     case expr of
                                                       Nothing -> Just $ x ==?. y
                                                       Just expr' -> Just $ expr' &&?. x ==?. y)
                                           return x') (withNullableConstraints @(HasSqlEqualityCheck be) `alongsideTable` a) b) Nothing
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

instance BeamSqlBackend be =>
  SqlOrd (QGenExpr context be s) (QGenExpr context be s a) where

  (<.) = qBinOpE (ltE Nothing)
  (>.) = qBinOpE (gtE Nothing)
  (<=.) = qBinOpE (leE Nothing)
  (>=.) = qBinOpE (geE Nothing)

instance BeamSqlBackend be =>
  SqlOrdQuantified (QGenExpr context be s) (QQuantified be s a) (QGenExpr context be s a) where
  a <*. QQuantified q b = qBinOpE (ltE (Just q)) a (QExpr b)
  a <=*. QQuantified q b = qBinOpE (leE (Just q)) a (QExpr b)
  a >*. QQuantified q b = qBinOpE (gtE (Just q)) a (QExpr b)
  a >=*. QQuantified q b = qBinOpE (geE (Just q)) a (QExpr b)

instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Text
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Integer
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Int
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Int8
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Int16
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Int32
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Int64
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Word
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Word8
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Word16
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Word32
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Word64
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Double
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Float
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Bool
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) UTCTime
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) LocalTime
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) Day
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlEqualityCheck (MockSqlBackend cmd) TimeOfDay
instance ( BeamSqlBackend (MockSqlBackend cmd)
         , HasSqlEqualityCheck (MockSqlBackend cmd) a
         ) => HasSqlEqualityCheck (MockSqlBackend cmd) (Tagged t a)

instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Text
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Integer
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Int
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Int8
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Int16
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Int32
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Int64
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Word
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Word8
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Word16
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Word32
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Word64
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Double
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Float
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Bool
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) UTCTime
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) LocalTime
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) Day
instance BeamSqlBackend (MockSqlBackend cmd) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) TimeOfDay
instance ( BeamSqlBackend (MockSqlBackend cmd)
         , HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) a
         ) => HasSqlQuantifiedEqualityCheck (MockSqlBackend cmd) (Tagged t a)

