{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors#-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Query.Internal where

import           Database.Beam.Syntax
import           Database.Beam.Backend.Types
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema.Tables

import           Data.String
import qualified Data.Text as T
import qualified Data.DList as DList
import           Data.Typeable
import           Data.Vector.Sized (Vector)

import           Control.Monad.Free.Church
import           Control.Monad.State
import           Control.Monad.Writer

import           GHC.TypeLits
import           GHC.Types

type ProjectibleInSelectSyntax a = ( Projectible ExpressionSyntax a, ProjectibleValue a )

type TablePrefix = T.Text

data QF (db :: (* -> *) -> *) s next where
  QDistinct
    :: Projectible ExpressionSyntax r
    => (r -> WithExprContext SelectSetQuantifierSyntax)
    -> QM db s r
    -> (r -> next)
    -> QF db s next
  
  QAll
    :: Beamable table
    => T.Text
    -> TableSettings table
    -> (table (QExpr s) -> Maybe (WithExprContext ExpressionSyntax))
    -> ((T.Text, table (QExpr s)) -> next) -> QF db s next

  QArbitraryJoin
    :: Projectible ExpressionSyntax r
    => QM db (QNested s) r
    -> (FromSyntax -> FromSyntax -> Maybe ExpressionSyntax -> FromSyntax) -> (r -> Maybe (WithExprContext ExpressionSyntax))
    -> (r -> next)
    -> QF db s next

  QTwoWayJoin
    :: (Projectible ExpressionSyntax a, Projectible ExpressionSyntax b)
    => QM db (QNested s) a
    -> QM db (QNested s) b
    -> (FromSyntax -> FromSyntax -> Maybe ExpressionSyntax -> FromSyntax)
    -> ((a, b) -> Maybe (WithExprContext ExpressionSyntax))
    -> ((a, b) -> next)
    -> QF db s next

  QSubSelect
    :: Projectible ExpressionSyntax r
    => QM db (QNested s) r
    -> (r -> next)
    -> QF db s next

  QGuard
    :: WithExprContext ExpressionSyntax
    -> next
    -> QF db s next

  QLimit
    :: Projectible ExpressionSyntax r
    => Integer
    -> QM db (QNested s) r
    -> (r -> next)
    -> QF db s next

  QOffset
    :: Projectible ExpressionSyntax r
    => Integer
    -> QM db (QNested s) r
    -> (r -> next)
    -> QF db s next

  QUnion
    :: Projectible ExpressionSyntax r
    => Bool
    -> QM db (QNested s) r
    -> QM db (QNested s) r
    -> (r -> next)
    -> QF db s next

  QIntersect
    :: Projectible ExpressionSyntax r
    => Bool
    -> QM db (QNested s) r
    -> QM db (QNested s) r
    -> (r -> next)
    -> QF db s next

  QExcept
    :: Projectible ExpressionSyntax r
    => Bool
    -> QM db (QNested s) r
    -> QM db (QNested s) r
    -> (r -> next)
    -> QF db s next

  QOrderBy
    :: Projectible ExpressionSyntax r
    => (r -> WithExprContext [ ExpressionSyntax ])
    -> QM db (QNested s) r
    -> (r -> next)
    -> QF db s next

  QWindowOver
    :: ( ProjectibleWithPredicate WindowFrameContext ExpressionSyntax window
       , Projectible ExpressionSyntax r
       , Projectible ExpressionSyntax a
       )
    => (r -> window)
    -> (r -> window -> a)
    -> QM db (QNested s) r
    -> (a -> next)
    -> QF db s next

  QAggregate
    :: ( Projectible ExpressionSyntax grouping
       , Projectible ExpressionSyntax a
       )
    => (a -> TablePrefix -> (Maybe GroupingSyntax, grouping))
    -> QM db (QNested s) a
    -> (grouping -> next)
    -> QF db s next

  -- Force the building of a select statement, using the given builder
--   QForceSelect :: Projectible (Sql92SelectExpressionSyntax select) r
--                => (r -> Sql92SelectSelectTableSyntax select -> [ Sql92SelectOrderingSyntax select ] ->
--                    Maybe Integer -> Maybe Integer -> select)
--                -> QM select db (QNested s) r
--                -> (r -> next)
--                -> QF select db s next

deriving instance Functor (QF db s)

type QM db s = F (QF db s)

-- | The type of queries over the database `db` returning results of type `a`.
-- The `s` argument is a threading argument meant to restrict cross-usage of
-- `QExpr`s. 'syntax' represents the SQL syntax that this query is building.
newtype Q (db :: (* -> *) -> *) s a
  = Q { runQ :: QM db s a }
    deriving (Monad, Applicative, Functor)

data QInternal
data QNested s

data QField s ty
  = QField
  { qFieldShouldQualify :: Bool
  , qFieldTblName :: T.Text
  , qFieldName    :: T.Text }
  deriving (Show, Eq, Ord)

newtype QAssignment s
  = QAssignment [(FieldNameSyntax, ExpressionSyntax)]
  deriving (Semigroup, Monoid)

-- * QGenExpr type

data QAggregateContext
data QGroupingContext
data QValueContext
data QOrderingContext
data QWindowingContext
data QWindowFrameContext

type family ContextSyntax (context :: *) :: * where
  ContextSyntax QValueContext = ExpressionSyntax
  ContextSyntax QOrderingContext = ExpressionSyntax
  ContextSyntax QAggregateContext = ExpressionSyntax
  ContextSyntax QWindowingContext = ExpressionSyntax
  ContextSyntax QAggregateContext = ExpressionSyntax
  ContextSyntax QGroupingContext = ExpressionSyntax

-- | The type of lifted beam expressions that will yield the haskell type 't'.
--
--   'context' is a type-level representation of the types of expressions this
--   can contain. For example, 'QAggregateContext' represents expressions that
--   may contain aggregates, and 'QWindowingContext' represents expressions that
--   may contain @OVER@.
--
--   'syntax' is the expression syntax being built (usually a type that
--   implements 'IsSql92ExpressionSyntax' at least, but not always).
--
--   's' is a state threading parameter that prevents 'QExpr's from incompatible
--   sources to be combined. For example, this is used to prevent monadic joins
--   from depending on the result of previous joins (so-called @LATERAL@ joins).
newtype QGenExpr context s t = QExpr (TablePrefix -> ContextSyntax context)
type WithExprContext a = TablePrefix -> a

-- | 'QExpr's represent expressions not containing aggregates.
type QExpr = QGenExpr QValueContext
type QAgg = QGenExpr QAggregateContext
type QOrd = QGenExpr QOrderingContext
type QWindowExpr = QGenExpr QWindowingContext
type QWindowFrame = QGenExpr QWindowFrameContext
type QGroupExpr = QGenExpr QGroupingContext
--deriving instance Show syntax => Show (QGenExpr context syntax s t)
instance Eq (ContextSyntax context) => Eq (QGenExpr context s t) where
  QExpr a == QExpr b = a "" == b ""

instance Retaggable (QGenExpr ctxt s) (QGenExpr ctxt s t) where
  type Retag tag (QGenExpr ctxt s t) = Columnar (tag (QGenExpr ctxt s)) t
  retag f e = case f (Columnar' e) of
                Columnar' a -> a

newtype QWindow s = QWindow (WithExprContext ExpressionSyntax)
newtype QFrameBounds = QFrameBounds (Maybe ExpressionSyntax)
newtype QFrameBound = QFrameBound WindowFrameBoundSyntax

class ContextSyntax ctxt ~ ExpressionSyntax => ExpressionContext ctxt
instance ContextSyntax ctxt ~ ExpressionSyntax => ExpressionContext ctxt

qBinOpE :: ExpressionContext context
        => (ExpressionSyntax -> ExpressionSyntax -> ExpressionSyntax)
        -> QGenExpr context s a -> QGenExpr context s b
        -> QGenExpr context s c
qBinOpE mkOpE (QExpr a) (QExpr b) = QExpr (mkOpE <$> a <*> b)

unsafeRetype :: QGenExpr ctxt s a -> QGenExpr ctxt s a'
unsafeRetype (QExpr v) = QExpr v

instance ( HasSqlValueSyntax [Char], ContextSyntax context ~ ExpressionSyntax ) =>
    IsString (QGenExpr context s T.Text) where
    fromString = QExpr . pure . valueE . sqlValueSyntax
instance (Num a, HasSqlValueSyntax a, ContextSyntax context ~ ExpressionSyntax) =>
    Num (QGenExpr context s a) where
    fromInteger x = let res :: QGenExpr context s a
                        res = QExpr (pure (valueE (sqlValueSyntax (fromIntegral x :: a))))
                    in res
    QExpr a + QExpr b = QExpr (addE <$> a <*> b)
    QExpr a - QExpr b = QExpr (subE <$> a <*> b)
    QExpr a * QExpr b = QExpr (mulE <$> a <*> b)
    negate (QExpr a) = QExpr (negateE <$> a)
    abs (QExpr x) = QExpr (absE <$> x)
    signum _ = error "signum: not defined for QExpr. Use CASE...WHEN"

instance ( Fractional a
         , HasSqlValueSyntax a
         , ContextSyntax context ~ ExpressionSyntax ) =>
  Fractional (QGenExpr context s a) where

  QExpr a / QExpr b = QExpr (divE <$> a <*> b)
  recip = (1.0 /)

  fromRational = QExpr . pure . valueE . sqlValueSyntax . (id :: a -> a) . fromRational

-- * Aggregations

data Aggregation s a
  = GroupAgg ()
  | ProjectAgg ()

-- * Sql Projections
--

-- | Typeclass for all haskell data types that can be used to create a projection in a SQL select
-- statement. This includes all tables as well as all tuple classes. Projections are only defined on
-- tuples up to size 5. If you need more, follow the implementations here.

class Typeable context => AggregateContext context
instance (IsAggregateContext a, Typeable a) => AggregateContext a

type family ContextName a :: Symbol
type instance ContextName QValueContext = "a value"
type instance ContextName QWindowingContext = "a window expression"
type instance ContextName QWindowFrameContext = "a window frame"
type instance ContextName QOrderingContext = "an ordering expression"
type instance ContextName QAggregateContext = "an aggregate"
type instance ContextName QGroupingContext = "an aggregate grouping"

type family IsAggregateContext a :: Constraint where
    IsAggregateContext QAggregateContext = ()
    IsAggregateContext QGroupingContext = ()
    IsAggregateContext a = TypeError ('Text "Non-aggregate expression where aggregate expected." :$$:
                                      ('Text "Got " :<>: 'Text (ContextName a) :<>: 'Text ". Expected an aggregate or a grouping") :$$:
                                      AggregateContextSuggestion a)

type family AggregateContextSuggestion a :: ErrorMessage where
    AggregateContextSuggestion QValueContext = 'Text "Perhaps you forgot to wrap a value expression with 'group_'"
    AggregateContextSuggestion QWindowingContext = 'Text "Perhaps you meant to use 'window_' instead of 'aggregate_'"
    AggregateContextSuggestion QOrderingContext = 'Text "You cannot use an ordering in an aggregate"
    AggregateContextSuggestion b = 'Text ""

class Typeable context => ValueContext context
instance (IsValueContext a, Typeable a, a ~ QValueContext) => ValueContext a

class Typeable context => WindowFrameContext context
instance (Typeable context, IsWindowFrameContext context, context ~ QWindowFrameContext) =>
  WindowFrameContext context

type family IsWindowFrameContext a :: Constraint where
  IsWindowFrameContext QWindowFrameContext = ()
  IsWindowFrameContext a = TypeError ('Text "Expected window frame." :$$:
                                      ('Text "Got " :<>: 'Text (ContextName a) :<>: 'Text ". Expected a window frame"))

class AnyType a
instance AnyType a

type family IsValueContext a :: Constraint where
    IsValueContext QValueContext = ()
    IsValueContext a = TypeError ('Text "Non-scalar context in projection" :$$:
                                  ('Text "Got " :<>: 'Text (ContextName a) :<>: 'Text ". Expected a value") :$$:
                                  ValueContextSuggestion a)

type family ValueContextSuggestion a :: ErrorMessage where
    ValueContextSuggestion QWindowingContext = 'Text "Use 'window_' to projecct aggregate expressions to the value level"
    ValueContextSuggestion QOrderingContext = 'Text "An ordering context cannot be used in a projection. Try removing the 'asc_' or 'desc_', or use 'orderBy_' to sort the result set"
    ValueContextSuggestion QAggregateContext = ('Text "Aggregate functions and groupings cannot be contained in value expressions." :$$:
                                                'Text "Use 'aggregate_' to compute aggregations at the value level.")
    ValueContextSuggestion QGroupingContext = ValueContextSuggestion QAggregateContext
    ValueContextSuggestion _ = 'Text ""

type Projectible = ProjectibleWithPredicate AnyType
type ProjectibleValue = ProjectibleWithPredicate ValueContext ExpressionSyntax

class ThreadRewritable (s :: *) (a :: *) | a -> s where
  type WithRewrittenThread s (s' :: *) a :: *

  rewriteThread :: Proxy s' -> a -> WithRewrittenThread s s' a
instance Beamable tbl => ThreadRewritable s (tbl (QGenExpr ctxt s)) where
  type WithRewrittenThread s s' (tbl (QGenExpr ctxt s)) = tbl (QGenExpr ctxt s')
  rewriteThread _ = changeBeamRep (\(Columnar' (QExpr a)) -> Columnar' (QExpr a))
instance Beamable tbl => ThreadRewritable s (tbl (Nullable (QGenExpr ctxt s))) where
  type WithRewrittenThread s s' (tbl (Nullable (QGenExpr ctxt s))) = tbl (Nullable (QGenExpr ctxt s'))
  rewriteThread _ = changeBeamRep (\(Columnar' (QExpr a)) -> Columnar' (QExpr a))
instance ThreadRewritable s (QGenExpr ctxt s a) where
  type WithRewrittenThread s s' (QGenExpr ctxt s a) = QGenExpr ctxt s' a
  rewriteThread _ (QExpr a) = QExpr a
instance ThreadRewritable s a => ThreadRewritable s [a] where
  type WithRewrittenThread s s' [a] = [WithRewrittenThread s s' a]
  rewriteThread s' qs = map (rewriteThread s') qs
instance (ThreadRewritable s a, KnownNat n) => ThreadRewritable s (Vector n a) where
  type WithRewrittenThread s s' (Vector n a) = Vector n (WithRewrittenThread s s' a)
  rewriteThread s' qs = fmap (rewriteThread s') qs
instance ( ThreadRewritable s a, ThreadRewritable s b ) =>
  ThreadRewritable s (a, b) where
  type WithRewrittenThread s s' (a, b) = (WithRewrittenThread s s' a, WithRewrittenThread s s' b)
  rewriteThread s' (a, b) = (rewriteThread s' a, rewriteThread s' b)
instance ( ThreadRewritable s a, ThreadRewritable s b, ThreadRewritable s c ) =>
  ThreadRewritable s (a, b, c) where
  type WithRewrittenThread s s' (a, b, c) =
    (WithRewrittenThread s s' a, WithRewrittenThread s s' b, WithRewrittenThread s s' c)
  rewriteThread s' (a, b, c) = (rewriteThread s' a, rewriteThread s' b, rewriteThread s' c)
instance ( ThreadRewritable s a, ThreadRewritable s b, ThreadRewritable s c, ThreadRewritable s d ) =>
  ThreadRewritable s (a, b, c, d) where
  type WithRewrittenThread s s' (a, b, c, d) =
    (WithRewrittenThread s s' a, WithRewrittenThread s s' b, WithRewrittenThread s s' c, WithRewrittenThread s s' d)
  rewriteThread s' (a, b, c, d) =
    (rewriteThread s' a, rewriteThread s' b, rewriteThread s' c, rewriteThread s' d)
instance ( ThreadRewritable s a, ThreadRewritable s b, ThreadRewritable s c, ThreadRewritable s d
         , ThreadRewritable s e ) =>
  ThreadRewritable s (a, b, c, d, e) where
  type WithRewrittenThread s s' (a, b, c, d, e) =
    ( WithRewrittenThread s s' a, WithRewrittenThread s s' b, WithRewrittenThread s s' c, WithRewrittenThread s s' d
    , WithRewrittenThread s s' e )
  rewriteThread s' (a, b, c, d, e) =
    ( rewriteThread s' a, rewriteThread s' b, rewriteThread s' c, rewriteThread s' d
    , rewriteThread s' e)
instance ( ThreadRewritable s a, ThreadRewritable s b, ThreadRewritable s c, ThreadRewritable s d
         , ThreadRewritable s e, ThreadRewritable s f ) =>
  ThreadRewritable s (a, b, c, d, e, f) where
  type WithRewrittenThread s s' (a, b, c, d, e, f) =
    ( WithRewrittenThread s s' a, WithRewrittenThread s s' b, WithRewrittenThread s s' c, WithRewrittenThread s s' d
    , WithRewrittenThread s s' e, WithRewrittenThread s s' f )
  rewriteThread s' (a, b, c, d, e, f) =
    ( rewriteThread s' a, rewriteThread s' b, rewriteThread s' c, rewriteThread s' d
    , rewriteThread s' e, rewriteThread s' f)
instance ( ThreadRewritable s a, ThreadRewritable s b, ThreadRewritable s c, ThreadRewritable s d
         , ThreadRewritable s e, ThreadRewritable s f, ThreadRewritable s g ) =>
  ThreadRewritable s (a, b, c, d, e, f, g) where
  type WithRewrittenThread s s' (a, b, c, d, e, f, g) =
    ( WithRewrittenThread s s' a, WithRewrittenThread s s' b, WithRewrittenThread s s' c, WithRewrittenThread s s' d
    , WithRewrittenThread s s' e, WithRewrittenThread s s' f, WithRewrittenThread s s' g)
  rewriteThread s' (a, b, c, d, e, f, g) =
    ( rewriteThread s' a, rewriteThread s' b, rewriteThread s' c, rewriteThread s' d
    , rewriteThread s' e, rewriteThread s' f, rewriteThread s' g )
instance ( ThreadRewritable s a, ThreadRewritable s b, ThreadRewritable s c, ThreadRewritable s d
         , ThreadRewritable s e, ThreadRewritable s f, ThreadRewritable s g, ThreadRewritable s h ) =>
  ThreadRewritable s (a, b, c, d, e, f, g, h) where
  type WithRewrittenThread s s' (a, b, c, d, e, f, g, h) =
    ( WithRewrittenThread s s' a, WithRewrittenThread s s' b, WithRewrittenThread s s' c, WithRewrittenThread s s' d
    , WithRewrittenThread s s' e, WithRewrittenThread s s' f, WithRewrittenThread s s' g, WithRewrittenThread s s' h)
  rewriteThread s' (a, b, c, d, e, f, g, h) =
    ( rewriteThread s' a, rewriteThread s' b, rewriteThread s' c, rewriteThread s' d
    , rewriteThread s' e, rewriteThread s' f, rewriteThread s' g, rewriteThread s' h )

class ContextRewritable a where
  type WithRewrittenContext a ctxt :: *
  type ContextCompatible a ctxt :: Constraint

  rewriteContext :: ContextCompatible a ctxt => Proxy ctxt -> a -> WithRewrittenContext a ctxt
instance Beamable tbl => ContextRewritable (tbl (QGenExpr old s)) where
  type WithRewrittenContext (tbl (QGenExpr old s)) ctxt = tbl (QGenExpr ctxt s)
  type ContextCompatible (tbl (QGenExpr old s)) ctxt = ContextSyntax old ~ ContextSyntax ctxt
  rewriteContext _ = changeBeamRep (\(Columnar' (QExpr a)) -> Columnar' (QExpr a))
instance Beamable tbl => ContextRewritable (tbl (Nullable (QGenExpr old s))) where
  type WithRewrittenContext (tbl (Nullable (QGenExpr old s))) ctxt = tbl (Nullable (QGenExpr ctxt s))
  type ContextCompatible (tbl (Nullable (QGenExpr old s))) ctxt = ContextSyntax old ~ ContextSyntax ctxt
  rewriteContext _ = changeBeamRep (\(Columnar' (QExpr a)) -> Columnar' (QExpr a))
instance ContextRewritable (QGenExpr old s a) where
  type WithRewrittenContext (QGenExpr old s a) ctxt = QGenExpr ctxt s a
  type ContextCompatible (QGenExpr old s a) ctxt = ContextSyntax old ~ ContextSyntax ctxt
  rewriteContext _ (QExpr a) = QExpr a
instance ContextRewritable a => ContextRewritable [a] where
  type WithRewrittenContext [a] ctxt = [ WithRewrittenContext a ctxt ]
  type ContextCompatible [a] ctxt = ContextCompatible a ctxt
  rewriteContext p as = map (rewriteContext p) as
instance (ContextRewritable a, KnownNat n) => ContextRewritable (Vector n a) where
  type WithRewrittenContext (Vector n a) ctxt = Vector n (WithRewrittenContext a ctxt)
  type ContextCompatible (Vector n a) ctxt = ContextCompatible a ctxt
  rewriteContext p as = fmap (rewriteContext p) as
instance (ContextRewritable a, ContextRewritable b) => ContextRewritable (a, b) where
  type WithRewrittenContext (a, b) ctxt = (WithRewrittenContext a ctxt, WithRewrittenContext b ctxt)
  type ContextCompatible (a, b) ctxt =
    ( ContextCompatible a ctxt
    , ContextCompatible b ctxt
    )
  rewriteContext p (a, b) = (rewriteContext p a, rewriteContext p b)
instance (ContextRewritable a, ContextRewritable b, ContextRewritable c) => ContextRewritable (a, b, c) where
  type WithRewrittenContext (a, b, c) ctxt = (WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt)
  type ContextCompatible (a, b, c) ctxt =
    ( ContextCompatible a ctxt
    , ContextCompatible b ctxt
    , ContextCompatible c ctxt
    )
  rewriteContext p (a, b, c) = (rewriteContext p a, rewriteContext p b, rewriteContext p c)
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d ) => ContextRewritable (a, b, c, d) where
  type WithRewrittenContext (a, b, c, d) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt )
  type ContextCompatible (a, b, c, d) ctxt =
    ( ContextCompatible a ctxt
    , ContextCompatible b ctxt
    , ContextCompatible c ctxt
    , ContextCompatible d ctxt
    )
  rewriteContext p (a, b, c, d) = ( rewriteContext p a, rewriteContext p b, rewriteContext p c
                                  , rewriteContext p d )
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d, ContextRewritable e ) =>
    ContextRewritable (a, b, c, d, e) where
  type WithRewrittenContext (a, b, c, d, e) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt, WithRewrittenContext e ctxt )
  type ContextCompatible (a, b, c, d, e) ctxt =
    ( ContextCompatible a ctxt
    , ContextCompatible b ctxt
    , ContextCompatible c ctxt
    , ContextCompatible d ctxt
    , ContextCompatible e ctxt
    )
  rewriteContext p (a, b, c, d, e) = ( rewriteContext p a, rewriteContext p b, rewriteContext p c
                                     , rewriteContext p d, rewriteContext p e )
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d, ContextRewritable e, ContextRewritable f ) =>
    ContextRewritable (a, b, c, d, e, f) where
  type WithRewrittenContext (a, b, c, d, e, f) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt, WithRewrittenContext e ctxt, WithRewrittenContext f ctxt )
  type ContextCompatible (a, b, c, d, e, f) ctxt =
    ( ContextCompatible a ctxt
    , ContextCompatible b ctxt
    , ContextCompatible c ctxt
    , ContextCompatible d ctxt
    , ContextCompatible e ctxt
    , ContextCompatible f ctxt
    )
  rewriteContext p (a, b, c, d, e, f) = ( rewriteContext p a, rewriteContext p b, rewriteContext p c
                                        , rewriteContext p d, rewriteContext p e, rewriteContext p f )
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d, ContextRewritable e, ContextRewritable f
         , ContextRewritable g ) =>
    ContextRewritable (a, b, c, d, e, f, g) where
  type WithRewrittenContext (a, b, c, d, e, f, g) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt, WithRewrittenContext e ctxt, WithRewrittenContext f ctxt
      , WithRewrittenContext g ctxt )
  type ContextCompatible (a, b, c, d, e, f, g) ctxt =
    ( ContextCompatible a ctxt
    , ContextCompatible b ctxt
    , ContextCompatible c ctxt
    , ContextCompatible d ctxt
    , ContextCompatible e ctxt
    , ContextCompatible f ctxt
    , ContextCompatible g ctxt
    )
  rewriteContext p (a, b, c, d, e, f, g) =
    ( rewriteContext p a, rewriteContext p b, rewriteContext p c
    , rewriteContext p d, rewriteContext p e, rewriteContext p f
    , rewriteContext p g )
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d, ContextRewritable e, ContextRewritable f
         , ContextRewritable g, ContextRewritable h
         ) =>
    ContextRewritable (a, b, c, d, e, f, g, h) where
  type WithRewrittenContext (a, b, c, d, e, f, g, h) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt, WithRewrittenContext e ctxt, WithRewrittenContext f ctxt
      , WithRewrittenContext g ctxt, WithRewrittenContext h ctxt
      )
  type ContextCompatible (a, b, c, d, e, f, g, h) ctxt =
    ( ContextCompatible a ctxt
    , ContextCompatible b ctxt
    , ContextCompatible c ctxt
    , ContextCompatible d ctxt
    , ContextCompatible e ctxt
    , ContextCompatible f ctxt
    , ContextCompatible g ctxt
    , ContextCompatible h ctxt
    )
  rewriteContext p (a, b, c, d, e, f, g, h) =
    ( rewriteContext p a, rewriteContext p b, rewriteContext p c
    , rewriteContext p d, rewriteContext p e, rewriteContext p f
    , rewriteContext p g, rewriteContext p h )

class ProjectibleWithPredicate (contextPredicate :: * -> Constraint) syntax a | a -> syntax where
  project' :: Monad m => Proxy contextPredicate
           -> (forall context. contextPredicate context => Proxy context -> WithExprContext syntax -> m (WithExprContext syntax))
           -> a -> m a
instance (Beamable t, contextPredicate context, ContextSyntax context ~ syn) => ProjectibleWithPredicate contextPredicate syn (t (QGenExpr context s)) where
  project' _ mutateM a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ ->
                      Columnar' . QExpr <$> mutateM (Proxy @context) e) a a
-- instance (Beamable t, contextPredicate context) => ProjectibleWithPredicate contextPredicate (t (Nullable (QGenExpr context s))) where
--   project' _ mutateM a =
--     zipBeamFieldsM (\(Columnar' (QExpr e)) _ ->
--                       Columnar' . QExpr <$> mutateM (Proxy @context) e) a a
-- instance ProjectibleWithPredicate WindowFrameContext (QWindow s) where
--   project' _ mutateM (QWindow a) =
--     QWindow <$> mutateM (Proxy @QWindowFrameContext) a
instance (contextPredicate context, syn ~ ContextSyntax context) => ProjectibleWithPredicate contextPredicate syn (QGenExpr context s a) where
  project' _ mkE (QExpr a) = QExpr <$> mkE (Proxy @context) a
-- instance ProjectibleWithPredicate contextPredicate a => ProjectibleWithPredicate contextPredicate [a] where
--   project' p mkE as = traverse (project' p mkE) as
-- instance (ProjectibleWithPredicate contextPredicate a, KnownNat n) => ProjectibleWithPredicate contextPredicate (Vector n a) where
--   project' p mkE as = traverse (project' p mkE) as
instance ( ProjectibleWithPredicate contextPredicate syn a, ProjectibleWithPredicate contextPredicate syn b ) =>
  ProjectibleWithPredicate contextPredicate syn (a, b) where

  project' p mkE (a, b) = (,) <$> project' p mkE a <*> project' p mkE b
-- instance ( ProjectibleWithPredicate contextPredicate a, ProjectibleWithPredicate contextPredicate b, ProjectibleWithPredicate contextPredicate c ) =>
--   ProjectibleWithPredicate contextPredicate (a, b, c) where

--   project' p mkE (a, b, c) = (,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
-- instance ( ProjectibleWithPredicate contextPredicate a, ProjectibleWithPredicate contextPredicate b, ProjectibleWithPredicate contextPredicate c
--          , ProjectibleWithPredicate contextPredicate d ) =>
--   ProjectibleWithPredicate contextPredicate (a, b, c, d) where

--   project' p mkE (a, b, c, d) = (,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
--                                       <*> project' p mkE d
-- instance ( ProjectibleWithPredicate contextPredicate a, ProjectibleWithPredicate contextPredicate b, ProjectibleWithPredicate contextPredicate c
--          , ProjectibleWithPredicate contextPredicate d, ProjectibleWithPredicate contextPredicate e ) =>
--   ProjectibleWithPredicate contextPredicate (a, b, c, d, e) where

--   project' p mkE (a, b, c, d, e) = (,,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
--                                           <*> project' p mkE d <*> project' p mkE e
-- instance ( ProjectibleWithPredicate contextPredicate a, ProjectibleWithPredicate contextPredicate b, ProjectibleWithPredicate contextPredicate c
--          , ProjectibleWithPredicate contextPredicate d, ProjectibleWithPredicate contextPredicate e, ProjectibleWithPredicate contextPredicate f ) =>
--   ProjectibleWithPredicate contextPredicate (a, b, c, d, e, f) where

--   project' p mkE (a, b, c, d, e, f) = (,,,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
--                                               <*> project' p mkE d <*> project' p mkE e <*> project' p mkE f
-- instance ( ProjectibleWithPredicate contextPredicate a, ProjectibleWithPredicate contextPredicate b, ProjectibleWithPredicate contextPredicate c
--          , ProjectibleWithPredicate contextPredicate d, ProjectibleWithPredicate contextPredicate e, ProjectibleWithPredicate contextPredicate f
--          , ProjectibleWithPredicate contextPredicate g ) =>
--   ProjectibleWithPredicate contextPredicate (a, b, c, d, e, f, g) where

--   project' p mkE (a, b, c, d, e, f, g) =
--     (,,,,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
--              <*> project' p mkE d <*> project' p mkE e <*> project' p mkE f
--              <*> project' p mkE g
-- instance ( ProjectibleWithPredicate contextPredicate x a, ProjectibleWithPredicate contextPredicate x b, ProjectibleWithPredicate contextPredicate x c
--          , ProjectibleWithPredicate contextPredicate x d, ProjectibleWithPredicate contextPredicate x e, ProjectibleWithPredicate contextPredicate x f
--          , ProjectibleWithPredicate contextPredicate x g, ProjectibleWithPredicate contextPredicate x h ) =>
--   ProjectibleWithPredicate contextPredicate x (a, b, c, d, e, f, g, h) where

--   project' p mkE (a, b, c, d, e, f, g, h) =
--     (,,,,,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
--               <*> project' p mkE d <*> project' p mkE e <*> project' p mkE f
--               <*> project' p mkE g <*> project' p mkE h

-- instance Beamable t => ProjectibleWithPredicate AnyType (t (QField s)) where
--   project' _ mutateM a =
--     zipBeamFieldsM (\(Columnar' f) _ ->
--                       Columnar' <$> project' (Proxy @AnyType) mutateM f) a a
-- instance Beamable t => ProjectibleWithPredicate AnyType T.Text (t (Nullable (QField s))) where
--   project' _ mutateM a =
--     zipBeamFieldsM (\(Columnar' f) _ ->
--                       Columnar' <$> project' (Proxy @AnyType) mutateM f) a a
instance ProjectibleWithPredicate AnyType T.Text (QField s a) where
  project' _ mutateM (QField q tbl f) =
    fmap (\f' -> QField q tbl (f' ""))
         (mutateM (Proxy @(QField s a)) (\_ -> f)) -- This is kind of a hack

project :: (Projectible b a) => a -> WithExprContext [b]
project = sequenceA . DList.toList . execWriter . project' (Proxy @AnyType) (\_ e -> tell (DList.singleton e) >> pure e)

reproject :: (Projectible b a) => (Int -> b) -> a -> a
reproject mkField a =
  evalState (project' (Proxy @AnyType) (\_ _ -> state (\i -> (i, i + 1)) >>= pure . pure . mkField) a) 0
