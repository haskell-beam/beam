{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors#-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Database.Beam.Query.Internal where

import           Database.Beam.Backend.Types
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema.Tables

import qualified Data.DList as DList
import           Data.Functor.Const
import           Data.String
import qualified Data.Text as T
import           Data.Typeable
import           Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as VS
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import           Control.Monad.Free.Church
import           Control.Monad.State
import           Control.Monad.Writer

import           GHC.TypeLits
import           GHC.Types

import           Unsafe.Coerce

type ProjectibleInBackend be a =
  ( Projectible be a
  , ProjectibleValue be a )

type TablePrefix = T.Text

data QF be (db :: (* -> *) -> *) s next where
  QDistinct :: Projectible be r
            => (r -> WithExprContext (BeamSqlBackendSetQuantifierSyntax be))
            -> QM be db s r -> (r -> next) -> QF be db s next

  QAll :: Projectible be r
       => (TablePrefix -> T.Text -> BeamSqlBackendFromSyntax be)
       -> (T.Text -> r)
       -> (r -> Maybe (WithExprContext (BeamSqlBackendExpressionSyntax be)))
       -> ((T.Text, r) -> next) -> QF be db s next

  QArbitraryJoin :: Projectible be r
                 => QM be db (QNested s) r
                 -> (BeamSqlBackendFromSyntax be -> BeamSqlBackendFromSyntax be ->
                     Maybe (BeamSqlBackendExpressionSyntax be) ->
                     BeamSqlBackendFromSyntax be)
                 -> (r -> Maybe (WithExprContext (BeamSqlBackendExpressionSyntax be)))
                 -> (r -> next)
                 -> QF be db s next
  QTwoWayJoin :: ( Projectible be a
                 , Projectible be b )
              => QM be db (QNested s) a
              -> QM be db (QNested s) b
              -> (BeamSqlBackendFromSyntax be -> BeamSqlBackendFromSyntax be ->
                  Maybe (BeamSqlBackendExpressionSyntax be) ->
                  BeamSqlBackendFromSyntax be)
              -> ((a, b) -> Maybe (WithExprContext (BeamSqlBackendExpressionSyntax be)))
              -> ((a, b) -> next)
              -> QF be db s next

  QSubSelect :: Projectible be r
             => QM be db (QNested s) r -> (r -> next)
             -> QF be db s next

  QGuard :: WithExprContext (BeamSqlBackendExpressionSyntax be) -> next -> QF be db s next

  QLimit  :: Projectible be r => Integer -> QM be db (QNested s) r -> (r -> next) -> QF be db s next
  QOffset :: Projectible be r => Integer -> QM be db (QNested s) r -> (r -> next) -> QF be db s next

  QSetOp :: Projectible be r
         => (BeamSqlBackendSelectTableSyntax be -> BeamSqlBackendSelectTableSyntax be -> BeamSqlBackendSelectTableSyntax be)
         -> QM be db (QNested s) r
         -> QM be db (QNested s) r -> (r -> next)
         -> QF be db s next

  QOrderBy :: Projectible be r
           => (r -> WithExprContext [ BeamSqlBackendOrderingSyntax be ])
           -> QM be db (QNested s) r -> (r -> next) -> QF be db s next

  QWindowOver :: ( ProjectibleWithPredicate WindowFrameContext be (WithExprContext (BeamSqlBackendWindowFrameSyntax' be)) window
                 , Projectible be r
                 , Projectible be a )
              => (r -> window) -> (r -> window -> a)
              -> QM be db (QNested s) r -> (a -> next) -> QF be db s next

  QAggregate :: ( Projectible be grouping
                , Projectible be a )
             => (a -> TablePrefix -> (Maybe (BeamSqlBackendGroupingSyntax be), grouping))
             -> QM be db (QNested s) a
             -> (grouping -> next)
             -> QF be db s next

  -- Force the building of a select statement, using the given builder
  QForceSelect :: Projectible be r
               => (r -> BeamSqlBackendSelectTableSyntax be -> [ BeamSqlBackendOrderingSyntax be ] ->
                   Maybe Integer -> Maybe Integer -> BeamSqlBackendSelectSyntax be)
               -> QM be db (QNested s) r
               -> (r -> next)
               -> QF be db s next

deriving instance Functor (QF be db s)

type QM be db s = F (QF be db s)

-- | The type of queries over the database `db` returning results of type `a`.
-- The `s` argument is a threading argument meant to restrict cross-usage of
-- `QExpr`s. 'syntax' represents the SQL syntax that this query is building.
newtype Q be (db :: (* -> *) -> *) s a
  = Q { runQ :: QM be db s a }
    deriving (Monad, Applicative, Functor)

data QInternal
data QNested s

data QField s ty
  = QField
  { qFieldShouldQualify :: !Bool
  , qFieldTblName       :: !T.Text
  , qFieldName          :: !T.Text }
  deriving (Show, Eq, Ord)

newtype QAssignment be s
  = QAssignment { unQAssignment :: [(BeamSqlBackendFieldNameSyntax be, BeamSqlBackendExpressionSyntax be)] }
  deriving (Monoid, Semigroup)

newtype QFieldAssignment be tbl a
  = QFieldAssignment (forall s. tbl (QExpr be s) -> Maybe (QExpr be s a))

-- * QGenExpr type

data QAggregateContext
data QGroupingContext
data QValueContext
data QWindowingContext
data QWindowFrameContext

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
newtype QGenExpr context be s t = QExpr (TablePrefix -> BeamSqlBackendExpressionSyntax be)
newtype QOrd be s t = QOrd (TablePrefix -> BeamSqlBackendOrderingSyntax be)

type WithExprContext a = TablePrefix -> a

-- | 'QExpr's represent expressions not containing aggregates.
type QExpr = QGenExpr QValueContext
type QAgg = QGenExpr QAggregateContext
type QWindowExpr = QGenExpr QWindowingContext
type QGroupExpr = QGenExpr QGroupingContext
--deriving instance Show syntax => Show (QGenExpr context syntax s t)
instance BeamSqlBackend be => Eq (QGenExpr context be s t) where
  QExpr a == QExpr b = a "" == b ""

instance Retaggable (QGenExpr ctxt expr s) (QGenExpr ctxt expr s t) where
  type Retag tag (QGenExpr ctxt expr s t) = Columnar (tag (QGenExpr ctxt expr s)) t
  retag f e = case f (Columnar' e) of
                Columnar' a -> a

newtype QWindow be s = QWindow (WithExprContext (BeamSqlBackendWindowFrameSyntax be))
newtype QFrameBounds be = QFrameBounds (Maybe (BeamSqlBackendWindowFrameBoundsSyntax be))
newtype QFrameBound be = QFrameBound (BeamSqlBackendWindowFrameBoundSyntax be)

qBinOpE :: BeamSqlBackend be
        => (BeamSqlBackendExpressionSyntax be ->
            BeamSqlBackendExpressionSyntax be ->
            BeamSqlBackendExpressionSyntax be)
        -> QGenExpr context be s a -> QGenExpr context be s b
        -> QGenExpr context be s c
qBinOpE mkOpE (QExpr a) (QExpr b) = QExpr (mkOpE <$> a <*> b)

unsafeRetype :: QGenExpr ctxt be s a -> QGenExpr ctxt be s a'
unsafeRetype (QExpr v) = QExpr v

instance ( BeamSqlBackend backend, BeamSqlBackendCanSerialize backend [Char] ) =>
    IsString (QGenExpr context backend s T.Text) where
    fromString = QExpr . pure . valueE . sqlValueSyntax
instance ( Num a, BeamSqlBackend be, BeamSqlBackendCanSerialize be a ) =>
    Num (QGenExpr context be s a) where
    fromInteger x = let res :: QGenExpr context be s a
                        res = QExpr (pure (valueE (sqlValueSyntax (fromIntegral x :: a))))
                    in res
    QExpr a + QExpr b = QExpr (addE <$> a <*> b)
    QExpr a - QExpr b = QExpr (subE <$> a <*> b)
    QExpr a * QExpr b = QExpr (mulE <$> a <*> b)
    negate (QExpr a) = QExpr (negateE <$> a)
    abs (QExpr x) = QExpr (absE <$> x)
    signum _ = error "signum: not defined for QExpr. Use CASE...WHEN"

instance ( Fractional a, BeamSqlBackend be, BeamSqlBackendCanSerialize be a ) =>
  Fractional (QGenExpr context be s a) where

  QExpr a / QExpr b = QExpr (divE <$> a <*> b)
  recip = (1.0 /)

  fromRational = QExpr . pure . valueE . sqlValueSyntax . (id :: a -> a) . fromRational

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
    ValueContextSuggestion QAggregateContext = ('Text "Aggregate functions and groupings cannot be contained in value expressions." :$$:
                                                'Text "Use 'aggregate_' to compute aggregations at the value level.")
    ValueContextSuggestion QGroupingContext = ValueContextSuggestion QAggregateContext
    ValueContextSuggestion _ = 'Text ""

type Projectible be = ProjectibleWithPredicate AnyType be (WithExprContext (BeamSqlBackendExpressionSyntax' be))
type ProjectibleValue be = ProjectibleWithPredicate ValueContext be (WithExprContext (BeamSqlBackendExpressionSyntax' be))

class ThreadRewritable (s :: *) (a :: *) | a -> s where
  type WithRewrittenThread s (s' :: *) a :: *

  rewriteThread :: Proxy s' -> a -> WithRewrittenThread s s' a
instance Beamable tbl => ThreadRewritable s (tbl (QGenExpr ctxt syntax s)) where
  type WithRewrittenThread s s' (tbl (QGenExpr ctxt syntax s)) = tbl (QGenExpr ctxt syntax s')
  rewriteThread _ = changeBeamRep (\(Columnar' (QExpr a)) -> Columnar' (QExpr a))
instance Beamable tbl => ThreadRewritable s (tbl (Nullable (QGenExpr ctxt syntax s))) where
  type WithRewrittenThread s s' (tbl (Nullable (QGenExpr ctxt syntax s))) = tbl (Nullable (QGenExpr ctxt syntax s'))
  rewriteThread _ = changeBeamRep (\(Columnar' (QExpr a)) -> Columnar' (QExpr a))
instance ThreadRewritable s (QGenExpr ctxt syntax s a) where
  type WithRewrittenThread s s' (QGenExpr ctxt syntax s a) = QGenExpr ctxt syntax s' a
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

  rewriteContext :: Proxy ctxt -> a -> WithRewrittenContext a ctxt
instance Beamable tbl => ContextRewritable (tbl (QGenExpr old syntax s)) where
  type WithRewrittenContext (tbl (QGenExpr old syntax s)) ctxt = tbl (QGenExpr ctxt syntax s)

  rewriteContext _ = changeBeamRep (\(Columnar' (QExpr a)) -> Columnar' (QExpr a))
instance Beamable tbl => ContextRewritable (tbl (Nullable (QGenExpr old syntax s))) where
  type WithRewrittenContext (tbl (Nullable (QGenExpr old syntax s))) ctxt = tbl (Nullable (QGenExpr ctxt syntax s))

  rewriteContext _ = changeBeamRep (\(Columnar' (QExpr a)) -> Columnar' (QExpr a))
instance ContextRewritable (QGenExpr old syntax s a) where
  type WithRewrittenContext (QGenExpr old syntax s a) ctxt = QGenExpr ctxt syntax s a
  rewriteContext _ (QExpr a) = QExpr a
instance ContextRewritable a => ContextRewritable [a] where
  type WithRewrittenContext [a] ctxt = [ WithRewrittenContext a ctxt ]
  rewriteContext p as = map (rewriteContext p) as
instance (ContextRewritable a, KnownNat n) => ContextRewritable (Vector n a) where
  type WithRewrittenContext (Vector n a) ctxt = Vector n (WithRewrittenContext a ctxt)
  rewriteContext p as = fmap (rewriteContext p) as
instance (ContextRewritable a, ContextRewritable b) => ContextRewritable (a, b) where
  type WithRewrittenContext (a, b) ctxt = (WithRewrittenContext a ctxt, WithRewrittenContext b ctxt)
  rewriteContext p (a, b) = (rewriteContext p a, rewriteContext p b)
instance (ContextRewritable a, ContextRewritable b, ContextRewritable c) => ContextRewritable (a, b, c) where
  type WithRewrittenContext (a, b, c) ctxt = (WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt)
  rewriteContext p (a, b, c) = (rewriteContext p a, rewriteContext p b, rewriteContext p c)
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d ) => ContextRewritable (a, b, c, d) where
  type WithRewrittenContext (a, b, c, d) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt )
  rewriteContext p (a, b, c, d) = ( rewriteContext p a, rewriteContext p b, rewriteContext p c
                                  , rewriteContext p d )
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d, ContextRewritable e ) =>
    ContextRewritable (a, b, c, d, e) where
  type WithRewrittenContext (a, b, c, d, e) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt, WithRewrittenContext e ctxt )
  rewriteContext p (a, b, c, d, e) = ( rewriteContext p a, rewriteContext p b, rewriteContext p c
                                     , rewriteContext p d, rewriteContext p e )
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d, ContextRewritable e, ContextRewritable f ) =>
    ContextRewritable (a, b, c, d, e, f) where
  type WithRewrittenContext (a, b, c, d, e, f) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt, WithRewrittenContext e ctxt, WithRewrittenContext f ctxt )
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
  rewriteContext p (a, b, c, d, e, f, g) =
    ( rewriteContext p a, rewriteContext p b, rewriteContext p c
    , rewriteContext p d, rewriteContext p e, rewriteContext p f
    , rewriteContext p g )
instance ( ContextRewritable a, ContextRewritable b, ContextRewritable c
         , ContextRewritable d, ContextRewritable e, ContextRewritable f
         , ContextRewritable g, ContextRewritable h ) =>
    ContextRewritable (a, b, c, d, e, f, g, h) where
  type WithRewrittenContext (a, b, c, d, e, f, g, h) ctxt =
      ( WithRewrittenContext a ctxt, WithRewrittenContext b ctxt, WithRewrittenContext c ctxt
      , WithRewrittenContext d ctxt, WithRewrittenContext e ctxt, WithRewrittenContext f ctxt
      , WithRewrittenContext g ctxt, WithRewrittenContext h ctxt )
  rewriteContext p (a, b, c, d, e, f, g, h) =
    ( rewriteContext p a, rewriteContext p b, rewriteContext p c
    , rewriteContext p d, rewriteContext p e, rewriteContext p f
    , rewriteContext p g, rewriteContext p h )

newtype BeamSqlBackendExpressionSyntax' be
  = BeamSqlBackendExpressionSyntax'
  { fromBeamSqlBackendExpressionSyntax :: BeamSqlBackendExpressionSyntax be
  }

newtype BeamSqlBackendWindowFrameSyntax' be
  = BeamSqlBackendWindowFrameSyntax'
  { fromBeamSqlBackendWindowFrameSyntax :: BeamSqlBackendWindowFrameSyntax be
  }

class ProjectibleWithPredicate (contextPredicate :: * -> Constraint) be res a | a -> be where
  project' :: Monad m => Proxy contextPredicate -> Proxy (be, res)
           -> (forall context. contextPredicate context =>
               Proxy context -> Proxy be -> res -> m res)
           -> a -> m a

  projectSkeleton' :: Monad m => Proxy contextPredicate -> Proxy (be, res)
                   -> (forall context. contextPredicate context =>
                       Proxy context -> Proxy be -> m res)
                   -> m a

instance (Beamable t, contextPredicate context) => ProjectibleWithPredicate contextPredicate be (WithExprContext (BeamSqlBackendExpressionSyntax' be)) (t (QGenExpr context be s)) where
  project' _ _ mutateM a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ ->
                      Columnar' . QExpr . fmap fromBeamSqlBackendExpressionSyntax <$> mutateM (Proxy @context) (Proxy @be) (BeamSqlBackendExpressionSyntax' . e)) a a

  projectSkeleton' _ _ mkM =
    zipBeamFieldsM (\_ _ -> Columnar' . QExpr . fmap fromBeamSqlBackendExpressionSyntax <$> mkM (Proxy @context)(Proxy @be))
                   (tblSkeleton :: TableSkeleton t)
                   (tblSkeleton :: TableSkeleton t)

instance (Beamable t, contextPredicate context) => ProjectibleWithPredicate contextPredicate be (WithExprContext (BeamSqlBackendExpressionSyntax' be)) (t (Nullable (QGenExpr context be s))) where
  project' _ _ mutateM a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ ->
                      Columnar' . QExpr . fmap fromBeamSqlBackendExpressionSyntax <$> mutateM (Proxy @context) (Proxy @be) (BeamSqlBackendExpressionSyntax' . e)) a a

  projectSkeleton' _ _ mkM =
    zipBeamFieldsM (\_ _ -> Columnar' . QExpr . fmap fromBeamSqlBackendExpressionSyntax <$> mkM (Proxy @context)(Proxy @be))
                   (tblSkeleton :: TableSkeleton t)
                   (tblSkeleton :: TableSkeleton t)

-- instance ProjectibleWithPredicate WindowFrameContext be (QWindow be s) where
--   project' _ be mutateM (QWindow a) =
--     QWindow <$> mutateM (Proxy @QWindowFrameContext) be a

instance contextPredicate context => ProjectibleWithPredicate contextPredicate be (WithExprContext (BeamSqlBackendExpressionSyntax' be)) (QGenExpr context be s a) where
  project' _ _ mkE (QExpr a) = QExpr . fmap fromBeamSqlBackendExpressionSyntax <$> mkE (Proxy @context) (Proxy @be) (BeamSqlBackendExpressionSyntax' . a)
  projectSkeleton' _ _ mkM = QExpr . fmap fromBeamSqlBackendExpressionSyntax <$> mkM (Proxy @context) (Proxy @be)

instance contextPredicate QWindowFrameContext => ProjectibleWithPredicate contextPredicate be (WithExprContext (BeamSqlBackendWindowFrameSyntax' be)) (QWindow be s) where
  project' _ _ mkW (QWindow w) = QWindow . fmap fromBeamSqlBackendWindowFrameSyntax <$> mkW (Proxy @QWindowFrameContext) (Proxy @be) (BeamSqlBackendWindowFrameSyntax' . w)
  projectSkeleton' _ _ mkM = QWindow . fmap fromBeamSqlBackendWindowFrameSyntax <$> mkM (Proxy @QWindowFrameContext) (Proxy @be)

-- instance ProjectibleWithPredicate contextPredicate be res a => ProjectibleWithPredicate contextPredicate be res [a] where
--   project' context be mkE as = traverse (project' context be mkE) as

instance (ProjectibleWithPredicate contextPredicate be res a, KnownNat n) => ProjectibleWithPredicate contextPredicate be res (Vector n a) where
  project' context be mkE as = traverse (project' context be mkE) as
  projectSkeleton' context be mkM = VS.replicateM (projectSkeleton' context be mkM)

instance ( ProjectibleWithPredicate contextPredicate be res a, ProjectibleWithPredicate contextPredicate be res b ) =>
  ProjectibleWithPredicate contextPredicate be res (a, b) where

  project' context be mkE (a, b) =
    (,) <$> project' context be mkE a <*> project' context be mkE b
  projectSkeleton' context be mkM =
    (,) <$> projectSkeleton' context be mkM
        <*> projectSkeleton' context be mkM

instance ( ProjectibleWithPredicate contextPredicate be res a, ProjectibleWithPredicate contextPredicate be res b, ProjectibleWithPredicate contextPredicate be res c ) =>
  ProjectibleWithPredicate contextPredicate be res (a, b, c) where

  project' context be mkE (a, b, c) =
    (,,) <$> project' context be mkE a <*> project' context be mkE b <*> project' context be mkE c
  projectSkeleton' context be mkM =
    (,,) <$> projectSkeleton' context be mkM
         <*> projectSkeleton' context be mkM
         <*> projectSkeleton' context be mkM

instance ( ProjectibleWithPredicate contextPredicate be res a, ProjectibleWithPredicate contextPredicate be res b, ProjectibleWithPredicate contextPredicate be res c
         , ProjectibleWithPredicate contextPredicate be res d ) =>
  ProjectibleWithPredicate contextPredicate be res (a, b, c, d) where

  project' context be mkE (a, b, c, d) =
    (,,,) <$> project' context be mkE a <*> project' context be mkE b <*> project' context be mkE c
          <*> project' context be mkE d
  projectSkeleton' context be mkM =
    (,,,) <$> projectSkeleton' context be mkM
          <*> projectSkeleton' context be mkM
          <*> projectSkeleton' context be mkM
          <*> projectSkeleton' context be mkM

instance ( ProjectibleWithPredicate contextPredicate be res a, ProjectibleWithPredicate contextPredicate be res b, ProjectibleWithPredicate contextPredicate be res c
         , ProjectibleWithPredicate contextPredicate be res d, ProjectibleWithPredicate contextPredicate be res e ) =>
  ProjectibleWithPredicate contextPredicate be res (a, b, c, d, e) where

  project' context be mkE (a, b, c, d, e) =
    (,,,,) <$> project' context be mkE a <*> project' context be mkE b <*> project' context be mkE c
           <*> project' context be mkE d <*> project' context be mkE e
  projectSkeleton' context be mkM =
    (,,,,) <$> projectSkeleton' context be mkM
           <*> projectSkeleton' context be mkM
           <*> projectSkeleton' context be mkM
           <*> projectSkeleton' context be mkM
           <*> projectSkeleton' context be mkM

instance ( ProjectibleWithPredicate contextPredicate be res a, ProjectibleWithPredicate contextPredicate be res b, ProjectibleWithPredicate contextPredicate be res c
         , ProjectibleWithPredicate contextPredicate be res d, ProjectibleWithPredicate contextPredicate be res e, ProjectibleWithPredicate contextPredicate be res f ) =>
  ProjectibleWithPredicate contextPredicate be res (a, b, c, d, e, f) where

  project' context be  mkE (a, b, c, d, e, f) =
    (,,,,,) <$> project' context be mkE a <*> project' context be mkE b <*> project' context be mkE c
            <*> project' context be mkE d <*> project' context be mkE e <*> project' context be mkE f
  projectSkeleton' context be mkM =
    (,,,,,) <$> projectSkeleton' context be mkM
            <*> projectSkeleton' context be mkM
            <*> projectSkeleton' context be mkM
            <*> projectSkeleton' context be mkM
            <*> projectSkeleton' context be mkM
            <*> projectSkeleton' context be mkM

instance ( ProjectibleWithPredicate contextPredicate be res a, ProjectibleWithPredicate contextPredicate be res b, ProjectibleWithPredicate contextPredicate be res c
         , ProjectibleWithPredicate contextPredicate be res d, ProjectibleWithPredicate contextPredicate be res e, ProjectibleWithPredicate contextPredicate be res f
         , ProjectibleWithPredicate contextPredicate be res g ) =>
  ProjectibleWithPredicate contextPredicate be res (a, b, c, d, e, f, g) where

  project' context be mkE (a, b, c, d, e, f, g) =
    (,,,,,,) <$> project' context be mkE a <*> project' context be mkE b <*> project' context be mkE c
             <*> project' context be mkE d <*> project' context be mkE e <*> project' context be mkE f
             <*> project' context be mkE g
  projectSkeleton' context be mkM =
    (,,,,,,) <$> projectSkeleton' context be mkM
             <*> projectSkeleton' context be mkM
             <*> projectSkeleton' context be mkM
             <*> projectSkeleton' context be mkM
             <*> projectSkeleton' context be mkM
             <*> projectSkeleton' context be mkM
             <*> projectSkeleton' context be mkM

instance ( ProjectibleWithPredicate contextPredicate be res a, ProjectibleWithPredicate contextPredicate be res b, ProjectibleWithPredicate contextPredicate be res c
         , ProjectibleWithPredicate contextPredicate be res d, ProjectibleWithPredicate contextPredicate be res e, ProjectibleWithPredicate contextPredicate be res f
         , ProjectibleWithPredicate contextPredicate be res g, ProjectibleWithPredicate contextPredicate be res h ) =>
  ProjectibleWithPredicate contextPredicate be res (a, b, c, d, e, f, g, h) where

  project' context be mkE (a, b, c, d, e, f, g, h) =
    (,,,,,,,) <$> project' context be mkE a <*> project' context be mkE b <*> project' context be mkE c
              <*> project' context be mkE d <*> project' context be mkE e <*> project' context be mkE f
              <*> project' context be mkE g <*> project' context be mkE h
  projectSkeleton' context be mkM =
    (,,,,,,,) <$> projectSkeleton' context be mkM
              <*> projectSkeleton' context be mkM
              <*> projectSkeleton' context be mkM
              <*> projectSkeleton' context be mkM
              <*> projectSkeleton' context be mkM
              <*> projectSkeleton' context be mkM
              <*> projectSkeleton' context be mkM
              <*> projectSkeleton' context be mkM

-- TODO add projectSkeleton'
instance Beamable t => ProjectibleWithPredicate AnyType () T.Text (t (QField s)) where
  project' _ be mutateM a =
    zipBeamFieldsM (\(Columnar' f) _ ->
                      Columnar' <$> project' (Proxy @AnyType) be mutateM f) a a

  projectSkeleton' _ _ mkM =
    zipBeamFieldsM (\_ _ -> Columnar' . QField False "" <$> (mkM (Proxy @()) (Proxy @())))
                   (tblSkeleton :: TableSkeleton t) (tblSkeleton :: TableSkeleton t)

instance Beamable t => ProjectibleWithPredicate AnyType () T.Text (t (Nullable (QField s))) where
  project' _ be mutateM a =
    zipBeamFieldsM (\(Columnar' f) _ ->
                      Columnar' <$> project' (Proxy @AnyType) be mutateM f) a a

  projectSkeleton' _ _ mkM =
    zipBeamFieldsM (\_ _ -> Columnar' . QField False "" <$> mkM (Proxy @()) (Proxy @()))
                   (tblSkeleton :: TableSkeleton t) (tblSkeleton :: TableSkeleton t)

instance Beamable t => ProjectibleWithPredicate AnyType () res (t (Const res)) where
  project' _ be mutateM a =
    zipBeamFieldsM (\(Columnar' f) _ ->
                      Columnar' <$> project' (Proxy @AnyType) be mutateM f) a a

  projectSkeleton' _ _ mkM =
    zipBeamFieldsM (\_ _ -> Columnar' . Const <$> mkM (Proxy @()) (Proxy @()))
                   (tblSkeleton :: TableSkeleton t) (tblSkeleton :: TableSkeleton t)

instance Beamable t => ProjectibleWithPredicate AnyType () T.Text (t (Nullable (Const T.Text))) where
  project' _ be mutateM a =
    zipBeamFieldsM (\(Columnar' f) _ ->
                      Columnar' <$> project' (Proxy @AnyType) be mutateM f) a a

  projectSkeleton' _ _ mkM =
    zipBeamFieldsM (\_ _ -> Columnar' . Const <$> mkM (Proxy @()) (Proxy @()))
                   (tblSkeleton :: TableSkeleton t) (tblSkeleton :: TableSkeleton t)

instance ProjectibleWithPredicate AnyType () res (Const res a) where
  project' _ _ mutateM (Const a) = Const <$> mutateM (Proxy @()) (Proxy @()) a

  projectSkeleton' _ _ mkM =
    Const <$> mkM (Proxy @()) (Proxy @())

instance ProjectibleWithPredicate AnyType () T.Text (QField s a) where
  project' _ _ mutateM (QField q tbl f) =
    fmap (QField q tbl)
         (mutateM (Proxy @(QField s a)) (Proxy @()) f)

  projectSkeleton' _ _ mkM =
    QField False "" <$> mkM (Proxy @()) (Proxy @())

project :: forall be a
         . Projectible be a => Proxy be -> a -> WithExprContext [ BeamSqlBackendExpressionSyntax be ]
project _ = fmap (fmap fromBeamSqlBackendExpressionSyntax) . sequenceA . DList.toList . execWriter .
            project' (Proxy @AnyType) (Proxy @(be, WithExprContext (BeamSqlBackendExpressionSyntax' be))) (\_ _ e -> tell (DList.singleton e) >> pure e)

reproject :: forall be a
           . (BeamSqlBackend be, Projectible be a)
          => Proxy be -> (Int -> BeamSqlBackendExpressionSyntax be) -> a -> a
reproject _ mkField a =
  evalState (project' (Proxy @AnyType) (Proxy @(be, WithExprContext (BeamSqlBackendExpressionSyntax' be))) (\_ _ _ -> state (\i -> (i, i + 1)) >>= pure . pure . BeamSqlBackendExpressionSyntax' . mkField) a) 0

-- | suitable as argument to 'QAll' in the case of a table result
tableFieldsToExpressions :: ( BeamSqlBackend be, Beamable table )
                         => TableSettings table -> T.Text -> table (QGenExpr ctxt be s)
tableFieldsToExpressions tblSettings newTblNm =
    changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField newTblNm (_fieldName f))))) tblSettings

mkFieldsSkeleton :: forall be res m
                  . (Projectible be res, MonadState Int m)
                 => (Int -> m (WithExprContext (BeamSqlBackendExpressionSyntax' be))) -> m res
mkFieldsSkeleton go =
    projectSkeleton' (Proxy @AnyType) (Proxy @(be, WithExprContext (BeamSqlBackendExpressionSyntax' be))) $ \_ _ ->
    do i <- get
       put (i + 1)
       go i

mkFieldNames :: forall be res
              . ( BeamSqlBackend be, Projectible be res )
             => (T.Text -> BeamSqlBackendFieldNameSyntax be) -> (res, [ T.Text ])
mkFieldNames mkField =
    runWriter . flip evalStateT 0 $
    mkFieldsSkeleton @be @res $ \i -> do
      let fieldName' = fromString ("res" ++ show i)
      tell [ fieldName' ]
      pure (\_ -> BeamSqlBackendExpressionSyntax' (fieldE (mkField fieldName')))

tableNameFromEntity :: IsSql92TableNameSyntax name
                    => DatabaseEntityDescriptor be (TableEntity tbl)
                    -> name

tableNameFromEntity = tableName <$> dbTableSchema <*> dbTableCurrentName

rescopeQ :: QM be db s res -> QM be db s' res
rescopeQ = unsafeCoerce
