{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors#-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Query.Internal where

import           Database.Beam.Backend.Types
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema.Tables

import           Data.String
import qualified Data.Text as T
import qualified Data.DList as DList
import           Data.Typeable

import           Control.Monad.Free.Church
import           Control.Monad.State
import           Control.Monad.Writer

import           GHC.TypeLits
import           GHC.Types

type ProjectibleInSelectSyntax syntax a =
  ( IsSql92SelectSyntax syntax
  , Eq (Sql92SelectExpressionSyntax syntax)
  , Sql92ProjectionExpressionSyntax (Sql92SelectProjectionSyntax syntax) ~ Sql92SelectExpressionSyntax syntax
  , Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax syntax)) ~ syntax
  , Projectible (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a
  , ProjectibleValue (Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax syntax))) a)

data QF select (db :: (* -> *) -> *) s next where
  QAll :: Beamable table
       => T.Text -> TableSettings table
       -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> Maybe (Sql92SelectExpressionSyntax select))
       -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> next) -> QF select db s next

  QArbitraryJoin :: Projectible (Sql92SelectExpressionSyntax select) r
                 => QM select db (QNested s) r
                 -> (Sql92SelectFromSyntax select -> Sql92SelectFromSyntax select ->
                     Maybe (Sql92FromExpressionSyntax (Sql92SelectFromSyntax select)) ->
                     Sql92SelectFromSyntax select)
                 -> (r -> Maybe (Sql92SelectExpressionSyntax select))
                 -> (r -> next)
                 -> QF select db s next
  QTwoWayJoin :: ( Projectible (Sql92SelectExpressionSyntax select) a
                 , Projectible (Sql92SelectExpressionSyntax select) b )
              => QM select db (QNested s) a
              -> QM select db (QNested s) b
              -> (Sql92SelectFromSyntax select -> Sql92SelectFromSyntax select ->
                   Maybe (Sql92FromExpressionSyntax (Sql92SelectFromSyntax select)) ->
                   Sql92SelectFromSyntax select)
              -> ((a, b) -> Maybe (Sql92SelectExpressionSyntax select))
              -> ((a, b) -> next)
              -> QF select db s next

  QSubSelect :: Projectible (Sql92SelectExpressionSyntax select) r =>
                QM select db (QNested s) r -> (r -> next) -> QF select db s next
  QGuard :: Sql92SelectExpressionSyntax select -> next -> QF select db s next

  QLimit :: Projectible (Sql92SelectExpressionSyntax select) r => Integer -> QM select db (QNested s) r -> (r -> next) -> QF select db s next
  QOffset :: Projectible (Sql92SelectExpressionSyntax select) r => Integer -> QM select db (QNested s) r -> (r -> next) -> QF select db s next

  QUnion ::Projectible (Sql92SelectExpressionSyntax select) r =>  Bool -> QM select db (QNested s) r -> QM select db (QNested s) r -> (r -> next) -> QF select db s next
  QIntersect :: Projectible (Sql92SelectExpressionSyntax select) r => Bool -> QM select db (QNested s) r -> QM select db (QNested s) r -> (r -> next) -> QF select db s next
  QExcept :: Projectible (Sql92SelectExpressionSyntax select) r => Bool -> QM select db (QNested s) r -> QM select db (QNested s) r -> (r -> next) -> QF select db s next
  QOrderBy :: Projectible (Sql92SelectExpressionSyntax select) r =>
              (r -> [ Sql92SelectOrderingSyntax select ])
           -> QM select db (QNested s) r -> (r -> next) -> QF select db s next

  QWindowOver :: ( ProjectibleWithPredicate WindowFrameContext (Sql2003ExpressionWindowFrameSyntax (Sql92SelectExpressionSyntax select)) window
                 , Projectible (Sql92SelectExpressionSyntax select) r
                 , Projectible (Sql92SelectExpressionSyntax select) a )
              => (r -> window) -> (r -> window -> a)
              -> QM select db (QNested s) r -> (a -> next) -> QF select db s next
  QAggregate :: ( Projectible (Sql92SelectExpressionSyntax select) grouping
                , Projectible (Sql92SelectExpressionSyntax select) a ) =>
                (a -> (Maybe (Sql92SelectGroupingSyntax select), grouping)) ->
                QM select db (QNested s) a ->
                (grouping -> next) -> QF select db s next
deriving instance Functor (QF select db s)

type QM select db s = F (QF select db s)

-- | The type of queries over the database `db` returning results of type `a`.
-- The `s` argument is a threading argument meant to restrict cross-usage of
-- `QExpr`s. 'syntax' represents the SQL syntax that this query is building.
newtype Q syntax (db :: (* -> *) -> *) s a
  = Q { runQ :: QM syntax db s a }
    deriving (Monad, Applicative, Functor)

data QInternal
data QNested s

data QField s ty
  = QField
  { qFieldTblName :: T.Text
  , qFieldName    :: T.Text }
  deriving (Show, Eq, Ord)

data QAssignment fieldName expr s
  = QAssignment [(fieldName, expr)]
  deriving (Show, Eq, Ord)

-- * QGenExpr type

data QAggregateContext
data QGroupingContext
data QValueContext
data QOrderingContext
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
newtype QGenExpr context syntax s t = QExpr syntax

-- | 'QExpr's represent expressions not containing aggregates.
type QExpr = QGenExpr QValueContext
type QAgg = QGenExpr QAggregateContext
type QOrd = QGenExpr QOrderingContext
type QWindowExpr = QGenExpr QWindowingContext
type QWindowFrame = QGenExpr QWindowFrameContext
type QGroupExpr = QGenExpr QGroupingContext
deriving instance Show syntax => Show (QGenExpr context syntax s t)
deriving instance Eq syntax => Eq (QGenExpr context syntax s t)

newtype QWindow syntax s = QWindow syntax
newtype QFrameBounds syntax = QFrameBounds (Maybe syntax)
newtype QFrameBound syntax = QFrameBound syntax

qBinOpE :: forall syntax context s a b c. IsSql92ExpressionSyntax syntax =>
           (syntax -> syntax -> syntax)
        -> QGenExpr context syntax s a -> QGenExpr context syntax s b
        -> QGenExpr context syntax s c
qBinOpE mkOpE (QExpr a) (QExpr b) = QExpr (mkOpE a b)

unsafeRetype :: QGenExpr ctxt syntax s a -> QGenExpr ctxt syntax s a'
unsafeRetype (QExpr v) = QExpr v

instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) [Char] ) =>
    IsString (QGenExpr context syntax s T.Text) where
    fromString = QExpr . valueE . sqlValueSyntax
instance (Num a
         , IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a) =>
    Num (QGenExpr context syntax s a) where
    fromInteger x = let res :: QGenExpr context syntax s a
                        res = QExpr (valueE (sqlValueSyntax (fromIntegral x :: a)))
                    in res
    QExpr a + QExpr b = QExpr (addE a b)
    QExpr a - QExpr b = QExpr (subE a b)
    QExpr a * QExpr b = QExpr (mulE a b)
    negate (QExpr a) = QExpr (negateE a)
    abs (QExpr x) = QExpr (absE x)
    signum _ = error "signum: not defined for QExpr. Use CASE...WHEN"

instance ( Fractional a
         , IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a ) =>
  Fractional (QGenExpr context syntax s a) where

  QExpr a / QExpr b = QExpr (divE a b)
  recip = (1.0 /)

  fromRational = QExpr . valueE . sqlValueSyntax . (id :: a -> a) . fromRational

-- * Aggregations

data Aggregation syntax s a
  = GroupAgg syntax --(Sql92ExpressionSyntax syntax)
  | ProjectAgg syntax --(Sql92ExpressionSyntax syntax)

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
type ProjectibleValue = ProjectibleWithPredicate ValueContext

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

class ProjectibleWithPredicate (contextPredicate :: * -> Constraint) syntax a | a -> syntax where
  project' :: Monad m => Proxy contextPredicate -> (forall context. contextPredicate context => Proxy context -> syntax -> m syntax) -> a -> m a
instance (Beamable t, contextPredicate context) => ProjectibleWithPredicate contextPredicate syntax (t (QGenExpr context syntax s)) where
  project' _ mutateM a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ ->
                      Columnar' . QExpr <$> mutateM (Proxy @context) e) a a
instance (Beamable t, contextPredicate context) => ProjectibleWithPredicate contextPredicate syntax (t (Nullable (QGenExpr context syntax s))) where
  project' _ mutateM a =
    zipBeamFieldsM (\(Columnar' (QExpr e)) _ ->
                      Columnar' . QExpr <$> mutateM (Proxy @context) e) a a
instance ProjectibleWithPredicate WindowFrameContext syntax (QWindow syntax s) where
  project' _ mutateM (QWindow a) =
    QWindow <$> mutateM (Proxy @QWindowFrameContext) a
instance contextPredicate context => ProjectibleWithPredicate contextPredicate syntax (QGenExpr context syntax s a) where
  project' _ mkE (QExpr a) = QExpr <$> mkE (Proxy @context) a
instance ( ProjectibleWithPredicate contextPredicate syntax a, ProjectibleWithPredicate contextPredicate syntax b ) =>
  ProjectibleWithPredicate contextPredicate syntax (a, b) where

  project' p mkE (a, b) = (,) <$> project' p mkE a <*> project' p mkE b
instance ( ProjectibleWithPredicate contextPredicate syntax a, ProjectibleWithPredicate contextPredicate syntax b, ProjectibleWithPredicate contextPredicate syntax c ) =>
  ProjectibleWithPredicate contextPredicate syntax (a, b, c) where

  project' p mkE (a, b, c) = (,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
instance ( ProjectibleWithPredicate contextPredicate syntax a, ProjectibleWithPredicate contextPredicate syntax b, ProjectibleWithPredicate contextPredicate syntax c
         , ProjectibleWithPredicate contextPredicate syntax d ) =>
  ProjectibleWithPredicate contextPredicate syntax (a, b, c, d) where

  project' p mkE (a, b, c, d) = (,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
                                      <*> project' p mkE d
instance ( ProjectibleWithPredicate contextPredicate syntax a, ProjectibleWithPredicate contextPredicate syntax b, ProjectibleWithPredicate contextPredicate syntax c
         , ProjectibleWithPredicate contextPredicate syntax d, ProjectibleWithPredicate contextPredicate syntax e ) =>
  ProjectibleWithPredicate contextPredicate syntax (a, b, c, d, e) where

  project' p mkE (a, b, c, d, e) = (,,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
                                          <*> project' p mkE d <*> project' p mkE e
instance ( ProjectibleWithPredicate contextPredicate syntax a, ProjectibleWithPredicate contextPredicate syntax b, ProjectibleWithPredicate contextPredicate syntax c
         , ProjectibleWithPredicate contextPredicate syntax d, ProjectibleWithPredicate contextPredicate syntax e, ProjectibleWithPredicate contextPredicate syntax f ) =>
  ProjectibleWithPredicate contextPredicate syntax (a, b, c, d, e, f) where

  project' p mkE (a, b, c, d, e, f) = (,,,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
                                              <*> project' p mkE d <*> project' p mkE e <*> project' p mkE f
instance ( ProjectibleWithPredicate contextPredicate syntax a, ProjectibleWithPredicate contextPredicate syntax b, ProjectibleWithPredicate contextPredicate syntax c
         , ProjectibleWithPredicate contextPredicate syntax d, ProjectibleWithPredicate contextPredicate syntax e, ProjectibleWithPredicate contextPredicate syntax f
         , ProjectibleWithPredicate contextPredicate syntax g ) =>
  ProjectibleWithPredicate contextPredicate syntax (a, b, c, d, e, f, g) where

  project' p mkE (a, b, c, d, e, f, g) =
    (,,,,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
             <*> project' p mkE d <*> project' p mkE e <*> project' p mkE f
             <*> project' p mkE g
instance ( ProjectibleWithPredicate contextPredicate syntax a, ProjectibleWithPredicate contextPredicate syntax b, ProjectibleWithPredicate contextPredicate syntax c
         , ProjectibleWithPredicate contextPredicate syntax d, ProjectibleWithPredicate contextPredicate syntax e, ProjectibleWithPredicate contextPredicate syntax f
         , ProjectibleWithPredicate contextPredicate syntax g, ProjectibleWithPredicate contextPredicate syntax h ) =>
  ProjectibleWithPredicate contextPredicate syntax (a, b, c, d, e, f, g, h) where

  project' p mkE (a, b, c, d, e, f, g, h) =
    (,,,,,,,) <$> project' p mkE a <*> project' p mkE b <*> project' p mkE c
              <*> project' p mkE d <*> project' p mkE e <*> project' p mkE f
              <*> project' p mkE g <*> project' p mkE h

project :: Projectible syntax a => a -> [ syntax ]
project = DList.toList . execWriter . project' (Proxy @AnyType) (\_ e -> tell (DList.singleton e) >> pure e)
reproject :: (IsSql92ExpressionSyntax syntax, Projectible syntax a) =>
             (Int -> syntax) -> a -> a
reproject mkField a =
  evalState (project' (Proxy @AnyType) (\_ _ -> state (\i -> (i, i + 1)) >>= pure . mkField) a) 0
