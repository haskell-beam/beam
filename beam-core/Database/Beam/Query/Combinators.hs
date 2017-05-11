{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.Query.Combinators
    ( -- * Various SQL functions and constructs
      coalesce_, position_
    , charLength_, octetLength_, bitLength_
    , currentTimestamp_

    -- ** @IF-THEN-ELSE@ support
    , if_, then_, else_

    -- * SQL @UPDATE@ assignments
    , (<-.), current_

    -- * Project Haskell values to 'QGenExpr's
    , HaskellLiteralForQExpr
    , SqlValable(..), SqlValableTable

    -- * General query combinators

    , all_
    , allFromView_, join_, guard_, filter_
    , related_, relatedBy_
    , leftJoin_, perhaps_
    , outerJoin_
    , subselect_, references_

    , SqlJustable(..)
    , SqlDeconstructMaybe(..)
    , SqlOrderable
    , QIfCond, QIfElse

    , limit_, offset_

    , as_

    -- ** Subqueries
    , exists_, unique_, distinct_, subquery_

    -- ** Set operations
    -- |  'Q' values can be combined using a variety of set operations. See the
    --    <https://tathougies.github.io/beam/user-guide/queries/combining-queries manual section>.
    , union_, unionAll_
    , intersect_, intersectAll_
    , except_, exceptAll_

    -- * Window functions
    -- | See the corresponding
    --   <https://tathougies.github.io/beam/user-guide/queries/window-functions manual section> for more.
    , over_, frame_, bounds_, unbounded_, nrows_, fromBound_
    , noBounds_, noOrder_, noPartition_
    , partitionBy_, orderPartitionBy_, withWindow_

    -- * Ordering primitives
    , orderBy_, asc_, desc_
    ) where

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL

import Database.Beam.Query.Internal
import Database.Beam.Query.Ord
import Database.Beam.Query.Operator
import Database.Beam.Query.Types

import Database.Beam.Schema.Tables

import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Free
import Control.Applicative

import Data.Maybe
import Data.Proxy
import Data.Time (LocalTime)

import GHC.Generics

-- | Introduce all entries of a table into the 'Q' monad
all_ :: forall be (db :: (* -> *) -> *) table select s.
        ( Database db
        , IsSql92SelectSyntax select

        , IsSql92FromSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
        , IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)))
        , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)) ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)

        , Table table )
       => DatabaseEntity be db (TableEntity table)
       -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
all_ (DatabaseEntity (DatabaseTable tblNm tblSettings)) =
    Q $ liftF (QAll tblNm tblSettings (\_ -> Nothing) id)

-- | Introduce all entries of a view into the 'Q' monad
allFromView_ :: forall be (db :: (* -> *) -> *) table select s.
                ( Database db
                , IsSql92SelectSyntax select

                , IsSql92FromSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
                , IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)))
                , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)) ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
                , Beamable table )
               => DatabaseEntity be db (ViewEntity table)
               -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
allFromView_ (DatabaseEntity (DatabaseView tblNm tblSettings)) =
    Q $ liftF (QAll tblNm tblSettings (\_ -> Nothing) id)

-- | Introduce all entries of a table into the 'Q' monad based on the given
--   QExpr
join_ :: ( Database db, Table table
         , IsSql92SelectSyntax select
         , IsSql92FromSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))
         , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)) ~ Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
         , IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))) ) =>
         DatabaseEntity be db (TableEntity table)
      -> (table (QExpr (Sql92SelectExpressionSyntax select) s) -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool)
      -> Q select db s (table (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
join_ (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkOn =
    Q $ liftF (QAll tblNm tblSettings (\tbl -> let QExpr on = mkOn tbl in Just on) id)

-- | Introduce a table using a left join with no ON clause. Because this is not
--   an inner join, the resulting table is made nullable. This means that each
--   field that would normally have type 'QExpr x' will now have type 'QExpr
--   (Maybe x)'.
perhaps_ :: forall s r select db.
          ( Projectible (Sql92SelectExpressionSyntax select) r
          , IsSql92SelectSyntax select
          , ThreadRewritable (QNested s) r
          , Retaggable (QExpr (Sql92SelectExpressionSyntax select) s) (WithRewrittenThread (QNested s) s r) )
         => Q select db (QNested s) r
         -> Q select db s (Retag Nullable (WithRewrittenThread (QNested s) s r))
perhaps_ (Q sub) =
  Q $ liftF (QArbitraryJoin
              sub leftJoin
              (\_ -> Nothing)
              (\r -> retag (\(Columnar' (QExpr e) :: Columnar' (QExpr (Sql92SelectExpressionSyntax select) s) a) ->
                                            Columnar' (QExpr e) :: Columnar' (Nullable (QExpr (Sql92SelectExpressionSyntax select) s)) a) $
                                  rewriteThread (Proxy @s) r))

outerJoin_ :: forall s a b select db.
              ( Projectible (Sql92SelectExpressionSyntax select) a, Projectible (Sql92SelectExpressionSyntax select) b
              , ThreadRewritable (QNested s) a, ThreadRewritable (QNested s) b
              , Retaggable (QExpr (Sql92SelectExpressionSyntax select) s) (WithRewrittenThread (QNested s) s a)
              , Retaggable (QExpr (Sql92SelectExpressionSyntax select) s) (WithRewrittenThread (QNested s) s b)
              , IsSql92FromOuterJoinSyntax (Sql92SelectFromSyntax select) )
           => Q select db (QNested s) a
           -> Q select db (QNested s) b
           -> ( (WithRewrittenThread (QNested s) s a, WithRewrittenThread (QNested s) s b) -> QExpr (Sql92SelectExpressionSyntax select) s Bool )
           -> Q select db s ( Retag Nullable (WithRewrittenThread (QNested s) s a)
                            , Retag Nullable (WithRewrittenThread (QNested s) s b) )
outerJoin_ (Q a) (Q b) on_ =
  Q $ liftF (QTwoWayJoin a b outerJoin
              (\(a', b') ->
                 let QExpr e = on_ (rewriteThread (Proxy @s) a', rewriteThread (Proxy @s) b')
                 in Just e)
              (\(a', b') ->
                 let retag' :: (ThreadRewritable (QNested s) x, Retaggable (QExpr (Sql92SelectExpressionSyntax select) s) (WithRewrittenThread (QNested s) s x))
                            => x -> Retag Nullable (WithRewrittenThread (QNested s) s x)
                     retag' = retag (\(Columnar' (QExpr e) :: Columnar' (QExpr (Sql92SelectExpressionSyntax select) s) x) ->
                                        Columnar' (QExpr e) :: Columnar' (Nullable (QExpr (Sql92SelectExpressionSyntax select) s)) x) .
                              rewriteThread (Proxy @s)
                 in ( retag' a', retag' b' )))

-- | Introduce a table using a left join. The ON clause is required here.Because
--   this is not an inner join, the resulting table is made nullable. This means
--   that each field that would normally have type 'QExpr x' will now have type
--   'QExpr (Maybe x)'.
leftJoin_ :: forall s r select db.
           ( Projectible (Sql92SelectExpressionSyntax select) r
           , IsSql92SelectSyntax select
           , ThreadRewritable (QNested s) r
           , Retaggable (QExpr (Sql92SelectExpressionSyntax select) s) (WithRewrittenThread (QNested s) s r) )
          => Q select db (QNested s) r
          -> (WithRewrittenThread (QNested s) s r -> QExpr (Sql92SelectExpressionSyntax select) s Bool)
          -> Q select db s (Retag Nullable (WithRewrittenThread (QNested s) s r))
leftJoin_ (Q sub) on_ =
  Q $ liftF (QArbitraryJoin
               sub leftJoin
               (\r -> let QExpr e = on_ (rewriteThread (Proxy @s) r) in Just e)
               (\r -> retag (\(Columnar' (QExpr e) :: Columnar' (QExpr (Sql92SelectExpressionSyntax select) s) a) ->
                                Columnar' (QExpr e) :: Columnar' (Nullable (QExpr (Sql92SelectExpressionSyntax select) s)) a) $
                      rewriteThread (Proxy @s) r))

subselect_ :: forall s r select db.
            ( ThreadRewritable (QNested s) r
            , ProjectibleInSelectSyntax select r )
           => Q select db (QNested s) r
           -> Q select db s (WithRewrittenThread (QNested s) s r)
subselect_ (Q q') =
  Q (liftF (QSubSelect q' (rewriteThread (Proxy @s))))

-- | Only allow results for which the 'QExpr' yields 'True'
guard_ :: forall select db s.
          ( IsSql92SelectSyntax select ) =>
          QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool -> Q select db s ()
guard_ (QExpr guardE') =
    Q (liftF (QGuard guardE' ()))

-- | Synonym for @clause >>= \x -> guard_ (mkExpr x)>> pure x@
filter_ :: forall r select db s.
           ( IsSql92SelectSyntax select )
        => (r -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool)
        -> Q select db s r -> Q select db s r
filter_ mkExpr clause = clause >>= \x -> guard_ (mkExpr x) >> pure x

-- | Introduce all entries of the given table which are referenced by the given 'PrimaryKey'
related_ :: forall be db rel select s.
            ( IsSql92SelectSyntax select
            , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select))) Bool
            , Database db, Table rel ) =>
            DatabaseEntity be db (TableEntity rel)
         -> PrimaryKey rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s)
         -> Q select db s (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
related_ relTbl relKey =
  join_ relTbl (\rel -> relKey ==. primaryKey rel)

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_ :: forall be db rel select s.
              ( Database db, Table rel
              , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select))) Bool
              , IsSql92SelectSyntax select )
           => DatabaseEntity be db (TableEntity rel)
           -> (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s) ->
                QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool)
           -> Q select db s (rel (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s))
relatedBy_ = join_

-- | Generate an appropriate boolean 'QGenExpr' comparing the given foreign key
--   to the given table. Useful for creating join conditions.
references_ :: ( IsSql92ExpressionSyntax expr
               , HasSqlValueSyntax (Sql92ExpressionValueSyntax expr) Bool
               , Table t )
            => PrimaryKey t (QGenExpr ctxt expr s) -> t (QGenExpr ctxt expr s) -> QGenExpr ctxt expr s Bool
references_ fk tbl = fk ==. pk tbl

-- | Limit the number of results returned by a query.
limit_ :: forall s a select db.
           ( ProjectibleInSelectSyntax select a
          , ThreadRewritable (QNested s) a ) =>
          Integer -> Q select db (QNested s) a -> Q select db s (WithRewrittenThread (QNested s) s a)
limit_ limit' (Q q) =
  Q (liftF (QLimit limit' q (rewriteThread (Proxy @s))))

-- | Drop the first `offset'` results.
offset_ :: forall s a select db.
           ( ProjectibleInSelectSyntax select a
           , ThreadRewritable (QNested s) a ) =>
           Integer -> Q select db (QNested s) a -> Q select db s (WithRewrittenThread (QNested s) s a)
offset_ offset' (Q q) =
  Q (liftF (QOffset offset' q (rewriteThread (Proxy @s))))

-- | Use the SQL @EXISTS@ operator to determine if the given query returns any results
exists_ :: ( IsSql92SelectSyntax select
           , HasQBuilder select
           , ProjectibleInSelectSyntax select a
           , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select)
        => Q select db s a
        -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool
exists_ = QExpr . existsE . buildSqlQuery

-- | Use the SQL @UNIQUE@ operator to determine if the given query produces a unique result
unique_ :: ( IsSql92SelectSyntax select
           , HasQBuilder select
           , ProjectibleInSelectSyntax select a
           , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select)
        => Q select db s a
        -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool
unique_ = QExpr . uniqueE . buildSqlQuery

-- | Use the SQL99 @DISTINCT@ operator to determine if the given query produces a distinct result
distinct_ :: ( IsSql99ExpressionSyntax (Sql92SelectExpressionSyntax select)
             , HasQBuilder select
             , ProjectibleInSelectSyntax select a
             , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select) =>
             Q select db s a
          -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s Bool
distinct_ = QExpr . distinctE . buildSqlQuery

-- | Project the (presumably) singular result of the given query as an expression
subquery_ ::
  ( IsSql92SelectSyntax select
  , HasQBuilder select
  , ProjectibleInSelectSyntax select (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a)
  , Sql92ExpressionSelectSyntax (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) ~ select) =>
  Q select (db :: (* -> *) -> *) s (QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a)
  -> QExpr (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) s a
subquery_ =
  QExpr . subqueryE . buildSqlQuery

-- | SQL @CHAR_LENGTH@ function
charLength_ :: ( IsSqlExpressionSyntaxStringType syntax text
               , IsSql92ExpressionSyntax syntax )
            => QGenExpr context syntax s text -> QGenExpr context syntax s Int
charLength_ (QExpr s) = QExpr (charLengthE s)

-- | SQL @OCTET_LENGTH@ function
octetLength_ :: ( IsSqlExpressionSyntaxStringType syntax text
                , IsSql92ExpressionSyntax syntax )
             => QGenExpr context syntax s text -> QGenExpr context syntax s Int
octetLength_ (QExpr s) = QExpr (octetLengthE s)

-- | SQL @BIT_LENGTH@ function
bitLength_ ::
  IsSql92ExpressionSyntax syntax =>
  QGenExpr context syntax s SqlBitString -> QGenExpr context syntax s Int
bitLength_ (QExpr x) = QExpr (bitLengthE x)

-- | SQL @CURRENT_TIMESTAMP@ function
currentTimestamp_ :: IsSql92ExpressionSyntax syntax => QGenExpr ctxt syntax s LocalTime
currentTimestamp_ = QExpr currentTimestampE

-- | SQL @POSITION(.. IN ..)@ function
position_ ::
  ( IsSqlExpressionSyntaxStringType syntax text
  , IsSql92ExpressionSyntax syntax, Integral b ) =>
  QExpr syntax s text -> QExpr syntax s text -> QExpr syntax s b
position_ (QExpr needle) (QExpr haystack) =
  QExpr (likeE needle haystack)

-- | Combine all the given boolean value 'QGenExpr's with the '&&.' operator.
allE :: ( IsSql92ExpressionSyntax syntax, HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool) =>
        [ QGenExpr context syntax s Bool ] -> QGenExpr context syntax s Bool
allE es = fromMaybe (QExpr (valueE (sqlValueSyntax True))) $
          foldl (\expr x ->
                   Just $ maybe x (\e -> e &&. x) expr)
                Nothing es

-- * UPDATE operators

-- | Extract an expression representing the current (non-UPDATEd) value of a 'QField'
current_ :: IsSql92ExpressionSyntax expr
         => QField s ty -> QExpr expr s ty
current_ (QField _ nm) = QExpr (fieldE (unqualifiedField nm))

infix 4 <-.
class SqlUpdatable expr s lhs rhs | rhs -> expr, lhs -> s, rhs -> s, lhs s expr -> rhs, rhs -> lhs where
  -- | Update a 'QField' or 'Beamable' type containing 'QField's with the given
  --   'QExpr' or 'Beamable' type containing 'QExpr'
  (<-.) :: forall fieldName.
           IsSql92FieldNameSyntax fieldName
        => lhs
        -> rhs
        -> QAssignment fieldName expr s
instance SqlUpdatable expr s (QField s a) (QExpr expr s a) where
  QField _ fieldNm <-. QExpr expr =
    QAssignment [(unqualifiedField fieldNm, expr)]
instance Beamable tbl => SqlUpdatable expr s (tbl (QField s)) (tbl (QExpr expr s)) where
  (<-.) :: forall fieldName.
           IsSql92FieldNameSyntax fieldName
        => tbl (QField s) -> tbl (QExpr expr s)
        -> QAssignment fieldName expr s
  lhs <-. rhs =
    QAssignment $
    allBeamValues (\(Columnar' (Const assignments)) -> assignments) $
    runIdentity $
    zipBeamFieldsM (\(Columnar' (QField _ f) :: Columnar' (QField s) t) (Columnar' (QExpr e)) ->
                       pure (Columnar' (Const (unqualifiedField f, e)) :: Columnar' (Const (fieldName,expr)) t)) lhs rhs

-- | SQL @UNION@ operator
union_ :: forall select db s a.
          ( IsSql92SelectSyntax select
          , Projectible (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) a
          , ProjectibleInSelectSyntax select a
          , ThreadRewritable (QNested s) a)
       => Q select db (QNested s) a -> Q select db (QNested s) a
       -> Q select db s (WithRewrittenThread (QNested s) s a)
union_ (Q a) (Q b) = Q (liftF (QUnion False a b (rewriteThread (Proxy @s))))

-- | SQL @UNION ALL@ operator
unionAll_ :: forall select db s a.
             ( IsSql92SelectSyntax select
             , Projectible (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) a
             , ProjectibleInSelectSyntax select a
             , ThreadRewritable (QNested s) a)
          => Q select db (QNested s) a -> Q select db (QNested s) a
          -> Q select db s (WithRewrittenThread (QNested s) s a)
unionAll_ (Q a) (Q b) = Q (liftF (QUnion True a b (rewriteThread (Proxy @s))))

-- | SQL @INTERSECT@ operator
intersect_ :: forall select db s a.
              ( IsSql92SelectSyntax select
              , Projectible (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) a
              , ProjectibleInSelectSyntax select a
              , ThreadRewritable (QNested s) a)
           => Q select db (QNested s) a -> Q select db (QNested s) a
           -> Q select db s (WithRewrittenThread (QNested s) s a)
intersect_ (Q a) (Q b) = Q (liftF (QIntersect False a b (rewriteThread (Proxy @s))))

-- | SQL @INTERSECT ALL@ operator
intersectAll_ :: forall select db s a.
                 ( IsSql92SelectSyntax select
                 , Projectible (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) a
                 , ProjectibleInSelectSyntax select a
                 , ThreadRewritable (QNested s) a)
              => Q select db (QNested s) a -> Q select db (QNested s) a
              -> Q select db s (WithRewrittenThread (QNested s) s a)
intersectAll_ (Q a) (Q b) = Q (liftF (QIntersect True a b (rewriteThread (Proxy @s))))

-- | SQL @EXCEPT@ operator
except_ :: forall select db s a.
           ( IsSql92SelectSyntax select
           , Projectible (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) a
           , ProjectibleInSelectSyntax select a
           , ThreadRewritable (QNested s) a)
        => Q select db (QNested s) a -> Q select db (QNested s) a
        -> Q select db s (WithRewrittenThread (QNested s) s a)
except_ (Q a) (Q b) = Q (liftF (QExcept False a b (rewriteThread (Proxy @s))))

-- | SQL @EXCEPT ALL@ operator
exceptAll_ :: forall select db s a.
              ( IsSql92SelectSyntax select
              , Projectible (Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)) a
              , ProjectibleInSelectSyntax select a
              , ThreadRewritable (QNested s) a)
           => Q select db (QNested s) a -> Q select db (QNested s) a
           -> Q select db s (WithRewrittenThread (QNested s) s a)
exceptAll_ (Q a) (Q b) = Q (liftF (QExcept True a b (rewriteThread (Proxy @s))))

-- | Convenience function that allows you to use type applications to specify
--   the result of a 'QGenExpr'.
--
--   Useful to disambiguate the types of 'QGenExpr's without having to provide a
--   complete type signature. As an example, the 'countAll_' aggregate can
--   return a result of any 'Integral' type. Without further constraints, the
--   type is ambiguous. You can use 'as_' to disambiguate the return type.
--
--   For example, this is ambiguous
--
-- > aggregate_ (\_ -> countAll_) ..
--
--   But this is not
--
-- > aggregate_ (\_ -> as_ @Int countAll_) ..
--
as_ :: forall a ctxt syntax s. QGenExpr ctxt syntax s a -> QGenExpr ctxt syntax s a
as_ = id

-- * Marshalling between Haskell literals and QExprs

type family HaskellLiteralForQExpr x = a
type instance HaskellLiteralForQExpr (QGenExpr context syntax s a) = a
type instance HaskellLiteralForQExpr (table (QGenExpr context syntax s)) = table Identity

type family QExprSyntax x where
  QExprSyntax (QGenExpr ctxt syntax s a) = syntax

type SqlValableTable table expr =
   ( Beamable table
   , IsSql92ExpressionSyntax expr
   , FieldsFulfillConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax expr)) table )

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a

instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) a, IsSql92ExpressionSyntax syntax) =>
  SqlValable (QGenExpr ctxt syntax s a) where

  val_ = QExpr . valueE . sqlValueSyntax
instance ( Beamable table
         , IsSql92ExpressionSyntax syntax
         , FieldsFulfillConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)) table ) =>
  SqlValable (table (QGenExpr ctxt syntax s)) where
  val_ tbl =
    let fields :: table (WithConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)))
        fields = to (gWithConstrainedFields (Proxy @(HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)))
                                            (Proxy @(Rep (table Exposed))) (from tbl))
    in changeBeamRep (\(Columnar' (WithConstraint x :: WithConstraint (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax)) x)) ->
                         Columnar' (QExpr (valueE (sqlValueSyntax x)))) fields

-- * Window functions

noBounds_ :: QFrameBounds syntax
noBounds_ = QFrameBounds Nothing

fromBound_ :: IsSql2003WindowFrameBoundsSyntax syntax
           => QFrameBound (Sql2003WindowFrameBoundsBoundSyntax syntax)
           -> QFrameBounds syntax
fromBound_ start = bounds_ start Nothing

bounds_ :: IsSql2003WindowFrameBoundsSyntax syntax
        => QFrameBound (Sql2003WindowFrameBoundsBoundSyntax syntax)
        -> Maybe (QFrameBound (Sql2003WindowFrameBoundsBoundSyntax syntax))
        -> QFrameBounds syntax
bounds_ (QFrameBound start) end =
    QFrameBounds . Just $
    fromToBoundSyntax start
      (fmap (\(QFrameBound end') -> end') end)

unbounded_ :: IsSql2003WindowFrameBoundSyntax syntax
           => QFrameBound syntax
unbounded_ = QFrameBound unboundedSyntax

nrows_ :: IsSql2003WindowFrameBoundSyntax syntax
       => Int -> QFrameBound syntax
nrows_ x = QFrameBound (nrowsBoundSyntax x)

noPartition_, noOrder_ :: Maybe (QOrd syntax s Int)
noOrder_ = Nothing
noPartition_ = Nothing

partitionBy_, orderPartitionBy_ :: partition -> Maybe partition
partitionBy_  = Just
orderPartitionBy_ = Just

-- | Specify a window frame with all the options
frame_ :: ( IsSql2003ExpressionSyntax syntax
          , SqlOrderable (Sql2003WindowFrameOrderingSyntax (Sql2003ExpressionWindowFrameSyntax syntax)) ordering
          , Projectible syntax partition
          , Sql2003ExpressionSanityCheck syntax )
       => Maybe partition             {-^ PARTITION BY -}
       -> Maybe ordering              {-^ ORDER BY -}
       -> QFrameBounds (Sql2003WindowFrameBoundsSyntax (Sql2003ExpressionWindowFrameSyntax syntax)) {-^ RANGE / ROWS -}
       -> QWindow (Sql2003ExpressionWindowFrameSyntax syntax) s
frame_ partition_ ordering_ (QFrameBounds bounds) =
    QWindow $
    frameSyntax (case maybe [] project partition_ of
                   [] -> Nothing
                   xs -> Just xs)
                (case fmap makeSQLOrdering ordering_ of
                   Nothing -> Nothing
                   Just [] -> Nothing
                   Just xs -> Just xs)
                bounds

-- | Produce a window expression given an aggregate function and a window.
over_ :: IsSql2003ExpressionSyntax syntax =>
         QAgg syntax s a -> QWindow (Sql2003ExpressionWindowFrameSyntax syntax) s -> QWindowExpr syntax s a
over_ (QExpr a) (QWindow frame) = QExpr (overE a frame)

-- | Compute a query over windows.
--
--   The first function builds window frames using the 'frame_', 'partitionBy_',
--   etc functions. The return type can be a single frame, tuples of frame, or
--   any arbitrarily nested tuple of the above. Instances up to 8-tuples are
--   provided.
--
--   The second function builds the resulting projection using the result of the
--   subquery as well as the window frames built in the first function. In this
--   function, window expressions can be included in the output using the
--   'over_' function.
--
withWindow_ :: forall window a s r select db.
               ( ProjectibleWithPredicate WindowFrameContext (Sql2003ExpressionWindowFrameSyntax (Sql92SelectExpressionSyntax select)) window
               , Projectible (Sql92SelectExpressionSyntax select) r
               , Projectible (Sql92SelectExpressionSyntax select) a
               , ContextRewritable a
               , ThreadRewritable (QNested s) (WithRewrittenContext a QValueContext)
               , IsSql92SelectSyntax select)
            => (r -> window)      -- ^ Window builder function
            -> (r -> window -> a) -- ^ Projection builder function. Has access to the windows generated above
            -> Q select db (QNested s) r -- ^ Query to window over
            -> Q select db s (WithRewrittenThread (QNested s) s (WithRewrittenContext a QValueContext))
withWindow_ mkWindow mkProjection (Q windowOver)=
  Q (liftF (QWindowOver mkWindow mkProjection windowOver (rewriteThread (Proxy @s) . rewriteContext (Proxy @QValueContext))))

-- * Order bys

class SqlOrderable syntax a | a -> syntax where
    makeSQLOrdering :: a -> [ syntax ]
instance SqlOrderable syntax (QOrd syntax s a) where
    makeSQLOrdering (QExpr x) = [x]
instance SqlOrderable syntax a => SqlOrderable syntax [a] where
    makeSQLOrdering = concatMap makeSQLOrdering
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b ) => SqlOrderable syntax (a, b) where
    makeSQLOrdering (a, b) = makeSQLOrdering a <> makeSQLOrdering b
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c ) => SqlOrderable syntax (a, b, c) where
    makeSQLOrdering (a, b, c) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c
         , SqlOrderable syntax d ) => SqlOrderable syntax (a, b, c, d) where
    makeSQLOrdering (a, b, c, d) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d
instance ( SqlOrderable syntax a
         , SqlOrderable syntax b
         , SqlOrderable syntax c
         , SqlOrderable syntax d
         , SqlOrderable syntax e ) => SqlOrderable syntax (a, b, c, d, e) where
    makeSQLOrdering (a, b, c, d, e) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d <> makeSQLOrdering e

-- | Order by the given expressions. The return type of the ordering key should
--   either be the result of 'asc_' or 'desc_' (or another ordering 'QOrd'
--   generated by a backend-specific ordering) or an (possibly nested) tuple of
--   results of the former.
--
--   The <https://tathougies.github.io/beam/user-guide/queries/ordering manual section>
--   has more information.
orderBy_ :: forall s a ordering syntax db.
            ( Projectible (Sql92SelectExpressionSyntax syntax) a
            , SqlOrderable (Sql92SelectOrderingSyntax syntax) ordering
            , ThreadRewritable (QNested s) a) =>
            (a -> ordering) -> Q syntax db (QNested s) a -> Q syntax db s (WithRewrittenThread (QNested s) s a)
orderBy_ orderer (Q q) =
    Q (liftF (QOrderBy (makeSQLOrdering . orderer) q (rewriteThread (Proxy @s))))

-- | Produce a 'QOrd' corresponding to a SQL @ASC@ ordering
asc_ :: forall syntax s a. IsSql92OrderingSyntax syntax
     => QExpr (Sql92OrderingExpressionSyntax syntax) s a
     -> QOrd syntax s a
asc_ (QExpr e) = QExpr (ascOrdering e)

-- | Produce a 'QOrd' corresponding to a SQL @DESC@ ordering
desc_ :: forall syntax s a. IsSql92OrderingSyntax syntax
      => QExpr (Sql92OrderingExpressionSyntax syntax) s a
      -> QOrd syntax s a
desc_ (QExpr e) = QExpr (descOrdering e)

-- * Subqueries

-- * Nullable conversions

-- | Type class for things that can be nullable. This includes 'QExpr (Maybe a)', 'tbl (Nullable
-- QExpr)', and 'PrimaryKey tbl (Nullable QExpr)'
class SqlJustable a b | b -> a where

    -- | Given something of type 'QExpr a', 'tbl QExpr', or 'PrimaryKey tbl
    --   QExpr', turn it into a 'QExpr (Maybe a)', 'tbl (Nullable QExpr)', or
    --   'PrimaryKey t (Nullable QExpr)' respectively that contains the same
    --   values.
    just_ :: a -> b

    -- | Return either a 'QExpr (Maybe x)' representing 'Nothing' or a nullable 'Table' or
    --   'PrimaryKey' filled with 'Nothing'.
    nothing_ :: b

instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SqlNull) =>
    SqlJustable (QExpr syntax s a) (QExpr syntax s (Maybe a)) where

    just_ (QExpr e) = QExpr e
    nothing_ = QExpr (valueE (sqlValueSyntax SqlNull))

instance {-# OVERLAPPING #-} ( Table t
                             , IsSql92ExpressionSyntax syntax
                             , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SqlNull ) =>
    SqlJustable (PrimaryKey t (QExpr syntax s)) (PrimaryKey t (Nullable (QExpr syntax s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' nothing_) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} ( Table t
                             , IsSql92ExpressionSyntax syntax
                             , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) SqlNull ) =>
    SqlJustable (t (QExpr syntax s)) (t (Nullable (QExpr syntax s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' nothing_) (tblSkeleton :: TableSkeleton t)

instance {-# OVERLAPPING #-} Table t => SqlJustable (PrimaryKey t Identity) (PrimaryKey t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' Nothing) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} Table t => SqlJustable (t Identity) (t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' Nothing) (tblSkeleton :: TableSkeleton t)

-- * Nullable checking

data QIfCond expr s a = QIfCond (QExpr expr s Bool) (QExpr expr s a)
newtype QIfElse expr s a = QIfElse (QExpr expr s a)

then_ :: QExpr expr s Bool -> QExpr expr s a -> QIfCond expr s a
then_ cond res = QIfCond cond res

else_ :: QExpr expr s a -> QIfElse expr s a
else_ = QIfElse

if_ :: IsSql92ExpressionSyntax expr =>
       [ QIfCond expr s a ]
    -> QIfElse expr s a
    -> QExpr expr s a
if_ conds (QIfElse (QExpr elseExpr)) =
  QExpr (caseE (map (\(QIfCond (QExpr cond) (QExpr res)) -> (cond, res)) conds) elseExpr)

-- | SQL @COALESCE@ support
coalesce_ :: IsSql92ExpressionSyntax expr =>
             [ QExpr expr s (Maybe a) ] -> QExpr expr s a -> QExpr expr s a
coalesce_ qs (QExpr onNull) =
  QExpr (coalesceE (map (\(QExpr q) -> q) qs <> [ onNull ]))

-- | Type class for anything which can be checked for null-ness. This includes 'QExpr (Maybe a)' as
-- well as 'Table's or 'PrimaryKey's over 'Nullable QExpr'.
class IsSql92ExpressionSyntax syntax => SqlDeconstructMaybe syntax a nonNullA s | a s -> syntax, a -> nonNullA, a -> s, nonNullA -> s where
    -- | Returns a 'QExpr' that evaluates to true when the first argument is not null
    isJust_ :: a -> QExpr syntax s Bool

    -- | Returns a 'QExpr' that evaluates to true when the first argument is null
    isNothing_ :: a -> QExpr syntax s Bool

    -- | Given an object (third argument) which may or may not be null, return the default value if
    -- null (first argument), or transform the value that could be null to yield the result of the
    -- expression (second argument)
    maybe_ :: QExpr syntax s y -> (nonNullA -> QExpr syntax s y) -> a -> QExpr syntax s y

instance IsSql92ExpressionSyntax syntax => SqlDeconstructMaybe syntax (QExpr syntax s (Maybe x)) (QExpr syntax s x) s where
    isJust_ (QExpr x) = QExpr (isNotNullE x)
    isNothing_ (QExpr x) = QExpr (isNullE x)

    maybe_ (QExpr onNothing) onJust (QExpr e) = let QExpr onJust' = onJust (QExpr e)
                                                in QExpr (caseE [(isNotNullE e, onJust')] onNothing)

instance ( IsSql92ExpressionSyntax syntax
         , HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) Bool
         , Beamable t )
    => SqlDeconstructMaybe syntax (t (Nullable (QExpr syntax s))) (t (QExpr syntax s)) s where
    isJust_ t = allE (allBeamValues (\(Columnar' e) -> isJust_ e) t)
    isNothing_ t = allE (allBeamValues (\(Columnar' e) -> isNothing_ e) t)
    maybe_ (QExpr onNothing) onJust tbl =
      let QExpr onJust' = onJust (changeBeamRep (\(Columnar' (QExpr e)) -> Columnar' (QExpr e)) tbl)
          QExpr cond = isJust_ tbl
      in QExpr (caseE [(cond, onJust')] onNothing)
