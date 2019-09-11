{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Database.Beam.Query.Combinators
    ( -- * Various SQL functions and constructs
      coalesce_, fromMaybe_, position_
    , charLength_, octetLength_, bitLength_
    , currentTimestamp_
    , lower_, upper_
    , trim_

    -- ** @IF-THEN-ELSE@ support
    , if_, then_, else_
    , then_'

    -- * SQL @UPDATE@ assignments
    , (<-.), current_

    -- * Project Haskell values to 'QGenExpr's
    , HaskellLiteralForQExpr
    , SqlValable(..), SqlValableTable
    , default_

    -- * General query combinators

    , all_, values_
    , allFromView_, join_, join_'
    , guard_, guard_', filter_, filter_'
    , related_, relatedBy_, relatedBy_'
    , leftJoin_, leftJoin_'
    , perhaps_, outerJoin_, outerJoin_'
    , subselect_, references_

    , nub_

    , SqlJustable(..)
    , SqlDeconstructMaybe(..)
    , SqlOrderable
    , QIfCond, QIfElse
    , (<|>.)

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
    , orderBy_, asc_, desc_, nullsFirst_, nullsLast_
    ) where

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL

import Database.Beam.Query.Internal
import Database.Beam.Query.Ord
import Database.Beam.Query.Operator
import Database.Beam.Query.Types

import Database.Beam.Schema.Tables

import Control.Monad.Identity
import Control.Monad.Free
import Control.Applicative

#if !MIN_VERSION_base(4, 11, 0)
import Control.Monad.Writer hiding ((<>))
import Data.Semigroup
#endif

import Data.Maybe
import Data.Proxy
import Data.Time (LocalTime)

import GHC.Generics

-- | Introduce all entries of a table into the 'Q' monad
all_ :: ( Database be db, BeamSqlBackend be )
       => DatabaseEntity be db (TableEntity table)
       -> Q be db s (table (QExpr be s))
all_ (DatabaseEntity dt@(DatabaseTable {})) =
    Q $ liftF (QAll (\_ -> fromTable (tableNamed (tableName (dbTableSchema dt) (dbTableCurrentName dt))) . Just . (,Nothing))
                    (tableFieldsToExpressions (dbTableSettings dt))
                    (\_ -> Nothing) snd)

-- | Introduce all entries of a view into the 'Q' monad
allFromView_ :: ( Database be db, Beamable table
                , BeamSqlBackend be )
               => DatabaseEntity be db (ViewEntity table)
               -> Q be db s (table (QExpr be s))
allFromView_ (DatabaseEntity vw) =
    Q $ liftF (QAll (\_ -> fromTable (tableNamed (tableName (dbViewSchema vw) (dbViewCurrentName vw))) . Just . (,Nothing))
                    (tableFieldsToExpressions (dbViewSettings vw))
                    (\_ -> Nothing) snd)

-- | SQL @VALUES@ clause. Introduce the elements of the given list as
-- rows in a joined table.
values_ :: forall be db s a
         . ( Projectible be a
           , BeamSqlBackend be )
        => [ a ] -> Q be db s a
values_ rows =
    Q $ liftF (QAll (\tblPfx -> fromTable (tableFromValues (map (\row -> project (Proxy @be) row tblPfx) rows)) . Just . (,Just fieldNames))
                    (\tblNm' -> fst $ mkFieldNames (qualifiedField tblNm'))
                    (\_ -> Nothing) snd)
    where
      fieldNames = snd $ mkFieldNames @be @a unqualifiedField

-- | Introduce all entries of a table into the 'Q' monad based on the
--   given QExpr. The join condition is expected to return a
--   'Bool'. For a version that takes 'SqlBool' (a possibly @UNKNOWN@
--   boolean, that maps more closely to the SQL standard), see
--   'join_''.
join_ :: ( Database be db, Table table, BeamSqlBackend be )
      => DatabaseEntity be db (TableEntity table)
      -> (table (QExpr be s) -> QExpr be s Bool)
      -> Q be db s (table (QExpr be s))
join_ tbl mkOn = join_' tbl (sqlBool_ . mkOn)

-- | Like 'join_', but accepting an @ON@ condition that returns
-- 'SqlBool'
join_' :: ( Database be db, Table table, BeamSqlBackend be )
       => DatabaseEntity be db (TableEntity table)
       -> (table (QExpr be s) -> QExpr be s SqlBool)
       -> Q be db s (table (QExpr be s))
join_' (DatabaseEntity tbl@(DatabaseTable {})) mkOn =
    Q $ liftF (QAll (\_ -> fromTable (tableNamed (tableName (dbTableSchema tbl) (dbTableCurrentName tbl))) . Just . (, Nothing))
                    (tableFieldsToExpressions (dbTableSettings tbl))
                    (\tbl' -> let QExpr on = mkOn tbl' in Just on) snd)

-- | Introduce a table using a left join with no ON clause. Because this is not
--   an inner join, the resulting table is made nullable. This means that each
--   field that would normally have type 'QExpr x' will now have type 'QExpr
--   (Maybe x)'.
perhaps_ :: forall s r be db.
          ( Projectible be r, BeamSqlBackend be
          , ThreadRewritable (QNested s) r
          , Retaggable (QExpr be s) (WithRewrittenThread (QNested s) s r) )
         => Q be db (QNested s) r
         -> Q be db s (Retag Nullable (WithRewrittenThread (QNested s) s r))
perhaps_ (Q sub) =
  Q $ liftF (QArbitraryJoin
              sub leftJoin
              (\_ -> Nothing)
              (\r -> retag (\(Columnar' (QExpr e) :: Columnar' (QExpr be s) a) ->
                                            Columnar' (QExpr e) :: Columnar' (Nullable (QExpr be s)) a) $
                                  rewriteThread (Proxy @s) r))

-- | Outer join. every row of each table, returning @NULL@ for any row
-- of either table for which the join condition finds no related rows.
--
-- This expects a join expression returning 'Bool', for a version that
-- accepts a 'SqlBool' (a possibly @UNKNOWN@ boolean, that maps more
-- closely to the SQL standard), see 'outerJoin_''
outerJoin_ :: forall s a b be db.
              ( BeamSqlBackend be, BeamSqlBackendSupportsOuterJoin be
              , Projectible be a, Projectible be b
              , ThreadRewritable (QNested s) a, ThreadRewritable (QNested s) b
              , Retaggable (QExpr be s) (WithRewrittenThread (QNested s) s a)
              , Retaggable (QExpr be s) (WithRewrittenThread (QNested s) s b)
              )
           => Q be db (QNested s) a
           -> Q be db (QNested s) b
           -> ( (WithRewrittenThread (QNested s) s a, WithRewrittenThread (QNested s) s b) -> QExpr be s Bool )
           -> Q be db s ( Retag Nullable (WithRewrittenThread (QNested s) s a)
                        , Retag Nullable (WithRewrittenThread (QNested s) s b) )
outerJoin_ a b on_ = outerJoin_' a b (sqlBool_ . on_)

-- | Like 'outerJoin_', but accepting 'SqlBool'. Pairs of rows for
-- which the join condition is unknown are considered to be unrelated,
-- by SQL compliant databases at least.
outerJoin_' :: forall s a b be db.
               ( BeamSqlBackend be, BeamSqlBackendSupportsOuterJoin be
               , Projectible be a, Projectible be b
               , ThreadRewritable (QNested s) a, ThreadRewritable (QNested s) b
               , Retaggable (QExpr be s) (WithRewrittenThread (QNested s) s a)
               , Retaggable (QExpr be s) (WithRewrittenThread (QNested s) s b)
               )
            => Q be db (QNested s) a
            -> Q be db (QNested s) b
            -> ( (WithRewrittenThread (QNested s) s a, WithRewrittenThread (QNested s) s b) -> QExpr be s SqlBool )
            -> Q be db s ( Retag Nullable (WithRewrittenThread (QNested s) s a)
                         , Retag Nullable (WithRewrittenThread (QNested s) s b) )
outerJoin_' (Q a) (Q b) on_ =
  Q $ liftF (QTwoWayJoin a b outerJoin
              (\(a', b') ->
                 let QExpr e = on_ (rewriteThread (Proxy @s) a', rewriteThread (Proxy @s) b')
                 in Just e)
              (\(a', b') ->
                 let retag' :: (ThreadRewritable (QNested s) x, Retaggable (QExpr be s) (WithRewrittenThread (QNested s) s x))
                            => x -> Retag Nullable (WithRewrittenThread (QNested s) s x)
                     retag' = retag (\(Columnar' (QExpr e) :: Columnar' (QExpr be s) x) ->
                                        Columnar' (QExpr e) :: Columnar' (Nullable (QExpr be s)) x) .
                              rewriteThread (Proxy @s)
                 in ( retag' a', retag' b' )))

-- | Introduce a table using a left join. The ON clause is required here.Because
--   this is not an inner join, the resulting table is made nullable. This means
--   that each field that would normally have type 'QExpr x' will now have type
--   'QExpr (Maybe x)'.
--
--   The @ON@ condition given must return 'Bool'. For a version that
--   accepts an @ON@ condition returning 'SqlBool', see 'leftJoin_''.
leftJoin_ :: forall s r be db.
           ( BeamSqlBackend be, Projectible be r
           , ThreadRewritable (QNested s) r
           , Retaggable (QExpr be s) (WithRewrittenThread (QNested s) s r) )
          => Q be db (QNested s) r
          -> (WithRewrittenThread (QNested s) s r -> QExpr be s Bool)
          -> Q be db s (Retag Nullable (WithRewrittenThread (QNested s) s r))
leftJoin_ sub on_ = leftJoin_' sub (sqlBool_ . on_)

-- | Like 'leftJoin_', but accepts an @ON@ clause returning 'SqlBool'.
leftJoin_' :: forall s r be db.
            ( BeamSqlBackend be, Projectible be r
            , ThreadRewritable (QNested s) r
            , Retaggable (QExpr be s) (WithRewrittenThread (QNested s) s r) )
           => Q be db (QNested s) r
           -> (WithRewrittenThread (QNested s) s r -> QExpr be s SqlBool)
           -> Q be db s (Retag Nullable (WithRewrittenThread (QNested s) s r))
leftJoin_' (Q sub) on_ =
  Q $ liftF (QArbitraryJoin
               sub leftJoin
               (\r -> let QExpr e = on_ (rewriteThread (Proxy @s) r) in Just e)
               (\r -> retag (\(Columnar' (QExpr e) :: Columnar' (QExpr be s) a) ->
                                Columnar' (QExpr e) :: Columnar' (Nullable (QExpr be s)) a) $
                      rewriteThread (Proxy @s) r))

subselect_ :: forall s r be db.
            ( ThreadRewritable (QNested s) r
            , Projectible be r )
           => Q be db (QNested s) r
           -> Q be db s (WithRewrittenThread (QNested s) s r)
subselect_ (Q q') =
  Q (liftF (QSubSelect q' (rewriteThread (Proxy @s))))

-- | Only allow results for which the 'QExpr' yields 'True'. For a
-- version that operates over possibly @NULL@ 'SqlBool's, see
-- 'guard_''.
guard_ :: forall be db s
        . BeamSqlBackend be
       => QExpr be s Bool -> Q be db s ()
guard_ = guard_' . sqlBool_

-- | Only allow results for which the 'QExpr' yields @TRUE@.
--
-- This function operates over 'SqlBool', which are like haskell
-- 'Bool's, except for the special @UNKNOWN@ value that occurs when
-- comparisons include @NULL@. For a version that operates over known
-- non-@NULL@ booleans, see 'guard_'.
guard_' :: forall be db s
         . BeamSqlBackend be
        => QExpr be s SqlBool -> Q be db s ()
guard_' (QExpr guardE') = Q (liftF (QGuard guardE' ()))

-- | Synonym for @clause >>= \x -> guard_ (mkExpr x)>> pure x@. Use 'filter_'' for comparisons with 'SqlBool'
filter_ :: forall r be db s
         . BeamSqlBackend be
        => (r -> QExpr be s Bool)
        -> Q be db s r -> Q be db s r
filter_ mkExpr clause = clause >>= \x -> guard_ (mkExpr x) >> pure x

-- | Synonym for @clause >>= \x -> guard_' (mkExpr x)>> pure x@. Use 'filter_' for comparisons with 'Bool'
filter_' :: forall r be db s
          . BeamSqlBackend be
        => (r -> QExpr be s SqlBool)
        -> Q be db s r -> Q be db s r
filter_' mkExpr clause = clause >>= \x -> guard_' (mkExpr x) >> pure x

-- | Introduce all entries of the given table which are referenced by the given 'PrimaryKey'
related_ :: forall be db rel s
          . ( Database be db, Table rel, BeamSqlBackend be
            , HasTableEquality be (PrimaryKey rel)
            )
         => DatabaseEntity be db (TableEntity rel)
         -> PrimaryKey rel (QExpr be s)
         -> Q be db s (rel (QExpr be s))
related_ relTbl relKey =
  join_ relTbl (\rel -> relKey ==. primaryKey rel)

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_ :: forall be db rel s
            . ( Database be db, Table rel, BeamSqlBackend be )
           => DatabaseEntity be db (TableEntity rel)
           -> (rel (QExpr be s) -> QExpr be s Bool)
           -> Q be db s (rel (QExpr be s))
relatedBy_ = join_

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_' :: forall be db rel s
             . ( Database be db, Table rel, BeamSqlBackend be )
            => DatabaseEntity be db (TableEntity rel)
            -> (rel (QExpr be s) -> QExpr be s SqlBool)
            -> Q be db s (rel (QExpr be s))
relatedBy_' = join_'

-- | Generate an appropriate boolean 'QGenExpr' comparing the given foreign key
--   to the given table. Useful for creating join conditions.
references_ :: ( Table t, BeamSqlBackend be
               , HasTableEquality be (PrimaryKey t) )
            => PrimaryKey t (QGenExpr ctxt be s) -> t (QGenExpr ctxt be s) -> QGenExpr ctxt be s Bool
references_ fk tbl = fk ==. pk tbl

-- | Only return distinct values from a query
nub_ :: ( BeamSqlBackend be, Projectible be r )
     => Q be db s r -> Q be db s r
nub_ (Q sub) = Q $ liftF (QDistinct (\_ _ -> setQuantifierDistinct) sub id)

-- | Limit the number of results returned by a query.
limit_ :: forall s a be db
        . ( Projectible be a
          , ThreadRewritable (QNested s) a )
        => Integer -> Q be db (QNested s) a -> Q be db s (WithRewrittenThread (QNested s) s a)
limit_ limit' (Q q) =
  Q (liftF (QLimit limit' q (rewriteThread (Proxy @s))))

-- | Drop the first `offset'` results.
offset_ :: forall s a be db
         . ( Projectible be a
           , ThreadRewritable (QNested s) a )
        => Integer -> Q be db (QNested s) a -> Q be db s (WithRewrittenThread (QNested s) s a)
offset_ offset' (Q q) =
  Q (liftF (QOffset offset' q (rewriteThread (Proxy @s))))

-- | Use the SQL @EXISTS@ operator to determine if the given query returns any results
exists_ :: ( BeamSqlBackend be, HasQBuilder be, Projectible be a)
        => Q be db s a -> QExpr be s Bool
exists_ q = QExpr (\tbl -> existsE (buildSqlQuery tbl q))

-- | Use the SQL @UNIQUE@ operator to determine if the given query produces a unique result
unique_ :: ( BeamSqlBackend be, HasQBuilder be, Projectible be a)
        => Q be db s a -> QExpr be s Bool
unique_ q = QExpr (\tbl -> uniqueE (buildSqlQuery tbl q))

-- | Use the SQL99 @DISTINCT@ operator to determine if the given query produces a distinct result
distinct_ :: ( BeamSqlBackend be, BeamSql99ExpressionBackend be, HasQBuilder be, Projectible be a)
          => Q be db s a -> QExpr be s Bool
distinct_ q = QExpr (\tbl -> distinctE (buildSqlQuery tbl q))

-- | Project the (presumably) singular result of the given query as an expression
subquery_ :: ( BeamSqlBackend be, HasQBuilder be, Projectible be (QExpr be s a) )
          => Q be db s (QExpr be s a)
          -> QGenExpr ctxt be s a
subquery_ q =
  QExpr (\tbl -> subqueryE (buildSqlQuery tbl q))

-- | SQL @CHAR_LENGTH@ function
charLength_ :: ( BeamSqlBackend be, BeamSqlBackendIsString be text )
            => QGenExpr context be s text -> QGenExpr context be s Int
charLength_ (QExpr s) = QExpr (charLengthE <$> s)

-- | SQL @OCTET_LENGTH@ function
octetLength_ :: ( BeamSqlBackend be, BeamSqlBackendIsString be text )
             => QGenExpr context be s text -> QGenExpr context be s Int
octetLength_ (QExpr s) = QExpr (octetLengthE <$> s)

-- | SQL @BIT_LENGTH@ function
bitLength_ :: BeamSqlBackend be
           => QGenExpr context be s SqlBitString -> QGenExpr context be s Int
bitLength_ (QExpr x) = QExpr (bitLengthE <$> x)

-- | SQL @CURRENT_TIMESTAMP@ function
currentTimestamp_ :: BeamSqlBackend be => QGenExpr ctxt be s LocalTime
currentTimestamp_ = QExpr (pure currentTimestampE)

-- | SQL @POSITION(.. IN ..)@ function
position_ :: ( BeamSqlBackendIsString be text
             , BeamSqlBackend be, Integral b )
          => QExpr be s text -> QExpr be s text -> QExpr be s b
position_ (QExpr needle) (QExpr haystack) =
  QExpr (liftA2 likeE needle haystack)

-- | SQL @LOWER@ function
lower_ ::  ( BeamSqlBackendIsString be text
           , BeamSqlBackend be )
       => QGenExpr context be s text -> QGenExpr context be s text
lower_ (QExpr s) = QExpr (lowerE <$> s)

-- | SQL @UPPER@ function
upper_ :: ( BeamSqlBackendIsString be text
          , BeamSqlBackend be )
       => QGenExpr context be s text -> QGenExpr context be s text
upper_ (QExpr s) = QExpr (upperE <$> s)

-- | SQL @TRIM@ function
trim_ :: ( BeamSqlBackendIsString be text
         , BeamSqlBackend be )
       => QGenExpr context be s text -> QGenExpr context be s text
trim_ (QExpr s) = QExpr (trimE <$> s)

-- | Combine all the given boolean value 'QGenExpr's with the '&&.' operator.
allE :: BeamSqlBackend be
     => [ QGenExpr context be s Bool ] -> QGenExpr context be s Bool
allE es = fromMaybe (QExpr (pure (valueE (sqlValueSyntax True)))) $
          foldl (\expr x ->
                   Just $ maybe x (\e -> e &&. x) expr)
                Nothing es

-- * UPDATE operators

-- | Extract an expression representing the current (non-UPDATEd) value of a 'QField'
current_ :: BeamSqlBackend be => QField s ty -> QExpr be s ty
current_ (QField False _ nm) = QExpr (pure (fieldE (unqualifiedField nm)))
current_ (QField True tbl nm) = QExpr (pure (fieldE (qualifiedField tbl nm)))

infix 4 <-.
class BeamSqlBackend be =>
  SqlUpdatable be s lhs rhs | rhs -> be, lhs -> s
                            , rhs -> s, lhs s be -> rhs
                            , rhs -> lhs where


  -- | Update a 'QField' or 'Beamable' type containing 'QField's with the given
  --   'QExpr' or 'Beamable' type containing 'QExpr'
  (<-.) :: lhs
        -> rhs
        -> QAssignment be s

instance BeamSqlBackend be => SqlUpdatable be s (QField s a) (QExpr be s a) where
  QField _ _ nm <-. QExpr expr =
    QAssignment [(unqualifiedField nm, expr "t")]

instance (BeamSqlBackend be, Beamable tbl) => SqlUpdatable be s (tbl (QField s)) (tbl (QExpr be s)) where
  lhs <-. rhs =
    QAssignment $
    allBeamValues (\(Columnar' (Const assignments)) -> assignments) $
    runIdentity $
    zipBeamFieldsM (\(Columnar' (QField _ _ f) :: Columnar' (QField s) t) (Columnar' (QExpr e)) ->
                       pure (Columnar' (Const (unqualifiedField f, e "t")) :: Columnar' (Const (BeamSqlBackendFieldNameSyntax be, BeamSqlBackendExpressionSyntax be)) t)) lhs rhs

instance (BeamSqlBackend be, Beamable tbl) => SqlUpdatable be s (tbl (Nullable (QField s))) (tbl (Nullable (QExpr be s))) where
  lhs <-. rhs =
    let lhs' = changeBeamRep (\(Columnar' (QField q tblName fieldName') :: Columnar' (Nullable (QField s)) a) ->
                                Columnar' (QField q tblName fieldName') :: Columnar' (QField s)  a) lhs
        rhs' = changeBeamRep (\(Columnar' (QExpr e) :: Columnar' (Nullable (QExpr be s)) a) ->
                                Columnar' (QExpr e) :: Columnar' (QExpr be s) a) rhs
    in lhs' <-. rhs'

-- | SQL @UNION@ operator
union_ :: forall be db s a
        . ( BeamSqlBackend be, Projectible be a
          , ThreadRewritable (QNested s) a )
       => Q be db (QNested s) a -> Q be db (QNested s) a
       -> Q be db s (WithRewrittenThread (QNested s) s a)
union_ (Q a) (Q b) = Q (liftF (QSetOp (unionTables False) a b (rewriteThread (Proxy @s))))

-- | SQL @UNION ALL@ operator
unionAll_ :: forall be db s a.
             ( BeamSqlBackend be, Projectible be a
             , ThreadRewritable (QNested s) a)
          => Q be db (QNested s) a -> Q be db (QNested s) a
          -> Q be db s (WithRewrittenThread (QNested s) s a)
unionAll_ (Q a) (Q b) = Q (liftF (QSetOp (unionTables True) a b (rewriteThread (Proxy @s))))

-- | SQL @INTERSECT@ operator
intersect_ :: forall be db s a.
              ( BeamSqlBackend be, Projectible be a
              , ThreadRewritable (QNested s) a)
           => Q be db (QNested s) a -> Q be db (QNested s) a
           -> Q be db s (WithRewrittenThread (QNested s) s a)
intersect_ (Q a) (Q b) = Q (liftF (QSetOp (intersectTables False) a b (rewriteThread (Proxy @s))))

-- | SQL @INTERSECT ALL@ operator
intersectAll_ :: forall be db s a.
                 ( BeamSqlBackend be, Projectible be a
                 , ThreadRewritable (QNested s) a)
              => Q be db (QNested s) a -> Q be db (QNested s) a
              -> Q be db s (WithRewrittenThread (QNested s) s a)
intersectAll_ (Q a) (Q b) = Q (liftF (QSetOp (intersectTables True) a b (rewriteThread (Proxy @s))))

-- | SQL @EXCEPT@ operator
except_ :: forall be db s a.
           ( BeamSqlBackend be, Projectible be a
           , ThreadRewritable (QNested s) a)
        => Q be db (QNested s) a -> Q be db (QNested s) a
        -> Q be db s (WithRewrittenThread (QNested s) s a)
except_ (Q a) (Q b) = Q (liftF (QSetOp (exceptTable False) a b (rewriteThread (Proxy @s))))

-- | SQL @EXCEPT ALL@ operator
exceptAll_ :: forall be db s a.
              ( BeamSqlBackend be, Projectible be a
              , ThreadRewritable (QNested s) a)
           => Q be db (QNested s) a -> Q be db (QNested s) a
           -> Q be db s (WithRewrittenThread (QNested s) s a)
exceptAll_ (Q a) (Q b) = Q (liftF (QSetOp (exceptTable True) a b (rewriteThread (Proxy @s))))

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
as_ :: forall a ctxt be s. QGenExpr ctxt be s a -> QGenExpr ctxt be s a
as_ = id

-- * Marshalling between Haskell literals and QExprs

type family HaskellLiteralForQExpr x = a
type instance HaskellLiteralForQExpr (QGenExpr context be s a) = a
type instance HaskellLiteralForQExpr (table (QGenExpr context be s)) = table Identity
type instance HaskellLiteralForQExpr (table (Nullable f)) = HaskellLiteralForQExpr_AddNullable (HaskellLiteralForQExpr (table f))

type family HaskellLiteralForQExpr_AddNullable x = a
type instance HaskellLiteralForQExpr_AddNullable (tbl f) = tbl (Nullable f)

type SqlValableTable be table =
   ( Beamable table
   , FieldsFulfillConstraint (HasSqlValueSyntax (BeamSqlBackendValueSyntax be)) table )

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a

instance ( BeamSqlBackendCanSerialize be a, BeamSqlBackend be ) =>
  SqlValable (QGenExpr ctxt be s a) where

  val_ = QExpr . pure . valueE . sqlValueSyntax
instance ( Beamable table, BeamSqlBackend be
         , FieldsFulfillConstraint (BeamSqlBackendCanSerialize be) table ) =>
  SqlValable (table (QGenExpr ctxt be s)) where
  val_ tbl =
    let fields :: table (WithConstraint (BeamSqlBackendCanSerialize be))
        fields = to (gWithConstrainedFields (Proxy @(BeamSqlBackendCanSerialize be))
                                            (Proxy @(Rep (table Exposed))) (from tbl))
    in changeBeamRep (\(Columnar' (WithConstraint x :: WithConstraint (BeamSqlBackendCanSerialize be) x)) ->
                         Columnar' (QExpr (pure (valueE (sqlValueSyntax x))))) fields
instance ( Beamable table, BeamSqlBackend be
         , FieldsFulfillConstraintNullable (BeamSqlBackendCanSerialize be) table ) =>

         SqlValable (table (Nullable (QGenExpr ctxt be s))) where

  val_ tbl =
    let fields :: table (Nullable (WithConstraint (BeamSqlBackendCanSerialize be)))
        fields = to (gWithConstrainedFields (Proxy @(BeamSqlBackendCanSerialize be))
                                            (Proxy @(Rep (table (Nullable Exposed)))) (from tbl))
    in changeBeamRep (\(Columnar' (WithConstraint x :: WithConstraint (BeamSqlBackendCanSerialize be) (Maybe x))) ->
                         Columnar' (QExpr (pure (valueE (sqlValueSyntax x))))) fields

default_ :: BeamSqlBackend be => QGenExpr ctxt be s a
default_ = QExpr (pure defaultE)

-- * Window functions

noBounds_ :: QFrameBounds be
noBounds_ = QFrameBounds Nothing

fromBound_ :: BeamSql2003ExpressionBackend be
           => QFrameBound be -> QFrameBounds be
fromBound_ start = bounds_ start Nothing

bounds_ :: BeamSql2003ExpressionBackend be
        => QFrameBound be
        -> Maybe (QFrameBound be)
        -> QFrameBounds be
bounds_ (QFrameBound start) end =
    QFrameBounds . Just $
    fromToBoundSyntax start
      (fmap (\(QFrameBound end') -> end') end)

unbounded_ :: BeamSql2003ExpressionBackend be => QFrameBound be
unbounded_ = QFrameBound unboundedSyntax

nrows_ :: BeamSql2003ExpressionBackend be
       => Int -> QFrameBound be
nrows_ x = QFrameBound (nrowsBoundSyntax x)

noPartition_ :: Maybe (QExpr be s Int)
noPartition_ = Nothing

noOrder_ :: Maybe (QOrd be s Int)
noOrder_ = Nothing

partitionBy_, orderPartitionBy_ :: partition -> Maybe partition
partitionBy_  = Just
orderPartitionBy_ = Just

-- | Specify a window frame with all the options
frame_ :: forall be ordering partition s
        . ( BeamSql2003ExpressionBackend be
          , SqlOrderable be ordering
          , Projectible be partition )
       => Maybe partition {-^ PARTITION BY -}
       -> Maybe ordering  {-^ ORDER BY -}
       -> QFrameBounds be {-^ RANGE / ROWS -}
       -> QWindow be s
frame_ partition_ ordering_ (QFrameBounds bounds) =
    QWindow $ \tblPfx ->
    frameSyntax (case maybe [] (flip (project (Proxy @be)) tblPfx) partition_ of
                   [] -> Nothing
                   xs -> Just xs)
                (case fmap (makeSQLOrdering (Proxy @be)) ordering_ of
                   Nothing -> Nothing
                   Just [] -> Nothing
                   Just xs -> Just (sequenceA xs tblPfx))
                bounds

-- | Produce a window expression given an aggregate function and a window.
over_ :: BeamSql2003ExpressionBackend be
      => QAgg be s a -> QWindow be s -> QWindowExpr be s a
over_ (QExpr a) (QWindow frame) = QExpr (overE <$> a <*> frame)

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
withWindow_ :: forall window a s r be db
             . ( ProjectibleWithPredicate WindowFrameContext be (WithExprContext (BeamSqlBackendWindowFrameSyntax' be)) window
               , Projectible be r, Projectible be a
               , ContextRewritable a
               , ThreadRewritable (QNested s) (WithRewrittenContext a QValueContext) )
            => (r -> window)      -- ^ Window builder function
            -> (r -> window -> a) -- ^ Projection builder function. Has access to the windows generated above
            -> Q be db (QNested s) r -- ^ Query to window over
            -> Q be db s (WithRewrittenThread (QNested s) s (WithRewrittenContext a QValueContext))
withWindow_ mkWindow mkProjection (Q windowOver)=
  Q (liftF (QWindowOver mkWindow mkProjection windowOver (rewriteThread (Proxy @s) . rewriteContext (Proxy @QValueContext))))

-- * Order bys

class SqlOrderable be a | a -> be where
    makeSQLOrdering :: Proxy be -> a -> [ WithExprContext (BeamSqlBackendOrderingSyntax be) ]
instance SqlOrderable be (QOrd be s a) where
    makeSQLOrdering _ (QOrd x) = [x]
instance SqlOrderable be a => SqlOrderable be [a] where
    makeSQLOrdering be = concatMap (makeSQLOrdering be)
instance ( SqlOrderable be a, SqlOrderable be b ) => SqlOrderable be (a, b) where
    makeSQLOrdering be (a, b) =
      makeSQLOrdering be a <> makeSQLOrdering be b
instance ( SqlOrderable be a, SqlOrderable be b
         , SqlOrderable be c ) => SqlOrderable be (a, b, c) where
    makeSQLOrdering be (a, b, c) =
      makeSQLOrdering be a <> makeSQLOrdering be b <> makeSQLOrdering be c
instance ( SqlOrderable be a, SqlOrderable be b
         , SqlOrderable be c, SqlOrderable be d ) => SqlOrderable be (a, b, c, d) where
    makeSQLOrdering be (a, b, c, d) =
      makeSQLOrdering be a <> makeSQLOrdering be b <> makeSQLOrdering be c <> makeSQLOrdering be d
instance ( SqlOrderable be a, SqlOrderable be b
         , SqlOrderable be c, SqlOrderable be d
         , SqlOrderable be e ) => SqlOrderable be (a, b, c, d, e) where
    makeSQLOrdering be (a, b, c, d, e) =
      makeSQLOrdering be a <> makeSQLOrdering be b <> makeSQLOrdering be c <> makeSQLOrdering be d <>
      makeSQLOrdering be e
instance ( SqlOrderable be a, SqlOrderable be b
         , SqlOrderable be c, SqlOrderable be d
         , SqlOrderable be e, SqlOrderable be f ) => SqlOrderable be (a, b, c, d, e, f) where
    makeSQLOrdering be (a, b, c, d, e, f) =
      makeSQLOrdering be a <> makeSQLOrdering be b <> makeSQLOrdering be c <> makeSQLOrdering be d <>
      makeSQLOrdering be e <> makeSQLOrdering be f
instance ( SqlOrderable be a, SqlOrderable be b
         , SqlOrderable be c, SqlOrderable be d
         , SqlOrderable be e, SqlOrderable be f
         , SqlOrderable be g ) => SqlOrderable be (a, b, c, d, e, f, g) where
    makeSQLOrdering be (a, b, c, d, e, f, g) =
      makeSQLOrdering be a <> makeSQLOrdering be b <> makeSQLOrdering be c <> makeSQLOrdering be d <>
      makeSQLOrdering be e <> makeSQLOrdering be f <> makeSQLOrdering be g
instance ( SqlOrderable be a, SqlOrderable be b
         , SqlOrderable be c, SqlOrderable be d
         , SqlOrderable be e, SqlOrderable be f
         , SqlOrderable be g, SqlOrderable be h ) => SqlOrderable be (a, b, c, d, e, f, g, h) where
    makeSQLOrdering be (a, b, c, d, e, f, g, h) =
      makeSQLOrdering be a <> makeSQLOrdering be b <> makeSQLOrdering be c <> makeSQLOrdering be d <>
      makeSQLOrdering be e <> makeSQLOrdering be f <> makeSQLOrdering be g <> makeSQLOrdering be h

-- | Order by the given expressions. The return type of the ordering key should
--   either be the result of 'asc_' or 'desc_' (or another ordering 'QOrd'
--   generated by a backend-specific ordering) or an (possibly nested) tuple of
--   results of the former.
--
--   The <https://tathougies.github.io/beam/user-guide/queries/ordering manual section>
--   has more information.
orderBy_ :: forall s a ordering be db
          . ( Projectible be a, SqlOrderable be ordering
            , ThreadRewritable (QNested s) a )
         => (a -> ordering) -> Q be db (QNested s) a -> Q be db s (WithRewrittenThread (QNested s) s a)
orderBy_ orderer (Q q) =
    Q (liftF (QOrderBy (sequenceA . makeSQLOrdering (Proxy @be) . orderer) q (rewriteThread (Proxy @s))))

nullsFirst_ :: IsSql2003OrderingElementaryOLAPOperationsSyntax (BeamSqlBackendOrderingSyntax be)
            => QOrd be s a -> QOrd be s a
nullsFirst_ (QOrd e) = QOrd (nullsFirstOrdering <$> e)

nullsLast_ :: IsSql2003OrderingElementaryOLAPOperationsSyntax (BeamSqlBackendOrderingSyntax be)
           => QOrd be s a -> QOrd be s a
nullsLast_ (QOrd e) = QOrd (nullsLastOrdering <$> e)

-- | Produce a 'QOrd' corresponding to a SQL @ASC@ ordering
asc_ :: forall be s a
      . BeamSqlBackend be
     => QExpr be s a -> QOrd be s a
asc_ (QExpr e) = QOrd (ascOrdering <$> e)

-- | Produce a 'QOrd' corresponding to a SQL @DESC@ ordering
desc_ :: forall be s a
       . BeamSqlBackend be
      => QExpr be s a -> QOrd be s a
desc_ (QExpr e) = QOrd (descOrdering <$> e)

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

instance BeamSqlBackend be =>
    SqlJustable (QExpr be s a) (QExpr be s (Maybe a)) where

    just_ (QExpr e) = QExpr e
    nothing_ = QExpr (pure (valueE (sqlValueSyntax SqlNull)))

instance {-# OVERLAPPING #-} ( Table t, BeamSqlBackend be ) =>
    SqlJustable (PrimaryKey t (QExpr be s)) (PrimaryKey t (Nullable (QExpr be s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' nothing_) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} ( Table t, BeamSqlBackend be ) =>
    SqlJustable (t (QExpr be s)) (t (Nullable (QExpr be s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' nothing_) (tblSkeleton :: TableSkeleton t)

instance {-# OVERLAPPING #-} Table t => SqlJustable (PrimaryKey t Identity) (PrimaryKey t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' Nothing) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} Table t => SqlJustable (t Identity) (t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' Nothing) (tblSkeleton :: TableSkeleton t)

-- * Nullable checking

data QIfCond context be s a = QIfCond (QGenExpr context be s SqlBool) (QGenExpr context be s a)
newtype QIfElse context be s a = QIfElse (QGenExpr context be s a)

then_ :: QGenExpr context be s Bool -> QGenExpr context be s a -> QIfCond context be s a
then_ cond res = QIfCond (sqlBool_ cond) res

then_' :: QGenExpr context be s SqlBool -> QGenExpr context be s a -> QIfCond context be s a
then_' cond res = QIfCond cond res

else_ :: QGenExpr context be s a -> QIfElse context be s a
else_ = QIfElse

if_ :: BeamSqlBackend be
    => [ QIfCond context be s a ]
    -> QIfElse context be s a
    -> QGenExpr context be s a
if_ conds (QIfElse (QExpr elseExpr)) =
  QExpr (\tbl -> caseE (map (\(QIfCond (QExpr cond) (QExpr res)) -> (cond tbl, res tbl)) conds) (elseExpr tbl))

-- | SQL @COALESCE@ support
coalesce_ :: BeamSqlBackend be
          => [ QGenExpr ctxt be s (Maybe a) ] -> QGenExpr ctxt be s a -> QGenExpr ctxt be s a
coalesce_ qs (QExpr onNull) =
  QExpr $ do
    onNull' <- onNull
    coalesceE . (<> [onNull']) <$> mapM (\(QExpr q) -> q) qs

-- | Converta a 'Maybe' value to a concrete value, by suppling a default
fromMaybe_ :: BeamSqlBackend be
           => QGenExpr ctxt be s a -> QGenExpr ctxt be s (Maybe a) -> QGenExpr ctxt be s a
fromMaybe_ onNull q = coalesce_ [q] onNull

-- | Type class for anything which can be checked for null-ness. This includes 'QExpr (Maybe a)' as
-- well as 'Table's or 'PrimaryKey's over 'Nullable QExpr'.
class BeamSqlBackend be => SqlDeconstructMaybe be a nonNullA s | a s -> be, a -> nonNullA, a -> s, nonNullA -> s where
    -- | Returns a 'QExpr' that evaluates to true when the first argument is not null
    isJust_ :: a -> QGenExpr ctxt be s Bool

    -- | Returns a 'QExpr' that evaluates to true when the first argument is null
    isNothing_ :: a -> QGenExpr ctxt be s Bool

    -- | Given an object (third argument) which may or may not be null, return the default value if
    -- null (first argument), or transform the value that could be null to yield the result of the
    -- expression (second argument)
    maybe_ :: QGenExpr ctxt be s y -> (nonNullA -> QGenExpr ctxt be s y) -> a -> QGenExpr ctxt be s y

instance BeamSqlBackend be => SqlDeconstructMaybe be (QGenExpr ctxt be s (Maybe x)) (QGenExpr ctxt be s x) s where
    isJust_ (QExpr x) = QExpr (isNotNullE <$> x)
    isNothing_ (QExpr x) = QExpr (isNullE <$> x)

    maybe_ (QExpr onNothing) onJust (QExpr e) =
        let QExpr onJust' = onJust (QExpr e)
        in QExpr (\tbl -> caseE [(isNotNullE (e tbl), onJust' tbl)] (onNothing tbl))

instance ( BeamSqlBackend be, Beamable t)
    => SqlDeconstructMaybe be (t (Nullable (QGenExpr ctxt be s))) (t (QGenExpr ctxt be s)) s where
    isJust_ t = allE (allBeamValues (\(Columnar' e) -> isJust_ e) t)
    isNothing_ t = allE (allBeamValues (\(Columnar' e) -> isNothing_ e) t)
    maybe_ (QExpr onNothing) onJust tbl =
      let QExpr onJust' = onJust (changeBeamRep (\(Columnar' (QExpr e)) -> Columnar' (QExpr e)) tbl)
          QExpr cond = isJust_ tbl
      in QExpr (\tblPfx -> caseE [(cond tblPfx, onJust' tblPfx)] (onNothing tblPfx))

infixl 3 <|>.
(<|>.) :: ( SqlJustable a (QGenExpr ctxt syntax s y)
          , SqlDeconstructMaybe syntax (QGenExpr ctxt syntax s y) a s
          )
       => QGenExpr ctxt syntax s y
       -> QGenExpr ctxt syntax s y
       -> QGenExpr ctxt syntax s y
l <|>. r = maybe_ r just_ l
