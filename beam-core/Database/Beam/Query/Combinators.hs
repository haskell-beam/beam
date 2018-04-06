{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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

    , all_
    , allFromView_, join_, join_'
    , guard_, guard_', filter_, filter_'
    , related_, relatedBy_, relatedBy_'
    , leftJoin_, leftJoin_'
    , perhaps_, outerJoin_, outerJoin_'
    , subselect_, references_

    , nub_

    , SqlJustable(..)
    , SqlDeconstructMaybe(..)
    , SqlOrderable(..)
    , QIfCond, QIfElse

    , limit_, offset_

    , as_

    -- ** Subqueries
    , exists_, unique_, subquery_

    -- ** Set operations
    -- |  'Q' values can be combined using a variety of set operations. See the
    --    <https://tathougies.github.io/beam/user-guide/queries/combining-queries manual section>.
    , union_, unionAll_
    , intersect_, intersectAll_
    , except_, exceptAll_

    -- * Ordering primitives
    , orderBy_, asc_, desc_
    ) where

import Database.Beam.Syntax
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
all_ :: DatabaseEntity db (TableEntity table)
     -> Q db s (table (QExpr s))
all_ (DatabaseEntity (DatabaseTable tblNm tblSettings)) =
    Q $ liftF (QAll tblNm tblSettings (\_ -> Nothing) snd)

-- | Introduce all entries of a view into the 'Q' monad
allFromView_ :: Beamable table
             => DatabaseEntity db (ViewEntity table)
             -> Q db s (table (QExpr s))
allFromView_ (DatabaseEntity (DatabaseView tblNm tblSettings)) =
    Q $ liftF (QAll tblNm tblSettings (\_ -> Nothing) snd)

-- | Introduce all entries of a table into the 'Q' monad based on the
--   given QExpr. The join condition is expected to return a
--   'Bool'. For a version that takes 'SqlBool' (a possibly @UNKNOWN@
--   boolean, that maps more closely to the SQL standard), see
--   'join_''.
join_ :: DatabaseEntity db (TableEntity table)
      -> (table (QExpr s) -> QExpr s Bool)
      -> Q db s (table (QExpr s))
join_ tbl mkOn = join_' tbl (sqlBool_ . mkOn)

-- | Like 'join_', but accepting an @ON@ condition that returns
-- 'SqlBool'
join_' :: DatabaseEntity db (TableEntity table)
       -> (table (QExpr s) -> QExpr s SqlBool)
       -> Q db s (table (QExpr s))
join_' (DatabaseEntity (DatabaseTable tblNm tblSettings)) mkOn =
    Q $ liftF (QAll tblNm tblSettings (\tbl -> let QExpr on = mkOn tbl in Just on) snd)

-- | Introduce a table using a left join with no ON clause. Because this is not
--   an inner join, the resulting table is made nullable. This means that each
--   field that would normally have type 'QExpr x' will now have type 'QExpr
--   (Maybe x)'.
perhaps_ :: forall db s r. ( ThreadRewritable (QNested s) r
            , Retaggable (QExpr s) (WithRewrittenThread (QNested s) s r)
            , Projectible ExpressionSyntax r
            )
         => Q db (QNested s) r
         -> Q db s (Retag Nullable (WithRewrittenThread (QNested s) s r))
perhaps_ (Q sub) =
  Q $ liftF (QArbitraryJoin
              sub leftJoin
              (\_ -> Nothing)
              (\r -> retag (\(Columnar' (QExpr e) :: Columnar' (QExpr s) a) ->
                                            Columnar' (QExpr e) :: Columnar' (Nullable (QExpr s)) a) $
                                  rewriteThread (Proxy @s) r))

-- | Outer join. every row of each table, returning @NULL@ for any row
-- of either table for which the join condition finds no related rows.
--
-- This expects a join expression returning 'Bool', for a version that
-- accepts a 'SqlBool' (a possibly @UNKNOWN@ boolean, that maps more
-- closely to the SQL standard), see 'outerJoin_''
outerJoin_ :: ( ThreadRewritable (QNested s) a, ThreadRewritable (QNested s) b
              , Retaggable (QExpr s) (WithRewrittenThread (QNested s) s a)
              , Retaggable (QExpr s) (WithRewrittenThread (QNested s) s b)
              , Projectible ExpressionSyntax a
              , Projectible ExpressionSyntax b
              )
           => Q db (QNested s) a
           -> Q db (QNested s) b
           -> ( (WithRewrittenThread (QNested s) s a, WithRewrittenThread (QNested s) s b) -> QExpr s Bool )
           -> Q db s ( Retag Nullable (WithRewrittenThread (QNested s) s a)
                     , Retag Nullable (WithRewrittenThread (QNested s) s b) )
outerJoin_ a b on_ = outerJoin_' a b (sqlBool_ . on_)

-- | Like 'outerJoin_', but accepting 'SqlBool'. Pairs of rows for
-- which the join condition is unknown are considered to be unrelated,
-- by SQL compliant databases at least.
outerJoin_' :: forall db s a b. ( ThreadRewritable (QNested s) a, ThreadRewritable (QNested s) b
               , Retaggable (QExpr s) (WithRewrittenThread (QNested s) s a)
               , Retaggable (QExpr s) (WithRewrittenThread (QNested s) s b)
               , Projectible ExpressionSyntax a
               , Projectible ExpressionSyntax b
               )
            => Q db (QNested s) a
            -> Q db (QNested s) b
            -> ( (WithRewrittenThread (QNested s) s a, WithRewrittenThread (QNested s) s b) -> QExpr s SqlBool )
            -> Q db s ( Retag Nullable (WithRewrittenThread (QNested s) s a)
                      , Retag Nullable (WithRewrittenThread (QNested s) s b) )
outerJoin_' (Q a) (Q b) on_ =
  Q $ liftF (QTwoWayJoin a b outerJoin
              (\(a', b') ->
                 let QExpr e = on_ (rewriteThread (Proxy @s) a', rewriteThread (Proxy @s) b')
                 in Just e)
              (\(a', b') ->
                 let retag' :: (ThreadRewritable (QNested s) x, Retaggable (QExpr s) (WithRewrittenThread (QNested s) s x))
                            => x -> Retag Nullable (WithRewrittenThread (QNested s) s x)
                     retag' = retag (\(Columnar' (QExpr e) :: Columnar' (QExpr s) x) ->
                                        Columnar' (QExpr e) :: Columnar' (Nullable (QExpr s)) x) .
                              rewriteThread (Proxy @s)
                 in ( retag' a', retag' b' )))

-- | Introduce a table using a left join. The ON clause is required here.Because
--   this is not an inner join, the resulting table is made nullable. This means
--   that each field that would normally have type 'QExpr x' will now have type
--   'QExpr (Maybe x)'.
--
--   The @ON@ condition given must return 'Bool'. For a version that
--   accepts an @ON@ condition returning 'SqlBool', see 'leftJoin_''.
leftJoin_ :: ( ThreadRewritable (QNested s) r
           , Retaggable (QExpr s) (WithRewrittenThread (QNested s) s r)
           , Projectible ExpressionSyntax r
           )
          => Q db (QNested s) r
          -> (WithRewrittenThread (QNested s) s r -> QExpr s Bool)
          -> Q db s (Retag Nullable (WithRewrittenThread (QNested s) s r))
leftJoin_ sub on_ = leftJoin_' sub (sqlBool_ . on_)

-- | Like 'leftJoin_', but accepts an @ON@ clause returning 'SqlBool'.
leftJoin_' :: forall db s r. ( ThreadRewritable (QNested s) r
            , Retaggable (QExpr s) (WithRewrittenThread (QNested s) s r)
            , Projectible ExpressionSyntax r
            )
           => Q db (QNested s) r
           -> (WithRewrittenThread (QNested s) s r -> QExpr s SqlBool)
           -> Q db s (Retag Nullable (WithRewrittenThread (QNested s) s r))
leftJoin_' (Q sub) on_ =
  Q $ liftF (QArbitraryJoin
               sub leftJoin
               (\r -> let QExpr e = on_ (rewriteThread (Proxy @s) r) in Just e)
               (\r -> retag (\(Columnar' (QExpr e) :: Columnar' (QExpr s) a) ->
                                Columnar' (QExpr e) :: Columnar' (Nullable (QExpr s)) a) $
                      rewriteThread (Proxy @s) r))

subselect_ :: forall db s r. ( ThreadRewritable (QNested s) r, Projectible ExpressionSyntax r )
           => Q db (QNested s) r
           -> Q db s (WithRewrittenThread (QNested s) s r)
subselect_ (Q q') =
  Q (liftF (QSubSelect q' (rewriteThread (Proxy @s))))

-- | Only allow results for which the 'QExpr' yields 'True'. For a
-- version that operates over possibly @NULL@ 'SqlBool's, see
-- 'guard_''.
guard_ :: QExpr s Bool -> Q db s ()
guard_ = guard_' . sqlBool_

-- | Only allow results for which the 'QExpr' yields @TRUE@.
--
-- This function operates over 'SqlBool', which are like haskell
-- 'Bool's, except for the special @UNKNOWN@ value that occurs when
-- comparisons include @NULL@. For a version that operates over known
-- non-@NULL@ booleans, see 'guard_'.
guard_' :: QExpr s SqlBool -> Q db s ()
guard_' (QExpr guardE') = Q (liftF (QGuard guardE' ()))

-- | Synonym for @clause >>= \x -> guard_ (mkExpr x)>> pure x@. Use 'filter_'' for comparisons with 'SqlBool'
filter_ :: (r -> QExpr s Bool)
        -> Q db s r
        -> Q db s r
filter_ mkExpr clause = clause >>= \x -> guard_ (mkExpr x) >> pure x

-- | Synonym for @clause >>= \x -> guard_' (mkExpr x)>> pure x@. Use 'filter_' for comparisons with 'Bool'
filter_' :: (r -> QExpr s SqlBool)
         -> Q db s r -> Q db s r
filter_' mkExpr clause = clause >>= \x -> guard_' (mkExpr x) >> pure x

-- | Introduce all entries of the given table which are referenced by the given 'PrimaryKey'
related_ :: ( HasTableEquality (PrimaryKey rel)
            , Database db
            , Table rel
            ) =>
            DatabaseEntity db (TableEntity rel)
         -> PrimaryKey rel (QExpr s)
         -> Q db s (rel (QExpr s))
related_ relTbl relKey =
  join_ relTbl (\rel -> relKey ==. primaryKey rel)

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_ :: ( Database db, Table rel )
           => DatabaseEntity db (TableEntity rel)
           -> (rel (QExpr s) -> QExpr s Bool)
           -> Q db s (rel (QExpr s))
relatedBy_ = join_

-- | Introduce all entries of the given table which for which the expression (which can depend on the queried table returns true)
relatedBy_' :: ( Database db, Table rel )
            => DatabaseEntity db (TableEntity rel)
            -> (rel (QExpr s) -> QExpr s SqlBool)
            -> Q db s (rel (QExpr s))
relatedBy_' = join_'

-- | Generate an appropriate boolean 'QGenExpr' comparing the given foreign key
--   to the given table. Useful for creating join conditions.
references_ :: ( HasTableEquality (PrimaryKey t), Table t, ExpressionContext ctxt )
            => PrimaryKey t (QGenExpr ctxt s) -> t (QGenExpr ctxt s) -> QGenExpr ctxt s Bool
references_ fk tbl = fk ==. pk tbl

-- | Only return distinct values from a query
nub_ :: Projectible ExpressionSyntax r => Q db s r -> Q db s r
nub_ (Q sub) = Q $ liftF (QDistinct (\_ _ -> setQuantifierDistinct) sub id)

-- | Limit the number of results returned by a query.
limit_ :: forall db s a. ( ThreadRewritable (QNested s) a, Projectible ExpressionSyntax a ) =>
          Integer -> Q db (QNested s) a -> Q db s (WithRewrittenThread (QNested s) s a)
limit_ limit' (Q q) =
  Q (liftF (QLimit limit' q (rewriteThread (Proxy @s))))

-- | Drop the first `offset'` results.
offset_ :: forall db s a. ( ThreadRewritable (QNested s) a, Projectible ExpressionSyntax a ) =>
           Integer -> Q db (QNested s) a -> Q db s (WithRewrittenThread (QNested s) s a)
offset_ offset' (Q q) =
  Q (liftF (QOffset offset' q (rewriteThread (Proxy @s))))

-- | Use the SQL @EXISTS@ operator to determine if the given query returns any results
exists_ :: Projectible ExpressionSyntax a => Q db s a -> QExpr s Bool
exists_ q = QExpr (\tbl -> existsE (buildSqlQuery tbl q))

-- | Use the SQL @UNIQUE@ operator to determine if the given query produces a unique result
unique_ :: Projectible ExpressionSyntax a => Q db s a -> QExpr s Bool
unique_ q = QExpr (\tbl -> uniqueE (buildSqlQuery tbl q))

-- | Project the (presumably) singular result of the given query as an expression
subquery_ :: Projectible ExpressionSyntax a => Q db s (QExpr s a) -> QExpr s a
subquery_ q =
  QExpr (\tbl -> subqueryE (buildSqlQuery tbl q))

-- | SQL @CHAR_LENGTH@ function
charLength_ :: (IsStringType text, ExpressionContext context)
            => QGenExpr context s text -> QGenExpr context s Int
charLength_ (QExpr s) = QExpr (charLengthE <$> s)

-- | SQL @OCTET_LENGTH@ function
octetLength_ :: (IsStringType text, ExpressionContext context) => QGenExpr context s text -> QGenExpr context s Int
octetLength_ (QExpr s) = QExpr (octetLengthE <$> s)

-- | SQL @BIT_LENGTH@ function
bitLength_ :: ExpressionContext context => QGenExpr context s SqlBitString -> QGenExpr context s Int
bitLength_ (QExpr x) = QExpr (bitLengthE <$> x)

-- | SQL @CURRENT_TIMESTAMP@ function
currentTimestamp_ :: ExpressionContext ctxt => QGenExpr ctxt s LocalTime
currentTimestamp_ = QExpr (pure currentTimestampE)

-- | SQL @POSITION(.. IN ..)@ function
position_ ::
  ( IsStringType text , Integral b ) => QExpr s text -> QExpr s text -> QExpr s b
position_ (QExpr needle) (QExpr haystack) =
  QExpr (liftA2 likeE needle haystack)

-- | SQL @LOWER@ function
lower_ :: ( IsStringType text, ExpressionContext context )
       => QGenExpr context s text -> QGenExpr context s text
lower_ (QExpr s) = QExpr (lowerE <$> s)

-- | SQL @UPPER@ function
upper_ :: ( IsStringType text, ExpressionContext context )
       => QGenExpr context s text -> QGenExpr context s text
upper_ (QExpr s) = QExpr (upperE <$> s)

-- | SQL @TRIM@ function
trim_ :: ( IsStringType text, ExpressionContext context )
      => QGenExpr context s text -> QGenExpr context s text
trim_ (QExpr s) = QExpr (trimE <$> s)

-- | Combine all the given boolean value 'QGenExpr's with the '&&.' operator.
allE :: ExpressionContext context => [ QGenExpr context s Bool ] -> QGenExpr context s Bool
allE es = fromMaybe (QExpr (pure (valueE (sqlValueSyntax True)))) $
          foldl (\expr x ->
                   Just $ maybe x (\e -> e &&. x) expr)
                Nothing es

-- * UPDATE operators

-- | Extract an expression representing the current (non-UPDATEd) value of a 'QField'
current_ :: QField s ty -> QExpr s ty
current_ (QField False _ nm) = QExpr (pure (fieldE (unqualifiedField nm)))
current_ (QField True tbl nm) = QExpr (pure (fieldE (qualifiedField tbl nm)))

infix 4 <-.
class SqlUpdatable s lhs rhs | lhs -> s, rhs -> s, lhs s -> rhs, rhs -> lhs where
  -- | Update a 'QField' or 'Beamable' type containing 'QField's with the given
  --   'QExpr' or 'Beamable' type containing 'QExpr'
  (<-.) :: lhs
        -> rhs
        -> QAssignment s
instance SqlUpdatable s (QField s a) (QExpr s a) where
  QField _ _ nm <-. QExpr expr =
    QAssignment [(unqualifiedField nm, expr "t")]
instance Beamable tbl => SqlUpdatable s (tbl (QField s)) (tbl (QExpr s)) where
  (<-.) :: tbl (QField s) -> tbl (QExpr s)
        -> QAssignment s
  lhs <-. rhs =
    QAssignment $
    allBeamValues (\(Columnar' (Const assignments)) -> assignments) $
    runIdentity $
    zipBeamFieldsM (\(Columnar' (QField _ _ f) :: Columnar' (QField s) t) (Columnar' (QExpr e)) ->
                       pure (Columnar' (Const (unqualifiedField f, e "t")) :: Columnar' (Const (FieldNameSyntax,ExpressionSyntax)) t)) lhs rhs
instance Beamable tbl => SqlUpdatable s (tbl (Nullable (QField s))) (tbl (Nullable (QExpr s))) where
  lhs <-. rhs =
    let lhs' = changeBeamRep (\(Columnar' (QField q tblName fieldName') :: Columnar' (Nullable (QField s)) a) ->
                                Columnar' (QField q tblName fieldName') :: Columnar' (QField s)  a) lhs
        rhs' = changeBeamRep (\(Columnar' (QExpr e) :: Columnar' (Nullable (QExpr s)) a) ->
                                Columnar' (QExpr e) :: Columnar' (QExpr s) a) rhs
    in lhs' <-. rhs'

-- | SQL @UNION@ operator
union_ :: forall db s a.
          ( Projectible ExpressionSyntax a
          , ThreadRewritable (QNested s) a)
       => Q db (QNested s) a -> Q db (QNested s) a
       -> Q db s (WithRewrittenThread (QNested s) s a)
union_ (Q a) (Q b) = Q (liftF (QUnion False a b (rewriteThread (Proxy @s))))

-- | SQL @UNION ALL@ operator
unionAll_ :: forall db s a.
             ( Projectible ExpressionSyntax a
             , ThreadRewritable (QNested s) a)
          => Q db (QNested s) a -> Q db (QNested s) a
          -> Q db s (WithRewrittenThread (QNested s) s a)
unionAll_ (Q a) (Q b) = Q (liftF (QUnion True a b (rewriteThread (Proxy @s))))

-- | SQL @INTERSECT@ operator
intersect_ :: forall db s a.
              ( Projectible ExpressionSyntax a
              , ThreadRewritable (QNested s) a)
           => Q db (QNested s) a -> Q db (QNested s) a
           -> Q db s (WithRewrittenThread (QNested s) s a)
intersect_ (Q a) (Q b) = Q (liftF (QIntersect False a b (rewriteThread (Proxy @s))))

-- | SQL @INTERSECT ALL@ operator
intersectAll_ :: forall db s a.
                 ( Projectible ExpressionSyntax a
                 , ThreadRewritable (QNested s) a)
              => Q db (QNested s) a -> Q db (QNested s) a
              -> Q db s (WithRewrittenThread (QNested s) s a)
intersectAll_ (Q a) (Q b) = Q (liftF (QIntersect True a b (rewriteThread (Proxy @s))))

-- | SQL @EXCEPT@ operator
except_ :: forall db s a.
           ( Projectible ExpressionSyntax a
           , ThreadRewritable (QNested s) a)
        => Q db (QNested s) a -> Q db (QNested s) a
        -> Q db s (WithRewrittenThread (QNested s) s a)
except_ (Q a) (Q b) = Q (liftF (QExcept False a b (rewriteThread (Proxy @s))))

-- | SQL @EXCEPT ALL@ operator
exceptAll_ :: forall db s a.
              ( Projectible ExpressionSyntax a
              , ThreadRewritable (QNested s) a)
           => Q db (QNested s) a -> Q db (QNested s) a
           -> Q db s (WithRewrittenThread (QNested s) s a)
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
as_ :: QGenExpr ctxt s a -> QGenExpr ctxt s a
as_ = id

-- * Marshalling between Haskell literals and QExprs

type family HaskellLiteralForQExpr x = a
type instance HaskellLiteralForQExpr (QGenExpr context s a) = a
type instance HaskellLiteralForQExpr (table (QGenExpr context s)) = table Identity
type instance HaskellLiteralForQExpr (table (Nullable f)) = HaskellLiteralForQExpr_AddNullable (HaskellLiteralForQExpr (table f))

type family HaskellLiteralForQExpr_AddNullable x = a
type instance HaskellLiteralForQExpr_AddNullable (tbl f) = tbl (Nullable f)

type family QExprSyntax x where
  QExprSyntax (QGenExpr ctxt s a) = ContextSyntax ctxt

type SqlValableTable table =
   ( Beamable table
   , FieldsFulfillConstraint HasSqlValueSyntax table )

class SqlValable a where
    val_ :: HaskellLiteralForQExpr a -> a

instance (HasSqlValueSyntax a, ExpressionContext ctxt) =>
  SqlValable (QGenExpr ctxt s a) where

  val_ = QExpr . pure . valueE . sqlValueSyntax
instance ( Beamable table
         , FieldsFulfillConstraint HasSqlValueSyntax table
         , ExpressionContext ctxt ) =>
  SqlValable (table (QGenExpr ctxt s)) where
  val_ tbl =
    let fields :: table (WithConstraint HasSqlValueSyntax)
        fields = to (gWithConstrainedFields (Proxy @HasSqlValueSyntax)
                                            (Proxy @(Rep (table Exposed))) (from tbl))
    in changeBeamRep (\(Columnar' (WithConstraint x :: WithConstraint HasSqlValueSyntax x)) ->
                         Columnar' (QExpr (pure (valueE (sqlValueSyntax x))))) fields
instance ( Beamable table
         , FieldsFulfillConstraintNullable HasSqlValueSyntax table
         , ExpressionContext ctxt) =>

         SqlValable (table (Nullable (QGenExpr ctxt s))) where

  val_ tbl =
    let fields :: table (Nullable (WithConstraint HasSqlValueSyntax))
        fields = to (gWithConstrainedFields (Proxy @HasSqlValueSyntax)
                                            (Proxy @(Rep (table (Nullable Exposed)))) (from tbl))
    in changeBeamRep (\(Columnar' (WithConstraint x :: WithConstraint HasSqlValueSyntax (Maybe x))) ->
                         Columnar' (QExpr (pure (valueE (sqlValueSyntax x))))) fields

default_ :: ExpressionContext ctxt => QGenExpr ctxt s a
default_ = QExpr (pure defaultE)

-- * Order bys

class SqlOrderable a where
    makeSQLOrdering :: a -> [ WithExprContext ExpressionSyntax ]
instance SqlOrderable (QOrd s a) where
    makeSQLOrdering (QExpr x) = [x]
instance SqlOrderable a => SqlOrderable [a] where
    makeSQLOrdering = concatMap makeSQLOrdering
instance ( SqlOrderable a
         , SqlOrderable b ) => SqlOrderable (a, b) where
    makeSQLOrdering (a, b) = makeSQLOrdering a <> makeSQLOrdering b
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c ) => SqlOrderable (a, b, c) where
    makeSQLOrdering (a, b, c) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c
         , SqlOrderable d ) => SqlOrderable (a, b, c, d) where
    makeSQLOrdering (a, b, c, d) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c
         , SqlOrderable d
         , SqlOrderable e ) => SqlOrderable (a, b, c, d, e) where
    makeSQLOrdering (a, b, c, d, e) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d <> makeSQLOrdering e
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c
         , SqlOrderable d
         , SqlOrderable e
         , SqlOrderable f ) => SqlOrderable (a, b, c, d, e, f) where
    makeSQLOrdering (a, b, c, d, e, f) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d <> makeSQLOrdering e <> makeSQLOrdering f
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c
         , SqlOrderable d
         , SqlOrderable e
         , SqlOrderable f
         , SqlOrderable g ) => SqlOrderable (a, b, c, d, e, f, g) where
    makeSQLOrdering (a, b, c, d, e, f, g) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d <>
                                            makeSQLOrdering e <> makeSQLOrdering f <> makeSQLOrdering g
instance ( SqlOrderable a
         , SqlOrderable b
         , SqlOrderable c
         , SqlOrderable d
         , SqlOrderable e
         , SqlOrderable f
         , SqlOrderable g
         , SqlOrderable h) => SqlOrderable (a, b, c, d, e, f, g, h) where
    makeSQLOrdering (a, b, c, d, e, f, g, h) = makeSQLOrdering a <> makeSQLOrdering b <> makeSQLOrdering c <> makeSQLOrdering d <>
                                            makeSQLOrdering e <> makeSQLOrdering f <> makeSQLOrdering g <> makeSQLOrdering h

-- | Order by the given expressions. The return type of the ordering key should
--   either be the result of 'asc_' or 'desc_' (or another ordering 'QOrd'
--   generated by a backend-specific ordering) or an (possibly nested) tuple of
--   results of the former.
--
--   The <https://tathougies.github.io/beam/user-guide/queries/ordering manual section>
--   has more information.
orderBy_ :: forall s a ordering db.
            ( Projectible ExpressionSyntax a
            , SqlOrderable ordering
            , ThreadRewritable (QNested s) a) =>
            (a -> ordering) -> Q db (QNested s) a -> Q db s (WithRewrittenThread (QNested s) s a)
orderBy_ orderer (Q q) =
    Q (liftF (QOrderBy (sequenceA . makeSQLOrdering . orderer) q (rewriteThread (Proxy @s))))

-- | Produce a 'QOrd' corresponding to a SQL @ASC@ ordering
asc_ :: QExpr s a -> QOrd s a
asc_ (QExpr e) = QExpr (ascOrdering <$> e)

-- | Produce a 'QOrd' corresponding to a SQL @DESC@ ordering
desc_ :: QExpr s a -> QOrd s a
desc_ (QExpr e) = QExpr (descOrdering <$> e)

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

instance HasSqlValueSyntax SqlNull

instance SqlJustable (QExpr s a) (QExpr s (Maybe a)) where
    just_ (QExpr e) = QExpr e
    nothing_ = QExpr (pure (valueE (sqlValueSyntax SqlNull)))

instance {-# OVERLAPPING #-} Table t =>
    SqlJustable (PrimaryKey t (QExpr s)) (PrimaryKey t (Nullable (QExpr s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' nothing_) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} Table t =>
    SqlJustable (t (QExpr s)) (t (Nullable (QExpr s))) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (just_ q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' nothing_) (tblSkeleton :: TableSkeleton t)

instance {-# OVERLAPPING #-} Table t => SqlJustable (PrimaryKey t Identity) (PrimaryKey t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' Nothing) (primaryKey (tblSkeleton :: TableSkeleton t))

instance {-# OVERLAPPING #-} Table t => SqlJustable (t Identity) (t (Nullable Identity)) where
    just_ = changeBeamRep (\(Columnar' q) -> Columnar' (Just q))
    nothing_ = changeBeamRep (\(Columnar' _) -> Columnar' Nothing) (tblSkeleton :: TableSkeleton t)

-- * Nullable checking

data QIfCond context s a = QIfCond (QGenExpr context s SqlBool) (QGenExpr context s a)
newtype QIfElse context s a = QIfElse (QGenExpr context s a)

then_ :: QGenExpr context s Bool -> QGenExpr context s a -> QIfCond context s a
then_ cond res = QIfCond (sqlBool_ cond) res

then_' :: QGenExpr context s SqlBool -> QGenExpr context s a -> QIfCond context s a
then_' cond res = QIfCond cond res

else_ :: QGenExpr context s a -> QIfElse context s a
else_ = QIfElse

if_ :: ExpressionContext context => [ QIfCond context s a ]
    -> QIfElse context s a
    -> QGenExpr context s a
if_ conds (QIfElse (QExpr elseExpr)) =
  QExpr (\tbl -> caseE (map (\(QIfCond (QExpr cond) (QExpr res)) -> (cond tbl, res tbl)) conds) (elseExpr tbl))

-- | SQL @COALESCE@ support
coalesce_ :: ExpressionContext ctxt => [ QGenExpr ctxt s (Maybe a) ] -> QGenExpr ctxt s a -> QGenExpr ctxt s a
coalesce_ qs (QExpr onNull) =
  QExpr $ do
    onNull' <- onNull
    coalesceE . (<> [onNull']) <$> mapM (\(QExpr q) -> q) qs

-- | Convert a 'Maybe' value to a concrete value, by suppling a default
fromMaybe_ :: ExpressionContext ctxt => QGenExpr ctxt s a -> QGenExpr ctxt s (Maybe a) -> QGenExpr ctxt s a
fromMaybe_ onNull q = coalesce_ [q] onNull

-- | Type class for anything which can be checked for null-ness. This includes 'QExpr (Maybe a)' as
-- well as 'Table's or 'PrimaryKey's over 'Nullable QExpr'.
class SqlDeconstructMaybe a nonNullA s | a -> nonNullA, a -> s, nonNullA -> s where
    -- | Returns a 'QExpr' that evaluates to true when the first argument is not null
    isJust_ :: ExpressionContext ctxt => a -> QGenExpr ctxt s Bool

    -- | Returns a 'QExpr' that evaluates to true when the first argument is null
    isNothing_ :: ExpressionContext ctxt => a -> QGenExpr ctxt s Bool

    -- | Given an object (third argument) which may or may not be null, return the default value if
    -- null (first argument), or transform the value that could be null to yield the result of the
    -- expression (second argument)
    maybe_ :: ExpressionContext ctxt => QGenExpr ctxt s y -> (nonNullA -> QGenExpr ctxt s y) -> a -> QGenExpr ctxt s y

instance ExpressionContext ctxt => SqlDeconstructMaybe (QGenExpr ctxt s (Maybe x)) (QGenExpr ctxt s x) s where
    isJust_ (QExpr x) = QExpr (isNotNullE <$> x)
    isNothing_ (QExpr x) = QExpr (isNullE <$> x)

    maybe_ (QExpr onNothing) onJust (QExpr e) =
        let QExpr onJust' = onJust (QExpr e)
        in QExpr (\tbl -> caseE [(isNotNullE (e tbl), onJust' tbl)] (onNothing tbl))

instance ( Beamable t, ExpressionContext ctxt )
    => SqlDeconstructMaybe (t (Nullable (QGenExpr ctxt s))) (t (QGenExpr ctxt s)) s where
    isJust_ t = allE (allBeamValues (\(Columnar' e) -> isJust_ e) t)
    isNothing_ t = allE (allBeamValues (\(Columnar' e) -> isNothing_ e) t)

    maybe_ (QExpr onNothing) onJust tbl =
      let onJust' :: TablePrefix -> ContextSyntax ctxt
          QExpr onJust' = onJust (changeBeamRep (\(Columnar' (QExpr e)) -> Columnar' (QExpr e)) tbl)

          cond :: TablePrefix -> ContextSyntax ctxt
          QExpr cond = isJust_ tbl :: QGenExpr ctxt s Bool
      in QExpr (\tblPfx -> caseE [(cond tblPfx, onJust' tblPfx)] (onNothing tblPfx))

