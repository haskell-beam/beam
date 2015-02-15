{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeOperators, FlexibleContexts, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Combinators where

import Database.Beam.Schema
import Database.Beam.Query.Types
import Database.Beam.Query.Rewrite

import Database.Beam.SQL
import Database.HDBC

import Control.Monad.Writer hiding (All)

import Data.Proxy
import Data.Semigroup hiding (All)
import Data.Typeable
import Data.Convertible
import Data.Coerce
import qualified Data.Text as T

import GHC.Generics

-- * Query combinators

of_ :: Table table => table Column
of_ = undefined

all_ :: (Table table, ScopeFields (Entity table Column)) => table Column -> Query (Entity table Column)
all_ (_ :: table Column) = All (Proxy :: Proxy table) 0

maxTableOrdinal :: Query a -> Int
maxTableOrdinal q = getMax (execWriter (traverseQueryM maxQ maxE q))
    where maxQ :: Query a -> Writer (Max Int) ()
          maxQ (All _ i) = tell (Max i)
          maxQ _ = return ()

          maxE :: QExpr a -> Writer (Max Int) ()
          maxE _ = return ()

join_ :: (Project (Scope a), Project (Scope b)) => Query a -> Query b -> Query (a :|: b)
join_ l r = Join l (rewriteForJoin l r)

rewriteForJoin :: Query a -> Query b -> Query b
rewriteForJoin l r = r'
    where maxOrdL = max (maxTableOrdinal l) (maxTableOrdinal r) + 1

          remapQuery :: Query b -> Maybe (Query b)
          remapQuery (All tbl i)
              | i < maxOrdL = Just (All tbl (i + maxOrdL))
              | otherwise = Nothing
          remapQuery _ = Nothing

          remapExpr :: QExpr b -> Maybe (QExpr b)
          remapExpr (FieldE (ScopedField i tbl :: ScopedField table c ty))
              | i < maxOrdL = Just (FieldE (ScopedField (i + maxOrdL) tbl :: ScopedField table c ty))
              | otherwise = Nothing
          remapExpr _ = Nothing

          r' = rewriteQuery remapQuery remapExpr r

leftJoin_ :: (Project (Scope a), Project (Scope b)) => Query a -> (Query b, Scope (a :|: b) -> QExpr Bool) -> Query (a :|: Maybe b)
leftJoin_ l (r, mkOn) = LeftJoin l r' (mkOn (getScope l :|: getScope r'))
    where r' = rewriteForJoin l r
rightJoin_ :: (Project (Scope a), Project (Scope b)) => Query a -> (Query b, Scope (a :|: b) -> QExpr Bool) -> Query (Maybe a :|: b)
rightJoin_ l (r, mkOn) = RightJoin l r' (mkOn (getScope l :|: getScope r'))
    where r' = rewriteForJoin l r
outerJoin_ :: (Project (Scope a), Project (Scope b)) => Query a -> (Query b, Scope (a :|: b) -> QExpr Bool) -> Query (Maybe a :|: Maybe b)
outerJoin_ l (r, mkOn) = OuterJoin l r' (mkOn ((getScope l) :|: (getScope r')))
    where r' = rewriteForJoin l r

project_ :: (Rescopable a, Rescopable b) =>  Query a -> (Scope a -> Scope b) -> Query b
project_ q a = Project q a

limit_, offset_ :: Query a -> Integer -> Query a
limit_ = Limit
offset_ = Offset

sortAsc_, sortDesc_ :: Typeable b => Query a -> (Scope a -> QExpr b) -> Query a
sortAsc_ q mkExpr = OrderBy q (GenQExpr (mkExpr (getScope q))) Ascending
sortDesc_ q mkExpr = OrderBy q (GenQExpr (mkExpr (getScope q))) Descending

groupBy_ :: Typeable b => Query a -> (Scope a -> QExpr b) -> Query a
groupBy_ q mkExpr = GroupBy q (GenQExpr (mkExpr (getScope q)))

where_ :: Query a -> (Scope a -> QExpr Bool) -> Query a
where_ q mkExpr = Filter q (mkExpr (getScope q))


primaryKeyExpr :: ( Table table, Table related ) =>
                  Proxy table -> Proxy related -> PrimaryKey table (ScopedField related Column) -> PrimaryKey table Column -> QExpr Bool
primaryKeyExpr (_ :: Proxy table) (_ :: Proxy related) pkFields pkValues =
    foldl1 (&&#) $
    zipWith (\(GenScopedField (x :: ScopedField related Column ty)) (GenQExpr (q :: QExpr ty2)) ->
                 case cast q of
                   Just q -> FieldE (x :: ScopedField related Column ty) ==# (q :: QExpr (ColumnType Column ty)) :: QExpr Bool
                   Nothing -> val_ False) pkFieldNames pkValueExprs
    where pkFieldNames = pkAllValues (Proxy :: Proxy table) mkScopedField pkFields
          pkValueExprs = pkAllValues (Proxy :: Proxy table) mkValE pkValues

          mkScopedField :: (Table table, FieldSchema a) => ScopedField related Column a -> GenScopedField related Column
          mkScopedField = GenScopedField
          mkValE :: FieldSchema a => Column a -> GenQExpr
          mkValE (x :: Column a) = GenQExpr (ValE (makeSqlValue (columnValue x)) :: QExpr a)

          from' :: Generic a => a -> Rep a ()
          from' = from

(@->) :: Table related =>
         Entity table Column -> (forall a. table a -> ForeignKey related a) -> Query (Entity related Column)
(Entity _ table) @-> (f :: forall a. table a -> ForeignKey related a) =
    all_ (of_ :: related Column)
      `where_` (\(Entity phantom fields) -> primaryKeyExpr (Proxy :: Proxy related) (Proxy :: Proxy related) (primaryKey phantom fields) pk)
    where pk :: PrimaryKey related Column
          ForeignKey pk = f table

(<-@) :: ( Table related, Table table ) =>
         (forall a. related a -> ForeignKey table a) -> Entity table Column -> Query (Entity related Column)
(f :: forall a. related a -> ForeignKey table a) <-@ (Entity phantom fields) =
    all_ (of_ :: related Column)
      `where_` (\(Entity _ scope) ->
                let ForeignKey pk = f scope
                in primaryKeyExpr (Proxy :: Proxy table) (Proxy :: Proxy related) pk (primaryKey phantom fields))

foreignKeyJoin :: ( Table table, Table related ) =>
                  Proxy table -> Proxy related -> PrimaryKey related (ScopedField table Column) -> PrimaryKey related (ScopedField related Column) -> QExpr Bool
foreignKeyJoin (_ :: Proxy table) (_ :: Proxy related) parent related = foldl1 (&&#) fieldEqExprs
    where parentFields = pkAllValues (Proxy :: Proxy related) genPar parent
          relatedFields = pkAllValues (Proxy :: Proxy related) genRel related

          from' :: Generic a => a -> Rep a ()
          from' = from

          genPar :: (Typeable ty, Show ty) => ScopedField table Column ty -> GenScopedField table Column
          genRel :: (Typeable ty, Show ty) => ScopedField related Column ty -> GenScopedField related Column
          genPar = gen
          genRel = gen

          gen :: (Table t, Typeable ty, Show ty) => ScopedField t Column ty -> GenScopedField t Column
          gen = GenScopedField

          fieldEqExprs :: [QExpr Bool]
          fieldEqExprs = zipWith (\(GenScopedField (ScopedField i name)) (GenScopedField (y :: ScopedField table' c' ty)) ->
                                  FieldE (ScopedField i name :: ScopedField table' c' ty) ==# FieldE y) parentFields relatedFields

-- | Given a query for a table, and an accessor for a foreignkey reference, return a query that joins the two tables
(==>) :: ( Table table, Table related
         , Project (Entity related (ScopedField related Column))
         , Project (Entity table (ScopedField table Column)) ) =>
         Query (Entity table Column) -> (forall a. table a -> ForeignKey related a) -> Query (Entity table Column :|: Entity related Column)
q ==> (f :: forall a. table a -> ForeignKey related a) =
    join_ q (All (Proxy :: Proxy related) 0) `where_`
      (\(Entity _ table :|: Entity relatedPhantom relatedFields) -> foreignKeyJoin (Proxy :: Proxy table) (Proxy :: Proxy related) (fk table) (primaryKey relatedPhantom relatedFields))
    where fk scope = let ForeignKey x = f scope
                     in x

(<==) :: ( Table table, Table related
         , Project (Entity related (ScopedField related Column))
         , Project (Entity table (ScopedField table Column)) ) =>
         Query (Entity table Column) -> (forall a. related a -> ForeignKey table a) -> Query (Entity table Column :|: Entity related Column)
q <== (f :: forall a. related a -> ForeignKey table a) =
    join_ q (All (Proxy :: Proxy related) 0) `where_`
              (\(Entity tablePhantom tableFields :|: Entity _ related) -> foreignKeyJoin (Proxy :: Proxy related) (Proxy :: Proxy table)
                                                                                         (reference . f $ related) (primaryKey tablePhantom tableFields))

-- -- | Get all related records for a relationship
-- (#@*) :: ( Relationship subject object r
--          , Project (Scope (QueryTable subject)), Project (Scope (QueryTable object))
--          , ScopeFields (QueryTable object) ) =>
--          Query (QueryTable subject) -> r -> Query (QueryTable subject :|: QueryTable object)
-- q #@* r = injectSubjectAndObjectProxy $
--           \subjectProxy (objectProxy :: Proxy object) ->
--           let allInRange = all_ (of_ :: object)
--           in (q `join_` allInRange) `where_` (\(sTbl :|: oTbl) -> joinCondition r subjectProxy objectProxy sTbl oTbl)
--     where injectSubjectAndObjectProxy :: (Proxy subject -> Proxy object -> Query (QueryTable subject :|: QueryTable object))
--                                       -> Query (QueryTable subject :|: QueryTable object)
--           injectSubjectAndObjectProxy f = f Proxy Proxy

-- (#*@) :: ( Relationship subject object r
--          , Project (Scope (QueryTable subject)), Project (Scope (QueryTable object))
--          , ScopeFields (QueryTable subject) ) =>
--          Query (QueryTable object) -> r -> Query (QueryTable subject :|: QueryTable object)
-- q #*@ r = let query = (all_ of_ `join_` q) `where_` (\(sTbl :|: oTbl) -> joinCondition r subjectProxy objectProxy sTbl oTbl)

--               proxies :: Query (QueryTable subject :|: QueryTable object) -> (Proxy subject, Proxy object)
--               proxies _ = (Proxy, Proxy)
--               (subjectProxy, objectProxy) = proxies query
--           in query

(#) :: (Table table, Typeable c, Typeable ty) => (a -> ScopedField table c ty) -> a -> QExpr (ColumnType c ty)
f # t = field_ (f t)

-- (#?) :: (Table table, Typeable ty) => (a -> Nullable (ScopedField table) ty) -> a -> QExpr (Maybe ty)
-- f #? t = let Nullable x = f t
--          in field_ x

infixr 5 # --, #?

(<#), (>#), (<=#), (>=#), (==#) :: (Typeable a, Show a) => QExpr a -> QExpr a -> QExpr Bool
(==#) = EqE
(<#) = LtE
(>#) = GtE
(<=#) = LeE
(>=#) = GeE

(&&#), (||#) :: QExpr Bool -> QExpr Bool -> QExpr Bool
(&&#) = AndE
(||#) = OrE

infixr 3 &&#
infixr 2 ||#
infix 4 ==#

(=#) :: Table table => ScopedField table c ty -> QExpr (ColumnType c ty) -> QAssignment
(=#) = QAssignment

list_ :: [QExpr a] -> QExpr [a]
list_ = ListE
in_ :: (Typeable a, Show a)=> QExpr a -> QExpr [a] -> QExpr Bool
in_ = InE

count_ :: Typeable a => QExpr a -> QExpr Int
count_ = CountE

text_ :: T.Text -> QExpr T.Text
text_ = ValE . SqlString . T.unpack
num_ :: Integral a => a -> QExpr a
num_ = ValE . SqlInteger . fromIntegral
val_ :: Convertible a SqlValue => a -> QExpr a
val_ = ValE . convert
enum_ :: Show a => a -> QExpr (BeamEnum a)
enum_ = ValE . SqlString . show
field_ :: (Table table, Typeable c, Typeable ty) => ScopedField table c ty -> QExpr (ColumnType c ty)
field_ = FieldE

just_ :: Show a => QExpr a -> QExpr (Maybe a)
just_ = JustE
nothing_ :: QExpr (Maybe a)
nothing_ = NothingE

isNothing_ :: Typeable a => QExpr (Maybe a) -> QExpr Bool
isNothing_ = IsNothingE
