{-# LANGUAGE ScopedTypeVariables, GADTs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, RankNTypes, TypeFamilies, UndecidableInstances, TypeSynonymInstances, RoleAnnotations #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.Query.Types
    ( ScopedField(..), Entity(..)
    , QueryField, QueryExpr(..)

    , Query(..), QExpr(..), QAssignment(..), QOrder(..), GenQExpr(..), GenScopedField(..)

    , ScopeFields(..), Scope(..), Scope'(..), Project(..), Rescopable(..), ScopingRule
    , getScope ) where

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Fields
import Database.Beam.SQL
import Database.HDBC

import Control.Applicative
import Control.Monad.Writer hiding (All)

import Data.Monoid hiding (All)
import Data.Proxy
import Data.Coerce
import Data.Data
import Data.String
import qualified Data.Text as T

import Unsafe.Coerce

-- * Beam queries

-- | A `ScopedField` represents a field that has been brought in scope via an `allQ`. The `q` type is a thread type that ensures that this field cannot escape this query.
data ScopedField (table :: (* -> *) -> *) (c :: * -> *) ty =
    ScopedField Int T.Text
    deriving (Show, Typeable, Eq)
--type ScopedTable (table :: (* -> *) -> *) = Entity table (ScopedField table)

data QueryField table field
data QueryExpr t = QueryExpr t deriving (Show, Read, Eq, Ord)

instance FieldSchema t => FromSqlValues (QueryExpr t) where
    fromSqlValues' = QueryExpr <$> fromSqlValue
    valuesNeeded _ = 1

type ScopingRule = SQLFieldName -> SQLFieldName

class Project s where
    project :: ScopingRule -> s -> [SQLExpr]

data QOrder = Ascending | Descending deriving (Show, Read, Eq, Ord, Enum)

data GenQExpr where
    GenQExpr :: Typeable t => QExpr t -> GenQExpr
data GenScopedField table c where
    GenScopedField :: (Table table, Typeable t, Show t) => ScopedField table c t -> GenScopedField table c

-- | A query that produces results of type `a`
data Query a where
    All :: (Table table, ScopeFields (Entity table Column)) => Proxy table -> Int -> Query (Entity table Column)
    EmptySet :: Query a
    Filter ::  Query a -> QExpr Bool -> Query a
    GroupBy :: Query a -> GenQExpr -> Query a
    OrderBy :: Query a -> GenQExpr -> QOrder -> Query a
    Join :: ( Project (Scope schema1), Project (Scope schema2) ) =>
            Query schema1 -> Query schema2 -> Query (schema1 :|: schema2)

    -- More joins...
    LeftJoin :: ( Project (Scope schema1), Project (Scope schema2) ) =>
                Query schema1 -> Query schema2 -> QExpr Bool -> Query (schema1 :|: Maybe schema2)
    RightJoin :: ( Project (Scope schema1), Project (Scope schema2) ) =>
                 Query schema1 -> Query schema2 -> QExpr Bool -> Query (Maybe schema1 :|: schema2)
    OuterJoin :: ( Project (Scope schema1), Project (Scope schema2)) =>
                 Query schema1 -> Query schema2 -> QExpr Bool -> Query (Maybe schema1 :|: Maybe schema2)

    Project :: (Rescopable a, Rescopable b) => Query a -> (Scope a -> Scope b) -> Query b

    Limit :: Query a -> Integer -> Query a
    Offset :: Query a -> Integer -> Query a

data QExpr t where
    FieldE :: (Table table, Typeable c, Typeable ty) => ScopedField table c ty -> QExpr (ColumnType c ty)
    MaybeFieldE :: (Table table, Typeable c, Typeable ty) => Maybe (ScopedField table c ty) -> QExpr (Maybe (ColumnType c ty))

    OrE :: QExpr Bool -> QExpr Bool -> QExpr Bool
    AndE :: QExpr Bool -> QExpr Bool -> QExpr Bool

    EqE, NeqE, LtE, GtE, LeE, GeE :: (Typeable a, Show a) => QExpr a -> QExpr a -> QExpr Bool

    ValE :: SqlValue -> QExpr a

    JustE :: Show a => QExpr a -> QExpr (Maybe a)
    NothingE :: QExpr (Maybe a)

    IsNothingE :: Typeable a => QExpr (Maybe a) -> QExpr Bool

    InE :: (Typeable a, Show a) => QExpr a -> QExpr [a] -> QExpr Bool

    ListE :: [QExpr a] -> QExpr [a]

    -- Aggregate functions
    CountE :: Typeable a => QExpr a -> QExpr Int

data QAssignment where
    QAssignment :: Table table =>
                   ScopedField table c ty
                -> QExpr (ColumnType c ty)
                -> QAssignment

deriving instance Typeable Query
deriving instance Typeable QExpr

-- ** Scoping class

class ScopeFields a where
    scopeFields :: Proxy a -> Proxy c -> Int -> Scope' a c
instance (ScopeFields a, ScopeFields b) => ScopeFields (a :|: b) where
    scopeFields  (_ :: Proxy (a :|: b)) c i = scopeFields (Proxy :: Proxy a) c i :|: scopeFields (Proxy :: Proxy b) c i
instance Table table => ScopeFields (Entity table d) where
    scopeFields (_ :: Proxy (Entity table d)) (_ :: Proxy c) i = Entity scopedPhantom scopedTable
        where scopedPhantom :: PhantomFields table (ScopedField table c)
              scopedPhantom = phantomChangeRep (Proxy :: Proxy table) scopeField $
                              phantomFieldSettings (Proxy :: Proxy table)
              scopedTable = changeRep scopeField $
                            (tblFieldSettings :: TableSettings table)

              scopeField :: TableField table a -> ScopedField table c a
              scopeField tf = ScopedField i (fieldName tf)

type family Scope' a c where
    Scope' (Maybe t) c = Scope' t (Nullable c)
    Scope' (a :|: b) c = Scope' a c :|: Scope' b c
    -- Scope' (Entity x c) (Nullable c) = Entity x (Nullable (ScopedField x))
    Scope' (Entity x a) c = Entity x (ScopedField x c)
    Scope' (QueryExpr t) c = QExpr t

type Scope a = Scope' a Column

class Rescopable a where
    rescope :: Proxy a -> Proxy c -> Proxy d ->  Scope' a c -> Scope' a d
instance Rescopable t => Rescopable (Maybe t) where
    rescope (Proxy :: Proxy (Maybe t)) (Proxy :: Proxy c) (Proxy :: Proxy d) s =
        rescope (Proxy :: Proxy t) (Proxy :: Proxy (Nullable c)) (Proxy :: Proxy (Nullable d)) s
instance (Rescopable a, Rescopable b) => Rescopable (a :|: b) where
    rescope (_ :: Proxy (a :|: b)) c d (a :|: b) = rescope (Proxy :: Proxy a) c d a :|: rescope (Proxy :: Proxy b) c d b
instance Table x => Rescopable (Entity x a) where
    rescope (_ :: Proxy (Entity x a)) (_ :: Proxy c) (_ :: Proxy d) (Entity phantom fields) = Entity (phantomChangeRep (Proxy :: Proxy x) scopeField phantom) (changeRep scopeField fields)
        where scopeField :: ScopedField x c t -> ScopedField x d t
              scopeField = coerce -- type-safe coercion thanks to GHC!
instance Rescopable (QueryExpr t) where
    rescope _ _ _ q = q

getScope' :: Proxy c -> Query a -> Scope' a c
getScope' f x@(All _ i) = scopeFields (aProxy x) f i
    where aProxy :: Query a -> Proxy a
          aProxy _ = Proxy
getScope' _ EmptySet = error "getScope': EmptySet"
getScope' f (Filter q _) = getScope' f q
getScope' f (GroupBy q _) = getScope' f q
getScope' f (Join q1 q2) = getScope' f q1 :|: getScope' f q2
getScope' (f :: Proxy c) (LeftJoin q1 q2 _) = getScope' f q1 :|: getScope' (Proxy :: Proxy (Nullable c)) q2
getScope' (f :: Proxy c) (RightJoin q1 q2 _) = getScope' (Proxy :: Proxy (Nullable c)) q1 :|: getScope' f q2
getScope' (f :: Proxy c) (OuterJoin q1 q2 _) = getScope' (Proxy :: Proxy (Nullable c)) q1 :|: getScope' (Proxy :: Proxy (Nullable c)) q2
getScope' (f :: Proxy c) (Project (q :: Query b) proj :: Query a) = rescope (Proxy :: Proxy a) (Proxy :: Proxy Column) (Proxy :: Proxy c) $
                                                                    proj $
                                                                    rescope (Proxy :: Proxy b) (Proxy :: Proxy c) (Proxy :: Proxy Column) $
                                                                    getScope' f q
getScope' f (Limit q _) = getScope' f q
getScope' f (Offset q _) = getScope' f q
getScope' f (OrderBy q _ _) = getScope' f q

getScope :: Query a -> Scope a
getScope = getScope' (Proxy :: Proxy Column)

-- ** Hand-derived instances for Query and QExpr since their types are too complicated for the deriving mechanism

instance Show (Query a) where
    show (All table i) = concat ["All ", show i, " :: ", T.unpack . dbTableName $ table]
    show (Filter q e) = concat ["Filter (", show q, ") (", show e, ")"]
    show (GroupBy q es) = concat ["GroupBy (", show q, ") (", show es, ")"]
    show (OrderBy q es ord) = concat ["OrderBy (", show q, ") (", show es, ") ", show ord]
    show (Join q1 q2) = concat ["Join (", show q1, ") (", show q2, ")"]
    show (LeftJoin q1 q2 e) = concat ["LeftJoin (", show q1, ") (", show q2, ") (", show e, ")"]
    show (OuterJoin q1 q2 e) = concat ["OuterJoin (", show q1, ") (", show q2, ") (", show e, ")"]
    show (RightJoin q1 q2 e) = concat ["RightJoin (", show q1, ") (", show q2, ") (", show e, ")"]
    show (Project q _) = concat ["Project (", show q, ") _"]
    show (Limit q i) = concat ["Limit (", show q, ") ", show i]
    show (Offset q i) = concat ["Offset (", show q, ") ", show i]
    show EmptySet = "EmptySet"

instance Show (QExpr t) where
    show (FieldE x) = concat ["FieldE (", show x, ")"]
    show (MaybeFieldE x) = concat ["MaybeFieldE (", show x, ")"]

    show (AndE a b) = concat ["AndE (", show a, ") (", show b, ")"]
    show (OrE a b) = concat ["OrE (", show a, ") (", show b, ")"]

    show (EqE a b) = concat ["EqE (", show a, ") (", show b, ")"]
    show (LtE a b) = concat ["LtE (", show a, ") (", show b, ")"]
    show (GtE a b) = concat ["GtE (", show a, ") (", show b, ")"]
    show (LeE a b) = concat ["LeE (", show a, ") (", show b, ")"]
    show (GeE a b) = concat ["GeE (", show a, ") (", show b, ")"]
    show (NeqE a b) = concat ["NeqE (", show a, ") (", show b, ")"]

    show (ValE v) = concat ["ValE ", show v]

    show (JustE b) = concat ["JustE ", show b]
    show NothingE = "NothingE"

    show (IsNothingE b) = concat ["IsNothingE ", show b]

    show (InE a xs) = concat ["InE (", show a, ") ", show xs]
    show (ListE xs) = concat ["ListE ", show xs]

    show (CountE x) = concat ["CountE (", show x, ")"]

instance Eq (Query a) where
    EmptySet == EmptySet = True
    All _ x == All _ y = x == y
    Filter q1 e1 == Filter q2 e2 = q1 == q2 && e1 == e2
    GroupBy q1 es1 == GroupBy q2 es2 = q1 == q2 && es1 == es2
    OrderBy q1 es1 ord1 == OrderBy q2 es2 ord2 = q1 == q2 && es1 == es2 && ord1 == ord2
    Join a1 b1 == Join a2 b2 = a1 == a2 && b1 == b2
    Limit q1 l1 == Limit q2 l2 = q1 == q2 && l1 == l2
    Offset q1 l1 == Offset q2 l2 = q1 == q2 && l1 == l2
    _ == _ = False

instance Eq (QExpr t) where
    FieldE q1 == FieldE q2 =
        case cast q1 of
          Nothing -> False
          Just q1 -> q1 == q2
    MaybeFieldE q1 == MaybeFieldE q2 =
        case cast q1 of
          Nothing -> False
          Just q1 -> q1 == q2

    AndE a1 b1 == AndE a2 b2 = a1 == a2 && b1 == b2

    EqE a1 b1 == EqE a2 b2 = case cast (a1, b1) of
                               Just (a1, b1)
                                   | a1 == a2 && b1 == b2 -> True
                                   | otherwise -> False
                               Nothing -> False

    ValE a == ValE b = a == b

    JustE a == JustE b = a == b
    NothingE == NothingE = True

    IsNothingE a == IsNothingE b = case cast a of
                                     Just a' -> a' == b
                                     Nothing -> False
    InE a as == InE b bs = case cast (a, as) of
                             Nothing -> False
                             Just (a', as') -> a' == b && as' == bs
    ListE a == ListE b = a == b

    CountE a == CountE b = case cast a of
                             Nothing -> False
                             Just a' -> a' == b

    _ == _ = False

deriving instance Show GenQExpr
instance Eq GenQExpr where
    GenQExpr q1 == GenQExpr q2 = case cast q1 of
                                   Nothing -> False
                                   Just q1 -> q1 == q2
