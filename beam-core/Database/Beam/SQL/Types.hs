{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Database.Beam.SQL.Types where

import Database.Beam.Backend.Types

import Data.Text (Text)
import Data.Time.Clock
import Data.Monoid
import Data.Functor
import Data.Typeable
import Data.Generics.Uniplate.Direct

-- -- * SQL queries
-- --
-- --   Types for most forms of SQL queries and data updates/inserts. This is the internal representation used by Beam.
-- --   Typically, you'd use the typed representation 'QExpr' and 'Q' to guarantee type-safety, and let Beam do the
-- --   low-level conversion to Sql

-- data SQLCommand be = Select (SQLSelect be)
--                    | Insert (SQLInsert be)
--                    | Update (SQLUpdate be)
--                    | Delete (SQLDelete be)

--                    -- DDL
--                    | CreateTable (SQLCreateTable be)
--                    deriving Show

-- data SQLCreateTable be
--     = SQLCreateTable
--     { ctTableName :: Text
--     , ctFields    :: [(Text, SQLColumnSchema be)] }
--     deriving Show

-- data SQLColumnSchema be
--     = SQLColumnSchema
--     { csType :: Text
--     , csIsNullable :: Bool
--     , csIsPrimaryKey :: Bool
--     , csIsAuto :: Bool
--     , csConstraints :: SQLConstraint be }
--     deriving Show

-- columnSchemaWithType :: Text -> SQLColumnSchema be
-- columnSchemaWithType t = SQLColumnSchema t False False False mempty

-- data SQLConstraint be
--     = SQLConstraint
--     { constraintPropagatesAs :: SQLConstraint be
--     , constraintNullifiesAs :: SQLConstraint be
--     , constraint :: [ Text ] }
--     deriving Show

-- instance Monoid (SQLConstraint be) where
--     mempty = SQLConstraint mempty mempty []
--     mappend (SQLConstraint propA nullA a) (SQLConstraint propB nullB b) =
--         SQLConstraint (mappend propA propB) (mappend nullA nullB) (mappend a b)

-- data SQLInsert be = SQLInsert
--                   { iTableName :: Text
--                   , iValues    :: [SQLValue be] }
--                   deriving Show

-- data SQLUpdate be = SQLUpdate
--                   { uTableNames  :: [Text]
--                   , uAssignments :: [(SQLFieldName, SQLExpr be)]
--                   , uWhere       :: Maybe (SQLExpr be) }
-- deriving instance BeamBackend be => Show (SQLUpdate be)

-- data SQLDelete be = SQLDelete
--                   { dTableName   :: Text
--                   , dWhere       :: Maybe (SQLExpr be) }
-- deriving instance BeamBackend be => Show (SQLDelete be)

-- data SQLSelect be = SQLSelect
--                   { selProjection :: SQLProjection be
--                   , selFrom       :: Maybe (SQLFrom be)
--                   , selWhere      :: SQLExpr be
--                   , selGrouping   :: Maybe (SQLGrouping be)
--                   , selOrderBy    :: [SQLOrdering be]
--                   , selLimit      :: Maybe Integer
--                   , selOffset     :: Maybe Integer }
-- deriving instance BeamBackend be => Show (SQLSelect be)
-- deriving instance BeamBackend be => Eq (SQLSelect be)

-- data SQLFieldName = SQLFieldName Text
--                   | SQLQualifiedFieldName Text Text
--                     deriving (Show, Eq)

-- data SQLAliased a = SQLAliased a (Maybe Text)
--                     deriving (Show, Eq)

-- data SQLProjection be = SQLProjStar -- ^ The * from SELECT *
--                       | SQLProj [SQLAliased (SQLExpr be)]
-- deriving instance BeamBackend be => Show (SQLProjection be)
-- deriving instance BeamBackend be => Eq (SQLProjection be)

-- data SQLSource be = SQLSourceTable Text
--                   | SQLSourceSelect (SQLSelect be)
--                   deriving (Show, Eq)

-- data SQLJoinType = SQLInnerJoin
--                  | SQLLeftJoin
--                  | SQLRightJoin
--                  | SQLOuterJoin
--                    deriving (Show, Eq)

-- data SQLFrom be = SQLFromSource (SQLAliased (SQLSource be))
--                 | SQLJoin SQLJoinType (SQLFrom be) (SQLFrom be) (SQLExpr be)
-- deriving instance BeamBackend be => Show (SQLFrom be)
-- deriving instance BeamBackend be => Eq (SQLFrom be)

-- data SQLGrouping be = SQLGrouping
--                     { sqlGroupBy :: [SQLExpr be]
--                     , sqlHaving  :: SQLExpr be }
-- deriving instance BeamBackend be => Show (SQLGrouping be)
-- deriving instance BeamBackend be => Eq (SQLGrouping be)

-- instance (BeamBackend be, FromBackendLiteral be Bool) => Monoid (SQLGrouping be) where
--     mappend (SQLGrouping group1 having1) (SQLGrouping group2 having2) =
--         SQLGrouping (group1 <> group2) (andE having1 having2)
--         where andE (SQLValE true) h
--                 | sqlValueIsTrue true = h
--               andE h (SQLValE true)
--                 | sqlValueIsTrue true = h
--               andE a b = SQLBinOpE "AND" a b
--     mempty = SQLGrouping mempty (SQLValE (SQLValue True))

-- data SQLOrdering be = Asc (SQLExpr be)
--                     | Desc (SQLExpr be)
-- deriving instance BeamBackend be => Show (SQLOrdering be)
-- deriving instance BeamBackend be => Eq (SQLOrdering be)

-- data SQLNull = SQLNull
--   deriving (Typeable, Show, Eq)

-- data SQLValue be where
--   SQLValue ::
--     (FromBackendLiteral be a, Typeable a, Show a, Eq a) =>
--     a -> SQLValue be

-- instance Eq (SQLValue be) where
--   SQLValue x == SQLValue y =
--     maybe False (== y) (cast x)
-- instance Show (SQLValue be) where
--   show (SQLValue x) =
--     "SQLValue (" ++ show x ++ ")"

-- sqlValueIsFalse, sqlValueIsTrue :: SQLValue be -> Bool
-- sqlValueIsTrue (SQLValue x) =
--   case cast x of
--     Just True -> True
--     _ -> False
-- sqlValueIsFalse (SQLValue x) =
--   case cast x of
--     Just False -> True
--     _ -> False

-- data SQLExpr' be f = SQLValE (SQLValue be)
--                    | SQLFieldE f

--                    | SQLBinOpE Text (SQLExpr' be f) (SQLExpr' be f)
--                    | SQLUnOpE Text (SQLExpr' be f)

--                    | SQLIsNothingE (SQLExpr' be f)
--                    | SQLIsJustE (SQLExpr' be f)

--                    | SQLListE [SQLExpr' be f]

--                    | SQLFuncE Text [SQLExpr' be f]

--                    | SQLExistsE (SQLSelect be)

--                    | SQLCaseE [(SQLExpr' be f, SQLExpr' be f)] (SQLExpr' be f)
--                    deriving Functor
-- deriving instance (BeamBackend be, Eq f) => Eq (SQLExpr' be f)
-- deriving instance (BeamBackend be, Show f) => Show (SQLExpr' be f)

-- type SQLExpr be = SQLExpr' be SQLFieldName

-- -- * Uniplate instances

-- instance Uniplate (SQLExpr be) where
--   uniplate (SQLValE v) = plate SQLValE |- v
--   uniplate (SQLFieldE f) = plate SQLFieldE |- f
--   uniplate (SQLBinOpE op a b) = plate SQLBinOpE |- op |* a |* b
--   uniplate (SQLUnOpE op a) = plate SQLUnOpE |- op |* a
--   uniplate (SQLIsNothingE e) = plate SQLIsNothingE |* e
--   uniplate (SQLIsJustE e) = plate SQLIsJustE |* e
--   uniplate (SQLListE es) = plate SQLListE ||* es
--   uniplate (SQLFuncE f es) = plate SQLFuncE |- f ||* es
--   uniplate (SQLExistsE s) = plate SQLExistsE |+ s
--   uniplate (SQLCaseE conds e) = plate SQLCaseE ||+ conds |* e

-- instance Biplate (SQLExpr be) (SQLExpr be) where
--   biplate = plateSelf
-- instance Biplate (SQLExpr be, SQLExpr be) (SQLExpr be) where
--   biplate (a, b) = plate (,) |* a |* b
-- instance Biplate (SQLSelect be) (SQLExpr be) where
--   biplate SQLSelect {..} =
--     plate SQLSelect |+ selProjection
--                     |+ selFrom
--                     |+ selWhere
--                     |+ selGrouping
--                     ||+ selOrderBy
--                     |- selLimit
--                     |- selOffset

-- instance Biplate (SQLProjection be) (SQLExpr be) where
--   biplate SQLProjStar = plate SQLProjStar
--   biplate (SQLProj es) = plate SQLProj ||+ es
-- instance Biplate y x => Biplate (Maybe y) x where
--   biplate Nothing = plate Nothing
--   biplate (Just x) = plate Just |+ x
-- instance Biplate y x => Biplate (SQLAliased y) x where
--   biplate (SQLAliased x nm) =
--     plate SQLAliased |+ x |- nm
-- instance Biplate (SQLFrom be) (SQLExpr be) where
--   biplate (SQLFromSource src) = plate SQLFromSource |+ src
--   biplate (SQLJoin ty a b e) = plate SQLJoin |- ty |+ a |+ b |* e
-- instance Biplate (SQLSource be) (SQLExpr be) where
--   biplate (SQLSourceTable tbl) = plate SQLSourceTable |- tbl
--   biplate (SQLSourceSelect sel) = plate SQLSourceSelect |+ sel
-- instance Biplate (SQLGrouping be) (SQLExpr be) where
--   biplate (SQLGrouping groupBy having) =
--     plate SQLGrouping ||* groupBy |* having
-- instance Biplate (SQLOrdering be) (SQLExpr be) where
--   biplate (Asc e) = plate Asc |* e
--   biplate (Desc e) = plate Desc |* e
