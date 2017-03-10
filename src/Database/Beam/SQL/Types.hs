{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.SQL.Types where

import Database.Beam.Backend.Types

import Data.Text (Text)
import Data.Time.Clock
import Data.Monoid
import Data.Data
import Data.Functor

import Database.HDBC

-- * SQL queries
--
--   Types for most forms of SQL queries and data updates/inserts. This is the internal representation used by Beam.
--   Typically, you'd use the typed representation 'QExpr' and 'Q' to guarantee type-safety, and let Beam do the
--   low-level conversion to Sql

data SQLCommand be = Select (SQLSelect be)
                   | Insert (SQLInsert be)
                   | Update (SQLUpdate be)
                   | Delete (SQLDelete be)

                   -- DDL
                   | CreateTable (SQLCreateTable be)
                   deriving Show

data SQLCreateTable be
    = SQLCreateTable
    { ctTableName :: Text
    , ctFields    :: [(Text, SQLColumnSchema be)] }
    deriving Show

data SQLColumnSchema be
    = SQLColumnSchema
    { csType :: Text
    , csIsNullable :: Bool
    , csIsPrimaryKey :: Bool
    , csIsAuto :: Bool
    , csConstraints :: SQLConstraint be }
    deriving Show

columnSchemaWithType :: Text -> SQLColumnSchema be
columnSchemaWithType t = SQLColumnSchema t False False False mempty

data SQLConstraint be
    = SQLConstraint
    { constraintPropagatesAs :: SQLConstraint be
    , constraintNullifiesAs :: SQLConstraint be
    , constraint :: [ Text ] }
    deriving Show

instance Monoid (SQLConstraint be) where
    mempty = SQLConstraint mempty mempty []
    mappend (SQLConstraint propA nullA a) (SQLConstraint propB nullB b) =
        SQLConstraint (mappend propA propB) (mappend nullA nullB) (mappend a b)

data SQLInsert be = SQLInsert
                  { iTableName :: Text
                  , iValues    :: [BackendLiteral be] }
deriving instance BeamBackend be => Show (SQLInsert be)

data SQLUpdate be = SQLUpdate
                  { uTableNames  :: [Text]
                  , uAssignments :: [(SQLFieldName, SQLExpr be)]
                  , uWhere       :: Maybe (SQLExpr be) }
deriving instance BeamBackend be => Show (SQLUpdate be)

data SQLDelete be = SQLDelete
                  { dTableName   :: Text
                  , dWhere       :: Maybe (SQLExpr be) }
deriving instance BeamBackend be => Show (SQLDelete be)

data SQLSelect be = SQLSelect
                  { selProjection :: SQLProjection be
                  , selFrom       :: Maybe (SQLFrom be)
                  , selWhere      :: SQLExpr be
                  , selGrouping   :: Maybe (SQLGrouping be)
                  , selOrderBy    :: [SQLOrdering be]
                  , selLimit      :: Maybe Integer
                  , selOffset     :: Maybe Integer }
deriving instance BeamBackend be => Show (SQLSelect be)
deriving instance BeamBackend be => Eq (SQLSelect be)
deriving instance BeamBackend be => Data (SQLSelect be)

data SQLFieldName = SQLFieldName Text
                  | SQLQualifiedFieldName Text Text
                    deriving (Show, Eq, Data)

data SQLAliased a = SQLAliased a (Maybe Text)
                    deriving (Show, Eq, Data)

data SQLProjection be = SQLProjStar -- ^ The * from SELECT *
                      | SQLProj [SQLAliased (SQLExpr be)]
deriving instance BeamBackend be => Show (SQLProjection be)
deriving instance BeamBackend be => Eq (SQLProjection be)
deriving instance BeamBackend be => Data (SQLProjection be)

data SQLSource be = SQLSourceTable Text
                  | SQLSourceSelect (SQLSelect be)
                  deriving (Show, Eq)
deriving instance BeamBackend be => Data (SQLSource be)

data SQLJoinType = SQLInnerJoin
                 | SQLLeftJoin
                 | SQLRightJoin
                 | SQLOuterJoin
                   deriving (Show, Eq, Data)

data SQLFrom be = SQLFromSource (SQLAliased (SQLSource be))
                | SQLJoin SQLJoinType (SQLFrom be) (SQLFrom be) (SQLExpr be)
deriving instance BeamBackend be => Show (SQLFrom be)
deriving instance BeamBackend be => Eq (SQLFrom be)
deriving instance BeamBackend be => Data (SQLFrom be)

data SQLGrouping be = SQLGrouping
                    { sqlGroupBy :: [SQLExpr be]
                    , sqlHaving  :: SQLExpr be }
deriving instance BeamBackend be => Show (SQLGrouping be)
deriving instance BeamBackend be => Eq (SQLGrouping be)
deriving instance BeamBackend be => Data (SQLGrouping be)

instance (BeamBackend be, FromBackendLiteral be Bool) => Monoid (SQLGrouping be) where
    mappend (SQLGrouping group1 having1) (SQLGrouping group2 having2) =
        SQLGrouping (group1 <> group2) (andE having1 having2)
        where andE (SQLValE true) h | Just True <- fromBackendLiteral true = h
              andE h (SQLValE true) | Just True <- fromBackendLiteral true = h
              andE a b = SQLBinOpE "AND" a b
    mempty = SQLGrouping mempty (SQLValE (toBackendLiteral True))

data SQLOrdering be = Asc (SQLExpr be)
                    | Desc (SQLExpr be)
deriving instance BeamBackend be => Show (SQLOrdering be)
deriving instance BeamBackend be => Eq (SQLOrdering be)
deriving instance BeamBackend be => Data (SQLOrdering be)

data SQLExpr' be f = SQLValE (BackendLiteral be)
                   | SQLFieldE f

                   | SQLBinOpE Text (SQLExpr' be f) (SQLExpr' be f)
                   | SQLUnOpE Text (SQLExpr' be f)

                   | SQLIsNothingE (SQLExpr' be f)
                   | SQLIsJustE (SQLExpr' be f)

                   | SQLListE [SQLExpr' be f]

                   | SQLFuncE Text [SQLExpr' be f]

                   | SQLExistsE (SQLSelect be)

                   | SQLCaseE [(SQLExpr' be f, SQLExpr' be f)] (SQLExpr' be f)
                   deriving Functor
deriving instance (BeamBackend be, Eq f) => Eq (SQLExpr' be f)
deriving instance (BeamBackend be, Show f) => Show (SQLExpr' be f)
deriving instance (BeamBackend be, Data f) => Data (SQLExpr' be f)

type SQLExpr be = SQLExpr' be SQLFieldName
