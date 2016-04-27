{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.SQL.Types where

import Database.Beam.Internal

import Data.Text (Text)
import Data.Time.Clock
import Data.Monoid
import Data.Data
import Data.Functor

import Database.HDBC

noConstraints, notNull :: SqlColDesc -> SQLColumnSchema
noConstraints desc = SQLColumnSchema desc []
notNull desc = SQLColumnSchema desc [SQLNotNull]

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
                   | CreateTable SQLCreateTable
deriving instance Show (BeamBackendValue be) => Show (SQLCommand be)

data SQLCreateTable = SQLCreateTable
                    { ctTableName :: Text
                    , ctFields    :: [(Text, SQLColumnSchema)] }
                      deriving Show

data SQLColumnSchema = SQLColumnSchema
                     { csType :: SqlColDesc
                     , csConstraints :: [SQLConstraint] }
                       deriving Show

data SQLConstraint = SQLPrimaryKey
                   | SQLAutoIncrement
                   | SQLNotNull
                     deriving (Show, Eq)

data SQLInsert be = SQLInsert
                  { iTableName :: Text
                  , iValues    :: [BeamBackendValue be] }
deriving instance Show (BeamBackendValue be) => Show (SQLInsert be)

data SQLUpdate be = SQLUpdate
                  { uTableNames  :: [Text]
                  , uAssignments :: [(SQLFieldName, SQLExpr be)]
                  , uWhere       :: Maybe (SQLExpr be) }
deriving instance Show (BeamBackendValue be) => Show (SQLUpdate be)

data SQLDelete be = SQLDelete
                  { dTableName   :: Text
                  , dWhere       :: Maybe (SQLExpr be) }
deriving instance Show (BeamBackendValue be) => Show (SQLDelete be)

data SQLSelect be = SQLSelect
                  { selProjection :: SQLProjection be
                  , selFrom       :: Maybe (SQLFrom be)
                  , selWhere      :: SQLExpr be
                  , selGrouping   :: Maybe (SQLGrouping be)
                  , selOrderBy    :: [SQLOrdering be]
                  , selLimit      :: Maybe Integer
                  , selOffset     :: Maybe Integer }
deriving instance Show (BeamBackendValue be) => Show (SQLSelect be)
deriving instance Eq (BeamBackendValue be) => Eq (SQLSelect be)
deriving instance (Data (BeamBackendValue be), Data be, Typeable be) => Data (SQLSelect be)

data SQLFieldName = SQLFieldName Text
                  | SQLQualifiedFieldName Text Text
                    deriving (Show, Eq, Data)

data SQLAliased a = SQLAliased a (Maybe Text)
                    deriving (Show, Eq, Data)

data SQLProjection be = SQLProjStar -- ^ The * from SELECT *
                      | SQLProj [SQLAliased (SQLExpr be)]

deriving instance Show (BeamBackendValue be) => Show (SQLProjection be)
deriving instance Eq (BeamBackendValue be) => Eq (SQLProjection be)
deriving instance (Data (BeamBackendValue be), Data be) => Data (SQLProjection be)

data SQLSource be = SQLSourceTable Text
                  | SQLSourceSelect (SQLSelect be)
deriving instance Show (BeamBackendValue be) => Show (SQLSource be)
deriving instance Eq (BeamBackendValue be) => Eq (SQLSource be)
deriving instance (Data (BeamBackendValue be), Data be) => Data (SQLSource be)

data SQLJoinType = SQLInnerJoin
                 | SQLLeftJoin
                 | SQLRightJoin
                 | SQLOuterJoin
                   deriving (Show, Eq, Data)

data SQLFrom be = SQLFromSource (SQLAliased (SQLSource be))
                | SQLJoin SQLJoinType (SQLFrom be) (SQLFrom be) (SQLExpr be)

deriving instance Show (BeamBackendValue be) => Show (SQLFrom be)
deriving instance Eq (BeamBackendValue be) => Eq (SQLFrom be)
deriving instance (Data (BeamBackendValue be), Data be) => Data (SQLFrom be)

data SQLGrouping be = SQLGrouping
                    { sqlGroupBy :: [SQLExpr be]
                    , sqlHaving  :: SQLExpr be }
deriving instance Show (BeamBackendValue be) => Show (SQLGrouping be)
deriving instance Eq (BeamBackendValue be) => Eq (SQLGrouping be)
deriving instance (Data (BeamBackendValue be), Data be) => Data (SQLGrouping be)

instance BeamBackend be => Monoid (SQLGrouping be) where
    mappend (SQLGrouping group1 having1) (SQLGrouping group2 having2) =
        SQLGrouping (group1 <> group2) (andE having1 having2)
        where andE (SQLValE true) h | backendAsBool true = h
              andE h (SQLValE true) | backendAsBool true = h
              andE a b = SQLBinOpE "AND" a b
    mempty = SQLGrouping mempty (SQLValE (sqlBool True))

data SQLOrdering be = Asc (SQLExpr be)
                    | Desc (SQLExpr be)
deriving instance Show (BeamBackendValue be) => Show (SQLOrdering be)
deriving instance (Data (BeamBackendValue be), Data be) => Data (SQLOrdering be)
deriving instance Eq (BeamBackendValue be) => Eq (SQLOrdering be)

data SQLExpr' be f = SQLValE (BeamBackendValue be)
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
deriving instance (Data (BeamBackendValue be), Data be, Data f, Typeable f) => Data (SQLExpr' be f)
deriving instance (Show (BeamBackendValue be), Show f) => Show (SQLExpr' be f)
deriving instance (Eq (BeamBackendValue be), Eq f) => Eq (SQLExpr' be f)
type SQLExpr be = SQLExpr' be SQLFieldName
