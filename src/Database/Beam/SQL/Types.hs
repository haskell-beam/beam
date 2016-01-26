{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.SQL.Types where

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

data SQLCommand = Select SQLSelect
                | Insert SQLInsert
                | Update SQLUpdate
                | Delete SQLDelete

                -- DDL
                | CreateTable SQLCreateTable
                deriving Show

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

data SQLInsert = SQLInsert
               { iTableName :: Text
               , iValues    :: [SqlValue] }
               deriving Show

data SQLUpdate = SQLUpdate
               { uTableNames  :: [Text]
               , uAssignments :: [(SQLFieldName, SQLExpr)]
               , uWhere       :: Maybe SQLExpr }
                 deriving Show

data SQLDelete = SQLDelete
               { dTableName   :: Text
               , dWhere       :: Maybe SQLExpr }
                 deriving Show

data SQLSelect = SQLSelect
               { selProjection :: SQLProjection
               , selFrom       :: Maybe SQLFrom
               , selWhere      :: SQLExpr
               , selGrouping   :: Maybe SQLGrouping
               , selOrderBy    :: [SQLOrdering]
               , selLimit      :: Maybe Integer
               , selOffset     :: Maybe Integer }
                 deriving (Show, Eq, Data)

data SQLFieldName = SQLFieldName Text
                  | SQLQualifiedFieldName Text Text
                    deriving (Show, Eq, Data)

data SQLAliased a = SQLAliased a (Maybe Text)
                    deriving (Show, Eq, Data)

data SQLProjection = SQLProjStar -- ^ The * from SELECT *
                   | SQLProj [SQLAliased SQLExpr]
                     deriving (Show, Eq, Data)

data SQLSource = SQLSourceTable Text
               | SQLSourceSelect SQLSelect
                 deriving (Show, Eq, Data)

data SQLJoinType = SQLInnerJoin
                 | SQLLeftJoin
                 | SQLRightJoin
                 | SQLOuterJoin
                   deriving (Show, Eq, Data)

data SQLFrom = SQLFromSource (SQLAliased SQLSource)
             | SQLJoin SQLJoinType SQLFrom SQLFrom SQLExpr
               deriving (Show, Eq, Data)

data SQLGrouping = SQLGrouping
                 { sqlGroupBy :: [SQLExpr]
                 , sqlHaving  :: SQLExpr }
                 deriving (Show, Eq, Data)

instance Monoid SQLGrouping where
    mappend (SQLGrouping group1 having1) (SQLGrouping group2 having2) =
        SQLGrouping (group1 <> group2) (andE having1 having2)
        where andE (SQLValE (SqlBool True)) h = h
              andE h (SQLValE (SqlBool True)) = h
              andE a b = SQLBinOpE "AND" a b
    mempty = SQLGrouping mempty (SQLValE (SqlBool True))

data SQLOrdering = Asc SQLExpr
                 | Desc SQLExpr
                   deriving (Show, Eq, Data)

data SQLExpr' f = SQLValE SqlValue
                | SQLFieldE f

                | SQLBinOpE Text (SQLExpr' f) (SQLExpr' f)
                | SQLUnOpE Text (SQLExpr' f)

                | SQLIsNothingE (SQLExpr' f)
                | SQLIsJustE (SQLExpr' f)

                | SQLListE [SQLExpr' f]

                | SQLFuncE Text [SQLExpr' f]

                | SQLExistsE SQLSelect

                | SQLCaseE [(SQLExpr' f, SQLExpr' f)] (SQLExpr' f)
                  deriving (Show, Functor, Eq, Data)
deriving instance Data SqlValue
type SQLExpr = SQLExpr' SQLFieldName
