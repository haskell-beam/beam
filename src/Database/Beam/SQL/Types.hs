{-# LANGUAGE StandaloneDeriving, GADTs #-}
module Database.Beam.SQL.Types where

import Data.Text (Text)
import Data.Time.Clock

import Database.HDBC

noConstraints, notNull :: SqlColDesc -> SQLColumnSchema
noConstraints desc = SQLColumnSchema desc []
notNull desc = SQLColumnSchema desc [SQLNotNull]

-- * SQL queries

data SQLCommand = Select SQLSelect
                | Insert SQLInsert
                | Update SQLUpdate

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

data SQLConstraint = SQLPrimaryKeyAutoIncrement
                   | SQLPrimaryKey
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

data SQLSelect = SQLSelect
               { selProjection :: SQLProjection
               , selFrom       :: SQLAliased SQLSource
               , selJoins      :: [SQLJoin]
               , selWhere      :: SQLExpr
               , selGrouping   :: Maybe SQLGrouping
               , selOrderBy    :: [SQLOrdering]
               , selLimit      :: Maybe Integer
               , selOffset     :: Maybe Integer }
                 deriving Show

data SQLFieldName = SQLFieldName Text
                  | SQLQualifiedFieldName Text Text
                    deriving Show

data SQLAliased a = SQLAliased a (Maybe Text)
                    deriving Show

data SQLProjection = SQLProjStar -- ^ The * from SELECT *
                   | SQLProjFields [SQLAliased SQLFieldName]
                     deriving Show

data SQLSource = SQLSourceTable Text
               | SQLSourceSelect SQLSelect
                 deriving Show

data SQLJoinType = SQLInnerJoin
                   deriving Show

data SQLJoin = SQLJoin SQLJoinType (SQLAliased SQLSource) SQLExpr
               deriving Show

data SQLGrouping = SQLGrouping
                 { sqlGroupBy :: [SQLFieldName]
                 , sqlHaving  :: SQLExpr }
                 deriving Show

data SQLOrdering = Asc SQLFieldName
                 | Desc SQLFieldName
                   deriving Show

data SQLExpr where
    SQLValE :: SqlValue -> SQLExpr
    SQLJustE :: SQLExpr -> SQLExpr

    SQLAndE :: SQLExpr -> SQLExpr -> SQLExpr
    SQLOrE :: SQLExpr -> SQLExpr -> SQLExpr

    SQLFieldE :: SQLFieldName -> SQLExpr

    SQLEqE :: SQLExpr -> SQLExpr -> SQLExpr

deriving instance Show SQLExpr