{-# LANGUAGE StandaloneDeriving, GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.SQL.Types where

import Data.Text (Text)
import Data.Time.Clock
import Data.Monoid

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
               , selFrom       :: SQLFrom
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
                   | SQLProj [SQLAliased SQLExpr]
                     deriving Show

data SQLSource = SQLSourceTable Text
               | SQLSourceSelect SQLSelect
                 deriving Show

data SQLJoinType = SQLInnerJoin
                 | SQLLeftJoin
                 | SQLRightJoin
                 | SQLOuterJoin
                   deriving Show

data SQLFrom = SQLFromSource (SQLAliased SQLSource)
             | SQLJoin SQLJoinType SQLFrom SQLFrom SQLExpr
               deriving Show

data SQLGrouping = SQLGrouping
                 { sqlGroupBy :: [SQLExpr]
                 , sqlHaving  :: SQLExpr }
                 deriving (Show)

instance Monoid SQLGrouping where
    mappend (SQLGrouping group1 having1) (SQLGrouping group2 having2) =
        SQLGrouping (group1 <> group2) (andE having1 having2)
        where andE (SQLValE (SqlBool True)) h = h
              andE h (SQLValE (SqlBool True)) = h
              andE a b = SQLAndE a b
    mempty = SQLGrouping mempty (SQLValE (SqlBool True))

data SQLOrdering = Asc SQLExpr
                 | Desc SQLExpr
                   deriving Show

data SQLExpr where
    SQLValE :: SqlValue -> SQLExpr

    SQLAndE :: SQLExpr -> SQLExpr -> SQLExpr
    SQLOrE :: SQLExpr -> SQLExpr -> SQLExpr

    SQLFieldE :: SQLFieldName -> SQLExpr

    SQLEqE, SQLLtE, SQLGtE, SQLLeE, SQLGeE, SQLNeqE :: SQLExpr -> SQLExpr -> SQLExpr

    SQLIsNothingE :: SQLExpr -> SQLExpr

    SQLInE :: SQLExpr -> SQLExpr -> SQLExpr
    SQLListE :: [SQLExpr] -> SQLExpr

    SQLCountE :: SQLExpr -> SQLExpr

deriving instance Show SQLExpr
