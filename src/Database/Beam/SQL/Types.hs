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
                | CreateTableCmd SQLCreateTable
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
                   | SQLNotNull
                     deriving (Show, Eq)

data SQLInsert = SQLInsert
               { iTableName :: Text
               , iValues    :: [SqlValue] }
               deriving Show

data SQLSelect = SQLSelect
               { selProjection :: SQLProjection
               , selFrom       :: SQLAliased SQLSource
               , selJoins      :: [SQLJoin]
               , selWhere      :: SQLExpr Bool
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

data SQLJoin = SQLJoin SQLJoinType (SQLAliased SQLSource) (SQLExpr Bool)
               deriving Show

data SQLGrouping = SQLGrouping
                 { sqlGroupBy :: [SQLFieldName]
                 , sqlHaving  :: SQLExpr Bool }
                 deriving Show

data SQLOrdering = Asc SQLFieldName
                 | Desc SQLFieldName
                   deriving Show

-- data SQLValue = SQLInt Integer
--               | SQLFloat Float
--               | SQLBoolean Bool
--               | SQLText Text
--               | SQLNull
--                 deriving (Show, Eq)

-- class Show v => SQLValable v where
--     toSQLVal :: v -> SQLValue

-- instance SQLValable Bool where
--     toSQLVal = SQLBoolean
-- instance SQLValable Text where
--     toSQLVal = SQLText
-- instance SQLValable Int where
--     toSQLVal = SQLInt . fromIntegral
-- instance SQLValable UTCTime where
--     toSQLVal = undefined

data SQLExpr ty where
    SQLValE :: SqlValue -> SQLExpr v
    SQLJustE :: Show v => SQLExpr v -> SQLExpr (Maybe v)

    SQLAndE :: SQLExpr Bool -> SQLExpr Bool -> SQLExpr Bool
    SQLOrE :: SQLExpr Bool -> SQLExpr Bool -> SQLExpr Bool

    SQLFieldE :: SQLFieldName -> SQLExpr a

    SQLEqE :: Show a => SQLExpr a -> SQLExpr a -> SQLExpr Bool

deriving instance Show ty => Show (SQLExpr ty)