{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.SQL
    ( module Database.Beam.SQL.Types

    , ppSQL

    , conjugateWhere) where

import Database.Beam.SQL.Types

import Control.Applicative hiding (empty)
import Control.Arrow hiding ((<+>))
import Control.Monad.Writer hiding ((<>))

import Data.Maybe
import Data.Text (Text, unpack)

import Database.HDBC

import Text.PrettyPrint

-- * Pretty printing support
ppSQL :: SQLCommand -> (String, [SqlValue])
ppSQL c = first render (runWriter (ppCmd c))

type DocAndVals = Writer [SqlValue] Doc

ppCmd :: SQLCommand -> DocAndVals
ppCmd (Select sel) = ppSel sel
ppCmd (CreateTable ct) = ppCreateTable ct
ppCmd (Insert i) = ppInsert i
ppCmd (Update u) = ppUpdate u

-- ** Create table printing support

ppCreateTable :: SQLCreateTable -> DocAndVals
ppCreateTable (SQLCreateTable tblName schema) =
    do fieldSchemas <- mapM ppFieldSchema schema
       return (text "CREATE TABLE" <+> text (unpack tblName) <+> parens (hsep (punctuate comma fieldSchemas)))

ppFieldSchema :: (Text, SQLColumnSchema) -> DocAndVals
ppFieldSchema (name, colSch) = (text (unpack name) <+>) <$> ppColSchema colSch

ppColSchema :: SQLColumnSchema -> DocAndVals
ppColSchema (SQLColumnSchema type_ constraints) =
    do typeDoc <- ppColType type_
       constraints <- mapM ppConstraint constraints
       return (typeDoc <+> hsep constraints)

ppConstraint :: SQLConstraint -> DocAndVals
ppConstraint SQLPrimaryKey = return (text "PRIMARY KEY")
ppConstraint SQLPrimaryKeyAutoIncrement = return (text "PRIMARY KEY")
ppConstraint SQLAutoIncrement = return (text "AUTOINCREMENT") -- TODO this is different dependingon the backend
ppConstraint SQLNotNull = return (text "NOT NULL")

ppColType :: SqlColDesc -> DocAndVals
ppColType SqlColDesc { colType = SqlVarCharT
                     , colSize = size } =
    return
      (text "VARCHAR" <+>
            case size of
              Nothing -> empty
              Just sz -> parens (text (show sz)))
ppColType SqlColDesc { colType = SqlNumericT } = return (text "INTEGER")
ppColType SqlColDesc { colType = SqlUTCDateTimeT } = return (text "DATETIME")

-- ** Insert printing support

ppInsert :: SQLInsert -> DocAndVals
ppInsert (SQLInsert tblName values) =
    do vals <- mapM ppVal values
       return (text "INSERT INTO" <+> text (unpack tblName) <+> text "VALUES" <+>
               parens (hsep (punctuate comma vals)))

-- ** Update printing support

ppAssignment :: SQLFieldName -> SQLExpr -> DocAndVals
ppAssignment field expr =
    do fieldD <- ppFieldName field
       exprD  <- ppExpr expr
       return (fieldD <> text "=" <> exprD)

ppUpdate :: SQLUpdate -> DocAndVals
ppUpdate (SQLUpdate tbls assignments where_) =
    do assignmentsDs <- mapM (uncurry ppAssignment) assignments
       whereClause_  <- case where_ of
                          Nothing -> return empty
                          Just where_ -> ppWhere where_
       return (text "UPDATE" <+> hsep (punctuate comma (map (text . unpack) tbls)) <+>
               text "SET"    <+> hsep (punctuate comma assignmentsDs) <+>
               whereClause_)

-- ** Select printing support

ppSel :: SQLSelect -> DocAndVals
ppSel sel =
    do proj   <- ppProj (selProjection sel)
       source <- ppAliased ppSource (selFrom sel)
       joins  <- mapM ppJoin (selJoins sel)
       where_ <- ppWhere  (selWhere sel)
       grouping <- case selGrouping sel of
                     Nothing -> return empty
                     Just grouping -> (text "GROUP BY" <+>) <$> ppGrouping grouping
       orderBy <- ppOrderBy (selOrderBy sel)
       let limit = maybe empty ((text "LIMIT" <+>) . text . show) (selLimit sel)
           offset = maybe empty ((text "OFFSET" <+>) . text . show) (selOffset sel)
       return (text "SELECT" <+> proj <+> text "FROM" <+> source <+>
               hsep joins <+> where_ <+> grouping <+> orderBy <+> limit <+> offset)

ppProj :: SQLProjection -> DocAndVals
ppProj SQLProjStar = return (text "*")
ppProj (SQLProjFields fields) =
    do fields <- mapM (ppAliased ppFieldName) fields
       return (hsep (punctuate comma fields))

ppAliased :: (a -> DocAndVals) -> SQLAliased a -> DocAndVals
ppAliased ppSub (SQLAliased x (Just as)) = do sub <- ppSub x
                                              return (sub <+> text "AS" <+> text (unpack as))
ppAliased ppSub (SQLAliased x Nothing) = ppSub x

ppSource :: SQLSource -> DocAndVals
ppSource (SQLSourceTable tbl) = return (text (unpack tbl))
ppSource (SQLSourceSelect sel) = parens <$> ppSel sel

ppWhere (SQLValE v)
    | safeFromSql v == Right True = return empty
ppWhere expr = (text "WHERE" <+>) <$> ppExpr expr

ppFieldName (SQLQualifiedFieldName t table) = return (text "`" <> text (unpack table) <> text "`.`" <>
                                                      text (unpack t) <> text "`")
ppFieldName (SQLFieldName t) = return (text (unpack t))

ppGrouping grouping = do fieldNames <- mapM ppFieldName (sqlGroupBy grouping)
                         having <-  ppHaving (sqlHaving grouping)
                         return (text "GROUP BY" <+> hsep (punctuate comma fieldNames) <+> having)

ppHaving (SQLValE v)
         | safeFromSql v == Right True = return empty
ppHaving expr = (text "HAVING" <+>) <$> ppExpr expr

ppJoin (SQLJoin SQLInnerJoin source on_) = do sourceDoc <- ppAliased ppSource source
                                              onDoc <-  ppOn on_
                                              return (text "INNER JOIN" <+> sourceDoc <+> onDoc)

ppOn (SQLValE v)
     | safeFromSql v == Right True = return empty
ppOn expr = (text "ON" <+>) <$> ppExpr expr

ppOrderBy [] = return empty
ppOrderBy xs = hsep <$> mapM ppOrdering xs
    where ppOrdering (Asc name) = do fieldName <- ppFieldName name
                                     return (fieldName <+> text "ASC")
          ppOrdering (Desc name) = do fieldName <- ppFieldName name
                                      return (fieldName <+> text "DESC")

ppVal :: SqlValue -> DocAndVals
ppVal val = tell [val] >> return (text "?")

ppExpr :: SQLExpr -> DocAndVals
ppExpr (SQLValE v) = ppVal v
ppExpr (SQLJustE v) = ppExpr v
ppExpr (SQLFieldE name) = ppFieldName name
ppExpr (SQLEqE a b) = binOp "==" a b
ppExpr (SQLAndE a b) = binOp "AND" a b
ppExpr (SQLOrE a b) = binOp "OR" a b

binOp :: String -> SQLExpr -> SQLExpr -> DocAndVals
binOp op a b = do aD <- ppExpr a
                  bD <- ppExpr b
                  return (aD <+> text op <+> bD)

-- * SQL statement combinators

conjugateWhere :: SQLSelect -> SQLExpr -> SQLSelect
conjugateWhere sel@(SQLSelect { selWhere = SQLValE (SqlBool True) }) e = sel { selWhere = e}
conjugateWhere sel e = sel { selWhere = SQLAndE (selWhere sel) e }