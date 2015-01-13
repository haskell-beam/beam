{-# LANGUAGE GADTs #-}
module Database.Beam.SQL
    ( module Database.Beam.SQL.Types

    , ppSQL

    , conjugateWhere) where

import Database.Beam.Types
import Database.Beam.SQL.Types

import Data.Maybe
import Data.Text (Text, unpack)

import Database.HDBC

import Text.PrettyPrint

-- * Pretty printing support
ppSQL :: SQLCommand -> String
ppSQL c = render (ppCmd c)

ppCmd :: SQLCommand -> Doc
ppCmd (Select sel) = ppSel sel
ppCmd (CreateTableCmd ct) = ppCreateTable ct
ppCmd (Insert i) = ppInsert i

-- ** Create table printing support

ppCreateTable :: SQLCreateTable -> Doc
ppCreateTable (SQLCreateTable tblName schema) =
    text "CREATE TABLE" <+> text (unpack tblName) <+> parens (hsep (punctuate comma (map ppFieldSchema schema)))

ppFieldSchema :: (Text, SQLColumnSchema) -> Doc
ppFieldSchema (name, colSch) = text (unpack name) <+> ppColSchema colSch

ppColSchema :: SQLColumnSchema -> Doc
ppColSchema (SQLColumnSchema type_ constraints) = ppColType type_ <+> hsep (map ppConstraint constraints)

ppConstraint :: SQLConstraint -> Doc
ppConstraint SQLPrimaryKey = text "PRIMARY KEY"

ppColType :: SqlColDesc -> Doc
ppColType SqlColDesc { colType = SqlVarCharT
                     , colSize = size } =
    text "VARCHAR" <+>
         case size of
           Nothing -> empty
           Just sz -> parens (text (show sz))
ppColType SqlColDesc { colType = SqlNumericT } = text "INTEGER"
ppColType SqlColDesc { colType = SqlUTCDateTimeT } = text "DATETIME"

-- ** Insert printing support

ppInsert :: SQLInsert -> Doc
ppInsert (SQLInsert tblName values) = text "INSERT INTO" <+> text (unpack tblName) <+> text "VALUES" <+>
                                      parens (hsep (punctuate comma (map ppVal values)))

-- ** Select printing support

ppSel :: SQLSelect -> Doc
ppSel sel = text "SELECT" <+>
            ppProj (selProjection sel) <+>
            text "FROM" <+>
            ppAliased ppSource (selFrom sel) <+>
            hsep (map ppJoin (selJoins sel)) <+>
            ppWhere  (selWhere sel) <+>
            maybe empty ((text "GROUP BY" <+>). ppGrouping) (selGrouping sel) <+>
            ppOrderBy (selOrderBy sel) <+>
            maybe empty ((text "LIMIT" <+>) . text . show) (selLimit sel) <+>
            maybe empty ((text "OFFSET" <+>) . text . show) (selOffset sel)

ppProj :: SQLProjection -> Doc
ppProj SQLProjStar = text "*"
ppProj (SQLProjFields fields) = hsep (punctuate comma (map (ppAliased ppFieldName) fields))

ppAliased :: (a -> Doc) -> SQLAliased a -> Doc
ppAliased ppSub (SQLAliased x (Just as)) = ppSub x <+> text "AS" <+> text (unpack as)
ppAliased ppSub (SQLAliased x Nothing) = ppSub x

ppSource :: SQLSource -> Doc
ppSource (SQLSourceTable tbl) = text (unpack tbl)
ppSource (SQLSourceSelect sel) = parens (ppSel sel)

ppWhere (SQLValE v)
    | toSQLVal v == SQLBoolean True = empty
ppWhere expr = text "WHERE" <+> ppExpr expr

ppFieldName (SQLQualifiedFieldName t table) = text "`" <> text (unpack table) <> text "`.`" <>
                                              text (unpack t) <> text "`"
ppFieldName (SQLFieldName t) = text (unpack t)

ppGrouping grouping = text "GROUP BY" <+> hsep (punctuate comma (map ppFieldName (sqlGroupBy grouping))) <+> ppHaving (sqlHaving grouping)

ppHaving (SQLValE v)
         | toSQLVal v == SQLBoolean True = empty
ppHaving expr = text "HAVING" <+> ppExpr expr

ppJoin (SQLJoin SQLInnerJoin source on_) = text "INNER JOIN" <+> ppAliased ppSource source <+> ppOn on_

ppOn (SQLValE v)
     | toSQLVal v == SQLBoolean True = empty
ppOn expr = text "ON" <+> ppExpr expr

ppOrderBy [] = empty
ppOrderBy xs = hsep (map ppOrdering xs)
    where ppOrdering (Asc name) = ppFieldName name <+> text "ASC"
          ppOrdering (Desc name) = ppFieldName name <+> text "DESC"

ppVal (SQLInt i) = text (show i)
ppVal (SQLFloat f) = text (show f)
ppVal (SQLText t) = text (show (unpack t))
ppVal (SQLBoolean True) = text "1"
ppVal (SQLBoolean False) = text "0"
ppVal SQLNull = text "NULL"

ppExpr :: SQLExpr a -> Doc
ppExpr (SQLValE v) = ppVal (toSQLVal v)
ppExpr (SQLFieldE name) = ppFieldName name
ppExpr (SQLEqE a b) = ppExpr a <+> text "==" <+> ppExpr b
ppExpr (SQLAndE a b) = ppExpr a <+> text " AND " <+> ppExpr b

-- * SQL statement combinators

conjugateWhere :: SQLSelect -> SQLExpr Bool -> SQLSelect
conjugateWhere sel e = sel { selWhere = SQLAndE (selWhere sel) e }