{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Database.Beam.SQL
    ( -- * SQL pretty printing
      ppSQL

      -- * Untyped SQL types
    , module Database.Beam.SQL.Types ) where

import Database.Beam.SQL.Types

import Control.Arrow hiding ((<+>))
import Control.Monad.Writer hiding ((<>))

import Data.Text (Text, unpack)

import Database.HDBC

import Text.PrettyPrint

-- * Pretty printing support

-- | Convert a 'SQLCommand' into a SQL expression (with placeholders) and literal values to be submitted to the SQL server.
--   Splitting into a SQL expression and literals prevents SQL injection attacks.
ppSQL :: SQLCommand -> (String, [SqlValue])
ppSQL c = first render (runWriter (ppCmd c))

type DocAndVals = Writer [SqlValue] Doc

ppCmd :: SQLCommand -> DocAndVals
ppCmd (Select sel) = ppSel sel
ppCmd (CreateTable ct) = ppCreateTable ct
ppCmd (Insert i) = ppInsert i
ppCmd (Update u) = ppUpdate u
ppCmd (Delete d) = ppDelete d

-- ** Create table printing support

ppCreateTable :: SQLCreateTable -> DocAndVals
ppCreateTable (SQLCreateTable tblName schema) =
    do fieldSchemas <- mapM ppFieldSchema schema
       let primaryKeys = filter (elem SQLPrimaryKey . csConstraints . snd) schema
           primaryKeyExpr = case primaryKeys of
                              [] -> []
                              keys ->
                                  let pks = map (text . unpack . fst) keys
                                  in [text "PRIMARY KEY(" <+> hsep (punctuate comma pks) <+> text ")" ]
       return (text "CREATE TABLE" <+> text (unpack tblName) <+> parens (hsep (punctuate comma (fieldSchemas ++ primaryKeyExpr))))

ppFieldSchema :: (Text, SQLColumnSchema) -> DocAndVals
ppFieldSchema (name, colSch) = (text (unpack name) <+>) <$> ppColSchema colSch

ppColSchema :: SQLColumnSchema -> DocAndVals
ppColSchema (SQLColumnSchema type_ constraints) =
    do typeDoc <- ppColType type_
       constraints' <- mapM ppConstraint constraints
       return (typeDoc <+> hsep constraints')

ppConstraint :: SQLConstraint -> DocAndVals
ppConstraint SQLPrimaryKey = return empty --(text "PRIMARY KEY")
--ppConstraint SQLPrimaryKeyAutoIncrement = return (text "PRIMARY KEY")
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
ppColType SqlColDesc { colType = SqlCharT
                     , colSize = size } =
    return
      (text "CHAR" <+>
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
                          Just x -> ppWhere x
       return (text "UPDATE" <+> hsep (punctuate comma (map (text . unpack) tbls)) <+>
               text "SET"    <+> hsep (punctuate comma assignmentsDs) <+>
               whereClause_)

-- ** Delete printing support

ppDelete :: SQLDelete -> DocAndVals
ppDelete (SQLDelete tbl where_) =
    do whereClause_ <- case where_ of
                         Nothing -> return empty
                         Just x -> ppWhere x
       return (text "DELETE FROM" <+> text (unpack tbl) <+> whereClause_)

-- ** Select printing support

ppSel :: SQLSelect -> DocAndVals
ppSel sel =
    do proj   <- ppProj (selProjection sel)
       source <- case selFrom sel of
                   Nothing -> pure empty
                   Just from -> do source <- ppFrom from
                                   pure (text "FROM " <+> source)
       where_ <- ppWhere  (selWhere sel)
       grouping <- case selGrouping sel of
                     Nothing -> return empty
                     Just grouping -> ppGrouping grouping
       orderBy <- ppOrderBy (selOrderBy sel)
       let limit = maybe empty ((text "LIMIT" <+>) . text . show) (selLimit sel)
           offset = maybe empty ((text "OFFSET" <+>) . text . show) (selOffset sel)
       return (text "SELECT" <+> proj <+> source <+>
               where_ <+> grouping <+> orderBy <+> limit <+> offset)

ppProj :: SQLProjection -> DocAndVals
ppProj SQLProjStar = return (text "*")
ppProj (SQLProj es) =
    do es' <- mapM (ppAliased ppExpr) es
       return (hsep (punctuate comma es'))

ppAliased :: (a -> DocAndVals) -> SQLAliased a -> DocAndVals
ppAliased ppSub (SQLAliased x (Just as)) = do sub <- ppSub x
                                              return (sub <+> text "AS" <+> text (unpack as))
ppAliased ppSub (SQLAliased x Nothing) = ppSub x

ppSource :: SQLSource -> DocAndVals
ppSource (SQLSourceTable tbl) = return (text (unpack tbl))
ppSource (SQLSourceSelect sel) = parens <$> ppSel sel

ppWhere :: SQLExpr' SQLFieldName -> Writer [SqlValue] Doc
ppWhere (SQLValE v)
    | safeFromSql v == Right True = return empty
ppWhere expr = (text "WHERE" <+>) <$> ppExpr expr

ppFieldName :: Applicative f => SQLFieldName -> f Doc
ppFieldName (SQLQualifiedFieldName t table) = pure (text "`" <> text (unpack table) <> text "`.`" <>
                                                     text (unpack t) <> text "`")
ppFieldName (SQLFieldName t) = pure (text (unpack t))

ppGrouping :: SQLGrouping -> Writer [SqlValue] Doc
ppGrouping grouping = do exprs <- mapM ppExpr (sqlGroupBy grouping)
                         having <-  ppHaving (sqlHaving grouping)
                         return (text "GROUP BY" <+> hsep (punctuate comma exprs) <+> having)

ppHaving :: SQLExpr' SQLFieldName -> Writer [SqlValue] Doc
ppHaving (SQLValE v)
         | safeFromSql v == Right True = return empty
ppHaving expr = (text "HAVING" <+>) <$> ppExpr expr

ppFrom :: SQLFrom -> Writer [SqlValue] Doc
ppFrom x = fst <$> ppFrom' x

ppFrom' :: SQLFrom -> Writer [SqlValue] (Doc, Bool)
ppFrom' (SQLFromSource a) = (,False) <$> ppAliased ppSource a
ppFrom' (SQLJoin jType x y on_) = do (xDoc, _) <- ppFrom' x
                                     jTypeDoc <- ppJType jType
                                     (yDoc, yIsJoin) <- ppFrom' y
                                     onDoc <- ppOn on_
                                     return (xDoc <+> jTypeDoc <+> if yIsJoin then yDoc else yDoc <+> onDoc, True)
ppJType :: Applicative f => SQLJoinType -> f Doc
ppJType SQLInnerJoin = pure (text "INNER JOIN")
ppJType SQLLeftJoin =  pure (text "LEFT JOIN")
ppJType SQLRightJoin = pure (text "RIGHT JOIN")
ppJType SQLOuterJoin = pure (text "OUTER JOIN")

ppOn :: SQLExpr' SQLFieldName -> Writer [SqlValue] Doc
ppOn (SQLValE v)
     | safeFromSql v == Right True = return empty
ppOn expr = (text "ON" <+>) <$> ppExpr expr

ppOrderBy :: [SQLOrdering] -> Writer [SqlValue] Doc
ppOrderBy [] = return empty
ppOrderBy xs = (text "ORDER BY" <+>) . hsep . punctuate comma <$> mapM ppOrdering xs
    where ppOrdering (Asc e) = do eDoc <- ppExpr e
                                  return (eDoc <+> text "ASC")
          ppOrdering (Desc e) = do eDoc <- ppExpr e
                                   return (eDoc <+> text "DESC")

ppVal :: SqlValue -> DocAndVals
ppVal val = tell [val] >> return (text "?")

ppExpr :: SQLExpr -> DocAndVals
ppExpr (SQLValE v) = ppVal v
ppExpr (SQLFieldE name) = ppFieldName name
ppExpr (SQLBinOpE op a b) =
    do aD <- ppExpr a
       bD <- ppExpr b
       return (parens (aD <+> text (unpack op) <+> bD))
ppExpr (SQLUnOpE op a) = do aDoc <- ppExpr a
                            return (parens (text (unpack op) <+> parens aDoc))
ppExpr (SQLIsNothingE q) = do qDoc <- ppExpr q
                              return (parens (qDoc <+> text "IS NULL"))
ppExpr (SQLIsJustE q) = do qDoc <- ppExpr q
                           return (parens (qDoc <+> text "IS NOT NULL"))
ppExpr (SQLListE xs) = do xsDoc <- mapM ppExpr xs
                          return (parens (hsep (punctuate comma xsDoc)))
ppExpr (SQLFuncE f args) = do argDocs <- mapM ppExpr args
                              return (text (unpack f) <> parens (hsep (punctuate comma argDocs)) )
ppExpr (SQLExistsE q) = do selectDoc <- ppSel q
                           pure (parens (text "EXISTS (" <> selectDoc <> text ")"))
ppExpr (SQLCaseE clauses else_) = do whenClauses <- forM clauses $ \(cond, then_) ->
                                                    do condDoc <- ppExpr cond
                                                       thenDoc <- ppExpr then_
                                                       pure (text "WHEN" <+> condDoc <+> text "THEN" <+> thenDoc)
                                     elseDoc <- ppExpr else_
                                     pure (parens (text "CASE" <+> hsep whenClauses <+> text "ELSE" <+> elseDoc <+> text "END"))
