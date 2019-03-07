{-# LANGUAGE OverloadedStrings #-}

-- | Postgres-specific types, functions, and operators
module Database.Beam.Sqlite.SqliteSpecific
    ( -- * Sqlite functions and aggregates
      sqliteGroupConcat
    , sqliteGroupConcatOver
    )
where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Query.Internal
import           Database.Beam.Sqlite.Syntax
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

-- | The SQLite @group_concat@ function.
-- Joins the value in each row of the first argument, using the second
-- argument as a delimiter. See 'sqliteGroupConcatOver' if you want to provide
-- explicit quantification.
sqliteGroupConcat 
    :: ( HasSqlValueSyntax SqliteValueSyntax a
       , IsSqlExpressionSyntaxStringType SqliteExpressionSyntax str 
       , IsSqlExpressionSyntaxStringType SqliteExpressionSyntax str2
       )
    => QExpr SqliteExpressionSyntax s a
    -> QExpr SqliteExpressionSyntax s str
    -> QAgg SqliteExpressionSyntax s (Maybe str2)
sqliteGroupConcat v delim = _sqliteGroupConcatOver allInGroup_ v (Just delim)


-- | The SQLite @group_concat@ function.
-- Joins the value in each row of the first argument using ','.
-- See 'sqliteGroupConcat' if you want to change the delimiter.
-- Choosing a custom delimiter and quantification isn't allowed by SQLite.
sqliteGroupConcatOver 
    :: ( HasSqlValueSyntax SqliteValueSyntax a
        , IsSqlExpressionSyntaxStringType SqliteExpressionSyntax str 
        )
    => Maybe SqliteAggregationSetQuantifierSyntax
    -> QExpr SqliteExpressionSyntax s a
    -> QAgg SqliteExpressionSyntax s (Maybe str)
sqliteGroupConcatOver quantifier v = _sqliteGroupConcatOver quantifier v Nothing

-- SQLite doesn't allow DISTINCT and a custom delimiter
_sqliteGroupConcatOver 
    :: ( HasSqlValueSyntax SqliteValueSyntax a
        , IsSqlExpressionSyntaxStringType SqliteExpressionSyntax str
        )
    => Maybe SqliteAggregationSetQuantifierSyntax
    -> QExpr SqliteExpressionSyntax s a
    -> Maybe (QExpr SqliteExpressionSyntax s str2)
    -> QAgg SqliteExpressionSyntax s (Maybe str)
_sqliteGroupConcatOver quantifier (QExpr v) delim =
    QExpr $ \tbl -> SqliteExpressionSyntax $
    emit "group_concat" <>
    parens ( maybe mempty (\q -> fromSqliteAggregationSetQuantifier q <> emit " ") quantifier <>
             fromSqliteExpression (v tbl) <> 
             maybe mempty (\(QExpr d) -> emit ", " <> fromSqliteExpression (d tbl)) delim)