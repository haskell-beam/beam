-- | Combinators and types specific to relationships.
--
--   These types and functions correspond with the relationships section in the
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ user guide>.
module Database.Beam.Query.Relationships
  ( ManyToMany, ManyToManyThrough
  , manyToMany_, manyToManyPassthrough_

  , OneToMany, OneToManyOptional
  , oneToMany_, oneToManyOptional_

  , OneToOne, OneToMaybe
  , oneToOne_, oneToMaybe_ ) where

import Database.Beam.Query.Combinators
import Database.Beam.Query.Operator
import Database.Beam.Query.Internal
import Database.Beam.Query.Ord

import Database.Beam.Schema

import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.SQL92

type OneToOne db s one many = OneToMany db s one many
type OneToMany db s one many =
  forall syntax.
  ( IsSql92SelectSyntax syntax
  , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) Bool ) =>
  one (QExpr (Sql92SelectExpressionSyntax syntax) s) ->
  Q syntax db s (many (QExpr (Sql92SelectExpressionSyntax syntax) s))

type OneToMaybe db s tbl rel = OneToManyOptional db s tbl rel
type OneToManyOptional db s tbl rel =
  forall syntax.
  ( IsSql92SelectSyntax syntax
  , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) Bool
  , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) SqlNull ) =>
  tbl (QExpr (Sql92SelectExpressionSyntax syntax) s) ->
  Q syntax db s (rel (Nullable (QExpr (Sql92SelectExpressionSyntax syntax) s)))

oneToMany_, oneToOne_
  :: ( IsSql92SelectSyntax syntax
     , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) Bool
     , Database db
     , Table tbl, Table rel )
  => DatabaseEntity be db (TableEntity rel) {-^ Table to fetch (many) -}
  -> (rel (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey tbl (QExpr (Sql92SelectExpressionSyntax syntax) s))
     {-^ Foreign key -}
  -> tbl (QExpr (Sql92SelectExpressionSyntax syntax) s)
  -> Q syntax db s (rel (QExpr (Sql92SelectExpressionSyntax syntax) s))
oneToMany_ rel getKey tbl =
  join_ rel (\rel -> getKey rel ==. pk tbl)
oneToOne_ = oneToMany_

oneToManyOptional_, oneToMaybe_
  :: ( IsSql92SelectSyntax syntax
     , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) Bool
     , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) SqlNull
     , Database db
     , Table tbl, Table rel )
  => DatabaseEntity be db (TableEntity rel) {-^ Table to fetch -}
  -> (rel (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey tbl (Nullable (QExpr (Sql92SelectExpressionSyntax syntax) s)))
     {-^ Foreign key -}
  -> tbl (QExpr (Sql92SelectExpressionSyntax syntax) s)
  -> Q syntax db s (rel (Nullable (QExpr (Sql92SelectExpressionSyntax syntax) s)))
oneToManyOptional_ rel getKey tbl =
  leftJoin_ rel (\rel -> getKey rel ==. just_ (pk tbl))
oneToMaybe_ = oneToManyOptional_

-- ** Many-to-many relationships

type ManyToMany db left right =
  forall syntax g h s.
  ( Sql92SelectSanityCheck syntax, IsSql92SelectSyntax syntax

  , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) ) =>
  Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s)) -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s)) ->
  Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s), right (QExpr (Sql92SelectExpressionSyntax syntax) s))
type ManyToManyThrough db through left right =
  forall syntax g h s.
  ( Sql92SelectSanityCheck syntax, IsSql92SelectSyntax syntax

  , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) ) =>
  Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s)) -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s)) ->
  Q syntax db s ( through (QExpr (Sql92SelectExpressionSyntax syntax) s)
                , left (QExpr (Sql92SelectExpressionSyntax syntax) s)
                , right (QExpr (Sql92SelectExpressionSyntax syntax) s))

manyToMany_
  :: ( Database db, Table joinThrough
     , Table left, Table right
     , Sql92SelectSanityCheck syntax
     , IsSql92SelectSyntax syntax

     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) )
  => DatabaseEntity be db (TableEntity joinThrough)
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s)) -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s), right (QExpr (Sql92SelectExpressionSyntax syntax) s))
manyToMany_ joinTbl leftKey rightKey left right = fmap (\(_, left, right) -> (left, right)) $
                                                  manyToManyPassthrough_ joinTbl leftKey rightKey left right

manyToManyPassthrough_
  :: ( Database db, Table joinThrough
     , Table left, Table right
     , Sql92SelectSanityCheck syntax
     , IsSql92SelectSyntax syntax

     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
     , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) )
  => DatabaseEntity be db (TableEntity joinThrough)
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> (joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s) -> PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s))
  -> Q syntax db s ( joinThrough (QExpr (Sql92SelectExpressionSyntax syntax) s)
                   , left (QExpr (Sql92SelectExpressionSyntax syntax) s)
                   , right (QExpr (Sql92SelectExpressionSyntax syntax) s))
manyToManyPassthrough_ joinTbl leftKey rightKey left right =
  do left_ <- left
     right_ <- right
     joinTbl_ <- join_ joinTbl (\joinTbl_ -> leftKey joinTbl_ ==. primaryKey left_ &&.
                                             rightKey joinTbl_ ==. primaryKey right_)
     pure (joinTbl_, left_, right_)



