-- | Combinators and types specific to relationships.
--
--   These types and functions correspond with the relationships section in the
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ user guide>.
module Database.Beam.Query.Relationships
  ( -- * Relationships

    -- ** Many-to-many relationships
    ManyToMany, ManyToManyThrough
  , manyToMany_, manyToManyPassthrough_

    -- ** One-to-many relationships
  , OneToMany, OneToManyOptional
  , oneToMany_, oneToManyOptional_

    -- ** One-to-one relationshships
  , OneToOne, OneToMaybe
  , oneToOne_, oneToMaybe_ ) where

import Database.Beam.Query.Combinators
import Database.Beam.Query.Operator
import Database.Beam.Query.Internal
import Database.Beam.Query.Ord

import Database.Beam.Schema

import Database.Beam.Backend.SQL


-- | Synonym of 'OneToMany'. Useful for giving more meaningful types, when the
--   relationship is meant to be one-to-one.
type OneToOne db s one many = OneToMany db s one many

-- | Convenience type to declare one-to-many relationships. See the manual
--   section on
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type OneToMany db s one many =
  forall syntax.
  ( IsSql92SelectSyntax syntax
  , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) Bool ) =>
  one (QExpr (Sql92SelectExpressionSyntax syntax) s) ->
  Q syntax db s (many (QExpr (Sql92SelectExpressionSyntax syntax) s))

-- | Synonym of 'OneToManyOptional'. Useful for giving more meaningful types,
--   when the relationship is meant to be one-to-one.
type OneToMaybe db s tbl rel = OneToManyOptional db s tbl rel

-- | Convenience type to declare one-to-many relationships with a nullable
--   foreign key. See the manual section on
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type OneToManyOptional db s tbl rel =
  forall syntax.
  ( IsSql92SelectSyntax syntax
  , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) Bool
  , HasSqlValueSyntax (Sql92ExpressionValueSyntax (Sql92SelectExpressionSyntax syntax)) SqlNull ) =>
  tbl (QExpr (Sql92SelectExpressionSyntax syntax) s) ->
  Q syntax db s (rel (Nullable (QExpr (Sql92SelectExpressionSyntax syntax) s)))

-- | Used to define one-to-many (or one-to-one) relationships. Takes the table
--   to fetch, a way to extract the foreign key from that table, and the table to
--   relate to.
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
  join_ rel (\rel' -> getKey rel' ==. pk tbl)
oneToOne_ = oneToMany_

-- | Used to define one-to-many (or one-to-one) relationships with a nullable
--   foreign key. Takes the table to fetch, a way to extract the foreign key
--   from that table, and the table to relate to.
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
  leftJoin_ (all_ rel) (\rel' -> getKey rel' ==. just_ (pk tbl))
oneToMaybe_ = oneToManyOptional_

-- ** Many-to-many relationships

-- | Convenience type to declare many-to-many relationships. See the manual
--   section on
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type ManyToMany db left right =
  forall syntax s.
  ( Sql92SelectSanityCheck syntax, IsSql92SelectSyntax syntax

  , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) ) =>
  Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s)) -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s)) ->
  Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s), right (QExpr (Sql92SelectExpressionSyntax syntax) s))

-- | Convenience type to declare many-to-many relationships with additional
--   data. See the manual section on
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type ManyToManyThrough db through left right =
  forall syntax s.
  ( Sql92SelectSanityCheck syntax, IsSql92SelectSyntax syntax

  , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey left (QExpr (Sql92SelectExpressionSyntax syntax) s))
  , SqlEq (QExpr (Sql92SelectExpressionSyntax syntax) s) (PrimaryKey right (QExpr (Sql92SelectExpressionSyntax syntax) s)) ) =>
  Q syntax db s (left (QExpr (Sql92SelectExpressionSyntax syntax) s)) -> Q syntax db s (right (QExpr (Sql92SelectExpressionSyntax syntax) s)) ->
  Q syntax db s ( through (QExpr (Sql92SelectExpressionSyntax syntax) s)
                , left (QExpr (Sql92SelectExpressionSyntax syntax) s)
                , right (QExpr (Sql92SelectExpressionSyntax syntax) s))

-- | Used to define many-to-many relationships without any additional data.
--   Takes the join table and two key extraction functions from that table to the
--   related tables. Also takes two `Q`s representing the table sources to relate.
--
--   See <http://tathougies.github.io/beam/user-guide/queries/relationships/ the manual>
--   for more indformation.
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
manyToMany_ joinTbl leftKey rightKey left right = fmap (\(_, l, r) -> (l, r)) $
                                                  manyToManyPassthrough_ joinTbl leftKey rightKey left right

-- | Used to define many-to-many relationships with additional data. Takes the
--   join table and two key extraction functions from that table to the related
--   tables. Also takes two `Q`s representing the table sources to relate.
--
--   See <http://tathougies.github.io/beam/user-guide/queries/relationships/ the manual>
--   for more indformation.
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



