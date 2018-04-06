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


-- | Synonym of 'OneToMany'. Useful for giving more meaningful types, when the
--   relationship is meant to be one-to-one.
type OneToOne db s one many = OneToMany db s one many

-- | Convenience type to declare one-to-many relationships. See the manual
--   section on
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type OneToMany db s one many =
  HasTableEquality (PrimaryKey one) => one (QExpr s) -> Q db s (many (QExpr s))

-- | Synonym of 'OneToManyOptional'. Useful for giving more meaningful types,
--   when the relationship is meant to be one-to-one.
type OneToMaybe db s tbl rel = OneToManyOptional db s tbl rel

-- | Convenience type to declare one-to-many relationships with a nullable
--   foreign key. See the manual section on
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type OneToManyOptional db s tbl rel =
  HasTableEqualityNullable (PrimaryKey tbl) => tbl (QExpr s) -> Q db s (rel (Nullable (QExpr s)))

-- | Used to define one-to-many (or one-to-one) relationships. Takes the table
--   to fetch, a way to extract the foreign key from that table, and the table to
--   relate to.
oneToMany_, oneToOne_
  :: ( Database db
     , HasTableEquality (PrimaryKey tbl)
     , Table tbl, Table rel )
  => DatabaseEntity db (TableEntity rel) {-^ Table to fetch (many) -}
  -> (rel (QExpr s) -> PrimaryKey tbl (QExpr s))
     {-^ Foreign key -}
  -> tbl (QExpr s)
  -> Q db s (rel (QExpr s))
oneToMany_ rel getKey tbl =
  join_ rel (\rel' -> getKey rel' ==. pk tbl)
oneToOne_ = oneToMany_

-- | Used to define one-to-many (or one-to-one) relationships with a nullable
--   foreign key. Takes the table to fetch, a way to extract the foreign key
--   from that table, and the table to relate to.
oneToManyOptional_, oneToMaybe_
  :: ( HasTableEqualityNullable (PrimaryKey tbl)
     , Database db
     , Table tbl, Table rel )
  => DatabaseEntity db (TableEntity rel) {-^ Table to fetch -}
  -> (rel (QExpr s) -> PrimaryKey tbl (Nullable (QExpr s)))
     {-^ Foreign key -}
  -> tbl (QExpr s)
  -> Q db s (rel (Nullable (QExpr s)))
oneToManyOptional_ rel getKey tbl =
  leftJoin_ (all_ rel) (\rel' -> getKey rel' ==. just_ (pk tbl))
oneToMaybe_ = oneToManyOptional_

-- ** Many-to-many relationships

-- | Convenience type to declare many-to-many relationships. See the manual
--   section on
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type ManyToMany db left right =
  forall s.
  ( SqlEq (QExpr s) (PrimaryKey left (QExpr s))
  , SqlEq (QExpr s) (PrimaryKey right (QExpr s)) ) =>
  Q db s (left (QExpr s)) -> Q db s (right (QExpr s)) ->
  Q db s (left (QExpr s), right (QExpr s))

-- | Convenience type to declare many-to-many relationships with additional
--   data. See the manual section on
--   <http://tathougies.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type ManyToManyThrough db through left right =
  forall s.
  ( SqlEq (QExpr s) (PrimaryKey left (QExpr s))
  , SqlEq (QExpr s) (PrimaryKey right (QExpr s)) ) =>
  Q db s (left (QExpr s)) -> Q db s (right (QExpr s)) ->
  Q db s ( through (QExpr s)
         , left (QExpr s)
         , right (QExpr s))

-- | Used to define many-to-many relationships without any additional data.
--   Takes the join table and two key extraction functions from that table to the
--   related tables. Also takes two `Q`s representing the table sources to relate.
--
--   See <http://tathougies.github.io/beam/user-guide/queries/relationships/ the manual>
--   for more indformation.
manyToMany_
  :: ( Database db, Table joinThrough
     , Table left, Table right
     , SqlEq (QExpr s) (PrimaryKey left (QExpr s))
     , SqlEq (QExpr s) (PrimaryKey right (QExpr s)) )
  => DatabaseEntity db (TableEntity joinThrough)
  -> (joinThrough (QExpr s) -> PrimaryKey left (QExpr s))
  -> (joinThrough (QExpr s) -> PrimaryKey right (QExpr s))
  -> Q db s (left (QExpr s)) -> Q db s (right (QExpr s))
  -> Q db s (left (QExpr s), right (QExpr s))
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
     , SqlEq (QExpr s) (PrimaryKey left (QExpr s))
     , SqlEq (QExpr s) (PrimaryKey right (QExpr s)) )
  => DatabaseEntity db (TableEntity joinThrough)
  -> (joinThrough (QExpr s) -> PrimaryKey left (QExpr s))
  -> (joinThrough (QExpr s) -> PrimaryKey right (QExpr s))
  -> Q db s (left (QExpr s))
  -> Q db s (right (QExpr s))
  -> Q db s ( joinThrough (QExpr s)
            , left (QExpr s)
            , right (QExpr s))
manyToManyPassthrough_ joinTbl leftKey rightKey left right =
  do left_ <- left
     right_ <- right
     joinTbl_ <- join_ joinTbl (\joinTbl_ -> leftKey joinTbl_ ==. primaryKey left_ &&.
                                             rightKey joinTbl_ ==. primaryKey right_)
     pure (joinTbl_, left_, right_)



