-- | Combinators and types specific to relationships.
--
--   These types and functions correspond with the relationships section in the
--   <https://haskell-beam.github.io/beam/user-guide/queries/relationships/ user guide>.
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
type OneToOne be db s one many = OneToMany be db s one many

-- | Convenience type to declare one-to-many relationships. See the manual
--   section on
--   <https://haskell-beam.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type OneToMany be db s one many =
  ( BeamSqlBackend be, BeamSqlBackendCanSerialize be Bool ) =>
  one (QExpr be s) -> Q be db s (many (QExpr be s))

-- | Synonym of 'OneToManyOptional'. Useful for giving more meaningful types,
--   when the relationship is meant to be one-to-one.
type OneToMaybe be db s tbl rel = OneToManyOptional be db s tbl rel

-- | Convenience type to declare one-to-many relationships with a nullable
--   foreign key. See the manual section on
--   <https://haskell-beam.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type OneToManyOptional be db s tbl rel =
  ( BeamSqlBackend be, BeamSqlBackendCanSerialize be Bool
  , BeamSqlBackendCanSerialize be SqlNull ) =>
  tbl (QExpr be s) -> Q be db s (rel (Nullable (QExpr be s)))

-- | Used to define one-to-many (or one-to-one) relationships. Takes the table
--   to fetch, a way to extract the foreign key from that table, and the table to
--   relate to.
oneToMany_, oneToOne_
  :: ( Database be db, BeamSqlBackend be
     , HasTableEquality be (PrimaryKey tbl)
     , Table tbl, Table rel )
  => DatabaseEntity be db (TableEntity rel) {-^ Table to fetch (many) -}
  -> (rel (QExpr be s) -> PrimaryKey tbl (QExpr be s))
     {-^ Foreign key -}
  -> tbl (QExpr be s)
  -> Q be db s (rel (QExpr be s))
oneToMany_ rel getKey tbl =
  join_ rel (\rel' -> getKey rel' ==. pk tbl)
oneToOne_ = oneToMany_

-- | Used to define one-to-many (or one-to-one) relationships with a nullable
--   foreign key. Takes the table to fetch, a way to extract the foreign key
--   from that table, and the table to relate to.
oneToManyOptional_, oneToMaybe_
  :: ( BeamSqlBackend be, Database be db
     , Table tbl, Table rel
     , HasTableEqualityNullable be (PrimaryKey tbl) )
  => DatabaseEntity be db (TableEntity rel) {-^ Table to fetch -}
  -> (rel (QExpr be s) -> PrimaryKey tbl (Nullable (QExpr be s)))
     {-^ Foreign key -}
  -> tbl (QExpr be s)
  -> Q be db s (rel (Nullable (QExpr be s)))
oneToManyOptional_ rel getKey tbl =
  leftJoin_ (all_ rel) (\rel' -> getKey rel' ==. just_ (pk tbl))
oneToMaybe_ = oneToManyOptional_

-- ** Many-to-many relationships

-- | Convenience type to declare many-to-many relationships. See the manual
--   section on
--   <https://haskell-beam.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type ManyToMany be db left right =
  forall s.
  ( BeamSqlBackend be

  , SqlEq (QExpr be s) (PrimaryKey left (QExpr be s))
  , SqlEq (QExpr be s) (PrimaryKey right (QExpr be s)) ) =>
  Q be db s (left (QExpr be s)) -> Q be db s (right (QExpr be s)) ->
  Q be db s (left (QExpr be s), right (QExpr be s))

-- | Convenience type to declare many-to-many relationships with additional
--   data. See the manual section on
--   <https://haskell-beam.github.io/beam/user-guide/queries/relationships/ relationships>
--   for more information
type ManyToManyThrough be db through left right =
  forall s.
  ( BeamSqlBackend be

  , SqlEq (QExpr be s) (PrimaryKey left (QExpr be s))
  , SqlEq (QExpr be s) (PrimaryKey right (QExpr be s)) ) =>
  Q be db s (left (QExpr be s)) -> Q be db s (right (QExpr be s)) ->
  Q be db s ( through (QExpr be s), left (QExpr be s), right (QExpr be s) )

-- | Used to define many-to-many relationships without any additional data.
--   Takes the join table and two key extraction functions from that table to the
--   related tables. Also takes two `Q`s representing the table sources to relate.
--
--   See <https://haskell-beam.github.io/beam/user-guide/queries/relationships/ the manual>
--   for more information.
manyToMany_
  :: ( Database be db
     , Table joinThrough, Table left, Table right
     , BeamSqlBackend be

     , SqlEq (QExpr be s) (PrimaryKey left (QExpr be s))
     , SqlEq (QExpr be s) (PrimaryKey right (QExpr be s)) )
  => DatabaseEntity be db (TableEntity joinThrough)
  -> (joinThrough (QExpr be s) -> PrimaryKey left (QExpr be s))
  -> (joinThrough (QExpr be s) -> PrimaryKey right (QExpr be s))
  -> Q be db s (left (QExpr be s)) -> Q be db s (right (QExpr be s))
  -> Q be db s (left (QExpr be s), right (QExpr be s))
manyToMany_ joinTbl leftKey rightKey left right = fmap (\(_, l, r) -> (l, r)) $
                                                  manyToManyPassthrough_ joinTbl leftKey rightKey left right

-- | Used to define many-to-many relationships with additional data. Takes the
--   join table and two key extraction functions from that table to the related
--   tables. Also takes two `Q`s representing the table sources to relate.
--
--   See <https://haskell-beam.github.io/beam/user-guide/queries/relationships/ the manual>
--   for more information.
manyToManyPassthrough_
  :: ( Database be db
     , Table joinThrough, Table left, Table right

     , BeamSqlBackend be

     , SqlEq (QExpr be s) (PrimaryKey left (QExpr be s))
     , SqlEq (QExpr be s) (PrimaryKey right (QExpr be s)) )
  => DatabaseEntity be db (TableEntity joinThrough)
  -> (joinThrough (QExpr be s) -> PrimaryKey left (QExpr be s))
  -> (joinThrough (QExpr be s) -> PrimaryKey right (QExpr be s))
  -> Q be db s (left (QExpr be s))
  -> Q be db s (right (QExpr be s))
  -> Q be db s ( joinThrough (QExpr be s)
               , left (QExpr be s)
               , right (QExpr be s))
manyToManyPassthrough_ joinTbl leftKey rightKey left right =
  do left_ <- left
     right_ <- right
     joinTbl_ <- join_ joinTbl (\joinTbl_ -> leftKey joinTbl_ ==. primaryKey left_ &&.
                                             rightKey joinTbl_ ==. primaryKey right_)
     pure (joinTbl_, left_, right_)



