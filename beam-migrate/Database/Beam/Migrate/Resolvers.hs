{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Database.Beam.Migrate.Resolvers where

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.Checks

import Control.Monad

import Data.Typeable
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Foldable

import Debug.Trace

newtype Resolver m a =
  Resolver (forall b.
             [ (b, SomeDatabasePredicate) ] ->
             [ (b, SomeDatabasePredicate) ] ->
             m ( [ (b, SomeDatabasePredicate) ], a ))

runResolver :: Resolver m a
            -> [ (b, SomeDatabasePredicate) ]
            -> [ (b, SomeDatabasePredicate) ]
            -> m ( [ (b, SomeDatabasePredicate) ], a )
runResolver (Resolver f) = f

instance (Monad m, Monoid a) => Monoid (Resolver m a) where
  mempty = Resolver (\_ _ -> pure ( [], mempty) )
  mappend (Resolver a) (Resolver b) =
    Resolver $ \roots context ->
    do (aSolved, aRes) <- a roots context
       (bSolved, bRes) <- b roots context
       let aSolvedCnt = length aSolved
           bSolvedCnt = length bSolved

       return $ if aSolvedCnt > bSolvedCnt then (aSolved, aRes) else (bSolved, bRes)

matchRoot :: forall m a p.
             (Monad m, Monoid a, Typeable p, DatabasePredicate p) =>
             (p -> SomeDatabasePredicate -> Bool)
          -> (forall b. p -> [ (b, SomeDatabasePredicate) ] -> m ( [ (b, SomeDatabasePredicate)], a ))
          -> Resolver m a
matchRoot filterCtxt resolve =
  Resolver $ \roots ctxt ->
  case mapMaybe (\(n, SomeDatabasePredicate root) -> (n,) <$> cast root) roots of
    [] -> trace ("NO roots matched: " ++ show (map (\(_, SomeDatabasePredicate p) -> typeOf p) roots) ++ " (wanted" ++ show (typeOf (undefined :: p)) ++ ")") $ pure ( [], mempty )
    (n, root):_ ->
      do let ctxt' = filter (filterCtxt root . snd) ctxt
         (resolved, res) <- resolve root ctxt'
         pure ((n, SomeDatabasePredicate root):resolved, res)

tryPredicate :: Typeable p => (p -> a) -> SomeDatabasePredicate -> Maybe a
tryPredicate f (SomeDatabasePredicate p) = f <$> cast p

createTableResolver :: forall cmd. Sql92SaneDdlCommandSyntax cmd => Resolver IO [cmd]
createTableResolver =
  matchRoot (\(TableExistsPredicate creatingTbl) ->
                checkAny [ checkPredicate (\(TableHasColumn tbl _ _ :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd)) -> creatingTbl == tbl)
                         , checkPredicate (\(TableColumnHasConstraint tbl _ _ :: TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd)) -> creatingTbl == tbl)
                         , checkPredicate (\(TableHasPrimaryKey tbl _) -> creatingTbl == tbl) ] ) $
  \(TableExistsPredicate creatingTbl) ctxt ->
    let cmd = createTableSyntax Nothing creatingTbl cols cs
        cols = mapMaybe mkCol ctxt
        mkCol (_, SomeDatabasePredicate p) =
          case cast p of
            Nothing -> Nothing
            Just (TableHasColumn _ nm schema :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd)) ->
              Just (nm, columnSchemaSyntax schema Nothing (fieldConstraints nm) Nothing)

        cs = foldMap mkConstraint ctxt
        mkConstraint (_, p) =
          maybeToList . asum . fmap ($p) $
          [ tryPredicate (\(TableHasPrimaryKey _ cols) -> primaryKeyConstraintSyntax cols) ]

        fieldConstraints fieldNm = foldMap (mkFieldConstraint fieldNm) ctxt
        mkFieldConstraint fieldNm (_, p) =
          maybeToList . asum . fmap (join . ($p)) $
          [ tryPredicate (\(TableColumnHasConstraint _ fieldNm' cs :: TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd)) ->
                            guard (fieldNm == fieldNm') >> Just cs) ]

    in pure (ctxt, [createTableCmd cmd])

addColumnResolver :: forall cmd. Sql92SaneDdlCommandSyntax cmd =>
                     Resolver IO [cmd]
addColumnResolver =
  matchRoot (\_ _ -> False) $
  \(TableHasColumn tbl colNm colType :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd)) _ ->
    pure ([], []) --"Alter table " ++ show tbl ++ " to add column " ++ show colNm ++ " with type " ++ show colType])

addConstraintResolver :: forall cmd. Sql92SaneDdlCommandSyntax cmd =>
                         Resolver IO [cmd]
addConstraintResolver =
  matchRoot (\_ _ -> False) $
  \(TableColumnHasConstraint tbl nm constraint :: TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd)) ctxt ->
    -- TODO
    let cmd = alterTableCmd (alterTableSyntax tbl (alterColumnSyntax nm setNotNullSyntax))
    in pure (ctxt, [ cmd ])

makeTrueResolver :: Sql92SaneDdlCommandSyntax cmd =>
                    Resolver IO [cmd]
makeTrueResolver = addColumnResolver <>
                   createTableResolver <>
                   addConstraintResolver
