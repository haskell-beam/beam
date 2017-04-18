{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Database.Beam.Migrate.Resolvers where

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.Checks

import Data.Typeable
import Data.Maybe
import Data.Monoid

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

checkPredicate :: Typeable p => (p -> Bool)
               -> SomeDatabasePredicate -> Bool
checkPredicate check (SomeDatabasePredicate p) =
  case cast p of
    Nothing -> False
    Just p -> check p

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

createTableResolver :: forall columnSchemaSyntax. Typeable columnSchemaSyntax => Resolver IO [String]
createTableResolver =
  matchRoot (\(TableExistsPredicate creatingTbl) -> checkPredicate (\(TableHasColumn tbl _ _ :: TableHasColumn columnSchemaSyntax) -> creatingTbl == tbl)) $
  \(TableExistsPredicate creatingTbl) ctxt ->
    pure (ctxt, ["Create table " ++ show creatingTbl])

addColumnResolver :: forall columnSchemaSyntax.
                     ( Show (Sql92ColumnSchemaColumnTypeSyntax columnSchemaSyntax)
                     , Eq (Sql92ColumnSchemaColumnTypeSyntax columnSchemaSyntax)
                     , Typeable columnSchemaSyntax) =>
                     Resolver IO [String]
addColumnResolver =
  matchRoot (\_ _ -> False) $
  \(TableHasColumn tbl colNm colType :: TableHasColumn columnSchemaSyntax) _ ->
    pure ([], ["Alter table " ++ show tbl ++ " to add column " ++ show colNm ++ " with type " ++ show colType])

makeTrueResolver :: forall columnSchemaSyntax.
                    ( Show (Sql92ColumnSchemaColumnTypeSyntax columnSchemaSyntax)
                    , Eq (Sql92ColumnSchemaColumnTypeSyntax columnSchemaSyntax)
                    , Typeable columnSchemaSyntax ) =>
                    Resolver IO [String]
makeTrueResolver = addColumnResolver @columnSchemaSyntax <>
                   createTableResolver @columnSchemaSyntax
