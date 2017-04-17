{-# LANGUAGE TupleSections #-}
module Database.Beam.Migrate.Diff where

import           Database.Beam.Migrate.Types

import           Control.Monad

import           Data.Graph.Inductive.Query.DFS (components)
import           Data.Graph.Inductive (Gr, delNode, lab, pre, mkGraph, insEdges, subgraph)
import           Data.List
import           Data.Maybe

import qualified Data.Vector as V

data DbDiff
  = DbDiff
  { diffGoalTrue  :: [ SomeDatabasePredicate ]
  , diffGoalFalse :: [ SomeDatabasePredicate ] }

data SolveStatus
  = SolveStatusUnknown
  | SolveStatusContradiction [ (SomeDatabasePredicate, SomeDatabasePredicate) ]
  | SolveStatusSolved
  deriving (Show, Eq)
type Solver = [ SomeDatabasePredicate ] -> SomeDatabasePredicate -> SolveStatus

instance Monoid SolveStatus where
  mempty = SolveStatusUnknown
  mappend SolveStatusUnknown x = x
  mappend x SolveStatusUnknown = x
  mappend (SolveStatusContradiction xs) (SolveStatusContradiction ys) =
    SolveStatusContradiction (xs ++ ys)
  mappend x@(SolveStatusContradiction {}) _ = x
  mappend _ y@(SolveStatusContradiction {}) = y
  mappend SolveStatusSolved SolveStatusSolved =
    SolveStatusSolved

trivialSolver :: [ SomeDatabasePredicate ] -> SomeDatabasePredicate -> SolveStatus
trivialSolver a b =
  if b `elem` a then SolveStatusSolved else SolveStatusUnknown

diff :: [ Solver ]
     -> [ SomeDatabasePredicate ] {-^ Current -}
     -> [ SomeDatabasePredicate ] {-^ Goal -}
     -> DbDiff
diff resolvers current goal =
  -- We need to check the goal for all constraints that are not satisfied.
  -- These are our new goals to make true

  -- Then we need to look at everything in current which cannot be satisfied by
  -- goal. These are what need to be made false.

  DbDiff (assume resolvers current goal)
         (assume resolvers goal current)


-- | Given a set of assumptions that are true, what remains to be proven?
assume :: [ Solver ]
       -> [ SomeDatabasePredicate ] {-^ Assumptions -}
       -> [ SomeDatabasePredicate ] {-^ Query -}
       -> [ SomeDatabasePredicate ] {-^ What remains -}
assume resolvers assumptions query =
  filter isUnfulfilled query
  where
    isUnfulfilled q =
      let sts = mconcat (resolvers <*> pure assumptions <*> pure q)
      in sts /= SolveStatusSolved

orderGoals :: (Monad m, Monoid a)
           => [ SomeDatabasePredicate ]
           -> ( forall b. [ (b, SomeDatabasePredicate) ] -> [ (b, SomeDatabasePredicate) ] -> m ([ (b, SomeDatabasePredicate) ], a) )
           -> m a
orderGoals goals solveGoal =
  let goalsVector = V.fromList goals
      allDependencies = fmap (\(i, SomeDatabasePredicate p) -> (i, mapMaybe (flip V.elemIndex goalsVector) $ dependencies p)) $
                        V.indexed goalsVector

      grNodes :: Gr SomeDatabasePredicate ()
      grNodes = mkGraph (V.toList (V.indexed goalsVector)) []
      gr = foldr (\(nodeIdx, depIdxs) gr -> insEdges (map (\i -> (i, nodeIdx, ())) depIdxs) gr ) grNodes allDependencies

      reduce gr =
        case components gr of
          [] -> pure mempty
          comps ->
            fmap mconcat . forM comps $ \comp ->
            case partition (null . pre gr) comp of
              ([], []) -> error "Impossible: empty component"
              ([], _)  -> error "No root in component"
              (roots, remaining) ->
                do let lroots = mapMaybe (\n -> (n,) <$> lab gr n) roots
                       lremaining = mapMaybe (\n -> (n,) <$> lab gr n) remaining
                   (solved, res) <-  solveGoal lroots lremaining
                   if null solved
                     then fail ("No resolution found: " ++ show (mapMaybe (lab gr) comp))
                     -- Since we've solved some, remove this from the graph
                     else let compGr' = foldr (\(n, _) gr -> delNode n gr) compGr solved
                              compGr = subgraph comp gr
                          in fmap (mappend res) (reduce compGr')
  in reduce gr
