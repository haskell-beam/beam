{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Database.Beam.Migrate.Actions where

import           Database.Beam.Migrate.Types
import           Database.Beam.Migrate.Checks
import           Database.Beam.Migrate.SQL

import           Control.Monad

import           Data.Foldable
import           Data.List (partition)
import           Data.Maybe
import           Data.Monoid
import qualified Data.PQueue.Min as PQ
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Data.Typeable
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

import           Debug.Trace

data DatabaseStateSource
  = DatabaseStateSourceOriginal
  | DatabaseStateSourceDerived
  deriving (Show, Eq, Ord, Enum, Bounded)

data DatabaseState cmd
  = DatabaseState
  { dbStatePostConditionsLeft :: HS.HashSet SomeDatabasePredicate
  , dbStatePreConditionsLeft  :: HS.HashSet SomeDatabasePredicate
  , dbStateCurrentState       :: HM.HashMap SomeDatabasePredicate DatabaseStateSource
  , dbStateCmdSequence        :: Seq.Seq cmd
  } deriving Show

stepWeight :: Integer
stepWeight = 100

data MeasuredDatabaseState cmd
  = MeasuredDatabaseState !Integer !Int !Int (DatabaseState cmd)
  deriving Show
instance Eq (MeasuredDatabaseState cmd) where
  a == b = measure a == measure b
instance Ord (MeasuredDatabaseState cmd) where
  compare a b = compare (measure a) (measure b)

measure :: MeasuredDatabaseState cmd -> Integer
measure (MeasuredDatabaseState a aPost aPre _) = a + stepWeight * (fromIntegral aPost + fromIntegral aPre)
measuredDbState :: MeasuredDatabaseState cmd -> DatabaseState cmd
measuredDbState (MeasuredDatabaseState _ _ _ s) = s

data PotentialAction cmd
  = PotentialAction
  { actionPreConditions  :: HS.HashSet SomeDatabasePredicate
    -- ^ Preconditions that will no longer apply
  , actionPostConditions :: HS.HashSet SomeDatabasePredicate
    -- ^ Conditions that will apply after we're done
  , actionCommands :: Seq.Seq cmd
  , actionEnglish  :: Text
  , actionScore    :: Integer
  } deriving Show

type ActionProviderFn cmd =
     (forall preCondition.  Typeable preCondition  => [ preCondition ])             {- The list of preconditions -}
  -> (forall postCondition. Typeable postCondition => [ postCondition ])             {- The list of postconditions (used for guiding action selection) -}
  -> [ PotentialAction cmd ]  {- A list of actions that we could perform -}

newtype ActionProvider cmd
  = ActionProvider (ActionProviderFn cmd)

rejectedCount :: Int
rejectedCount = 50

ensuringNot_ :: [ a ] -> [ () ]
ensuringNot_ [] = [ () ]
ensuringNot_ _  = []

justOne_ :: [ a ] -> [ a ]
justOne_ [x] = [x]
justOne_ _ = []

createTableWeight, dropTableWeight :: Integer
createTableWeight = 500
dropTableWeight = 100

createTableActionProvider :: forall cmd
                           . Sql92SaneDdlCommandSyntax cmd
                          => ActionProvider cmd
createTableActionProvider =
  ActionProvider provider
  where
    provider :: ActionProviderFn cmd
    provider findPreConditions findPostConditions =
      do tblP@(TableExistsPredicate postTblNm) <- findPostConditions
         -- Make sure there's no corresponding predicate in the precondition
         ensuringNot_ $
           do TableExistsPredicate preTblNm <- findPreConditions
              guard (preTblNm == postTblNm)

         (columnsP, columns) <- pure . unzip $
           do columnP@
                (TableHasColumn tblNm colNm schema
                 :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd)) <-
                findPostConditions
              guard (tblNm == postTblNm)

              (constraintsP, constraints) <-
                pure . unzip $ do
                constraintP@
                  (TableColumnHasConstraint tblNm colNm' c
                   :: TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd)) <-
                  findPostConditions
                guard (tblNm == postTblNm)
                guard (colNm == colNm')

                pure (p constraintP, c)

              pure (p columnP:constraintsP, (colNm, schema, constraints))
         (primaryKeyP, primaryKey) <- justOne_ $ do
           primaryKeyP@(TableHasPrimaryKey tblNm primaryKey) <-
             findPostConditions
           guard (tblNm == postTblNm)
           pure (primaryKeyP, primaryKey)

         let postConditions = [ p tblP, p primaryKeyP ] ++ concat columnsP
             cmd = createTableCmd (createTableSyntax Nothing postTblNm colsSyntax tblConstraints)
             tblConstraints = [ primaryKeyConstraintSyntax primaryKey ]
             colsSyntax = map (\(colNm, type_, cs) -> (colNm, columnSchemaSyntax type_ Nothing cs Nothing)) columns
         pure (PotentialAction mempty (HS.fromList postConditions) (Seq.singleton cmd) ("Create the table " <> postTblNm) createTableWeight)

dropTableActionProvider :: forall cmd
                        . Sql92SaneDdlCommandSyntax cmd
                        => ActionProvider cmd
dropTableActionProvider =
 ActionProvider provider
 where
   -- Look for tables that exist as a precondition but not a post condition
   provider :: ActionProviderFn cmd
   provider findPreConditions findPostConditions =
     do tblP@(TableExistsPredicate preTblNm) <- findPreConditions
        ensuringNot_ $
          do TableExistsPredicate postTblNm <- findPostConditions
             guard (preTblNm == postTblNm)

        relatedPreds <-
          pure $ do p'@(SomeDatabasePredicate p) <- findPreConditions
                    guard (p `predicateCascadesDropOn` tblP)
                    pure p'

        -- Now, collect all preconditions that may be related to the dropped table
        let cmd = dropTableCmd (dropTableSyntax preTblNm)
        pure (PotentialAction (HS.fromList (SomeDatabasePredicate tblP:relatedPreds)) mempty (Seq.singleton cmd) ("Drop table " <> preTblNm) dropTableWeight)

solvedState :: DatabaseState cmd -> Bool
--solvedState (DatabaseState post pre _ _)
--  | trace ("Solved state " ++ show (HS.size post, HS.size pre)) False = undefined
solvedState (DatabaseState post pre _ _) = HS.null post && HS.null pre
solvedState _ = False

defaultActionProviders :: Sql92SaneDdlCommandSyntax cmd
                       => [ ActionProvider cmd ]
defaultActionProviders = [ createTableActionProvider
                         , dropTableActionProvider ]

data SolutionSequence cmd
  = Solutions [cmd] (SolutionSequence cmd)
  | NoMoreSolutions [ DatabaseState cmd ]
  deriving Show

data GuessResult cmd
  = GuessResultSolved [ [cmd] ]
  | GuessResultCandidates [ DatabaseState cmd ]
  deriving Show

guessActions :: [ ActionProvider cmd ]
             -> [ SomeDatabasePredicate ] -- ^ Pre conditions
             -> [ SomeDatabasePredicate ] -- ^ Post conditions
             -> GuessResult cmd      -- ^ List of actions to take (most likely to less likely)
guessActions providers preConditions postConditions =

  case guessActions' initQueue mempty PQ.empty of
    NoMoreSolutions candidates -> GuessResultCandidates candidates
    s -> GuessResultSolved $ sequenceToResult s

  where
    initQueue = PQ.singleton (MeasuredDatabaseState 0 (length postConditions) (length preConditions)
                              (DatabaseState (HS.fromList postConditions) (HS.fromList preConditions) (HM.fromList $ fmap (,DatabaseStateSourceOriginal) preConditions) mempty))

    sequenceToResult (Solutions x xs) = x:sequenceToResult xs
    sequenceToResult (NoMoreSolutions {}) = []

    guessActions' q visited bestRejected =
      case PQ.minView q of
        Nothing -> NoMoreSolutions (measuredDbState <$>  PQ.toList bestRejected)
        Just (mdbState@(MeasuredDatabaseState stateScore _ _ dbState), q')
          | solvedState dbState -> Solutions (toList (dbStateCmdSequence dbState)) (guessActions' q' visited bestRejected)
          | otherwise ->
              case trivialAction dbState of
                Just dbState' ->
                  guessActions' (PQ.insert (MeasuredDatabaseState (stateScore + 1) (HS.size (dbStatePostConditionsLeft dbState')) (HS.size (dbStatePreConditionsLeft dbState')) dbState') q')
                                (HS.insert (HS.fromMap (() <$ dbStateCurrentState dbState')) visited)
                                bestRejected

                Nothing ->
                  -- Otherwise, check each action provider
                  let steps = foldMap (\(ActionProvider provider) ->
                                         provider (findPredicates (HM.keys $ dbStateCurrentState dbState))
                                                  (findPredicates (HS.toList $ dbStatePostConditionsLeft dbState)))
                                       providers
                  in case steps of
                       -- Since no steps were generated, this is a dead end. Add to the rejected queue
                       [] -> trace "No steps generated" $ guessActions' q' visited (reject mdbState bestRejected)
                       steps ->
                         let (q'', visited') = foldr (insertActionInQueue stateScore dbState) (q', visited) steps
                         in guessActions' q'' visited' bestRejected

    reject :: MeasuredDatabaseState cmd -> PQ.MinQueue (MeasuredDatabaseState cmd)
           -> PQ.MinQueue (MeasuredDatabaseState cmd)
    reject mdbState q =
      let q' = PQ.insert mdbState q
      in PQ.fromAscList (PQ.take rejectedCount q')

    insertActionInQueue stateScore dbState action (q, visited)
      | action `actionProgressesState` dbState =
        let dbState' = dbStateAfterAction dbState action
            stateRepr = HS.fromMap (() <$ dbStateCurrentState dbState')
        in if HS.member stateRepr visited
           then (q, visited)
           else ( PQ.insert (MeasuredDatabaseState (stateScore + actionScore action) (HS.size (dbStatePostConditionsLeft dbState')) (HS.size (dbStatePreConditionsLeft dbState')) dbState') q
                , HS.insert stateRepr visited )
      | otherwise =
        let stateRepr = HS.fromMap (() <$ dbStateCurrentState dbState)
        in (q, HS.insert stateRepr visited)

    -- An action progresses the state if it either solves a post condition, or
    -- gets rid of an original pre condition
    actionProgressesState :: PotentialAction cmd -> DatabaseState cmd
                          -> Bool
    actionProgressesState action st =
      actionSolvesPostCondition action st ||
      actionSolvesPreCondition action st

    actionSolvesPreCondition, actionSolvesPostCondition
      :: PotentialAction cmd -> DatabaseState cmd
      -> Bool
    actionSolvesPreCondition (PotentialAction solvedPres _ _ _ _)
                             (DatabaseState _ pres _ _) =
      not (HS.null (HS.intersection pres solvedPres))
    actionSolvesPostCondition (PotentialAction _ solvedPosts _ _ _)
                              (DatabaseState posts _ _ _) =
      not (HS.null (HS.intersection posts solvedPosts))
      --any (`elem` posts) solvedPosts

    findPredicates :: forall predicate. Typeable predicate
                   => [ SomeDatabasePredicate ]
                   -> [ predicate ]
    findPredicates =
      case eqT :: Maybe (predicate :~: SomeDatabasePredicate) of
        Just Refl -> id
        _ -> mapMaybe (\(SomeDatabasePredicate p) -> cast p)

    dbStateAfterAction (DatabaseState postConditionsLeft origConditionsLeft curState cmds) action =
      DatabaseState (postConditionsLeft `HS.difference` actionPostConditions action)
                    (origConditionsLeft `HS.difference` actionPreConditions action)
                    ((curState `HM.difference` HS.toMap (actionPreConditions action))
                     `HM.union` (DatabaseStateSourceDerived <$ HS.toMap (actionPostConditions action)))
                    (cmds <> actionCommands action)

    ------------------------------------------------------------------------------------------------------
    -- exceptPredicates a except = filter (not . (`elem` except)) a                                     --
    -- exceptPredicatesCur :: [(DatabaseStateSource, SomeDatabasePredicate)]                            --
    --                     -> [SomeDatabasePredicate] -> [(DatabaseStateSource, SomeDatabasePredicate)] --
    -- exceptPredicatesCur a except = filter (not . (`elem` except) . snd) a                            --
    --                                                                                                  --
    -- withNewConditions :: [(DatabaseStateSource, SomeDatabasePredicate)]                              --
    --                   -> [SomeDatabasePredicate]                                                     --
    --                   -> [(DatabaseStateSource,SomeDatabasePredicate)]                               --
    -- withNewConditions =                                                                              --
    --   foldr (\newCond oldConds -> withNewCondition oldConds newCond)                                 --
    --                                                                                                  --
    -- withNewCondition :: [(DatabaseStateSource, SomeDatabasePredicate)]                               --
    --                  -> SomeDatabasePredicate                                                        --
    --                  -> [(DatabaseStateSource, SomeDatabasePredicate)]                               --
    -- withNewCondition oldConds newCond                                                                --
    --   | newCond `elem` fmap snd oldConds = oldConds                                                  --
    --   | otherwise = (DatabaseStateSourceDerived, newCond):oldConds                                   --
    ------------------------------------------------------------------------------------------------------

    -- Sees if we can trivially match any current state with the post condition.
    -- If so, return a copy of the state with the post conditions simplified.
    -- Otherwise, return Nothing
    trivialAction :: DatabaseState cmd -> Maybe (DatabaseState cmd)
    trivialAction (DatabaseState postConditions origPreConditions curState cmds)
      -- If there's any postcondition that is already met by curstate
      | not (HS.null triviallySatisfied) = --any (\postCondition -> any ((==postCondition) . snd) curState) postConditions =
          let origPreConditions' = origPreConditions `HS.difference` triviallySatisfied
          in Just (DatabaseState (postConditions `HS.difference` triviallySatisfied) origPreConditions' curState cmds)
      | otherwise = Nothing
      where
        triviallySatisfied = HS.fromMap $ HM.intersection (HS.toMap postConditions) curState
