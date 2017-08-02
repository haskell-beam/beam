{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Beam.Migrate.Actions where

import           Database.Beam.Migrate.Types
import           Database.Beam.Migrate.Checks
import           Database.Beam.Migrate.SQL

import           Control.DeepSeq
import           Control.Monad
import           Control.Parallel.Strategies

import           Data.Foldable

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Maybe
import           Data.Monoid
import qualified Data.PQueue.Min as PQ
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import           GHC.Generics

data DatabaseStateSource
  = DatabaseStateSourceOriginal
  | DatabaseStateSourceDerived
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance NFData DatabaseStateSource

type DatabaseStateKey = HS.HashSet SomeDatabasePredicate

data DatabaseState cmd
  = DatabaseState
  { dbStateCurrentState       :: !(HM.HashMap SomeDatabasePredicate DatabaseStateSource)
  , dbStateKey                :: !(HS.HashSet SomeDatabasePredicate)
  , dbStateCmdSequence        :: !(Seq.Seq cmd)
  } deriving Show

instance NFData SomeDatabasePredicate where
  rnf p' = p' `seq` ()
instance NFData (DatabaseState cmd) where
  rnf d@DatabaseState {..} = d `seq` () --rnf dbStateCurrentState `seq` dbStateCmdSequence `seq` ()

stepWeight :: Integer
stepWeight = 100

data MeasuredDatabaseState cmd
  = MeasuredDatabaseState {-# UNPACK #-} !Int {-# UNPACK #-} !Int (DatabaseState cmd)
  deriving (Show, Generic)
instance NFData (MeasuredDatabaseState cmd)
instance Eq (MeasuredDatabaseState cmd) where
  a == b = measure a == measure b
instance Ord (MeasuredDatabaseState cmd) where
  compare a b = compare (measure a) (measure b)

measure :: MeasuredDatabaseState cmd -> Int
measure (MeasuredDatabaseState cmdLength estGoalDistance _) = cmdLength + 100 * estGoalDistance -- + stepWeight * (fromIntegral aPost + fromIntegral aPre)
measuredDbState :: MeasuredDatabaseState cmd -> DatabaseState cmd
measuredDbState (MeasuredDatabaseState _ _ s) = s

measureDb' :: HS.HashSet SomeDatabasePredicate
           -> HS.HashSet SomeDatabasePredicate
           -> Int
           -> DatabaseState cmd
           -> MeasuredDatabaseState cmd
measureDb' _ post cmdLength st@(DatabaseState _ repr _) =
  MeasuredDatabaseState cmdLength distToGoal st
  where

    distToGoal = HS.size ((repr `HS.difference` post) `HS.union`
                          (post `HS.difference` repr))


--      leftToFalsify `par` leftToSatisfy `par`
--                 leftToFalsify + leftToSatisfy
--   leftToFalsify = HS.size (allToFalsify `HS.intersection` repr)
--    leftToSatisfy = HS.size (post `HS.difference` repr)

data PotentialAction cmd
  = PotentialAction
  { actionPreConditions  :: !(HS.HashSet SomeDatabasePredicate)
    -- ^ Preconditions that will no longer apply
  , actionPostConditions :: !(HS.HashSet SomeDatabasePredicate)
    -- ^ Conditions that will apply after we're done
  , actionCommands :: !(Seq.Seq cmd)
  , actionEnglish  :: !Text
  , actionScore    :: {-# UNPACK #-} !Int
  }

instance NFData cmd => NFData (PotentialAction cmd) where
  rnf PotentialAction {..} = actionPreConditions `seq`
                             actionPostConditions `seq`
                             actionCommands `seq`
                             rnf actionEnglish `seq`
                             rnf actionScore `seq` ()

instance Monoid (PotentialAction cmd) where
  mempty = PotentialAction mempty mempty mempty  "" 0
  mappend a b =
    PotentialAction (actionPreConditions a <> actionPreConditions b)
                    (actionPostConditions a <> actionPostConditions b)
                    (actionCommands a <> actionCommands b)
                    (if T.null (actionEnglish a) then actionEnglish b
                      else if T.null (actionEnglish b) then actionEnglish a
                           else actionEnglish a <> "; " <> actionEnglish b)
                    (actionScore a + actionScore b)

type ActionProviderFn cmd =
     (forall preCondition.  Typeable preCondition  => [ preCondition ])             {- The list of preconditions -}
  -> (forall postCondition. Typeable postCondition => [ postCondition ])            {- The list of postconditions (used for guiding action selection) -}
  -> [ PotentialAction cmd ]  {- A list of actions that we could perform -}
type ActionCheckFn =
     (forall preCondition.  Typeable preCondition  => [ preCondition ])             {- The list of preconditions -}
  -> (forall postCondition. Typeable postCondition => [ postCondition ])            {- The list of postconditions (used for guiding action selection) -}
  -> Bool

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

createTableWeight, dropTableWeight, addColumnWeight, dropColumnWeight :: Int
createTableWeight = 500
dropTableWeight = 100
addColumnWeight = 1
dropColumnWeight = 1

ensureAll_ :: [ SomeDatabasePredicate ] -> [ SomeDatabasePredicate ]
           -> Bool
ensureAll_ preds =
  all (`elem` preds)

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
                  (TableColumnHasConstraint tblNm' colNm' c
                   :: TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd)) <-
                  findPostConditions
                guard (postTblNm == tblNm')
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
          pure $ do p'@(SomeDatabasePredicate pred') <- findPreConditions
                    guard (pred' `predicateCascadesDropOn` tblP)
                    pure p'

        -- Now, collect all preconditions that may be related to the dropped table
        let cmd = dropTableCmd (dropTableSyntax preTblNm)
        pure ({-trace ("Dropping table " <> show preTblNm <> " would drop " <> show relatedPreds) $ -}
              PotentialAction (HS.fromList (SomeDatabasePredicate tblP:relatedPreds)) mempty (Seq.singleton cmd) ("Drop table " <> preTblNm) dropTableWeight)

addColumnProvider :: forall cmd
                   . Sql92SaneDdlCommandSyntax cmd
                   => ActionProvider cmd
addColumnProvider =
  ActionProvider provider
  where
    provider :: ActionProviderFn cmd
    provider findPreConditions findPostConditions =
      do colP@(TableHasColumn tblNm colNm colType :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd))
           <- findPostConditions
         TableExistsPredicate tblNm' <- findPreConditions
         guard (tblNm' == tblNm)
         ensuringNot_ $ do
           TableHasColumn tblNm'' colNm' _ :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd) <-
             findPreConditions
           guard (tblNm'' == tblNm && colNm == colNm') -- This column exists as a different type

         let cmd = alterTableCmd (alterTableSyntax tblNm (addColumnSyntax colNm schema))
             schema = columnSchemaSyntax colType Nothing [] Nothing
         pure (PotentialAction mempty (HS.fromList [SomeDatabasePredicate colP])
                               (Seq.singleton cmd)
                               ("Add column " <> colNm <> " to " <> tblNm)
                (addColumnWeight + fromIntegral (T.length tblNm + T.length colNm)))

dropColumnProvider :: forall cmd
                    . Sql92SaneDdlCommandSyntax cmd
                   => ActionProvider cmd
dropColumnProvider = ActionProvider provider
  where
    provider :: ActionProviderFn cmd
    provider findPreConditions _ =
      do colP@(TableHasColumn tblNm colNm _ :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd))
           <- findPreConditions

--         TableExistsPredicate tblNm' <- trace ("COnsider drop " <> show tblNm <> " " <> show colNm)  findPreConditions
--         guard (any (\(TableExistsPredicate tblNm') -> tblNm' == tblNm) findPreConditions) --tblNm' == tblNm)
--         ensuringNot_ $ do
--           TableHasColumn tblNm' colNm' colType' :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd) <-
--             findPostConditions
--           guard (tblNm' == tblNm && colNm == colNm' && colType == colType') -- This column exists as a different type

         relatedPreds <- --pure []
           pure $ do p'@(SomeDatabasePredicate pred') <- findPreConditions
                     guard (pred' `predicateCascadesDropOn` colP)
                     pure p'

         let cmd = alterTableCmd (alterTableSyntax tblNm (dropColumnSyntax colNm))
         pure (PotentialAction (HS.fromList (SomeDatabasePredicate colP:relatedPreds)) mempty
                               (Seq.singleton cmd)
                               ("Drop column " <> colNm <> " from " <> tblNm)
                (dropColumnWeight + fromIntegral (T.length tblNm + T.length colNm)))

addColumnNullProvider :: forall cmd
                       . Sql92SaneDdlCommandSyntax cmd
                      => ActionProvider cmd
addColumnNullProvider = ActionProvider provider
  where
    provider :: ActionProviderFn cmd
    provider findPreConditions findPostConditions =
      do colP@(TableColumnHasConstraint tblNm colNm _ :: TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd))
           <- findPostConditions
-- TODO         guard (c == notNullConstraintSyntax)

         TableExistsPredicate tblNm' <- findPreConditions
         guard (tblNm == tblNm')

         TableHasColumn tblNm'' colNm' _ :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd) <- findPreConditions
         guard (tblNm == tblNm'' && colNm == colNm')

         let cmd = alterTableCmd (alterTableSyntax tblNm (alterColumnSyntax colNm setNotNullSyntax))
         pure (PotentialAction mempty (HS.fromList [SomeDatabasePredicate colP]) (Seq.singleton cmd)
                               ("Add not null constraint to " <> colNm <> " on " <> tblNm) 100)

dropColumnNullProvider :: forall cmd
                        . Sql92SaneDdlCommandSyntax cmd
                       => ActionProvider cmd
dropColumnNullProvider = ActionProvider provider
  where
    provider :: ActionProviderFn cmd
    provider findPreConditions _ =
      do colP@(TableColumnHasConstraint tblNm colNm _ :: TableColumnHasConstraint (Sql92DdlCommandColumnSchemaSyntax cmd))
           <- findPreConditions
-- TODO         guard (c == notNullConstraintSyntax)

         TableExistsPredicate tblNm' <- findPreConditions
         guard (tblNm == tblNm')

         TableHasColumn tblNm'' colNm' _ :: TableHasColumn (Sql92DdlCommandColumnSchemaSyntax cmd) <- findPreConditions
         guard (tblNm == tblNm'' && colNm == colNm')

         let cmd = alterTableCmd (alterTableSyntax tblNm (alterColumnSyntax colNm setNullSyntax))
         pure (PotentialAction (HS.fromList [SomeDatabasePredicate colP]) mempty (Seq.singleton cmd)
                               ("Drop not null constraint for " <> colNm <> " on " <> tblNm) 100)

solvedState :: HS.HashSet SomeDatabasePredicate -> DatabaseState cmd -> Bool
--solvedState (DatabaseState post pre _ _)
--  | trace ("Solved state " ++ show (HS.size post, HS.size pre)) False = undefined
solvedState goal (DatabaseState _ cur _) = goal == cur
--  all (`HM.member` curs) post
  --post `HS.isSubsetOf` 
  --HS.null post && HS.null pre
--solvedState _ = False

defaultActionProviders :: Sql92SaneDdlCommandSyntax cmd
                       => [ ActionProvider cmd ]
defaultActionProviders = [ createTableActionProvider
                         , dropTableActionProvider

                         , addColumnProvider
                         , dropColumnProvider

                         , addColumnNullProvider
                         , dropColumnNullProvider ]

data Solver cmd where
  ProvideSolution :: Maybe [cmd] -> [ DatabaseState cmd ] -> Solver cmd
  ChooseActions   :: !(DatabaseState cmd)       {- Given a database state -}
                  -> (f -> PotentialAction cmd )
                  -> [ f ] {- Potential actions -}
                  -> ( [ f ] -> Solver cmd ) {- Get the next solver given these actions -}
                  -> Solver cmd

data FinalSolution cmd
  = Solved [ cmd ]
  | Candidates [ DatabaseState cmd ]
  deriving Show

finalSolution :: Solver cmd -> FinalSolution cmd
finalSolution (ProvideSolution Nothing sts)    = Candidates sts
finalSolution (ProvideSolution (Just cmds) _)  = Solved cmds
finalSolution (ChooseActions _ _ actions next) = finalSolution (next actions)

{-# INLINE heuristicSolver #-}
heuristicSolver :: [ ActionProvider cmd ]
                -> [ SomeDatabasePredicate ] -- ^ Pre conditions
                -> [ SomeDatabasePredicate ] -- ^ Post conditions
                -> Solver cmd      -- ^ List of actions to take (most likely to less likely)
heuristicSolver providers preConditionsL postConditionsL =

  heuristicSolver' initQueue mempty PQ.empty

  where
    postConditions = HS.fromList postConditionsL
    preConditions = HS.fromList preConditionsL
    allToFalsify = preConditions `HS.difference` postConditions
    measureDb = measureDb' allToFalsify postConditions

    initQueue = let mdb = measureDb 0 initDbState
                in PQ.singleton mdb
    initDbState = DatabaseState (DatabaseStateSourceOriginal <$ HS.toMap preConditions)
                                preConditions
                                mempty

    heuristicSolver' !q visited bestRejected =
      case PQ.minView q of
        Nothing -> ProvideSolution Nothing (measuredDbState <$>  PQ.toList bestRejected)
        Just (mdbState@(MeasuredDatabaseState _ _ dbState), q')
          | dbStateKey dbState `HS.member` visited -> heuristicSolver' q' visited bestRejected
          | solvedState postConditions (measuredDbState mdbState) ->
              ProvideSolution (Just (toList (dbStateCmdSequence dbState)))
                              (measuredDbState <$>  PQ.toList bestRejected)
          | otherwise ->
                  let steps = foldMap
                                     (\(ActionProvider provider) ->
                                         withStrategy (parList rseq) $
                                         provider (findPredicates (HM.keys $ dbStateCurrentState dbState))
                                                  (findPredicates postConditionsL))
                                     providers
                      steps' = filter (not . (`HS.member` visited) . dbStateKey . measuredDbState . snd) $
                               --filter (\(_, MeasuredDatabaseState _ distToGoal' _) -> distToGoal' < distToGoal) $
                               withStrategy (parList rseq) $
                               map (\step -> let dbState' = applyStep step mdbState
                                             in dbState' `seq` (step, dbState')) steps

                      applyStep step (MeasuredDatabaseState score _ dbState') =
                        let dbState'' = dbStateAfterAction dbState' step
                        in measureDb (score + 1) dbState''

                  in case steps' of
                       -- Since no steps were generated, this is a dead end. Add to the rejected queue
                       [] ->
                         --ProvideSolution Nothing ([dbState])

                         --error (" No steps generated (" <> show (length steps) <> " rejected.\ncur: " <> show (dbStateCurrentState dbState) <> "\npost:" <> show postConditionsL)
                         heuristicSolver' q' visited (reject mdbState bestRejected)
                       _ -> ChooseActions dbState fst steps' $ \groupedSteps' ->
                                let q'' = foldr (\(_, dbState') -> PQ.insert dbState') q' groupedSteps'
                                    visited' = HS.insert (dbStateKey dbState) visited
                                in -- trace ("QUEUE sive " <> show (PQ.size q'')) $
                                   withStrategy (rparWith rseq) q'' `seq` heuristicSolver' q'' visited' bestRejected

    reject :: MeasuredDatabaseState cmd -> PQ.MinQueue (MeasuredDatabaseState cmd)
           -> PQ.MinQueue (MeasuredDatabaseState cmd)
    reject mdbState q =
      let q' = PQ.insert mdbState q
      in PQ.fromAscList (PQ.take rejectedCount q')

    findPredicates :: forall predicate. Typeable predicate
                   => [ SomeDatabasePredicate ]
                   -> [ predicate ]
    findPredicates =
      case eqT :: Maybe (predicate :~: SomeDatabasePredicate) of
        Just Refl -> id
        _ -> mapMaybe (\(SomeDatabasePredicate p') -> cast p')

    dbStateAfterAction (DatabaseState curState _ cmds) action =
      let curState' = ((curState `HM.difference` HS.toMap (actionPreConditions action))
                     `HM.union` (DatabaseStateSourceDerived <$ HS.toMap (actionPostConditions action)))
      in DatabaseState curState' (HS.fromMap (() <$ curState'))
                       (cmds <> actionCommands action)
