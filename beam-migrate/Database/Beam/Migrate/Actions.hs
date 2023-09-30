{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data types and functions to discover sequences of DDL commands to go from
-- one database state to another. Used for migration generation.
--
-- For our purposes, a database state is fully specified by the set of
-- predicates that apply to that database.
--
-- Migration generation is approached as a graph search problem over the
-- infinite graph of databases /G/. The nodes of /G/ are database states, which
-- (as said above) are simply sets of predicates (see 'DatabaseState' for the
-- realization of this concept in code). For two vertices /S1/ and /S2/ in /G/,
-- there is an edge between the two if and only if there is a DDL command that
-- can take a database at /S1/ to /S2/.
--
-- We generate migrations by exploring this graph, starting at the source state
-- and ending at the destination state. By default we use an optimizing solver
-- that weights each edge by the complexity of the particular command, and we
-- attempt to find the shortest path using Dijkstra's algorithm, although a user
-- may override this behavior and provide a custom edge selection mechanism (or
-- even defer this choice to the user).
--
-- In order to conduct the breadth-first search, we must know which edges lead
-- out of whichever vertex we're currently visiting. The solving algorithm thus
-- takes a set of 'ActionProvider's, which are means of discovering edges that
-- are incident to the current database state.
--
-- Conceptually, an 'ActionProvider' is a function of type 'ActionProviderFn',
-- which takes the current database state and produces a list of edges in the
-- form of 'PotentialAction' objects. For optimization purposes,
-- 'ActionProvider's also take in the desired destination state, which it can
-- use to select only edges that make sense. This does not affect the result,
-- just the amount of time it may take to get there.
--
-- Note that because the graph of database states is infinite, a breadth-first
-- search may easily end up continuing to explore when there is no chance of
-- reaching our goal. This would result in non-termination and is highly
-- undesirable. In order to prevent this, we limit ourselves to only exploring
-- edges that take us /closer/ to the destination state. Here, we measure
-- distance between two states as the number of elements in the symmetric
-- difference of two database states. Thus, every action we take must either
-- remove a predicate that doesn't exist in the destination state, or add a
-- predicate that does. If a potential action only adds predicates that do not
-- exist in the final state or removes predicates that do not exist in the
-- first, then we never explore that edge.
--
-- == A note on speed
--
-- There are some issues with this approach. Namely, if there is no solution, we
-- can end up exploring the entire action space, which may be quite a lot. While
-- @beam-migrate@ can solve all databases that can be made up of predicates in
-- this module, other beam backends may not make such strict guarantees
-- (although in practice, all do). Nevertheless, if you're hacking on this
-- module and notice what seems like an infinite loop, you may have accidentally
-- removed code that exposed the edge that leads to a solution to the migration.
--
--
module Database.Beam.Migrate.Actions
  (
  -- * Database state
    DatabaseStateSource(..)
  , DatabaseState(..)

  -- * Action generation
  , PotentialAction(..)

  , ActionProvider(..)
  , ActionProviderFn

  , ensuringNot_
  , justOne_

  , createTableActionProvider
  , dropTableActionProvider
  , addColumnProvider
  , addColumnNullProvider
  , dropColumnNullProvider
  , defaultActionProvider

  -- * Solver
  , Solver(..), FinalSolution(..)
  , finalSolution
  , heuristicSolver
  ) where

import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.Checks
import           Database.Beam.Migrate.SQL
import           Database.Beam.Migrate.Types
import           Database.Beam.Migrate.Types.Predicates (qnameAsText, qnameAsTableName)

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Parallel.Strategies

import           Data.Foldable

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.PQueue.Min as PQ
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import           GHC.Generics

-- | Used to indicate whether a particular predicate is from the initial
-- database state, or due to a sequence of actions we've committed too. Used to
-- prevent runaway action generation based off of derived states.
data DatabaseStateSource
  = DatabaseStateSourceOriginal -- ^ Predicate is from the original set given by the user
  | DatabaseStateSourceDerived  -- ^ Predicate is from an action we've committed to in this action chain
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance NFData DatabaseStateSource

-- | Represents the state of a database as a migration is being generated
data DatabaseState be
  = DatabaseState
  { dbStateCurrentState       :: !(HM.HashMap SomeDatabasePredicate DatabaseStateSource)
    -- ^ The current set of predicates that apply to this database as well as
    -- their source (user or from previous actions)
  , dbStateKey                :: !(HS.HashSet SomeDatabasePredicate)
    -- ^ HS.fromMap of 'dbStateCurrentState', for maximal sharing
  , dbStateCmdSequence        :: !(Seq.Seq (MigrationCommand be))
    -- ^ The current sequence of commands we've committed to in this state
  }
deriving instance Show (BeamSqlBackendSyntax be) => Show (DatabaseState be)

instance NFData (DatabaseState cmd) where
  rnf d@DatabaseState {..} = d `seq` ()

-- | Wrapper for 'DatabaseState' that keeps track of the command sequence length
-- and goal distance. Used for sorting states when conducting the search.
data MeasuredDatabaseState be
  = MeasuredDatabaseState {-# UNPACK #-} !Int {-# UNPACK #-} !Int (DatabaseState be)
  deriving Generic
deriving instance Show (BeamSqlBackendSyntax be) => Show (MeasuredDatabaseState be)
instance NFData (MeasuredDatabaseState cmd)
instance Eq (MeasuredDatabaseState cmd) where
  a == b = measure a == measure b
instance Ord (MeasuredDatabaseState cmd) where
  compare a b = compare (measure a) (measure b)

measure :: MeasuredDatabaseState cmd -> Int
measure (MeasuredDatabaseState cmdLength estGoalDistance _) = cmdLength + 100 * estGoalDistance

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

-- | Represents an edge (or a path) in the database graph.
--
-- Given a particular starting point, the destination database is the database
-- where each predicate in 'actionPreConditions' has been removed and each
-- predicate in 'actionPostConditions' has been added.
data PotentialAction be
  = PotentialAction
  { actionPreConditions  :: !(HS.HashSet SomeDatabasePredicate)
    -- ^ Preconditions that will no longer apply
  , actionPostConditions :: !(HS.HashSet SomeDatabasePredicate)
    -- ^ Conditions that will apply after we're done
  , actionCommands :: !(Seq.Seq (MigrationCommand be))
    -- ^ The sequence of commands that accomplish this movement in the database
    -- graph. For an edge, 'actionCommands' contains one command; for a path, it
    -- will contain more.
  , actionEnglish  :: !Text
    -- ^ An english description of the movement
  , actionScore    :: {-# UNPACK #-} !Int
    -- ^ A heuristic notion of complexity or weight; used to find the "easiest"
    -- path through the graph.
  }

instance Semigroup (PotentialAction be) where
  (<>) = mappend

-- | 'PotentialAction's can represent edges or paths. Monadically combining two
-- 'PotentialAction's results in the path between the source of the first and
-- the destination of the second. 'mempty' here returns the action that does
-- nothing (i.e., the edge going back to the same database state)
instance Monoid (PotentialAction be) where
  mempty = PotentialAction mempty mempty mempty  "" 0
  mappend a b =
    PotentialAction (actionPreConditions a <> actionPreConditions b)
                    (actionPostConditions a <> actionPostConditions b)
                    (actionCommands a <> actionCommands b)
                    (if T.null (actionEnglish a) then actionEnglish b
                      else if T.null (actionEnglish b) then actionEnglish a
                           else actionEnglish a <> "; " <> actionEnglish b)
                    (actionScore a + actionScore b)

-- | See 'ActionProvider'
type ActionProviderFn be =
     (forall preCondition.  Typeable preCondition  => [ preCondition ])             {- The list of preconditions -}
  -> (forall postCondition. Typeable postCondition => [ postCondition ])            {- The list of postconditions (used for guiding action selection) -}
  -> [ PotentialAction be ]  {- A list of actions that we could perform -}

-- | Edge discovery mechanism. A newtype wrapper over 'ActionProviderFn'.
--
-- An 'ActionProviderFn' takes two arguments. The first is the set of predicates
-- that exist in the current database.
--
-- The function should a set of edges from the database specified in the first
-- argument to possible destination databases. For optimization purposes, the
-- second argument is the set of predicates that ought to exist in the
-- destination database. This can be used to eliminate edges that will not lead
-- to a solution.
--
-- This second argument is just an optimization and doesn't change the final
-- result, although it can significantly impact the time it takes to get there.
--
-- Both the current database set and the destination database set are given as
-- polymorphic lists of predicates. When you instantiate the type, the current
-- database predicate set is queried for predicates of that type.
--
-- For example, 'dropTableActionProvider' provides a @DROP TABLE@ action edge
-- whenever it encounters a table that exists. In order to do this, it attempts
-- to find all 'TableExistsPredicate' that do not exist in the destination
-- database. Its 'ActionProviderFn' may be implemented like such:
--
-- > dropTableActionProvider preConditions postConditions = do
-- >     TableExistsPredicate srcTblNm <- preConditions
-- >     ensuringNot_ $ $
-- >       do TableExistsPredicate destTblNm <- postConditions
-- >          guard (srcTblNm == destTblNm)
--
-- 'ensuringNot_' is a function that causes the action provider to return no
-- results if there are any elements in the provided list. In this case, it's
-- used to stop @DROP TABLE@ action generation for tables which must be present
-- in the final database.
newtype ActionProvider be
  = ActionProvider { getPotentialActions :: ActionProviderFn be }

instance Semigroup (ActionProvider be) where
  (<>) = mappend

instance Monoid (ActionProvider be) where
  mempty = ActionProvider (\_ _ -> [])
  mappend (ActionProvider a) (ActionProvider b) =
    ActionProvider $ \pre post ->
    let aRes = a pre post
        bRes = b pre post

    in withStrategy (rparWith (parList rseq)) aRes `seq`
       withStrategy (rparWith (parList rseq)) bRes `seq`
       aRes ++ bRes

createTableWeight, dropTableWeight, addColumnWeight, dropColumnWeight :: Int
createTableWeight = 500
dropTableWeight = 100
addColumnWeight = 1
dropColumnWeight = 1

-- | Proceeds only if no predicate matches the given pattern. See the
-- implementation of 'dropTableActionProvider' for an example of usage.
ensuringNot_ :: Alternative m => [ a ] -> m ()
ensuringNot_ [] = pure ()
ensuringNot_ _  = empty

-- | Used to ensure that only one predicate matches the given pattern. See the
-- implementation of 'createTableActionProvider' for an example of usage.
justOne_ :: [ a ] -> [ a ]
justOne_ [x] = [x]
justOne_ _ = []

-- | Action provider for SQL92 @CREATE TABLE@ actions.
createTableActionProvider :: forall be
                           . ( Typeable be, BeamMigrateOnlySqlBackend be )
                          => ActionProvider be
createTableActionProvider =
  ActionProvider provider
  where
    provider :: ActionProviderFn be
    provider findPreConditions findPostConditions =
      do tblP@(TableExistsPredicate postTblNm) <- findPostConditions
         -- Make sure there's no corresponding predicate in the precondition
         ensuringNot_ $
           do TableExistsPredicate preTblNm <- findPreConditions
              guard (preTblNm == postTblNm)

         (columnsP, columns) <- pure . unzip $
           do columnP@(TableHasColumn tblNm colNm schema :: TableHasColumn be) <-
                findPostConditions
              guard (tblNm == postTblNm && dataTypeHasBeenCreated schema findPreConditions)

              (constraintsP, constraints) <-
                pure . unzip $ do
                constraintP@(TableColumnHasConstraint tblNm' colNm' c
                   :: TableColumnHasConstraint be) <-
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
             cmd = createTableCmd (createTableSyntax Nothing (qnameAsTableName postTblNm) colsSyntax tblConstraints)
             tblConstraints = if null primaryKey then [] else [ primaryKeyConstraintSyntax primaryKey ]
             colsSyntax = map (\(colNm, type_, cs) -> (colNm, columnSchemaSyntax type_ Nothing cs Nothing)) columns
         pure (PotentialAction mempty (HS.fromList postConditions)
                               (Seq.singleton (MigrationCommand cmd MigrationKeepsData))
                               ("Create the table " <> qnameAsText postTblNm) createTableWeight)

-- | Action provider for SQL92 @DROP TABLE@ actions
dropTableActionProvider :: forall be
                         . BeamMigrateOnlySqlBackend be
                        => ActionProvider be
dropTableActionProvider =
 ActionProvider provider
 where
   -- Look for tables that exist as a precondition but not a post condition
   provider :: ActionProviderFn be
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
        let cmd = dropTableCmd (dropTableSyntax (qnameAsTableName preTblNm))
        pure ({-trace ("Dropping table " <> show preTblNm <> " would drop " <> show relatedPreds) $ -}
              PotentialAction (HS.fromList (SomeDatabasePredicate tblP:relatedPreds)) mempty
                              (Seq.singleton (MigrationCommand cmd MigrationLosesData))
                              ("Drop table " <> qnameAsText preTblNm) dropTableWeight)

-- | Action provider for SQL92 @ALTER TABLE ... ADD COLUMN ...@ actions
addColumnProvider :: forall be
                   . ( Typeable be, BeamMigrateOnlySqlBackend be )
                  => ActionProvider be
addColumnProvider =
  ActionProvider provider
  where
    provider :: ActionProviderFn be
    provider findPreConditions findPostConditions =
      do colP@(TableHasColumn tblNm colNm colType :: TableHasColumn be)
           <- findPostConditions
         TableExistsPredicate tblNm' <- findPreConditions
         guard (tblNm'  == tblNm && dataTypeHasBeenCreated colType findPreConditions)
         ensuringNot_ $ do
           TableHasColumn tblNm'' colNm' _ :: TableHasColumn be <-
             findPreConditions
           guard (tblNm'' == tblNm && colNm == colNm') -- This column exists as a different type

         (constraintsP, constraints) <-
           pure . unzip $ do
           constraintP@(TableColumnHasConstraint tblNm'' colNm' c
              :: TableColumnHasConstraint be) <-
             findPostConditions
           guard (tblNm == tblNm'')
           guard (colNm == colNm')

           pure (p constraintP, c)

         let cmd = alterTableCmd (alterTableSyntax (qnameAsTableName tblNm) (addColumnSyntax colNm schema))
             schema = columnSchemaSyntax colType Nothing constraints Nothing
         pure (PotentialAction mempty (HS.fromList ([SomeDatabasePredicate colP] ++ constraintsP))
                               (Seq.singleton (MigrationCommand cmd MigrationKeepsData))
                               ("Add column " <> colNm <> " to " <> qnameAsText tblNm)
                (addColumnWeight + fromIntegral (T.length (qnameAsText tblNm) + T.length colNm)))

-- | Action provider for SQL92 @ALTER TABLE ... DROP COLUMN ...@ actions
dropColumnProvider :: forall be
                    . ( Typeable be, BeamMigrateOnlySqlBackend be )
                   => ActionProvider be
dropColumnProvider = ActionProvider provider
  where
    provider :: ActionProviderFn be
    provider findPreConditions _ =
      do colP@(TableHasColumn tblNm colNm _ :: TableHasColumn be)
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

         let cmd = alterTableCmd (alterTableSyntax (qnameAsTableName tblNm) (dropColumnSyntax colNm))
         pure (PotentialAction (HS.fromList (SomeDatabasePredicate colP:relatedPreds)) mempty
                               (Seq.singleton (MigrationCommand cmd MigrationLosesData))
                               ("Drop column " <> colNm <> " from " <> qnameAsText tblNm)
                (dropColumnWeight + fromIntegral (T.length (qnameAsText tblNm) + T.length colNm)))

-- | Action provider for SQL92 @ALTER TABLE ... ALTER COLUMN ... SET NULL@
addColumnNullProvider :: forall be
                       . ( Typeable be, BeamMigrateOnlySqlBackend be )
                      => ActionProvider be
addColumnNullProvider = ActionProvider provider
  where
    provider :: ActionProviderFn be
    provider findPreConditions findPostConditions =
      do colP@(TableColumnHasConstraint tblNm colNm _ :: TableColumnHasConstraint be)
           <- findPostConditions
-- TODO         guard (c == notNullConstraintSyntax)

         TableExistsPredicate tblNm' <- findPreConditions
         guard (tblNm == tblNm')

         TableHasColumn tblNm'' colNm' _ :: TableHasColumn be <-
           findPreConditions
         guard (tblNm == tblNm'' && colNm == colNm')

         let cmd = alterTableCmd (alterTableSyntax (qnameAsTableName tblNm) (alterColumnSyntax colNm setNotNullSyntax))
         pure (PotentialAction mempty (HS.fromList [SomeDatabasePredicate colP])
                               (Seq.singleton (MigrationCommand cmd MigrationKeepsData))
                               ("Add not null constraint to " <> colNm <> " on " <> qnameAsText tblNm) 100)

-- | Action provider for SQL92 @ALTER TABLE ... ALTER COLUMN ... SET  NOT NULL@
dropColumnNullProvider :: forall be
                        . ( Typeable be, BeamMigrateOnlySqlBackend be )
                       => ActionProvider be
dropColumnNullProvider = ActionProvider provider
  where
    provider :: ActionProviderFn be
    provider findPreConditions _ =
      do colP@(TableColumnHasConstraint tblNm colNm _ :: TableColumnHasConstraint be)
           <- findPreConditions
-- TODO         guard (c == notNullConstraintSyntax)

         TableExistsPredicate tblNm' <- findPreConditions
         guard (tblNm == tblNm')

         TableHasColumn tblNm'' colNm' _ :: TableHasColumn be <-
           findPreConditions
         guard (tblNm == tblNm'' && colNm == colNm')

         let cmd = alterTableCmd (alterTableSyntax (qnameAsTableName tblNm) (alterColumnSyntax colNm setNullSyntax))
         pure (PotentialAction (HS.fromList [SomeDatabasePredicate colP]) mempty
                               (Seq.singleton (MigrationCommand cmd MigrationKeepsData))
                               ("Drop not null constraint for " <> colNm <> " on " <> qnameAsText tblNm) 100)

-- | Default action providers for any SQL92 compliant syntax.
--
-- In particular, this provides edges consisting of the following statements:
--
--  * CREATE TABLE
--  * DROP TABLE
--  * ALTER TABLE ... ADD COLUMN ...
--  * ALTER TABLE ... DROP COLUMN ...
--  * ALTER TABLE ... ALTER COLUMN ... SET [NOT] NULL
defaultActionProvider :: ( Typeable be
                         , BeamMigrateOnlySqlBackend be )
                      => ActionProvider be
defaultActionProvider =
  mconcat
  [ createTableActionProvider
  , dropTableActionProvider

  , addColumnProvider
  , dropColumnProvider

  , addColumnNullProvider
  , dropColumnNullProvider ]

-- | Represents current state of a database graph search.
--
-- If 'ProvideSolution', the destination database has been reached, and the
-- given list of commands provides the path from the source database to the
-- destination.
--
-- If 'SearchFailed', the search has failed. The provided 'DatabaseState's
-- represent the closest we could make it to the destination database. By
-- default, only the best 10 are kept around (to avoid unbounded memory growth).
--
-- If 'ChooseActions', we are still searching. The caller is provided with the
-- current state as well as a list of actions, provided as an opaque type @f@.
-- The 'getPotentialActionChoice' function can be used to get the
-- 'PotentialAction' corresponding to any given @f@. The caller is free to cull
-- the set of potential actions according however they'd like (for example, by
-- prompting the user). The selected actions to explore should be passed to the
-- 'continueSearch' function.
--
-- Use of the @f@ existential type may seem obtuse, but it prevents the caller
-- from injecting arbitrary actions. Instead the caller is limited to choosing
-- only valid actions as provided by the suppled 'ActionProvider'.
data Solver cmd where
  ProvideSolution :: [ MigrationCommand cmd ] -> Solver cmd
  SearchFailed    :: [ DatabaseState cmd ] -> Solver cmd
  ChooseActions   :: { choosingActionsAtState :: !(DatabaseState cmd)
                       -- ^ The current node we're searching at
                     , getPotentialActionChoice :: f -> PotentialAction cmd
                       -- ^ Convert the opaque @f@ type to a 'PotentialAction'.
                       -- This can be used to present the actions to the user or
                       -- to inspect the action to make a more informed choice
                       -- on exploration strategies.
                     , potentialActionChoices :: [ f ]
                       -- ^ The possible actions that we can take, presented as
                       -- an opaque list. Use the 'getPotentialActionChoice'
                       -- function to get the corresponding 'PotentialAction'.
                     , continueSearch :: [ f ] -> Solver cmd
                       -- ^ Continue the search and get the next 'Solver'
                     } -> Solver cmd

-- | Represents the final results of a search
data FinalSolution be
  = Solved [ MigrationCommand be ]
    -- ^ The search found a path from the source to the destination database,
    -- and has provided a set of commands that would work
  | Candidates [ DatabaseState be ]
    -- ^ The search failed, but provided a set of 'DatbaseState's it encountered
    -- that were the closest to the destination database. By default, only 10
    -- candidates are provided.
deriving instance Show (BeamSqlBackendSyntax be) => Show (FinalSolution be)

-- | Returns 'True' if the state has been solved
solvedState :: HS.HashSet SomeDatabasePredicate -> DatabaseState be -> Bool
solvedState goal (DatabaseState _ cur _) = goal == cur

-- | An exhaustive solving strategy that simply continues the search, while
-- exploring every possible action. If there is a solution, this will find it.
finalSolution :: Solver be -> FinalSolution be
finalSolution (SearchFailed sts)     = Candidates sts
finalSolution (ProvideSolution cmds) = Solved cmds
finalSolution (ChooseActions _ _ actions next) =
  finalSolution (next actions)

{-# INLINE heuristicSolver #-}
-- | Conduct a breadth-first search of the database graph to find a path from
-- the source database to the destination database, using the given
-- 'ActionProvider' to discovere "edges" (i.e., DDL commands) between the
-- databases.
--
-- See the documentation on 'Solver' for more information on how to consume the
-- result.
heuristicSolver :: ActionProvider be         -- ^ Edge discovery function
                -> [ SomeDatabasePredicate ] -- ^ Source database state
                -> [ SomeDatabasePredicate ] -- ^ Destination database state
                -> Solver be
heuristicSolver provider preConditionsL postConditionsL =

  heuristicSolver' initQueue mempty PQ.empty

  where
    -- Number of failed action chains to keep
    rejectedCount = 10

    postConditions = HS.fromList postConditionsL
    preConditions = HS.fromList preConditionsL
    allToFalsify = preConditions `HS.difference` postConditions
    measureDb = measureDb' allToFalsify postConditions

    initQueue = PQ.singleton (measureDb 0 initDbState)
    initDbState = DatabaseState (DatabaseStateSourceOriginal <$ HS.toMap preConditions)
                                preConditions
                                mempty

    findPredicate :: forall predicate. Typeable predicate
                   => SomeDatabasePredicate
                   -> [ predicate ] -> [ predicate ]
    findPredicate
      | Just (Refl :: predicate :~: SomeDatabasePredicate) <- eqT =
          (:)
      | otherwise =
          \(SomeDatabasePredicate pred') ps ->
              maybe ps (:ps) (cast pred')

    findPredicates :: forall predicate f. (Typeable predicate, Foldable f)
                   => f SomeDatabasePredicate -> [ predicate ]
    findPredicates = foldr findPredicate []

    heuristicSolver' !q !visited !bestRejected =
      case PQ.minView q of
        Nothing -> SearchFailed (measuredDbState <$> PQ.toList bestRejected)
        Just (mdbState@(MeasuredDatabaseState _ _ dbState), q')
          | dbStateKey dbState `HS.member` visited -> heuristicSolver' q' visited bestRejected
          | solvedState postConditions (measuredDbState mdbState) ->
              ProvideSolution (toList (dbStateCmdSequence dbState))
          | otherwise ->
              let steps = getPotentialActions
                              provider
                              (findPredicates (dbStateKey dbState))
                              (findPredicates postConditionsL)

                  steps' = filter (not . (`HS.member` visited) . dbStateKey . measuredDbState . snd) $
                           withStrategy (parList rseq) $
                           map (\step -> let dbState' = applyStep step mdbState
                                         in dbState' `seq` (step, dbState')) steps

                  applyStep step (MeasuredDatabaseState score _ dbState') =
                    let dbState'' = dbStateAfterAction dbState' step
                    in measureDb (score + 1) dbState''

              in case steps' of
                   -- Since no steps were generated, this is a dead end. Add to the rejected queue
                   [] -> heuristicSolver' q' visited (reject mdbState bestRejected)
                   _ -> ChooseActions dbState fst steps' $ \chosenSteps ->
                            let q'' = foldr (\(_, dbState') -> PQ.insert dbState') q' chosenSteps
                                visited' = HS.insert (dbStateKey dbState) visited
                            in withStrategy (rparWith rseq) q'' `seq` heuristicSolver' q'' visited' bestRejected

    reject :: MeasuredDatabaseState cmd -> PQ.MinQueue (MeasuredDatabaseState cmd)
           -> PQ.MinQueue (MeasuredDatabaseState cmd)
    reject mdbState q =
      let q' = PQ.insert mdbState q
      in PQ.fromAscList (PQ.take rejectedCount q')

    dbStateAfterAction (DatabaseState curState _ cmds) action =
      let curState' = ((curState `HM.difference` HS.toMap (actionPreConditions action))
                     `HM.union` (DatabaseStateSourceDerived <$ HS.toMap (actionPostConditions action)))
      in DatabaseState curState' (HS.fromMap (() <$ curState'))
                       (cmds <> actionCommands action)
