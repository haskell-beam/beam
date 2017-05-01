{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Database.Beam.Migrate.Tool.Interactive where

import           Database.Beam.Migrate.Actions
import           Database.Beam.Migrate.Tool.Types
import           Database.Beam.Migrate.Types
import           Database.Beam.Migrate.SQL.SQL92

import           Brick
import qualified Brick.Main as M
import qualified Brick.AttrMap as A
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified Brick.Widgets.Dialog as W
import qualified Brick.Widgets.List as W
import qualified Brick.Widgets.Center as C
import qualified Brick.Types as T

import           Control.Concurrent
import           Control.Monad

import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import qualified Data.HashSet as HS

import qualified Graphics.Vty as V

data BeamMigratePane
  = PreConditionsList
  | PostConditionsList
  | AvailableActionsList
  | ChosenActionsList
  deriving (Show, Eq, Ord)

data BeamMigrateUI cmd
  = BeamMigrateUI
  { preConditions  :: W.List BeamMigratePane SomeDatabasePredicate
  , postConditions :: W.List BeamMigratePane SomeDatabasePredicate
  , paneFocus      :: BeamMigratePane
  , solveStatus    :: Solver cmd
  , availableCmds  :: W.List BeamMigratePane (PotentialAction cmd)
  , currentCmds    :: W.List BeamMigratePane cmd

  , uiPre, uiPost  :: HS.HashSet SomeDatabasePredicate

  , showWelcome    :: Maybe (W.Dialog ())
  }

beamBrickTheme :: A.AttrMap
beamBrickTheme = A.attrMap V.defAttr
    [ (W.dialogAttr, V.white `on` V.blue)
    , (W.buttonAttr, V.black `on` V.white)
    , (W.listSelectedAttr, V.brightWhite `on` V.black)
    , (W.listSelectedFocusedAttr, V.white `on` V.blue)
    , (W.buttonSelectedAttr, bg V.yellow)
    ]

nextFocus :: BeamMigratePane -> BeamMigratePane
nextFocus PreConditionsList = PostConditionsList
nextFocus PostConditionsList = AvailableActionsList
nextFocus AvailableActionsList = ChosenActionsList
nextFocus ChosenActionsList = PreConditionsList

drawApp :: (cmd -> String) -> BeamMigrateUI cmd -> [Widget BeamMigratePane]
drawApp renderSyntax ui =
  maybeToList (fmap welcomeDialog (showWelcome ui)) ++ [ withBorderStyle unicode base ]
  where
    welcomeDialog d = W.renderDialog d $ C.hCenter $ padAll 1 $ str "Welcome to the beam-migrate interactive differ"

    base :: Widget BeamMigratePane
    base = (borderWithLabel (str "Preconditions") preConditionsW <+> borderWithLabel (str "Available actions") availableActionsW)
           <=>
           (borderWithLabel (str "Postconditions") postConditionsW <+> borderWithLabel (str "Chosen actions") chosenActionsW)

    hasFocus w = paneFocus ui == w && isNothing (showWelcome ui)
    preConditionsW = let focused = hasFocus PreConditionsList
                     in padLeftRight 1 $
                        W.renderList (renderPred focused) focused (preConditions ui)
    postConditionsW = let focused = hasFocus PostConditionsList
                      in padLeftRight 1 $
                         W.renderList (renderPred focused) focused (postConditions ui)
    chosenActionsW = let focused = hasFocus ChosenActionsList
                     in padLeftRight 1 $
                        W.renderList (renderCmd focused) focused (currentCmds ui)
    availableActionsW = let focused = hasFocus AvailableActionsList
                        in padLeftRight 1 $
                           W.renderList (renderAction focused) focused (availableCmds ui)

    withFocus listFocused itemFocused =
      if itemFocused then if listFocused then withAttr W.listSelectedFocusedAttr else withAttr W.listSelectedAttr else id
    renderPred listFocused itemFocused (SomeDatabasePredicate p) =
      withFocus listFocused itemFocused (str (englishDescription p))
    renderCmd listFocused itemFocused cmd =
      withFocus listFocused itemFocused (str (renderSyntax cmd))
    renderAction listFocused itemFocused =
      withFocus listFocused itemFocused .
      str . intercalate ";" . map renderSyntax .
      toList . actionCommands

solveUI ui 0 = ui
solveUI ui k =
  case solveStatus ui of
    ChooseActions dbState getAction actions' next ->
      let solveStatus' = next actions'
          (preConditionsL, postConditionsL, availableCommandsL, currentCommandsL) =
            choosersForStatus (uiPre ui) (uiPost ui) solveStatus'
          ui' = ui { solveStatus = solveStatus'
                   , preConditions = preConditionsL
                   , postConditions = postConditionsL
                   , availableCmds = availableCommandsL
                   , currentCmds = currentCommandsL }
      in solveUI ui' (k - 1)

handleEvent :: BeamMigrateUI cmd -> BrickEvent BeamMigratePane e -> T.EventM BeamMigratePane (T.Next (BeamMigrateUI cmd))
handleEvent ui (VtyEvent ev) =
  case ev of
    V.EvKey (V.KChar 'q') [V.MCtrl] -> M.halt ui
    V.EvKey (V.KChar 'g') [] -> M.continue (solveUI ui 1)
    V.EvKey (V.KChar 'G') [] -> M.continue (solveUI ui 10)
    V.EvKey V.KEsc []
      | Just {} <- showWelcome ui -> M.continue (ui { showWelcome = Nothing })
      | otherwise -> M.halt ui
    V.EvKey V.KEnter []
      | Just {} <- showWelcome ui -> M.continue (ui { showWelcome = Nothing })
    V.EvKey (V.KChar '\t') []
      | Nothing <- showWelcome ui ->
        M.continue (ui { paneFocus = nextFocus $ paneFocus ui })
    _ | Just d <- showWelcome ui ->
          do d' <- W.handleDialogEvent ev d
             M.continue (ui { showWelcome = Just d' })
      | PreConditionsList <- paneFocus ui ->
          do l' <- W.handleListEvent ev (preConditions ui)
             M.continue (ui { preConditions = l' })
      | PostConditionsList <- paneFocus ui ->
          do l' <- W.handleListEvent ev (postConditions ui)
             M.continue (ui { postConditions = l' })
      | AvailableActionsList <- paneFocus ui ->
          do l' <- W.handleListEvent ev (availableCmds ui)
             M.continue (ui { availableCmds = l' })
      | ChosenActionsList <- paneFocus ui ->
          do l' <- W.handleListEvent ev (currentCmds ui)
             M.continue (ui { currentCmds = l' })

beamMigrateApp :: (cmd -> String) -> M.App (BeamMigrateUI cmd) e BeamMigratePane
beamMigrateApp renderSyntax =
  M.App { M.appDraw = drawApp renderSyntax
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = handleEvent
        , M.appStartEvent = return
        , M.appAttrMap = const beamBrickTheme}

diffInteractive :: Sql92SaneDdlCommandSyntax cmdSyntax
                => BeamMigrationBackend be cmdSyntax options
                -> options -> [SomeDatabasePredicate] -> [SomeDatabasePredicate]
                -> IO ()
diffInteractive BeamMigrationBackend{..} options preConditions postConditions =
  void (M.defaultMain (beamMigrateApp backendRenderSyntax) initUI)
  where
    initUI = BeamMigrateUI preConditionsL postConditionsL PreConditionsList
                           initSolveStatus
                           availableCommandsL currentCommandsL
                           preConditionsH postConditionsH
                           (Just welcomeDialog)
    initSolveStatus =
      heuristicSolver defaultActionProviders preConditions postConditions

    welcomeDialog = W.dialog (Just "Welcome") (Just (0, [ ("Ok", ()) ])) 50

    preConditionsH = HS.fromList preConditions
    postConditionsH = HS.fromList postConditions

    (preConditionsL, postConditionsL, availableCommandsL, currentCommandsL) =
      choosersForStatus preConditionsH postConditionsH initSolveStatus

choosersForStatus pre post sts =
  case sts of
    ProvideSolution (Just found) candidates ->
      ( W.list PreConditionsList  mempty 1, W.list PostConditionsList mempty 1

      , W.list AvailableActionsList mempty 1
      , W.list ChosenActionsList  (Vector.fromList found) 1 )
    ProvideSolution Nothing [] ->
      ( W.list PreConditionsList mempty 1, W.list PostConditionsList mempty 1

      , W.list AvailableActionsList mempty 1, W.list ChosenActionsList mempty 1 )
    ProvideSolution Nothing (dbState:_) ->
      let (preConditionsL, postConditionsL) = conditionsListForDbState pre post dbState
          commandsL = cmdsListForDbState dbState
      in ( preConditionsL, postConditionsL
         , W.list AvailableActionsList mempty 1, commandsL )

    ChooseActions dbState getAction actions' next ->
      let (preConditionsL, postConditionsL) = conditionsListForDbState pre post dbState
          commandsL = cmdsListForDbState dbState
          availableCommandsL = availableList (map getAction actions')
      in ( preConditionsL, postConditionsL
         , availableCommandsL, commandsL )

conditionsListForDbState :: HS.HashSet SomeDatabasePredicate
                         -> HS.HashSet SomeDatabasePredicate
                         -> DatabaseState cmd
                         -> ( W.List BeamMigratePane SomeDatabasePredicate
                            , W.List BeamMigratePane SomeDatabasePredicate )
conditionsListForDbState pre post dbState =
  ( W.list PreConditionsList  preConditions  1
  , W.list PostConditionsList postConditions 1 )
  where
    preConditions  = Vector.fromList (toList preLeft)
    postConditions = Vector.fromList (toList postLeft)

    cur = HS.fromMap (()<$ dbStateCurrentState dbState)
    preLeft  = (pre `HS.difference` post) `HS.intersection` cur
    postLeft = post `HS.difference` cur

cmdsListForDbState :: DatabaseState cmd -> W.List BeamMigratePane cmd
cmdsListForDbState dbState =
  W.list ChosenActionsList (Vector.fromList (toList (dbStateCmdSequence dbState))) 1

availableList :: [cmd] -> W.List BeamMigratePane cmd
availableList cmds = W.list AvailableActionsList (Vector.fromList cmds) 1
