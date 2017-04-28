{-# LANGUAGE RecordWildCards #-}
module Database.Beam.Migrate.Tool.Interactive where

import           Database.Beam.Migrate.Types
import           Database.Beam.Migrate.Tool.Types

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

import           Control.Monad

import           Data.Maybe
import qualified Data.Vector as Vector

import qualified Graphics.Vty as V

data Widgets = PreConditionsList
             | PostConditionsList
             deriving (Show, Eq, Ord)

data BeamMigrateUI
  = BeamMigrateUI
  { preConditions  :: W.List Widgets SomeDatabasePredicate
  , postConditions :: W.List Widgets SomeDatabasePredicate
  , paneFocus      :: Widgets
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

nextFocus :: Widgets -> Widgets
nextFocus PreConditionsList = PostConditionsList
nextFocus PostConditionsList = PreConditionsList

drawApp :: BeamMigrateUI -> [Widget Widgets]
drawApp ui =
  maybeToList (fmap welcomeDialog (showWelcome ui)) ++ [ base ]
  where
    welcomeDialog d = W.renderDialog d $ C.hCenter $ padAll 1 $ str "Welcome to the beam-migrate interactive differ"

    base :: Widget Widgets
    base = withBorderStyle unicode $
           (borderWithLabel (str "Preconditions") preConditionsW <+>
            borderWithLabel (str "Postconditios") postConditionsW)

    hasFocus w = paneFocus ui == w && isNothing (showWelcome ui)
    preConditionsW = let focused = hasFocus PreConditionsList
                     in padLeftRight 1 $
                        W.renderList (renderPred focused) focused (preConditions ui)
    postConditionsW = let focused = hasFocus PostConditionsList
                      in padLeftRight 1 $
                         W.renderList (renderPred focused) focused (postConditions ui)

    renderPred listFocused itemFocused (SomeDatabasePredicate p) =
      (if itemFocused then if listFocused then withAttr W.listSelectedFocusedAttr else withAttr W.listSelectedAttr else id) (str (englishDescription p))

handleEvent :: BeamMigrateUI -> BrickEvent Widgets e -> T.EventM Widgets (T.Next BeamMigrateUI)
handleEvent ui (VtyEvent ev) =
  case ev of
    V.EvKey (V.KChar 'q') [V.MCtrl] -> M.halt ui
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

beamMigrateApp :: M.App BeamMigrateUI e Widgets
beamMigrateApp =
  M.App { M.appDraw = drawApp
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = handleEvent
        , M.appStartEvent = return
        , M.appAttrMap = const beamBrickTheme}

diffInteractive :: BeamMigrationBackend be cmdSyntax options
                -> options
                -> [SomeDatabasePredicate] -> [SomeDatabasePredicate]
                -> IO ()
diffInteractive BeamMigrationBackend{..} options preConditions postConditions =
  void (M.defaultMain beamMigrateApp (BeamMigrateUI preConditionsL postConditionsL PreConditionsList (Just welcomeDialog)))
  where
    welcomeDialog = W.dialog (Just "Welcome") (Just (0, [ ("Ok", ()) ])) 50

    preConditionsL = W.list PreConditionsList (Vector.fromList preConditions) 1
    postConditionsL = W.list PostConditionsList (Vector.fromList postConditions) 1
