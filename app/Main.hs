{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev, focusRing, withFocusRing)
import Brick.Main (App (..), defaultMain, halt, showCursorNamed)
import Brick.Types (BrickEvent (..), CursorLocation, EventM, Widget)
import Brick.Util (on)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Border.Style (borderStyleFromChar, unicodeBold)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core (
    emptyWidget,
    hBox,
    hLimit,
    padAll,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
    (<=>),
 )
import Brick.Widgets.Dialog (
    Dialog,
    buttonAttr,
    buttonSelectedAttr,
    dialog,
    dialogSelection,
    handleDialogEvent,
    renderDialog,
 )
import Brick.Widgets.Edit (
    Editor,
    editor,
    getEditContents,
    handleEditorEvent,
    renderEditor,
 )
import Control.Monad (void)
import Control.Monad.IO.Class
import Graphics.Vty (black, defAttr, white, yellow)
import Graphics.Vty.Input.Events (Event (..), Key (..), Modifier (..))
import Language (Program)
import Lens.Micro ((&), (^.), _Just)
import Lens.Micro.Mtl (use, zoom, (%=), (.=), (?=))
import Lens.Micro.TH (makeLenses)
import Parser
import Semantics
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

data NewFileOpt = Save | Cancel

data Name
    = TextEditor
    | SaveButton
    | CancelButton
    | SaveTextEditor
    | FocusFilePrompt
    deriving (Eq, Ord, Show)

data St = St
    { _textEditor :: Editor String Name
    , _mfile :: Maybe FilePath
    , _newFilePrompt :: Maybe (Dialog NewFileOpt Name)
    , _saveAs :: Maybe (Editor String Name)
    , _newFileFocus :: Maybe (FocusRing Name)
    }
makeLenses ''St

initState :: Maybe FilePath -> String -> St
initState f c = St (editor TextEditor Nothing c) f Nothing Nothing Nothing

draw :: St -> [Widget Name]
draw st = drawSave st : [vCenter $ padAll 1 $ maxWidth 200 $ vBox content]
  where
    content = [textEd, instructions]
    textEd = withBorderStyle (borderStyleFromChar ' ') $ withAttr borderLabelAttr $ borderWithLabel (str "Turbo Calculus") e
    e = renderEditor (str . unlines) True (st ^. textEditor)
    maxWidth w = hCenter . hLimit w
    instructions =
        maxWidth 100 $
            hBox
                [ drawInstruction "^X" "Exit"
                , drawInstruction "^S" "Save"
                , drawInstruction "^C" "Compile"
                ]

drawInstruction :: String -> String -> Widget Name
drawInstruction keys action =
    withAttr keyAttr (str keys) <+> str " to " <+> str action & hCenter

drawSave :: St -> Widget Name
drawSave st = do
    case (st ^. newFilePrompt, st ^. saveAs, st ^. newFileFocus) of
        (Just sd, Just ed, Just f) -> renderDialog sd $ hCenter $ padAll 1 $ str "New file name" <=> drawSaveAs f ed
        _ -> emptyWidget

drawSaveAs :: FocusRing Name -> Editor String Name -> Widget Name
drawSaveAs f ed = withBorderStyle unicodeBold $ border $ vLimit 1 $ label <+> saveAsEditor
  where
    saveAsEditor = withFocusRing f (renderEditor (str . unlines)) ed
    label = str " Filter: "

compile :: FilePath -> IO ()
compile f = do
    contents <- readFile f
    case parser contents :: Either String Program of
        Left err -> putStrLn err >> exitFailure
        Right ast -> do
            stack <- initStack
            result <- semanticsEval stack (analyzeProgram ast)
            print result
            exitSuccess

saveFile :: FilePath -> EventM Name St ()
saveFile file = do
    contents <- unlines . getEditContents <$> use textEditor
    liftIO $ writeFile file contents

createDialog :: Dialog NewFileOpt Name
createDialog = dialog (Just $ str "Save new file") (Just (SaveButton, choices)) 50
  where
    choices =
        [ ("Save", SaveButton, Save)
        , ("Cancel", CancelButton, Cancel)
        ]

createSaveAs :: Editor String Name
createSaveAs = editor SaveTextEditor Nothing ""

createFocusRing :: FocusRing Name
createFocusRing = focusRing [SaveTextEditor, SaveButton]

saveNewPrompt :: EventM Name St ()
saveNewPrompt = do
    newFileFocus ?= createFocusRing
    newFilePrompt ?= createDialog
    saveAs ?= createSaveAs

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent ev@(VtyEvent e)
    | isQuit e = quit
    | otherwise = do
        prompt <- use newFilePrompt
        case prompt of
            Just _ -> appEventDialog ev
            Nothing -> appEventMain ev
  where
    isQuit (EvKey (KChar 'x') [MCtrl]) = True
    isQuit (EvKey (KChar 'q') [MCtrl]) = True
    isQuit (EvKey (KChar 'k') [MCtrl]) = True
    isQuit (EvKey (KChar 'c') [MCtrl]) = True
    isQuit _ = False
appEvent _ = pure ()

quit :: EventM Name St ()
quit = halt

appEventDialog :: BrickEvent Name e -> EventM Name St ()
appEventDialog (VtyEvent (EvKey KEsc [])) = newFilePrompt .= Nothing
appEventDialog (VtyEvent (EvKey KEnter [])) = do
    filePrompt <- use newFilePrompt
    case dialogSelection =<< filePrompt of
        Just (CancelButton, Cancel) -> newFilePrompt .= Nothing
        Just (SaveButton, Save) -> undefined
        _ -> pure ()
appEventDialog (VtyEvent (EvKey (KChar '\t') [])) = (newFileFocus . _Just) %= focusNext
appEventDialog (VtyEvent (EvKey KBackTab [])) = (newFileFocus . _Just) %= focusPrev
appEventDialog ev@(VtyEvent e) = do
    f <- use newFileFocus
    case f of
        Nothing -> pure ()
        Just fring ->
            case focusGetCurrent fring of
                Just SaveButton -> zoom (newFilePrompt . _Just) $ handleDialogEvent e
                Just SaveTextEditor -> zoom (saveAs . _Just) $ handleEditorEvent ev
                _ -> pure ()
appEventDialog ev = zoom (saveAs . _Just) $ handleEditorEvent ev

appEventMain :: BrickEvent Name e -> EventM Name St ()
appEventMain (VtyEvent (EvKey (KChar 's') [MCtrl])) = do
    f <- use mfile
    maybe saveNewPrompt saveFile f
appEventMain ev = zoom textEditor $ handleEditorEvent ev

keyAttr :: AttrName
keyAttr = attrName "key"

borderLabelAttr :: AttrName
borderLabelAttr = attrName "borderLabel"

appMap :: AttrMap
appMap =
    attrMap
        defAttr
        [ (keyAttr, black `on` white)
        , (borderLabelAttr, black `on` white)
        , (buttonAttr, white `on` black)
        , (buttonSelectedAttr, black `on` yellow)
        ]

appCursor :: St -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor st cursors =
    case st ^. newFilePrompt of
        Just _ -> showCursorNamed SaveTextEditor cursors
        Nothing -> showCursorNamed TextEditor cursors
app :: App St e Name
app =
    App
        { appDraw = draw
        , appChooseCursor = appCursor
        , appHandleEvent = appEvent
        , appStartEvent = pure ()
        , appAttrMap = const appMap
        }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> do
            contents <- readFile f
            void $ defaultMain app (initState (Just f) contents)
        [] -> void $ defaultMain app (initState Nothing "")
        _ -> putStrLn "Usage: rcc [FILE]"
